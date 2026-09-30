# Calling Generated Programs from Multi-Threaded Java Applications

Programs compiled by `cobj` can be called from a multi-threaded Java application
such as a Tomcat or Spring Boot server: several threads of one JVM may run COBOL
programs at the same time.

This document describes the model that makes this work, and what an application
has to do at the boundary between Java and COBOL.

Programs compiled by an earlier version of `cobj` must be recompiled: the
generated code now calls accessor methods for the per-thread state of `libcobj`
instead of the public static fields it used before, so an old class fails with
`NoSuchFieldError` on this `libcobj.jar`.

## One run unit per thread

`libcobj` keeps the state of a COBOL **run unit** per thread, so **each thread is
an independent run unit**. The following belong to the calling thread and are
never shared with another thread:

* the module stack and the current module (`CobolModule`)
* the last exception code and the last error file
* the cache of programs resolved by `CALL` and the call stack
* the files opened by the run unit, their position, and the locks they hold
* `EXTERNAL` data items and `EXTERNAL` files
* `SWITCH-1` through `SWITCH-8`, the number of `CALL` parameters, and the
  `DISPLAY UPON COMMAND-LINE` data
* the ESQL connection, its cursors and prepared statements, and the `SQLCA`

The following are shared by the whole JVM:

* `System.out`, `System.err` and `System.in`
* the runtime settings read from the `COB_*` environment variables
  (see [Environment Variables Reference](./environment_variables.md)); they are
  read once for the JVM, not once per thread. `COB_SWITCH_1` through
  `COB_SWITCH_8` are the exception: they are read for each run unit
* the file system, and therefore the file locks and record locks that the run
  units of the JVM take on the same file

`WORKING-STORAGE` is not static in the generated Java code: it lives in the
instance of the generated class. **Give each thread its own instance of the
generated program**; an instance must not be used by two threads at the same
time.

```java
import jp.osscons.opensourcecobol.libcobj.common.CobolRunUnit;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;

// One instance of the COBOL program "sample" per thread
private static final ThreadLocal<sample> program = ThreadLocal.withInitial(sample::new);

CobolDataStorage arg1 = new CobolDataStorage(9);
CobolDataStorage arg2 = new CobolDataStorage(9);
int returnCode = program.get().run(arg1, arg2);
```

A generated program implements
`jp.osscons.opensourcecobol.libcobj.call.CobolRunnable`, whose `run` method takes
the `PROCEDURE DIVISION USING` parameters as `CobolDataStorage` arguments and
returns `RETURN-CODE`. Programs reached by a `CALL` statement are resolved and
instantiated by the run unit of the calling thread, so their `WORKING-STORAGE`
is private to that thread as well.

## Initializing the runtime

`CobolUtil.ensureInitialized()` initializes the runtime settings (the `COB_*`
environment variables, the program switches, the file I/O subsystem). It
initializes at most once per JVM and is safe to call from several threads at the
same time.

Generated programs call it themselves when they are entered, so an application
that only calls COBOL programs does not have to call it. Call it explicitly when
the runtime settings should be in effect before the first COBOL call — for
example at application start-up:

```java
import jp.osscons.opensourcecobol.libcobj.common.CobolUtil;

CobolUtil.ensureInitialized();
```

`CobolUtil.isInitialized()` reports whether the runtime has been initialized.

## Ending a run unit

```java
jp.osscons.opensourcecobol.libcobj.common.CobolRunUnit.end();
```

`CobolRunUnit.end()` ends the run unit of the calling thread: files that are
still open are closed implicitly (which commits their records and releases their
locks) and all run-unit state listed above is discarded.

Call it when a thread has finished with COBOL — typically at the end of a request
in a server that reuses its threads. Without it, the state of the previous run
unit (open files, resolved programs, `EXTERNAL` items, switches) stays on the
pooled thread and is seen by the next request that lands on it.

ESQL connections that the program left open (no `DISCONNECT`) are rolled back
and closed, which is what the database does when a process exits without
disconnecting. Uncommitted changes are therefore never made permanent by the end
of a run unit, even when it ends because of a runtime error.

`CobolRunUnit.resetThreadState()` discards the same state **without** closing the
open files. Use it only when the files have already been closed.

Ending the run unit does not reset the Java objects of the programs themselves:
if the same instance of a generated program is run again afterwards, its
`WORKING-STORAGE` still holds the values of the previous run. Create a new
instance for a new run unit when the program must start from its `VALUE`
clauses. Programs reached through `CALL` are instantiated anew, because the
`CALL` cache belongs to the run unit.

Values set with `SET ENVIRONMENT` or `DISPLAY ... UPON ENVIRONMENT-VALUE` belong
to the run unit too: they are visible to the programs of the same thread only and
are discarded with it.

If the program ends with `STOP RUN`, the run unit is already ended (see below)
and a further `CobolRunUnit.end()` is harmless.

## `STOP RUN`

`STOP RUN` does **not** terminate the JVM when the program was entered from Java.
It ends the run unit of the calling thread — open files are closed implicitly —
and returns to the Java caller of the root program of that run unit; `run`
returns the `STOP RUN` return code. Other threads keep running.

If `STOP RUN` is executed in a program that was reached by a `CALL`, control
unwinds through the calling COBOL programs of the same thread up to the root
program of the run unit, and only that program returns to Java.

`System.exit` is called only by the `main` method of a generated program, that
is, when the program is started from the command line with
`java [PROGRAM-ID]`. The process then exits with the `STOP RUN` return code.

## `DISPLAY` and `ACCEPT`

`System.out` and `System.err` are shared by the whole JVM. The output of a single
`DISPLAY` statement is written atomically, so the output of two threads is never
interleaved inside one statement. The order in which concurrent threads write is
not defined.

`ACCEPT` reads from the shared standard input, which is rarely meaningful in a
server; a thread that executes `ACCEPT` competes with every other thread for the
same stream.

## Sharing files between threads

The run units of one JVM share files under the same rules as separate processes.

* **SEQUENTIAL, LINE SEQUENTIAL and RELATIVE files**: the lock the run unit takes
  on `OPEN` is recorded inside the JVM as well as on the operating system, so a
  thread that opens a file another thread of the same JVM has open exclusively
  gets `FILE STATUS 61`, exactly as it would if another process held the file.
  Threads that all open the file for input share it.
* **INDEXED files**: the owner of a file lock or a record lock is a run unit, not
  a process, so a record locked by one thread cannot be read with a lock by
  another thread of the same JVM (`FILE STATUS 51`), and a file locked by
  `OPEN OUTPUT` in one thread cannot be opened by another (`FILE STATUS 61`).
  See
  [The specification of locking for INDEXED files](./specification-locking-indexed-file.md).

Closing a file, `STOP RUN` and `CobolRunUnit.end()` all release the locks the run
unit holds on it.

## Logging (SLF4J)

`libcobj.jar` is a shadow (fat) jar that bundles `slf4j-simple` as its SLF4J
provider. A Spring Boot application brings its own provider (Logback) through
`spring-boot-starter-logging`, and two providers on the class path conflict, so
one of them has to be excluded. To keep the provider bundled in `libcobj.jar`:

```kotlin
// build.gradle.kts
configurations.all {
    exclude(group = "org.springframework.boot", module = "spring-boot-starter-logging")
    exclude(group = "ch.qos.logback")
}
```

See also
[Build a Spring Boot project using Visual Studio Code](./cobj-api_SpringBoot.md).

## Example and tests

* `samples/spring-boot-smoke/` is a small Spring Boot application that runs COBOL
  programs on HTTP request threads, with one program instance and one run unit
  per thread, and an endpoint that calls `CobolRunUnit.end()`.
* `tests/multithread` is the test suite that runs generated programs
  concurrently: arithmetic and intrinsic functions, `WORKING-STORAGE` of `CALL`ed
  programs, `STOP RUN`, sequential files, file sharing, INDEXED record locks and
  `SORT`.
