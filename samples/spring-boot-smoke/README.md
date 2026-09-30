# Spring Boot smoke test

A minimal Spring Boot application that runs COBOL programs compiled by
`cobj` on HTTP request threads. CI builds it, starts it and sends many
concurrent requests to check that the generated code and `libcobj` work in
a multi-threaded servlet container.

```sh
# opensource COBOL 4J must be installed (cobj on PATH) and libcobj built
../../libcobj/gradlew -p . bootJar
java -jar build/libs/spring-boot-smoke.jar &
curl 'http://localhost:18080/calc?num=12&txt=banana'
curl 'http://localhost:18080/end'
```

Pass `-PcobjBin=/path/to/cobj` and `-PlibcobjJar=/path/to/libcobj.jar` to
`gradlew` when they are not in the default locations.

The controller keeps one instance of the COBOL program per request thread
(`ThreadLocal`), which gives every thread its own run unit with its own
WORKING-STORAGE, open files and EXTERNAL items. `GET /end` ends the run unit of
the thread that serves the request by calling `CobolRunUnit.end()`, which closes
the files it left open and discards its run-unit state -- worth doing on a
pooled thread so nothing leaks into the next request.

`STOP RUN` in a program called this way does not stop the server: it ends the run
unit of the request thread and returns to the Java caller.

`libcobj.jar` bundles `slf4j-simple` as its SLF4J provider, which conflicts with
Spring Boot's Logback, so `build.gradle.kts` excludes
`spring-boot-starter-logging` and `ch.qos.logback`.

See [doc/multithreading.md](../../doc/multithreading.md) for the full model.
