# Embedded SQL (ESQL) Guide for opensource COBOL 4J

## Overview

opensource COBOL 4J supports Embedded SQL (EXEC SQL) to interact with PostgreSQL databases directly from COBOL programs. COBOL host variables are used as bind parameters in SQL statements, enabling seamless data exchange between COBOL and SQL.

When a COBOL source file containing `EXEC SQL ... END-EXEC` statements is compiled with `cobj`, the compiler translates the embedded SQL into Java database calls via JDBC.

## Prerequisites

- **opensource COBOL 4J** installed (see [README.md](../README.md))
- **PostgreSQL** server (version 9.6 or later)

## Quick Start

Below is a minimal COBOL program that connects to PostgreSQL, inserts a row, reads it back, and disconnects.

```cobol
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 QUICK-START.
       DATA                        DIVISION.
       WORKING-STORAGE             SECTION.

       EXEC SQL INCLUDE SQLCA END-EXEC.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  DBNAME    PIC X(30) VALUE "testdb@localhost:5432".
       01  USERNAME  PIC X(30) VALUE "main_user".
       01  PASSWD    PIC X(10) VALUE "password".
       01  EMP-NAME  PIC X(20).
       EXEC SQL END DECLARE SECTION END-EXEC.

       PROCEDURE                   DIVISION.
           EXEC SQL
               CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME
           END-EXEC.

           MOVE "Alice" TO EMP-NAME.
           EXEC SQL
               INSERT INTO employees (name) VALUES (:EMP-NAME)
           END-EXEC.

           MOVE SPACES TO EMP-NAME.
           EXEC SQL
               SELECT name INTO :EMP-NAME FROM employees LIMIT 1
           END-EXEC.
           DISPLAY "Employee: " EMP-NAME.

           EXEC SQL COMMIT END-EXEC.
           EXEC SQL DISCONNECT ALL END-EXEC.
           STOP RUN.
```

Compile and run:

```bash
cobj quick-start.cbl
java QUICK-START
```

> [!NOTE]
> `EXEC SQL INCLUDE SQLCA END-EXEC` is handled internally by the compiler, so you do
> not need a COPY file for the SQLCA, nor an `-I` option to point at a copy directory.
> (Only when you also use your own COPY books do you need `-I` to specify their directory.)

## Supported SQL Statements

### CONNECT / DISCONNECT

```cobol
       EXEC SQL
           CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME
       END-EXEC.

       EXEC SQL
           DISCONNECT ALL
       END-EXEC.
```

The connection string format for `:DBNAME` is:

```
dbname@host:port
```

For example: `"testdb@localhost:5432"`.

If the user, password, or database-name host variable is empty, the corresponding value
falls back to the `OCDB_DB_USER`, `OCDB_DB_PASS`, and `OCDB_DB_NAME` environment variables
(see [Environment Variables](#environment-variables)). In particular, the short form
`EXEC SQL CONNECT END-EXEC` relies entirely on those environment variables.

### BEGIN / END DECLARE SECTION

`EXEC SQL BEGIN DECLARE SECTION END-EXEC` and `EXEC SQL END DECLARE SECTION END-EXEC` are accepted for backward compatibility but are **ignored**. All variables in WORKING-STORAGE SECTION and LINKAGE SECTION are always available as host variables in SQL statements, regardless of whether they are enclosed in a DECLARE SECTION or not.

```cobol
       WORKING-STORAGE SECTION.
       01  MY-VAR  PIC X(20).
       01  MY-NUM  PIC 9(5).
       PROCEDURE DIVISION.
           EXEC SQL
               INSERT INTO TBL VALUES (:MY-VAR, :MY-NUM)
           END-EXEC.
```

### Host variable forms

Host variables can take the following forms:

| Form | Meaning |
|---|---|
| `:VAR` | Plain host variable |
| `:VAR(IDX)` | OCCURS element; `IDX` is a literal integer or a COBOL variable |
| `:VAR(I, J)` | Multi-dimensional OCCURS element |
| `:GRP.SUB` | Group-qualified host variable (`SUB` inside `GRP`) |
| `:GRP.SUB(IDX)` | Group-qualified + subscripted |
| `:GRP.SUB(GRP2.IDX)` | The subscript value itself is a group-qualified COBOL variable |

The constraint on subscript values is that the variable used as a subscript cannot itself carry a subscript (i.e. `:VAR(IDX(1))` is not supported). If you need an indirected index, MOVE it into a scratch variable first.

```cobol
       01  GRP.
         03 ROW OCCURS 5.
           05 VAL PIC 9(4).
       01  GRP2.
         03 TMP-IDX PIC S9(2).
       01  IDX PIC S9(2).
       ...
           MOVE 3 TO TMP-IDX OF GRP2.
           EXEC SQL
               SELECT FIELD INTO :GRP.VAL(GRP2.TMP-IDX)
                 FROM TESTTABLE WHERE N = :IDX
           END-EXEC.
```

### SQLCA (SQL Communication Area)

The SQLCA is automatically defined when a program executes `EXEC SQL` statements. It provides `SQLCODE`, `SQLSTATE`, `SQLERRMC`, and other diagnostic fields that are updated after each SQL statement.

You should declare it explicitly with `EXEC SQL INCLUDE SQLCA END-EXEC.` in the WORKING-STORAGE SECTION. If a program contains embedded SQL but omits this declaration, `cobj` still defines the SQLCA implicitly but emits the following compile-time warning:

```
embedded SQL is used without 'EXEC SQL INCLUDE SQLCA END-EXEC'; SQLCA is declared implicitly
```

The implicitly defined SQLCA has the following structure:

```cobol
       01  SQLCA GLOBAL.
           05  SQLCAID               PIC X(8).
           05  SQLCABC               PIC S9(9) COMP-5.
           05  SQLCODE               PIC S9(9) COMP-5.
           05  SQLERRM.
           49  SQLERRML              PIC S9(4) COMP-5.
           49  SQLERRMC              PIC X(70).
           05  SQLERRP               PIC X(8).
           05  SQLERRD OCCURS 6 TIMES
                                     PIC S9(9) COMP-5.
           05  SQLWARN.
               10 SQLWARN0           PIC X(1).
               10 SQLWARN1           PIC X(1).
               10 SQLWARN2           PIC X(1).
               10 SQLWARN3           PIC X(1).
               10 SQLWARN4           PIC X(1).
               10 SQLWARN5           PIC X(1).
               10 SQLWARN6           PIC X(1).
               10 SQLWARN7           PIC X(1).
           05  SQLSTATE              PIC X(5).
```

| Field | Description |
|-------|-------------|
| `SQLCODE` | Return code: 0 = success, 100 = no more rows, negative = error |
| `SQLSTATE` | 5-character SQL state code (e.g., `"00000"`, `"02000"`, `"08001"`) |
| `SQLERRMC` | Error message text (up to 70 characters) |
| `SQLERRML` | Length of error message in `SQLERRMC` |
| `SQLERRD(3)` | Number of rows affected by the last statement |

> [!NOTE]
> On a successful `CONNECT`, `SQLERRMC` is **not** overwritten: the COBOL-initialized
> value (70 spaces) is left in place and `SQLERRML` is set to 0. This matches Open COBOL
> ESQL 4J behavior. Only error paths write a message into `SQLERRMC`.

### SELECT INTO

Single-row select:

```cobol
       EXEC SQL
           SELECT emp_name, emp_salary
               INTO :EMP-NAME, :EMP-SALARY
               FROM employees
               WHERE emp_no = :EMP-NO
       END-EXEC.
```

Array fetch with OCCURS (`SELECT ... INTO` an OCCURS host variable returns up to
`OCCURS` rows; the number of rows actually stored is reported in `SQLERRD(3)`):

```cobol
       01  EMP-NAMES.
           05 EMP-NAME PIC X(20) OCCURS 10 TIMES.

       EXEC SQL
           SELECT emp_name INTO :EMP-NAME FROM employees
       END-EXEC.
```

### INSERT, UPDATE, DELETE

Use host variables prefixed with `:` as bind parameters:

```cobol
       EXEC SQL
           INSERT INTO employees (emp_no, emp_name, emp_salary)
               VALUES (:EMP-NO, :EMP-NAME, :EMP-SALARY)
       END-EXEC.

       EXEC SQL
           UPDATE employees SET emp_salary = :NEW-SALARY
               WHERE emp_no = :EMP-NO
       END-EXEC.

       EXEC SQL
           DELETE FROM employees WHERE emp_no = :EMP-NO
       END-EXEC.
```

### DECLARE CURSOR / OPEN / FETCH / CLOSE

```cobol
       EXEC SQL
           DECLARE emp_cursor CURSOR FOR
               SELECT emp_no, emp_name FROM employees
               ORDER BY emp_no
       END-EXEC.

       EXEC SQL OPEN emp_cursor END-EXEC.

       PERFORM UNTIL SQLCODE NOT = ZERO
           EXEC SQL
               FETCH emp_cursor INTO :EMP-NO, :EMP-NAME
           END-EXEC
           IF SQLCODE = ZERO
               DISPLAY "No: " EMP-NO " Name: " EMP-NAME
           END-IF
       END-PERFORM.

       EXEC SQL CLOSE emp_cursor END-EXEC.
```

> [!NOTE]
> A single-row FETCH pre-reads the number of rows given by the `OCESQL4J_FETCH_RECORDS`
> environment variable (see below) in one `FETCH FORWARD`, then serves the rows one at a
> time from a buffer on subsequent FETCHes. This reduces the number of round trips to the
> database. The default is 1, in which case rows are fetched one at a time as before.
> For positioned UPDATE/DELETE using `WHERE CURRENT OF`, the cursor position advanced by
> the pre-read is automatically rewound before the statement runs, so the logical
> "current row" is updated/deleted.

#### Fetching multiple rows into an OCCURS array

`FETCH ... INTO` an OCCURS host variable retrieves several rows at once. The runtime
issues a single `FETCH FORWARD <occurs-max>` directly (bypassing the single-row pre-read
buffer) and stores up to `OCCURS` rows. The number of rows actually fetched is reported in
`SQLERRD(3)`.

```cobol
       01  EMP-NAMES.
           05 EMP-NAME PIC X(20) OCCURS 10 TIMES.

       EXEC SQL OPEN emp_cursor END-EXEC.
       EXEC SQL
           FETCH emp_cursor INTO :EMP-NAME
       END-EXEC.
       DISPLAY "Rows fetched: " SQLERRD(3).
```

#### Cursor error behavior

| Situation | Result |
|---|---|
| OPEN / FETCH / CLOSE on a cursor that was never DECLAREd | `SQLCODE = -602`, `SQLSTATE = 34000` |
| CLOSE on a DECLAREd but never-OPENed cursor | Success (`SQLCODE = 0`) |
| FETCH on a DECLAREd but never-OPENed cursor | The statement is still sent to PostgreSQL; the resulting `"cursor does not exist"` `SQLSTATE` and message are stored in the SQLCA (not a fixed built-in message) |

### PREPARE / EXECUTE

```cobol
       01  SQL-STMT  PIC X(200).

       MOVE "INSERT INTO employees (emp_no, emp_name) VALUES (?, ?)"
           TO SQL-STMT.
       EXEC SQL
           PREPARE stmt1 FROM :SQL-STMT
       END-EXEC.

       EXEC SQL
           EXECUTE stmt1 USING :EMP-NO, :EMP-NAME
       END-EXEC.
```

### COMMIT / ROLLBACK

```cobol
       EXEC SQL COMMIT END-EXEC.
       EXEC SQL ROLLBACK END-EXEC.
```

## COBOL Items Usable as Host Variables

The following COBOL items can be used as host variables. The runtime interprets the item's bytes according to its PIC clause / USAGE when exchanging data with SQL.

| COBOL item | Description |
|---|---|
| `PIC X(n)` | Alphanumeric string |
| `PIC A(n)` | Alphabetic |
| `PIC 9(n)` / `PIC S9(n)` | DISPLAY (zoned decimal); supports signed/unsigned and sign position (LEADING/TRAILING, SEPARATE/combined) |
| `PIC 9(n)V9(m)` | Fixed-point decimal |
| `USAGE COMP-3` (packed decimal) | Packed decimal |
| `PIC N(n)` | Japanese, Shift-JIS |
| Group item | Treated as alphanumeric |
| VARYING item | Variable-length alphanumeric / Japanese string |

## SQLCA Error Handling

After each `EXEC SQL` statement, the SQLCA fields are updated:

| Field | Description |
|---|---|
| `SQLCODE` | 0 = success, +100 = not found, negative = error |
| `SQLSTATE` | 5-character SQL state code |
| `SQLERRMC` | Error message text |
| `SQLERRD(3)` | Number of rows affected |

Common SQLCODE values (defined in `SqlCA.java`):

| SQLCODE | SQLSTATE | Meaning |
|---|---|---|
| `+0` | `00000` | Success |
| `+100` | `02000` | No more rows / end of cursor (`ECPG_NOT_FOUND`) |
| `-213` | `22002` | NULL value read into a host variable without an indicator (`ECPG_MISSING_INDICATOR`) |
| `-220` | `08003` | No active connection (`ECPG_NO_CONN`) |
| `-402` | `08001` etc. | CONNECT failed (`ECPG_CONNECT`) |
| `-602` | `34000` | Cursor (portal) does not exist (`ECPG_WARNING_UNKNOWN_PORTAL`) |
| `-9999` | (server) | PostgreSQL error that maps to no specific ECPG code (`ECPG_UNKNOWN_ERROR`) |

Example error handling:

```cobol
       IF SQLCODE NOT = ZERO
           DISPLAY "SQL Error: " SQLCODE
           DISPLAY "State: " SQLSTATE
           DISPLAY "Message: " SQLERRMC
           EXEC SQL ROLLBACK END-EXEC
           STOP RUN
       END-IF.
```

## Connection String Format

The connection string passed to CONNECT follows this format:

```
dbname@host:port
```

Example:

```cobol
       MOVE "testdb@localhost:5432" TO DBNAME.
```

## Environment Variables

| Variable | Description |
|---|---|
| `OCDB_DB_NAME` | Default database name |
| `OCDB_DB_USER` | Default database user |
| `OCDB_DB_PASS` | Default database password |
| `OCDB_DB_CHAR` | Character encoding for the database connection. When unset, the connection defaults to `UTF-8`. |
| `OCESQL4J_FETCH_RECORDS` | Cursor pre-read (bulk fetch) count: the number of rows pulled in a single `FETCH FORWARD`. Defaults to 1 (one row at a time). Values of 0 or less, or non-numeric values, are treated as 1. Read once at process startup. |

`OCDB_DB_NAME`, `OCDB_DB_USER`, and `OCDB_DB_PASS` are used as fallbacks when the
corresponding `CONNECT` host variable is empty, which is how the short form
`EXEC SQL CONNECT END-EXEC` obtains its connection parameters.

## Compilation

```bash
# Compile a COBOL program with EXEC SQL
cobj program.cbl

# Run the compiled program
java program
```

## Runtime Logging

The ESQL runtime emits logs through [SLF4J](https://www.slf4j.org/). When you execute an
`EXEC SQL` statement, the following logs are produced depending on its kind. They are useful
for inspecting the SQL being run and the connection state, and for troubleshooting.

### Logs per statement

| ESQL statement | ERROR (on failure) | DEBUG | TRACE |
|---|---|---|---|
| `CONNECT` | connection failure | connection success (connection id), JDBC URL and user name | host-variable values of the connection parameters (user name, DB name) |
| `DISCONNECT` | disconnect failure | disconnect start (connection id) | - |
| `INSERT` / `UPDATE` / `DELETE` / DDL and other `EXEC SQL` | SQL execution failure | the SQL statement being executed (trimmed) | - |
| Parameterized `EXEC SQL` (e.g. `EXECUTE ... USING`) | SQL execution failure | the SQL statement and parameter count | - |
| `SELECT ... INTO` | SELECT INTO failure | the SQL statement being executed | - |
| `DECLARE ... CURSOR` | - | cursor name and SQL statement | - |
| `OPEN` cursor | cursor open failure | cursor name | - |
| `FETCH` | - | - | cursor name |

Key points:

- **ERROR** … when a SQL statement fails, the offending SQL statement and the error message are logged.
- **DEBUG** … normal progress is logged: the SQL executed, connection establishment/teardown, and cursor operations.
- **TRACE** … high-frequency or detailed information such as the `CONNECT` host-variable values and `FETCH` (kept separate from DEBUG).
- Positioned `UPDATE` / `DELETE` with `WHERE CURRENT OF` have no dedicated logs; they correct the
  cursor position internally and then run as a normal `EXEC SQL`, so the `EXEC SQL` logs above are emitted.

> [!NOTE]
> Logs may include host-variable values (such as the `CONNECT` user name and DB name) and SQL
> statements containing parameters. When enabling TRACE/DEBUG in production, be aware that
> sensitive information may end up in the logs.

### Configuring logging

`libcobj.jar` bundles slf4j-simple, and logs are written to standard error. By default
the ESQL loggers are turned **off**: `libcobj.jar` ships a `simplelogger.properties` that sets
`org.slf4j.simpleLogger.log.jp.osscons.opensourcecobol.libcobj.sql=off`, so a program run with a
plain `java` command and no options produces no ESQL logs. The setting is scoped to the ESQL logger
namespace (rather than the global `defaultLogLevel`) so the bundled file never silences other
slf4j loggers your application may use. To see logs, raise the level for that namespace with a `-D`
system property (a `-D` property always overrides the bundled default). The ESQL runtime emits three
kinds of logs — ERROR / DEBUG / TRACE — so there are three patterns to choose from depending on the
value you set.

ERROR only (output just the errors on failure):

```bash
java -Dorg.slf4j.simpleLogger.log.jp.osscons.opensourcecobol.libcobj.sql=error YourProgram
```

ERROR and DEBUG (also output the SQL being executed, connect/disconnect, and cursor operations):

```bash
java -Dorg.slf4j.simpleLogger.log.jp.osscons.opensourcecobol.libcobj.sql=debug YourProgram
```

ERROR, DEBUG, and TRACE (output everything, including the `CONNECT` host-variable values and `FETCH`):

```bash
java -Dorg.slf4j.simpleLogger.log.jp.osscons.opensourcecobol.libcobj.sql=trace YourProgram
```

> [!NOTE]
> The `...libcobj.sql` namespace covers both the `CobolEsql` logger (EXEC SQL execution) and the
> `SqlConnection` logger (connect/disconnect), so the settings above enable all ESQL logs. To target
> a single logger instead, append its class name, e.g.
> `-Dorg.slf4j.simpleLogger.log.jp.osscons.opensourcecobol.libcobj.sql.CobolEsql=debug`.

## Limitations

All of the limitations listed below are also present in Open COBOL ESQL 4J, the project this feature was ported from. Because this feature is ported to match the behavior of Open COBOL ESQL 4J, it inherits the same restrictions as the original implementation on these points.

- COBOL-classic `OF` qualification (`:VAR OF GRP`) is not supported. Use dotted qualification (`:GRP.VAR`) instead.
- Subscript values cannot be arithmetic expressions (`:VAR(I+1)`) and cannot themselves be subscripted host variables (`:VAR(IDX(1))`). Compute the index into a scratch COBOL variable first.
- Only PostgreSQL is supported as the target database.
- The following ECPG/embedded-SQL features are **not** supported:
  - `EXECUTE IMMEDIATE`.
  - `WHENEVER` (declarative condition handling). Check `SQLCODE` / `SQLSTATE` explicitly instead.
  - Backward / scrollable FETCH (`FETCH PRIOR`, `FETCH BACKWARD`, scrollable cursors, etc.). Only forward FETCH is available to the program. (`FETCH BACKWARD` is used internally only to correct the cursor position for `WHERE CURRENT OF`.)
  - Multiple connections. The `AT db` clause is accepted syntactically but **ignored**; all statements run against the single default connection. `DISCONNECT ALL` also affects only the default connection.
  - Indicator variables (`:VAR:IND`). A NULL fetched into a host variable without an indicator is reported via `SQLCODE = -213` (`SQLSTATE 22002`) instead.
- User-name, password, and connection-string values have their trailing spaces (the COBOL fixed-length padding) stripped at runtime. Embedded spaces are preserved, but a value cannot carry significant trailing spaces.

For the internal architecture and how these forms are parsed and translated into Java, see [esql-design.md](./esql-design.md).

## Examples

See the [example/esql/](../example/esql/) directory for complete working examples:

- **sample.cbl** -- Basic CONNECT, INSERT, SELECT, DISCONNECT
- **cursor.cbl** -- Cursor-based row iteration
- **prepare.cbl** -- Prepared statements with EXECUTE USING
- **INSERTTBL.cbl** -- Create the EMP table and INSERT rows with literal values and host variables (OCCURS table)
- **FETCHTBL.cbl** -- SELECT COUNT(*), then fetch the EMP table via a cursor and display it (run after INSERTTBL.cbl)
