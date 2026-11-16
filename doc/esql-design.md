# Embedded SQL (ESQL) Design

## Overview

This document describes how Embedded SQL (`EXEC SQL ... END-EXEC`) support is wired into opensource COBOL 4J. For end-user usage, see [esql-guide.md](./esql-guide.md).

ESQL support is split into two layers:

1. **Compiler layer (`cobj/`)**: lexes/parses `EXEC SQL` blocks, lands the result on the same AST as ordinary COBOL, and emits Java code that passes host variables to the runtime as `AbstractCobolField` arguments.
2. **Runtime layer (`libcobj/.../sql/`)**: called from the generated Java; drives JDBC (PostgreSQL) and writes results back into COBOL storage.

## Compiler layer

### Key files

| File | Role |
|---|---|
| `cobj/pplex.l.m4` (preprocessor, generates `pplex.l`) | Rewrites `EXEC SQL INCLUDE <name>` to `COPY`, handles `BEGIN/END DECLARE SECTION`, and records whether `INCLUDE SQLCA` was seen (`cb_sqlca_include_seen`). Any other `EXEC SQL` text is passed through to the main scanner. |
| `cobj/scanner.l.m4` (main lexer, `ESQL_STATE`) | Accumulates the `EXEC SQL` … `END-EXEC` body into one string and emits it as a single `EXEC_SQL_STATEMENT` token (an alphanumeric literal). This is where the SQL block is captured as a single string. |
| `cobj/parser.y` (`exec_sql_statement`) | Receives `EXEC_SQL_STATEMENT` and calls `cb_parse_exec_sql()` to drive the ESQL-specific scanner/parser; runs `esql_inject_sqlca()` on the first embedded SQL statement. |
| `cobj/esql-scanner.l` | flex lexer. Tokenizes the SQL text between `EXEC SQL` and `END-EXEC`. |
| `cobj/esql-parser.y` | bison parser. Builds host-variable lists and a `cb_exec_sql` node. |
| `cobj/esql-common.h` | Shared types and prototypes between `esql-parser.y` / `esql-scanner.l` and `esql.c`. Deliberately does NOT include `tree.h` (its `YYSTYPE` would collide with the ESQL parser's). |
| `cobj/esql.c` | Provides `esql_build_and_resolve()` (host-var type resolution, GROUP expansion for SELECT INTO / FETCH OCCURS, and thin wrappers that build `cb_tree`s without exposing `tree.h` to the ESQL parser) and `esql_inject_sqlca()` (auto-injects `01 SQLCA GLOBAL.` on the first embedded SQL statement). |
| `cobj/codegen.c` (around `joutput_exec_sql`) | Expands a `cb_exec_sql` node into Java calls like `CobolEsql.exec(...)`. |

### Parsing pipeline

```
EXEC SQL ... END-EXEC (COBOL source)
        │
        │  preproc (cobj/pplex.l.m4): rewrites EXEC SQL INCLUDE <name> to COPY,
        │  handles BEGIN/END DECLARE SECTION, and records whether INCLUDE SQLCA
        │  was seen (cb_sqlca_include_seen). Any other EXEC SQL text is passed
        │  through unchanged to the main scanner.
        ▼
main scanner (cobj/scanner.l.m4, ESQL_STATE)
        │
        │  Accumulates the EXEC SQL ... END-EXEC body into one string and, at
        │  END-EXEC, emits it as a single EXEC_SQL_STATEMENT token (an
        │  alphanumeric literal). This is where the SQL block becomes one string.
        ▼
main parser (cobj/parser.y, exec_sql_statement)
        │
        │  Calls cb_parse_exec_sql(<literal string>) to drive the ESQL-specific
        │  scanner/parser below, and runs esql_inject_sqlca() once on the first
        │  embedded SQL statement.
        ▼
esql-scanner.l (flex)
        │
        │  Builds the SQL body, replacing host vars with '?'.
        │  Host vars, subscripts, and dotted qualifications are
        │  returned to the parser as ESQL_HOSTTOKEN / HOSTSUB_*.
        ▼
esql-parser.y (bison)
        │
        │  Assembles a cb_sql_host_var list, cursor name, and SQL
        │  body, then calls esql_build_and_resolve() in esql.c.
        ▼
esql.c
        │
        │  - resolve_host_var_type(): maps COBOL PIC / USAGE to
        │    HVARTYPE_*.
        │  - expand_group_host_vars(): if SELECT INTO got a GROUP,
        │    expands it into per-child bindings.
        │  - OCCURS detection: if the leaf has flag_occurs, promote
        │    to SELECT_INTO_OCCURS / FETCH_OCCURS.
        │  cb_build_exec_sql() returns the cb_exec_sql node.
        ▼
typeck.c / codegen.c
        │
        │  The cb_exec_sql node rides the same type-check / codegen
        │  path as any other COBOL statement. Each cb_reference in
        │  host_list / res_host_list goes through joutput_param,
        │  yielding AbstractCobolField arguments that include
        │  b_X.getSubDataStorage(...) for subscripted hosts.
        ▼
Java source (CobolEsql.exec / .selectInto / .fetchCursor / ...)
```

### Host variable AST representation

ESQL host variables introduce no new AST node type. They ride on the standard COBOL `cb_reference`.

| Source form | Internal representation |
|---|---|
| `:VAR` | `cb_reference{ word: VAR }` |
| `:VAR(IDX)` | `cb_reference{ word: VAR, subs: [IDX] }` |
| `:VAR(I, J)` | `cb_reference{ word: VAR, subs: [J, I] }` (leaf→root order) |
| `:GRP.SUB` | `cb_reference{ word: SUB, chain: cb_reference{ word: GRP } }` |
| `:GRP.SUB(IDX)` | above + `subs: [IDX]` |
| `:GRP.SUB(GRP2.IDX)` | above + `subs: [cb_reference{ word: IDX, chain: GRP2 }]` |

As a result, the existing `joutput_param` / `joutput_data` in `codegen.c` (specifically the OCCURS resolution loop inside `joutput_data`) produce correct `AbstractCobolField` arguments — including `b_X.getSubDataStorage(...)` — **without any ESQL-specific code path**.

### Scanner states

`esql-scanner.l` uses these exclusive states:

- `INITIAL` -- waiting for the first SQL keyword (`SELECT` / `INSERT` / `CONNECT` / ...).
- `ESQL_STATE` -- reading SQL body. Whitespace, comments, and tokens are appended to `esql_sqlbody`; host variables are replaced with `?`.
- `ESQL_HOSTSUB_STATE` -- pushed when a host token is immediately followed by `(`. Returns `(`, `)`, `,`, integer literals, and dotted identifiers as tokens **without writing them to `esql_sqlbody`**. Popped on `)`.
- `ESQL_DBNAME_STATE` -- captures `:NAME` after the `AT` keyword.
- `WHERE_CURRENT_OF` -- captures the cursor name after `WHERE CURRENT OF`.

The `HOSTWORD` regex includes dotted qualification (`("\."IDENT)*`). The `HOSTWORD/"("` trailing context decides whether to push `HOSTSUB_STATE`: it does so only when `(` follows directly.

### Subscript side channel `esql_pending_subs`

Subscript lists are passed between grammar actions via a static `cb_tree` variable, `esql_pending_subs`. The action that reduces `host_reference` stores the (reversed) subscript list there, and the very next parent rule's action — `esql_add_host_var()` — consumes and clears it. Because every `host_reference` reduction is followed immediately by exactly one consumer, no two host variables can collide. This is a small price paid to avoid widening the parser's `%union`.

### Building qualified references

`esql_build_qualified_ref()` in `cobj/esql.c` splits a dotted name like `"GRP.SUB.X"` with `strtok`, takes the rightmost segment as the leaf, and links the rest with `chain` (leaf → parent → grandparent). The resulting AST is identical in shape to plain COBOL `X OF SUB OF GRP`, so `cb_ref` resolves it through the same path.

### OCCURS handling

For `SELECT INTO` / `FETCH`, `esql_build_and_resolve()` checks whether the leaf host variable carries `flag_occurs`. If so, it promotes the command to `CB_SQL_SELECT_INTO_OCCURS` or `CB_SQL_FETCH_OCCURS` and records `occurs_size` (bytes per element) and `occurs_max` (element count) on the `cb_exec_sql` node. The runtime uses both numbers to scatter result rows into a contiguous COBOL array.

`expand_group_host_vars()` is responsible for the case where the caller passed a GROUP as a SELECT INTO target: the GROUP's children are expanded into individual host-variable bindings, which lets PostgreSQL see an N-column SELECT.

## Runtime layer

### Package layout (`jp.osscons.opensourcecobol.libcobj.sql`)

| Class | Visibility | Role |
|---|---|---|
| `CobolEsql` | `public` | The single public API called from generated Java. Provides `connect`, `disconnect`, `exec`, `execWithParams`, `execWhereCurrentOf`, `execWithParamsWhereCurrentOf`, `selectInto`, `selectIntoOccurs`, `declareCursor`, `declareCursorWithParams`, `openCursor`, `openCursorWithParams`, `fetchCursor`, `fetchCursorOccurs`, `closeCursor`, `prepare`, `executePrepared`, `commit`, `rollback`. `execWhereCurrentOf` / `execWithParamsWhereCurrentOf` are dedicated to statements containing `WHERE CURRENT OF`; they rewind the cursor position advanced by pre-reading before running. |
| `SqlState` | package-private | Internal state: connection table (`addConnection`/`getConnection`), prepared-statement table, cursor table. `clearCursors()` marks every cursor closed and discards its pre-read buffer; it is called on COMMIT / ROLLBACK. |
| `SqlConnection` | package-private | Wraps a JDBC `Connection`; parses connection strings of the form `dbname@host:port`; falls back to the `OCDB_DB_NAME` / `OCDB_DB_USER` / `OCDB_DB_PASS` environment variables when a value is empty; sets the connection encoding from `OCDB_DB_CHAR` (default `UTF-8`); strips each value's trailing spaces — the COBOL fixed-length padding — while preserving embedded spaces (`stripTrailingSpaces`); runs in autocommit mode with an explicit `BEGIN` per transaction (`beginTransaction`). |
| `SqlCursor` | package-private | Holds cursor state (open/closed), `ResultSet`, `PreparedStatement`. Also holds the pre-read (bulk fetch) buffer and the `overFetch` flag. A cursor may be DECLAREd from either an inline `SELECT` or a previously PREPAREd statement name. |
| `BulkFetchConfig` | package-private | Reads the pre-read count from the `OCESQL4J_FETCH_RECORDS` environment variable and caches it for the process. |
| `SqlCA` | package-private | Writes SQLCA fields (`SQLCODE`, `SQLSTATE`, `SQLERRMC`, `SQLERRD`, ...) back into the corresponding `CobolDataStorage`. Maps a JDBC `SQLState` to an ECPG code via `sqlStateToCode`. |
| `CobolDataConverter` | package-private | Converts between `AbstractCobolField` and JDBC `PreparedStatement.setXxx` / `ResultSet.getXxx`. Dispatches on the COBOL field type: numeric (display), packed decimal (COMP-3, signed/unsigned), native binary (COMP-5), float/double, alphanumeric / group, national (`PIC N`), and alphanumeric / Japanese `VARYING` (4-byte big-endian length header + data). National and Japanese values are converted through SHIFT-JIS. Note that native binary (COMP-5) and float/double are implemented only for the send direction (COBOL→SQL, `cobolToString`); the receive direction (SQL→COBOL, `stringToCobol`) has no dedicated write-back and falls back to copying the bytes as alphanumeric (correct binary write-back is not supported). |

All classes except `CobolEsql` are package-private. They appear as SLF4J logger names but are not part of the external API.

### CobolEsql signatures

Host variables arrive from generated Java as `AbstractCobolField[]`. Because the compiler has already resolved subscripts and qualifications, the runtime is **subscript- and qualification-agnostic**: it simply binds each array element to a JDBC parameter or result column via `CobolDataConverter`.

### Implicit SQLCA

`parser.y` calls `esql_inject_sqlca()` (in `cobj/esql.c`) to inject `01 SQLCA GLOBAL.` into the WORKING-STORAGE the first time an actual embedded SQL statement is seen, even without an explicit `EXEC SQL INCLUDE SQLCA`. Injection happens only for programs that contain a real `EXEC SQL` statement; a program with only `EXEC SQL INCLUDE SQLCA` or `BEGIN/END DECLARE SECTION` and no executable SQL gets no SQLCA. When an explicit `EXEC SQL INCLUDE SQLCA END-EXEC` is absent (tracked via `cb_sqlca_include_seen`, set during preprocessing in `pplex.l.m4`), `cobj` emits a compile-time warning. `SQLERRD OCCURS 6` becomes one of the fields that codegen wires up via `b_SQLERRD__SQLCA.getSubDataStorage(...)`.

### NULL column notification (`ECPG_MISSING_INDICATOR`)

Compatible with ECPG, the runtime returns `SQLCODE = -213` / `SQLSTATE = 22002` (`ECPG_MISSING_INDICATOR`) when a NULL column is read into a host variable without an indicator variable. In `SqlCA.java`, `ECPG_MISSING_INDICATOR = -213` and `setMissingIndicator()` sets the state to `22002`. The COBOL field itself is still written (zero-filled), so the row counts as processed. The JUnit suites `CobolEsqlTest` and `SqlCATest` cover the relevant cases.

### Bulk fetch (pre-read) and WHERE CURRENT OF position correction

`SqlCursor.fetch` pulls the number of rows given by the `OCESQL4J_FETCH_RECORDS` environment variable (cached by `BulkFetchConfig`, default 1) in a single `FETCH FORWARD N FROM <cursor>` and keeps them in `fetchBuffer`. Subsequent `fetchCursor` calls serve one buffered row at a time without hitting the database until the buffer is exhausted, at which point the next N rows are pre-read. This collapses N COBOL FETCHes into one database round trip. With the default value of 1, rows are fetched one at a time as before. The buffer is cleared on COMMIT / ROLLBACK / CLOSE (`clearBuffer`).

Because pre-reading advances the server-side cursor past the actual current row, positioned UPDATE/DELETE using `WHERE CURRENT OF` would target the wrong row. To correct this, `SqlCursor` records the over-advanced state in the `overFetch` flag, and `CobolEsql.execWhereCurrentOf` / `execWithParamsWhereCurrentOf` rewind the cursor with `FETCH BACKWARD` before issuing the SQL, then invalidate the pre-read buffer (same behavior as Open COBOL ESQL 4J's overFetch correction). `joutput_exec_sql` in `codegen.c` dispatches statements containing `WHERE CURRENT OF` to these dedicated APIs.

### Transaction model

`SqlConnection.connect` puts the JDBC connection into `setAutoCommit(true)` and then issues an explicit `BEGIN`. After every `COMMIT`, `ROLLBACK`, and on `DISCONNECT`, the runtime calls `SqlState.clearCursors()` (which closes all cursors and discards their pre-read buffers, since the server-side portals no longer exist) and `SqlConnection.beginTransaction()` to open the next transaction. This keeps a transaction always active between commits, matching ECPG semantics where embedded statements run inside a transaction block.

### Error handling on statement failure

The runtime keeps a transaction open (see *Transaction model* above) and runs each embedded statement inside it. When a statement fails, the runtime records the error into the SQLCA (`SQLCODE` / `SQLSTATE` / `SQLERRMC`) and leaves the transaction in PostgreSQL's aborted state. As in ECPG / PostgreSQL, once a transaction is aborted every subsequent statement is rejected with SQLSTATE `25P02` (`in_failed_sql_transaction`) until the program issues `ROLLBACK` (or `COMMIT`); recovering from an error is therefore the COBOL program's responsibility.

For parameterized statements, the JDBC Describe (`getParameterMetaData`) runs in the same `try` block as `execute()`, so a Describe failure (for example, a missing table) propagates directly to the error handler and the SQLCA reports the real error (e.g. `42P01`).

### Prepared-statement cache (`stmtCache`)

`CobolEsql` keeps `PreparedStatement`s in a nested `ConcurrentHashMap` (`stmtCache`) keyed by `Connection` and, within that, by the SQL string. Connections are matched by object identity and queries by exact string equality, so there is no risk of a hash collision returning the wrong statement. `getOrCreatePreparedStatement` reuses a cached `PreparedStatement` for a repeated `(connection, query)` pair instead of re-preparing it on every `execWithParams` / `selectInto`.

### Cursor name qualification

Cursor names are program-qualified as `<program-id>_<cursor>`: `codegen.c` emits every cursor API call with the format `"%s_%s"` (`excp_current_program_id`, `cursor_name`), so two programs declaring a cursor of the same name do not collide on the shared server connection. The runtime stores and looks up cursors under this qualified name in `SqlState`.

For `WHERE CURRENT OF`, codegen does **not** put the cursor name into the SQL text. `repositionForCurrentOf` appends it at runtime (`query + " " + cursorName`) after rewinding the cursor, so the final statement targets the correct, fully qualified portal.

### FETCH ... INTO OCCURS bypasses the pre-read buffer

`fetchCursorOccurs` does not use the single-row pre-read buffer. It calls `clearBuffer()` first (to avoid a mismatch with the server-side cursor position left by any prior single-row pre-read) and then issues `FETCH FORWARD <occursMax>` directly, scattering the rows into the contiguous OCCURS storage and reporting the row count in `SQLERRD(3)`.

### Error mapping

On a JDBC `SQLException`, `SqlCA.setResultFromException` maps the exception's `SQLState` to an ECPG `SQLCODE` via `SqlCA.sqlStateToCode` and stores `e.getMessage()` into `SQLERRMC` (truncated to 70 bytes). Notable mappings: `02000` → `+100` (`ECPG_NOT_FOUND`), `08001`/`08003` → `-402` (`ECPG_CONNECT`), `34000` → `-602`, `YE002` → `-212` (`ECPG_EMPTY`); any unrecognized state becomes `-9999` (`ECPG_UNKNOWN_ERROR`).

Cursor anomaly handling is decided in the runtime rather than coming from PostgreSQL: OPEN / FETCH / CLOSE on an unregistered cursor returns `-602` / `34000`; CLOSE on a registered-but-unopened cursor returns success; and `WHERE CURRENT OF` against an unregistered cursor returns `ECPG_EMPTY` / `YE002`. A FETCH on a registered-but-unopened cursor (including one whose OPEN failed) is still sent to PostgreSQL: if the transaction is healthy it comes back as `cursor "..." does not exist`, and if a failed OPEN left the transaction aborted it comes back as `25P02`. Recovering (issuing `ROLLBACK`) is the COBOL program's responsibility.

## Tests

### Compiler / integration tests (`tests/`)

Autotest suites that drive a PostgreSQL container live under:

| Directory | Coverage |
|---|---|
| `tests/esql-basic.src/` | CONNECT / DISCONNECT / basic INSERT / SELECT |
| `tests/esql-cobol-data.src/` | COBOL data type × SQL type round-trips (numeric, packed decimal, alphanumeric, Japanese, VARYING, subscripted host vars) |
| `tests/esql-sql-data.src/` | SQL-side type variations |
| `tests/esql-sqlca.src/` | SQLCA field assertions |
| `tests/esql-misc.src/` | Cursors / PREPARE / EXECUTE etc. |
| `tests/esql-utf8.src/` | UTF-8 build variants |

Each suite is generated by `make <name>` and runnable locally with `./<name>` (requires a PostgreSQL container). CI drives them through `.github/workflows/test-esql.yml`.

### libcobj unit tests (`libcobj/app/src/test/.../sql/`)

`CobolEsqlTest`, `SqlStateTest`, `SqlCursorTest`, `SqlConnectionTest`, `CobolDataConverterTest`, `CobolDataConverterPureTest`, `CobolEsqlLoggingTest`, `SqlCATest` provide unit coverage. Those needing a database use the testcontainers PostgreSQL image, while others — such as `CobolDataConverterPureTest` — verify data conversion without a database.

## Design decisions

- **No ESQL-specific AST node**: host variables ride directly on `cb_reference->subs` / `cb_reference->chain`, so multi-dimensional subscripts, group qualification, and qualified subscripts all flow through the existing OCCURS resolution loop in `codegen.c`.
- **State-machine isolation for subscripts**: subscript and qualification parsing is contained in `ESQL_HOSTSUB_STATE`, which by construction never appends to the SQL body string. There is no way for a stray `(` or identifier to leak into a placeholder slot.
- **Drop dblibj**: the Open-COBOL-ESQL Scala dependency (dblibj) is removed; the runtime is pure Java, bundled inside `libcobj.jar` together with the PostgreSQL JDBC driver.
- **No `OF` qualification**: COBOL itself accepts `X OF Y`, but ESQL only accepts dotted qualification (`:Y.X`). This keeps the scanner state count small and avoids reserved-word collisions with `OF` inside SQL clauses.
- **Wrap host-variable lists in the generated Java**: the host variables passed to `CobolEsql.*` calls are emitted by `joutput_sql_host_list_newline` (argument lists) and `joutput_sql_field_array` (`new AbstractCobolField[]{...}` literals) in `codegen.c`. Both insert a line break every `SQL_HOST_VAR_WRAP` (= 5) host variables instead of putting them all on one line, so a statement with many host variables stays readable in the generated source. This is purely cosmetic and does not change the arguments passed.

## Related documents

- [esql-guide.md](./esql-guide.md) -- end-user usage (including SLF4J runtime logging configuration)
- [esql-design_JP.md](./esql-design_JP.md) -- Japanese version of this document
