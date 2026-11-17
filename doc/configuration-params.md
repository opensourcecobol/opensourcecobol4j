# Configuration Parameters Reference

This document describes the parameters you can set in an opensource COBOL 4J
compiler configuration file, and how to pick a configuration file when
compiling.

## Configuration files

When opensource COBOL 4J is installed, the default configuration file is placed
at `/usr/local/share/opensource-cobol-4j-<version>/config/default.conf` and is
loaded automatically at compile time. You can use a different file with the
`-conf=<file>` option, or pick one of the built-in dialect files with
`-std=<name>`, which loads `config/<name>.conf`.

The repository ships these files under `config/`:

- `boundary-limit.conf`
- `bs2000.conf`
- `cobol85.conf`
- `cobol2002.conf`
- `default-en.conf`
- `default-jp.conf`
- `ibm.conf`
- `jp-compat.conf`
- `mf.conf`
- `mvs.conf`

Most of them define the same parameters as `default.conf` with different
values; each file corresponds to the dialect in its name. A handful of
parameters, such as `not-reserved`, only appear in some files (`mvs.conf`, for
example). The default values in the table below come from `default.conf`.

A dialect file usually starts with `include "default.conf"` and then overrides
only the parameters it cares about. For example `ibm.conf` inherits everything
from `default.conf` and then sets `assign-clause: ibm`,
`binary-size: 2-4-8`, `perform-osvs: yes`, and so on.

## Syntax support levels

Some parameters take one of these values, which control whether the compiler
accepts, warns about, or rejects a particular syntax construct:

| Level          | Behaviour (where `xxx` is the construct and `name` is the `name` value) |
|----------------|-------------------------------------------------------------------------|
| `ok`           | accepted silently                                                        |
| `archaic`      | warning: `xxx is archaic in name`                                        |
| `obsolete`     | warning: `xxx is obsolete in name`                                       |
| `skip`         | accepted silently                                                        |
| `ignore`       | warning: `xxx ignored`                                                   |
| `unconformable`| error: `xxx does not conform to name`                                    |
| `error`        | syntax error                                                             |

## Parameter list

| No | Parameter | Values | Default | Description | Notes |
|----|-----------|--------|---------|-------------|-------|
| 1  | `name` | any string | `OpenCOBOL` | Name of the configuration | |
| 2  | `tab-width` | integer | `8` | Number of spaces a tab character expands to | unstable |
| 3  | `text-column` | integer | `72` | Last source column that is read | |
| 4  | `default-organization` | `record-sequential`, `line-sequential` | `record-sequential` | Organization used when `SELECT` does not specify `ORGANIZATION` | |
| 5  | `assign-clause` | `cobol2002`, `mf`, `ibm`, `jph1` | `mf` | Which `ASSIGN` clause syntax is accepted in `SELECT` | unstable |
| 6  | `filename-mapping` | `yes`, `no` | `yes` | Resolve file names from environment variables at run time | |
| 7  | `pretty-display` | `yes`, `no` | `yes` | Display format for `USAGE BINARY` data items | not implemented |
| 8  | `auto-initialize` | `yes`, `no` | `yes` | Initialize `WORKING-STORAGE` items even when no `VALUE` clause is given | |
| 9  | `complex-odo` | `yes`, `no` | `no` | Allow `OCCURS ... DEPENDING ON` inside a group item (not only first/last) | |
| 10 | `indirect-redefines` | `yes`, `no` | `no` | Allow multi-level `REDEFINES` | |
| 11 | `binary-size` | `2-4-8`, `1-2-4-8`, `1--8` | `1-2-4-8` | Byte allocation for `PIC 9(n) BINARY` | |
| 12 | `binary-truncate` | `yes`, `no` | `yes` | Raise an exception when an overflow causes truncation | unstable |
| 13 | `binary-byteorder` | `native`, `big-endian` | `big-endian` | Byte order for binary items | Java defaults to big-endian |
| 14 | `abort-on-io-exception` | `any`, `fatal`, `never` | `any` | Error handling level for file operations; affects the generated Java | |
| 15 | `larger-redefines-ok` | `yes`, `no` | `no` | Allow a `REDEFINES` item to be larger than the item it redefines | |
| 16 | `relaxed-syntax-check` | `yes`, `no` | `no` | Allow MF-style syntax: omitting `INPUT-OUTPUT SECTION`, omitting `FILE SECTION`, and `PIC X(n) REDEFINES` | |
| 17 | `perform-osvs` | `yes`, `no` | `no` | Enable the IBM OS/VS COBOL dialect (different `PERFORM` behaviour) | not implemented |
| 18 | `sticky-linkage` | `yes`, `no` | `no` | Keep `LINKAGE SECTION` data between `CALL`s instead of reinitializing | not implemented |
| 19 | `assign_external` | `yes`, `no` | `no` | Treat every `ASSIGN` clause as if `EXTERNAL` was specified | same as the `-assign_external` compile option |
| 20 | `relax-level-hierarchy` | `yes`, `no` | `no` | Allow level numbers that do not increase consistently | |
| 21 | `author-paragraph` | support level | `obsolete` | Whether the `AUTHOR` paragraph is accepted | |
| 22 | `memory-size-clause` | support level | `obsolete` | Whether `MEMORY SIZE` in `OBJECT-COMPUTER` is accepted | |
| 23 | `multiple-file-tape-clause` | support level | `obsolete` | Whether `MULTIPLE FILE TAPE` in `I-O CONTROL` is accepted | |
| 24 | `label-records-clause` | support level | `obsolete` | Whether `LABEL RECORDS` in an `FD` entry is accepted | |
| 25 | `value-of-clause` | support level | `obsolete` | Whether `VALUE OF {WORD}` in an `FD` entry is accepted | |
| 26 | `data-records-clause` | support level | `obsolete` | Whether `DATA RECORD` in an `FD` entry is accepted | |
| 27 | `top-level-occurs-clause` | support level | `skip` | Whether `OCCURS` on 01 and 77 level items is accepted | |
| 28 | `synchronized-clause` | support level | `ok` | Whether `SYNCHRONIZED` in a data definition is accepted | |
| 29 | `goto-statement-without-name` | support level | `obsolete` | Whether a `GO TO` with no target is accepted | |
| 30 | `stop-literal-statement` | support level | `obsolete` | Whether `STOP LITERAL` is accepted | not implemented |
| 31 | `debugging-line` | support level | `obsolete` | Whether `WITH DEBUGGING MODE` in `SOURCE-COMPUTER` is accepted | |
| 32 | `padding-character-clause` | support level | `obsolete` | Whether `PADDING CHARACTER` in `SELECT` is accepted | |
| 33 | `next-sentence-phrase` | support level | `archaic` | Whether `NEXT SENTENCE` is accepted | |
| 34 | `eject-statement` | support level | `skip` | Whether `EJECT` is accepted | not implemented |
| 35 | `entry-statement` | support level | `obsolete` | Whether `ENTRY` is accepted | not implemented |
| 36 | `move-noninteger-to-alphanumeric` | support level | `error` | Whether moving a non-integer (e.g. decimal) to an alphanumeric item is accepted | warning when `ok`, none when `error` |
| 37 | `odo-without-to` | support level | `ok` | Whether `OCCURS ... DEPENDING ON` without a `TO` maximum raises an error | |
| 38 | `default-currency-symbol` | any single character | `$` | Default currency symbol | |
| 39 | `max-alpha-character-data-size` | integer | `2147483647` | Maximum size for `PIC X` | |
| 40 | `max-sjis-character-data-size` | integer | `1073741823` | Maximum size for `PIC N` (Shift-JIS) | |
| 41 | `max-utf8-character-data-size` | integer | `715827882` | Maximum size for `PIC N` (UTF-8) | |
| 42 | `c89-identifier-length-check` | `yes`, `no` | `no` | Warn when a `PROGRAM-ID` exceeds 31 characters | unstable |
| 43 | `allow-end-program-with-wrong-name` | `yes`, `no` | `no` | Allow `END PROGRAM` to name a different program than `PROGRAM-ID` | |
| 44 | `allow-missing-also-clause-in-evaluate` | `yes`, `no` | `no` | Allow omitting `ALSO` in an `EVALUATE` condition | |
| 45 | `allow-empty-imperative-statement` | `yes`, `no` | `no` | Allow an empty imperative statement in `IF`/`EVALUATE` | |
| 46 | `enable-program-status-register` | `yes`, `no` | `no` | Enable the `PROGRAM-STATUS` special register | |
| 47 | `enable-sort-status-register` | `yes`, `no` | `no` | Enable the `SORT-STATUS` special register | |
| 48 | `enable-special-names-argument-clause` | `yes`, `no` | `no` | Enable `ARGUMENT-NUMBER` and `ARGUMENT-VALUE` in `SPECIAL-NAMES` | |
| 49 | `enable-special-names-environment-clause` | `yes`, `no` | `no` | Enable `ENVIRONMENT-NAME` and `ENVIRONMENT-VALUE` in `SPECIAL-NAMES` | |
| 50 | `enable-leng-intrinsic-function` | `yes`, `no` | `no` | Enable the `FUNCTION LENG` intrinsic function | |
| 51 | `enable-length-an-intrinsic-function` | `yes`, `no` | `no` | Enable the `FUNCTION LENGTH-AN` intrinsic function | |
| 52 | `enable-national-intrinsic-function` | `yes`, `no` | `no` | Enable the `FUNCTION NATIONAL` intrinsic function | |
| 53 | `use-invalidkey-handler-on-status34` | `yes`, `no` | `no` | Enable `INVALID KEY` on `READ`/`WRITE` of SAM files | |
| 54 | `cobol68-copy-in-data-description` | `yes`, `no` | `no` | Allow COBOL68-style `COPY` in `DATA DIVISION` | |
| 55 | `switch-no-mnemonic` | `yes`, `no` | `no` | Allow referencing a `SPECIAL-NAMES` switch without its mnemonic name | |
| 56 | `allow-is-in-sort-key-spec` | `yes`, `no` | `no` | Allow `KEY IS` in a `SORT` key specification | |
| 57 | `allow-search-key-in-rhs` | `yes`, `no` | `no` | Allow a key item on the right-hand side of a `SEARCH ALL` condition | |
| 58 | `ignore-invalid-record-contains` | `yes`, `no` | `no` | Ignore the record length limit from `RECORD CONTAINS` in an `FD` entry | |
| 59 | `enable-zero-division-error` | `yes`, `no` | `no` | Raise a compile error when a program can divide by zero | |
| 60 | `enable-check-subscript-out-of-bounds` | `yes`, `no` | `no` | Raise a compile error when a subscript can exceed the `OCCURS ... DEPENDING ON` size | |
| 61 | `enable-expect-numeric-error` | `yes`, `no` | `no` | Raise a compile error when moving an alphanumeric item to a numeric item | |
| 62 | `enable-expect-compute-string-error` | `yes`, `no` | `no` | Raise an error when arithmetic mixes strings and numerics | unstable |

## Other parameters

Two parameters are handled separately from the table above:

- `include "file"` — loads another configuration file. Dialect files use this
  to inherit from `default.conf`.
- `not-reserved` — removes a word from the reserved word list (case
  insensitive). It appears in some dialect files, such as `mvs.conf`.
