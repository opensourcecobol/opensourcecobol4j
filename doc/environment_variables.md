# Environment Variables Reference

This document describes the environment variables used by opensource COBOL 4J.

## Compiler Settings

Under construction.

## Runtime Settings

### General Settings

#### COB_DATE

Fixes the date returned at runtime.

- **Format**: `YYYY/MM/DD`
- **Example**: `COB_DATE=2024/01/15`
- **Purpose**: Fixes the date returned by built-in functions such as `CURRENT-DATE`. Useful for testing and reproducible execution.

**Sample Program**

This program displays the date obtained via both `ACCEPT FROM DATE YYYYMMDD` and `FUNCTION CURRENT-DATE`. When `COB_DATE` is set, both methods return the fixed date.

```cobol
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      cobdate.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  DATE-YMD.
         03  DATE-YYYY PIC X(4).
         03  DATE-MM   PIC X(2).
         03  DATE-DD   PIC X(2).
       01  CURR-DATE.
         03  CURR-YYYY PIC X(4).
         03  CURR-MM   PIC X(2).
         03  CURR-DD   PIC X(2).
         03  FILLER    PIC X(13).
       PROCEDURE        DIVISION.
           ACCEPT DATE-YMD FROM DATE YYYYMMDD.
           DISPLAY "ACCEPT FROM DATE: " DATE-YMD.
           MOVE FUNCTION CURRENT-DATE TO CURR-DATE.
           DISPLAY "CURRENT-DATE:     "
               CURR-YYYY CURR-MM CURR-DD.
           STOP RUN.
```

Example run:

```bash
$ cobj cobdate.cbl

# Without the environment variable (today's date is displayed)
$ java cobdate
ACCEPT FROM DATE: 20260227
CURRENT-DATE:     20260227

# With COB_DATE set (date is fixed)
$ COB_DATE=1970/01/02 java cobdate
ACCEPT FROM DATE: 19700102
CURRENT-DATE:     19700102
```

#### COB_VERBOSE

Controls whether detailed debug information is output.

- **Value**: Enabled by `Y` or `y`
- **Example**: `COB_VERBOSE=Y`
- **Purpose**: Use when you want to check runtime behavior in detail.

**Sample Program**

This program performs a SORT operation. When `COB_VERBOSE=Y` is set, the record count and other information are displayed to stdout upon SORT completion.

```cobol
       IDENTIFICATION       DIVISION.
       PROGRAM-ID.          cobverbose.
       ENVIRONMENT          DIVISION.
       INPUT-OUTPUT         SECTION.
       FILE-CONTROL.
           SELECT SORT-FILE  ASSIGN TO "S01"
               ORGANIZATION SEQUENTIAL.
           SELECT INPUT-FILE  ASSIGN TO "./sort-input.txt".
           SELECT OUTPUT-FILE ASSIGN TO "./sort-output.txt".
       DATA                 DIVISION.
       FILE                 SECTION.
       SD SORT-FILE.
       01 SORT-REC.
          02 SORT-KEY  PIC XX.
          02 SORT-DATA PIC XX.
       FD INPUT-FILE.
       01 INPUT-REC.
          02 XFLD1   PIC X(2).
          02 XFLD2   PIC X(2).
       FD OUTPUT-FILE.
       01 OUTPUT-REC.
          02 XFLD1   PIC X(2).
          02 XFLD2   PIC X(2).
       PROCEDURE            DIVISION.
          SORT SORT-FILE ON ASCENDING KEY SORT-KEY
             USING INPUT-FILE
             GIVING OUTPUT-FILE.
          STOP RUN.
```

Example run:

```bash
$ cobj cobverbose.cbl

# Create input file (3 records x 4 bytes: 2-byte key + 2-byte data)
$ printf "11CC33AA22BB" > sort-input.txt

# Without the environment variable (no message)
$ java cobverbose

# With COB_VERBOSE=Y (SORT completion message is displayed)
$ COB_VERBOSE=Y java cobverbose
libcobj: END OF SORT/MERGE, RECORD=3.
```

#### COB_TERMINAL_ENCODING

Specifies the terminal encoding used by DISPLAY and ACCEPT statements.

- **Value**: `UTF-8` or `UTF8` (default is Shift_JIS)
- **Example**: `COB_TERMINAL_ENCODING=UTF-8`
- **Purpose**: Used for terminal I/O in UTF-8 environments.

#### COB_SWITCH_1 through COB_SWITCH_8

Sets the initial values of switches that can be referenced within COBOL programs.

- **Value**: Enabled by `ON` (any other value disables)
- **Example**: `COB_SWITCH_1=ON`
- **Purpose**: Used as flags to switch program behavior.

**Sample Program**

This program checks the state of SWITCH-1 and SWITCH-2 defined in SPECIAL-NAMES using IF statements and displays the result. The initial values of each switch can be set to ON/OFF via `COB_SWITCH_1` and `COB_SWITCH_2`.

```cobol
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      cobswitch.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           SWITCH-1 IS SW1
             ON STATUS IS SWIT1-ON
             OFF STATUS IS SWIT1-OFF
           SWITCH-2 IS SW2
             ON STATUS IS SWIT2-ON
             OFF STATUS IS SWIT2-OFF.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       PROCEDURE        DIVISION.
           DISPLAY "SWITCH-1: " NO ADVANCING.
           IF SWIT1-ON
              DISPLAY "ON"
           ELSE
              DISPLAY "OFF"
           END-IF.
           DISPLAY "SWITCH-2: " NO ADVANCING.
           IF SWIT2-ON
              DISPLAY "ON"
           ELSE
              DISPLAY "OFF"
           END-IF.
           STOP RUN.
```

Example run:

```bash
$ cobj cobswitch.cbl

$ COB_SWITCH_1=ON COB_SWITCH_2=OFF java cobswitch
SWITCH-1: ON
SWITCH-2: OFF

$ COB_SWITCH_1=OFF COB_SWITCH_2=ON java cobswitch
SWITCH-1: OFF
SWITCH-2: ON
```

### File I/O

#### COB_FILE_PATH

Specifies the default directory path for file searches.

- **Example**: `COB_FILE_PATH=/data/cobol/files`
- **Purpose**: Files specified in SELECT clauses are searched relative to this path.

**Sample Program**

This program writes to a file defined as `SELECT TEST-FILE ASSIGN "PATHTEST"`. When `COB_FILE_PATH` is set, the file creation directory changes.

```cobol
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      cobfilepath.
       ENVIRONMENT      DIVISION.
       INPUT-OUTPUT     SECTION.
       FILE-CONTROL.
       SELECT TEST-FILE ASSIGN "PATHTEST".
       DATA             DIVISION.
       FILE             SECTION.
       FD TEST-FILE.
       01 TEST-REC      PIC X(4).
       PROCEDURE        DIVISION.
           OPEN OUTPUT TEST-FILE.
           MOVE "DATA" TO TEST-REC.
           WRITE TEST-REC.
           CLOSE TEST-FILE.
           DISPLAY "File created successfully.".
           STOP RUN.
```

Example run:

```bash
$ cobj cobfilepath.cbl

# Without the environment variable (PATHTEST is created in the current directory)
$ java cobfilepath
File created successfully.
$ ls PATHTEST
PATHTEST

# With COB_FILE_PATH set (subdir/PATHTEST is created)
$ mkdir -p subdir
$ COB_FILE_PATH=subdir java cobfilepath
File created successfully.
$ ls subdir/PATHTEST
subdir/PATHTEST
```

#### DD_filename / dd_filename

Specifies the actual file path for a specific file.

- **Format**: `DD_<filename>=<actual path>` or `dd_<filename>=<actual path>`
- **Example**: `DD_MASTER=/data/master.dat`
- **Purpose**: Maps file names in COBOL programs to different paths at runtime.

**Sample Program**

This program defines a file as `SELECT TEST-FILE ASSIGN "MYDATA"`. When the `DD_MYDATA` environment variable is set, the logical name `MYDATA` is mapped to the specified path.

```cobol
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      ddfilename.
       ENVIRONMENT      DIVISION.
       INPUT-OUTPUT     SECTION.
       FILE-CONTROL.
       SELECT TEST-FILE ASSIGN "MYDATA".
       DATA             DIVISION.
       FILE             SECTION.
       FD TEST-FILE.
       01 TEST-REC      PIC X(4).
       PROCEDURE        DIVISION.
           OPEN OUTPUT TEST-FILE.
           MOVE "DATA" TO TEST-REC.
           WRITE TEST-REC.
           CLOSE TEST-FILE.
           DISPLAY "File created successfully.".
           STOP RUN.
```

Example run:

```bash
$ cobj ddfilename.cbl

# Without the environment variable (a file named MYDATA is created)
$ java ddfilename
File created successfully.
$ ls MYDATA
MYDATA

# With DD_MYDATA set (mapped to actual-output.dat)
$ DD_MYDATA=actual-output.dat java ddfilename
File created successfully.
$ ls actual-output.dat
actual-output.dat
```

#### COB_SYNC

Controls whether a flush (writing out the buffer) is performed immediately after a successful WRITE, REWRITE, or DELETE statement.

- **Value**: Enabled by `Y` or `y` (performs flush), also enabled by `P` or `p`
- **Example**: `COB_SYNC=Y`
- **Purpose**: By flushing the buffer to disk after each write operation, data loss due to abnormal termination can be reduced. However, performance decreases because a flush occurs on every operation.

#### COB_LS_NULLS

Under preparation.

#### COB_LS_FIXED

Controls whether line sequential files are treated as fixed-length.

- **Value**: Enabled by `Y` or `y`
- **Purpose**: Specifies how record length is handled for line sequential files.

**Sample Program**

This program writes `"AB"` to a `PIC X(20)` record in an `ORGANIZATION LINE SEQUENTIAL` file. Normally, trailing spaces are trimmed, but when `COB_LS_FIXED=Y` is set, the record is written at fixed length (20 bytes + newline).

```cobol
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      coblsfixed.
       ENVIRONMENT      DIVISION.
       INPUT-OUTPUT     SECTION.
       FILE-CONTROL.
       SELECT TEST-FILE ASSIGN       "./ls-test-output.txt"
                        ORGANIZATION LINE SEQUENTIAL.
       DATA             DIVISION.
       FILE             SECTION.
       FD TEST-FILE.
       01 TEST-REC      PIC X(20).
       PROCEDURE        DIVISION.
           OPEN OUTPUT TEST-FILE.
           MOVE "AB" TO TEST-REC.
           WRITE TEST-REC.
           CLOSE TEST-FILE.
           DISPLAY "File written.".
           STOP RUN.
```

Example run:

```bash
$ cobj coblsfixed.cbl

# Without the environment variable (trailing spaces trimmed, "AB" + newline = 3 bytes)
$ java coblsfixed
File written.
$ wc -c < ls-test-output.txt
3

# With COB_LS_FIXED=Y (fixed length 20 bytes + newline = 21 bytes)
$ COB_LS_FIXED=Y java coblsfixed
File written.
$ wc -c < ls-test-output.txt
21
```

#### OC_IO_CREATES

Controls whether files are automatically created when opening in I-O mode if the file does not exist.

- **Value**: Enabled by `yes`
- **Example**: `OC_IO_CREATES=yes`
- **Purpose**: Controls behavior when a file does not exist during OPEN I-O.

**Sample Program**

This program executes `OPEN I-O` on a non-existent file and checks the FILE STATUS. Normally the status is `35` (file not found), but when `OC_IO_CREATES=yes` is set, the file is automatically created and the status becomes `00`.

```cobol
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      ociocreates.
       ENVIRONMENT      DIVISION.
       INPUT-OUTPUT     SECTION.
       FILE-CONTROL.
       SELECT TEST-FILE ASSIGN       "./TEST-IO-FILE"
                        ORGANIZATION SEQUENTIAL
                        FILE STATUS  F-STATUS.
       DATA             DIVISION.
       FILE             SECTION.
       FD TEST-FILE.
       01 TEST-RECORD.
         03 TEST-KEY    PIC 9(10).
       WORKING-STORAGE  SECTION.
       77 F-STATUS      PIC X(02).
       PROCEDURE DIVISION.
           OPEN I-O TEST-FILE.
           DISPLAY "FILE STATUS: " F-STATUS.
           CLOSE TEST-FILE.
           STOP RUN.
```

Example run:

```bash
$ cobj ociocreates.cbl

# Without the environment variable (file not found, status 35)
$ java ociocreates
FILE STATUS: 35

# With OC_IO_CREATES=yes (file auto-created, status 00)
$ OC_IO_CREATES=yes java ociocreates
FILE STATUS: 00
```

#### OC_EXTEND_CREATES

Controls whether files are automatically created when opening in EXTEND mode if the file does not exist.

- **Value**: Enabled by `yes`
- **Example**: `OC_EXTEND_CREATES=yes`
- **Purpose**: Controls behavior when a file does not exist during OPEN EXTEND.

**Sample Program**

This program executes `OPEN EXTEND` on a non-existent file and checks the FILE STATUS. Like OC_IO_CREATES, when `OC_EXTEND_CREATES=yes` is set, the file is automatically created.

```cobol
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      ocextcreates.
       ENVIRONMENT      DIVISION.
       INPUT-OUTPUT     SECTION.
       FILE-CONTROL.
       SELECT TEST-FILE ASSIGN       "./TEST-EXT-FILE"
                        ORGANIZATION SEQUENTIAL
                        FILE STATUS  F-STATUS.
       DATA             DIVISION.
       FILE             SECTION.
       FD TEST-FILE.
       01 TEST-RECORD.
         03 TEST-KEY    PIC 9(10).
       WORKING-STORAGE  SECTION.
       77 F-STATUS      PIC X(02).
       PROCEDURE DIVISION.
           OPEN EXTEND TEST-FILE.
           DISPLAY "FILE STATUS: " F-STATUS.
           CLOSE TEST-FILE.
           STOP RUN.
```

Example run:

```bash
$ cobj ocextcreates.cbl

# Without the environment variable (file not found, status 35)
$ java ocextcreates
FILE STATUS: 35

# With OC_EXTEND_CREATES=yes (file auto-created, status 00)
$ OC_EXTEND_CREATES=yes java ocextcreates
FILE STATUS: 00
```

#### COB_FILE_SEQ_WRITE_BUFFER_SIZE

Specifies the write buffer size for sequential files.

- **Value**: Integer >= 0 (default: 10)
- **Example**: `COB_FILE_SEQ_WRITE_BUFFER_SIZE=100`
- **Purpose**: Adjusts write performance.

#### COB_IO_ASSUME_REWRITE

Controls whether READ is required before REWRITE.

- **Value**: Enabled by `Y` or `y`
- **Example**: `COB_IO_ASSUME_REWRITE=Y`
- **Purpose**: Use when allowing REWRITE without a preceding READ.

**Sample Program**

This program opens a file containing 3 records (4 bytes each) with `OPEN I-O`, READs to the 2nd record, then executes `WRITE`. When `COB_IO_ASSUME_REWRITE=Y` is set, `WRITE` acts as `REWRITE` and the 2nd record is overwritten.

```cobol
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      cobrewrite.
       ENVIRONMENT      DIVISION.
       INPUT-OUTPUT     SECTION.
       FILE-CONTROL.
       SELECT TEST-FILE ASSIGN "TEST-REWRITE".
       DATA             DIVISION.
       FILE             SECTION.
       FD TEST-FILE.
       01 TEST-REC      PIC X(4).
       PROCEDURE        DIVISION.
           OPEN I-O TEST-FILE.
           READ  TEST-FILE.
           READ  TEST-FILE.
           MOVE  "AAAA" TO TEST-REC.
           WRITE TEST-REC.
           CLOSE TEST-FILE.
           STOP RUN.
```

Example run:

```bash
$ cobj cobrewrite.cbl

# Create input file (3 records x 4 bytes: "0000", "1111", "2222")
$ printf "000011112222" > TEST-REWRITE

# Run with COB_IO_ASSUME_REWRITE=Y
$ COB_IO_ASSUME_REWRITE=Y java cobrewrite

# The 2nd record has been overwritten with "AAAA"
$ cat TEST-REWRITE
0000AAAA2222
```

### Temporary Files

#### TMPDIR / TMP / TEMP

Specifies the directory for temporary files used by sort operations and similar processes.

- **Priority**: `TMPDIR` > `TMP` > `TEMP`
- **Example**: `TMPDIR=/tmp/cobol`
- **Purpose**: Specifies the location for sort and temporary file creation.

### Program Calls

#### COB_LOAD_CASE

Under preparation.

#### COB_LIBRARY_PATH

An environment variable intended to specify search paths for programs called by CALL statements.

- **Note**: In the current implementation, the value is stored in an internal variable but is not referenced during program resolution, so it has no effect on behavior.

#### COB_PACKAGE_PATH

Specifies the package path for Java classes called by CALL statements.

- **Example**: `COB_PACKAGE_PATH=com.example.cobol`
- **Purpose**: Used for program searching within Java package structures.

#### COB_PRE_LOAD

Specifies modules to pre-load at program start.

- **Note**: Currently not implemented.

### Numeric Processing

#### COB_NIBBLE_C_UNSIGNED

Controls nibble value processing for unsigned packed-decimal numbers.

- **Value**: Enabled by `Y` or `y`
- **Example**: `COB_NIBBLE_C_UNSIGNED=Y`
- **Purpose**: Used for compatibility with specific COBOL dialects.

**Sample Program**

This program uses a REDEFINES of `S9(2) COMP-3` (signed) and `9(2) COMP-3` (unsigned) to display the results of `IS NUMERIC` tests. Normally only the signed field is judged as NUMERIC, but when `COB_NIBBLE_C_UNSIGNED=Y` is set, nibble C is treated as unsigned and the unsigned field is also judged as NUMERIC.

```cobol
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      cobnibble.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  UNI01.
         03 SIGNED01.
           05 SDEC01 PIC S9(2) COMP-3.
         03 UNSIGNED01 REDEFINES SIGNED01.
           05 UDEC01 PIC  9(2) COMP-3.
       PROCEDURE        DIVISION.
           MOVE 1 TO SDEC01.
           IF SDEC01 IS NUMERIC THEN
               DISPLAY "Signed:   NUMERIC"
           ELSE
               DISPLAY "Signed:   NOT NUMERIC"
           END-IF.
           IF UDEC01 IS NUMERIC THEN
               DISPLAY "Unsigned: NUMERIC"
           ELSE
               DISPLAY "Unsigned: NOT NUMERIC"
           END-IF.
           STOP RUN.
```

Example run:

```bash
$ cobj cobnibble.cbl

# Without the environment variable (only signed is NUMERIC)
$ java cobnibble
Signed:   NUMERIC
Unsigned: NOT NUMERIC

# With COB_NIBBLE_C_UNSIGNED=Y (both are NUMERIC)
$ COB_NIBBLE_C_UNSIGNED=Y java cobnibble
Signed:   NUMERIC
Unsigned: NUMERIC
```
