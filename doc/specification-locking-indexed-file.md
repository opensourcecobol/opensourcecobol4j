# The specification of locking for INDEXED files in opensource COBOL 4J

## SELECT Statement LOCK MODE

- The SELECT statement LOCK MODE now accepts only `AUTOMATIC` or `MANUAL` specifications
- When LOCK MODE is omitted, the default value is `MANUAL`
- The default value for omitted LOCK MODE can be changed via compile options

## Record Locking Behavior

When a file is opened with `OPEN I-O` and a READ statement is executed:

- **Without WITH LOCK or WITH NO LOCK specification:**
  - If SELECT statement LOCK MODE is `MANUAL`: The record is not locked
  - If SELECT statement LOCK MODE is `AUTOMATIC`: The record is locked

- **With WITH LOCK specification:**
  - The record is locked regardless of LOCK MODE setting

- **With WITH NO LOCK specification:**
  - The record is not locked regardless of LOCK MODE setting

## File Locking Behavior

- When a file is opened with `OPEN OUTPUT`, a file lock is automatically applied to the file

## Migration and Compatibility

### INDEXED File Migration

Legacy INDEXED files are not directly compatible with the new version of opensource COBOL 4J. To convert INDEXED files created with previous versions for use with the new version, please use the following command:

```sh
cobj-idx migrate <<INDEXED_FILE>>
```

This command converts the specified INDEXED file to be compatible with the new version.

### INDEXED File Lock Release

You can release file locks and all record locks on INDEXED files using the following command:

```sh
cobj-idx unlock <<INDEXED_FILE>>
```
