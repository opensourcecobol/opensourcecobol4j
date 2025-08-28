# INDEXED File Locking Specification

## LOCK MODE in the SELECT Statement

- The SELECT statement's LOCK MODE can only specify AUTOMATIC or MANUAL.
- When omitted, the default value for LOCK MODE is MANUAL
  - When `-lock-mode-automatic` is specified at compile time, the default value for omitted LOCK MODE becomes AUTOMATIC.

## Record Locking Behavior

- When a file is opened with OPEN I-O and a READ statement without WITH NO LOCK or WITH LOCK is executed:
  - If the SELECT statement's LOCK MODE is MANUAL, the record is not locked
  - If the SELECT statement's LOCK MODE is AUTOMATIC, the record is locked
- When a file is opened with OPEN I-O and a READ statement with WITH LOCK is executed, the record is locked
- When a file is opened with OPEN I-O and a READ statement with WITH NO LOCK is executed, the record is not locked
- When processing fails due to record locking, the file status becomes 51

## File Locking Behavior

- When a file is opened with OPEN OUTPUT, a file lock is applied to the file
- When processing fails due to file locking, the file status becomes 61

## Migrating Legacy INDEXED Files to the New Version

Legacy INDEXED files cannot be used directly with the new version of opensource COBOL 4J.
To convert INDEXED files created with legacy versions to be compatible with the new version, please use the following command:

```sh
cobj-idx migrate <<INDEXED_FILE>>
```

This command converts the specified INDEXED file to be compatible with the new version.

## Unlocking INDEXED Files

The following command can release all file locks and record locks on an INDEXED file:

```sh
cobj-idx unlock <<INDEXED_FILE>>
```

## Behavior When Opening Legacy INDEXED Files

When attempting to open legacy INDEXED files, the file status becomes 92.
