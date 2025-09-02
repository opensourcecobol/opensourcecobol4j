# File Locking Implementation for SEQUENTIAL, LINE-SEQUENTIAL, and RELATIVE Files

## Overview

File locking is implemented for all file organization types in the base `CobolFile` class, including:
- SEQUENTIAL files
- LINE-SEQUENTIAL files  
- RELATIVE files

## Implementation Details

### Location
File locking logic is implemented in:
- `libcobj/app/src/main/java/jp/osscons/opensourcecobol/libcobj/file/CobolFile.java`
- Method: `open_(String filename, int mode, int sharing)`

### Locking Behavior

#### Lock Type Determination
```java
boolean lockFlag;
if (sharing != 0 || mode == COB_OPEN_OUTPUT) {
    lockFlag = false;  // Shared lock
} else {
    lockFlag = true;   // Exclusive lock
}
```

#### Lock Acquisition
- Uses Java NIO `FileLock`: `fp.tryLock(0L, Long.MAX_VALUE, lockFlag)`
- If lock acquisition fails or returns null, file status 61 is returned (`COB_STATUS_61_FILE_SHARING`)

#### Error Handling
- `ClosedChannelException` → File status 61
- Failed lock acquisition (`fl == null || !fl.isValid()`) → File status 61

### File Status Codes

- **Status 00**: Successful operation
- **Status 61**: File sharing conflict (lock failed)

## Test Coverage

Added tests in `tests/run.src/miscellaneous.at`:

1. **File locking - SEQUENTIAL files** (Test #108)
2. **File locking - LINE SEQUENTIAL files** (Test #109)  
3. **File locking - RELATIVE files** (Test #110)

### Test Scenarios
Each test verifies that files can be successfully opened in different modes:
- OUTPUT mode (creates file)
- INPUT mode (reads file)
- I-O mode (read/write access)
- EXTEND mode (append access)

All operations should return file status 00 for successful operations.

## Notes

- File locking is not applied to device files (paths starting with `/dev/`)
- OUTPUT mode uses shared locks by default
- Other modes use exclusive locks unless sharing is explicitly enabled
- RELATIVE files have additional locking implementation in `CobolRelativeFile.java` that extends the base functionality