package jp.osscons.opensourcecobol.libcobj.user_util.indexed_file;

/** Enum to specify the result of the load operation. */
enum LoadResult {
    /** The load operation succeeded. */
    LoadResultSuccess,
    /** The load operation failed because there exists a record whose size is invalid. */
    LoadResultDataSizeMismatch,
    /** The load operation failed because duplicate keys are detected or other error occurs. */
    LoadResultOther,
    /** Reach the end of input data */
    AtEnd,
};
