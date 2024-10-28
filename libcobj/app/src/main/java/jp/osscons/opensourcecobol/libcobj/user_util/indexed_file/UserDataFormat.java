package jp.osscons.opensourcecobol.libcobj.user_util.indexed_file;

/** Enum to specify the format of the input and output data. */
enum UserDataFormat {
    /**
     * This value means the line-sequential format of COBOL. Each records are separated by a newline
     * character (0x20).
     */
    LINE_SEQUENTIAL,
    /**
     * This value means the sequential format of COBOL. Each records are concatenated without any
     * separator.
     */
    SEQUENTIAL,
}
