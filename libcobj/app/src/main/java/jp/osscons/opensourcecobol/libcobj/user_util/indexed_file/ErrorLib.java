package jp.osscons.opensourcecobol.libcobj.user_util.indexed_file;

/** Manage error info */
class ErrorLib {
    /**
     * Error when indexed file does not exist
     *
     * @param indexedFilePath TODO: 準備中
     * @return 1
     */
    static int errorFileDoesNotExist(String indexedFilePath) {
        System.err.println("error: '" + indexedFilePath + "' does not exist.");
        return 1;
    }

    /**
     * Error when indexed file is not a valid indexed file
     *
     * @param indexedFilePath TODO: 準備中
     * @return 1
     */
    static int errorInvalidIndexedFile(String indexedFilePath) {
        System.err.println("error: '" + indexedFilePath + "' is not a valid indexed file.");
        return 1;
    }

    /**
     * Error when IO operations of indexed files
     *
     * @return 1
     */
    static int errorIO() {
        System.err.println("error: IO error.");
        return 1;
    }

    /**
     * Error when some keys in input data have conflicts
     *
     * @return 1
     */
    static int errorDuplicateKeys() {
        System.err.println("error: loading fails because of duplicate keys.");
        return 1;
    }

    /**
     * Error when some records in input data have invalid size
     *
     * @param correctSize TODO: 準備中
     * @return TODO: 準備中
     */
    static int errorDataSizeMismatch(int correctSize) {
        System.err.println("error: all record must have the length of " + correctSize + " bytes.");
        return 1;
    }
}
