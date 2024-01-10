package jp.osscons.opensourcecobol.libcobj.user_util.indexed_file;

/** Manage error info */
public class ErrorLib {
  /**
   * Error when indexed file does not exist
   *
   * @param indexedFilePath
   * @return 1
   */
  public static int errorFileDoesNotExist(String indexedFilePath) {
    System.err.println("error: '" + indexedFilePath + "' does not exist.");
    return 1;
  }
  /**
   * Error when indexed file is not a valid indexed file
   *
   * @param indexedFilePath
   * @return 1
   */
  public static int errorInvalidIndexedFile(String indexedFilePath) {
    System.err.println("error: '" + indexedFilePath + "' is not a valid indexed file.");
    return 1;
  }

  /**
   * Error when IO operations of indexed files
   *
   * @param indexedFilePath
   * @return 1
   */
  public static int errorIO() {
    System.err.println("error: IO error.");
    return 1;
  }

  /**
   * Error when some keys in input data have conflicts
   *
   * @return
   */
  public static int errorDuplicateKeys() {
    System.err.println("error: loading fails because of duplicate keys.");
    return 1;
  }

  /**
   * Error when some records in input data have invalid size
   *
   * @param correctSize
   * @return
   */
  public static int errorDataSizeMismatch(int correctSize) {
    System.err.println("error: all record must have the length of " + correctSize + " bytes.");
    return 1;
  }
}
