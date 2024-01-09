package jp.osscons.opensourcecobol.libcobj.user_util.indexed_file;

public class ErrorLib {
  public static int errorFileDoesNotExist(String indexedFilePath) {
    System.err.println("error: '" + indexedFilePath + "' does not exist.");
    return 1;
  }

  public static int errorInvalidIndexedFile(String indexedFilePath) {
    System.err.println("error: '" + indexedFilePath + "' is not a valid indexed file.");
    return 1;
  }
}
