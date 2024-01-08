package jp.osscons.opensourcecobol.libcobj.user_util.indexed_file;

public class IndexedFileUtilMain {
  public static void main(String[] args) {
    if (args.length == 0) {
      printHelpMessage();
      System.exit(0);
    }
    String subCommand = args[0];
    if ("info".equals(subCommand)) {
      if (args.length != 2) {
        if (args.length < 2) {
          System.err.println("error: no indexed file is specified.");
        } else {
          System.err.println("error: too many indexed files are specified.");
        }
        System.exit(1);
      }
      String indexedFilePath = args[1];
      processInfoCommand(indexedFilePath);
    } else if ("load".equals(subCommand)) {
      System.err.println("error: load command is not implemented yet.");
      System.exit(1);
    } else if ("unload".equals(subCommand)) {
      System.err.println("error: load command is not implemented yet.");
      System.exit(1);
    } else {
      printHelpMessage();
      System.exit(1);
    }
  }

  private static void printHelpMessage() {
    System.out.println("cobj-idx - Utility tool to handle a indexed file of opensource COBOL 4J");
    System.out.println();
    System.out.println("Usage:");
    System.out.println();
    System.out.println("cobj-idx info <indexed-file>   - Show information of the indexed file.");
    System.out.println(
        "cobj-idx load <indexed-file>   - Load data inputted from stdin to the indexed file.");
    System.out.println(
        "                                 The format of the input data is line-sequential of COBOL.");
    System.out.println(
        "cobj-idx unload <indexed-file> - Write records stored in the indexed file to stdout.");
    System.out.println(
        "                                 The format of the output data is line-sequential of COBOL.");
  }

  private static void processInfoCommand(String indexedFilePath) {

  }
}
