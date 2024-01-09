package jp.osscons.opensourcecobol.libcobj.user_util.indexed_file;

import java.io.File;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import org.sqlite.SQLiteConfig;

/**
 * Main class of the utility tool `cobj-idx` to handle a indexed file of opensource COBOL 4J. This
 * tool is used to show information of the indexed file, load data to the indexed file, and unload
 * data from the indexed file.
 */
public class IndexedFileUtilMain {
  /**
   * Main method
   *
   * @param args
   */
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
      int exitCode = processInfoCommand(indexedFilePath);
      System.exit(exitCode);
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

  /** Print help message. */
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

  /**
   * Process info sub command, which shows information of the indexed file.
   *
   * @param indexedFilePath
   * @return 0 if success, otherwise non-zero. The return value is error code.
   */
  private static int processInfoCommand(String indexedFilePath) {
    File indexedFile = new File(indexedFilePath);
    if (!indexedFile.exists()) {
      return ErrorLib.errorFileDoesNotExist(indexedFilePath);
    }
    if (!indexedFile.isFile()) {
      return ErrorLib.errorInvalidIndexedFile(indexedFilePath);
    }

    SQLiteConfig config = new SQLiteConfig();
    config.setReadOnly(true);
    Connection conn = null;
    StringBuilder sb = new StringBuilder();

    try {
      // Open the database file.
      conn = DriverManager.getConnection("jdbc:sqlite:" + indexedFilePath, config.toProperties());
      Statement stmt = conn.createStatement();

      // Retrieve the record size
      ResultSet rs =
          stmt.executeQuery("select value from metadata_string_int where key = 'record_size'");
      if (!rs.next()) {
        return ErrorLib.errorInvalidIndexedFile(indexedFilePath);
      }
      int recordSize = rs.getInt("value");
      sb.append("Size of a record: " + recordSize + "\n");

      // Retrieve the number of records
      rs = stmt.executeQuery("select count(*) from table0");
      if (!rs.next()) {
        return ErrorLib.errorInvalidIndexedFile(indexedFilePath);
      }
      sb.append("Number of records: " + rs.getInt(1) + "\n");

      // Retrieve the number of keys
      rs = stmt.executeQuery("select idx, offset, size, duplicate from metadata_key order by idx");
      while (rs.next()) {
        int idx = rs.getInt("idx");
        int offset = rs.getInt("offset") + 1;
        int size = rs.getInt("size");
        boolean duplicate = rs.getBoolean("duplicate");
        if (idx == 0) {
          sb.append("Primary key position: ");
        } else {
          sb.append("Alternate key position ");
          if (duplicate) {
            sb.append("(Duplicates): ");
          } else {
            sb.append("(No duplicate): ");
          }
        }
        sb.append(offset + "-" + (offset + size - 1) + "\n");
      }

      System.out.print(sb.toString());
      return 0;
    } catch (SQLException e) {
      return ErrorLib.errorInvalidIndexedFile(indexedFilePath);
    } finally {
      if (conn != null) {
        try {
          conn.close();
        } catch (SQLException e) {
          return ErrorLib.errorInvalidIndexedFile(indexedFilePath);
        }
      }
    }
  }
}
