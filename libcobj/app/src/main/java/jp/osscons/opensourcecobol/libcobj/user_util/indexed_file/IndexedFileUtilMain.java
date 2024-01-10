package jp.osscons.opensourcecobol.libcobj.user_util.indexed_file;

import java.io.File;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.Scanner;
import jp.osscons.opensourcecobol.libcobj.common.CobolModule;
import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldAttribute;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldFactory;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolRuntimeException;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolStopRunException;
import jp.osscons.opensourcecobol.libcobj.file.CobolFile;
import jp.osscons.opensourcecobol.libcobj.file.CobolFileFactory;
import jp.osscons.opensourcecobol.libcobj.file.CobolFileKey;
import jp.osscons.opensourcecobol.libcobj.file.CobolIndexedFile;
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
      if (args.length != 2) {
        if (args.length < 2) {
          System.err.println("error: no indexed file is specified.");
        } else {
          System.err.println("error: too many indexed files are specified.");
        }
        System.exit(1);
      }
      String indexedFilePath = args[1];
      int exitCode = processLoadCommand(indexedFilePath);
      System.exit(exitCode);
    } else if ("unload".equals(subCommand)) {
      if (args.length != 2) {
        if (args.length < 2) {
          System.err.println("error: no indexed file is specified.");
        } else {
          System.err.println("error: too many indexed files are specified.");
        }
        System.exit(1);
      }
      String indexedFilePath = args[1];
      int exitCode = processUnloadCommand(indexedFilePath);
      System.exit(exitCode);
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
      conn.close();
      return 0;
    } catch (SQLException e) {
      if (conn != null) {
        try {
          conn.close();
        } catch (SQLException ee) {
          return ErrorLib.errorIO();
        }
      }
      return ErrorLib.errorInvalidIndexedFile(indexedFilePath);
    }
  }

  enum LoadResult {
    Success,
    DataSizeMismatch,
    Other,
  };
  /**
   * Process load sub command, which loads data inputted from stdin to the indexed file.
   *
   * @param indexedFilePath
   * @return
   */
  private static int processLoadCommand(String indexedFilePath) {
    File indexedFile = new File(indexedFilePath);
    if (!indexedFile.exists()) {
      return ErrorLib.errorFileDoesNotExist(indexedFilePath);
    }
    if (!indexedFile.isFile()) {
      return ErrorLib.errorInvalidIndexedFile(indexedFilePath);
    }
    Optional<CobolFile> cobolFileRet = createCobolFileFromIndexedFilePath(indexedFilePath);
    if (!cobolFileRet.isPresent()) {
      return ErrorLib.errorInvalidIndexedFile(indexedFilePath);
    }
    CobolIndexedFile cobolIndexedFile = (CobolIndexedFile) cobolFileRet.get();

    // Set the module
    CobolModule module =
        new CobolModule(null, null, null, null, 0, '.', '$', ',', 1, 1, 1, 0, null);
    CobolModule.push(module);

    // Open the indexed file
    CobolRuntimeException.code = 0;
    cobolIndexedFile.setCommitOnModification(false);
    cobolIndexedFile.open(CobolFile.COB_OPEN_EXTEND, 0, null);
    if (CobolRuntimeException.code != 0) {
      return ErrorLib.errorIO();
    }

    // Read records from stdin and write them to the indexed file
    String line = null;
    Scanner scan = new Scanner(System.in);
    LoadResult loadResult = LoadResult.Success;
    while (scan.hasNextLine()) {
      line = scan.nextLine();
      CobolRuntimeException.code = 0;
      byte[] readData = line.getBytes();
      if (readData.length != cobolIndexedFile.record.getSize()) {
        loadResult = LoadResult.DataSizeMismatch;
        break;
      }
      cobolIndexedFile.record.getDataStorage().memcpy(readData);
      try {
        cobolIndexedFile.write(cobolIndexedFile.record, 0, null);
      } catch (CobolStopRunException e) {
        loadResult = LoadResult.Other;
        break;
      }
      if (CobolRuntimeException.code != 0) {
        loadResult = LoadResult.Other;
        break;
      }
    }

    scan.close();

    if (loadResult == LoadResult.DataSizeMismatch) {
      return ErrorLib.errorDataSizeMismatch(cobolIndexedFile.record.getSize());
    } else if (loadResult == LoadResult.Other) {
      return ErrorLib.errorDuplicateKeys();
    } else {
      cobolIndexedFile.commitJdbcTransaction();
      cobolIndexedFile.close(0, null);
      return 0;
    }
  }

  /**
   * Process unload sub command, which writes records stored in the indexed file to stdout.
   *
   * @param indexedFilePath
   * @return 0 if success, otherwise non-zero. The return value is error code.
   */
  private static int processUnloadCommand(String indexedFilePath) {
    File indexedFile = new File(indexedFilePath);
    if (!indexedFile.exists()) {
      return ErrorLib.errorFileDoesNotExist(indexedFilePath);
    }
    if (!indexedFile.isFile()) {
      return ErrorLib.errorInvalidIndexedFile(indexedFilePath);
    }
    Optional<CobolFile> cobolFileRet = createCobolFileFromIndexedFilePath(indexedFilePath);
    if (!cobolFileRet.isPresent()) {
      return ErrorLib.errorInvalidIndexedFile(indexedFilePath);
    }
    CobolFile cobolFile = cobolFileRet.get();

    // Set the module
    CobolModule module =
        new CobolModule(null, null, null, null, 0, '.', '$', ',', 1, 1, 1, 0, null);
    CobolModule.push(module);

    // Open the indexed file
    CobolRuntimeException.code = 0;
    cobolFile.open(CobolFile.COB_OPEN_INPUT, 0, null);
    if (CobolRuntimeException.code != 0) {
      return ErrorLib.errorIO();
    }

    // Read records from the indexed file and write them to stdout
    while (true) {
      CobolRuntimeException.code = 0;
      cobolFile.read(0, null, 1);
      if (CobolRuntimeException.code == 0) {
        System.out.println(cobolFile.record.getString());
      } else if (CobolRuntimeException.code == 0x0501) {
        break;
      } else {
        return ErrorLib.errorIO();
      }
    }
    cobolFile.close(CobolFile.COB_CLOSE_NORMAL, null);

    CobolModule.pop();
    return 0;
  }

  /**
   * Create a CobolFile instance from the path of the indexed file.
   *
   * @param indexedFilePath
   * @return CobolFile instance if success, otherwise empty.
   */
  private static Optional<CobolFile> createCobolFileFromIndexedFilePath(String indexedFilePath) {
    SQLiteConfig config = new SQLiteConfig();
    config.setReadOnly(true);
    Connection conn = null;

    try {
      // Open the database file.
      conn = DriverManager.getConnection("jdbc:sqlite:" + indexedFilePath, config.toProperties());
      Statement stmt = conn.createStatement();

      // Retrieve the record size
      ResultSet rs =
          stmt.executeQuery("select value from metadata_string_int where key = 'record_size'");
      if (!rs.next()) {
        return Optional.empty();
      }
      int recordSize = rs.getInt("value");

      // Create a record field
      byte[] recordByteArray = new byte[recordSize];
      CobolDataStorage recordDataStorage = new CobolDataStorage(recordByteArray);
      AbstractCobolField recordField =
          CobolFieldFactory.makeCobolField(
              recordSize, recordDataStorage, new CobolFieldAttribute(1, 0, 0, 0, null));

      // Retrive key information
      List<CobolFileKey> keyList = new ArrayList<CobolFileKey>();
      rs = stmt.executeQuery("select idx, offset, size, duplicate from metadata_key order by idx");
      while (rs.next()) {
        int offset = rs.getInt("offset");
        int size = rs.getInt("size");
        boolean duplicate = rs.getBoolean("duplicate");

        CobolFileKey cobolFileKey = new CobolFileKey();
        cobolFileKey.setOffset(offset);
        cobolFileKey.setFlag(duplicate ? 1 : 0);
        AbstractCobolField keyField =
            CobolFieldFactory.makeCobolField(
                size,
                recordDataStorage.getSubDataStorage(offset),
                new CobolFieldAttribute(33, 0, 0, 0, null));
        cobolFileKey.setField(keyField);

        keyList.add(cobolFileKey);
      }

      // Construct a CobolFile instance
      byte[] fileStatus = new byte[4];
      byte[] indxedFilePathBytes = indexedFilePath.getBytes();
      AbstractCobolField assignField =
          CobolFieldFactory.makeCobolField(
              indxedFilePathBytes.length,
              new CobolDataStorage(indxedFilePathBytes),
              new CobolFieldAttribute(33, 0, 0, 0, null));
      CobolFileKey[] keyArray = new CobolFileKey[keyList.size()];
      keyList.toArray(keyArray);
      CobolFile cobolFile =
          CobolFileFactory.makeCobolFileInstance(
              "f",
              fileStatus,
              assignField,
              recordField,
              null,
              recordSize,
              recordSize,
              keyArray.length,
              keyArray,
              (char) 3,
              (char) 1,
              (char) 0,
              (char) 0,
              false,
              (char) 0,
              (char) 0,
              false,
              false,
              false,
              (char) 0,
              false,
              (char) 2,
              false,
              false,
              (char) 0);
      conn.close();
      return Optional.of(cobolFile);
    } catch (SQLException e) {
      if (conn != null) {
        try {
          conn.close();
        } catch (SQLException ee) {
          return Optional.empty();
        }
      }
      return Optional.empty();
    }
  }
}
