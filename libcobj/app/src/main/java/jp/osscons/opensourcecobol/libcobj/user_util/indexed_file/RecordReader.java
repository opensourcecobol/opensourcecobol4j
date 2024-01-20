package jp.osscons.opensourcecobol.libcobj.user_util.indexed_file;

import java.util.Optional;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;

public interface RecordReader {
  void open();

  LoadResult read(CobolDataStorage record);

  void close();

  static RecordReader getInstance(
      UserDataFormat userDataFormat, int recordSize, Optional<String> filePath) {
    if (filePath.isPresent()) {
      switch (userDataFormat) {
        case LINE_SEQUENTIAL:
          return new FileLineSeqRecordReader(recordSize, filePath.get());
        case SEQUENTIAL:
          return new FileSeqRecordReader(recordSize, filePath.get());
        default:
          return null;
      }
    } else {
      return StdinRecordReader.getInstance(userDataFormat, recordSize);
    }
  }
}
