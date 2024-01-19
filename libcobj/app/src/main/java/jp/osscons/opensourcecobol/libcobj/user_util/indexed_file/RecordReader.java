package jp.osscons.opensourcecobol.libcobj.user_util.indexed_file;

import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;

public interface RecordReader {
  void open();

  LoadResult read(CobolDataStorage record);

  void close();

  static RecordReader getInstance(UserDataFormat userDataFormat, int recordSize) {
    return StdinRecordReader.getInstance(userDataFormat, recordSize);
  }
}
