package jp.osscons.opensourcecobol.libcobj.user_util.indexed_file;

public interface RecordWriter {
  void open();

  boolean write(byte[] record);

  void close();
}
