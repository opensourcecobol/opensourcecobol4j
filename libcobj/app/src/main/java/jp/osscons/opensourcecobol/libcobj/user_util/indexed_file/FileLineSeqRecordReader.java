package jp.osscons.opensourcecobol.libcobj.user_util.indexed_file;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;

public class FileLineSeqRecordReader implements RecordReader {
  protected int recordSize;
  protected BufferedReader reader;
  private String filePath;

  public FileLineSeqRecordReader(int recordSize, String filePath) {
    this.recordSize = recordSize;
    this.filePath = filePath;
  }

  @Override
  public void open() {
    try {
      this.reader = new BufferedReader(new FileReader(this.filePath));
    } catch (IOException e) {
      this.reader = null;
    }
  }

  @Override
  public LoadResult read(CobolDataStorage record) {
    if (this.reader == null) {
      return LoadResult.LoadResultOther;
    }
    try {
      String line = this.reader.readLine();
      if (line == null) {
        return LoadResult.AtEnd;
      }
      byte[] readData = line.getBytes();
      if (readData.length != this.recordSize) {
        return LoadResult.LoadResultDataSizeMismatch;
      }
      record.memcpy(readData, this.recordSize);
      return LoadResult.LoadResultSuccess;
    } catch (IOException e) {
      return LoadResult.LoadResultOther;
    }
  }

  @Override
  public void close() {
    try {
      this.reader.close();
    } catch (IOException e) {
      return;
    }
  }
}
