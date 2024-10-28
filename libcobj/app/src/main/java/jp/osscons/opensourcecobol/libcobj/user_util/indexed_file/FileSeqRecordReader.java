package jp.osscons.opensourcecobol.libcobj.user_util.indexed_file;

import java.io.FileInputStream;
import java.io.IOException;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;

/** TODO: 準備中 */
public class FileSeqRecordReader implements RecordReader {
    private FileInputStream reader;
    private String filePath;
    private byte[] readData;

    /**
     * TODO: 準備中
     *
     * @param recordSize TODO: 準備中
     * @param filePath TODO: 準備中
     */
    FileSeqRecordReader(int recordSize, String filePath) {
        this.filePath = filePath;
        this.readData = new byte[recordSize];
    }

    @Override
    public void open() {
        try {
            this.reader = new FileInputStream(this.filePath);
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
            int readSize = this.reader.read(this.readData);
            if (readSize == -1) {
                return LoadResult.AtEnd;
            }
            if (readSize != this.readData.length) {
                if (readSize == 1 && this.readData[0] == '\n') {
                    return LoadResult.AtEnd;
                } else if (readSize == 2 && this.readData[0] == '\r' && this.readData[1] == '\n') {
                    return LoadResult.AtEnd;
                } else {
                    return LoadResult.LoadResultDataSizeMismatch;
                }
            }
            record.memcpy(this.readData, this.readData.length);
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
