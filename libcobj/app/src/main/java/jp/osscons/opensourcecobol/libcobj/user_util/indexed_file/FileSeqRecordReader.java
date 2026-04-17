package jp.osscons.opensourcecobol.libcobj.user_util.indexed_file;

import java.io.FileInputStream;
import java.io.IOException;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;

/**
 * SEQUENTIAL形式で書かれたファイルから1レコードずつ読み込む{@link RecordReader}の実装。<br>
 * 入力ファイルはレコード区切りを持たず、{@code recordSize}バイトずつ連続して並んでいることを想定する。
 * 末尾に{@code \n}または{@code \r\n}のみが残っている場合は終端として扱う。
 */
public class FileSeqRecordReader implements RecordReader {
    private FileInputStream reader;
    private String filePath;
    private byte[] readData;

    /**
     * 新しい{@code FileSeqRecordReader}インスタンスを生成する。<br>
     * この時点ではファイルは開かれず、{@link #open()}の呼び出しによって開かれる。
     *
     * @param recordSize 1レコードのバイト数
     * @param filePath 読み込み対象のファイルのパス
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
