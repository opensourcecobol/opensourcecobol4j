package jp.osscons.opensourcecobol.libcobj.user_util.indexed_file;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;

/**
 * LINE SEQUENTIAL形式で書かれたファイルから1レコードずつ読み込む{@link RecordReader}の実装。<br>
 * 1行を1レコードとして扱い、各行のバイト数がコンストラクタで指定された{@code recordSize}と一致することを期待する。
 */
class FileLineSeqRecordReader implements RecordReader {
    /** 1レコードのバイト数 */
    int recordSize;

    /** 入力ファイルを行単位で読み込むためのリーダ */
    BufferedReader reader;

    private String filePath;

    /**
     * 新しい{@code FileLineSeqRecordReader}インスタンスを生成する。<br>
     * この時点ではファイルは開かれず、{@link #open()}の呼び出しによって開かれる。
     *
     * @param recordSize 1レコードのバイト数
     * @param filePath 読み込み対象のファイルのパス
     */
    FileLineSeqRecordReader(int recordSize, String filePath) {
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
