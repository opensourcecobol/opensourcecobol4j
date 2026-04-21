package jp.osscons.opensourcecobol.libcobj.user_util.indexed_file;

import java.util.Scanner;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;

/**
 * 標準入力から1レコードずつ読み込む{@link RecordReader}の基底クラス。<br>
 * 具体的な読み込みロジックは入力データ形式ごとに内部クラス
 * ({@link StdinLineSeqReader} / {@link StdinSeqReader})で実装され、
 * {@link #getInstance(UserDataFormat, int)}から対応するインスタンスが取得される。<br>
 * このクラス自体の{@link #read(CobolDataStorage)}は呼び出されることを想定せず、{@code null}を返す。
 */
class StdinRecordReader implements RecordReader {
    /** 1レコードのバイト数 */
    protected int recordSize;

    /** 標準入力から読み込みを行うScanner */
    protected Scanner scan;

    /**
     * 新しい{@code StdinRecordReader}インスタンスを生成する。
     *
     * @param recordSize 1レコードのバイト数
     */
    protected StdinRecordReader(int recordSize) {
        this.recordSize = recordSize;
    }

    @Override
    public void open() {
        this.scan = new Scanner(System.in);
    }

    @Override
    public LoadResult read(CobolDataStorage record) {
        return null;
    }

    @Override
    public void close() {
        this.scan.close();
    }

    /**
     * 標準入力からLINE SEQUENTIAL形式で1レコードずつ読み込む内部クラス。<br>
     * 1行を1レコードとして扱い、行のバイト数が{@code recordSize}と一致することを期待する。
     */
    static class StdinLineSeqReader extends StdinRecordReader {
        /**
         * 新しい{@code StdinLineSeqReader}インスタンスを生成する。
         *
         * @param recordSize 1レコードのバイト数
         */
        StdinLineSeqReader(int recordSize) {
            super(recordSize);
        }

        @Override
        public LoadResult read(CobolDataStorage record) {
            if (scan.hasNextLine()) {
                byte[] readData = scan.nextLine().getBytes();
                if (readData.length != this.recordSize) {
                    return LoadResult.LoadResultDataSizeMismatch;
                }
                record.memcpy(readData, this.recordSize);
                return LoadResult.LoadResultSuccess;
            } else {
                return LoadResult.AtEnd;
            }
        }
    }

    /**
     * 標準入力からSEQUENTIAL形式で1レコードずつ読み込む内部クラス。<br>
     * 実装上は{@link Scanner#next()}で標準入力から空白区切りの1トークンを{@link String}として取得し、
     * そのバイト列を{@code recordSize}バイトずつ区切って1レコードとして扱う。このため、入力全体が
     * 空白文字を含まない1つのトークンであり、かつそのバイト長が{@code recordSize}の倍数であることを想定する。
     * 1トークン分のバイト長が{@code recordSize}の倍数でない場合は
     * {@link LoadResult#LoadResultDataSizeMismatch}を返す。
     */
    static class StdinSeqReader extends StdinRecordReader {
        private boolean firstFetchFail;
        private byte[] readData;
        private int readDataOffset;

        /**
         * 新しい{@code StdinSeqReader}インスタンスを生成する。
         *
         * @param recordSize 1レコードのバイト数
         */
        public StdinSeqReader(int recordSize) {
            super(recordSize);
        }

        @Override
        public void open() {
            super.open();
            this.firstFetchFail = false;
            this.readData = null;
            this.readDataOffset = 0;
        }

        @Override
        public LoadResult read(CobolDataStorage record) {
            if (firstFetchFail) {
                return LoadResult.LoadResultDataSizeMismatch;
            }
            if (readData == null) {
                if (!this.scan.hasNextLine()) {
                    return LoadResult.AtEnd;
                }
                this.readData = this.scan.next().getBytes();
                if (readData.length % this.recordSize != 0) {
                    this.firstFetchFail = true;
                    return LoadResult.LoadResultDataSizeMismatch;
                }
            }
            if (readDataOffset >= readData.length) {
                return LoadResult.AtEnd;
            }
            record.memcpy(this.readData, this.readDataOffset, this.recordSize);
            this.readDataOffset += this.recordSize;
            return LoadResult.LoadResultSuccess;
        }
    }

    /**
     * 指定された入力データ形式に対応する標準入力用の{@link RecordReader}実装を返す。
     *
     * @param userDataFormat 入力データの形式
     * @param recordSize 1レコードのバイト数
     * @return 入力データ形式に対応する{@code RecordReader}の実装。形式が未対応の場合は{@code null}
     */
    static RecordReader getInstance(UserDataFormat userDataFormat, int recordSize) {
        switch (userDataFormat) {
            case LINE_SEQUENTIAL:
                {
                    return new StdinLineSeqReader(recordSize);
                }
            case SEQUENTIAL:
                {
                    return new StdinSeqReader(recordSize);
                }
            default:
                return null;
        }
    }
}
