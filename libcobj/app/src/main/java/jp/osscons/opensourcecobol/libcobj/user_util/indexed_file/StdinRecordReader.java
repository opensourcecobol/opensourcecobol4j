package jp.osscons.opensourcecobol.libcobj.user_util.indexed_file;

import java.util.Scanner;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;

/** TODO: 準備中 */
class StdinRecordReader implements RecordReader {
    /** TODO: 準備中 */
    protected int recordSize;

    /** TODO: 準備中 */
    protected Scanner scan;

    /**
     * TODO: 準備中
     *
     * @param recordSize TODO: 準備中
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

    static class StdinLineSeqReader extends StdinRecordReader {
        /**
         * TODO: 準備中
         *
         * @param recordSize TODO: 準備中
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

    static class StdinSeqReader extends StdinRecordReader {
        private boolean firstFetchFail;
        private byte[] readData;
        private int readDataOffset;

        /**
         * TODO: 準備中
         *
         * @param recordSize TODO: 準備中
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
     * TODO: 準備中
     *
     * @param userDataFormat TODO: 準備中
     * @param recordSize TODO: 準備中
     * @return TODO: 準備中
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
