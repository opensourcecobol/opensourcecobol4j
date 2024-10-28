package jp.osscons.opensourcecobol.libcobj.user_util.indexed_file;

import java.util.Optional;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;

/** TODO: 準備中 */
interface RecordReader {
    /** TODO: 準備中 */
    void open();

    /**
     * TODO: 準備中
     *
     * @param record TODO: 準備中
     * @return TODO: 準備中
     */
    LoadResult read(CobolDataStorage record);

    /** TODO: 準備中 */
    void close();

    /**
     * TODO: 準備中
     *
     * @param userDataFormat TODO: 準備中
     * @param recordSize TODO: 準備中
     * @param filePath TODO: 準備中
     * @return TODO: 準備中
     */
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
