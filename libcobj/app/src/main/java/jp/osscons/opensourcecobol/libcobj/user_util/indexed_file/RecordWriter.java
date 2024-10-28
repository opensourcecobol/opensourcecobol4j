package jp.osscons.opensourcecobol.libcobj.user_util.indexed_file;

/** TODO: 準備中 */
interface RecordWriter {
    /** TODO: 準備中 */
    void open();

    /**
     * TODO: 準備中
     *
     * @param record TODO: 準備中
     * @return TODO: 準備中
     */
    boolean write(byte[] record);

    /** TODO: 準備中 */
    void close();
}
