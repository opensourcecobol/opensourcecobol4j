package jp.osscons.opensourcecobol.libcobj.ui;

/** TODO: 準備中 */
public class CobolResultSet {
    private CobolCallResult results[];
    private int returnCode;

    /**
     * TODO: 準備中
     *
     * @param returnCode TODO: 準備中
     * @param results TODO: 準備中
     */
    public CobolResultSet(int returnCode, CobolCallResult... results) {
        this.returnCode = returnCode;
        this.results = results;
    }

    private void checkIndexInValidRange(int index) throws CobolResultSetException {
        if (results.length == 0 || index < 1 || this.results.length < index) {
            throw new CobolResultSetException("The index is out of range.");
        }
    }

    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     */
    public int getReturnCode() {
        return this.returnCode;
    }

    /**
     * TODO: 準備中
     *
     * @param index TODO: 準備中
     * @return TODO: 準備中
     * @throws CobolResultSetException TODO: 準備中
     */
    public String getString(int index) throws CobolResultSetException {
        this.checkIndexInValidRange(index);
        return this.results[index - 1].getString();
    }

    /**
     * TODO: 準備中
     *
     * @param index TODO: 準備中
     * @return TODO: 準備中
     * @throws CobolResultSetException TODO: 準備中
     */
    public int getInt(int index) throws CobolResultSetException {
        this.checkIndexInValidRange(index);
        return this.results[index - 1].getInt();
    }

    /**
     * TODO: 準備中
     *
     * @param index TODO: 準備中
     * @return TODO: 準備中
     * @throws CobolResultSetException TODO: 準備中
     */
    public double getDouble(int index) throws CobolResultSetException {
        this.checkIndexInValidRange(index);
        return this.results[index - 1].getDouble();
    }
}
