package jp.osscons.opensourcecobol.libcobj.ui;

/** TODO: 準備中 */
public class CobolCallResult {
    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     * @throws CobolResultSetException TODO: 準備中
     */
    public int getInt() throws CobolResultSetException {
        throw new CobolResultSetException("The result type is not 'int'");
    }

    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     * @throws CobolResultSetException TODO: 準備中
     */
    public double getDouble() throws CobolResultSetException {
        throw new CobolResultSetException("The result type is not 'double'");
    }

    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     * @throws CobolResultSetException TODO: 準備中
     */
    public String getString() throws CobolResultSetException {
        throw new CobolResultSetException("The result type is not 'String'");
    }
}
