package jp.osscons.opensourcecobol.libcobj.ui;

/** TODO: 準備中 */
public class CobolResultDouble extends CobolCallResult {
    private double value;

    /**
     * TODO: 準備中
     *
     * @param d TODO: 準備中
     */
    public CobolResultDouble(double d) {
        this.value = d;
    }

    /** TODO: 準備中 */
    @Override
    public double getDouble() throws CobolResultSetException {
        return this.value;
    }
}
