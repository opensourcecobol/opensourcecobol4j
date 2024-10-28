package jp.osscons.opensourcecobol.libcobj.ui;

/** TODO: 準備中 */
public class CobolResultInt extends CobolCallResult {
    private int value;

    /**
     * TODO: 準備中
     *
     * @param i TODO: 準備中
     */
    public CobolResultInt(int i) {
        this.value = i;
    }

    /** TODO: 準備中 */
    @Override
    public int getInt() throws CobolResultSetException {
        return this.value;
    }
}
