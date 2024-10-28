package jp.osscons.opensourcecobol.libcobj.ui;

/** TODO: 準備中 */
public class CobolResultString extends CobolCallResult {
    private String value;

    /**
     * TODO: 準備中
     *
     * @param s TODO: 準備中
     */
    public CobolResultString(String s) {
        this.value = s;
    }

    /** TODO: 準備中 */
    @Override
    public String getString() throws CobolResultSetException {
        return this.value;
    }
}
