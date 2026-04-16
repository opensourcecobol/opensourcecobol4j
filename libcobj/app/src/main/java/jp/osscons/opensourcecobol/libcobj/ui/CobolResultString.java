package jp.osscons.opensourcecobol.libcobj.ui;

/** String型のCALL文の結果を保持する{@link CobolCallResult}のサブクラス */
public class CobolResultString extends CobolCallResult {
    private String value;

    /**
     * 指定されたString値を保持するCobolResultStringを生成する。
     *
     * @param s 保持するString値
     */
    public CobolResultString(String s) {
        this.value = s;
    }

    /** {@inheritDoc} */
    @Override
    public String getString() throws CobolResultSetException {
        return this.value;
    }
}
