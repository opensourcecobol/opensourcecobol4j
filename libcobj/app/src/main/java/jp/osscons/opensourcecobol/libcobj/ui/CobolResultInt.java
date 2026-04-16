package jp.osscons.opensourcecobol.libcobj.ui;

/** int型のCALL文の結果を保持する{@link CobolCallResult}のサブクラス */
public class CobolResultInt extends CobolCallResult {
    private int value;

    /**
     * 指定されたint値を保持するCobolResultIntを生成する。
     *
     * @param i 保持するint値
     */
    public CobolResultInt(int i) {
        this.value = i;
    }

    /** {@inheritDoc} */
    @Override
    public int getInt() throws CobolResultSetException {
        return this.value;
    }
}
