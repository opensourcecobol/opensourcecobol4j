package jp.osscons.opensourcecobol.libcobj.ui;

/** double型のCALL文の結果を保持する{@link CobolCallResult}のサブクラス */
public class CobolResultDouble extends CobolCallResult {
    private double value;

    /**
     * 指定されたdouble値を保持するCobolResultDoubleを生成する。
     *
     * @param d 保持するdouble値
     */
    public CobolResultDouble(double d) {
        this.value = d;
    }

    /** {@inheritDoc} */
    @Override
    public double getDouble() throws CobolResultSetException {
        return this.value;
    }
}
