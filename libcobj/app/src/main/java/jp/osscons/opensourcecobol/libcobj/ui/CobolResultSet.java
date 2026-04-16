package jp.osscons.opensourcecobol.libcobj.ui;

/**
 * CALL文の実行結果をまとめて保持するクラス<br>
 * リターンコードと複数の{@link CobolCallResult}を保持し、インデックス指定で各結果にアクセスできる。
 * インデックスは1始まりで指定する。
 */
public class CobolResultSet {
    private CobolCallResult results[];
    private int returnCode;

    /**
     * 指定されたリターンコードと結果の配列を持つCobolResultSetを生成する。
     *
     * @param returnCode CALL文のリターンコード
     * @param results CALL文の結果の配列
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
     * CALL文のリターンコードを取得する。
     *
     * @return リターンコード
     */
    public int getReturnCode() {
        return this.returnCode;
    }

    /**
     * 指定されたインデックスの結果をString型として取得する。
     *
     * @param index 結果のインデックス(1始まり)
     * @return String型の結果値
     * @throws CobolResultSetException インデックスが範囲外の場合、または結果の型がStringでない場合
     */
    public String getString(int index) throws CobolResultSetException {
        this.checkIndexInValidRange(index);
        return this.results[index - 1].getString();
    }

    /**
     * 指定されたインデックスの結果をint型として取得する。
     *
     * @param index 結果のインデックス(1始まり)
     * @return int型の結果値
     * @throws CobolResultSetException インデックスが範囲外の場合、または結果の型がintでない場合
     */
    public int getInt(int index) throws CobolResultSetException {
        this.checkIndexInValidRange(index);
        return this.results[index - 1].getInt();
    }

    /**
     * 指定されたインデックスの結果をdouble型として取得する。
     *
     * @param index 結果のインデックス(1始まり)
     * @return double型の結果値
     * @throws CobolResultSetException インデックスが範囲外の場合、または結果の型がdoubleでない場合
     */
    public double getDouble(int index) throws CobolResultSetException {
        this.checkIndexInValidRange(index);
        return this.results[index - 1].getDouble();
    }
}
