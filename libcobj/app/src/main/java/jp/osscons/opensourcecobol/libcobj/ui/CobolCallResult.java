package jp.osscons.opensourcecobol.libcobj.ui;

/**
 * CALL文の結果を保持するための基底クラス<br>
 * サブクラスで保持する型に対応するgetメソッドをオーバーライドする。
 * 対応しない型のgetメソッドを呼び出した場合は{@link CobolResultSetException}がスローされる。
 */
public class CobolCallResult {
    /**
     * 結果をint型として取得する。
     *
     * @return int型の結果値
     * @throws CobolResultSetException 結果の型がintでない場合
     */
    public int getInt() throws CobolResultSetException {
        throw new CobolResultSetException("The result type is not 'int'");
    }

    /**
     * 結果をdouble型として取得する。
     *
     * @return double型の結果値
     * @throws CobolResultSetException 結果の型がdoubleでない場合
     */
    public double getDouble() throws CobolResultSetException {
        throw new CobolResultSetException("The result type is not 'double'");
    }

    /**
     * 結果をString型として取得する。
     *
     * @return String型の結果値
     * @throws CobolResultSetException 結果の型がStringでない場合
     */
    public String getString() throws CobolResultSetException {
        throw new CobolResultSetException("The result type is not 'String'");
    }
}
