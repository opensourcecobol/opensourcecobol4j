package jp.osscons.opensourcecobol.libcobj.ui;

/** {@link CobolResultSet}や{@link CobolCallResult}の操作で不正なアクセスが行われた場合にスローされる例外 */
public class CobolResultSetException extends Exception {
    /**
     * 指定されたメッセージを持つCobolResultSetExceptionを生成する。
     *
     * @param message 例外の詳細メッセージ
     */
    public CobolResultSetException(String message) {
        super(message);
    }
}
