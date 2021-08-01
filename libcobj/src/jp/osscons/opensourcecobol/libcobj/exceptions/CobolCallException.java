package jp.osscons.opensourcecobol.libcobj.exceptions;

/**
 * CobolResolveなどでCALLの失敗したときの例外を示すクラス
 */
public class CobolCallException extends Exception {

	public static CobolCallException ex;

	/**
	 * コンストラクタ
	 */
	public CobolCallException() {
	}
}
