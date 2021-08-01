package jp.osscons.opensourcecobol.libcobj.exceptions;

import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;

/**
 * GOBACK機能の実装に用いられる例外
 *
 */
public class CobolGoBackException extends Exception {

	/**
	 * 返り値
	 */
	private int returnCode;

	/**
	 * コンストラクタ
	 * @param returnCode 返り値
	 */
	public CobolGoBackException(int returnCode) {
		this.returnCode = returnCode;
	}

	/**
	 * コンストラクタ
	 * @param storage 返り値を格納したCobolDataStorageのインスタンス
	 */
	public CobolGoBackException(CobolDataStorage storage) {
		this(storage.intValue());
	}

	/**
	 * returnCodeのgetter
	 * @return this.returnCode
	 */
	public int getReturnCode() {
		return returnCode;
	}

	/**
	 * CobolGoBackExceptionを例外として投げる
	 * @param returnCode 返り値
	 * @throws CobolGoBackException
	 */
	public static void throwException(int returnCode) throws CobolGoBackException {
		throw new CobolGoBackException(returnCode);
	}

	/**
	 * CobolGoBackExceptionを例外として投げる
	 * @param returnCode 返り値を格納したCobolDataStorageのインスタンス
	 * @throws CobolGoBackException
	 */
	public static void throwException(CobolDataStorage storage) throws CobolGoBackException {
		throw new CobolGoBackException(storage);
	}

	/**
	 * javaコンパイラの解析でthrowが発生しない部分がtry節でくくられていると判断されるとコンパイルエラーになる.
	 * これを回避するためのダミーのメソッド
	 * @throws CobolGoBackException
	 */
	public static void dummy() throws CobolGoBackException {
		if(false)
			throw new CobolGoBackException(0);
	}
}
