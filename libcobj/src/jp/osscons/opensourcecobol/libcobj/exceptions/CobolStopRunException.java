package jp.osscons.opensourcecobol.libcobj.exceptions;

import java.util.List;

import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.file.CobolFile;

/**
 * STOP RUN機能の実装に用いられる例外
 */
public class CobolStopRunException extends Exception {

	/**
	 * 返り値
	 */
	private int returnCode;
	
	public static List<RuntimeExitHandler> exitHandlers = null;

	/**
	 * コンストラクタ
	 * @param returnCode 返り値
	 */
	public CobolStopRunException(int returnCode) {
		this.returnCode = returnCode;
	}

	/**
	 * コンストラクタ
	 * @param storage 返り血を格納したCobolDataStorageのインスタンス
	 */
	public CobolStopRunException(CobolDataStorage storage) {
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
	 *  CobolStopRunExceptionを例外として投げる
	 * @param returnCode 返り値
	 * @throws CobolStopRunException
	 */
	public static void throwException(int returnCode) throws CobolStopRunException {
		throw new CobolStopRunException(returnCode);
	}

	/**
	 *  CobolStopRunExceptionを例外として投げる
	 * @param returnCode 返り値を格納したCobolDataStorageのインスタンス
	 * @throws CobolStopRunException
	 */
	public static void throwException(CobolDataStorage storage) throws CobolStopRunException {
		throw new CobolStopRunException(storage);
	}

	/**
	 * javaコンパイラの解析でthrowが発生しない部分がtry節でくくられていると判断されるとコンパイルエラーになる.
	 * これを回避するためのダミーのメソッド
	 * @throws CobolGoBackException
	 */
	public static void dummy() throws CobolStopRunException {
		if(false)
			throw new CobolStopRunException(0);
	}

	/**
	 * libcob/common.cのcob_stop_runの実装
	 */
	public static void stopRunAndThrow(int status) throws CobolStopRunException {
		stopRun();
		CobolStopRunException.throwException(status);
	}
	
	public static void stopRun(){
		if(exitHandlers != null) {
			for(RuntimeExitHandler handler : exitHandlers) {
				handler.proc();
			}
		}
		//TODO screen実装時に追加
		//cob_screen_terminate();
		CobolFile.exitFileIO();
	}
}
