/*
 * Copyright (C) 2020 TOKYO SYSTEM HOUSE Co., Ltd.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, 51 Franklin Street, Fifth Floor
 * Boston, MA 02110-1301 USA
 */

package jp.osscons.opensourcecobol.libcobj.exceptions;

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
	public static void stopRun() {
		//TODO 残りを実装
		CobolFile.exitFileIO();
	}
}
