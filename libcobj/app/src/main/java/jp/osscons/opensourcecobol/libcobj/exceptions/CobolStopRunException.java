/*
 * Copyright (C) 2021-2022 TOKYO SYSTEM HOUSE Co., Ltd.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 3.0,
 * or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; see the file COPYING.LIB.  If
 * not, write to the Free Software Foundation, 51 Franklin Street, Fifth Floor
 * Boston, MA 02110-1301 USA
 */
package jp.osscons.opensourcecobol.libcobj.exceptions;

import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.file.CobolFile;

/** STOP RUNの呼び出し時にスローされる例外。返り値を保持する。 */
public final class CobolStopRunException extends Exception {

    /** 返り値 */
    private int returnCode;

    /**
     * コンストラクタ
     *
     * @param returnCode 返り値
     */
    private CobolStopRunException(int returnCode) {
        this.returnCode = returnCode;
    }

    /**
     * コンストラクタ
     *
     * @param storage 返り値を格納したCobolDataStorageのインスタンス
     */
    private CobolStopRunException(CobolDataStorage storage) {
        this(storage.intValue());
    }

    /**
     * このクラスが保持するSTOP RUNの返り値を取得する。
     *
     * @return このクラスが保持するSTOP RUNの返り値
     */
    public int getReturnCode() {
        return returnCode;
    }

    /**
     * CobolStopRunExceptionを例外としてスローする。 COBOLプログラム終了時のデフォルトの終了処理は実行されない。
     *
     * @param returnCode STOP RUNの返り値
     * @throws CobolStopRunException TODO: 準備中
     */
    public static void throwException(int returnCode) throws CobolStopRunException {
        throw new CobolStopRunException(returnCode);
    }

    /**
     * CobolStopRunExceptionを例外としてスローする。 COBOLプログラム終了時のデフォルトの終了処理は実行されない。
     *
     * @param storage STOP RUNの返り値
     * @throws CobolStopRunException TODO: 準備中
     */
    public static void throwException(CobolDataStorage storage) throws CobolStopRunException {
        throw new CobolStopRunException(storage);
    }

    /**
     * javaコンパイラは、try節の中にthrowが発生しないと判断するとコンパイルエラーになる。 Javaコード生成時にこの問題を回避するため、このメソッドが挿入される。 throws
     * CobolRunExceptionが指定されているが、このメソッドは決して例外をスローせず、その他の処理も実行しない。
     *
     * @throws CobolStopRunException TODO: 準備中
     */
    public static void dummy() throws CobolStopRunException {
        if (false) {
            throw new CobolStopRunException(0);
        }
    }

    /**
     * デフォルトの終了処理を実行してから、CobolStopRunExceptionをスローする。
     * デフォルトの終了処理では、COBOLプログラムによってオープンされたファイルのクローズが行われる。
     *
     * @param status STOP RUNの返り値
     * @throws CobolStopRunException このメソッドでは必ずCobolStopRunExceptionがスローされる。
     */
    public static void stopRunAndThrow(int status) throws CobolStopRunException {
        stopRun();
        CobolStopRunException.throwException(status);
    }

    /** デフォルトの終了処理を実行する。CobolStopRunExceptionをスローしない。 */
    public static void stopRun() {
        // TODO screen実装時に追加
        // cob_screen_terminate();
        CobolFile.exitFileIO();
    }
}
