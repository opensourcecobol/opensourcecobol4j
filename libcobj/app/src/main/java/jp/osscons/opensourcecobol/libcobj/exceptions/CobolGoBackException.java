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

/** GOBACKの呼び出し時にスローされる例外。返り値を保持する */
public final class CobolGoBackException extends Exception {

    /** 返り値 */
    private int returnCode;

    /**
     * コンストラクタ
     *
     * @param returnCode 返り値
     */
    private CobolGoBackException(int returnCode) {
        this.returnCode = returnCode;
    }

    /**
     * コンストラクタ
     *
     * @param returnCode 返り値
     */
    private CobolGoBackException(CobolDataStorage storage) {
        this(storage.intValue());
    }

    /**
     * このクラスが保持するGOBACKの返り値を取得する。
     *
     * @return このクラスが保持するGOBACKの返り値
     */
    public int getReturnCode() {
        return returnCode;
    }

    /**
     * CobolGoBackExceptionを例外として投げる。
     *
     * @param returnCode GOBACKの返り値
     * @throws CobolGoBackException TODO: 準備中
     */
    public static void throwException(int returnCode) throws CobolGoBackException {
        throw new CobolGoBackException(returnCode);
    }

    /**
     * CobolGoBackExceptionを例外として投げる。
     *
     * @param storage GOBACKの返り値
     * @throws CobolGoBackException TODO: 準備中
     */
    public static void throwException(CobolDataStorage storage) throws CobolGoBackException {
        throw new CobolGoBackException(storage);
    }

    /**
     * javaコンパイラは、try節の中にthrowが発生しないと判断するとコンパイルエラーになる。 Javaコード生成時にこの問題を回避するため、このメソッドが挿入される。 throws
     * CobolGoBackExceptionが指定されているが、このメソッドは決して例外をスローせず、その他の処理も実行しない。
     *
     * @throws CobolGoBackException TODO: 準備中
     */
    public static void dummy() throws CobolGoBackException {
        if (false) {
            throw new CobolGoBackException(0);
        }
    }
}
