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

/** GOBACK機能の実装に用いられる例外 */
public class CobolGoBackException extends Exception {

  /** 返り値 */
  private int returnCode;

  /**
   * コンストラクタ
   *
   * @param returnCode 返り値
   */
  public CobolGoBackException(int returnCode) {
    this.returnCode = returnCode;
  }

  /**
   * コンストラクタ
   *
   * @param storage 返り値を格納したCobolDataStorageのインスタンス
   */
  public CobolGoBackException(CobolDataStorage storage) {
    this(storage.intValue());
  }

  /**
   * returnCodeのgetter
   *
   * @return this.returnCode
   */
  public int getReturnCode() {
    return returnCode;
  }

  /**
   * CobolGoBackExceptionを例外として投げる
   *
   * @param returnCode 返り値
   * @throws CobolGoBackException
   */
  public static void throwException(int returnCode) throws CobolGoBackException {
    throw new CobolGoBackException(returnCode);
  }

  /**
   * CobolGoBackExceptionを例外として投げる
   *
   * @param returnCode 返り値を格納したCobolDataStorageのインスタンス
   * @throws CobolGoBackException
   */
  public static void throwException(CobolDataStorage storage) throws CobolGoBackException {
    throw new CobolGoBackException(storage);
  }

  /**
   * javaコンパイラの解析でthrowが発生しない部分がtry節でくくられていると判断されるとコンパイルエラーになる. これを回避するためのダミーのメソッド
   *
   * @throws CobolGoBackException
   */
  public static void dummy() throws CobolGoBackException {
    if (false) throw new CobolGoBackException(0);
  }
}
