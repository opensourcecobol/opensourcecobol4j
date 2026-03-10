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

/** エラーコードを保持する。 */
public class CobolExceptionInfo {
    /**
     * 現在のエラーコード。CobolExceptionTabCode.codeテーブルから取得した16進数のエラーコードが格納される。
     * CobolRuntimeExceptionとは異なりコンテキスト情報は保持せず、エラーコードのみを管理する。
     * 主にACCEPT文やDISPLAY文の実装固有エラーの判定に使用される。
     */
    public static int code = 0;

    /**
     * エラーコードを設定する。指定された例外IDに対応するエラーコードをCobolExceptionTabCode.codeテーブルから取得し、codeフィールドに設定する。
     *
     * @param id CobolExceptionIdで定義された例外ID
     */
    public static void setException(int id) {
        CobolExceptionInfo.code = CobolExceptionTabCode.code[id];
    }
}
