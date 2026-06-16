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
package jp.osscons.opensourcecobol.libcobj.common;

/**
 * CALL文で呼び出されたプログラムに渡された引数の個数を保持するクラス<br>
 * libcobのcob_call_paramsに対応し、USING句で渡された実引数の個数を表す。<br>
 * 呼び出されたプログラム側でこの値を参照することで、省略された引数の判定などに利用する。
 */
public class CobolCallParams {
    /** CALL文で渡された引数の個数 */
    public static int callParams = 0;
}
