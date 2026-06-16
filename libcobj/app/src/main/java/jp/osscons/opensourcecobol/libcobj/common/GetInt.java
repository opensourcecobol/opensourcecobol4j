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

import jp.osscons.opensourcecobol.libcobj.exceptions.CobolStopRunException;

/**
 * int型の値を返す処理を遅延実行するための関数型インターフェイス<br>
 * 生成されたJavaコード中で、int型の値を後から評価したい箇所(コールバック)を表現するために用いる。
 */
public interface GetInt {
    /**
     * 処理を実行し、結果のint型の値を返す。
     *
     * @return 処理結果のint型の値
     * @throws CobolStopRunException 処理中にSTOP RUNが実行された場合
     */
    int run() throws CobolStopRunException;
}
