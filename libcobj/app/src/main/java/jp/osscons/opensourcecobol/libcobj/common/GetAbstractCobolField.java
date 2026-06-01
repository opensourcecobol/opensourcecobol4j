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

import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolStopRunException;

/**
 * {@link AbstractCobolField}を返す処理を遅延実行するための関数型インターフェイス<br>
 * 生成されたJavaコード中で、COBOLフィールドを後から評価したい箇所(コールバック)を表現するために用いる。
 */
public interface GetAbstractCobolField {
    /**
     * 処理を実行し、結果の{@link AbstractCobolField}を返す。
     *
     * @return 処理結果のCOBOLフィールド
     * @throws CobolStopRunException 処理中にSTOP RUNが実行された場合
     */
    AbstractCobolField run() throws CobolStopRunException;
}
