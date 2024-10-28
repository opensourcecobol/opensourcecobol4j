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
package jp.osscons.opensourcecobol.libcobj.data;

/** TODO: 準備中 */
public class CobolNationalAllField extends CobolNationalField {
    /**
     * コンストラクタ
     *
     * @param size データを格納するバイト配列の長さ
     * @param dataStorage データを格納するバイト配列を扱うオブジェクト
     * @param attribute 変数に関する様々な情報を保持するオブジェクト
     */
    public CobolNationalAllField(
            int size, CobolDataStorage dataStorage, CobolFieldAttribute attribute) {
        super(size, dataStorage, attribute);
    }
}
