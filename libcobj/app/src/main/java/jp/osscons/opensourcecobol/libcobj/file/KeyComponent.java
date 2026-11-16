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
package jp.osscons.opensourcecobol.libcobj.file;

import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;

/**
 * INDEXEDファイルのSPLIT KEYを構成する1つの要素を表す。
 *
 * <p>COBOLでは、レコードキーを連続しない複数のフィールドから構成できる（SPLIT KEY。例：{@code RECORD KEY IS K = F1 F2
 * ...}）。その構成フィールド1つ1つが{@code KeyComponent}で表現される。コンパイラ（{@code
 * cobj}）は、2つ以上の要素を持つキーごとに{@code KeyComponent}の配列を生成し、{@link
 * CobolFileKey#setComponent(KeyComponent[])}を通じて所有者である{@link CobolFileKey}に格納する。
 */
public class KeyComponent {
    /** このキー要素を構成するCOBOLフィールド。 */
    public AbstractCobolField field;

    /** レコード先頭からのこの要素の0始まりのバイトオフセット。 */
    public int rb;
}
