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
 * INDEXEDファイルの1つのキー（主キーまたは副キー）を表す。
 *
 * <p>コンパイラ（{@code cobj}）は、{@code SELECT}句で宣言された{@code RECORD KEY} / {@code ALTERNATE RECORD
 * KEY}ごとに{@code CobolFileKey}を1つ生成し、その配列を{@link
 * CobolFileFactory#makeCobolFileInstance}に渡す。各要素は、キー値を保持するCOBOLフィールド、重複フラグ、レコード内でのキーのバイトオフセット、そしてキーがSPLIT
 * KEYの場合はそれを構成する{@link KeyComponent}のリストを保持する。
 *
 * <p>{@link
 * CobolIndexedFile}が利用するSQLiteストレージでは、インデックス{@code 0}の要素が主キー（{@code table0}に格納）であり、インデックス{@code
 * i >= 1}の要素が副キー（{@code tableI}に格納）である。
 */
public class CobolFileKey {
    /** 1つのSPLIT KEYを構成できる{@link KeyComponent}の最大数。 */
    public static final int COB_MAX_KEY_COMPONENTS = 8;

    private AbstractCobolField field;
    private int flag;
    private int offset;
    private int countComponents;
    private KeyComponent[] component = new KeyComponent[COB_MAX_KEY_COMPONENTS];

    /**
     * このキーの値を保持するCOBOLフィールドを返す。
     *
     * @return キーフィールド
     */
    public AbstractCobolField getField() {
        return field;
    }

    /**
     * このキーの値を保持するCOBOLフィールドを設定する。
     *
     * @param field キーフィールド
     */
    public void setField(AbstractCobolField field) {
        this.field = field;
    }

    /**
     * このキーの重複フラグを返す。
     *
     * @return 重複値を許可しない場合（主キー、または{@code WITH DUPLICATES}なしの{@code ALTERNATE RECORD
     *     KEY}）は{@code 0}、重複値を許可する場合（{@code ALTERNATE RECORD KEY ... WITH
     *     DUPLICATES}）は非ゼロ値
     */
    public int getFlag() {
        return flag;
    }

    /**
     * このキーの重複フラグを設定する。
     *
     * @param flag 重複値を許可しない場合は{@code 0}、許可する場合は非ゼロ値
     */
    public void setFlag(int flag) {
        this.flag = flag;
    }

    /**
     * レコード内でのこのキーのバイトオフセットを返す。
     *
     * @return レコード先頭からの0始まりのバイトオフセット。キーがSPLIT KEYの場合は{@code
     *     -1}（その場合のレイアウトは{@link #getComponent()}で記述される）
     */
    public int getOffset() {
        return offset;
    }

    /**
     * レコード内でのこのキーのバイトオフセットを設定する。
     *
     * @param offset レコード先頭からの0始まりのバイトオフセット。SPLIT KEYの場合は{@code -1}
     */
    public void setOffset(int offset) {
        this.offset = offset;
    }

    /**
     * このキーを構成する{@link KeyComponent}の数を返す。
     *
     * @return {@link
     *     #getComponent()}が返す配列のうち有効な要素数。SPLIT KEYは2以上、通常のキーは0
     */
    public int getCountComponents() {
        return countComponents;
    }

    /**
     * このキーを構成する{@link KeyComponent}の数を設定する。
     *
     * @param countComponents 構成要素配列のうち有効な要素数
     */
    public void setCountComponents(int countComponents) {
        this.countComponents = countComponents;
    }

    /**
     * SPLIT KEYを記述する{@link KeyComponent}の配列を返す。
     *
     * <p>有効なのは先頭{@link #getCountComponents()}個の要素のみである。
     *
     * @return 構成要素配列（{@code null}にはならない。容量は{@link #COB_MAX_KEY_COMPONENTS}）
     */
    public KeyComponent[] getComponent() {
        return component;
    }

    /**
     * SPLIT KEYを記述する{@link KeyComponent}の配列を設定する。
     *
     * @param component 構成要素配列
     */
    public void setComponent(KeyComponent[] component) {
        this.component = component;
    }

    /**
     * SPLIT KEYを構成できる要素の最大数を返す。
     *
     * @return {@link #COB_MAX_KEY_COMPONENTS}の値
     */
    public static int getCobMaxKeyComponents() {
        return COB_MAX_KEY_COMPONENTS;
    }
}
