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

import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;

/** SORT/MERGE でメモリ上に読み込んだ 1 レコードを表す連結リストの要素. */
class CobolItem {
    private CobolItem next;
    private int endOfBlock;
    private int recordSize;
    private byte blockByte = 0;
    private CobolDataStorage unique = new CobolDataStorage(new byte[8]);
    private CobolDataStorage item;

    /** 連結リストの次要素をnull,ブロック終端フラグを0にして初期化する. */
    CobolItem() {
        this.next = null;
        this.endOfBlock = 0;
    }

    /**
     * 連結リスト上で次に位置する要素を取得する.
     *
     * @return 次のCobolItem. 末尾の場合はnull
     */
    CobolItem getNext() {
        return next;
    }

    /**
     * 連結リスト上で次に位置する要素を設定する.
     *
     * @param next 次のCobolItem
     */
    void setNext(CobolItem next) {
        this.next = next;
    }

    /**
     * このレコードがブロックの終端であるかを表すフラグを取得する.
     *
     * @return ブロックの終端なら1,そうでなければ0
     */
    int getEndOfBlock() {
        return endOfBlock;
    }

    /**
     * このレコードがブロックの終端であるかを表すフラグを設定する.
     *
     * @param endOfBlock ブロックの終端なら1,そうでなければ0
     */
    void setEndOfBlock(int endOfBlock) {
        this.endOfBlock = endOfBlock;
    }

    /**
     * このレコードの実際のサイズ(可変長レコードの長さ)を取得する.
     *
     * @return レコードのサイズ(バイト数)
     */
    int getRecordSize() {
        return recordSize;
    }

    /**
     * このレコードの実際のサイズ(可変長レコードの長さ)を設定する.
     *
     * @param recordSize レコードのサイズ(バイト数)
     */
    void setRecordSize(int recordSize) {
        this.recordSize = recordSize;
    }

    /**
     * 一時ファイルへ書き込む際に各レコードの先頭へ付加するブロック区切り用のバイトを取得する.
     *
     * @return ブロック区切り用のバイト
     */
    byte getBlockByte() {
        return blockByte;
    }

    /**
     * 一時ファイルへ書き込む際に各レコードの先頭へ付加するブロック区切り用のバイトを設定する.
     *
     * @param blockByte ブロック区切り用のバイト
     */
    void setBlockByte(byte blockByte) {
        this.blockByte = blockByte;
    }

    /**
     * レコードの投入順を表す一意な番号(安定ソートのために使うキー)を取得する.
     *
     * @return 8バイトで表現された一意な番号を保持するCobolDataStorage
     */
    CobolDataStorage getUnique() {
        return unique;
    }

    /**
     * レコードの投入順を表す一意な番号(安定ソートのために使うキー)を設定する.
     *
     * @param unique 8バイトで表現された一意な番号を保持するCobolDataStorage
     */
    void setUnique(CobolDataStorage unique) {
        this.unique = unique;
    }

    /**
     * レコードのデータ本体を取得する.
     *
     * @return レコードのデータ本体を保持するCobolDataStorage
     */
    CobolDataStorage getItem() {
        return item;
    }

    /**
     * レコードのデータ本体を設定する.
     *
     * @param item レコードのデータ本体を保持するCobolDataStorage
     */
    void setItem(CobolDataStorage item) {
        this.item = item;
    }
}
