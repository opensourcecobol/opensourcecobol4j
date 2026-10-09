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

/** SORT/MERGE でメモリ上に読み込んだレコード(CobolItem)の連結リストを表す構造. 先頭・末尾・要素数を保持する. */
class MemoryStruct {
    private CobolItem first;
    private CobolItem last;
    private int count;

    /** 空の連結リストとして初期化する. 先頭と末尾をnullにする. */
    MemoryStruct() {
        this.first = null;
        this.last = null;
    }

    /**
     * 連結リストの先頭要素を取得する.
     *
     * @return 連結リストの先頭のCobolItem
     */
    CobolItem getFirst() {
        return first;
    }

    /**
     * 連結リストの先頭要素を設定する.
     *
     * @param first 連結リストの先頭とするCobolItem
     */
    void setFirst(CobolItem first) {
        this.first = first;
    }

    /**
     * 連結リストの末尾要素を取得する.
     *
     * @return 連結リストの末尾のCobolItem
     */
    CobolItem getLast() {
        return last;
    }

    /**
     * 連結リストの末尾要素を設定する.
     *
     * @param last 連結リストの末尾とするCobolItem
     */
    void setLast(CobolItem last) {
        this.last = last;
    }

    /**
     * 連結リストの要素数を取得する.
     *
     * @return 連結リストに含まれる要素数
     */
    int getCount() {
        return count;
    }

    /**
     * 連結リストの要素数を設定する.
     *
     * @param count 連結リストに含まれる要素数
     */
    void setCount(int count) {
        this.count = count;
    }
}
