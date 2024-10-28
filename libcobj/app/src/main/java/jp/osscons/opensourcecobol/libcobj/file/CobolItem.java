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

/** TODO: 準備中 */
class CobolItem {
    private CobolItem next;
    private int endOfBlock;
    private int recordSize;
    private byte blockByte = 0;
    private CobolDataStorage unique = new CobolDataStorage(new byte[8]);
    private CobolDataStorage item;

    /** TODO: 準備中 */
    CobolItem() {
        this.next = null;
        this.endOfBlock = 0;
    }

    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     */
    CobolItem getNext() {
        return next;
    }

    /**
     * TODO: 準備中
     *
     * @param next TODO: 準備中
     */
    void setNext(CobolItem next) {
        this.next = next;
    }

    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     */
    int getEndOfBlock() {
        return endOfBlock;
    }

    /**
     * TODO: 準備中
     *
     * @param endOfBlock TODO: 準備中
     */
    void setEndOfBlock(int endOfBlock) {
        this.endOfBlock = endOfBlock;
    }

    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     */
    int getRecordSize() {
        return recordSize;
    }

    /**
     * TODO: 準備中
     *
     * @param recordSize TODO: 準備中
     */
    void setRecordSize(int recordSize) {
        this.recordSize = recordSize;
    }

    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     */
    byte getBlockByte() {
        return blockByte;
    }

    /**
     * TODO: 準備中
     *
     * @param blockByte TODO: 準備中
     */
    void setBlockByte(byte blockByte) {
        this.blockByte = blockByte;
    }

    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     */
    CobolDataStorage getUnique() {
        return unique;
    }

    /**
     * TODO: 準備中
     *
     * @param unique TODO: 準備中
     */
    void setUnique(CobolDataStorage unique) {
        this.unique = unique;
    }

    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     */
    CobolDataStorage getItem() {
        return item;
    }

    /**
     * TODO: 準備中
     *
     * @param item TODO: 準備中
     */
    void setItem(CobolDataStorage item) {
        this.item = item;
    }
}
