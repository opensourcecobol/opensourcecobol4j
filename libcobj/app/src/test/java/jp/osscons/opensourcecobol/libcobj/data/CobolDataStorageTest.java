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

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

/** memcpy / memset の各オーバーロードが相対位置(index)を維持したまま動作することを確認する。 */
class CobolDataStorageTest {

    /** 全バイトが0xFFで埋められたバイト配列を返す。書き込み範囲外が変化していないことの検出に使う。 */
    private static byte[] filledArray(int size) {
        byte[] array = new byte[size];
        java.util.Arrays.fill(array, (byte) 0xFF);
        return array;
    }

    @Test
    void memcpyByteArrayRespectsIndex() {
        byte[] data = filledArray(8);
        CobolDataStorage storage = new CobolDataStorage(data, 2);
        storage.memcpy(new byte[] {1, 2, 3, 4}, 3);
        assertArrayEquals(
                new byte[] {
                    (byte) 0xFF, (byte) 0xFF, 1, 2, 3, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF
                },
                data);
    }

    @Test
    void memcpyByteArrayWithOffsetRespectsIndex() {
        byte[] data = filledArray(8);
        CobolDataStorage storage = new CobolDataStorage(data, 2);
        storage.memcpy(3, new byte[] {1, 2, 3, 4}, 2);
        assertArrayEquals(
                new byte[] {
                    (byte) 0xFF,
                    (byte) 0xFF,
                    (byte) 0xFF,
                    (byte) 0xFF,
                    (byte) 0xFF,
                    1,
                    2,
                    (byte) 0xFF
                },
                data);
    }

    @Test
    void memcpyStorageRespectsBothIndexes() {
        byte[] srcData = new byte[] {9, 8, 7, 6, 5};
        byte[] dstData = filledArray(6);
        CobolDataStorage src = new CobolDataStorage(srcData, 1);
        CobolDataStorage dst = new CobolDataStorage(dstData, 2);
        dst.memcpy(src, 3);
        assertArrayEquals(new byte[] {(byte) 0xFF, (byte) 0xFF, 8, 7, 6, (byte) 0xFF}, dstData);
    }

    @Test
    void memcpyStorageWithOffsetRespectsBothIndexes() {
        byte[] srcData = new byte[] {9, 8, 7, 6, 5};
        byte[] dstData = filledArray(8);
        CobolDataStorage src = new CobolDataStorage(srcData, 2);
        CobolDataStorage dst = new CobolDataStorage(dstData, 1);
        dst.memcpy(4, src, 2);
        assertArrayEquals(
                new byte[] {
                    (byte) 0xFF,
                    (byte) 0xFF,
                    (byte) 0xFF,
                    (byte) 0xFF,
                    (byte) 0xFF,
                    7,
                    6,
                    (byte) 0xFF
                },
                dstData);
    }

    @Test
    void memcpyStringEncodesWithShiftJis() {
        byte[] data = filledArray(4);
        CobolDataStorage storage = new CobolDataStorage(data, 1);
        storage.memcpy("あ", 2);
        assertArrayEquals(new byte[] {(byte) 0xFF, (byte) 0x82, (byte) 0xA0, (byte) 0xFF}, data);
    }

    /** memcpy(byte[], int, int)のoffsetはコピー元、memcpy(int, byte[], int)のoffsetはコピー先を指す。 */
    @Test
    void memcpyByteArrayTrailingOffsetIsSourceOffset() {
        byte[] data = filledArray(5);
        CobolDataStorage storage = new CobolDataStorage(data, 1);
        storage.memcpy(new byte[] {1, 2, 3, 4}, 2, 2);
        assertArrayEquals(new byte[] {(byte) 0xFF, 3, 4, (byte) 0xFF, (byte) 0xFF}, data);
    }

    @Test
    void memcpyByteArrayCopiesWholeArray() {
        byte[] data = filledArray(5);
        CobolDataStorage storage = new CobolDataStorage(data, 2);
        storage.memcpy(new byte[] {1, 2, 3});
        assertArrayEquals(new byte[] {(byte) 0xFF, (byte) 0xFF, 1, 2, 3}, data);
    }

    @Test
    void memsetRespectsIndex() {
        byte[] data = filledArray(6);
        CobolDataStorage storage = new CobolDataStorage(data, 2);
        storage.memset((byte) ' ', 3);
        assertArrayEquals(
                new byte[] {(byte) 0xFF, (byte) 0xFF, 0x20, 0x20, 0x20, (byte) 0xFF}, data);
    }

    @Test
    void memsetWithOffsetRespectsIndex() {
        byte[] data = filledArray(8);
        CobolDataStorage storage = new CobolDataStorage(data, 2);
        storage.memset(3, (byte) '0', 2);
        assertArrayEquals(
                new byte[] {
                    (byte) 0xFF,
                    (byte) 0xFF,
                    (byte) 0xFF,
                    (byte) 0xFF,
                    (byte) 0xFF,
                    (byte) '0',
                    (byte) '0',
                    (byte) 0xFF
                },
                data);
    }

    @Test
    void memsetWithIntValueTruncatesToByte() {
        byte[] data = filledArray(3);
        CobolDataStorage storage = new CobolDataStorage(data, 0);
        storage.memset(0x141, 2);
        assertArrayEquals(new byte[] {0x41, 0x41, (byte) 0xFF}, data);
    }

    @Test
    void memcpyOfZeroSizeChangesNothing() {
        byte[] data = filledArray(3);
        CobolDataStorage storage = new CobolDataStorage(data, 3);
        storage.memcpy(new byte[] {1}, 0);
        storage.memset((byte) 0, 0);
        assertArrayEquals(filledArray(3), data);
    }

    @Test
    void memcpyBetweenOverlappingRegionsMovesBytes() {
        byte[] data = new byte[] {1, 2, 3, 4, 5, 6};
        CobolDataStorage src = new CobolDataStorage(data, 0);
        CobolDataStorage dst = new CobolDataStorage(data, 2);
        dst.memcpy(src, 4);
        assertArrayEquals(new byte[] {1, 2, 1, 2, 3, 4}, data);
    }
}
