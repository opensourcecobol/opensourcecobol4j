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

import static org.junit.jupiter.api.Assertions.*;

import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldAttribute;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldFactory;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolRuntimeException;
import org.junit.jupiter.api.Test;

class CobolStringTest {

    private static AbstractCobolField makeField(int size, String value, int type, int digits) {
        CobolFieldAttribute attr = new CobolFieldAttribute(type, digits, 0, 0, null);
        CobolDataStorage storage = new CobolDataStorage(size);
        storage.memset((byte) ' ', size);
        if (value != null) {
            storage.memcpy(value, value.length());
        }
        return CobolFieldFactory.makeCobolField(size, storage, attr);
    }

    /**
     * STRING文の連結先が日本語項目で、WITH POINTERが項目の文字数を超えている場合。<br>
     * stringInitがバイト数との比較を2倍補正の前に行うため連結先の残り領域が負になるが、
     * 例外を送出せずオーバーフロー状態になることを確認する。
     */
    @Test
    void stringAppendIntoNationalWithPointerBeyondItemSetsOverflow() {
        // PIC N(5) 相当 (5文字 = 10バイト)
        AbstractCobolField dst = makeField(10, null, CobolFieldAttribute.COB_TYPE_NATIONAL, 0);
        AbstractCobolField ptr =
                makeField(2, "07", CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY, 2);
        AbstractCobolField src = makeField(4, "ABCD", CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0);

        CobolString.stringInit(dst, ptr);
        CobolString.stringDelimited(null);
        CobolString.stringAppend(src);
        CobolString.stringFinish();

        assertNotEquals(0, CobolRuntimeException.code);
        assertEquals(6, ptr.getInt());
        CobolRuntimeException.code = 0;
    }

    /** 連結先に収まる通常のSTRING文が連結先と文字位置を正しく更新することを確認する。 */
    @Test
    void stringAppendWithinDestinationUpdatesPointer() {
        AbstractCobolField dst = makeField(6, null, CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0);
        AbstractCobolField ptr =
                makeField(2, "02", CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY, 2);
        AbstractCobolField src = makeField(3, "XYZ", CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0);

        CobolString.stringInit(dst, ptr);
        CobolString.stringDelimited(null);
        CobolString.stringAppend(src);
        CobolString.stringFinish();

        assertEquals(0, CobolRuntimeException.code);
        assertEquals(" XYZ  ", dst.getString());
        assertEquals(5, ptr.getInt());
    }
}
