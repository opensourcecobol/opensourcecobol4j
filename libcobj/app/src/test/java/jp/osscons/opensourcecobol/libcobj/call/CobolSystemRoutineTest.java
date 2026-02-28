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
package jp.osscons.opensourcecobol.libcobj.call;

import static org.junit.jupiter.api.Assertions.*;

import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

class CobolSystemRoutineTest {

    @Test
    void testCBL_AND() {
        CobolDataStorage data1 = new CobolDataStorage(new byte[] {(byte) 0xFF, (byte) 0x0F});
        CobolDataStorage data2 = new CobolDataStorage(new byte[] {(byte) 0xF0, (byte) 0xFF});

        int result = CobolSystemRoutine.CBL_AND(data1, data2, 2);

        assertEquals(0, result);
        assertEquals((byte) 0xF0, data2.getByte(0));
        assertEquals((byte) 0x0F, data2.getByte(1));
    }

    @Test
    void testCBL_AND_ZeroLength() {
        CobolDataStorage data1 = new CobolDataStorage(new byte[] {(byte) 0xFF});
        CobolDataStorage data2 = new CobolDataStorage(new byte[] {(byte) 0x0F});

        int result = CobolSystemRoutine.CBL_AND(data1, data2, 0);

        assertEquals(0, result);
        assertEquals((byte) 0x0F, data2.getByte(0));
    }

    @Test
    void testCBL_AND_NegativeLength() {
        CobolDataStorage data1 = new CobolDataStorage(new byte[] {(byte) 0xFF});
        CobolDataStorage data2 = new CobolDataStorage(new byte[] {(byte) 0x0F});

        int result = CobolSystemRoutine.CBL_AND(data1, data2, -1);

        assertEquals(0, result);
        assertEquals((byte) 0x0F, data2.getByte(0));
    }

    @Test
    void testCBL_OR() {
        CobolDataStorage data1 = new CobolDataStorage(new byte[] {(byte) 0xF0, (byte) 0x0F});
        CobolDataStorage data2 = new CobolDataStorage(new byte[] {(byte) 0x0F, (byte) 0xF0});

        int result = CobolSystemRoutine.CBL_OR(data1, data2, 2);

        assertEquals(0, result);
        assertEquals((byte) 0xFF, data2.getByte(0));
        assertEquals((byte) 0xFF, data2.getByte(1));
    }

    @Test
    void testCBL_OR_ZeroLength() {
        CobolDataStorage data1 = new CobolDataStorage(new byte[] {(byte) 0xFF});
        CobolDataStorage data2 = new CobolDataStorage(new byte[] {(byte) 0x00});

        int result = CobolSystemRoutine.CBL_OR(data1, data2, 0);

        assertEquals(0, result);
        assertEquals((byte) 0x00, data2.getByte(0));
    }

    @Test
    void testCBL_XOR() {
        CobolDataStorage data1 = new CobolDataStorage(new byte[] {(byte) 0xFF, (byte) 0xAA});
        CobolDataStorage data2 = new CobolDataStorage(new byte[] {(byte) 0xFF, (byte) 0x55});

        int result = CobolSystemRoutine.CBL_XOR(data1, data2, 2);

        assertEquals(0, result);
        assertEquals((byte) 0x00, data2.getByte(0));
        assertEquals((byte) 0xFF, data2.getByte(1));
    }

    @Test
    void testCBL_XOR_ZeroLength() {
        CobolDataStorage data1 = new CobolDataStorage(new byte[] {(byte) 0xFF});
        CobolDataStorage data2 = new CobolDataStorage(new byte[] {(byte) 0xAA});

        int result = CobolSystemRoutine.CBL_XOR(data1, data2, 0);

        assertEquals(0, result);
        assertEquals((byte) 0xAA, data2.getByte(0));
    }

    @Test
    void testCBL_NOR() {
        CobolDataStorage data1 = new CobolDataStorage(new byte[] {(byte) 0x00});
        CobolDataStorage data2 = new CobolDataStorage(new byte[] {(byte) 0x00});

        int result = CobolSystemRoutine.CBL_NOR(data1, data2, 1);

        assertEquals(0, result);
        assertEquals((byte) 0xFF, data2.getByte(0));
    }

    @Test
    void testCBL_NOR_AllOnes() {
        CobolDataStorage data1 = new CobolDataStorage(new byte[] {(byte) 0xFF});
        CobolDataStorage data2 = new CobolDataStorage(new byte[] {(byte) 0xFF});

        int result = CobolSystemRoutine.CBL_NOR(data1, data2, 1);

        assertEquals(0, result);
        assertEquals((byte) 0x00, data2.getByte(0));
    }

    @Test
    void testCBL_NIMP() {
        CobolDataStorage data1 = new CobolDataStorage(new byte[] {(byte) 0xFF});
        CobolDataStorage data2 = new CobolDataStorage(new byte[] {(byte) 0x0F});

        int result = CobolSystemRoutine.CBL_NIMP(data1, data2, 1);

        assertEquals(0, result);
        assertEquals((byte) 0xF0, data2.getByte(0));
    }

    @Test
    void testCBL_NIMP_ZeroLength() {
        CobolDataStorage data1 = new CobolDataStorage(new byte[] {(byte) 0xFF});
        CobolDataStorage data2 = new CobolDataStorage(new byte[] {(byte) 0x0F});

        int result = CobolSystemRoutine.CBL_NIMP(data1, data2, 0);

        assertEquals(0, result);
        assertEquals((byte) 0x0F, data2.getByte(0));
    }

    @Test
    void testCBL_EQ() {
        CobolDataStorage data1 = new CobolDataStorage(new byte[] {(byte) 0xAA});
        CobolDataStorage data2 = new CobolDataStorage(new byte[] {(byte) 0xAA});

        int result = CobolSystemRoutine.CBL_EQ(data1, data2, 1);

        assertEquals(0, result);
        assertEquals((byte) 0xFF, data2.getByte(0));
    }

    @Test
    void testCBL_EQ_Different() {
        CobolDataStorage data1 = new CobolDataStorage(new byte[] {(byte) 0xAA});
        CobolDataStorage data2 = new CobolDataStorage(new byte[] {(byte) 0x55});

        int result = CobolSystemRoutine.CBL_EQ(data1, data2, 1);

        assertEquals(0, result);
        assertEquals((byte) 0x00, data2.getByte(0));
    }

    @Test
    void testCBL_NOT() {
        CobolDataStorage data = new CobolDataStorage(new byte[] {(byte) 0xAA, (byte) 0x55});

        int result = CobolSystemRoutine.CBL_NOT(data, 2);

        assertEquals(0, result);
        assertEquals((byte) 0x55, data.getByte(0));
        assertEquals((byte) 0xAA, data.getByte(1));
    }

    @Test
    void testCBL_NOT_ZeroLength() {
        CobolDataStorage data = new CobolDataStorage(new byte[] {(byte) 0xAA});

        int result = CobolSystemRoutine.CBL_NOT(data, 0);

        assertEquals(0, result);
        assertEquals((byte) 0xAA, data.getByte(0));
    }

    @Test
    void testCBL_NOT_AllZeros() {
        CobolDataStorage data = new CobolDataStorage(new byte[] {(byte) 0x00});

        int result = CobolSystemRoutine.CBL_NOT(data, 1);

        assertEquals(0, result);
        assertEquals((byte) 0xFF, data.getByte(0));
    }

    @Test
    void testCBL_NOT_AllOnes() {
        CobolDataStorage data = new CobolDataStorage(new byte[] {(byte) 0xFF});

        int result = CobolSystemRoutine.CBL_NOT(data, 1);

        assertEquals(0, result);
        assertEquals((byte) 0x00, data.getByte(0));
    }

    @Test
    void testCBL_XF4() {
        CobolDataStorage data1 = new CobolDataStorage(new byte[] {0, 0, 0, 0, 0, 0, 0, 0});
        CobolDataStorage data2 =
                new CobolDataStorage(
                        new byte[] {
                            (byte) 1, (byte) 0, (byte) 1, (byte) 0, (byte) 1, (byte) 0, (byte) 1,
                            (byte) 0
                        });

        int result = CobolSystemRoutine.CBL_XF4(data1, data2);

        assertEquals(0, result);
        assertEquals((byte) 0x80, data1.getByte(0));
        assertEquals((byte) 0x00, data1.getByte(1));
        assertEquals((byte) 0x20, data1.getByte(2));
        assertEquals((byte) 0x00, data1.getByte(3));
        assertEquals((byte) 0x08, data1.getByte(4));
        assertEquals((byte) 0x00, data1.getByte(5));
        assertEquals((byte) 0x02, data1.getByte(6));
        assertEquals((byte) 0x00, data1.getByte(7));
    }

    @Test
    void testCBL_XF5() {
        CobolDataStorage data1 = new CobolDataStorage(new byte[] {(byte) 0xAA});
        CobolDataStorage data2 = new CobolDataStorage(new byte[] {0, 0, 0, 0, 0, 0, 0, 0});

        int result = CobolSystemRoutine.CBL_XF5(data1, data2);

        assertEquals(0, result);
        assertEquals((byte) 1, data2.getByte(0));
        assertEquals((byte) 0, data2.getByte(1));
        assertEquals((byte) 1, data2.getByte(2));
        assertEquals((byte) 0, data2.getByte(3));
        assertEquals((byte) 1, data2.getByte(4));
        assertEquals((byte) 0, data2.getByte(5));
        assertEquals((byte) 1, data2.getByte(6));
        assertEquals((byte) 0, data2.getByte(7));
    }

    @Test
    void testCBL_TOLOWER() {
        CobolDataStorage data = new CobolDataStorage(new byte[] {'A', 'B', 'C'});

        int result = CobolSystemRoutine.CBL_TOLOWER(data, 3);

        assertEquals(0, result);
        assertEquals((byte) 'a', data.getByte(0));
        assertEquals((byte) 'b', data.getByte(1));
        assertEquals((byte) 'c', data.getByte(2));
    }

    @Test
    void testCBL_TOLOWER_ZeroLength() {
        CobolDataStorage data = new CobolDataStorage(new byte[] {'A'});

        int result = CobolSystemRoutine.CBL_TOLOWER(data, 0);

        assertEquals(0, result);
        assertEquals((byte) 'A', data.getByte(0));
    }

    @Test
    void testCBL_TOLOWER_AlreadyLower() {
        CobolDataStorage data = new CobolDataStorage(new byte[] {'a', 'b', 'c'});

        int result = CobolSystemRoutine.CBL_TOLOWER(data, 3);

        assertEquals(0, result);
        assertEquals((byte) 'a', data.getByte(0));
        assertEquals((byte) 'b', data.getByte(1));
        assertEquals((byte) 'c', data.getByte(2));
    }

    @Test
    void testCBL_TOUPPER() {
        CobolDataStorage data = new CobolDataStorage(new byte[] {'a', 'b', 'c'});

        int result = CobolSystemRoutine.CBL_TOUPPER(data, 3);

        assertEquals(0, result);
        assertEquals((byte) 'A', data.getByte(0));
        assertEquals((byte) 'B', data.getByte(1));
        assertEquals((byte) 'C', data.getByte(2));
    }

    @Test
    void testCBL_TOUPPER_ZeroLength() {
        CobolDataStorage data = new CobolDataStorage(new byte[] {'a'});

        int result = CobolSystemRoutine.CBL_TOUPPER(data, 0);

        assertEquals(0, result);
        assertEquals((byte) 'a', data.getByte(0));
    }

    @Test
    void testCBL_TOUPPER_AlreadyUpper() {
        CobolDataStorage data = new CobolDataStorage(new byte[] {'A', 'B', 'C'});

        int result = CobolSystemRoutine.CBL_TOUPPER(data, 3);

        assertEquals(0, result);
        assertEquals((byte) 'A', data.getByte(0));
        assertEquals((byte) 'B', data.getByte(1));
        assertEquals((byte) 'C', data.getByte(2));
    }

    @Test
    void testCBL_X91_SetSwitches() {
        CobolDataStorage result = new CobolDataStorage(new byte[] {(byte) 0xFF});
        CobolDataStorage func = new CobolDataStorage(new byte[] {(byte) 11});
        CobolDataStorage parm = new CobolDataStorage(new byte[] {1, 0, 1, 0, 1, 0, 1, 0});

        int ret = CobolSystemRoutine.CBL_X91(result, func, parm);

        assertEquals(0, ret);
        assertEquals((byte) 0, result.getByte(0));
    }

    @Test
    void testCBL_X91_GetSwitches() {
        CobolDataStorage result = new CobolDataStorage(new byte[] {(byte) 0xFF});
        CobolDataStorage func = new CobolDataStorage(new byte[] {(byte) 12});
        CobolDataStorage parm = new CobolDataStorage(new byte[] {0, 0, 0, 0, 0, 0, 0, 0});

        int ret = CobolSystemRoutine.CBL_X91(result, func, parm);

        assertEquals(0, ret);
        assertEquals((byte) 0, result.getByte(0));
    }

    @Test
    void testCBL_X91_GetCallParams() {
        CobolDataStorage result = new CobolDataStorage(new byte[] {(byte) 0xFF});
        CobolDataStorage func = new CobolDataStorage(new byte[] {(byte) 16});
        CobolDataStorage parm = new CobolDataStorage(new byte[] {0});

        int ret = CobolSystemRoutine.CBL_X91(result, func, parm);

        assertEquals(0, ret);
        assertEquals((byte) 0, result.getByte(0));
    }

    @Test
    void testCBL_X91_UnknownFunction() {
        CobolDataStorage result = new CobolDataStorage(new byte[] {(byte) 0});
        CobolDataStorage func = new CobolDataStorage(new byte[] {(byte) 99});
        CobolDataStorage parm = new CobolDataStorage(new byte[] {0});

        int ret = CobolSystemRoutine.CBL_X91(result, func, parm);

        assertEquals(0, ret);
        assertEquals((byte) 1, result.getByte(0));
    }

    @Disabled("Environment-dependent: executes real shell commands")
    @Test
    void testSYSTEM_String() {
        int result = CobolSystemRoutine.SYSTEM("echo test");
        assertEquals(0, result);
    }

    @Disabled("Environment-dependent: executes real shell commands")
    @Test
    void testSYSTEM_InvalidCommand() {
        int result = CobolSystemRoutine.SYSTEM("command_that_does_not_exist_xyz123");
        assertNotEquals(0, result);
    }

    @Test
    void testChainedBitwiseOperations() {
        CobolDataStorage data1 = new CobolDataStorage(new byte[] {(byte) 0xFF});
        CobolDataStorage data2 = new CobolDataStorage(new byte[] {(byte) 0x0F});
        CobolDataStorage data3 = new CobolDataStorage(new byte[] {(byte) 0x00});

        CobolSystemRoutine.CBL_AND(data1, data2, 1);
        assertEquals((byte) 0x0F, data2.getByte(0));

        CobolSystemRoutine.CBL_OR(data2, data3, 1);
        assertEquals((byte) 0x0F, data3.getByte(0));

        CobolSystemRoutine.CBL_XOR(data1, data3, 1);
        assertEquals((byte) 0xF0, data3.getByte(0));
    }

    @Test
    void testBitwiseOperationsLargeArray() {
        int size = 256;
        byte[] largeData1 = new byte[size];
        byte[] largeData2 = new byte[size];
        for (int i = 0; i < size; i++) {
            largeData1[i] = (byte) 0xAA;
            largeData2[i] = (byte) 0x55;
        }

        CobolDataStorage data1 = new CobolDataStorage(largeData1);
        CobolDataStorage data2 = new CobolDataStorage(largeData2);

        int result = CobolSystemRoutine.CBL_XOR(data1, data2, size);

        assertEquals(0, result);
        assertEquals((byte) 0xFF, data2.getByte(0));
        assertEquals((byte) 0xFF, data2.getByte(size / 2));
        assertEquals((byte) 0xFF, data2.getByte(size - 1));
    }

    @Test
    void testCBL_AND_PartialLength() {
        CobolDataStorage data1 =
                new CobolDataStorage(new byte[] {(byte) 0xFF, (byte) 0xFF, (byte) 0xFF});
        CobolDataStorage data2 =
                new CobolDataStorage(new byte[] {(byte) 0x0F, (byte) 0xF0, (byte) 0xAA});

        int result = CobolSystemRoutine.CBL_AND(data1, data2, 2);

        assertEquals(0, result);
        assertEquals((byte) 0x0F, data2.getByte(0));
        assertEquals((byte) 0xF0, data2.getByte(1));
        assertEquals((byte) 0xAA, data2.getByte(2));
    }

    @Test
    void testCBL_NOT_MultipleBytes() {
        CobolDataStorage data =
                new CobolDataStorage(
                        new byte[] {(byte) 0x00, (byte) 0xFF, (byte) 0xAA, (byte) 0x55});

        int result = CobolSystemRoutine.CBL_NOT(data, 4);

        assertEquals(0, result);
        assertEquals((byte) 0xFF, data.getByte(0));
        assertEquals((byte) 0x00, data.getByte(1));
        assertEquals((byte) 0x55, data.getByte(2));
        assertEquals((byte) 0xAA, data.getByte(3));
    }

    @Test
    void testCBL_TOLOWER_MixedCase() {
        CobolDataStorage data = new CobolDataStorage(new byte[] {'H', 'e', 'L', 'l', 'O'});

        int result = CobolSystemRoutine.CBL_TOLOWER(data, 5);

        assertEquals(0, result);
        assertEquals((byte) 'h', data.getByte(0));
        assertEquals((byte) 'e', data.getByte(1));
        assertEquals((byte) 'l', data.getByte(2));
        assertEquals((byte) 'l', data.getByte(3));
        assertEquals((byte) 'o', data.getByte(4));
    }

    @Test
    void testCBL_TOUPPER_MixedCase() {
        CobolDataStorage data = new CobolDataStorage(new byte[] {'H', 'e', 'L', 'l', 'O'});

        int result = CobolSystemRoutine.CBL_TOUPPER(data, 5);

        assertEquals(0, result);
        assertEquals((byte) 'H', data.getByte(0));
        assertEquals((byte) 'E', data.getByte(1));
        assertEquals((byte) 'L', data.getByte(2));
        assertEquals((byte) 'L', data.getByte(3));
        assertEquals((byte) 'O', data.getByte(4));
    }

    @Test
    void testCBL_EQ_MultipleBytes() {
        CobolDataStorage data1 =
                new CobolDataStorage(new byte[] {(byte) 0xAA, (byte) 0x55, (byte) 0xFF});
        CobolDataStorage data2 =
                new CobolDataStorage(new byte[] {(byte) 0xAA, (byte) 0xAA, (byte) 0x00});

        int result = CobolSystemRoutine.CBL_EQ(data1, data2, 3);

        assertEquals(0, result);
        assertEquals((byte) 0xFF, data2.getByte(0));
        assertEquals((byte) 0x00, data2.getByte(1));
        assertEquals((byte) 0x00, data2.getByte(2));
    }
}
