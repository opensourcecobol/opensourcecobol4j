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

import java.lang.reflect.Field;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolRuntimeException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class CobolResolveTest {

    @BeforeEach
    void resetCallStackState() throws Exception {
        Field headField = CobolResolve.class.getDeclaredField("callStackListHead");
        headField.setAccessible(true);
        headField.set(null, null);

        Field currentField = CobolResolve.class.getDeclaredField("currentCallStackList");
        currentField.setAccessible(true);
        currentField.set(null, null);
    }

    @Test
    void testCobExceptionMap() {
        assertEquals("EC-ALL", CobolResolve.cobException.get(0xFFFF));
        assertEquals("EC-ARGUMENT", CobolResolve.cobException.get(0x0100));
        assertEquals("EC-ARGUMENT-FUNCTION", CobolResolve.cobException.get(0x0101));
        assertEquals("EC-BOUND", CobolResolve.cobException.get(0x0200));
        assertEquals("EC-DATA", CobolResolve.cobException.get(0x0300));
        assertEquals("EC-FLOW", CobolResolve.cobException.get(0x0400));
        assertEquals("EC-I-O", CobolResolve.cobException.get(0x0500));
        assertEquals("EC-PROGRAM", CobolResolve.cobException.get(0x0B00));
        assertEquals("EC-PROGRAM-NOT-FOUND", CobolResolve.cobException.get(0x0B05));
        assertEquals("EC-SIZE", CobolResolve.cobException.get(0x1000));
        assertEquals("EC-SIZE-ZERO-DIVIDE", CobolResolve.cobException.get(0x1007));
    }

    @Test
    void testCobExceptionMapContainsAllExpectedCategories() {
        assertNotNull(CobolResolve.cobException.get(0x0100));
        assertNotNull(CobolResolve.cobException.get(0x0200));
        assertNotNull(CobolResolve.cobException.get(0x0300));
        assertNotNull(CobolResolve.cobException.get(0x0400));
        assertNotNull(CobolResolve.cobException.get(0x0500));
        assertNotNull(CobolResolve.cobException.get(0x0600));
        assertNotNull(CobolResolve.cobException.get(0x0700));
        assertNotNull(CobolResolve.cobException.get(0x0800));
        assertNotNull(CobolResolve.cobException.get(0x0900));
        assertNotNull(CobolResolve.cobException.get(0x0A00));
        assertNotNull(CobolResolve.cobException.get(0x0B00));
        assertNotNull(CobolResolve.cobException.get(0x0C00));
        assertNotNull(CobolResolve.cobException.get(0x0D00));
        assertNotNull(CobolResolve.cobException.get(0x0E00));
        assertNotNull(CobolResolve.cobException.get(0x0F00));
        assertNotNull(CobolResolve.cobException.get(0x1000));
        assertNotNull(CobolResolve.cobException.get(0x1100));
        assertNotNull(CobolResolve.cobException.get(0x1200));
        assertNotNull(CobolResolve.cobException.get(0x1300));
        assertNotNull(CobolResolve.cobException.get(0x1400));
        assertNotNull(CobolResolve.cobException.get(0x1500));
        assertNotNull(CobolResolve.cobException.get(0x1600));
    }

    @Test
    void testCobExceptionMapXmlCategory() {
        assertEquals("EC-XML", CobolResolve.cobException.get(0x1600));
        assertEquals("EC-XML-CODESET", CobolResolve.cobException.get(0x1601));
        assertEquals("EC-XML-CODESET-CONVERSION", CobolResolve.cobException.get(0x1602));
        assertEquals("EC-XML-COUNT", CobolResolve.cobException.get(0x1603));
        assertEquals("EC-XML-DOCUMENT-TYPE", CobolResolve.cobException.get(0x1604));
        assertEquals("EC-XML-IMPLICIT-CLOSE", CobolResolve.cobException.get(0x1605));
        assertEquals("EC-XML-INVALID", CobolResolve.cobException.get(0x1606));
        assertEquals("EC-XML-NAMESPACE", CobolResolve.cobException.get(0x1607));
        assertEquals("EC-XML-STACKED-OPEN", CobolResolve.cobException.get(0x1608));
        assertEquals("EC-XML-RANGE", CobolResolve.cobException.get(0x1609));
    }

    @Test
    void testCobExceptionMapBoundCategory() {
        assertEquals("EC-BOUND", CobolResolve.cobException.get(0x0200));
        assertEquals("EC-BOUND-IMP", CobolResolve.cobException.get(0x0201));
        assertEquals("EC-BOUND-ODO", CobolResolve.cobException.get(0x0202));
        assertEquals("EC-BOUND-OVERFLOW", CobolResolve.cobException.get(0x0203));
        assertEquals("EC-BOUND-PTR", CobolResolve.cobException.get(0x0204));
        assertEquals("EC-BOUND-REF-MOD", CobolResolve.cobException.get(0x0205));
        assertEquals("EC-BOUND-SET", CobolResolve.cobException.get(0x0206));
        assertEquals("EC-BOUND-SUBSCRIPT", CobolResolve.cobException.get(0x0207));
        assertEquals("EC-BOUND-TABLE-LIMIT", CobolResolve.cobException.get(0x0208));
    }

    @Test
    void testCobExceptionMapSizeCategory() {
        assertEquals("EC-SIZE", CobolResolve.cobException.get(0x1000));
        assertEquals("EC-SIZE-ADDRESS", CobolResolve.cobException.get(0x1001));
        assertEquals("EC-SIZE-EXPONENTIATION", CobolResolve.cobException.get(0x1002));
        assertEquals("EC-SIZE-IMP", CobolResolve.cobException.get(0x1003));
        assertEquals("EC-SIZE-OVERFLOW", CobolResolve.cobException.get(0x1004));
        assertEquals("EC-SIZE-TRUNCATION", CobolResolve.cobException.get(0x1005));
        assertEquals("EC-SIZE-UNDERFLOW", CobolResolve.cobException.get(0x1006));
        assertEquals("EC-SIZE-ZERO-DIVIDE", CobolResolve.cobException.get(0x1007));
    }

    @Test
    void testResolveWithNonNullRunner() throws CobolRuntimeException {
        MockCobolRunnable runner = new MockCobolRunnable();
        CobolRunnable result = CobolResolve.resolve("test.package", "TestProgram", runner);
        assertSame(runner, result);
    }

    @Test
    void testResolveWithExistingRunner() throws CobolRuntimeException {
        MockCobolRunnable existingRunner = new MockCobolRunnable();
        CobolRunnable result = CobolResolve.resolve(null, "AnyName", existingRunner);
        assertSame(existingRunner, result);
    }

    @Test
    void testCancelWithNullName() {
        assertThrows(
                CobolRuntimeException.class,
                () -> {
                    CobolResolve.cancel((String) null);
                });
    }

    @Test
    void testPushAndPopCallStackList() {
        assertDoesNotThrow(() -> {
            CobolResolve.pushCallStackList("PROGRAM1");
            CobolResolve.pushCallStackList("PROGRAM2");
            CobolResolve.popCallStackList();
            CobolResolve.popCallStackList();
        });
    }

    @Test
    void testPushCallStackListSameName() {
        assertDoesNotThrow(() -> {
            CobolResolve.pushCallStackList("PROGRAM1");
            CobolResolve.pushCallStackList("PROGRAM1");
            CobolResolve.popCallStackList();
            CobolResolve.popCallStackList();
        });
    }

    @Test
    void testPushCallStackListMultipleSiblings() {
        assertDoesNotThrow(() -> {
            CobolResolve.pushCallStackList("PROGRAM1");
            CobolResolve.popCallStackList();
            CobolResolve.pushCallStackList("PROGRAM2");
            CobolResolve.popCallStackList();
            CobolResolve.pushCallStackList("PROGRAM3");
            CobolResolve.popCallStackList();
        });
    }

    @Test
    void testPushCallStackListDeepNesting() {
        assertDoesNotThrow(() -> {
            CobolResolve.pushCallStackList("LEVEL1");
            CobolResolve.pushCallStackList("LEVEL2");
            CobolResolve.pushCallStackList("LEVEL3");
            CobolResolve.pushCallStackList("LEVEL4");
            CobolResolve.popCallStackList();
            CobolResolve.popCallStackList();
            CobolResolve.popCallStackList();
            CobolResolve.popCallStackList();
        });
    }

    @Test
    void testPushCallStackListMixedPattern() {
        assertDoesNotThrow(() -> {
            CobolResolve.pushCallStackList("MAIN");
            CobolResolve.pushCallStackList("SUB1");
            CobolResolve.popCallStackList();
            CobolResolve.pushCallStackList("SUB2");
            CobolResolve.pushCallStackList("SUB2A");
            CobolResolve.popCallStackList();
            CobolResolve.popCallStackList();
            CobolResolve.popCallStackList();
        });
    }

    @Test
    void testPopCallStackListOnEmpty() {
        assertDoesNotThrow(() -> {
            CobolResolve.popCallStackList();
            CobolResolve.popCallStackList();
        });
    }

    @Test
    void testCobExceptionMapIOCategory() {
        assertEquals("EC-I-O", CobolResolve.cobException.get(0x0500));
        assertEquals("EC-I-O-AT-END", CobolResolve.cobException.get(0x0501));
        assertEquals("EC-I-O-EOP", CobolResolve.cobException.get(0x0502));
        assertEquals("EC-I-O-EOP-OVERFLOW", CobolResolve.cobException.get(0x0503));
        assertEquals("EC-I-O-FILE-SHARING", CobolResolve.cobException.get(0x0504));
        assertEquals("EC-I-O-IMP", CobolResolve.cobException.get(0x0505));
        assertEquals("EC-I-O-INVALID-KEY", CobolResolve.cobException.get(0x0506));
        assertEquals("EC-I-O-LINAGE", CobolResolve.cobException.get(0x0507));
        assertEquals("EC-I-O-LOGIC-ERROR", CobolResolve.cobException.get(0x0508));
        assertEquals("EC-I-O-PERMANENT-ERROR", CobolResolve.cobException.get(0x0509));
        assertEquals("EC-I-O-RECORD-OPERATION", CobolResolve.cobException.get(0x050A));
    }

    @Test
    void testCobExceptionMapProgramCategory() {
        assertEquals("EC-PROGRAM", CobolResolve.cobException.get(0x0B00));
        assertEquals("EC-PROGRAM-ARG-MISMATCH", CobolResolve.cobException.get(0x0B01));
        assertEquals("EC-PROGRAM-ARG-OMITTED", CobolResolve.cobException.get(0x0B02));
        assertEquals("EC-PROGRAM-CANCEL-ACTIVE", CobolResolve.cobException.get(0x0B03));
        assertEquals("EC-PROGRAM-IMP", CobolResolve.cobException.get(0x0B04));
        assertEquals("EC-PROGRAM-NOT-FOUND", CobolResolve.cobException.get(0x0B05));
        assertEquals("EC-PROGRAM-PTR-NULL", CobolResolve.cobException.get(0x0B06));
        assertEquals("EC-PROGRAM-RECURSIVE-CALL", CobolResolve.cobException.get(0x0B07));
        assertEquals("EC-PROGRAM-RESOURCES", CobolResolve.cobException.get(0x0B08));
    }

    @Test
    void testCobExceptionMapDataCategory() {
        assertEquals("EC-DATA", CobolResolve.cobException.get(0x0300));
        assertEquals("EC-DATA-CONVERSION", CobolResolve.cobException.get(0x0301));
        assertEquals("EC-DATA-IMP", CobolResolve.cobException.get(0x0302));
        assertEquals("EC-DATA-INCOMPATIBLE", CobolResolve.cobException.get(0x0303));
        assertEquals("EC-DATA-INTEGRITY", CobolResolve.cobException.get(0x0304));
        assertEquals("EC-DATA-PTR-NULL", CobolResolve.cobException.get(0x0305));
        assertEquals("EC-DATA-INFINITY", CobolResolve.cobException.get(0x0306));
        assertEquals("EC-DATA-NEGATIVE-INFINITY", CobolResolve.cobException.get(0x0307));
        assertEquals("EC-DATA-NOT_A_NUMBER", CobolResolve.cobException.get(0x0308));
    }

    @Test
    void testCobExceptionMapTotalCount() {
        assertTrue(CobolResolve.cobException.size() > 100);
    }

    @Test
    void testResolveWithDifferentRunners() throws CobolRuntimeException {
        MockCobolRunnable runner1 = new MockCobolRunnable();
        MockCobolRunnable runner2 = new MockCobolRunnable();

        CobolRunnable result1 = CobolResolve.resolve("pkg1", "Prog1", runner1);
        CobolRunnable result2 = CobolResolve.resolve("pkg2", "Prog2", runner2);

        assertSame(runner1, result1);
        assertSame(runner2, result2);
        assertNotSame(result1, result2);
    }

    private static class MockCobolRunnable implements CobolRunnable {
        private boolean active = false;

        @Override
        public int run(CobolDataStorage... storages) {
            return 0;
        }

        @Override
        public void cancel() {
            active = false;
        }

        @Override
        public boolean isActive() {
            return active;
        }
    }
}
