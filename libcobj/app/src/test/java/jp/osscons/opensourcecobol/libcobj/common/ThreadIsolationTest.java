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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import jp.osscons.opensourcecobol.libcobj.call.CobolResolve;
import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldAttribute;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldFactory;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolExceptionId;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolRuntimeException;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolStopRunException;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolStopRunSignal;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/** 実行単位の状態がスレッドごとに分離されていることを確認するテスト。 */
class ThreadIsolationTest {

    private static final int THREADS = 8;
    private static final int ITERATIONS = 200;

    @BeforeEach
    void setUp() {
        CobolUtil.ensureInitialized();
        CobolRunUnit.resetThreadState();
    }

    @AfterEach
    void tearDown() {
        CobolRunUnit.resetThreadState();
    }

    private static AbstractCobolField makeAlnum(String value, int size) {
        CobolFieldAttribute attr =
                new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null);
        CobolDataStorage storage = new CobolDataStorage(size);
        storage.memset((byte) ' ', size);
        storage.memcpy(value, value.length());
        return CobolFieldFactory.makeCobolField(size, storage, attr);
    }

    private static AbstractCobolField makeNumeric(int value, int digits) {
        CobolFieldAttribute attr =
                new CobolFieldAttribute(
                        CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY, digits, 0, 0, null);
        CobolDataStorage storage = new CobolDataStorage(digits);
        AbstractCobolField field = CobolFieldFactory.makeCobolField(digits, storage, attr);
        field.setInt(value);
        return field;
    }

    private static CobolModule makeModule(String programId) {
        CobolModule module =
                new CobolModule(null, null, null, null, 0, '.', '$', ',', 1, 1, 1, 0, null);
        module.setProgramId(programId);
        return module;
    }

    /** 全スレッドを同時に開始してそれぞれの結果を集める。 */
    private static <T> List<T> runConcurrently(Callable<T> task) throws Exception {
        ExecutorService executor = Executors.newFixedThreadPool(THREADS);
        CountDownLatch ready = new CountDownLatch(THREADS);
        CountDownLatch start = new CountDownLatch(1);
        List<Future<T>> futures = new ArrayList<>();
        try {
            for (int i = 0; i < THREADS; i++) {
                futures.add(
                        executor.submit(
                                () -> {
                                    ready.countDown();
                                    start.await();
                                    try {
                                        return task.call();
                                    } finally {
                                        CobolRunUnit.resetThreadState();
                                    }
                                }));
            }
            assertTrue(ready.await(30, TimeUnit.SECONDS), "workers did not start");
            start.countDown();
            List<T> results = new ArrayList<>();
            for (Future<T> f : futures) {
                results.add(f.get(60, TimeUnit.SECONDS));
            }
            return results;
        } finally {
            executor.shutdownNow();
        }
    }

    @Test
    void moduleStackIsPerThread() throws Exception {
        List<Boolean> results =
                runConcurrently(
                        () -> {
                            String name = "PROG-" + Thread.currentThread().getId();
                            for (int i = 0; i < ITERATIONS; i++) {
                                CobolModule outer = makeModule(name);
                                CobolModule inner = makeModule(name + "-SUB");
                                CobolModule.push(outer);
                                CobolModule.push(inner);
                                Thread.yield();
                                if (CobolModule.getCurrentModule() != inner) {
                                    return false;
                                }
                                CobolModule.pop();
                                if (CobolModule.getCurrentModule() != outer) {
                                    return false;
                                }
                                CobolModule.pop();
                                if (CobolModule.getStackDepth() != 0) {
                                    return false;
                                }
                            }
                            return true;
                        });
        for (Boolean ok : results) {
            assertTrue(ok, "module stack leaked between threads");
        }
    }

    @Test
    void popRestoresCallerModule() {
        CobolModule outer = makeModule("OUTER");
        CobolModule inner = makeModule("INNER");
        CobolModule.push(outer);
        CobolModule.push(inner);
        CobolModule.pop();
        assertSame(outer, CobolModule.getCurrentModule(), "pop must restore the caller module");
        CobolModule.pop();
        assertSame(outer, CobolModule.getCurrentModule(), "last popped module stays visible");
        CobolModule.resetThreadState();
        assertNull(CobolModule.getCurrentModule(), "reset clears the module");
    }

    @Test
    void exceptionCodeIsPerThread() throws Exception {
        List<Boolean> results =
                runConcurrently(
                        () -> {
                            CobolModule.push(makeModule("EXC"));
                            int id =
                                    (Thread.currentThread().getId() % 2 == 0)
                                            ? CobolExceptionId.COB_EC_SIZE_ZERO_DIVIDE
                                            : CobolExceptionId.COB_EC_OVERFLOW_STRING;
                            CobolRuntimeException.setException(id);
                            int expected = CobolRuntimeException.getExceptionCode();
                            for (int i = 0; i < ITERATIONS; i++) {
                                CobolRuntimeException.setException(id);
                                Thread.yield();
                                if (CobolRuntimeException.getExceptionCode() != expected) {
                                    return false;
                                }
                                CobolRuntimeException.clearExceptionCode();
                                Thread.yield();
                                if (CobolRuntimeException.getExceptionCode() != 0) {
                                    return false;
                                }
                            }
                            CobolModule.pop();
                            return true;
                        });
        for (Boolean ok : results) {
            assertTrue(ok, "exception code leaked between threads");
        }
    }

    @Test
    void inspectAndStringAreReentrantAcrossThreads() throws Exception {
        List<Boolean> results =
                runConcurrently(
                        () -> {
                            CobolModule.push(makeModule("INSP"));
                            long tid = Thread.currentThread().getId();
                            String pattern = (tid % 3 == 0) ? "ab" : (tid % 3 == 1) ? "xy" : "q";
                            int expectedCount = 12;
                            StringBuilder sb = new StringBuilder();
                            for (int i = 0; i < 12; i++) {
                                sb.append(tid % 3 == 2 ? "q" : pattern);
                            }
                            for (int i = 0; i < ITERATIONS; i++) {
                                AbstractCobolField var = makeAlnum(sb.toString(), 24);
                                AbstractCobolField counter = makeNumeric(0, 4);
                                CobolInspect.init(var, 0);
                                CobolInspect.start();
                                Thread.yield();
                                CobolInspect.all(counter, makeAlnum(pattern, pattern.length()));
                                CobolInspect.finish();
                                if (counter.getInt() != expectedCount) {
                                    return false;
                                }

                                AbstractCobolField dst = makeAlnum("", 16);
                                CobolString.stringInit(dst, null);
                                CobolString.stringDelimited(0);
                                CobolString.stringAppend(makeAlnum(pattern, pattern.length()));
                                Thread.yield();
                                CobolString.stringAppend(makeAlnum(pattern, pattern.length()));
                                CobolString.stringFinish();
                                if (!dst.getString().startsWith(pattern + pattern)) {
                                    return false;
                                }
                            }
                            CobolModule.pop();
                            return true;
                        });
        for (Boolean ok : results) {
            assertTrue(ok, "INSPECT/STRING work area leaked between threads");
        }
    }

    @Test
    void intrinsicResultsAreNotSharedBetweenThreads() throws Exception {
        List<Boolean> results =
                runConcurrently(
                        () -> {
                            CobolModule.push(makeModule("INTR"));
                            String value = "v" + Thread.currentThread().getId();
                            for (int i = 0; i < ITERATIONS; i++) {
                                AbstractCobolField src = makeAlnum(value, value.length());
                                AbstractCobolField upper = CobolIntrinsic.funcUpperCase(0, 0, src);
                                Thread.yield();
                                AbstractCobolField len = CobolIntrinsic.funcLength(src);
                                if (!upper.getString().equals(value.toUpperCase())) {
                                    return false;
                                }
                                if (len.getInt() != value.length()) {
                                    return false;
                                }
                            }
                            CobolModule.pop();
                            return true;
                        });
        for (Boolean ok : results) {
            assertTrue(ok, "intrinsic result field leaked between threads");
        }
    }

    @Test
    void callParamsAndSwitchesArePerThread() throws Exception {
        List<Boolean> results =
                runConcurrently(
                        () -> {
                            int n = (int) (Thread.currentThread().getId() % 7) + 1;
                            for (int i = 0; i < ITERATIONS; i++) {
                                CobolCallParams.set(n);
                                CobolUtil.setSwitch(n % 8, 1);
                                Thread.yield();
                                if (CobolCallParams.get() != n) {
                                    return false;
                                }
                                if (!CobolUtil.getSwitch(n % 8)) {
                                    return false;
                                }
                                CobolUtil.setSwitch(n % 8, 0);
                            }
                            return true;
                        });
        for (Boolean ok : results) {
            assertTrue(ok, "call params or switches leaked between threads");
        }
    }

    @Test
    void stopRunAtNestedFrameUnwindsToRoot() {
        CobolModule root = makeModule("ROOT");
        CobolModule sub = makeModule("SUB");
        CobolModule.push(root);
        CobolResolve.pushCallStackList("ROOT");
        CobolModule.push(sub);
        CobolResolve.pushCallStackList("SUB");

        CobolStopRunSignal signal = null;
        try {
            CobolStopRunException.handleAtFrame(5);
        } catch (CobolStopRunSignal e) {
            signal = e;
        }
        assertNotNull(signal, "nested frame must rethrow to the caller");
        assertEquals(5, signal.getReturnCode(), "return code is carried to the caller");
        assertSame(root, CobolModule.getCurrentModule(), "nested frame is popped");

        int rc = CobolStopRunException.handleAtFrame(signal.getReturnCode());
        assertEquals(5, rc, "root frame returns the STOP RUN code");
        assertEquals(0, CobolModule.getStackDepth(), "root frame is popped");
        assertEquals(
                Integer.valueOf(5),
                CobolStopRunException.consumeStopRunCode(),
                "STOP RUN code is recorded for main()");
        assertNull(CobolStopRunException.consumeStopRunCode(), "record is consumed once");
    }

    @Test
    void runUnitEndClearsThreadState() {
        CobolModule.push(makeModule("END"));
        CobolCallParams.set(3);
        CobolRuntimeException.setException(CobolExceptionId.COB_EC_OVERFLOW_STRING);
        CobolRunUnit.end();
        assertNull(CobolModule.getCurrentModule(), "module stack is cleared");
        assertEquals(0, CobolCallParams.get(), "call params are cleared");
        assertEquals(0, CobolRuntimeException.getExceptionCode(), "exception code is cleared");
    }
}
