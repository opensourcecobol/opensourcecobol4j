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
package jp.osscons.opensourcecobol.libcobj.sql;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Proxy;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

/** 実行単位の終了時のESQLの後始末({@code endRunUnit}と{@code CobolEsql.resetThreadState})のテスト。 */
class EsqlRunUnitEndTest {

    /** DBへ接続しないスタブバックエンド。フックの呼び出しをイベントログに記録する。 */
    private static class StubBackend extends AbstractCobolEsqlBackend {

        final List<String> events = new ArrayList<>();

        boolean failOnRollback;

        @Override
        public String id() {
            return "stub-run-unit-end";
        }

        @Override
        protected String buildJdbcUrl(DbSpec spec) {
            return "jdbc:stub";
        }

        @Override
        protected void configureConnection(Connection c) {}

        @Override
        protected void commitTransaction(Connection c) {
            events.add("commitTransaction");
        }

        @Override
        protected void rollbackTransaction(Connection c) throws SQLException {
            events.add("rollbackTransaction");
            if (failOnRollback) {
                throw new SQLException("rollback failure (expected by test)");
            }
        }

        @Override
        protected void openCursorImpl(
                Connection c, String cursorName, String query, AbstractCobolField[] params) {}

        @Override
        protected boolean fetchRowImpl(
                Connection c, String cursorName, AbstractCobolField[] out, CobolDataStorage sqlca) {
            return false;
        }

        @Override
        protected void fetchOccursImpl(
                Connection c,
                String cursorName,
                int occursSize,
                int occursMax,
                AbstractCobolField[] resultParams,
                CobolDataStorage sqlca) {}

        @Override
        protected void closeCursorImpl(Connection c, String cursorName) {}

        @Override
        protected void repositionForCurrentOf(
                Connection c, String cursorName, CobolDataStorage sqlca) {}

        @Override
        protected SqlErrorMapping mapSqlException(SQLException e) {
            return new SqlErrorMapping(SqlCA.ECPG_UNKNOWN_ERROR, e.getSQLState());
        }
    }

    @AfterEach
    void tearDown() {
        CobolEsql.backend.remove();
    }

    /**
     * 動的プロキシ製のJDBC接続。close()の呼び出しをイベントログに記録する。
     *
     * @param events 記録先
     * @param closed isClosed()が返す値
     * @param failOnIsClosed trueならisClosed()がSQLExceptionをスローする
     */
    private static Connection stubConnection(
            List<String> events, boolean closed, boolean failOnIsClosed) {
        return (Connection)
                Proxy.newProxyInstance(
                        EsqlRunUnitEndTest.class.getClassLoader(),
                        new Class<?>[] {Connection.class},
                        (proxy, method, args) -> {
                            if ("close".equals(method.getName())) {
                                events.add("connectionClose");
                                return null;
                            }
                            if ("isClosed".equals(method.getName())) {
                                if (failOnIsClosed) {
                                    throw new SQLException("isClosed failure (expected by test)");
                                }
                                return closed;
                            }
                            Class<?> returnType = method.getReturnType();
                            if (returnType == boolean.class) {
                                return false;
                            }
                            if (returnType == int.class) {
                                return 0;
                            }
                            return null;
                        });
    }

    @Test
    void endRunUnitOnEmptyBackendIsHarmless() {
        StubBackend backend = new StubBackend();
        backend.endRunUnit();
        assertTrue(backend.connections.isEmpty(), "no connections");
        assertNull(backend.defaultConnId, "no default connection");
    }

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void endRunUnitRollsBackAndClosesOpenConnections() {
        StubBackend backend = new StubBackend();
        backend.addConnection("c1", stubConnection(backend.events, false, false));
        backend.addConnection("c2", stubConnection(backend.events, false, false));
        backend.prepared.put("p1", new String[] {"SELECT 1", "0"});
        backend.endRunUnit();
        assertEquals(
                2,
                backend.events.stream().filter("rollbackTransaction"::equals).count(),
                "uncommitted work of every open connection is rolled back");
        assertEquals(
                0,
                backend.events.stream().filter("commitTransaction"::equals).count(),
                "the end of a run unit never commits");
        assertEquals(2, backend.events.stream().filter("connectionClose"::equals).count());
        assertTrue(backend.connections.isEmpty(), "connections are cleared");
        assertTrue(backend.prepared.isEmpty(), "prepared statements are cleared");
        assertTrue(backend.cursors.isEmpty(), "cursors are cleared");
        assertTrue(backend.stmtCache.isEmpty(), "statement cache is cleared");
        assertNull(backend.defaultConnId, "default connection is cleared");
    }

    @Test
    void endRunUnitClosesTheConnectionEvenIfRollbackFails() {
        StubBackend backend = new StubBackend();
        backend.failOnRollback = true;
        backend.addConnection("c1", stubConnection(backend.events, false, false));
        backend.endRunUnit();
        assertEquals(1, backend.events.stream().filter("connectionClose"::equals).count());
        assertTrue(backend.connections.isEmpty(), "connections are cleared");
    }

    @Test
    void endRunUnitSkipsClosedConnectionsAndSurvivesFailures() {
        StubBackend backend = new StubBackend();
        backend.addConnection("closed", stubConnection(backend.events, true, false));
        backend.addConnection("failing", stubConnection(backend.events, false, true));
        backend.endRunUnit();
        assertEquals(
                0,
                backend.events.stream().filter("connectionClose"::equals).count(),
                "neither connection is closed: one is already closed, one fails");
        assertTrue(backend.connections.isEmpty(), "connections are cleared anyway");
    }

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void resetThreadStateEndsTheRunUnitOfTheThread() throws Exception {
        CobolEsql.resetThreadState();

        StubBackend backend = new StubBackend();
        backend.addConnection("c1", stubConnection(backend.events, false, false));
        CobolEsql.backend.set(backend);

        ExecutorService executor = Executors.newSingleThreadExecutor();
        try {
            Object other = executor.submit(() -> CobolEsql.backend.get()).get(30, TimeUnit.SECONDS);
            assertNull(other, "the backend belongs to the thread that set it");
        } finally {
            executor.shutdownNow();
        }

        assertSame(backend, CobolEsql.backend.get(), "backend of this thread");
        CobolEsql.resetThreadState();
        assertEquals(1, backend.events.stream().filter("connectionClose"::equals).count());
        assertNull(CobolEsql.backend.get(), "backend is removed with the run unit");
    }
}
