package jp.osscons.opensourcecobol.libcobj.sql;

import static org.junit.jupiter.api.Assertions.*;

import java.lang.reflect.Proxy;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * {@link AbstractCobolEsqlBackend} のカーソル無効化ライフサイクル（{@code onCursorsInvalidated}
 * フックの配線）を、DB 接続なしで検証する。スタブバックエンドと {@link Proxy} 製の {@link Connection}
 * でフックの発火回数・発火順序（特に DISCONNECT では接続クローズより前）を確認する。
 */
class AbstractCobolEsqlBackendLifecycleTest {

    private static final String CONN_ID = "OCDB_DEFAULT_DBNAME";

    /** フック呼び出しと接続クローズの順序を記録するイベントログ。 */
    private List<String> events;

    private StubBackend backend;
    private CobolDataStorage sqlca;

    /** DB へ接続しないスタブバックエンド。ライフサイクルフックの呼び出しを記録するだけ。 */
    private static class StubBackend extends AbstractCobolEsqlBackend {

        private final List<String> events;

        StubBackend(List<String> events) {
            this.events = events;
        }

        @Override
        public String id() {
            return "stub";
        }

        @Override
        protected String buildJdbcUrl(DbSpec spec) {
            return "jdbc:stub";
        }

        @Override
        protected void configureConnection(Connection c) {
            // no-op（接続後初期化なし）
        }

        @Override
        protected void commitTransaction(Connection c) {
            events.add("commitTransaction");
        }

        @Override
        protected void rollbackTransaction(Connection c) {
            events.add("rollbackTransaction");
        }

        @Override
        protected void openCursorImpl(
                Connection c, String cursorName, String query, AbstractCobolField[] params) {
            // no-op（SQL は発行しない）
        }

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
                CobolDataStorage sqlca) {
            // no-op
        }

        @Override
        protected void closeCursorImpl(Connection c, String cursorName) {
            events.add("closeCursorImpl");
        }

        @Override
        protected void repositionForCurrentOf(
                Connection c, String cursorName, CobolDataStorage sqlca) {
            // no-op
        }

        @Override
        protected SqlErrorMapping mapSqlException(SQLException e) {
            return new SqlErrorMapping(SqlCA.ECPG_UNKNOWN_ERROR, e.getSQLState());
        }

        @Override
        protected void onCursorsInvalidated() {
            events.add("onCursorsInvalidated");
        }
    }

    /** クローズされたことをイベントログに記録するテスト用資源。 */
    private static final class RecordingResource implements AutoCloseable {

        private final List<String> events;
        private final String name;
        boolean closed;
        boolean failOnClose;

        RecordingResource(List<String> events, String name) {
            this.events = events;
            this.name = name;
        }

        @Override
        public void close() {
            closed = true;
            events.add("resourceClose:" + name);
            if (failOnClose) {
                throw new IllegalStateException("close failure (expected by test)");
            }
        }
    }

    /**
     * カーソルごとに生きた資源を自前の Map で管理するバックエンド（Db2 の ライブ ResultSet 方式を
     * 模す）。SPI 契約どおり、明示 CLOSE は {@code closeCursorImpl} で、COMMIT/ROLLBACK/DISCONNECT
     * の一括無効化は {@code onCursorsInvalidated} で解放する。
     */
    private static final class ResourceOwningBackend extends StubBackend {

        final Map<String, RecordingResource> openResources = new LinkedHashMap<>();
        private final List<String> resourceEvents;

        ResourceOwningBackend(List<String> events) {
            super(events);
            this.resourceEvents = events;
        }

        @Override
        protected void openCursorImpl(
                Connection c, String cursorName, String query, AbstractCobolField[] params) {
            openResources.put(cursorName, new RecordingResource(resourceEvents, cursorName));
        }

        @Override
        protected void closeCursorImpl(Connection c, String cursorName) {
            super.closeCursorImpl(c, cursorName);
            closeQuietly(openResources.remove(cursorName));
        }

        @Override
        protected void onCursorsInvalidated() {
            super.onCursorsInvalidated();
            for (RecordingResource resource : openResources.values()) {
                closeQuietly(resource);
            }
            openResources.clear();
        }

        /** 個々の close() 失敗が残りの資源の解放を妨げないようにする。 */
        private static void closeQuietly(RecordingResource resource) {
            if (resource == null) {
                return;
            }
            try {
                resource.close();
            } catch (RuntimeException ignored) {
                // クローズ失敗は無視して続行する。
            }
        }
    }

    /**
     * 接続クローズ前の commit だけを別扱いにするバックエンド（PostgreSQL のような明示
     * {@code BEGIN} 流儀の DB を模す）。切断経路が {@code commitBeforeClose} を通ること、
     * 継続用の {@code commitTransaction} と混ざらないことを確かめるために使う。
     */
    private static final class SeparateCloseCommitBackend extends StubBackend {

        private final List<String> closeEvents;

        SeparateCloseCommitBackend(List<String> events) {
            super(events);
            this.closeEvents = events;
        }

        @Override
        protected void commitBeforeClose(Connection c) {
            closeEvents.add("commitBeforeClose");
        }
    }

    @BeforeEach
    void setUp() {
        events = new ArrayList<>();
        backend = new StubBackend(events);
        sqlca = new CobolDataStorage(136);
    }

    /**
     * 動的プロキシ製の JDBC 接続。{@code close()} の呼び出しをイベントログへ記録し、
     * {@code isClosed()} は false を返す。それ以外のメソッドは型の既定値を返す。
     */
    private Connection stubConnection() {
        return (Connection)
                Proxy.newProxyInstance(
                        AbstractCobolEsqlBackendLifecycleTest.class.getClassLoader(),
                        new Class<?>[] {Connection.class},
                        (proxy, method, args) -> {
                            if ("close".equals(method.getName())) {
                                events.add("connectionClose");
                                return null;
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

    private void registerConnection() {
        backend.addConnection(CONN_ID, stubConnection());
    }

    private void registerOpenedCursor(String name) {
        AbstractCobolEsqlBackend.Cursor cursor =
                new AbstractCobolEsqlBackend.Cursor(name, "SELECT 1", 0);
        cursor.isOpened = true;
        backend.cursors.put(name, cursor);
    }

    private int sqlCode() {
        return SqlCA.getCode(sqlca);
    }

    private long count(String event) {
        return events.stream().filter(event::equals).count();
    }

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testCommit_NotifiesCursorInvalidation() {
        registerConnection();
        registerOpenedCursor("c1");
        backend.commit(sqlca);
        assertEquals(0, sqlCode(), "commit should succeed");
        assertEquals(1, count("onCursorsInvalidated"), "COMMIT should notify invalidation once");
        assertFalse(backend.cursors.get("c1").isOpened, "cursor should be closed after COMMIT");
    }

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testRollback_NotifiesCursorInvalidation() {
        registerConnection();
        registerOpenedCursor("c1");
        backend.rollback(sqlca);
        assertEquals(0, sqlCode(), "rollback should succeed");
        assertEquals(1, count("onCursorsInvalidated"), "ROLLBACK should notify invalidation once");
    }

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testDisconnect_NotifiesCursorInvalidationBeforeConnectionClose() {
        registerConnection();
        registerOpenedCursor("c1");
        backend.disconnect(sqlca);
        assertEquals(0, sqlCode(), "disconnect should succeed");
        // 切断経路でもフックが発火し、かつ接続クローズより前であること（クローズ後の解放は
        // ドライバ依存の JDBC カスケードに巻き込まれ、解放順序を保証できない）。
        // 先頭が commitTransaction なのは commitBeforeClose の既定実装がそこへ委譲するため。
        assertEquals(
                Arrays.asList("commitTransaction", "onCursorsInvalidated", "connectionClose"),
                events,
                "DISCONNECT should notify invalidation after commit and before connection close");
        assertFalse(backend.cursors.get("c1").isOpened, "cursor should be closed after DISCONNECT");
    }

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testDisconnect_UsesCommitBeforeCloseHookWhenOverridden() {
        SeparateCloseCommitBackend b = new SeparateCloseCommitBackend(events);
        b.addConnection(CONN_ID, stubConnection());
        b.disconnect(sqlca);
        assertEquals(0, sqlCode(), "disconnect should succeed");
        assertEquals(
                Arrays.asList("commitBeforeClose", "onCursorsInvalidated", "connectionClose"),
                events,
                "DISCONNECT should commit through commitBeforeClose, not commitTransaction");
    }

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testCommit_DoesNotUseCommitBeforeCloseHook() {
        SeparateCloseCommitBackend b = new SeparateCloseCommitBackend(events);
        b.addConnection(CONN_ID, stubConnection());
        b.commit(sqlca);
        assertEquals(0, sqlCode(), "commit should succeed");
        assertEquals(1, count("commitTransaction"), "COMMIT should keep using commitTransaction");
        assertEquals(
                0, count("commitBeforeClose"), "COMMIT must not use the close-only commit hook");
    }

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testCloseCursor_DoesNotNotifyInvalidation() {
        registerConnection();
        registerOpenedCursor("c1");
        backend.closeCursor(sqlca, "c1");
        assertEquals(0, sqlCode(), "close cursor should succeed");
        assertEquals(1, count("closeCursorImpl"), "explicit CLOSE should reach closeCursorImpl");
        assertEquals(
                0,
                count("onCursorsInvalidated"),
                "explicit CLOSE of a single cursor is not a bulk invalidation");
    }

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testCommit_NoConnection_DoesNotNotify() {
        backend.commit(sqlca);
        assertEquals(SqlCA.ECPG_NO_CONN, sqlCode(), "commit without connection should fail");
        assertEquals(
                0, count("onCursorsInvalidated"), "failed commit should not notify invalidation");
    }

    // ============================================================
    // 自前 Map で per-cursor 資源を管理するバックエンド（Db2 型）の解放契約
    // ============================================================

    /** 資源管理型バックエンドを生成し、接続を登録して名前のカーソルを DECLARE + OPEN する。 */
    private ResourceOwningBackend openResourceCursors(String... cursorNames) {
        ResourceOwningBackend rb = new ResourceOwningBackend(events);
        rb.addConnection(CONN_ID, stubConnection());
        for (String name : cursorNames) {
            rb.declareCursor(sqlca, name, "SELECT 1");
            rb.openCursor(sqlca, name);
            assertEquals(0, sqlCode(), "open of " + name + " should succeed");
        }
        return rb;
    }

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testResourceBackend_CloseCursorReleasesResource() {
        ResourceOwningBackend rb = openResourceCursors("c1");
        RecordingResource resource = rb.openResources.get("c1");
        assertNotNull(resource, "OPEN should register a live resource");
        rb.closeCursor(sqlca, "c1");
        assertEquals(0, sqlCode(), "close cursor should succeed");
        assertTrue(resource.closed, "explicit CLOSE should release the cursor resource");
        assertTrue(rb.openResources.isEmpty(), "resource map should be empty after CLOSE");
    }

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testResourceBackend_CommitReleasesAllResources() {
        ResourceOwningBackend rb = openResourceCursors("c1", "c2");
        RecordingResource r1 = rb.openResources.get("c1");
        RecordingResource r2 = rb.openResources.get("c2");
        rb.commit(sqlca);
        assertEquals(0, sqlCode(), "commit should succeed");
        assertTrue(r1.closed, "COMMIT should release the first cursor resource");
        assertTrue(r2.closed, "COMMIT should release the second cursor resource");
        assertTrue(rb.openResources.isEmpty(), "resource map should be empty after COMMIT");
    }

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testResourceBackend_RollbackReleasesAllResources() {
        ResourceOwningBackend rb = openResourceCursors("c1");
        RecordingResource resource = rb.openResources.get("c1");
        rb.rollback(sqlca);
        assertEquals(0, sqlCode(), "rollback should succeed");
        assertTrue(resource.closed, "ROLLBACK should release the cursor resource");
        assertTrue(rb.openResources.isEmpty(), "resource map should be empty after ROLLBACK");
    }

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testResourceBackend_DisconnectReleasesResourcesBeforeConnectionClose() {
        ResourceOwningBackend rb = openResourceCursors("c1");
        rb.disconnect(sqlca);
        assertEquals(0, sqlCode(), "disconnect should succeed");
        int resourceClose = events.indexOf("resourceClose:c1");
        int connectionClose = events.indexOf("connectionClose");
        assertTrue(resourceClose >= 0, "DISCONNECT should release the cursor resource");
        assertTrue(
                resourceClose < connectionClose,
                "cursor resource should be released before the connection is closed");
    }

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testResourceBackend_CloseFailureDoesNotStopOthers() {
        ResourceOwningBackend rb = openResourceCursors("c1", "c2");
        rb.openResources.get("c1").failOnClose = true;
        RecordingResource following = rb.openResources.get("c2");
        rb.commit(sqlca);
        assertEquals(0, sqlCode(), "commit should succeed despite resource close failure");
        assertTrue(following.closed, "close failure should not stop closing remaining resources");
        assertTrue(
                rb.openResources.isEmpty(), "resource map should be cleared even on close failure");
    }
}
