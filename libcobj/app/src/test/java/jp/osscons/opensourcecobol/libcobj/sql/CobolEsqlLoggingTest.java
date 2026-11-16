package jp.osscons.opensourcecobol.libcobj.sql;

import static org.junit.jupiter.api.Assertions.*;

import com.github.valfirst.slf4jtest.LoggingEvent;
import com.github.valfirst.slf4jtest.TestLogger;
import com.github.valfirst.slf4jtest.TestLoggerFactory;
import java.nio.ByteBuffer;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.List;
import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldAttribute;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldFactory;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.slf4j.event.Level;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

@Testcontainers
class CobolEsqlLoggingTest {

    @Container
    static PostgreSQLContainer<?> postgres =
            new PostgreSQLContainer<>("postgres:15")
                    .withDatabaseName("testdb")
                    .withUsername("test_user")
                    .withPassword("test_pass");

    private CobolDataStorage sqlca;

    // 操作系ログ（CONNECT/EXEC SQL/DISCONNECT/エラー）は CobolEsql 名のロガーへ出る。
    private TestLogger cobolSqlLogger;
    // 接続確立ログ（Connecting to...）は抽象基底クラス AbstractCobolEsqlBackend のロガーへ出る。
    private TestLogger connLogger;

    @BeforeEach
    void setUp() {
        sqlca = new CobolDataStorage(136);
        // 環境変数からの解決を経ずに backend を据える（このテストは PostgreSQL 実装を対象にする）。
        CobolEsql.backend = new CobolEsqlBackendPostgresql();
        cobolSqlLogger = TestLoggerFactory.getTestLogger(CobolEsql.class);
        connLogger = TestLoggerFactory.getTestLogger(AbstractCobolEsqlBackend.class);
        TestLoggerFactory.clear();
    }

    @AfterEach
    void tearDown() {
        closeRegisteredConnections();
        CobolEsql.backend = null;
        TestLoggerFactory.clear();
    }

    /** 登録済みの全接続を閉じて登録内容を空にする（Testcontainers 接続のリーク防止）。 */
    private void closeRegisteredConnections() {
        AbstractCobolEsqlBackend b = (AbstractCobolEsqlBackend) CobolEsql.backend;
        for (Connection c : b.connections.values()) {
            try {
                if (c != null && !c.isClosed()) {
                    c.close();
                }
            } catch (SQLException ignored) {
                // 後始末のエラーは無視する
            }
        }
        b.connections.clear();
        b.defaultConnId = null;
        b.stmtCache.clear();
    }

    private static AbstractCobolField makeAlphaField(int size, byte[] data) {
        CobolFieldAttribute attr =
                new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null);
        CobolDataStorage storage = new CobolDataStorage(data);
        return CobolFieldFactory.makeCobolField(size, storage, attr);
    }

    private int getSqlCode() {
        return ByteBuffer.wrap(sqlca.getByteArray(12, 4)).getInt();
    }

    private void connectToPostgres() {
        String dbSpec =
                "testdb@"
                        + postgres.getHost()
                        + ":"
                        + postgres.getMappedPort(PostgreSQLContainer.POSTGRESQL_PORT);
        byte[] userBytes = postgres.getUsername().getBytes();
        byte[] passBytes = postgres.getPassword().getBytes();
        byte[] dbBytes = dbSpec.getBytes();
        AbstractCobolField userField = makeAlphaField(userBytes.length, userBytes);
        AbstractCobolField passField = makeAlphaField(passBytes.length, passBytes);
        AbstractCobolField dbField = makeAlphaField(dbBytes.length, dbBytes);
        CobolEsql.connect(sqlca, userField, passField, dbField);
    }

    @Test
    void testConnect_logsDebugMessage() {
        connectToPostgres();
        assertEquals(0, getSqlCode());

        List<LoggingEvent> events = cobolSqlLogger.getLoggingEvents();
        assertTrue(
                events.stream()
                        .anyMatch(
                                e ->
                                        e.getLevel() == Level.DEBUG
                                                && e.getMessage().contains("CONNECT")),
                "Expected DEBUG log containing 'CONNECT', got: " + events);
    }

    @Test
    void testConnect_connectionLog_logsDebugMessage() {
        connectToPostgres();
        assertEquals(0, getSqlCode());

        List<LoggingEvent> events = connLogger.getLoggingEvents();
        assertTrue(
                events.stream()
                        .anyMatch(
                                e ->
                                        e.getLevel() == Level.DEBUG
                                                && e.getMessage().contains("Connecting to")),
                "Expected DEBUG log containing 'Connecting to', got: " + events);
    }

    @Test
    void testExec_logsDebugMessage() throws Exception {
        connectToPostgres();
        TestLoggerFactory.clear();

        CobolEsql.exec(sqlca, "DROP TABLE IF EXISTS test_log_tbl");
        assertEquals(0, getSqlCode());

        List<LoggingEvent> events = cobolSqlLogger.getLoggingEvents();
        assertTrue(
                events.stream()
                        .anyMatch(
                                e ->
                                        e.getLevel() == Level.DEBUG
                                                && e.getMessage().contains("EXEC SQL")),
                "Expected DEBUG log containing 'EXEC SQL', got: " + events);
    }

    @Test
    void testExec_failure_logsError() throws Exception {
        connectToPostgres();
        TestLoggerFactory.clear();

        CobolEsql.exec(sqlca, "INVALID SQL SYNTAX HERE");
        assertNotEquals(0, getSqlCode());

        List<LoggingEvent> events = cobolSqlLogger.getLoggingEvents();
        assertTrue(
                events.stream().anyMatch(e -> e.getLevel() == Level.ERROR),
                "Expected ERROR log on SQL failure, got: " + events);
    }

    @Test
    void testDisconnect_logsDebugMessage() throws Exception {
        connectToPostgres();
        TestLoggerFactory.clear();

        CobolEsql.disconnect(sqlca);
        assertEquals(0, getSqlCode());

        List<LoggingEvent> events = cobolSqlLogger.getLoggingEvents();
        assertTrue(
                events.stream()
                        .anyMatch(
                                e ->
                                        e.getLevel() == Level.DEBUG
                                                && e.getMessage().contains("DISCONNECT")),
                "Expected DEBUG log containing 'DISCONNECT', got: " + events);
    }

    @Test
    void testConnect_doesNotLogPassword() {
        connectToPostgres();

        List<LoggingEvent> allEvents = new java.util.ArrayList<>(cobolSqlLogger.getLoggingEvents());
        allEvents.addAll(connLogger.getLoggingEvents());

        String password = postgres.getPassword();
        assertTrue(
                allEvents.stream().noneMatch(e -> e.getFormattedMessage().contains(password)),
                "Password should never appear in log output");
    }
}
