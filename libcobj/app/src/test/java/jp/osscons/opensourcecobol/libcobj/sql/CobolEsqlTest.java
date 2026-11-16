package jp.osscons.opensourcecobol.libcobj.sql;

import static org.junit.jupiter.api.Assertions.*;

import java.nio.ByteBuffer;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;
import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldAttribute;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldFactory;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

@Testcontainers
class CobolEsqlTest {

    @Container
    static PostgreSQLContainer<?> postgres =
            new PostgreSQLContainer<>("postgres:15")
                    .withDatabaseName("testdb")
                    .withUsername("test_user")
                    .withPassword("test_pass");

    private CobolDataStorage sqlca;

    private static AbstractCobolField makeAlphaField(int size, byte[] data) {
        CobolFieldAttribute attr =
                new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null);
        CobolDataStorage storage = new CobolDataStorage(data);
        return CobolFieldFactory.makeCobolField(size, storage, attr);
    }

    private static AbstractCobolField makeNumericField(int size, byte[] data) {
        CobolFieldAttribute attr =
                new CobolFieldAttribute(
                        CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY, size, 0, 0, null);
        CobolDataStorage storage = new CobolDataStorage(data);
        return CobolFieldFactory.makeCobolField(size, storage, attr);
    }

    @BeforeEach
    void setUp() {
        sqlca = new CobolDataStorage(136);
        // 環境変数からの解決を経ずに backend を据える（このテストは PostgreSQL 実装を対象にする）。
        CobolEsql.backend = new CobolEsqlBackendPostgresql();
        // 先読み件数を既定 (1) に戻す（テスト間で N をリークさせない）。
        BulkFetchConfig.setFetchRecords(1);
    }

    @AfterEach
    void tearDown() {
        BulkFetchConfig.setFetchRecords(1);
        closeRegisteredConnections();
        CobolEsql.backend = null;
    }

    /** このテストが据えた backend を返す（内部状態の作り込みと検証に使う）。 */
    private AbstractCobolEsqlBackend backend() {
        return (AbstractCobolEsqlBackend) CobolEsql.backend;
    }

    /** 登録済みの全接続を閉じて登録簿を空にする（Testcontainers 接続のリーク防止）。 */
    private void closeRegisteredConnections() {
        AbstractCobolEsqlBackend b = backend();
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

    private void connectToPostgres() throws Exception {
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
        assertEquals(0, getSqlCode(), "Connect failed: " + getSqlState());
    }

    private CobolDataStorage makeStorage(String value) {
        byte[] bytes = value.getBytes();
        CobolDataStorage s = new CobolDataStorage(bytes.length);
        s.memcpy(bytes, bytes.length);
        return s;
    }

    private int getSqlCode() {
        return ByteBuffer.wrap(sqlca.getByteArray(12, 4)).getInt();
    }

    private String getSqlState() {
        return new String(sqlca.getByteArray(128, 5));
    }

    // SQLERRD(3) = 直前の文が処理した行数。SQLERRD はオフセット 96、3 番目 (0 始まり index 2)。
    private int getRowCount() {
        return ByteBuffer.wrap(sqlca.getByteArray(96 + 2 * 4, 4)).getInt();
    }

    // 生の JDBC 接続を作って backend へ直接登録するヘルパ（CobolEsql.connect を経由しない）。
    // PostgreSQL バックエンドの connect 相当（autoCommit(true) + 明示 BEGIN）を再現する。
    private Connection registerRealConnection() throws Exception {
        Connection realConn =
                DriverManager.getConnection(
                        postgres.getJdbcUrl(), postgres.getUsername(), postgres.getPassword());
        // autoCommit=true のまま明示 BEGIN すると、COMMIT/ROLLBACK まで複数文をまたぐ TX になる。
        realConn.setAutoCommit(true);
        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("BEGIN");
        }
        backend().addConnection("OCDB_DEFAULT_DBNAME", realConn);
        return realConn;
    }

    // ============================================================
    // connect() / disconnect()
    // ============================================================

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testConnect_Success() throws Exception {
        connectToPostgres();
        assertNotNull(backend().getDefaultConnection(), "Default connection should not be null");

        // Verify the connection is usable by executing a simple query
        Connection c = backend().getDefaultConnection();
        try (Statement stmt = c.createStatement();
                java.sql.ResultSet rs = stmt.executeQuery("SELECT 1")) {
            assertTrue(rs.next(), "Query should return a row");
            assertEquals(1, rs.getInt(1), "SELECT 1 should return 1");
        }
    }

    @Test
    void testConnect_NullStorage() {
        CobolEsql.connect(sqlca, null, null, null);
        // With null storage and no env vars, should get a connection error
        assertNotEquals(
                0, getSqlCode(), "Connect with null storage should produce non-zero SQLCODE");
    }

    @Test
    void testDisconnect_NoConnection() {
        CobolEsql.disconnect(sqlca);
        assertEquals(
                SqlCA.ECPG_NO_CONN,
                getSqlCode(),
                "Disconnect without connection should return ECPG_NO_CONN");
    }

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testDisconnect_Success() throws Exception {
        connectToPostgres();
        CobolEsql.disconnect(sqlca);
        assertEquals(0, getSqlCode(), "Disconnect should succeed");
        assertNull(
                backend().getDefaultConnection(),
                "Default connection should be null after disconnect");
    }

    // ============================================================
    // exec() with real DB
    // ============================================================

    @Test
    void testExec_NoConnection() {
        CobolEsql.exec(sqlca, "SELECT 1");
        assertEquals(
                SqlCA.ECPG_NO_CONN,
                getSqlCode(),
                "Exec without connection should return ECPG_NO_CONN");
    }

    @Test
    void testExec_NullQuery() throws Exception {
        registerRealConnection();
        CobolEsql.exec(sqlca, null);
        assertEquals(
                SqlCA.ECPG_EMPTY, getSqlCode(), "Exec with null query should return ECPG_EMPTY");
    }

    @Test
    void testExec_EmptyQuery() throws Exception {
        registerRealConnection();
        CobolEsql.exec(sqlca, "");
        assertEquals(
                SqlCA.ECPG_EMPTY, getSqlCode(), "Exec with empty query should return ECPG_EMPTY");
    }

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testExec_CreateTable() throws Exception {
        // Use a fresh connection to avoid stale state from other tests
        Connection freshConn =
                DriverManager.getConnection(
                        postgres.getJdbcUrl(), postgres.getUsername(), postgres.getPassword());
        freshConn.setAutoCommit(true);
        try (Statement stmt = freshConn.createStatement()) {
            stmt.execute("DROP TABLE IF EXISTS exec_test");
        }
        freshConn.close();

        registerRealConnection();
        CobolEsql.exec(sqlca, "CREATE TABLE exec_test (id INTEGER, name VARCHAR(20))");
        assertEquals(0, getSqlCode(), "CREATE TABLE failed with state: " + getSqlState());
        // Clean up
        CobolEsql.exec(sqlca, "DROP TABLE exec_test");
        assertEquals(0, getSqlCode(), "DROP TABLE should succeed");
    }

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testExec_InsertAndCommit() throws Exception {
        Connection realConn = registerRealConnection();
        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE IF EXISTS exec_test");
            stmt.execute("CREATE TABLE exec_test (id INTEGER)");
        }

        CobolEsql.exec(sqlca, "INSERT INTO exec_test VALUES (42)");
        assertEquals(0, getSqlCode(), "INSERT should succeed");

        CobolEsql.exec(sqlca, "COMMIT");
        assertEquals(0, getSqlCode(), "COMMIT should succeed");

        // Clean up
        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE exec_test");
        }
    }

    @Test
    void testExec_Rollback() throws Exception {
        registerRealConnection();
        CobolEsql.exec(sqlca, "ROLLBACK");
        assertEquals(0, getSqlCode(), "ROLLBACK should succeed");
    }

    @Test
    void testExec_Begin() throws Exception {
        registerRealConnection();
        CobolEsql.exec(sqlca, "BEGIN");
        assertEquals(0, getSqlCode(), "BEGIN should succeed");
    }

    @Test
    void testExec_InvalidTable() throws Exception {
        registerRealConnection();
        CobolEsql.exec(sqlca, "INSERT INTO nonexistent_table VALUES (1)");
        assertNotEquals(0, getSqlCode(), "Insert into nonexistent table should fail");
    }

    // ============================================================
    // execWithParams() with real DB
    // ============================================================

    @Test
    void testExecWithParams_NoConnection() {
        CobolEsql.execWithParams(
                sqlca, "INSERT INTO t VALUES(?)", makeNumericField(4, "0042".getBytes()));
        assertEquals(
                SqlCA.ECPG_NO_CONN,
                getSqlCode(),
                "ExecWithParams without connection should return ECPG_NO_CONN");
    }

    @Test
    void testExecWithParams_NullQuery() throws Exception {
        registerRealConnection();
        CobolEsql.execWithParams(sqlca, null, makeNumericField(4, "0042".getBytes()));
        assertEquals(
                SqlCA.ECPG_EMPTY,
                getSqlCode(),
                "ExecWithParams with null query should return ECPG_EMPTY");
    }

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testExecWithParams_Success() throws Exception {
        Connection realConn = registerRealConnection();
        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE IF EXISTS param_exec");
            stmt.execute("CREATE TABLE param_exec (id INTEGER, name VARCHAR(20))");
        }

        CobolEsql.execWithParams(
                sqlca,
                "INSERT INTO param_exec VALUES (?, ?)",
                makeNumericField(4, "0042".getBytes()),
                makeAlphaField(5, "Hello".getBytes()));
        assertEquals(0, getSqlCode(), "ExecWithParams INSERT should succeed");

        // Verify
        try (Statement stmt = realConn.createStatement();
                java.sql.ResultSet rs = stmt.executeQuery("SELECT id, name FROM param_exec")) {
            assertTrue(rs.next(), "Should have a row");
            assertEquals(42, rs.getInt(1), "Inserted id should be 42");
            assertEquals("Hello", rs.getString(2), "Inserted name should be Hello");
        }

        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE param_exec");
        }
    }

    @Test
    void testExecWithParams_NullParams() throws Exception {
        Connection realConn = registerRealConnection();
        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE IF EXISTS param_exec2");
            stmt.execute("CREATE TABLE param_exec2 (id INTEGER)");
        }
        CobolEsql.execWithParams(
                sqlca, "INSERT INTO param_exec2 VALUES (1)", (AbstractCobolField[]) null);
        assertEquals(0, getSqlCode(), "ExecWithParams with null params should succeed");
        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE param_exec2");
        }
    }

    @Test
    void testExecWithParams_ConstraintViolation() throws Exception {
        Connection realConn = registerRealConnection();
        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE IF EXISTS uniq_test");
            stmt.execute("CREATE TABLE uniq_test (id INTEGER UNIQUE)");
            stmt.execute("INSERT INTO uniq_test VALUES (1)");
        }

        CobolEsql.execWithParams(
                sqlca, "INSERT INTO uniq_test VALUES (?)", makeNumericField(4, "0001".getBytes()));
        assertEquals(
                SqlCA.ECPG_DUPLICATE_KEY,
                getSqlCode(),
                "Duplicate key should return ECPG_DUPLICATE_KEY");

        // 文が失敗するとトランザクションは aborted のままになる (文単位の SAVEPOINT 隔離は
        // 行わない)。回復するには COBOL プログラム同様に ROLLBACK が必要。
        CobolEsql.rollback(sqlca);
        assertEquals(0, getSqlCode(), "ROLLBACK should recover the aborted transaction");

        // ROLLBACK は (同一トランザクション内で作成した) uniq_test ごと巻き戻すため、
        // 後始末は IF EXISTS で行う。
        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE IF EXISTS uniq_test");
        }
    }

    // ============================================================
    // selectInto() with real DB
    // ============================================================

    @Test
    void testSelectInto_NoConnection() {
        CobolEsql.selectInto(sqlca, "SELECT 1", null, null);
        assertEquals(
                SqlCA.ECPG_NO_CONN,
                getSqlCode(),
                "SelectInto without connection should return ECPG_NO_CONN");
    }

    @Test
    void testSelectInto_NullQuery() throws Exception {
        registerRealConnection();
        CobolEsql.selectInto(sqlca, null, null, null);
        assertEquals(
                SqlCA.ECPG_EMPTY,
                getSqlCode(),
                "SelectInto with null query should return ECPG_EMPTY");
    }

    @Test
    void testSelectInto_NoResults() throws Exception {
        Connection realConn = registerRealConnection();
        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE IF EXISTS sel_test");
            stmt.execute("CREATE TABLE sel_test (id INTEGER, name VARCHAR(20))");
        }

        byte[] data = new byte[20];
        AbstractCobolField resultField = makeAlphaField(20, data);
        CobolEsql.selectInto(
                sqlca,
                "SELECT name FROM sel_test WHERE id = 999",
                null,
                new AbstractCobolField[] {resultField});
        assertEquals(
                SqlCA.ECPG_NOT_FOUND,
                getSqlCode(),
                "SelectInto with no results should return ECPG_NOT_FOUND");

        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE sel_test");
        }
    }

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testSelectInto_WithResults() throws Exception {
        Connection realConn = registerRealConnection();
        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE IF EXISTS sel_test");
            stmt.execute("CREATE TABLE sel_test (id INTEGER, name VARCHAR(20))");
            stmt.execute("INSERT INTO sel_test VALUES (1, 'World')");
        }

        byte[] data = new byte[20];
        AbstractCobolField resultField = makeAlphaField(20, data);
        CobolEsql.selectInto(
                sqlca,
                "SELECT name FROM sel_test WHERE id = 1",
                null,
                new AbstractCobolField[] {resultField});
        assertEquals(0, getSqlCode(), "SelectInto should succeed");
        String result = new String(resultField.getDataStorage().getByteArray(0, 5)).trim();
        assertEquals("World", result, "Selected value should be World");

        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE sel_test");
        }
    }

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testSelectInto_WithInputParams() throws Exception {
        Connection realConn = registerRealConnection();
        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE IF EXISTS sel_test");
            stmt.execute("CREATE TABLE sel_test (id INTEGER, name VARCHAR(20))");
            stmt.execute("INSERT INTO sel_test VALUES (1, 'World')");
        }

        byte[] data = new byte[20];
        AbstractCobolField resultField = makeAlphaField(20, data);
        AbstractCobolField inputField = makeNumericField(4, "0001".getBytes());
        CobolEsql.selectInto(
                sqlca,
                "SELECT name FROM sel_test WHERE id = ?",
                new AbstractCobolField[] {inputField},
                new AbstractCobolField[] {resultField});
        assertEquals(0, getSqlCode(), "SelectInto with params should succeed");
        String result = new String(resultField.getDataStorage().getByteArray(0, 5)).trim();
        assertEquals("World", result, "Selected value should be World");

        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE sel_test");
        }
    }

    @Test
    void testSelectInto_NullResultParams() throws Exception {
        Connection realConn = registerRealConnection();
        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE IF EXISTS sel_test");
            stmt.execute("CREATE TABLE sel_test (id INTEGER)");
            stmt.execute("INSERT INTO sel_test VALUES (1)");
        }

        CobolEsql.selectInto(sqlca, "SELECT id FROM sel_test", null, null);
        assertEquals(0, getSqlCode(), "SelectInto with null result params should succeed");

        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE sel_test");
        }
    }

    @Test
    void testSelectInto_EmptyResultParams() throws Exception {
        Connection realConn = registerRealConnection();
        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE IF EXISTS sel_test");
            stmt.execute("CREATE TABLE sel_test (id INTEGER)");
            stmt.execute("INSERT INTO sel_test VALUES (1)");
        }

        CobolEsql.selectInto(sqlca, "SELECT id FROM sel_test", null, new AbstractCobolField[] {});
        assertEquals(0, getSqlCode(), "SelectInto with empty result params should succeed");

        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE sel_test");
        }
    }

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testSelectInto_NullValueInResult() throws Exception {
        Connection realConn = registerRealConnection();
        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE IF EXISTS sel_test");
            stmt.execute("CREATE TABLE sel_test (name VARCHAR(20))");
            stmt.execute("INSERT INTO sel_test VALUES (NULL)");
        }

        byte[] data = new byte[10];
        java.util.Arrays.fill(data, (byte) 'X');
        AbstractCobolField resultField = makeAlphaField(10, data);
        CobolEsql.selectInto(
                sqlca, "SELECT name FROM sel_test", null, new AbstractCobolField[] {resultField});
        // ECPG semantics: NULL without indicator => sqlcode=-213 (ECPG_MISSING_INDICATOR).
        // The row is still considered fetched and the target field is filled with its
        // type-appropriate empty value (alphanumeric => spaces, numeric => zeros), not raw 0x00.
        assertEquals(
                SqlCA.ECPG_MISSING_INDICATOR,
                getSqlCode(),
                "SelectInto with NULL value should signal ECPG_MISSING_INDICATOR");
        assertEquals(
                (byte) ' ',
                resultField.getDataStorage().getByte(0),
                "NULL fills an alphanumeric host variable with spaces (type-aware empty)");

        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE sel_test");
        }
    }

    // ============================================================
    // cursor lifecycle with real DB
    // ============================================================

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testDeclareCursor_Success() {
        CobolEsql.declareCursor(sqlca, "c1", "SELECT * FROM t");
        assertEquals(0, getSqlCode(), "Declare cursor should succeed");
        assertNotNull(backend().cursors.get("c1"), "Cursor should be registered");
    }

    @Test
    void testDeclareCursor_NullName() {
        CobolEsql.declareCursor(sqlca, null, "SELECT 1");
        assertEquals(
                SqlCA.ECPG_EMPTY, getSqlCode(), "Declare with null name should return ECPG_EMPTY");
    }

    @Test
    void testDeclareCursor_NullQuery() {
        CobolEsql.declareCursor(sqlca, "c1", null);
        assertEquals(
                SqlCA.ECPG_EMPTY, getSqlCode(), "Declare with null query should return ECPG_EMPTY");
    }

    @Test
    void testDeclareCursor_EmptyName() {
        CobolEsql.declareCursor(sqlca, "", "SELECT 1");
        assertEquals(
                SqlCA.ECPG_EMPTY, getSqlCode(), "Declare with empty name should return ECPG_EMPTY");
    }

    @Test
    void testDeclareCursor_EmptyQuery() {
        CobolEsql.declareCursor(sqlca, "c1", "");
        assertEquals(
                SqlCA.ECPG_EMPTY,
                getSqlCode(),
                "Declare with empty query should return ECPG_EMPTY");
    }

    @Test
    void testDeclareCursor_AlreadyOpened() {
        AbstractCobolEsqlBackend.Cursor cursor =
                new AbstractCobolEsqlBackend.Cursor("c1", "SELECT 1", 0);
        cursor.isOpened = true;
        backend().cursors.put("c1", cursor);
        CobolEsql.declareCursor(sqlca, "c1", "SELECT 2");
        assertEquals(
                SqlCA.ECPG_WARNING_PORTAL_EXISTS,
                getSqlCode(),
                "Declaring already-opened cursor should return ECPG_WARNING_PORTAL_EXISTS");
    }

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testDeclareCursor_ExistingClosed() {
        AbstractCobolEsqlBackend.Cursor cursor =
                new AbstractCobolEsqlBackend.Cursor("c1", "SELECT 1", 0);
        cursor.isOpened = false;
        backend().cursors.put("c1", cursor);
        CobolEsql.declareCursor(sqlca, "c1", "SELECT 2");
        assertEquals(0, getSqlCode(), "Re-declaring closed cursor should succeed");
        assertEquals(
                "SELECT 2", backend().cursors.get("c1").query, "Cursor query should be updated");
    }

    @Test
    void testOpenCursor_NoConnection() {
        CobolEsql.openCursor(sqlca, "c1");
        assertEquals(
                SqlCA.ECPG_NO_CONN,
                getSqlCode(),
                "Open cursor without connection should return ECPG_NO_CONN");
    }

    @Test
    void testOpenCursor_CursorNotFound() throws Exception {
        registerRealConnection();
        CobolEsql.openCursor(sqlca, "nonexistent");
        assertEquals(
                SqlCA.ECPG_WARNING_UNKNOWN_PORTAL,
                getSqlCode(),
                "Open nonexistent cursor should return ECPG_WARNING_UNKNOWN_PORTAL");
    }

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testCursorLifecycle_RealDB() throws Exception {
        Connection realConn = registerRealConnection();
        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE IF EXISTS cur_test");
            stmt.execute("CREATE TABLE cur_test (id INTEGER, name VARCHAR(20))");
            stmt.execute("INSERT INTO cur_test VALUES (1, 'Alice')");
            stmt.execute("INSERT INTO cur_test VALUES (2, 'Bob')");
        }

        // Declare
        CobolEsql.declareCursor(sqlca, "myc", "SELECT name FROM cur_test ORDER BY id");
        assertEquals(0, getSqlCode(), "Declare cursor should succeed");

        // Open
        CobolEsql.openCursor(sqlca, "myc");
        assertEquals(0, getSqlCode(), "Open cursor should succeed");
        assertTrue(backend().cursors.get("myc").isOpened, "Cursor should be opened");

        // Fetch first row
        byte[] data1 = new byte[20];
        AbstractCobolField field1 = makeAlphaField(20, data1);
        CobolEsql.fetchCursor(sqlca, "myc", field1);
        assertEquals(0, getSqlCode(), "Fetch first row should succeed");
        String result1 = new String(field1.getDataStorage().getByteArray(0, 5)).trim();
        assertEquals("Alice", result1, "First row should be Alice");

        // Fetch second row
        byte[] data2 = new byte[20];
        AbstractCobolField field2 = makeAlphaField(20, data2);
        CobolEsql.fetchCursor(sqlca, "myc", field2);
        assertEquals(0, getSqlCode(), "Fetch second row should succeed");
        String result2 = new String(field2.getDataStorage().getByteArray(0, 3)).trim();
        assertEquals("Bob", result2, "Second row should be Bob");

        // Fetch past end
        CobolEsql.fetchCursor(sqlca, "myc");
        assertEquals(
                SqlCA.ECPG_NOT_FOUND, getSqlCode(), "Fetch past end should return ECPG_NOT_FOUND");

        // Close
        CobolEsql.closeCursor(sqlca, "myc");
        assertEquals(0, getSqlCode(), "Close cursor should succeed");
        assertFalse(backend().cursors.get("myc").isOpened, "Cursor should be closed");

        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE cur_test");
        }
    }

    @Test
    void testFetchCursor_NoConnection() {
        CobolEsql.fetchCursor(sqlca, "c1");
        assertEquals(
                SqlCA.ECPG_NO_CONN,
                getSqlCode(),
                "Fetch without connection should return ECPG_NO_CONN");
    }

    @Test
    void testFetchCursor_CursorNotFound() throws Exception {
        registerRealConnection();
        CobolEsql.fetchCursor(sqlca, "nonexistent");
        assertEquals(
                SqlCA.ECPG_WARNING_UNKNOWN_PORTAL,
                getSqlCode(),
                "Fetch nonexistent cursor should return ECPG_WARNING_UNKNOWN_PORTAL");
    }

    @Test
    void testFetchCursor_CursorNotOpened() throws Exception {
        registerRealConnection();
        AbstractCobolEsqlBackend.Cursor cursor =
                new AbstractCobolEsqlBackend.Cursor("c1", "SELECT 1", 0);
        cursor.isOpened = false;
        backend().cursors.put("c1", cursor);
        CobolEsql.fetchCursor(sqlca, "c1");
        assertEquals(
                SqlCA.ECPG_WARNING_UNKNOWN_PORTAL,
                getSqlCode(),
                "Fetch closed cursor should return ECPG_WARNING_UNKNOWN_PORTAL");
    }

    @Test
    void testCloseCursor_NoConnection() {
        CobolEsql.closeCursor(sqlca, "c1");
        assertEquals(
                SqlCA.ECPG_NO_CONN,
                getSqlCode(),
                "Close cursor without connection should return ECPG_NO_CONN");
    }

    @Test
    void testCloseCursor_CursorNotFound() throws Exception {
        registerRealConnection();
        CobolEsql.closeCursor(sqlca, "nonexistent");
        assertEquals(
                SqlCA.ECPG_WARNING_UNKNOWN_PORTAL,
                getSqlCode(),
                "Close nonexistent cursor should return ECPG_WARNING_UNKNOWN_PORTAL");
    }

    @Test
    void testCloseCursor_CursorNotOpened() throws Exception {
        registerRealConnection();
        AbstractCobolEsqlBackend.Cursor cursor =
                new AbstractCobolEsqlBackend.Cursor("c1", "SELECT 1", 0);
        cursor.isOpened = false;
        backend().cursors.put("c1", cursor);
        CobolEsql.closeCursor(sqlca, "c1");
        // 登録済みだが未 OPEN のカーソルの CLOSE は成功扱い (Open-COBOL-ESQL-4J に合わせる)。
        assertEquals(
                SqlCA.ECPG_NO_ERROR,
                getSqlCode(),
                "Close of a registered but not-opened cursor should succeed");
    }

    // ============================================================
    // declareCursorWithParams
    // ============================================================

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testDeclareCursorWithParams_Success() {
        AbstractCobolField param = makeNumericField(4, "0001".getBytes());
        CobolEsql.declareCursorWithParams(sqlca, "c1", "SELECT * FROM t WHERE id=?", param);
        assertEquals(0, getSqlCode(), "DeclareCursorWithParams should succeed");
        AbstractCobolEsqlBackend.Cursor c = backend().cursors.get("c1");
        assertNotNull(c, "Cursor should be registered");
        assertEquals(1, c.nParams, "Cursor should have 1 param");
    }

    @Test
    void testDeclareCursorWithParams_NullName() {
        CobolEsql.declareCursorWithParams(sqlca, null, "SELECT 1");
        assertEquals(
                SqlCA.ECPG_EMPTY,
                getSqlCode(),
                "DeclareCursorWithParams with null name should return ECPG_EMPTY");
    }

    @Test
    void testDeclareCursorWithParams_EmptyQuery() {
        CobolEsql.declareCursorWithParams(sqlca, "c1", "");
        assertEquals(
                SqlCA.ECPG_EMPTY,
                getSqlCode(),
                "DeclareCursorWithParams with empty query should return ECPG_EMPTY");
    }

    @Test
    void testDeclareCursorWithParams_EmptyName() {
        CobolEsql.declareCursorWithParams(sqlca, "", "SELECT 1");
        assertEquals(
                SqlCA.ECPG_EMPTY,
                getSqlCode(),
                "DeclareCursorWithParams with empty name should return ECPG_EMPTY");
    }

    @Test
    void testDeclareCursorWithParams_NullQuery() {
        CobolEsql.declareCursorWithParams(sqlca, "c1", null);
        assertEquals(
                SqlCA.ECPG_EMPTY,
                getSqlCode(),
                "DeclareCursorWithParams with null query should return ECPG_EMPTY");
    }

    @Test
    void testDeclareCursorWithParams_AlreadyOpened() {
        AbstractCobolEsqlBackend.Cursor cursor =
                new AbstractCobolEsqlBackend.Cursor("c1", "SELECT 1", 0);
        cursor.isOpened = true;
        backend().cursors.put("c1", cursor);
        CobolEsql.declareCursorWithParams(sqlca, "c1", "SELECT 2");
        assertEquals(
                SqlCA.ECPG_WARNING_PORTAL_EXISTS,
                getSqlCode(),
                "Declaring already-opened cursor should return ECPG_WARNING_PORTAL_EXISTS");
    }

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testDeclareCursorWithParams_NullParams() {
        CobolEsql.declareCursorWithParams(sqlca, "c1", "SELECT 1", (AbstractCobolField[]) null);
        assertEquals(0, getSqlCode(), "DeclareCursorWithParams with null params should succeed");
        assertEquals(0, backend().cursors.get("c1").nParams, "Cursor should have 0 params");
    }

    // ============================================================
    // openCursorWithParams
    // ============================================================

    @Test
    void testOpenCursorWithParams_NoConnection() {
        CobolEsql.openCursorWithParams(sqlca, "c1", makeNumericField(4, "0001".getBytes()));
        assertEquals(
                SqlCA.ECPG_NO_CONN,
                getSqlCode(),
                "OpenCursorWithParams without connection should return ECPG_NO_CONN");
    }

    @Test
    void testOpenCursorWithParams_CursorNotFound() throws Exception {
        registerRealConnection();
        CobolEsql.openCursorWithParams(
                sqlca, "nonexistent", makeNumericField(4, "0001".getBytes()));
        assertEquals(
                SqlCA.ECPG_WARNING_UNKNOWN_PORTAL,
                getSqlCode(),
                "OpenCursorWithParams for nonexistent cursor should return"
                        + " ECPG_WARNING_UNKNOWN_PORTAL");
    }

    // ============================================================
    // fetchCursorOccurs
    // ============================================================

    @Test
    void testFetchCursorOccurs_NoConnection() {
        CobolEsql.fetchCursorOccurs(sqlca, "c1", 10, 5);
        assertEquals(
                SqlCA.ECPG_NO_CONN,
                getSqlCode(),
                "FetchCursorOccurs without connection should return ECPG_NO_CONN");
    }

    @Test
    void testFetchCursorOccurs_CursorNotFound() throws Exception {
        registerRealConnection();
        CobolEsql.fetchCursorOccurs(sqlca, "c1", 10, 5);
        assertEquals(
                SqlCA.ECPG_WARNING_UNKNOWN_PORTAL,
                getSqlCode(),
                "FetchCursorOccurs for nonexistent cursor should return"
                        + " ECPG_WARNING_UNKNOWN_PORTAL");
    }

    @Test
    void testFetchCursorOccurs_CursorNotOpened() throws Exception {
        registerRealConnection();
        AbstractCobolEsqlBackend.Cursor cursor =
                new AbstractCobolEsqlBackend.Cursor("c1", "SELECT 1", 0);
        cursor.isOpened = false;
        backend().cursors.put("c1", cursor);
        CobolEsql.fetchCursorOccurs(sqlca, "c1", 10, 5);
        assertEquals(
                SqlCA.ECPG_WARNING_UNKNOWN_PORTAL,
                getSqlCode(),
                "FetchCursorOccurs for closed cursor should return ECPG_WARNING_UNKNOWN_PORTAL");
    }

    // ============================================================
    // bulk fetch (OCESQL4J_FETCH_RECORDS prefetch) + WHERE CURRENT OF
    // ============================================================

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testFetchCursor_BulkPrefetch() throws Exception {
        // 先読み 3 件。7 行を 3+3+1 で読み、単一行 FETCH と同じ順序・結果になること。
        BulkFetchConfig.setFetchRecords(3);
        Connection realConn = registerRealConnection();
        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE IF EXISTS bulk_test");
            stmt.execute("CREATE TABLE bulk_test (id INTEGER, name VARCHAR(20))");
            for (int i = 1; i <= 7; i++) {
                stmt.execute("INSERT INTO bulk_test VALUES (" + i + ", 'Name" + i + "')");
            }
        }
        CobolEsql.declareCursor(sqlca, "bc", "SELECT name FROM bulk_test ORDER BY id");
        assertEquals(0, getSqlCode(), "declare should succeed");
        CobolEsql.openCursor(sqlca, "bc");
        assertEquals(0, getSqlCode(), "open should succeed");

        for (int i = 1; i <= 7; i++) {
            byte[] data = new byte[20];
            CobolEsql.fetchCursor(sqlca, "bc", makeAlphaField(20, data));
            assertEquals(0, getSqlCode(), "fetch row " + i + " should succeed");
            assertEquals("Name" + i, new String(data).trim(), "row " + i + " value");
        }
        CobolEsql.fetchCursor(sqlca, "bc", makeAlphaField(20, new byte[20]));
        assertEquals(SqlCA.ECPG_NOT_FOUND, getSqlCode(), "fetch past end should be NOT_FOUND");

        CobolEsql.closeCursor(sqlca, "bc");
        assertEquals(0, getSqlCode(), "close should succeed");
        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE bulk_test");
        }
    }

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testWhereCurrentOf_BulkPrefetch() throws Exception {
        // 先読み 3 件で 2 行 FETCH 後、WHERE CURRENT OF で現在行(2 行目)のみ更新されること。
        BulkFetchConfig.setFetchRecords(3);
        Connection realConn = registerRealConnection();
        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE IF EXISTS wco_test");
            stmt.execute("CREATE TABLE wco_test (id INTEGER, name VARCHAR(20))");
            for (int i = 1; i <= 5; i++) {
                stmt.execute("INSERT INTO wco_test VALUES (" + i + ", 'Name" + i + "')");
            }
        }
        // 更新対象カーソルは ORDER BY を持たない単純スキャン（PostgreSQL の WHERE CURRENT OF の
        // 要件）。新規テーブルへの連番 INSERT なのでスキャン順＝挿入順で、2 行目は id=2。
        CobolEsql.declareCursor(sqlca, "wc", "SELECT name FROM wco_test");
        CobolEsql.openCursor(sqlca, "wc");
        assertEquals(0, getSqlCode(), "open should succeed");

        // 2 行フェッチ（論理現在行は 2 行目）。先読みで server カーソルは 3 行目にある。
        CobolEsql.fetchCursor(sqlca, "wc", makeAlphaField(20, new byte[20]));
        CobolEsql.fetchCursor(sqlca, "wc", makeAlphaField(20, new byte[20]));
        assertEquals(0, getSqlCode(), "two fetches should succeed");

        // WHERE CURRENT OF: 位置補正してから現在行を更新。
        CobolEsql.execWhereCurrentOf(
                sqlca, "UPDATE wco_test SET name = 'UPDATED' WHERE CURRENT OF", "wc");
        assertEquals(0, getSqlCode(), "positioned update should succeed: " + getSqlState());

        CobolEsql.closeCursor(sqlca, "wc");

        // id=2 のみ 'UPDATED'、他は元のまま。
        try (Statement stmt = realConn.createStatement();
                java.sql.ResultSet rs =
                        stmt.executeQuery("SELECT id, name FROM wco_test ORDER BY id")) {
            String[] expected = {"Name1", "UPDATED", "Name3", "Name4", "Name5"};
            int idx = 0;
            while (rs.next()) {
                assertEquals(expected[idx], rs.getString(2).trim(), "row id=" + rs.getInt(1));
                idx++;
            }
            assertEquals(5, idx, "should have 5 rows");
        }
        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE wco_test");
        }
    }

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testWhereCurrentOf_OverFetchPastEnd() throws Exception {
        // 先読み件数 (5) > 全行数 (2) のため、1 回の先読みで結果末尾に到達し overFetch=true になる
        // （server カーソルは最終行の「さらに先」へ進む）。この状態でも WHERE CURRENT OF が +1 補正して
        // 論理現在行（1 行目）だけを更新できることを検証する。overFetch=true 経路の回帰テスト。
        BulkFetchConfig.setFetchRecords(5);
        Connection realConn = registerRealConnection();
        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE IF EXISTS wco_of");
            stmt.execute("CREATE TABLE wco_of (id INTEGER, name VARCHAR(20))");
            stmt.execute("INSERT INTO wco_of VALUES (1, 'Name1')");
            stmt.execute("INSERT INTO wco_of VALUES (2, 'Name2')");
        }
        // WHERE CURRENT OF の要件に合わせ ORDER BY を持たない単純スキャン。連番 INSERT なので
        // スキャン順＝挿入順で 1 行目は id=1。
        CobolEsql.declareCursor(sqlca, "wof", "SELECT name FROM wco_of");
        CobolEsql.openCursor(sqlca, "wof");
        assertEquals(0, getSqlCode(), "open should succeed");

        // 1 行だけ FETCH（論理現在行は 1 行目）。先読みは 2 行取得して末尾に達し overFetch=true。
        CobolEsql.fetchCursor(sqlca, "wof", makeAlphaField(20, new byte[20]));
        assertEquals(0, getSqlCode(), "first fetch should succeed");
        assertTrue(
                ((CobolEsqlBackendPostgresql) backend()).cursorStates.get("wof").overFetch,
                "prefetch returned fewer rows than requested -> overFetch=true");

        CobolEsql.execWhereCurrentOf(
                sqlca, "UPDATE wco_of SET name = 'UPDATED' WHERE CURRENT OF", "wof");
        assertEquals(0, getSqlCode(), "positioned update should succeed: " + getSqlState());

        CobolEsql.closeCursor(sqlca, "wof");

        // id=1 のみ 'UPDATED'、id=2 は元のまま（誤位置なら id=2 が更新される）。
        try (Statement stmt = realConn.createStatement();
                java.sql.ResultSet rs =
                        stmt.executeQuery("SELECT id, name FROM wco_of ORDER BY id")) {
            String[] expected = {"UPDATED", "Name2"};
            int idx = 0;
            while (rs.next()) {
                assertEquals(expected[idx], rs.getString(2).trim(), "row id=" + rs.getInt(1));
                idx++;
            }
            assertEquals(2, idx, "should have 2 rows");
        }
        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE wco_of");
        }
    }

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testWhereCurrentOf_ExactPrefetchBoundary() throws Exception {
        // 先読み件数 (2) == 全行数 (2)。ちょうど要求件数ぶん返るため overFetch=false で、server
        // カーソルは最終行の「上」に留まる（末尾の先ではない）。1 行だけ FETCH した後の WHERE CURRENT OF
        // が +1 せずに論理現在行（1 行目）だけを更新できることを検証する。境界での off-by-one を防ぐ。
        BulkFetchConfig.setFetchRecords(2);
        Connection realConn = registerRealConnection();
        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE IF EXISTS wco_eb");
            stmt.execute("CREATE TABLE wco_eb (id INTEGER, name VARCHAR(20))");
            stmt.execute("INSERT INTO wco_eb VALUES (1, 'Name1')");
            stmt.execute("INSERT INTO wco_eb VALUES (2, 'Name2')");
        }
        CobolEsql.declareCursor(sqlca, "web", "SELECT name FROM wco_eb");
        CobolEsql.openCursor(sqlca, "web");
        assertEquals(0, getSqlCode(), "open should succeed");

        CobolEsql.fetchCursor(sqlca, "web", makeAlphaField(20, new byte[20]));
        assertEquals(0, getSqlCode(), "first fetch should succeed");
        assertFalse(
                ((CobolEsqlBackendPostgresql) backend()).cursorStates.get("web").overFetch,
                "exact-count prefetch -> overFetch=false (cursor on last row, not past end)");

        CobolEsql.execWhereCurrentOf(
                sqlca, "UPDATE wco_eb SET name = 'UPDATED' WHERE CURRENT OF", "web");
        assertEquals(0, getSqlCode(), "positioned update should succeed: " + getSqlState());

        CobolEsql.closeCursor(sqlca, "web");

        try (Statement stmt = realConn.createStatement();
                java.sql.ResultSet rs =
                        stmt.executeQuery("SELECT id, name FROM wco_eb ORDER BY id")) {
            String[] expected = {"UPDATED", "Name2"};
            int idx = 0;
            while (rs.next()) {
                assertEquals(expected[idx], rs.getString(2).trim(), "row id=" + rs.getInt(1));
                idx++;
            }
            assertEquals(2, idx, "should have 2 rows");
        }
        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE wco_eb");
        }
    }

    // ============================================================
    // selectIntoOccurs
    // ============================================================

    @Test
    void testSelectIntoOccurs_NoConnection() {
        CobolEsql.selectIntoOccurs(sqlca, 10, 5, "SELECT 1", null, null);
        assertEquals(
                SqlCA.ECPG_NO_CONN,
                getSqlCode(),
                "SelectIntoOccurs without connection should return ECPG_NO_CONN");
    }

    @Test
    void testSelectIntoOccurs_NullQuery() throws Exception {
        registerRealConnection();
        CobolEsql.selectIntoOccurs(sqlca, 10, 5, null, null, null);
        assertEquals(
                SqlCA.ECPG_EMPTY,
                getSqlCode(),
                "SelectIntoOccurs with null query should return ECPG_EMPTY");
    }

    // ============================================================
    // commit() / rollback()
    // ============================================================

    @Test
    void testCommit_NoConnection() {
        CobolEsql.commit(sqlca);
        assertEquals(
                SqlCA.ECPG_NO_CONN,
                getSqlCode(),
                "Commit without connection should return ECPG_NO_CONN");
    }

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testCommit_Success() throws Exception {
        registerRealConnection();
        backend().cursors.put("c1", new AbstractCobolEsqlBackend.Cursor("c1", "SELECT 1", 0));
        backend().cursors.get("c1").isOpened = true;
        CobolEsql.commit(sqlca);
        assertEquals(0, getSqlCode(), "Commit should succeed");
        assertFalse(backend().cursors.get("c1").isOpened, "Cursor should be closed after commit");
    }

    @Test
    void testRollback_NoConnection() {
        CobolEsql.rollback(sqlca);
        assertEquals(
                SqlCA.ECPG_NO_CONN,
                getSqlCode(),
                "Rollback without connection should return ECPG_NO_CONN");
    }

    @Test
    void testRollback_Success() throws Exception {
        registerRealConnection();
        CobolEsql.rollback(sqlca);
        assertEquals(0, getSqlCode(), "Rollback should succeed");
    }

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testRollback_UndoesInsert() throws Exception {
        // Create the table OUTSIDE any transaction so it persists after rollback
        Connection setupConn =
                DriverManager.getConnection(
                        postgres.getJdbcUrl(), postgres.getUsername(), postgres.getPassword());
        setupConn.setAutoCommit(true);
        try (Statement stmt = setupConn.createStatement()) {
            stmt.execute("DROP TABLE IF EXISTS rollback_test");
            stmt.execute("CREATE TABLE rollback_test (id INTEGER)");
        }
        setupConn.close();

        Connection realConn = registerRealConnection();

        CobolEsql.exec(sqlca, "INSERT INTO rollback_test VALUES (1)");
        assertEquals(0, getSqlCode(), "INSERT should succeed");

        CobolEsql.exec(sqlca, "ROLLBACK");
        assertEquals(0, getSqlCode(), "ROLLBACK should succeed");

        // Verify the insert was rolled back
        try (Statement stmt = realConn.createStatement();
                java.sql.ResultSet rs = stmt.executeQuery("SELECT COUNT(*) FROM rollback_test")) {
            assertTrue(rs.next(), "COUNT query should return a row");
            assertEquals(0, rs.getInt(1), "Rolled-back table should have 0 rows");
        }

        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE rollback_test");
        }
    }

    // ============================================================
    // prepare() / executePrepared()
    // ============================================================

    @Test
    void testPrepare_NullName() {
        CobolEsql.prepare(sqlca, null, null);
        assertEquals(
                SqlCA.ECPG_EMPTY, getSqlCode(), "Prepare with null name should return ECPG_EMPTY");
    }

    @Test
    void testPrepare_EmptyName() {
        CobolEsql.prepare(sqlca, "", null);
        assertEquals(
                SqlCA.ECPG_EMPTY, getSqlCode(), "Prepare with empty name should return ECPG_EMPTY");
    }

    @Test
    void testPrepare_NullQueryField() {
        CobolEsql.prepare(sqlca, "stmt1", null);
        assertEquals(
                SqlCA.ECPG_EMPTY,
                getSqlCode(),
                "Prepare with null query field should return ECPG_EMPTY");
    }

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testPrepare_Success_NoParams() {
        byte[] data = "SELECT * FROM t".getBytes();
        AbstractCobolField field = makeAlphaField(data.length, data);
        CobolEsql.prepare(sqlca, "stmt1", field);
        assertEquals(0, getSqlCode(), "Prepare should succeed");
        String[] prepared = backend().prepared.get("stmt1");
        assertNotNull(prepared, "Prepared statement should be registered");
        assertEquals("SELECT * FROM t", prepared[0], "Prepared query should match");
        assertEquals("0", prepared[1], "Prepared param count should be 0");
    }

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testPrepare_Success_WithHostVars() {
        byte[] data = "SELECT * FROM t WHERE id=:id AND name=:name".getBytes();
        AbstractCobolField field = makeAlphaField(data.length, data);
        CobolEsql.prepare(sqlca, "stmt2", field);
        assertEquals(0, getSqlCode(), "Prepare with host vars should succeed");
        String[] prepared = backend().prepared.get("stmt2");
        assertNotNull(prepared, "Prepared statement should be registered");
        assertTrue(prepared[0].contains("?"), "Host vars should be replaced with ?");
        assertEquals("2", prepared[1], "Prepared param count should be 2");
    }

    /** :name の直後が演算子 (区切り空白なし) でも、演算子以降のトークンを名前に飲み込まないこと。 */
    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testPrepare_HostVarFollowedByOperator_NotSwallowed() {
        byte[] data = "SELECT :a+:b FROM t".getBytes();
        AbstractCobolField field = makeAlphaField(data.length, data);
        CobolEsql.prepare(sqlca, "stmt_op", field);
        assertEquals(0, getSqlCode(), "Prepare should succeed");
        String[] prepared = backend().prepared.get("stmt_op");
        assertNotNull(prepared, "Prepared statement should be registered");
        assertEquals("SELECT ?+? FROM t", prepared[0], "演算子は名前に飲み込まれず保持される");
        assertEquals("2", prepared[1], "Prepared param count should be 2");
    }

    /** :name の直後が改行＋空白なしの語でも、続く語 (例 "AND") を名前に飲み込まないこと。 */
    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testPrepare_HostVarFollowedByNewline_NotSwallowed() {
        byte[] data = "DELETE FROM t WHERE id=:v1\nAND k=:v2".getBytes();
        AbstractCobolField field = makeAlphaField(data.length, data);
        CobolEsql.prepare(sqlca, "stmt_nl", field);
        assertEquals(0, getSqlCode(), "Prepare should succeed");
        String[] prepared = backend().prepared.get("stmt_nl");
        assertNotNull(prepared, "Prepared statement should be registered");
        assertEquals(
                "DELETE FROM t WHERE id=?\nAND k=?", prepared[0], "改行直後の 'AND' は名前に飲み込まれず保持される");
        assertEquals("2", prepared[1], "Prepared param count should be 2");
    }

    @Test
    void testExecutePrepared_NotFound() {
        CobolEsql.executePrepared(sqlca, "nonexistent");
        assertEquals(
                SqlCA.ECPG_INVALID_STMT,
                getSqlCode(),
                "Execute nonexistent prepared should return ECPG_INVALID_STMT");
    }

    @Test
    void testExecutePrepared_NoParams() throws Exception {
        Connection realConn = registerRealConnection();
        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE IF EXISTS prep_test");
            stmt.execute("CREATE TABLE prep_test (id INTEGER)");
            stmt.execute("INSERT INTO prep_test VALUES (1)");
        }

        backend().prepared.put("stmt1", new String[] {"SELECT 1", "0"});
        CobolEsql.executePrepared(sqlca, "stmt1");
        assertEquals(0, getSqlCode(), "ExecutePrepared should succeed");

        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE prep_test");
        }
    }

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testExecutePrepared_WithParams() throws Exception {
        Connection realConn = registerRealConnection();
        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE IF EXISTS prep_test");
            stmt.execute("CREATE TABLE prep_test (id INTEGER)");
        }

        backend().prepared.put("stmt1", new String[] {"INSERT INTO prep_test VALUES (?)", "1"});
        CobolEsql.executePrepared(sqlca, "stmt1", makeNumericField(4, "0042".getBytes()));
        assertEquals(0, getSqlCode(), "ExecutePrepared with params should succeed");

        try (Statement stmt = realConn.createStatement();
                java.sql.ResultSet rs = stmt.executeQuery("SELECT id FROM prep_test")) {
            assertTrue(rs.next(), "Should have a row");
            assertEquals(42, rs.getInt(1), "Prepared insert value should be 42");
        }

        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE prep_test");
        }
    }

    // ============================================================
    // Error handling
    // ============================================================

    @Test
    void testExec_TableNotExists() throws Exception {
        registerRealConnection();
        CobolEsql.exec(sqlca, "SELECT * FROM table_that_does_not_exist");
        assertNotEquals(0, getSqlCode(), "Query on nonexistent table should fail");
    }

    @Test
    void testExecWithParams_TableNotExists() throws Exception {
        registerRealConnection();
        CobolEsql.execWithParams(
                sqlca,
                "INSERT INTO table_that_does_not_exist VALUES (?)",
                makeNumericField(4, "0001".getBytes()));
        assertNotEquals(0, getSqlCode(), "Insert into nonexistent table should fail");
    }

    // ============================================================
    // selectIntoOccurs with real DB
    // ============================================================

    @Test
    void testSelectIntoOccurs_WithResults() throws Exception {
        Connection realConn = registerRealConnection();
        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE IF EXISTS occ_test");
            stmt.execute("CREATE TABLE occ_test (val VARCHAR(10))");
            stmt.execute("INSERT INTO occ_test VALUES ('AAA')");
            stmt.execute("INSERT INTO occ_test VALUES ('BBB')");
            stmt.execute("INSERT INTO occ_test VALUES ('CCC')");
        }

        // Create an array large enough for 3 rows of 10 bytes each
        byte[] data = new byte[30];
        AbstractCobolField field = makeAlphaField(10, data);
        CobolEsql.selectIntoOccurs(
                sqlca,
                10,
                3,
                "SELECT val FROM occ_test ORDER BY val",
                null,
                new AbstractCobolField[] {field});
        assertEquals(0, getSqlCode(), "SelectIntoOccurs should succeed");

        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE occ_test");
        }
    }

    @Test
    void testSelectIntoOccurs_NoResults() throws Exception {
        Connection realConn = registerRealConnection();
        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE IF EXISTS occ_test");
            stmt.execute("CREATE TABLE occ_test (val VARCHAR(10))");
        }

        byte[] data = new byte[30];
        AbstractCobolField field = makeAlphaField(10, data);
        CobolEsql.selectIntoOccurs(
                sqlca, 10, 3, "SELECT val FROM occ_test", null, new AbstractCobolField[] {field});
        assertEquals(
                SqlCA.ECPG_NOT_FOUND,
                getSqlCode(),
                "SelectIntoOccurs with no results should return ECPG_NOT_FOUND");

        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE occ_test");
        }
    }

    @Test
    void testSelectIntoOccurs_WithInputParams() throws Exception {
        Connection realConn = registerRealConnection();
        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE IF EXISTS occ_test");
            stmt.execute("CREATE TABLE occ_test (id INTEGER, val VARCHAR(10))");
            stmt.execute("INSERT INTO occ_test VALUES (1, 'AAA')");
            stmt.execute("INSERT INTO occ_test VALUES (1, 'BBB')");
        }

        byte[] data = new byte[20];
        AbstractCobolField field = makeAlphaField(10, data);
        AbstractCobolField input = makeNumericField(4, "0001".getBytes());
        CobolEsql.selectIntoOccurs(
                sqlca,
                10,
                2,
                "SELECT val FROM occ_test WHERE id = ?",
                new AbstractCobolField[] {input},
                new AbstractCobolField[] {field});
        assertEquals(0, getSqlCode(), "SelectIntoOccurs with input params should succeed");

        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE occ_test");
        }
    }

    // ============================================================
    // fetchCursorOccurs with real DB
    // ============================================================

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testFetchCursorOccurs_WithData() throws Exception {
        Connection realConn = registerRealConnection();
        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE IF EXISTS focc_test");
            stmt.execute("CREATE TABLE focc_test (val VARCHAR(10))");
            stmt.execute("INSERT INTO focc_test VALUES ('XX')");
            stmt.execute("INSERT INTO focc_test VALUES ('YY')");
        }

        CobolEsql.declareCursor(sqlca, "focc", "SELECT val FROM focc_test ORDER BY val");
        assertEquals(0, getSqlCode(), "Declare cursor should succeed");

        CobolEsql.openCursor(sqlca, "focc");
        assertEquals(0, getSqlCode(), "Open cursor should succeed");

        byte[] data = new byte[20];
        AbstractCobolField field = makeAlphaField(10, data);
        CobolEsql.fetchCursorOccurs(sqlca, "focc", 10, 2, field);
        assertEquals(0, getSqlCode(), "FetchCursorOccurs should succeed");

        CobolEsql.closeCursor(sqlca, "focc");
        assertEquals(0, getSqlCode(), "Close cursor should succeed");

        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE focc_test");
        }
    }

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testFetchCursorOccurs_EmptyResult() throws Exception {
        Connection realConn = registerRealConnection();
        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE IF EXISTS focc_test2");
            stmt.execute("CREATE TABLE focc_test2 (val VARCHAR(10))");
        }

        CobolEsql.declareCursor(sqlca, "focc2", "SELECT val FROM focc_test2");
        assertEquals(0, getSqlCode(), "Declare cursor should succeed");
        CobolEsql.openCursor(sqlca, "focc2");
        assertEquals(0, getSqlCode(), "Open cursor should succeed");

        byte[] data = new byte[20];
        AbstractCobolField field = makeAlphaField(10, data);
        CobolEsql.fetchCursorOccurs(sqlca, "focc2", 10, 2, field);
        assertEquals(0, getSqlCode(), "FetchCursorOccurs on empty should succeed");

        CobolEsql.closeCursor(sqlca, "focc2");
        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE focc_test2");
        }
    }

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testFetchCursorOccurs_EndOfDataResetsRowCount() throws Exception {
        // 全行数 (2) が occursMax (2) のちょうど整数倍。満杯バッチを取得した後の 2 回目の
        // FETCH は 0 行 (結果末尾) になる。このとき SQLCODE=0 を保ちつつ SQLERRD(3) が
        // 前回の 2 のまま残らず 0 にリセットされることを検証する (stale 行数による再処理防止)。
        Connection realConn = registerRealConnection();
        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE IF EXISTS focc_eod");
            stmt.execute("CREATE TABLE focc_eod (val VARCHAR(10))");
            stmt.execute("INSERT INTO focc_eod VALUES ('AA')");
            stmt.execute("INSERT INTO focc_eod VALUES ('BB')");
        }

        CobolEsql.declareCursor(sqlca, "feod", "SELECT val FROM focc_eod ORDER BY val");
        CobolEsql.openCursor(sqlca, "feod");
        assertEquals(0, getSqlCode(), "open should succeed");

        // 1 回目: 満杯の 2 行を取得。SQLERRD(3)=2。
        byte[] data = new byte[20];
        AbstractCobolField field = makeAlphaField(10, data);
        CobolEsql.fetchCursorOccurs(sqlca, "feod", 10, 2, field);
        assertEquals(0, getSqlCode(), "first batch should succeed");
        assertEquals(2, getRowCount(), "first batch should fetch 2 rows");

        // 2 回目: 末尾なので 0 行。SQLCODE=0 を保ちつつ SQLERRD(3)=0 にリセットされること。
        CobolEsql.fetchCursorOccurs(sqlca, "feod", 10, 2, field);
        assertEquals(0, getSqlCode(), "end-of-data batch should keep SQLCODE=0");
        assertEquals(0, getRowCount(), "end-of-data batch should reset SQLERRD(3) to 0");

        CobolEsql.closeCursor(sqlca, "feod");
        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE focc_eod");
        }
    }

    // ============================================================
    // openCursorWithParams with real DB
    // ============================================================

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testOpenCursorWithParams_Success() throws Exception {
        Connection realConn = registerRealConnection();
        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE IF EXISTS ocp_test");
            stmt.execute("CREATE TABLE ocp_test (id INTEGER, val VARCHAR(10))");
            stmt.execute("INSERT INTO ocp_test VALUES (1, 'Hello')");
        }

        CobolEsql.declareCursorWithParams(
                sqlca,
                "ocp",
                "SELECT val FROM ocp_test WHERE id = ?",
                makeNumericField(4, "0001".getBytes()));
        assertEquals(0, getSqlCode(), "DeclareCursorWithParams should succeed");

        CobolEsql.openCursorWithParams(sqlca, "ocp", makeNumericField(4, "0001".getBytes()));
        assertEquals(0, getSqlCode(), "OpenCursorWithParams should succeed");

        byte[] data = new byte[10];
        AbstractCobolField field = makeAlphaField(10, data);
        CobolEsql.fetchCursor(sqlca, "ocp", field);
        assertEquals(0, getSqlCode(), "FetchCursor should succeed");
        String val = new String(field.getDataStorage().getByteArray(0, 5)).trim();
        assertEquals("Hello", val, "Fetched value should be Hello");

        CobolEsql.closeCursor(sqlca, "ocp");
        try (Statement stmt = realConn.createStatement()) {
            stmt.execute("DROP TABLE ocp_test");
        }
    }

    // ============================================================
    // prepare with VARYING prefix
    // ============================================================

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testPrepare_VaryingHostVariable() {
        // Proper VARYING host variable: 4-byte binary length header followed by the
        // data (mirrors a COBOL `PIC X(n) VARYING` field). The SQL text must be read
        // from the data using the length header, not by sniffing leading digits.
        byte[] sql = "SELECT * FROM t".getBytes();
        byte[] data = new byte[4 + sql.length];
        ByteBuffer.wrap(data, 0, 4).putInt(sql.length);
        System.arraycopy(sql, 0, data, 4, sql.length);
        CobolFieldAttribute attr =
                new CobolFieldAttribute(
                        CobolFieldAttribute.COB_TYPE_GROUP,
                        0,
                        0,
                        CobolFieldAttribute.COB_FLAG_VARYING,
                        null);
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(data.length, new CobolDataStorage(data), attr);
        CobolEsql.prepare(sqlca, "stmt3", field);
        assertEquals(0, getSqlCode(), "Prepare with VARYING host variable should succeed");
        String[] prepared = backend().prepared.get("stmt3");
        assertNotNull(prepared, "Prepared statement should be registered");
        assertEquals(
                "SELECT * FROM t",
                prepared[0],
                "Prepared query should be read from VARYING data via its length header");
    }

    @Test
    void testPrepare_EmptyQueryField() {
        CobolFieldAttribute attr =
                new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null);
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(0, new CobolDataStorage(1), attr);
        CobolEsql.prepare(sqlca, "stmt1", field);
        assertEquals(
                SqlCA.ECPG_EMPTY,
                getSqlCode(),
                "Prepare with empty query field should return ECPG_EMPTY");
    }

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testPrepare_HostVarWithParenthesis() {
        byte[] data = "INSERT INTO t(id) VALUES(:id)".getBytes();
        AbstractCobolField field = makeAlphaField(data.length, data);
        CobolEsql.prepare(sqlca, "stmt4", field);
        assertEquals(0, getSqlCode(), "Prepare with host var in parens should succeed");
        String[] prepared = backend().prepared.get("stmt4");
        assertNotNull(prepared, "Prepared statement should be registered");
        assertTrue(prepared[0].contains("?"), "Host var should be replaced with ?");
        assertTrue(prepared[0].contains(")"), "Parenthesis should be preserved");
    }

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testPrepare_CastOperatorPreserved() {
        byte[] data = "SELECT col::text FROM t WHERE id = :id::integer".getBytes();
        AbstractCobolField field = makeAlphaField(data.length, data);
        CobolEsql.prepare(sqlca, "stmtCast", field);
        assertEquals(0, getSqlCode(), "Prepare with :: cast should succeed");
        String[] prepared = backend().prepared.get("stmtCast");
        assertNotNull(prepared, "Prepared statement should be registered");
        assertTrue(prepared[0].contains("col::text"), ":: cast operator should be preserved");
        assertTrue(prepared[0].contains("?::integer"), ":id should become ? and ::integer kept");
        assertEquals("1", prepared[1], "Only :id is a parameter");
    }

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testPrepare_StringLiteralColonPreserved() {
        byte[] data = "INSERT INTO t (a, b) VALUES (:v, 'x:y')".getBytes();
        AbstractCobolField field = makeAlphaField(data.length, data);
        CobolEsql.prepare(sqlca, "stmtLit", field);
        assertEquals(0, getSqlCode(), "Prepare with string literal should succeed");
        String[] prepared = backend().prepared.get("stmtLit");
        assertNotNull(prepared, "Prepared statement should be registered");
        assertTrue(
                prepared[0].contains("'x:y'"), "Colon inside a string literal should be preserved");
        assertEquals("1", prepared[1], "Only :v is a parameter, not :y in the literal");
    }

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testPrepare_DoubleQuotedIdentifierColon() {
        byte[] data = "SELECT \"co:l\" FROM t WHERE id = :id".getBytes();
        AbstractCobolField field = makeAlphaField(data.length, data);
        CobolEsql.prepare(sqlca, "stmtDq", field);
        assertEquals(0, getSqlCode(), "Prepare with quoted identifier should succeed");
        String[] prepared = backend().prepared.get("stmtDq");
        assertNotNull(prepared, "Prepared statement should be registered");
        assertTrue(
                prepared[0].contains("\"co:l\""),
                "Colon inside a quoted identifier should be preserved");
        assertEquals("1", prepared[1], "Only :id is a parameter");
    }

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testPrepare_EscapedQuoteInLiteral() {
        byte[] data = "INSERT INTO t VALUES ('it''s :x')".getBytes();
        AbstractCobolField field = makeAlphaField(data.length, data);
        CobolEsql.prepare(sqlca, "stmtEsc", field);
        assertEquals(0, getSqlCode(), "Prepare with escaped quote should succeed");
        String[] prepared = backend().prepared.get("stmtEsc");
        assertNotNull(prepared, "Prepared statement should be registered");
        assertTrue(
                prepared[0].contains("'it''s :x'"),
                "Escaped quote and colon inside the literal should be preserved");
        assertEquals("0", prepared[1], "No parameters; :x is inside a string literal");
    }

    // ============================================================
    // ExecWithParams commit/rollback paths
    // ============================================================

    @Test
    void testExecWithParams_CommitQuery() throws Exception {
        registerRealConnection();
        backend().cursors.put("c1", new AbstractCobolEsqlBackend.Cursor("c1", "SELECT 1", 0));
        backend().cursors.get("c1").isOpened = true;
        CobolEsql.execWithParams(sqlca, "COMMIT");
        // Accept both success (0) and "no transaction" warning (-604)
        assertTrue(
                getSqlCode() == 0 || getSqlCode() == SqlCA.ECPG_WARNING_NO_TRANSACTION,
                "Unexpected code: " + getSqlCode());
    }

    @Test
    void testExecWithParams_RollbackQuery() throws Exception {
        registerRealConnection();
        CobolEsql.execWithParams(sqlca, "ROLLBACK");
        // Accept both success (0) and "no transaction" warning (-604)
        assertTrue(
                getSqlCode() == 0 || getSqlCode() == SqlCA.ECPG_WARNING_NO_TRANSACTION,
                "Unexpected code: " + getSqlCode());
    }

    @Test
    void testExecWithParams_EmptyQuery() throws Exception {
        registerRealConnection();
        CobolEsql.execWithParams(sqlca, "", makeNumericField(4, "0042".getBytes()));
        assertEquals(
                SqlCA.ECPG_EMPTY,
                getSqlCode(),
                "ExecWithParams with empty query should return ECPG_EMPTY");
    }

    // ============================================================
    // Full integration: connect -> create -> insert -> select -> disconnect
    // ============================================================

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testFullLifecycle() throws Exception {
        connectToPostgres();

        // CREATE TABLE
        CobolEsql.exec(sqlca, "CREATE TABLE lifecycle_test (id INTEGER, name VARCHAR(20))");
        assertEquals(0, getSqlCode(), "CREATE TABLE should succeed");

        // INSERT with params
        CobolEsql.execWithParams(
                sqlca,
                "INSERT INTO lifecycle_test VALUES (?, ?)",
                makeNumericField(4, "0001".getBytes()),
                makeAlphaField(5, "Alice".getBytes()));
        assertEquals(0, getSqlCode(), "INSERT should succeed");

        // SELECT INTO
        byte[] nameData = new byte[20];
        AbstractCobolField nameField = makeAlphaField(20, nameData);
        CobolEsql.selectInto(
                sqlca,
                "SELECT name FROM lifecycle_test WHERE id = 1",
                null,
                new AbstractCobolField[] {nameField});
        assertEquals(0, getSqlCode(), "SELECT INTO should succeed");
        String fetchedName = new String(nameField.getDataStorage().getByteArray(0, 5)).trim();
        assertEquals("Alice", fetchedName, "Fetched name should be Alice");

        // COMMIT
        CobolEsql.commit(sqlca);
        assertEquals(0, getSqlCode(), "COMMIT should succeed");

        // Clean up
        CobolEsql.exec(sqlca, "DROP TABLE lifecycle_test");
        assertEquals(0, getSqlCode(), "DROP TABLE should succeed");

        // Disconnect
        CobolEsql.disconnect(sqlca);
        assertEquals(0, getSqlCode(), "DISCONNECT should succeed");
    }
}
