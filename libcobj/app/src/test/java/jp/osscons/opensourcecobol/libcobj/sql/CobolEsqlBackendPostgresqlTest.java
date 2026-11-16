package jp.osscons.opensourcecobol.libcobj.sql;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import java.nio.ByteBuffer;
import java.sql.SQLException;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * {@link CobolEsqlBackendPostgresql} の DB 依存ロジック（SQLSTATE→ECPG 変換、JDBC URL 構築、接続文字列
 * パース）と、{@link AbstractCobolEsqlBackend} の共通エラー報告（setResultFromException）を、
 * DB 接続なしで検証する。旧 SqlCATest の変換検証と旧 SqlConnectionTest の URL 検証の移植先。
 */
class CobolEsqlBackendPostgresqlTest {

    private CobolEsqlBackendPostgresql pg;
    private CobolDataStorage sqlca;

    @BeforeEach
    void setUp() {
        pg = new CobolEsqlBackendPostgresql();
        sqlca = new CobolDataStorage(133);
    }

    private int codeOf(String sqlState) {
        return pg.mapSqlException(new SQLException("msg", sqlState)).sqlCode;
    }

    // ---------- sqlStateToCode（旧 SqlCATest から移植。SQLException 経由で検証）----------

    @Test
    void testSqlStateToCode_NoError() {
        assertEquals(SqlCA.ECPG_NO_ERROR, codeOf("00000"), "00000 -> ECPG_NO_ERROR");
    }

    @Test
    void testSqlStateToCode_NotFound() {
        assertEquals(SqlCA.ECPG_NOT_FOUND, codeOf("02000"), "02000 -> ECPG_NOT_FOUND");
    }

    @Test
    void testSqlStateToCode_Empty() {
        assertEquals(SqlCA.ECPG_EMPTY, codeOf("YE002"), "YE002 -> ECPG_EMPTY");
    }

    @Test
    void testSqlStateToCode_Connect_08001() {
        assertEquals(SqlCA.ECPG_CONNECT, codeOf("08001"), "08001 -> ECPG_CONNECT");
    }

    @Test
    void testSqlStateToCode_Connect_08003() {
        assertEquals(SqlCA.ECPG_CONNECT, codeOf("08003"), "08003 -> ECPG_CONNECT");
    }

    @Test
    void testSqlStateToCode_Trans() {
        assertEquals(SqlCA.ECPG_TRANS, codeOf("08007"), "08007 -> ECPG_TRANS");
    }

    @Test
    void testSqlStateToCode_SubselectNotOne() {
        assertEquals(
                SqlCA.ECPG_SUBSELECT_NOT_ONE, codeOf("21000"), "21000 -> ECPG_SUBSELECT_NOT_ONE");
    }

    @Test
    void testSqlStateToCode_DuplicateKey() {
        assertEquals(SqlCA.ECPG_DUPLICATE_KEY, codeOf("23505"), "23505 -> ECPG_DUPLICATE_KEY");
    }

    @Test
    void testSqlStateToCode_InTransaction() {
        assertEquals(
                SqlCA.ECPG_WARNING_IN_TRANSACTION,
                codeOf("25001"),
                "25001 -> WARNING_IN_TRANSACTION");
    }

    @Test
    void testSqlStateToCode_NoTransaction() {
        assertEquals(
                SqlCA.ECPG_WARNING_NO_TRANSACTION,
                codeOf("25P01"),
                "25P01 -> WARNING_NO_TRANSACTION");
    }

    @Test
    void testSqlStateToCode_UnknownPortal() {
        assertEquals(
                SqlCA.ECPG_WARNING_UNKNOWN_PORTAL,
                codeOf("34000"),
                "34000 -> WARNING_UNKNOWN_PORTAL");
    }

    @Test
    void testSqlStateToCode_DataFormatError() {
        assertEquals(
                SqlCA.ECPG_DATA_FORMAT_ERROR, codeOf("42804"), "42804 -> ECPG_DATA_FORMAT_ERROR");
    }

    @Test
    void testSqlStateToCode_PortalExists() {
        assertEquals(
                SqlCA.ECPG_WARNING_PORTAL_EXISTS,
                codeOf("42P03"),
                "42P03 -> WARNING_PORTAL_EXISTS");
    }

    @Test
    void testSqlStateToCode_Pgsql() {
        assertEquals(SqlCA.ECPG_PGSQL, codeOf("55P03"), "55P03 -> ECPG_PGSQL");
    }

    @Test
    void testSqlStateToCode_AuthFailure_Unknown() {
        // 認証失敗 (28P01/28000) は明示マッピングを持たず不明エラーに落ちる（Open COBOL ESQL 4J と同じ）。
        assertEquals(SqlCA.ECPG_UNKNOWN_ERROR, codeOf("28P01"), "28P01 has no explicit mapping");
        assertEquals(SqlCA.ECPG_UNKNOWN_ERROR, codeOf("28000"), "28000 has no explicit mapping");
    }

    @Test
    void testSqlStateToCode_Unknown() {
        assertEquals(SqlCA.ECPG_UNKNOWN_ERROR, codeOf("XXXXX"), "unknown -> ECPG_UNKNOWN_ERROR");
    }

    @Test
    void testSqlStateToCode_Null() {
        assertEquals(SqlCA.ECPG_UNKNOWN_ERROR, codeOf(null), "null state -> ECPG_UNKNOWN_ERROR");
    }

    // ---------- mapSqlException が (コード, SQLSTATE) を一貫して返すこと ----------

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testMapSqlException_CarriesSqlState() {
        SqlErrorMapping m = pg.mapSqlException(new SQLException("dup", "23505"));
        assertEquals(SqlCA.ECPG_DUPLICATE_KEY, m.sqlCode, "code mapped from 23505");
        // PostgreSQL のネイティブ SQLSTATE はそのまま正規化値として採用される。
        assertEquals("23505", m.sqlState, "SQLSTATE carried through unchanged");
    }

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testMapSqlException_NullSqlState() {
        SqlErrorMapping m = pg.mapSqlException(new SQLException("err", (String) null));
        assertEquals(SqlCA.ECPG_UNKNOWN_ERROR, m.sqlCode, "null state -> ECPG_UNKNOWN_ERROR");
        assertNull(m.sqlState, "null SQLSTATE carried through as null (common flow fills spaces)");
    }

    // ---------- setResultFromException（旧 SqlCATest から移植。共通フローを backend で検証）----------

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testSetResultFromException() {
        pg.setResultFromException(sqlca, new SQLException("connection refused", "08001"));
        assertEquals(SqlCA.ECPG_CONNECT, SqlCA.getCode(sqlca), "SQLCODE should be ECPG_CONNECT");
        byte[] state = sqlca.getByteArray(128, 5);
        assertEquals("08001", new String(state), "SQLSTATE should be 08001");
    }

    @Test
    void testSetResultFromException_NullSqlState() {
        pg.setResultFromException(sqlca, new SQLException("some error", (String) null));
        byte[] state = sqlca.getByteArray(128, 5);
        assertEquals("     ", new String(state), "SQLSTATE should be spaces for null state");
    }

    @Test
    void testSetResultFromException_NullMessage() {
        pg.setResultFromException(sqlca, new SQLException(null, "00000"));
        short len = ByteBuffer.wrap(sqlca.getByteArray(16, 2)).getShort();
        assertEquals(0, len, "ERRMC length should be 0 for null message");
    }

    @Test
    void testSetResultFromException_NullSqlca() {
        assertDoesNotThrow(
                () -> pg.setResultFromException(null, new SQLException("test", "00000")),
                "setResultFromException with null sqlca should not throw");
    }

    // ---------- SqlCA.setNotFound（NOT_FOUND 集約ヘルパ）----------

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testSetNotFound() {
        SqlCA.setNotFound(sqlca);
        assertEquals(
                SqlCA.ECPG_NOT_FOUND, SqlCA.getCode(sqlca), "SQLCODE should be ECPG_NOT_FOUND");
        assertEquals("02000", new String(sqlca.getByteArray(128, 5)), "SQLSTATE should be 02000");
    }

    @Test
    void testSetNotFound_NullSqlca() {
        assertDoesNotThrow(
                () -> SqlCA.setNotFound(null), "setNotFound with null sqlca should not throw");
    }

    // ---------- buildJdbcUrl / 接続文字列パース（旧 SqlConnectionTest から移植）----------

    private String url(String dbSpecString) {
        return pg.buildJdbcUrl(pg.buildSpec(null, null, dbSpecString));
    }

    @Test
    void testBuildJdbcUrl_NullSpec() {
        // env フォールバックが絡むため、OCDB_DB_NAME 未設定時のみ検証する。
        assumeTrue(System.getenv("OCDB_DB_NAME") == null);
        assertEquals("jdbc:postgresql://localhost:5432/", url(null));
    }

    @Test
    void testBuildJdbcUrl_EmptySpec() {
        assumeTrue(System.getenv("OCDB_DB_NAME") == null);
        assertEquals("jdbc:postgresql://localhost:5432/", url(""));
    }

    @Test
    void testBuildJdbcUrl_DbNameOnly() {
        assertEquals("jdbc:postgresql://localhost/mydb", url("mydb"));
    }

    @Test
    void testBuildJdbcUrl_DbNameAndHost() {
        assertEquals("jdbc:postgresql://myhost/mydb", url("mydb@myhost"));
    }

    @Test
    void testBuildJdbcUrl_DbNameHostPort() {
        assertEquals("jdbc:postgresql://myhost:5433/mydb", url("mydb@myhost:5433"));
    }

    @Test
    void testBuildJdbcUrl_EmptyHostWithPort() {
        // "mydb@:5433": host が空なので localhost にフォールバックする。
        assertEquals("jdbc:postgresql://localhost:5433/mydb", url("mydb@:5433"));
    }

    @Test
    void testBuildJdbcUrl_HostNoPort() {
        assertEquals("jdbc:postgresql://server1/testdb", url("testdb@server1"));
    }

    @Test
    void testBuildJdbcUrl_MultipleAtSigns() {
        // 最後の '@' で分割する: host="c", dbname="a@b"。
        assertEquals("jdbc:postgresql://c/a@b", url("a@b@c"));
    }

    @Test
    @SuppressWarnings("PMD.JUnitTestContainsTooManyAsserts")
    void testBuildSpec_StripsTrailingSpaces() {
        DbSpec spec = pg.buildSpec("user  ", "pass  ", "mydb@myhost  ");
        assertEquals("user", spec.user, "trailing spaces stripped from user");
        assertEquals("pass", spec.passwd, "trailing spaces stripped from passwd");
        assertEquals("mydb", spec.dbname, "trailing spaces stripped from dbname");
        assertEquals("myhost", spec.host, "host parsed");
    }
}
