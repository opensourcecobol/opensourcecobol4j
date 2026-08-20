package jp.osscons.opensourcecobol.libcobj.sql;

import static org.junit.jupiter.api.Assertions.*;

import jp.osscons.opensourcecobol.libcobj.exceptions.CobolRuntimeException;
import org.junit.jupiter.api.Test;

/**
 * {@link CobolEsqlBackendFactory#resolve(String)} が {@code OCDB_DB_TYPE} の値からバックエンドを
 * 選ぶ規則（未指定なら PostgreSQL、大文字小文字を区別しない、対応する実装が無ければ例外）を検証する。
 *
 * <p>解決に失敗したときの実行時の見え方（実行が打ち切られ、原因が stderr に出る）は
 * {@code tests/esql-misc.src/db-type-env.at} が実際のプログラムで検証している。
 */
class CobolEsqlBackendFactoryTest {

    @Test
    void testResolve_Null_DefaultsToPostgresql() {
        assertInstanceOf(
                CobolEsqlBackendPostgresql.class, CobolEsqlBackendFactory.resolve((String) null));
    }

    @Test
    void testResolve_Empty_DefaultsToPostgresql() {
        assertInstanceOf(CobolEsqlBackendPostgresql.class, CobolEsqlBackendFactory.resolve(""));
    }

    @Test
    void testResolve_Postgresql() {
        assertInstanceOf(
                CobolEsqlBackendPostgresql.class, CobolEsqlBackendFactory.resolve("postgresql"));
    }

    @Test
    void testResolve_CaseInsensitive() {
        assertInstanceOf(
                CobolEsqlBackendPostgresql.class, CobolEsqlBackendFactory.resolve("PostgreSQL"));
    }

    @Test
    void testResolve_Unsupported_Throws() {
        // 実装される見込みのない名前を使う（将来 DB を追加してもこのテストが意味を失わないように）。
        assertThrows(
                CobolRuntimeException.class,
                () -> CobolEsqlBackendFactory.resolve("Non-existentDB"));
    }
}
