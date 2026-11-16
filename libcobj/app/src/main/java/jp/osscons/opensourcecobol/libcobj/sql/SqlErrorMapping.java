package jp.osscons.opensourcecobol.libcobj.sql;

import java.sql.SQLException;

/**
 * {@link AbstractCobolEsqlBackend#mapSqlException(SQLException)} の戻り値。JDBC ドライバが投げた
 * {@link SQLException} を、COBOL 側が見る SQLCODE と SQLSTATE の組へ読み替えた結果を保持する。
 */
public final class SqlErrorMapping {
    /** ECPG が定めるエラーコード（SQLCA の SQLCODE に書かれる）。 */
    final int sqlCode;

    /** ECPG が定める SQLSTATE（5 桁、SQLCA の SQLSTATE に書かれる）。{@code null} なら空白を補う。 */
    final String sqlState;

    /**
     * エラーコードと SQLSTATE の組を生成する。
     *
     * @param sqlCode ECPG が定めるエラーコード（SQLCODE）
     * @param sqlState ECPG が定める SQLSTATE（5 桁、{@code null} 可）
     */
    public SqlErrorMapping(int sqlCode, String sqlState) {
        this.sqlCode = sqlCode;
        this.sqlState = sqlState;
    }

    /**
     * エラーコード（SQLCODE）を返す。
     *
     * @return ECPG が定めるエラーコード
     */
    public int getSqlCode() {
        return sqlCode;
    }

    /**
     * SQLSTATE を返す。
     *
     * @return ECPG が定める SQLSTATE（5 桁）。未設定なら {@code null}
     */
    public String getSqlState() {
        return sqlState;
    }
}
