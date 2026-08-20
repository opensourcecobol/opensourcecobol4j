package jp.osscons.opensourcecobol.libcobj.sql;

import java.sql.Connection;
import java.sql.ParameterMetaData;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;

/**
 * PostgreSQL 向けの {@link CobolEsqlBackendInterface} 実装。{@link AbstractCobolEsqlBackend} の DB 依存フックを
 * PostgreSQL の流儀（明示 BEGIN によるトランザクション管理、サーバカーソル + {@code FETCH FORWARD}
 * による先読み、SQLSTATE をそのまま使うエラー変換）で実装する。
 *
 * <p>{@link java.util.ServiceLoader} 経由で発見されるため public であり、
 * {@code META-INF/services/jp.osscons.opensourcecobol.libcobj.sql.CobolEsqlBackendInterface} に
 * 登録されている。
 */
public final class CobolEsqlBackendPostgresql extends AbstractCobolEsqlBackend {

    /**
     * カーソル名 → 先読みバッファ。サーバカーソルから {@code FETCH FORWARD} でまとめて取ってくる
     * のは PostgreSQL 側の都合なので、基底クラスが持つカーソルの状態とは別に本クラスで持つ。
     * JDBC 資源は含まないため、破棄はメモリ上の状態を捨てるだけでよい。
     */
    final Map<String, PgCursorState> cursorStates = new HashMap<>();

    /** 1 カーソルぶんの先読み（バルクフェッチ）状態。OPEN のたびに空の状態へ置き換える。 */
    static final class PgCursorState {

        /** 先読みバッファ。各要素は 1 行ぶんの列値配列で、SQL NULL の列は null。 */
        List<byte[][]> fetchBuffer = new ArrayList<>();

        /** {@link #fetchBuffer} 内で次に供給する行の位置。 */
        int bufferPos;

        /**
         * 直近の先読みが要求件数より少ない行数で終わった（＝結果末尾に達した）かどうか。
         * WHERE CURRENT OF のカーソル位置補正に使う（Open COBOL ESQL 4J の overFetch 相当）。
         */
        boolean overFetch;

        /** まだ COBOL 側へ供給していない（バッファに残っている）先読み行数を返す。 */
        int remainingBuffered() {
            return fetchBuffer.size() - bufferPos;
        }
    }

    /** 指定カーソルの先読み状態を返す（未登録なら空の状態を割り当てる）。 */
    private PgCursorState state(String cursorName) {
        return cursorStates.computeIfAbsent(cursorName, k -> new PgCursorState());
    }

    @Override
    public String id() {
        return "postgresql";
    }

    // -------------------------------------------------------
    // 接続・トランザクション
    // -------------------------------------------------------

    @Override
    protected String buildJdbcUrl(DbSpec s) {
        return "jdbc:postgresql://" + s.host + s.port + "/" + s.dbname;
    }

    @Override
    protected void configureConnection(Connection c) throws SQLException {
        // PostgreSQL は autoCommit(true) のまま、明示 BEGIN でトランザクションを管理する。
        c.setAutoCommit(true);
        begin(c);
    }

    @Override
    protected void commitTransaction(Connection c) throws SQLException {
        // autoCommit(true) のまま明示 BEGIN / COMMIT でトランザクションを管理するため、COMMIT に
        // 続けて次の BEGIN を発行する。BEGIN を欠くと以降の SQL が 1 文ごとに確定してしまい、
        // COBOL プログラムが次に COMMIT / ROLLBACK を書くまで確定を保留する、という埋め込み SQL の
        // 前提が崩れる（後から ROLLBACK しても戻せなくなる）。
        issueCommit(c);
        begin(c);
    }

    @Override
    protected void commitBeforeClose(Connection c) throws SQLException {
        // 直後に接続を閉じるため、次の BEGIN は発行しない。発行しても空のトランザクションが
        // クローズで破棄されるだけで、往復 1 回と紛らわしい BEGIN のログが増える。
        issueCommit(c);
    }

    @Override
    protected void rollbackTransaction(Connection c) throws SQLException {
        LOG.debug("ROLLBACK (postgresql)");
        try (Statement stmt = c.createStatement()) {
            stmt.execute("ROLLBACK");
        }
        begin(c);
    }

    /** {@code COMMIT} を発行する。次のトランザクションを開始するかどうかは呼び出し側が決める。 */
    private void issueCommit(Connection c) throws SQLException {
        LOG.debug("COMMIT (postgresql)");
        try (Statement stmt = c.createStatement()) {
            stmt.execute("COMMIT");
        }
    }

    /** 明示 {@code BEGIN} を発行して新しいトランザクションを開始する（PostgreSQL 固有の流儀）。 */
    private void begin(Connection c) throws SQLException {
        if (c != null && !c.isClosed()) {
            LOG.debug("BEGIN (postgresql)");
            try (Statement stmt = c.createStatement()) {
                stmt.execute("BEGIN");
            }
        }
    }

    // -------------------------------------------------------
    // カーソル
    // -------------------------------------------------------

    @Override
    protected void openCursorImpl(
            Connection c, String cursorName, String query, AbstractCobolField[] params)
            throws SQLException {
        String command = "DECLARE " + cursorName + " CURSOR FOR " + query;

        // params は基底クラスが選び終えたパラメータ（OPEN ... USING 優先、なければ DECLARE 時の
        // パラメータ）。空ならパラメータなしで Statement を実行する。
        if (LOG.isTraceEnabled()) {
            LOG.trace(
                    "DECLARE CURSOR (postgresql): {} [params={}]",
                    collapseWhitespace(command),
                    params != null ? params.length : 0);
        }
        if (params != null) {
            try (PreparedStatement pstmt = c.prepareStatement(command)) {
                ParameterMetaData metaData = pstmt.getParameterMetaData();
                for (int i = 0; i < params.length; i++) {
                    CobolDataConverter.setParam(pstmt, i + 1, metaData, params[i]);
                }
                pstmt.execute();
            }
        } else {
            try (Statement stmt = c.createStatement()) {
                stmt.execute(command);
            }
        }
        // 新たにオープンしたカーソルは先読みバッファを空から始める（DECLARE 失敗時は置き換えない）。
        cursorStates.put(cursorName, new PgCursorState());
    }

    @Override
    protected boolean fetchRowImpl(
            Connection c, String cursorName, AbstractCobolField[] out, CobolDataStorage sqlca)
            throws SQLException {
        PgCursorState st = state(cursorName);
        // バッファを使い切っていれば、OCESQL4J_FETCH_RECORDS 件をまとめて先読みする。
        if (st.bufferPos >= st.fetchBuffer.size()) {
            refill(c, cursorName, st);
        }
        if (st.bufferPos >= st.fetchBuffer.size()) {
            // 先読みしても行が無い＝これ以上の行は無い。
            return false;
        }

        byte[][] row = st.fetchBuffer.get(st.bufferPos);
        st.bufferPos++;
        if (out != null) {
            boolean sawNullWithoutIndicator = false;
            for (int i = 0; i < out.length && i < row.length; i++) {
                byte[] value = row[i];
                if (value != null) {
                    CobolDataConverter.stringToCobol(out[i], value);
                } else {
                    // 指標変数なしの NULL は型に応じた空値で埋める。空バイト列を型対応の
                    // 変換に渡すと、数値=0/英数字=空白/national=全角空白になる。raw な
                    // memset 0 は packed の符号ニブルやゾーン10進で不正表現になるため避ける。
                    CobolDataConverter.stringToCobol(out[i], EMPTY_RESULT);
                    sawNullWithoutIndicator = true;
                }
            }
            if (sawNullWithoutIndicator) {
                SqlCA.setMissingIndicator(sqlca);
            }
        }
        return true;
    }

    /**
     * {@code FETCH FORWARD <OCESQL4J_FETCH_RECORDS> FROM name} を実行し、返った全行を
     * 先読みバッファへ格納する。
     */
    private void refill(Connection c, String cursorName, PgCursorState st) throws SQLException {
        st.fetchBuffer = new ArrayList<>();
        st.bufferPos = 0;
        int fetchRecords = BulkFetchConfig.getFetchRecords();
        String fetchSql = "FETCH FORWARD " + fetchRecords + " FROM " + cursorName;
        LOG.trace("FETCH FORWARD (postgresql): {}", fetchSql);
        try (Statement stmt = c.createStatement()) {
            boolean hasResult = stmt.execute(fetchSql);
            if (!hasResult) {
                st.overFetch = false;
                return;
            }
            ResultSet rs = stmt.getResultSet();
            if (rs != null) {
                int columnCount = rs.getMetaData().getColumnCount();
                while (rs.next()) {
                    byte[][] row = new byte[columnCount][];
                    for (int i = 0; i < columnCount; i++) {
                        row[i] = CobolDataConverter.getValueFromResultSet(rs, i + 1);
                    }
                    st.fetchBuffer.add(row);
                }
                rs.close();
            }
            int size = st.fetchBuffer.size();
            // 要求件数より少ない行数しか取れなかった＝結果末尾に達した（サーバカーソルは末尾の先）。
            st.overFetch = size > 0 && size < fetchRecords;
        }
    }

    @Override
    protected void fetchOccursImpl(
            Connection c,
            String cursorName,
            int occursSize,
            int occursMax,
            AbstractCobolField[] resultParams,
            CobolDataStorage sqlca)
            throws SQLException {
        // OCCURS への複数行 FETCH は単一行 FETCH の先読みバッファを使わない。
        // 同一カーソルに対する単一行 FETCH の先読みバッファが残っているとサーバカーソル位置と
        // 食い違うため、ここで破棄しておく。
        cursorStates.remove(cursorName);
        // 登録済みだが未 OPEN のカーソルでも、ここで打ち切らず FETCH を PostgreSQL へ送り、
        // そのエラー (メッセージ・SQLSTATE) を SQLCA に反映させる (Open COBOL ESQL 4J と同じ)。
        String fetchSql = "FETCH FORWARD " + occursMax + " FROM " + cursorName;
        LOG.trace("FETCH FORWARD occurs (postgresql): {}", fetchSql);
        try (Statement stmt = c.createStatement();
                ResultSet rs = stmt.executeQuery(fetchSql)) {
            if (!rs.next()) {
                // カーソル FETCH の末尾 (0 行) は SQLCODE=0。SQLERRD(3) は呼び出し側
                // (fetchCursorOccurs) が委譲前に 0 へ初期化済みなのでここでは触らない。
                SqlCA.setSuccess(sqlca);
                return;
            }
            // OCCURS 配列への複数行書き込みと SQLCA 更新は基底の共通処理を再利用する。
            writeOccursRows(rs, resultParams, occursSize, occursMax, sqlca);
        }
    }

    @Override
    protected void closeCursorImpl(Connection c, String cursorName) throws SQLException {
        LOG.trace("CLOSE CURSOR (postgresql): {}", cursorName);
        try (Statement stmt = c.createStatement()) {
            stmt.execute("CLOSE " + cursorName);
        }
        // クローズしたカーソルの先読みバッファを破棄する（CLOSE 失敗時は保持したまま）。
        cursorStates.remove(cursorName);
    }

    @Override
    protected void repositionForCurrentOf(Connection c, String cursorName, CobolDataStorage sqlca)
            throws SQLException {
        // 先読みバッファに未供給行が remaining 行残っていれば、さらに overFetch（結果末尾に達し
        // サーバカーソルが末尾の先にある）なら +1 行ぶん FETCH BACKWARD してから、バッファを破棄する。
        PgCursorState st = state(cursorName);
        int backward = st.remainingBuffered() + (st.overFetch ? 1 : 0);
        if (backward > 0) {
            LOG.trace("FETCH BACKWARD (postgresql): {} FROM {}", backward, cursorName);
            try (Statement stmt = c.createStatement()) {
                stmt.execute("FETCH BACKWARD " + backward + " FROM " + cursorName);
            }
        }
        // 位置補正後は先読みバッファを無効化し、次の FETCH は補正後の位置から取り直す。
        cursorStates.remove(cursorName);
    }

    @Override
    protected void onCursorsInvalidated() {
        // COMMIT/ROLLBACK（および DISCONNECT）でサーバカーソルは消えるため、先読みバッファも
        // すべて破棄する。JDBC 資源は持たないので、メモリ上の状態を捨てるだけでよい。
        cursorStates.clear();
    }

    // -------------------------------------------------------
    // エラー変換
    // -------------------------------------------------------

    @Override
    protected SqlErrorMapping mapSqlException(SQLException e) {
        // JDBC ドライバが投げた SQLException を (ECPG の SQLCODE, SQLSTATE) へ読み替える。
        // PostgreSQL が返す SQLSTATE は ECPG が前提とする値そのものなので、そのまま採用する。
        // 別の DB を追加する場合は、その DB の SQLSTATE を ECPG の値へ読み替える処理がここに入る。
        // エラーの読み替えは各バックエンドに閉じ込め、SQLCODE と SQLSTATE を 1 箇所で決める。
        String sqlState = e.getSQLState();
        return new SqlErrorMapping(sqlStateToCode(sqlState), sqlState);
    }

    /** PostgreSQL の SQLSTATE → ECPG コード変換表。 */
    private static int sqlStateToCode(String sqlState) {
        if (sqlState == null) {
            return SqlCA.ECPG_UNKNOWN_ERROR;
        }
        switch (sqlState) {
            case "00000":
                return SqlCA.ECPG_NO_ERROR;
            case "02000":
                return SqlCA.ECPG_NOT_FOUND;
            case "YE002":
                return SqlCA.ECPG_EMPTY;
            case "08001":
            case "08003":
                return SqlCA.ECPG_CONNECT;
            case "08007":
                return SqlCA.ECPG_TRANS;
            case "21000":
                return SqlCA.ECPG_SUBSELECT_NOT_ONE;
            case "23505":
                return SqlCA.ECPG_DUPLICATE_KEY;
            case "25001":
                return SqlCA.ECPG_WARNING_IN_TRANSACTION;
            case "25P01":
                return SqlCA.ECPG_WARNING_NO_TRANSACTION;
            case "34000":
                return SqlCA.ECPG_WARNING_UNKNOWN_PORTAL;
            case "42804":
                return SqlCA.ECPG_DATA_FORMAT_ERROR;
            case "42P03":
                return SqlCA.ECPG_WARNING_PORTAL_EXISTS;
            case "55P03":
                return SqlCA.ECPG_PGSQL;
            default:
                return SqlCA.ECPG_UNKNOWN_ERROR;
        }
    }
}
