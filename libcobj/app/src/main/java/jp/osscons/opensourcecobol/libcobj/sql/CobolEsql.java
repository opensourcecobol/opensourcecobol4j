package jp.osscons.opensourcecobol.libcobj.sql;

import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;

/**
 * COBOL の埋め込み SQL 操作（CONNECT、EXEC SQL、カーソル、トランザクション）のエントリポイント。
 *
 * <p>本クラスは呼び出しを受け取るだけで、実際の処理は環境変数 {@code OCDB_DB_TYPE} から選んだ
 * {@link CobolEsqlBackendInterface} の実装へ渡す。メソッドの形は変えていないので、生成コード
 * （{@code CobolEsql.xxx(...)} という静的呼び出し）はそのままで、DB ごとに実装を入れ替えられる。
 */
public final class CobolEsql {

    /** ユーティリティクラスのインスタンス化を防ぐための private コンストラクタ。 */
    private CobolEsql() {}

    /** 環境変数から 1 度だけ解決する backend（遅延初期化）。 */
    static CobolEsqlBackendInterface backend;

    /**
     * backend を遅延初期化して返す。{@code OCDB_DB_TYPE} の解決はプロセスで 1 回だけ行う。
     * アクセスを synchronized で直列化し、初回同時アクセス時も生成は 1 回に保たれる。
     *
     * <p>{@code OCDB_DB_TYPE} に対応する実装が無いのは設定の誤りであり、SQL のエラーとして
     * 報告すると接続失敗などと区別がつかない。解決に失敗した場合は
     * {@link CobolEsqlBackendFactory#resolve()} の例外をそのまま伝播させ、実行を打ち切る。
     *
     * @throws jp.osscons.opensourcecobol.libcobj.exceptions.CobolRuntimeException
     *     {@code OCDB_DB_TYPE} に対応する実装が無い/生成できない場合
     */
    private static synchronized CobolEsqlBackendInterface backend() {
        if (backend == null) {
            backend = CobolEsqlBackendFactory.resolve();
        }
        return backend;
    }

    // -------------------------------------------------------
    // 接続
    // -------------------------------------------------------
    /**
     * user、password、dbname を個別のパラメータとして受け取り、データベース接続を確立する。
     *
     * @param sqlca ステータス報告用の SQLCA データストレージ
     * @param user ユーザー名フィールド
     * @param passwd パスワードフィールド
     * @param dbname データベース名フィールド
     */
    public static void connect(
            CobolDataStorage sqlca,
            AbstractCobolField user,
            AbstractCobolField passwd,
            AbstractCobolField dbname) {
        backend().connect(sqlca, user, passwd, dbname);
    }

    /**
     * commit を実行したうえで、デフォルトのデータベース接続を切断する。
     *
     * @param sqlca ステータス報告用の SQLCA データストレージ
     */
    public static void disconnect(CobolDataStorage sqlca) {
        backend().disconnect(sqlca);
    }

    // -------------------------------------------------------
    // 単純な SQL の実行（ホスト変数なし）
    // -------------------------------------------------------
    /**
     * ホスト変数パラメータを持たない SQL 文を実行する。
     *
     * @param sqlca ステータス報告用の SQLCA データストレージ
     * @param query SQL クエリ文字列
     */
    public static void exec(CobolDataStorage sqlca, String query) {
        backend().exec(sqlca, query);
    }

    // -------------------------------------------------------
    // パラメータ付き SQL の実行
    // -------------------------------------------------------
    /**
     * COBOL のホスト変数をバインドしたパラメータ付き SQL 文を実行する。
     *
     * @param sqlca ステータス報告用の SQLCA データストレージ
     * @param query '?' プレースホルダを含む SQL クエリ文字列
     * @param params COBOL のホスト変数パラメータ
     */
    public static void execWithParams(
            CobolDataStorage sqlca, String query, AbstractCobolField... params) {
        backend().execWithParams(sqlca, query, params);
    }

    // -------------------------------------------------------
    // WHERE CURRENT OF (位置付き UPDATE/DELETE)
    // -------------------------------------------------------
    /**
     * WHERE CURRENT OF を伴う UPDATE/DELETE（ホスト変数なし）を実行する。
     *
     * @param sqlca ステータス報告用の SQLCA データストレージ
     * @param query 末尾が {@code WHERE CURRENT OF} の SQL（カーソル名は含まない）
     * @param cursorName 位置付け対象の（修飾済み）カーソル名
     */
    public static void execWhereCurrentOf(CobolDataStorage sqlca, String query, String cursorName) {
        backend().execWhereCurrentOf(sqlca, query, cursorName);
    }

    /**
     * WHERE CURRENT OF を伴う UPDATE/DELETE（ホスト変数あり）を実行する。
     *
     * @param sqlca ステータス報告用の SQLCA データストレージ
     * @param query 末尾が {@code WHERE CURRENT OF} の SQL（'?' プレースホルダを含む。カーソル名は含まない）
     * @param cursorName 位置付け対象の（修飾済み）カーソル名
     * @param params COBOL のホスト変数パラメータ
     */
    public static void execWithParamsWhereCurrentOf(
            CobolDataStorage sqlca, String query, String cursorName, AbstractCobolField... params) {
        backend().execWithParamsWhereCurrentOf(sqlca, query, cursorName, params);
    }

    // -------------------------------------------------------
    // SELECT INTO
    // -------------------------------------------------------
    /**
     * SELECT INTO 文を実行し、結果を COBOL のホスト変数に書き戻す。
     *
     * @param sqlca ステータス報告用の SQLCA データストレージ
     * @param query SELECT クエリ文字列
     * @param inputParams 入力のホスト変数パラメータ（WHERE 句のバインド）
     * @param resultParams 選択された列の値を受け取る出力ホスト変数
     */
    public static void selectInto(
            CobolDataStorage sqlca,
            String query,
            AbstractCobolField[] inputParams,
            AbstractCobolField[] resultParams) {
        backend().selectInto(sqlca, query, inputParams, resultParams);
    }

    /**
     * OCCURS 配列に対する SELECT INTO 文を実行し、複数行を書き込む。
     *
     * @param sqlca SQLCA データストレージ
     * @param occursSize OCCURS 要素 1 つあたりのバイト数（ストライド）
     * @param occursMax OCCURS 要素の最大数
     * @param query SELECT クエリ文字列
     * @param inputParams 入力のホスト変数パラメータ
     * @param resultParams 出力ホスト変数（1 つの OCCURS 要素のフィールド群）
     */
    public static void selectIntoOccurs(
            CobolDataStorage sqlca,
            int occursSize,
            int occursMax,
            String query,
            AbstractCobolField[] inputParams,
            AbstractCobolField[] resultParams) {
        backend().selectIntoOccurs(sqlca, occursSize, occursMax, query, inputParams, resultParams);
    }

    // -------------------------------------------------------
    // カーソル操作
    // -------------------------------------------------------
    /**
     * パラメータを持たない SQL カーソルを宣言する。
     *
     * @param sqlca ステータス報告用の SQLCA データストレージ
     * @param cursorName カーソル名
     * @param query カーソル用の SQL クエリ
     */
    public static void declareCursor(CobolDataStorage sqlca, String cursorName, String query) {
        backend().declareCursor(sqlca, cursorName, query);
    }

    /**
     * ホスト変数パラメータを持つ SQL カーソルを宣言する。
     *
     * @param sqlca ステータス報告用の SQLCA データストレージ
     * @param cursorName カーソル名
     * @param query カーソル用の SQL クエリ
     * @param params カーソルをオープンする際にバインドするホスト変数パラメータ
     */
    public static void declareCursorWithParams(
            CobolDataStorage sqlca, String cursorName, String query, AbstractCobolField... params) {
        backend().declareCursorWithParams(sqlca, cursorName, query, params);
    }

    /**
     * 事前に宣言されたカーソルをオープンする。
     *
     * @param sqlca ステータス報告用の SQLCA データストレージ
     * @param cursorName オープンするカーソル名
     */
    public static void openCursor(CobolDataStorage sqlca, String cursorName) {
        backend().openCursor(sqlca, cursorName);
    }

    /**
     * 事前に宣言されたカーソルをホスト変数パラメータ付きでオープンする。
     *
     * @param sqlca ステータス報告用の SQLCA データストレージ
     * @param cursorName オープンするカーソル名
     * @param params カーソルクエリ用のホスト変数パラメータ
     */
    public static void openCursorWithParams(
            CobolDataStorage sqlca, String cursorName, AbstractCobolField... params) {
        backend().openCursorWithParams(sqlca, cursorName, params);
    }

    /**
     * オープン済みのカーソルから次の行を取得し、COBOL のホスト変数に格納する。
     *
     * @param sqlca ステータス報告用の SQLCA データストレージ
     * @param cursorName fetch 対象のカーソル名
     * @param resultParams 列の値を受け取る出力ホスト変数
     */
    public static void fetchCursor(
            CobolDataStorage sqlca, String cursorName, AbstractCobolField... resultParams) {
        backend().fetchCursor(sqlca, cursorName, resultParams);
    }

    /**
     * カーソルから複数行を取得し、OCCURS 配列フィールドに格納する。
     *
     * @param sqlca ステータス報告用の SQLCA データストレージ
     * @param cursorName fetch 対象のカーソル名
     * @param occursSize OCCURS 要素 1 つあたりのバイトサイズ
     * @param occursMax 取得する行の最大数
     * @param resultParams 結果パラメータフィールド（列ごとに 1 つ）
     */
    public static void fetchCursorOccurs(
            CobolDataStorage sqlca,
            String cursorName,
            int occursSize,
            int occursMax,
            AbstractCobolField... resultParams) {
        backend().fetchCursorOccurs(sqlca, cursorName, occursSize, occursMax, resultParams);
    }

    /**
     * オープン済みのカーソルをクローズする。
     *
     * @param sqlca ステータス報告用の SQLCA データストレージ
     * @param cursorName クローズするカーソル名
     */
    public static void closeCursor(CobolDataStorage sqlca, String cursorName) {
        backend().closeCursor(sqlca, cursorName);
    }

    // -------------------------------------------------------
    // prepared statement の操作
    // -------------------------------------------------------
    /**
     * SQL 文を prepare し、COBOL のホスト変数参照を '?' プレースホルダに置き換える。
     *
     * @param sqlca ステータス報告用の SQLCA データストレージ
     * @param stmtName prepared statement に割り当てる名前
     * @param queryField SQL クエリ文を保持する COBOL フィールド
     */
    public static void prepare(
            CobolDataStorage sqlca, String stmtName, AbstractCobolField queryField) {
        backend().prepare(sqlca, stmtName, queryField);
    }

    /**
     * 事前に prepare された statement を実行する。
     *
     * @param sqlca ステータス報告用の SQLCA データストレージ
     * @param stmtName prepared statement の名前
     * @param params バインドするホスト変数パラメータ
     */
    public static void executePrepared(
            CobolDataStorage sqlca, String stmtName, AbstractCobolField... params) {
        backend().executePrepared(sqlca, stmtName, params);
    }

    // -------------------------------------------------------
    // トランザクション
    // -------------------------------------------------------
    /**
     * デフォルト接続の現在の transaction を commit し、新しい transaction を開始する。
     *
     * @param sqlca ステータス報告用の SQLCA データストレージ
     */
    public static void commit(CobolDataStorage sqlca) {
        backend().commit(sqlca);
    }

    /**
     * デフォルト接続の現在の transaction を rollback し、新しい transaction を開始する。
     *
     * @param sqlca ステータス報告用の SQLCA データストレージ
     */
    public static void rollback(CobolDataStorage sqlca) {
        backend().rollback(sqlca);
    }
}
