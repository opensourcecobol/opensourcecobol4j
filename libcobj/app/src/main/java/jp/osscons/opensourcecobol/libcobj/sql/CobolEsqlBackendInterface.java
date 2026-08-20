package jp.osscons.opensourcecobol.libcobj.sql;

import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;

/**
 * COBOL の埋め込み SQL 操作（CONNECT、EXEC SQL、カーソル、トランザクション）の DB バックエンドが
 * 公開すべき全操作を定義するインターフェース。
 *
 * <p>{@link CobolEsql} は受け取った呼び出しを、このインターフェースの実装へ渡す。DB ごとの
 * 差し替えは、実装（{@link CobolEsqlBackendPostgresql} など）を切り替えることで行う。
 * 接続・文の実行・カーソル・WHERE CURRENT OF・SELECT INTO・prepared statement・トランザクション・
 * エラー報告まで、必要な操作はすべてこのインターフェースに並べている。
 */
public interface CobolEsqlBackendInterface {

    /**
     * 実装ごとに決まっている ID（小文字）を返す。例: {@code "postgresql"} / {@code "mysql"}。
     *
     * <p>{@link java.util.ServiceLoader} が見つけた実装の中から 1 つを選ぶときに、環境変数
     * {@code OCDB_DB_TYPE} の値（小文字化済み）と突き合わせるキー。ログやデバッグ表示にも使う。
     *
     * @return DB 種別の ID
     */
    String id();

    // --- 接続 ---

    /**
     * user、password、dbname を個別のパラメータとして受け取り、データベース接続を確立する。
     *
     * @param sqlca ステータス報告用の SQLCA データストレージ
     * @param user ユーザー名フィールド
     * @param passwd パスワードフィールド
     * @param dbname データベース名フィールド
     */
    void connect(
            CobolDataStorage sqlca,
            AbstractCobolField user,
            AbstractCobolField passwd,
            AbstractCobolField dbname);

    /**
     * commit を実行したうえで、デフォルトのデータベース接続を切断する。
     *
     * @param sqlca ステータス報告用の SQLCA データストレージ
     */
    void disconnect(CobolDataStorage sqlca);

    // --- 文の実行 ---

    /**
     * ホスト変数パラメータを持たない SQL 文を実行する。
     *
     * @param sqlca ステータス報告用の SQLCA データストレージ
     * @param query SQL クエリ文字列
     */
    void exec(CobolDataStorage sqlca, String query);

    /**
     * COBOL のホスト変数をバインドしたパラメータ付き SQL 文を実行する。
     *
     * @param sqlca ステータス報告用の SQLCA データストレージ
     * @param query '?' プレースホルダを含む SQL クエリ文字列
     * @param params COBOL のホスト変数パラメータ
     */
    void execWithParams(CobolDataStorage sqlca, String query, AbstractCobolField... params);

    // --- WHERE CURRENT OF（位置付き UPDATE/DELETE） ---

    /**
     * WHERE CURRENT OF を伴う UPDATE/DELETE（ホスト変数なし）を実行する。
     *
     * @param sqlca ステータス報告用の SQLCA データストレージ
     * @param query 末尾が {@code WHERE CURRENT OF} の SQL（カーソル名は含まない）
     * @param cursorName 位置付け対象の（修飾済み）カーソル名
     */
    void execWhereCurrentOf(CobolDataStorage sqlca, String query, String cursorName);

    /**
     * WHERE CURRENT OF を伴う UPDATE/DELETE（ホスト変数あり）を実行する。
     *
     * @param sqlca ステータス報告用の SQLCA データストレージ
     * @param query 末尾が {@code WHERE CURRENT OF} の SQL（'?' プレースホルダを含む。カーソル名は含まない）
     * @param cursorName 位置付け対象の（修飾済み）カーソル名
     * @param params COBOL のホスト変数パラメータ
     */
    void execWithParamsWhereCurrentOf(
            CobolDataStorage sqlca, String query, String cursorName, AbstractCobolField... params);

    // --- SELECT INTO ---

    /**
     * SELECT INTO 文を実行し、結果を COBOL のホスト変数に書き戻す。
     *
     * @param sqlca ステータス報告用の SQLCA データストレージ
     * @param query SELECT クエリ文字列
     * @param inputParams 入力のホスト変数パラメータ（WHERE 句のバインド）
     * @param resultParams 選択された列の値を受け取る出力ホスト変数
     */
    void selectInto(
            CobolDataStorage sqlca,
            String query,
            AbstractCobolField[] inputParams,
            AbstractCobolField[] resultParams);

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
    void selectIntoOccurs(
            CobolDataStorage sqlca,
            int occursSize,
            int occursMax,
            String query,
            AbstractCobolField[] inputParams,
            AbstractCobolField[] resultParams);

    // --- カーソル ---

    /**
     * パラメータを持たない SQL カーソルを宣言する。
     *
     * @param sqlca ステータス報告用の SQLCA データストレージ
     * @param cursorName カーソル名
     * @param query カーソル用の SQL クエリ
     */
    void declareCursor(CobolDataStorage sqlca, String cursorName, String query);

    /**
     * ホスト変数パラメータを持つ SQL カーソルを宣言する。
     *
     * @param sqlca ステータス報告用の SQLCA データストレージ
     * @param cursorName カーソル名
     * @param query カーソル用の SQL クエリ
     * @param params カーソルをオープンする際にバインドするホスト変数パラメータ
     */
    void declareCursorWithParams(
            CobolDataStorage sqlca, String cursorName, String query, AbstractCobolField... params);

    /**
     * 事前に宣言されたカーソルをオープンする。
     *
     * @param sqlca ステータス報告用の SQLCA データストレージ
     * @param cursorName オープンするカーソル名
     */
    void openCursor(CobolDataStorage sqlca, String cursorName);

    /**
     * 事前に宣言されたカーソルをホスト変数パラメータ付きでオープンする。
     *
     * @param sqlca ステータス報告用の SQLCA データストレージ
     * @param cursorName オープンするカーソル名
     * @param params カーソルクエリ用のホスト変数パラメータ
     */
    void openCursorWithParams(
            CobolDataStorage sqlca, String cursorName, AbstractCobolField... params);

    /**
     * オープン済みのカーソルから次の行を取得し、COBOL のホスト変数に格納する。
     *
     * @param sqlca ステータス報告用の SQLCA データストレージ
     * @param cursorName fetch 対象のカーソル名
     * @param resultParams 列の値を受け取る出力ホスト変数
     */
    void fetchCursor(CobolDataStorage sqlca, String cursorName, AbstractCobolField... resultParams);

    /**
     * カーソルから複数行を取得し、OCCURS 配列フィールドに格納する。
     *
     * @param sqlca ステータス報告用の SQLCA データストレージ
     * @param cursorName fetch 対象のカーソル名
     * @param occursSize OCCURS 要素 1 つあたりのバイトサイズ
     * @param occursMax 取得する行の最大数
     * @param resultParams 結果パラメータフィールド（列ごとに 1 つ）
     */
    void fetchCursorOccurs(
            CobolDataStorage sqlca,
            String cursorName,
            int occursSize,
            int occursMax,
            AbstractCobolField... resultParams);

    /**
     * オープン済みのカーソルをクローズする。
     *
     * @param sqlca ステータス報告用の SQLCA データストレージ
     * @param cursorName クローズするカーソル名
     */
    void closeCursor(CobolDataStorage sqlca, String cursorName);

    // --- prepared statement ---

    /**
     * SQL 文を prepare し、COBOL のホスト変数参照を '?' プレースホルダに置き換える。
     *
     * @param sqlca ステータス報告用の SQLCA データストレージ
     * @param stmtName prepared statement に割り当てる名前
     * @param queryField SQL クエリ文を保持する COBOL フィールド
     */
    void prepare(CobolDataStorage sqlca, String stmtName, AbstractCobolField queryField);

    /**
     * 事前に prepare された statement を実行する。
     *
     * @param sqlca ステータス報告用の SQLCA データストレージ
     * @param stmtName prepared statement の名前
     * @param params バインドするホスト変数パラメータ
     */
    void executePrepared(CobolDataStorage sqlca, String stmtName, AbstractCobolField... params);

    // --- トランザクション ---

    /**
     * デフォルト接続の現在の transaction を commit し、新しい transaction を開始する。
     *
     * @param sqlca ステータス報告用の SQLCA データストレージ
     */
    void commit(CobolDataStorage sqlca);

    /**
     * デフォルト接続の現在の transaction を rollback し、新しい transaction を開始する。
     *
     * @param sqlca ステータス報告用の SQLCA データストレージ
     */
    void rollback(CobolDataStorage sqlca);
}
