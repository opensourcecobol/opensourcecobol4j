package jp.osscons.opensourcecobol.libcobj.sql;

/**
 * カーソルの先読み（バルクフェッチ）件数の設定。
 *
 * <p>環境変数 {@code OCESQL4J_FETCH_RECORDS} から取得する。既定値は 1（＝1 行ずつ取得）で、
 * 0 以下や数値でない値は 1 として扱う。Open COBOL ESQL 4J の {@code GlobalState.fetch_records}
 * と同じ規則。プロセス起動時に一度だけ環境変数を読み取り、以降はキャッシュ値を返す。
 */
final class BulkFetchConfig {

    /** ユーティリティクラスのインスタンス化を防ぐための private コンストラクタ。 */
    private BulkFetchConfig() {}

    /** 先読み件数を指定する環境変数名。 */
    static final String FETCH_RECORDS_ENV = "OCESQL4J_FETCH_RECORDS";

    @SuppressWarnings("PMD.AvoidUsingVolatile")
    private static volatile int fetchRecords = readFromEnv();

    private static int readFromEnv() {
        String value = System.getenv(FETCH_RECORDS_ENV);
        if (value != null) {
            try {
                int n = Integer.parseInt(value.trim());
                if (n > 0) {
                    return n;
                }
            } catch (NumberFormatException ignored) {
                // 数値でない値は既定値 1 として扱う
            }
        }
        return 1;
    }

    /**
     * 1 回の {@code FETCH FORWARD} で先読みする行数を返す。
     *
     * @return 先読み件数（1 以上）
     */
    static int getFetchRecords() {
        return fetchRecords;
    }

    /**
     * 先読み件数を上書きする（主にユニットテスト用）。正の値のみ採用する。
     *
     * @param n 先読み件数
     */
    static void setFetchRecords(int n) {
        if (n > 0) {
            fetchRecords = n;
        }
    }
}
