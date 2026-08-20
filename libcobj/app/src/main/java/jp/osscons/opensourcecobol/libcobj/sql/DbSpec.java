package jp.osscons.opensourcecobol.libcobj.sql;

/**
 * {@link AbstractCobolEsqlBackend#buildJdbcUrl(DbSpec)} の入力。接続文字列 {@code dbname@host:port} の
 * パース・環境変数フォールバック・末尾空白除去まで適用済みの、DB 非依存な接続情報。
 *
 * <p>生成は {@link AbstractCobolEsqlBackend} の接続共通フロー（buildSpec）だけが行う
 * （コンストラクタは package-private）。バックエンド実装は getter で読み取るのみ。
 */
public final class DbSpec {

    /** 各フィールドを {@link AbstractCobolEsqlBackend} の buildSpec が設定する空の接続情報を生成する。 */
    DbSpec() {}

    /** ホスト名（既定 "localhost"）。 */
    String host;

    /** ポート（":port" 形式、未指定なら空文字）。 */
    String port;

    /** データベース名。 */
    String dbname;

    /** 接続ユーザー名。 */
    String user;

    /** 接続パスワード。 */
    String passwd;

    /** 文字コード（OCDB_DB_CHAR、既定 UTF-8）。 */
    String charset;

    /**
     * ホスト名を返す。
     *
     * @return ホスト名（既定 "localhost"）
     */
    public String getHost() {
        return host;
    }

    /**
     * ポートを返す。
     *
     * @return ":port" 形式のポート。未指定なら空文字
     */
    public String getPort() {
        return port;
    }

    /**
     * データベース名を返す。
     *
     * @return データベース名
     */
    public String getDbname() {
        return dbname;
    }

    /**
     * 接続ユーザー名を返す。
     *
     * @return 接続ユーザー名
     */
    public String getUser() {
        return user;
    }

    /**
     * 接続パスワードを返す。
     *
     * @return 接続パスワード
     */
    public String getPasswd() {
        return passwd;
    }

    /**
     * 文字コードを返す。
     *
     * @return 文字コード（OCDB_DB_CHAR、既定 UTF-8）
     */
    public String getCharset() {
        return charset;
    }
}
