package jp.osscons.opensourcecobol.libcobj.sql;

import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.nio.charset.CharsetEncoder;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;

/**
 * COBOL の埋め込み SQL で使用する SQLCA (SQL Communication Area) 構造体を管理する。
 *
 * <p>{@code ECPG_*} 定数と {@link #setSuccess}・{@link #setMissingIndicator} は、
 * {@link CobolEsqlBackendInterface} 実装（別パッケージで提供されるバックエンドを含む）が利用できる
 * 公開 API。それ以外のメンバはパッケージ内部用である。
 */
public final class SqlCA {

    /** ユーティリティクラスのインスタンス化を防ぐための private コンストラクタ。 */
    private SqlCA() {}

    /** COBOL フィールド (SQLSTATE/SQLERRMC) のバイト表現に用いる文字コード。 */
    private static final Charset SHIFT_JIS = Charset.forName("SHIFT-JIS");

    /** SQLERRMC エラーメッセージフィールドの最大長。 */
    static final int SQLERRMC_LEN = 70;

    // SQLCA 構造体内のオフセット (全体で 133 バイト)
    // OFFSET_SQLCAID (0), OFFSET_SQLCABC (8), OFFSET_SQLERRP (88), OFFSET_SQLWARN (120)
    // は SQLCA の仕様上定義されているが、現在のコードでは使用していない。
    private static final int OFFSET_SQLCODE = 12; // 4 バイト (int)
    private static final int OFFSET_SQLERRML = 16; // 2 バイト (short)
    private static final int OFFSET_SQLERRMC = 18; // 70 バイト
    private static final int OFFSET_SQLERRD = 96; // 24 バイト (int 6 個)
    private static final int OFFSET_SQLSTATE = 128; // 5 バイト

    /** エラーなし。 */
    public static final int ECPG_NO_ERROR = 0;

    /** 行が見つからない (SQLSTATE 02000)。 */
    public static final int ECPG_NOT_FOUND = 100;

    /** メモリ不足。 */
    public static final int ECPG_OUT_OF_MEMORY = -12;

    /** 未対応の機能。 */
    public static final int ECPG_UNSUPPORTED = -200;

    /** ホスト変数の引数が多すぎる。 */
    public static final int ECPG_TOO_MANY_ARGUMENTS = -201;

    /** ホスト変数の引数が少なすぎる。 */
    public static final int ECPG_TOO_FEW_ARGUMENTS = -202;

    /** 一致する行が多すぎる。 */
    public static final int ECPG_TOO_MANY_MATCHES = -203;

    /** データ形式エラー。 */
    public static final int ECPG_DATA_FORMAT_ERROR = -204;

    /** 空のクエリまたは文。 */
    public static final int ECPG_EMPTY = -212;

    /** 指標変数が指定されていない。 */
    public static final int ECPG_MISSING_INDICATOR = -213;

    /** 有効な接続がない。 */
    public static final int ECPG_NO_CONN = -220;

    /** 未接続。 */
    public static final int ECPG_NOT_CONN = -221;

    /** 不正な準備済みステートメント。 */
    public static final int ECPG_INVALID_STMT = -230;

    /** Informix 互換の一意キー違反エラー。 */
    public static final int ECPG_INFORMIX_DUPLICATE_KEY = -239;

    /** 不明なデスクリプタ。 */
    public static final int ECPG_UNKNOWN_DESCRIPTOR = -240;

    /** 不正なデスクリプタインデックス。 */
    public static final int ECPG_INVALID_DESCRIPTOR_INDEX = -241;

    /** 不明なデスクリプタ項目。 */
    public static final int ECPG_UNKNOWN_DESCRIPTOR_ITEM = -242;

    /** 変数が数値型でない。 */
    public static final int ECPG_VAR_NOT_NUMERIC = -243;

    /** 変数が文字型でない。 */
    public static final int ECPG_VAR_NOT_CHAR = -244;

    /** Informix 互換の副問い合わせが 2 行以上を返した。 */
    public static final int ECPG_INFORMIX_SUBSELECT_NOT_ONE = -284;

    /** PostgreSQL バックエンドエラー。 */
    public static final int ECPG_PGSQL = -400;

    /** トランザクションエラー。 */
    public static final int ECPG_TRANS = -401;

    /** 接続エラー。 */
    public static final int ECPG_CONNECT = -402;

    /** 一意キー違反。 */
    public static final int ECPG_DUPLICATE_KEY = -403;

    /** 副問い合わせが 2 行以上を返した。 */
    public static final int ECPG_SUBSELECT_NOT_ONE = -404;

    /** 不明なカーソル (ポータル)。 */
    public static final int ECPG_WARNING_UNKNOWN_PORTAL = -602;

    /** 既にトランザクション中。 */
    public static final int ECPG_WARNING_IN_TRANSACTION = -603;

    /** 有効なトランザクションがない。 */
    public static final int ECPG_WARNING_NO_TRANSACTION = -604;

    /** カーソル (ポータル) が既に存在する。 */
    public static final int ECPG_WARNING_PORTAL_EXISTS = -605;

    /** ロックエラー。 */
    public static final int ECPG_LOCK_ERROR = -606;

    /** JDD (Java Database Driver) エラー。 */
    public static final int ECPG_JDD_ERROR = -607;

    /** 認識できないエラー。 */
    public static final int ECPG_UNKNOWN_ERROR = -9999;

    /**
     * SQLCA 構造体の SQLCODE フィールドを設定する。
     *
     * @param sqlca SQLCA のデータストレージ
     * @param code 設定する SQLCODE 値
     */
    static void setCode(CobolDataStorage sqlca, int code) {
        if (sqlca == null) {
            return;
        }
        sqlca.getSubDataStorage(OFFSET_SQLCODE).set(code);
    }

    /**
     * SQLCA 構造体から SQLCODE フィールドを取得する。
     *
     * @param sqlca SQLCA のデータストレージ
     * @return 現在の SQLCODE 値。sqlca が null の場合は 0
     */
    public static int getCode(CobolDataStorage sqlca) {
        if (sqlca == null) {
            return 0;
        }
        return ByteBuffer.wrap(
                        sqlca.getByteArrayRef(OFFSET_SQLCODE, 4),
                        sqlca.getIndex() + OFFSET_SQLCODE,
                        4)
                .getInt();
    }

    /**
     * SQLCA 構造体の 5 文字の SQLSTATE フィールドを設定する。
     *
     * @param sqlca SQLCA のデータストレージ
     * @param state SQLSTATE 文字列 (例: "00000")
     */
    static void setState(CobolDataStorage sqlca, String state) {
        if (sqlca == null || state == null) {
            return;
        }
        byte[] stateBytes = state.getBytes(SHIFT_JIS);
        for (int i = 0; i < 5; i++) {
            if (i < stateBytes.length) {
                sqlca.setByte(OFFSET_SQLSTATE + i, stateBytes[i]);
            } else {
                sqlca.setByte(OFFSET_SQLSTATE + i, (byte) ' ');
            }
        }
    }

    /**
     * SQLCA 構造体の SQLERRMC エラーメッセージフィールドを設定する。
     *
     * @param sqlca SQLCA のデータストレージ
     * @param message エラーメッセージ ({@link #SQLERRMC_LEN} バイトに切り詰められる)
     */
    static void setErrmc(CobolDataStorage sqlca, String message) {
        if (sqlca == null) {
            return;
        }
        if (message == null) {
            clearErrmc(sqlca);
            return;
        }
        byte[] msgBytes = encodeWithinBytes(message, SQLERRMC_LEN);
        int len = msgBytes.length;
        // SQLERRML を設定する
        sqlca.getSubDataStorage(OFFSET_SQLERRML).set((short) len);
        // SQLERRMC を設定する
        for (int i = 0; i < SQLERRMC_LEN; i++) {
            if (i < len) {
                sqlca.setByte(OFFSET_SQLERRMC + i, msgBytes[i]);
            } else {
                sqlca.setByte(OFFSET_SQLERRMC + i, (byte) 0);
            }
        }
    }

    /**
     * 文字列を SHIFT-JIS でエンコードし、maxBytes バイト以内に収める。上限を超える場合は
     * 多バイト文字を途中で分割しないよう、文字境界で切り詰める。
     *
     * @param s エンコードする文字列
     * @param maxBytes 許容する最大バイト数
     * @return SHIFT-JIS バイト列 (maxBytes 以下、末尾の不完全な多バイト文字なし)
     */
    private static byte[] encodeWithinBytes(String s, int maxBytes) {
        byte[] full = s.getBytes(SHIFT_JIS);
        if (full.length <= maxBytes) {
            return full;
        }
        // 文字単位でエンコードし、出力バッファが一杯になった時点 (OVERFLOW) で停止することで
        // 多バイト文字の途中切れを防ぐ。
        CharsetEncoder encoder = SHIFT_JIS.newEncoder();
        ByteBuffer out = ByteBuffer.allocate(maxBytes);
        encoder.encode(CharBuffer.wrap(s), out, true);
        out.flip();
        byte[] truncated = new byte[out.remaining()];
        out.get(truncated);
        return truncated;
    }

    /**
     * 処理行数を保持する SQLERRD の 0 始まりインデックス。COBOL では {@code SQLERRD(3)} に対応し、
     * 直前の SQL 文が処理した行数 (INSERT/UPDATE/DELETE の影響行数、FETCH した行数など) が入る。
     */
    private static final int SQLERRD_INDEX_ROW_COUNT = 2;

    /**
     * 6 個ある SQLERRD 診断値のうちの 1 つを設定する。
     *
     * @param sqlca SQLCA のデータストレージ
     * @param index SQLERRD のインデックス (0〜5)
     * @param value 設定する整数値
     */
    static void setErrd(CobolDataStorage sqlca, int index, int value) {
        if (sqlca == null || index < 0 || index >= 6) {
            return;
        }
        sqlca.getSubDataStorage(OFFSET_SQLERRD + index * 4).set(value);
    }

    /**
     * 直前の SQL 文が処理した行数を SQLERRD(3) に設定する。
     *
     * @param sqlca SQLCA のデータストレージ
     * @param rowCount 処理した行数
     */
    static void setRowCount(CobolDataStorage sqlca, int rowCount) {
        setErrd(sqlca, SQLERRD_INDEX_ROW_COUNT, rowCount);
    }

    /**
     * SQLCA 構造体の SQLERRMC および SQLERRML フィールドをクリアする。
     *
     * @param sqlca SQLCA のデータストレージ
     */
    static void clearErrmc(CobolDataStorage sqlca) {
        if (sqlca == null) {
            return;
        }
        sqlca.getSubDataStorage(OFFSET_SQLERRML).set((short) 0);
        for (int i = 0; i < SQLERRMC_LEN; i++) {
            sqlca.setByte(OFFSET_SQLERRMC + i, (byte) 0);
        }
    }

    /**
     * SQLCA を正常終了に設定するが、SQLERRMC のバイト列は変更しない (SQLERRML は 0 にする)。
     *
     * <p>CONNECT 成功時に使う。Open-COBOL-ESQL-4J は CONNECT 成功時に SQLERRMC を
     * 上書きせず、COBOL が初期化した値 (PIC X のスペース) をそのまま残すため、それに合わせる。
     * メッセージ長 SQLERRML は 0 にして「メッセージなし」と整合させる (直前のエラーの長さが
     * 残らないようにする)。
     *
     * @param sqlca SQLCA のデータストレージ
     */
    static void setSuccessKeepErrmc(CobolDataStorage sqlca) {
        if (sqlca == null) {
            return;
        }
        setCode(sqlca, ECPG_NO_ERROR);
        setState(sqlca, "00000");
        sqlca.getSubDataStorage(OFFSET_SQLERRML).set((short) 0);
    }

    /**
     * SQLCA を正常終了を示す状態に設定する (SQLCODE=0, SQLSTATE="00000")。
     *
     * @param sqlca SQLCA のデータストレージ
     */
    public static void setSuccess(CobolDataStorage sqlca) {
        if (sqlca == null) {
            return;
        }
        setCode(sqlca, ECPG_NO_ERROR);
        setState(sqlca, "00000");
        clearErrmc(sqlca);
    }

    /**
     * SQLCA を「該当行なし」を示す状態に設定する (SQLCODE=ECPG_NOT_FOUND, SQLSTATE="02000")。
     * SELECT INTO やカーソル FETCH で結果が得られなかった場合に使う。
     *
     * @param sqlca SQLCA のデータストレージ
     */
    static void setNotFound(CobolDataStorage sqlca) {
        if (sqlca == null) {
            return;
        }
        setCode(sqlca, ECPG_NOT_FOUND);
        setState(sqlca, "02000");
        clearErrmc(sqlca);
    }

    /**
     * SQLCA を ECPG_MISSING_INDICATOR に設定する (sqlcode=-213, sqlstate="22002")。
     *
     * <p>「指標変数なしの NULL 値」を通知する。これは、フェッチした列が SQL NULL であり、
     * かつホスト変数に対応する指標変数がない場合の ECPG 標準の動作である。COBOL フィールド
     * 自体には依然として値が書き込まれる (通常はゼロ埋め) ため行は処理済みとみなされる。
     * アプリケーション側のコードは FETCH/SELECT の後に SQLCODE/SQLSTATE を確認することが想定されている。
     *
     * @param sqlca SQLCA のデータストレージ
     */
    public static void setMissingIndicator(CobolDataStorage sqlca) {
        setError(sqlca, ECPG_MISSING_INDICATOR, "22002", "Null value without indicator");
    }

    /**
     * SQLCA を、指定したコード・状態・メッセージのエラーを示す状態に設定する。
     *
     * @param sqlca SQLCA のデータストレージ
     * @param code SQLCODE のエラーコード
     * @param state 5 文字の SQLSTATE
     * @param message エラーメッセージ
     */
    static void setError(CobolDataStorage sqlca, int code, String state, String message) {
        if (sqlca == null) {
            return;
        }
        setCode(sqlca, code);
        setState(sqlca, state);
        setErrmc(sqlca, message);
    }
}
