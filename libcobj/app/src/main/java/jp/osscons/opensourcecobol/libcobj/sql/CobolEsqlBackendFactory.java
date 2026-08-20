package jp.osscons.opensourcecobol.libcobj.sql;

import java.util.Locale;
import java.util.ServiceConfigurationError;
import java.util.ServiceLoader;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolRuntimeException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * 環境変数 {@code OCDB_DB_TYPE} を見て {@link CobolEsqlBackendInterface} 実装を 1 つ解決する Factory。
 *
 * <p>実装の発見には {@link java.util.ServiceLoader} を使う。各バックエンド実装は
 * {@code META-INF/services/jp.osscons.opensourcecobol.libcobj.sql.CobolEsqlBackendInterface} に
 * 自身を登録し、{@code OCDB_DB_TYPE} の値（小文字化済み）と {@link CobolEsqlBackendInterface#id()}
 * の一致で選択される。これにより実装はパッケージ・JAR の場所に縛られず、classpath に登録済みの
 * JAR を置くだけで追加できる。未指定（null/空）の場合は後方互換のため PostgreSQL
 * （{@code id() == "postgresql"}）を返す。
 */
final class CobolEsqlBackendFactory {

    private static final Logger LOG = LoggerFactory.getLogger(CobolEsqlBackendFactory.class);

    /** ユーティリティクラスのインスタンス化を防ぐための private コンストラクタ。 */
    private CobolEsqlBackendFactory() {}

    /**
     * 環境変数 {@code OCDB_DB_TYPE} からバックエンドを解決する。
     *
     * @return 解決された DB バックエンド
     * @throws CobolRuntimeException {@code OCDB_DB_TYPE} に対応する実装クラスが無い/生成できない場合
     */
    static CobolEsqlBackendInterface resolve() {
        return resolve(System.getenv("OCDB_DB_TYPE"));
    }

    /**
     * 渡された値からバックエンドを解決する（{@link #resolve()} の実装本体。環境変数を自分で
     * 読まないので、値を直接渡して確かめられる）。未指定（null/空）の場合は後方互換のため
     * PostgreSQL を返す。
     *
     * <p>{@link ServiceLoader} が発見した実装のうち、{@link CobolEsqlBackendInterface#id()} が
     * 小文字化済みの DB 種別文字列と一致する最初のものを返す。
     *
     * @param t DB 種別文字列（{@code OCDB_DB_TYPE} の値）
     * @return 解決された DB バックエンド
     * @throws CobolRuntimeException {@code t} に対応する登録済み実装が無い/生成できない場合
     */
    static CobolEsqlBackendInterface resolve(String t) {
        String type = (t == null || t.isEmpty()) ? "postgresql" : t.toLowerCase(Locale.ROOT);
        try {
            for (CobolEsqlBackendInterface backend :
                    ServiceLoader.load(CobolEsqlBackendInterface.class)) {
                if (type.equals(backend.id())) {
                    return backend;
                }
            }
        } catch (ServiceConfigurationError e) { // 登録ファイル不正・実装の生成失敗
            LOG.error("Failed to load ESQL backend providers (OCDB_DB_TYPE: {})", t, e);
            CobolRuntimeException failure =
                    new CobolRuntimeException(
                            CobolRuntimeException.COBOL_FATAL_ERROR,
                            "Unsupported OCDB_DB_TYPE: " + t);
            failure.initCause(e);
            throw failure;
        }
        LOG.error("Unsupported OCDB_DB_TYPE: {}", t);
        throw new CobolRuntimeException(
                CobolRuntimeException.COBOL_FATAL_ERROR, "Unsupported OCDB_DB_TYPE: " + t);
    }
}
