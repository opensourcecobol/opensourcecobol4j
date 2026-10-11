/*
 * Copyright (C) 2021-2022 TOKYO SYSTEM HOUSE Co., Ltd.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 3.0,
 * or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; see the file COPYING.LIB.  If
 * not, write to the Free Software Foundation, 51 Franklin Street, Fifth Floor
 * Boston, MA 02110-1301 USA
 */
package jp.osscons.opensourcecobol.libcobj.file;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;

/**
 * 1つのINDEXEDファイル（＝1つのSQLite接続）で発行するSQL文の{@link PreparedStatement}を、
 * SQL文字列をキーとしてキャッシュする。
 *
 * <p>INDEXEDファイルのテーブル構成（テーブル数・スキーマ）はオープン成功時点で確定し、
 * オープン中に変わることはない。またSQL文はすべて定数テンプレートとテーブル名から
 * 組み立てられ、値は必ずプレースホルダでバインドされるため、SQL文字列の種類は有界で、
 * 同じ文字列は常に同じ意味を持つ。したがって文ごとに毎回SQLを解析し直す代わりに、
 * 接続ごとに1つのPreparedStatementを生成して使い回すことができる。
 *
 * <p>PreparedStatementは接続内で完結するため、ファイルを他プロセスと共有していても、
 * コミット・ロールバック・セーブポイントをまたいでも安全に再利用できる。
 *
 * <p>使用上の規約:
 *
 * <ul>
 *   <li>{@link #get(String)}が返すPreparedStatementを呼び出し側でcloseしてはならない。
 *       解放は{@link #closeAll()}が一括して行い、その後に接続をcloseする。
 *   <li>パラメータは実行のたびにすべてバインドし直すこと。
 *   <li>{@code executeQuery}が返す{@link java.sql.ResultSet}は、従来どおり呼び出し側が
 *       try-with-resourcesでcloseすること。
 * </ul>
 */
final class IndexedStatementCache {
    /** キャッシュしたPreparedStatementの生成元となるJDBC接続。 */
    private final Connection connection;

    /** SQL文字列からPreparedStatementへのキャッシュ。 */
    private final Map<String, PreparedStatement> cache = new HashMap<>();

    /**
     * 指定した接続に対するキャッシュを構築する。
     *
     * @param connection PreparedStatementの生成に使用するJDBC接続
     */
    IndexedStatementCache(Connection connection) {
        this.connection = connection;
    }

    /**
     * 指定したSQL文に対応するPreparedStatementを返す。初回はprepareしてキャッシュし、
     * 2回目以降はキャッシュ済みのインスタンスをそのまま返す。
     *
     * @param sql 発行するSQL文
     * @return キャッシュされたPreparedStatement。呼び出し側でcloseしてはならない
     * @throws SQLException SQL文の解析に失敗した場合
     */
    PreparedStatement get(String sql) throws SQLException {
        PreparedStatement statement = this.cache.get(sql);
        if (statement == null) {
            statement = this.connection.prepareStatement(sql);
            this.cache.put(sql, statement);
        }
        return statement;
    }

    /**
     * キャッシュしているすべてのPreparedStatementをcloseし、キャッシュを空にする。
     * 接続をcloseする直前に呼ぶこと。
     */
    void closeAll() {
        for (PreparedStatement statement : this.cache.values()) {
            try {
                statement.close();
            } catch (SQLException e) {
                System.err.println("Failed to close a prepared statement");
            }
        }
        this.cache.clear();
    }
}
