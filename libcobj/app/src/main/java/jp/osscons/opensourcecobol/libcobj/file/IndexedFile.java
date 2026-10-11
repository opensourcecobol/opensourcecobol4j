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
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;

/**
 * オープン中の1つのINDEXEDファイルの実行時状態を保持する。
 *
 * <p>インスタンスは{@link CobolIndexedFile#open_(String, int, int)}で生成され、オープンの間{@code
 * CobolFile.filei}に保持される。バックエンドのSQLiteデータベースへのJDBC接続に加え、現在のキー／レコードバッファや、COBOLのI/O文を実行する際に使用するその他の管理情報をまとめて保持する。これはパッケージプライベートな単純なデータ保持クラスであり、実際のロジックは{@link
 * CobolIndexedFile}に存在する。
 */
class IndexedFile {
    /** 直近に使用されたキー（例：{@code START}で使用）を示すキー配列へのインデックス。 */
    int key_index;

    /** 直前の{@code WRITE}で書き込まれた主キー値。順次書き込み時の順序チェックに使用する。 */
    CobolDataStorage last_key;

    /** 最大のキーを格納できる大きさのスクラッチバッファ。オープン時にサイズが決定される。 */
    CobolDataStorage temp_key;

    /** このファイルのバックエンドであるSQLiteデータベースへのJDBC接続。 */
    Connection connection;

    /** 読み書き中の現在のキー値（生のバイト列）。 */
    byte[] key;

    /** 読み書き中の現在のレコード値（生のバイト列）。 */
    byte[] data;

    /** キーごとの読み込み管理用に予約されているが、現在の実装では実際には使用されていない。 */
    byte[][] last_readkey;

    /**
     * キーごとの次の重複番号（{@code dupNo}）。OUTPUTモード（遅延コミット中）のWRITEでは、{@code select
     * max(dupNo)}の代わりにこの配列で採番する。それ以外のモードでは確保されるのみで使用されない。
     */
    int[] last_dupno;

    /**
     * WRITE/REWRITEで使い回す、テーブルごとのINSERT用PreparedStatement。
     * 最初に使うときに生成され、CLOSE時に解放される。
     */
    PreparedStatement[] cachedInsertStatements;

    /** REWRITE用のキーごとの状態として予約されているが、オープン時に確保されるのみで、現在の実装では代わりにローカル配列で重複番号を受け渡しており、実際には使用されていない。 */
    int[] rewrite_sec_key;

    /** 解決済みのSQLiteデータベースファイルのパス。 */
    String filename;

    /** レコードロックの管理用に予約されている。 */
    Object record_lock;

    /** WRITE/REWRITE/DELETE操作中に書き込みカーソルがオープンしている間は{@code true}。 */
    boolean write_cursor_open;

    /** 予約済みのロック識別子。 */
    int lock_id;

    /** 現在未使用。予約済み。 */
    boolean record_locked;

    /** {@link #filename}の文字数。 */
    int filenamelen;
}
