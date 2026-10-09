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

import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;

/** SORT/MERGE の実行状態(入出力ファイル,比較キー,マージ用のキューと一時ファイル等)を保持するクラス. */
class CobolSort {
    private CobolFile pointer;
    private CobolItem empty;
    private CobolDataStorage sortReturn;
    private AbstractCobolField fnstatus;
    private int unique;
    private int retrieving;
    private int filesUsed;
    private int size;
    private int rSize;
    private int wSize;
    private int memory;
    private int destinationFile;
    private int retrievalQueue;
    private MemoryStruct[] queue = new MemoryStruct[4];
    private FileStruct[] file = new FileStruct[4];

    /** マージ用の 4 本のキュー(MemoryStruct)と 4 本の一時ファイル(FileStruct)を生成して初期化する. */
    CobolSort() {
        for (int i = 0; i < 4; ++i) {
            this.queue[i] = new MemoryStruct();
            this.file[i] = new FileStruct();
        }
    }

    /**
     * SORT/MERGE の対象となるCobolFileを取得する.
     *
     * @return SORT/MERGE の対象となるCobolFile
     */
    CobolFile getPointer() {
        return pointer;
    }

    /**
     * SORT/MERGE の対象となるCobolFileを設定する.
     *
     * @param pointer SORT/MERGE の対象となるCobolFile
     */
    void setPointer(CobolFile pointer) {
        this.pointer = pointer;
    }

    /**
     * 再利用可能な空きCobolItemを繋いだフリーリストの先頭を取得する.
     *
     * @return 空きCobolItemのフリーリストの先頭
     */
    CobolItem getEmpty() {
        return empty;
    }

    /**
     * 再利用可能な空きCobolItemを繋いだフリーリストの先頭を設定する.
     *
     * @param empty 空きCobolItemのフリーリストの先頭
     */
    void setEmpty(CobolItem empty) {
        this.empty = empty;
    }

    /**
     * SORT/MERGE の結果コード(SORT-RETURN 特殊レジスタ相当. エラー時に16が設定される)を保持する領域を取得する.
     *
     * @return SORT-RETURN 特殊レジスタ相当の領域
     */
    CobolDataStorage getSortReturn() {
        return sortReturn;
    }

    /**
     * SORT/MERGE の結果コード(SORT-RETURN 特殊レジスタ相当. エラー時に16が設定される)を保持する領域を設定する.
     *
     * @param sortReturn SORT-RETURN 特殊レジスタ相当の領域
     */
    void setSortReturn(CobolDataStorage sortReturn) {
        this.sortReturn = sortReturn;
    }

    /**
     * ファイル状態(FILE STATUS)を格納する項目を取得する.
     *
     * @return FILE STATUS を格納する項目
     */
    AbstractCobolField getFnstatus() {
        return fnstatus;
    }

    /**
     * ファイル状態(FILE STATUS)を格納する項目を設定する.
     *
     * @param fnstatus FILE STATUS を格納する項目
     */
    void setFnstatus(AbstractCobolField fnstatus) {
        this.fnstatus = fnstatus;
    }

    /**
     * 次に投入するレコードへ割り当てる一意番号(レコード投入ごとに増加し,安定ソートに用いる)を取得する.
     *
     * @return 次に割り当てる一意番号
     */
    int getUnique() {
        return unique;
    }

    /**
     * 次に投入するレコードへ割り当てる一意番号(レコード投入ごとに増加し,安定ソートに用いる)を設定する.
     *
     * @param unique 次に割り当てる一意番号
     */
    void setUnique(int unique) {
        this.unique = unique;
    }

    /**
     * ソートが完了し取り出し(RETURN)フェーズに入っているかを表すフラグを取得する.
     *
     * @return 取り出しフェーズに入っていれば0以外,そうでなければ0
     */
    int getRetrieving() {
        return retrieving;
    }

    /**
     * ソートが完了し取り出し(RETURN)フェーズに入っているかを表すフラグを設定する.
     *
     * @param retrieving 取り出しフェーズに入っていれば0以外,そうでなければ0
     */
    void setRetrieving(int retrieving) {
        this.retrieving = retrieving;
    }

    /**
     * メモリに収まらず一時ファイルを用いた外部マージソートを行っているかを表すフラグを取得する.
     *
     * @return 一時ファイルを使用していれば0以外,そうでなければ0
     */
    int getFilesUsed() {
        return filesUsed;
    }

    /**
     * メモリに収まらず一時ファイルを用いた外部マージソートを行っているかを表すフラグを設定する.
     *
     * @param filesUsed 一時ファイルを使用していれば0以外,そうでなければ0
     */
    void setFilesUsed(int filesUsed) {
        this.filesUsed = filesUsed;
    }

    /**
     * ソート対象レコード 1 件のサイズ(バイト数)を取得する.
     *
     * @return レコード 1 件のサイズ(バイト数)
     */
    int getSize() {
        return size;
    }

    /**
     * ソート対象レコード 1 件のサイズ(バイト数)を設定する.
     *
     * @param size レコード 1 件のサイズ(バイト数)
     */
    void setSize(int size) {
        this.size = size;
    }

    /**
     * 一時ファイルからの読み込み単位のサイズ(レコードサイズ + 一意番号 8 バイト)を取得する.
     *
     * @return 読み込み単位のサイズ(バイト数)
     */
    int getrSize() {
        return rSize;
    }

    /**
     * 一時ファイルからの読み込み単位のサイズ(レコードサイズ + 一意番号 8 バイト)を設定する.
     *
     * @param rSize 読み込み単位のサイズ(バイト数)
     */
    void setrSize(int rSize) {
        this.rSize = rSize;
    }

    /**
     * 一時ファイルへの書き込み単位のサイズ(レコードサイズ + 一意番号 8 バイト + ブロックバイト 1 バイト)を取得する.
     *
     * @return 書き込み単位のサイズ(バイト数)
     */
    int getwSize() {
        return wSize;
    }

    /**
     * 一時ファイルへの書き込み単位のサイズ(レコードサイズ + 一意番号 8 バイト + ブロックバイト 1 バイト)を設定する.
     *
     * @param wSize 書き込み単位のサイズ(バイト数)
     */
    void setwSize(int wSize) {
        this.wSize = wSize;
    }

    /**
     * 一時ファイルへ書き出す前にメモリ上に保持できるレコード数の上限を取得する.
     *
     * @return メモリ上に保持できるレコード数の上限
     */
    int getMemory() {
        return memory;
    }

    /**
     * 一時ファイルへ書き出す前にメモリ上に保持できるレコード数の上限を設定する.
     *
     * @param memory メモリ上に保持できるレコード数の上限
     */
    void setMemory(int memory) {
        this.memory = memory;
    }

    /**
     * ソート済みブロックの書き込み先となる一時ファイルの番号を取得する.
     *
     * @return 書き込み先の一時ファイルの番号
     */
    int getDestinationFile() {
        return destinationFile;
    }

    /**
     * ソート済みブロックの書き込み先となる一時ファイルの番号を設定する.
     *
     * @param destinationFile 書き込み先の一時ファイルの番号
     */
    void setDestinationFile(int destinationFile) {
        this.destinationFile = destinationFile;
    }

    /**
     * 取り出し(RETURN)時にソート済みレコードを読み出す元となるキュー/一時ファイルの番号を取得する.
     *
     * @return 取り出し元のキュー/一時ファイルの番号
     */
    int getRetrievalQueue() {
        return retrievalQueue;
    }

    /**
     * 取り出し(RETURN)時にソート済みレコードを読み出す元となるキュー/一時ファイルの番号を設定する.
     *
     * @param retrievalQueue 取り出し元のキュー/一時ファイルの番号
     */
    void setRetrievalQueue(int retrievalQueue) {
        this.retrievalQueue = retrievalQueue;
    }

    /**
     * マージ処理で用いるメモリ上のキュー(4 本)の配列を取得する.
     *
     * @return メモリ上のキュー(MemoryStruct)の配列
     */
    MemoryStruct[] getQueue() {
        return queue;
    }

    /**
     * マージ処理で用いるメモリ上のキュー(4 本)の配列を設定する.
     *
     * @param queue メモリ上のキュー(MemoryStruct)の配列
     */
    void setQueue(MemoryStruct[] queue) {
        this.queue = queue;
    }

    /**
     * マージ処理で用いる一時ファイル(4 本)の配列を取得する.
     *
     * @return 一時ファイル(FileStruct)の配列
     */
    FileStruct[] getFile() {
        return file;
    }

    /**
     * マージ処理で用いる一時ファイル(4 本)の配列を設定する.
     *
     * @param file 一時ファイル(FileStruct)の配列
     */
    void setFile(FileStruct[] file) {
        this.file = file;
    }
}
