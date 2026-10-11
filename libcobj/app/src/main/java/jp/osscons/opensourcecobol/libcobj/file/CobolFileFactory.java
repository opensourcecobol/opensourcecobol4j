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

/** ファイル編成に応じた適切なCobolFileのサブクラスを生成するファクトリクラス. */
public final class CobolFileFactory {

    /** ユーティリティクラスのインスタンス化を防ぐための private コンストラクタ。 */
    private CobolFileFactory() {}

    /**
     * ファイル編成(organization)に応じて適切なCobolFileのサブクラスを生成する.
     * 順編成ならCobolSequentialFile,行順編成ならCobolLineSequentialFile,相対編成なら
     * CobolRelativeFile,索引編成ならCobolIndexedFileを生成し,いずれにも該当しない場合は
     * 基底クラスCobolFileを生成する.
     *
     * @param selectName SELECT句で指定されたファイル名
     * @param fileStatus FILE STATUSのファイル状態コードを格納するバイト配列
     * @param assign ASSIGN先(割り当てるファイル名)を表すCOBOLデータ項目
     * @param record レコード領域を表すCOBOLデータ項目
     * @param recordSize 現在のレコード長を表すCOBOLデータ項目
     * @param recordMin レコードの最小長
     * @param recordMax レコードの最大長
     * @param nkeys 索引編成ファイルのキーの個数
     * @param keys 索引編成ファイルのキー情報の配列
     * @param organization ファイル編成(COB_ORG_*)
     * @param accessMode アクセスモード(COB_ACCESS_*)
     * @param lockMode ロック方式(COB_LOCK_*)
     * @param openMode 現在のオープンモード(COB_OPEN_*)
     * @param flagOptional SELECT句のOPTIONAL指定の有無
     * @param lastOpenMode 直近のオープンモード(COB_OPEN_*)
     * @param special 標準入出力などの特殊ファイルの種別
     * @param flagNonexistent ファイルが存在しないことを示すフラグ
     * @param flagEndOfFile ファイル終端に達したことを示すフラグ
     * @param flagBeginOfFile ファイル先頭に達したことを示すフラグ
     * @param flagFirstRead 最初の読み込みかどうかを示すフラグ
     * @param flagReadDone 読み込みが実行済みかどうかを示すフラグ
     * @param flagSelectFeatures SELECT句で指定された機能(COB_SELECT_*)を表すフラグ
     * @param flagNeedsNl 次の書き込み前に改行の出力が必要かどうかを示すフラグ
     * @param flagNeedsTop ページ先頭の処理が必要かどうかを示すフラグ
     * @param fileVersion ファイルのバージョン
     * @return organizationに応じて生成されたCobolFileのサブクラスのインスタンス
     */
    public static CobolFile makeCobolFileInstance(
            String selectName,
            byte[] fileStatus,
            AbstractCobolField assign,
            AbstractCobolField record,
            AbstractCobolField recordSize,
            int recordMin,
            int recordMax,
            int nkeys,
            CobolFileKey[] keys,
            char organization,
            char accessMode,
            char lockMode,
            char openMode,
            boolean flagOptional,
            char lastOpenMode,
            char special,
            boolean flagNonexistent,
            boolean flagEndOfFile,
            boolean flagBeginOfFile,
            char flagFirstRead,
            boolean flagReadDone,
            char flagSelectFeatures,
            boolean flagNeedsNl,
            boolean flagNeedsTop,
            char fileVersion) {
        switch (organization) {
            case CobolFile.COB_ORG_SEQUENTIAL:
                return new CobolSequentialFile(
                        selectName,
                        fileStatus,
                        assign,
                        record,
                        recordSize,
                        recordMin,
                        recordMax,
                        nkeys,
                        keys,
                        organization,
                        accessMode,
                        lockMode,
                        openMode,
                        flagOptional,
                        lastOpenMode,
                        special,
                        flagNonexistent,
                        flagEndOfFile,
                        flagBeginOfFile,
                        flagFirstRead,
                        flagReadDone,
                        flagSelectFeatures,
                        flagNeedsNl,
                        flagNeedsTop,
                        fileVersion);
            case CobolFile.COB_ORG_LINE_SEQUENTIAL:
                return new CobolLineSequentialFile(
                        selectName,
                        fileStatus,
                        assign,
                        record,
                        recordSize,
                        recordMin,
                        recordMax,
                        nkeys,
                        keys,
                        organization,
                        accessMode,
                        lockMode,
                        openMode,
                        flagOptional,
                        lastOpenMode,
                        special,
                        flagNonexistent,
                        flagEndOfFile,
                        flagBeginOfFile,
                        flagFirstRead,
                        flagReadDone,
                        flagSelectFeatures,
                        flagNeedsNl,
                        flagNeedsTop,
                        fileVersion);
            case CobolFile.COB_ORG_RELATIVE:
                return new CobolRelativeFile(
                        selectName,
                        fileStatus,
                        assign,
                        record,
                        recordSize,
                        recordMin,
                        recordMax,
                        nkeys,
                        keys,
                        organization,
                        accessMode,
                        lockMode,
                        openMode,
                        flagOptional,
                        lastOpenMode,
                        special,
                        flagNonexistent,
                        flagEndOfFile,
                        flagBeginOfFile,
                        flagFirstRead,
                        flagReadDone,
                        flagSelectFeatures,
                        flagNeedsNl,
                        flagNeedsTop,
                        fileVersion);
            case CobolFile.COB_ORG_INDEXED:
                return new CobolIndexedFile(
                        selectName,
                        fileStatus,
                        assign,
                        record,
                        recordSize,
                        recordMin,
                        recordMax,
                        nkeys,
                        keys,
                        organization,
                        accessMode,
                        lockMode,
                        openMode,
                        flagOptional,
                        lastOpenMode,
                        special,
                        flagNonexistent,
                        flagEndOfFile,
                        flagBeginOfFile,
                        flagFirstRead,
                        flagReadDone,
                        flagSelectFeatures,
                        flagNeedsNl,
                        flagNeedsTop,
                        fileVersion);
            default:
                return new CobolFile(
                        selectName,
                        fileStatus,
                        assign,
                        record,
                        recordSize,
                        recordMin,
                        recordMax,
                        nkeys,
                        keys,
                        organization,
                        accessMode,
                        lockMode,
                        openMode,
                        flagOptional,
                        lastOpenMode,
                        special,
                        flagNonexistent,
                        flagEndOfFile,
                        flagBeginOfFile,
                        flagFirstRead,
                        flagReadDone,
                        flagSelectFeatures,
                        flagNeedsNl,
                        flagNeedsTop,
                        fileVersion);
        }
    }
}
