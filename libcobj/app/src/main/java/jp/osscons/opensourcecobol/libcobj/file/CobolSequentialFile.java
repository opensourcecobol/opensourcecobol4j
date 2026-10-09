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

import java.io.IOException;
import java.nio.ByteBuffer;
import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolStopRunException;

/** 順編成ファイル(SEQUENTIAL)のI/Oを実装するCobolFileのサブクラス. */
public class CobolSequentialFile extends CobolFile {

    /**
     * 順編成ファイルを表すインスタンスを生成する.
     *
     * @param selectName SELECT句で指定されたファイルの内部名.
     * @param fileStatus FILE STATUS句に対応するファイル状態コードを格納するバイト配列.
     * @param assign ASSIGN句で指定された割り当て先(物理ファイル名等)を保持する変数.
     * @param record レコード領域を表す変数.
     * @param recordSize 実際のレコード長を格納する変数.
     * @param recordMin 最小レコード長.
     * @param recordMax 最大レコード長.
     * @param nkeys キーの数.
     * @param keys ファイルのキー情報の配列.
     * @param organization ファイル編成を表す値.
     * @param accessMode アクセスモード(順/動的/乱).
     * @param lockMode ファイルのロック方式.
     * @param openMode 現在のオープンモード.
     * @param flagOptional SELECT句にOPTIONALが指定されているかどうかを表すフラグ.
     * @param lastOpenMode 最後に開いたときのオープンモード.
     * @param special 特殊ファイル(標準入出力等)の種別.
     * @param flagNonexistent ファイルが存在しないかどうかを表すフラグ.
     * @param flagEndOfFile ファイル終端に達したかどうかを表すフラグ.
     * @param flagBeginOfFile ファイル先頭に達したかどうかを表すフラグ.
     * @param flagFirstRead 最初の読み込みかどうかを表すフラグ.
     * @param flagReadDone 読み込みが行われたかどうかを表すフラグ.
     * @param flagSelectFeatures SELECT句で指定された機能(FILE STATUS/EXTERNAL/LINAGE/SPLIT KEY)の有無を表すビットフラグ.
     * @param flagNeedsNl 次の書き込み前に改行の出力が必要かどうかを表すフラグ.
     * @param flagNeedsTop 次の書き込み前にページ先頭の処理が必要かどうかを表すフラグ.
     * @param fileVersion ファイル構造のバージョン.
     */
    public CobolSequentialFile(
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
        super(
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

    @Override
    public int start_(int cond, AbstractCobolField key) {
        System.out.println("Seq.start");
        return 0;
    }

    @Override
    public int read_(AbstractCobolField key, int readOpts) {
        System.out.println("Seq.read");
        return 0;
    }

    @Override
    public int readNext(int readOpts) {
        byte[] sbuff = new byte[4];
        this.file.seekInit();

        if (this.record_min != this.record_max) {
            if (this.file.read(sbuff, 4) != 1) {
                if (this.file.isAtEnd()) {
                    return COB_STATUS_10_END_OF_FILE;
                } else {
                    return COB_STATUS_30_PERMANENT_ERROR;
                }
            }
            int size = ByteBuffer.wrap(sbuff).getInt();
            size = Math.max(size, this.record_min);
            size = Math.min(size, this.record_max);
            this.record.setSize(size);
        }

        try {
            int bytesread = this.file.read(this.record.getDataStorage(), this.record.getSize());
            if (bytesread != this.record.getSize()) {
                if (bytesread == 0) {
                    return COB_STATUS_10_END_OF_FILE;
                } else {
                    return COB_STATUS_04_SUCCESS_INCOMPLETE;
                }
            }
        } catch (IOException e) {
            return COB_STATUS_30_PERMANENT_ERROR;
        }
        return COB_STATUS_00_SUCCESS;
    }

    @Override
    public int write_(int opt) throws CobolStopRunException {
        int ret;
        byte[] sbuff = new byte[4];

        this.file.seekInit();

        /* WRITE AFTER */
        if ((opt & COB_WRITE_AFTER) != 0) {
            ret = file_write_opt(opt);
            if (ret != 0) {
                return ret;
            }
        }

        if (this.record_min != this.record_max) {
            ByteBuffer.wrap(sbuff).putInt(this.record.getSize());
            if (!this.file.write(sbuff, 4)) {
                return COB_STATUS_30_PERMANENT_ERROR;
            }
        }

        /* write the record */
        if (!this.file.write(this.record.getDataStorage(), this.record.getSize())) {
            return COB_STATUS_30_PERMANENT_ERROR;
        }

        /* WRITE BEFORE */
        if ((opt & COB_WRITE_BEFORE) != 0) {
            ret = this.file_write_opt(opt);
            if (ret != 0) {
                return ret;
            }
            this.flag_needs_nl = false;
        }
        return 0;
    }

    @Override
    public int rewrite_(int opt) {
        if (!this.file.seek(-this.record.getSize(), FileIO.SEEK_CUR)) {
            return COB_STATUS_30_PERMANENT_ERROR;
        }
        if (!this.file.write(this.record.getDataStorage(), this.record.getSize())) {
            return COB_STATUS_30_PERMANENT_ERROR;
        }
        return COB_STATUS_00_SUCCESS;
    }

    @Override
    public int delete_() {
        System.out.println("Seq.delete");
        return 0;
    }
}
