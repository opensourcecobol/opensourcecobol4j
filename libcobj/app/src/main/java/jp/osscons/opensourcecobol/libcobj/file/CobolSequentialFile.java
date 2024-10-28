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

/** TODO: 準備中 */
public class CobolSequentialFile extends CobolFile {

    /**
     * TODO: 準備中
     *
     * @param selectName TODO: 準備中
     * @param fileStatus TODO: 準備中
     * @param assign TODO: 準備中
     * @param record TODO: 準備中
     * @param recordSize TODO: 準備中
     * @param recordMin TODO: 準備中
     * @param recordMax TODO: 準備中
     * @param nkeys TODO: 準備中
     * @param keys TODO: 準備中
     * @param organization TODO: 準備中
     * @param accessMode TODO: 準備中
     * @param lockMode TODO: 準備中
     * @param openMode TODO: 準備中
     * @param flagOptional TODO: 準備中
     * @param lastOpenMode TODO: 準備中
     * @param special TODO: 準備中
     * @param flagNonexistent TODO: 準備中
     * @param flagEndOfFile TODO: 準備中
     * @param flagBeginOfFile TODO: 準備中
     * @param flagFirstRead TODO: 準備中
     * @param flagReadDone TODO: 準備中
     * @param flagSelectFeatures TODO: 準備中
     * @param flagNeedsNl TODO: 準備中
     * @param flagNeedsTop TODO: 準備中
     * @param fileVersion TODO: 準備中
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
