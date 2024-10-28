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
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolRuntimeException;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolStopRunException;

/** TODO: 準備中 */
public class CobolLineSequentialFile extends CobolFile {

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
    public CobolLineSequentialFile(
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
        System.out.println("LineSeq.start");
        return 0;
    }

    @Override
    public int read_(AbstractCobolField key, int readOpts) {
        System.out.println("LineSeq.read");
        return 0;
    }

    @Override
    public int readNext(int readOpts) {
        int dataptr = 0;
        int n;
        int i = 0;
        CobolDataStorage data = this.record.getDataStorage();

        for (; ; ) {
            n = this.file.getc();
            if (n == -1) {
                if (i == 0) {
                    return COB_STATUS_10_END_OF_FILE;
                } else {
                    break;
                }
            }
            if (n == 0 && cob_ls_nulls != null) {
                n = this.file.getc();
                if (n == -1) {
                    return COB_STATUS_30_PERMANENT_ERROR;
                }
            } else {
                if (n == '\r') {
                    continue;
                }
                if (n == '\n') {
                    break;
                }
            }
            if (i < this.record.getSize()) {
                data.setByte(dataptr++, (byte) n);
                i++;
            }
        }
        if (i < this.record.getSize()) {
            for (int j = 0; i + j < this.record.getSize(); ++j) {
                data.setByte(i + j, (byte) ' ');
            }
        }
        if (this.record_size != null) {
            this.record_size.setInt(i);
        }
        return COB_STATUS_00_SUCCESS;
    }

    @Override
    public int write_(int opt) throws CobolStopRunException {
        int size;
        int i;

        /* determine the size to be written */
        if (cob_ls_fixed != null) {
            size = this.record.getSize();
        } else {
            CobolDataStorage data = this.record.getDataStorage();
            for (i = this.record.getSize() - 1; i >= 0; i--) {
                if (data.getByte(i) != ' ') {
                    break;
                }
            }
            size = i + 1;
        }

        if ((this.flag_select_features & COB_SELECT_LINAGE) != 0) {
            if (this.flag_needs_top) {
                this.flag_needs_top = false;
                Linage lingptr = this.linorkeyptr;
                for (i = 0; i < lingptr.getLinTop(); i++) {
                    this.file.putc((byte) '\n');
                }
            }
        }

        if ((opt & COB_WRITE_AFTER) != 0) {
            int ret = file_write_opt(opt);
            if (ret != 0) {
                return ret;
            }
            this.flag_needs_nl = true;
        }

        if (size != 0) {
            if (cob_ls_nulls != null) {
                int p = 0;
                CobolDataStorage data = this.record.getDataStorage();
                for (i = 0; i < size; ++i, ++p) {
                    if (data.getByte(p) < ' ') {
                        this.file.putc((byte) 0);
                    }
                    this.file.putc(data.getByte(p));
                }
            } else {
                if (!this.file.write(this.record.getDataStorage(), size)) {
                    return COB_STATUS_30_PERMANENT_ERROR;
                }
            }
        }

        if ((this.flag_select_features & COB_SELECT_LINAGE) != 0) {
            this.file.putc((byte) '\n');
        }

        if ((opt & COB_WRITE_BEFORE) != 0) {
            int ret = file_write_opt(opt);
            if (ret != 0) {
                return ret;
            }
            this.flag_needs_nl = false;
        }

        if (this.flag_needs_nl && (this.flag_select_features & COB_SELECT_LINAGE) == 0) {
            this.file.putc((byte) '\n');
            this.flag_needs_nl = false;
        }

        if (eop_status != 0) {
            eop_status = 0;
            CobolRuntimeException.code = 0x0502;
            return COB_STATUS_52_EOP;
        }
        return COB_STATUS_00_SUCCESS;
    }

    @Override
    public int rewrite_(int opt) {
        System.out.println("LineSeq.rewrite");
        return 0;
    }

    @Override
    public int delete_() {
        System.out.println("LineSeq.delete");
        return 0;
    }
}
