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

/** TODO: 準備中 */
public class CobolFileFactory {

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
     * @return TODO: 準備中
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
