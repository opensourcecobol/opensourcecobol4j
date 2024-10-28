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
package jp.osscons.opensourcecobol.libcobj.common;

import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldAttribute;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldFactory;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolExceptionId;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolExceptionInfo;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolRuntimeException;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolStopRunException;

class Dlm {
    public AbstractCobolField dlm;
    public int all;
}

/** TODO: 準備中 */
public class CobolString {
    private static final int DLM_DEFAULT_NUM = 8;

    private static int udlmCount = 0;

    private static AbstractCobolField stringDst;
    private static AbstractCobolField stringPtr;
    private static AbstractCobolField stringDlm;
    private static AbstractCobolField stringDstCopy;
    private static AbstractCobolField stringPtrCopy;
    private static AbstractCobolField stringDlmCopy;
    private static int stringOffset;

    private static Dlm[] dlmList;
    private static AbstractCobolField unstringSrc;
    private static AbstractCobolField unstringPtr;
    private static AbstractCobolField unstringSrcCopy;
    private static AbstractCobolField unstringPtrCopy;
    private static int unstringOffset;
    private static int unstringCount;
    private static int unstringNdlms;

    /**
     * TODO: 準備中
     *
     * @param dst TODO: 準備中
     * @param ptr TODO: 準備中
     */
    public static void stringInit(int dst, AbstractCobolField ptr) {
        stringInit(null, ptr);
    }

    /**
     * TODO: 準備中
     *
     * @param dst TODO: 準備中
     * @param ptr TODO: 準備中
     */
    public static void stringInit(AbstractCobolField dst, int ptr) {
        stringInit(dst, null);
    }

    /**
     * TODO: 準備中
     *
     * @param dst TODO: 準備中
     * @param ptr TODO: 準備中
     */
    public static void stringInit(int dst, int ptr) {
        stringInit(null, null);
    }

    /**
     * TODO: 準備中
     *
     * @param dst TODO: 準備中
     * @param ptr TODO: 準備中
     */
    public static void stringInit(AbstractCobolField dst, AbstractCobolField ptr) {
        stringDstCopy = dst;
        stringDst = stringDstCopy;
        stringPtr = null;
        if (ptr != null) {
            stringPtrCopy = ptr;
            stringPtr = stringPtrCopy;
        }
        stringOffset = 0;
        CobolRuntimeException.setException(0);

        if (stringPtr != null) {
            stringOffset = stringPtr.getInt() - 1;
            if (stringOffset < 0 || stringOffset >= stringDst.getSize()) {
                CobolRuntimeException.setException(CobolExceptionId.COB_EC_OVERFLOW_STRING);
            }
        }

        switch (stringDst.getAttribute().getType()) {
            case CobolFieldAttribute.COB_TYPE_NATIONAL:
            case CobolFieldAttribute.COB_TYPE_NATIONAL_EDITED:
                stringOffset *= 2;
                break;
            default:
                break;
        }
    }

    /**
     * TODO: 準備中
     *
     * @param dlm TODO: 準備中
     */
    public static void stringDelimited(int dlm) {
        stringDelimited(null);
    }

    /**
     * TODO: 準備中
     *
     * @param dlm TODO: 準備中
     */
    public static void stringDelimited(AbstractCobolField dlm) {
        switch (stringDst.getAttribute().getType()) {
            case CobolFieldAttribute.COB_TYPE_NATIONAL:
            case CobolFieldAttribute.COB_TYPE_NATIONAL_EDITED:
                if (dlm == CobolConstant.quote) {
                    dlm = CobolConstant.zenQuote;
                } else if (dlm == CobolConstant.space) {
                    dlm = CobolConstant.zenSpace;
                } else if (dlm == CobolConstant.zero) {
                    dlm = CobolConstant.zenZero;
                }
                break;
            default:
                break;
        }
        stringDlm = null;
        if (dlm != null) {
            stringDlmCopy = dlm;
            stringDlm = stringDlmCopy;
        }
    }

    /**
     * TODO: 準備中
     *
     * @param src TODO: 準備中
     */
    public static void stringAppend(int src) {
        stringAppend(null);
    }

    /**
     * TODO: 準備中
     *
     * @param src TODO: 準備中
     */
    public static void stringAppend(AbstractCobolField src) {
        if (CobolRuntimeException.code != 0) {
            return;
        }

        int srcSize = src.getSize();
        if (stringDlm != null) {
            int size = srcSize - stringDlm.getSize() + 1;
            CobolDataStorage srcData = src.getDataStorage();
            CobolDataStorage dlmData = stringDlm.getDataStorage();
            int dlmSize = stringDlm.getSize();
            for (int i = 0; i < size; ++i) {
                if (srcData.getSubDataStorage(i).memcmp(dlmData, dlmSize) == 0) {
                    srcSize = i;
                    break;
                }
            }
        }

        if (srcSize <= stringDst.getSize() - stringOffset) {
            stringDst
                    .getDataStorage()
                    .getSubDataStorage(stringOffset)
                    .memcpy(src.getDataStorage(), srcSize);
            stringOffset += srcSize;
        } else {
            int size = stringDst.getSize() - stringOffset;
            stringDst
                    .getDataStorage()
                    .getSubDataStorage(stringOffset)
                    .memcpy(src.getDataStorage(), size);
            stringOffset += size;
            CobolRuntimeException.setException(CobolExceptionId.COB_EC_OVERFLOW_STRING);
        }
    }

    /** TODO: 準備中 */
    public static void stringFinish() {
        int type = stringDst.getAttribute().getType();
        if (type == CobolFieldAttribute.COB_TYPE_NATIONAL
                || type == CobolFieldAttribute.COB_TYPE_NATIONAL_EDITED) {

            stringOffset /= 2;
        }
        if (stringPtr != null) {
            stringPtr.setInt(stringOffset + 1);
        }
    }

    /**
     * TODO: 準備中
     *
     * @param src TODO: 準備中
     * @param ptr TODO: 準備中
     * @param numDlm TODO: 準備中
     */
    public static void unstringInit(int src, int ptr, int numDlm) {
        unstringInit(null, null, numDlm);
    }

    /**
     * TODO: 準備中
     *
     * @param src TODO: 準備中
     * @param ptr TODO: 準備中
     * @param numDlm TODO: 準備中
     */
    public static void unstringInit(AbstractCobolField src, int ptr, int numDlm) {
        unstringInit(src, null, numDlm);
    }

    /**
     * TODO: 準備中
     *
     * @param src TODO: 準備中
     * @param ptr TODO: 準備中
     * @param numDlm TODO: 準備中
     */
    public static void unstringInit(int src, AbstractCobolField ptr, int numDlm) {
        unstringInit(null, ptr, numDlm);
    }

    /**
     * TODO: 準備中
     *
     * @param src TODO: 準備中
     * @param ptr TODO: 準備中
     * @param numDlm TODO: 準備中
     */
    public static void unstringInit(AbstractCobolField src, AbstractCobolField ptr, int numDlm) {
        unstringSrcCopy = src;
        unstringSrc = unstringSrcCopy;
        unstringPtr = null;
        if (ptr != null) {
            unstringPtrCopy = ptr;
            unstringPtr = unstringPtrCopy;
        }

        unstringOffset = 0;
        unstringCount = 0;
        unstringNdlms = 0;
        CobolRuntimeException.setException(0);

        if (dlmList == null) {
            if (numDlm <= DLM_DEFAULT_NUM) {
                dlmList = new Dlm[DLM_DEFAULT_NUM];
                udlmCount = DLM_DEFAULT_NUM;
            } else {
                dlmList = new Dlm[numDlm];
                udlmCount = numDlm;
            }
        } else {
            if (numDlm > udlmCount) {
                dlmList = new Dlm[numDlm];
                udlmCount = numDlm;
            }
        }

        for (int i = 0; i < dlmList.length; ++i) {
            dlmList[i] = new Dlm();
        }

        if (unstringPtr != null) {
            unstringOffset = unstringPtr.getInt() - 1;
            if (unstringOffset < 0 || unstringOffset >= unstringSrc.getSize()) {
                CobolRuntimeException.setException(CobolExceptionId.COB_EC_OVERFLOW_UNSTRING);
            }
        }

        switch (unstringSrc.getAttribute().getType()) {
            case CobolFieldAttribute.COB_TYPE_NATIONAL:
            case CobolFieldAttribute.COB_TYPE_NATIONAL_EDITED:
                unstringOffset *= 2;
                break;
            default:
                break;
        }
    }

    /**
     * TODO: 準備中
     *
     * @param dlm TODO: 準備中
     * @param all TODO: 準備中
     */
    public static void unstringDelimited(int dlm, int all) {
        unstringDelimited(null, all);
    }

    /**
     * TODO: 準備中
     *
     * @param dlm TODO: 準備中
     * @param all TODO: 準備中
     */
    public static void unstringDelimited(AbstractCobolField dlm, int all) {
        AbstractCobolField addDlm = null;

        switch (unstringSrc.getAttribute().getType()) {
            case CobolFieldAttribute.COB_TYPE_NATIONAL:
            case CobolFieldAttribute.COB_TYPE_NATIONAL_EDITED:
                if (dlm == CobolConstant.quote) {
                    dlm = CobolConstant.zenQuote;
                } else if (dlm == CobolConstant.space) {
                    dlm = CobolConstant.zenSpace;
                    addDlm = CobolConstant.zenBlank;
                } else if (dlm == CobolConstant.zero) {
                    dlm = CobolConstant.zenZero;
                }
                break;
            default:
                break;
        }

        dlmList[unstringNdlms].dlm = dlm;
        dlmList[unstringNdlms].all = all;
        unstringNdlms++;

        if (addDlm != null) {
            dlmList[unstringNdlms].dlm = addDlm;
            dlmList[unstringNdlms].all = all;
            unstringNdlms++;
        }
    }

    /**
     * TODO: 準備中
     *
     * @param dst TODO: 準備中
     * @param dlm TODO: 準備中
     * @param cnt TODO: 準備中
     */
    public static void unstringInto(AbstractCobolField dst, AbstractCobolField dlm, int cnt) {
        unstringInto(dst, dlm, null);
    }

    /**
     * TODO: 準備中
     *
     * @param dst TODO: 準備中
     * @param dlm TODO: 準備中
     * @param cnt TODO: 準備中
     */
    public static void unstringInto(AbstractCobolField dst, int dlm, AbstractCobolField cnt) {
        unstringInto(dst, null, cnt);
    }

    /**
     * TODO: 準備中
     *
     * @param dst TODO: 準備中
     * @param dlm TODO: 準備中
     * @param cnt TODO: 準備中
     */
    public static void unstringInto(AbstractCobolField dst, int dlm, int cnt) {
        unstringInto(dst, null, null);
    }

    /**
     * TODO: 準備中
     *
     * @param dst TODO: 準備中
     * @param dlm TODO: 準備中
     * @param cnt TODO: 準備中
     */
    public static void unstringInto(int dst, int dlm, int cnt) {
        unstringInto(null, null, null);
    }

    /**
     * TODO: 準備中
     *
     * @param dst TODO: 準備中
     * @param dlm TODO: 準備中
     * @param cnt TODO: 準備中
     */
    public static void unstringInto(int dst, AbstractCobolField dlm, int cnt) {
        unstringInto(null, dlm, null);
    }

    /**
     * TODO: 準備中
     *
     * @param dst TODO: 準備中
     * @param dlm TODO: 準備中
     * @param cnt TODO: 準備中
     */
    public static void unstringInto(int dst, int dlm, AbstractCobolField cnt) {
        unstringInto(null, null, cnt);
    }

    /**
     * TODO: 準備中
     *
     * @param dst TODO: 準備中
     * @param dlm TODO: 準備中
     * @param cnt TODO: 準備中
     */
    public static void unstringInto(int dst, AbstractCobolField dlm, AbstractCobolField cnt) {
        unstringInto(null, dlm, cnt);
    }

    /**
     * TODO: 準備中
     *
     * @param dst TODO: 準備中
     * @param dlm TODO: 準備中
     * @param cnt TODO: 準備中
     */
    public static void unstringInto(
            AbstractCobolField dst, AbstractCobolField dlm, AbstractCobolField cnt) {
        if (CobolExceptionInfo.code != 0) {
            return;
        }
        if (unstringOffset >= unstringSrc.getSize()) {
            return;
        }

        CobolDataStorage srcData = unstringSrc.getDataStorage();
        int start = unstringOffset;
        int matchSize = 0;
        int dlmSize = 0;
        CobolDataStorage dlmData = null;
        if (unstringNdlms == 0) {
            matchSize = Math.min(dst.getFieldSize(), unstringSrc.getSize() - unstringOffset);
            dst.moveFrom(
                    CobolFieldFactory.makeCobolField(
                            matchSize,
                            srcData.getSubDataStorage(unstringOffset),
                            new CobolFieldAttribute(
                                    CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null)));
            unstringOffset += matchSize;
        } else {
            int srSize = unstringSrc.getSize();
            int s = srSize;
            boolean brkpt = false;
            for (int p = start; p < s; ++p) {
                for (int i = 0; i < unstringNdlms; ++i) {
                    int dlsize = dlmList[i].dlm.getSize();
                    CobolDataStorage dp = dlmList[i].dlm.getDataStorage();
                    if (p + dlsize > s) {
                        continue;
                    }
                    if (srcData.getSubDataStorage(p).memcmp(dp, dlsize) == 0) {
                        matchSize = (p - start);
                        dst.moveFrom(
                                CobolFieldFactory.makeCobolField(
                                        matchSize,
                                        srcData.getSubDataStorage(start),
                                        new CobolFieldAttribute(
                                                CobolFieldAttribute.COB_TYPE_ALPHANUMERIC,
                                                0,
                                                0,
                                                0,
                                                null)));
                        unstringOffset += matchSize + dlsize;
                        dlmData = dp;
                        dlmSize = dlsize;
                        if (dlmList[i].all != 0) {
                            for (p += dlsize; p < s; p += dlsize) {
                                if (p + dlsize > s) {
                                    break;
                                }
                                if (srcData.getSubDataStorage(p).memcmp(dp, dlsize) != 0) {
                                    break;
                                }
                                unstringOffset += dlsize;
                            }
                        }
                        brkpt = true;
                        break;
                    }
                }
                switch (unstringSrc.getAttribute().getType()) {
                    case CobolFieldAttribute.COB_TYPE_NATIONAL:
                    case CobolFieldAttribute.COB_TYPE_NATIONAL_EDITED:
                        p++;
                        break;
                    default:
                        break;
                }

                if (brkpt) {
                    break;
                }
            }
            if (!brkpt) {
                matchSize = unstringSrc.getSize() - unstringOffset;
                dst.moveFrom(
                        CobolFieldFactory.makeCobolField(
                                matchSize,
                                srcData.getSubDataStorage(start),
                                new CobolFieldAttribute(
                                        CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null)));
                unstringOffset = unstringSrc.getSize();
                dlmData = null;
            }
        }
        unstringCount++;

        if (dlm != null) {
            if (dlmData != null) {
                dlm.moveFrom(
                        CobolFieldFactory.makeCobolField(
                                dlmSize,
                                dlmData,
                                new CobolFieldAttribute(
                                        CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null)));
            } else if (dlm.getAttribute().isTypeNumeric()) {
                dlm.moveFrom(CobolConstant.zero);
            } else {
                dlm.moveFrom(CobolConstant.space);
            }
        }

        switch (unstringSrc.getAttribute().getType()) {
            case CobolFieldAttribute.COB_TYPE_NATIONAL:
            case CobolFieldAttribute.COB_TYPE_NATIONAL_EDITED:
                matchSize /= 2;
                break;
            default:
                break;
        }

        if (cnt != null) {
            cnt.setInt(matchSize);
        }
    }

    /**
     * TODO: 準備中
     *
     * @param f TODO: 準備中
     * @throws CobolStopRunException TODO: 準備中
     */
    public static void unstringTallying(int f) throws CobolStopRunException {}

    /**
     * TODO: 準備中
     *
     * @param f TODO: 準備中
     * @throws CobolStopRunException TODO: 準備中
     */
    public static void unstringTallying(AbstractCobolField f) throws CobolStopRunException {
        f.addInt(unstringCount);
    }

    /** TODO: 準備中 */
    public static void unstringFinish() {
        if (unstringOffset < unstringSrc.getSize()) {
            CobolRuntimeException.setException(CobolExceptionId.COB_EC_OVERFLOW_UNSTRING);
        }

        switch (unstringSrc.getAttribute().getType()) {
            case CobolFieldAttribute.COB_TYPE_NATIONAL:
            case CobolFieldAttribute.COB_TYPE_NATIONAL_EDITED:
                unstringOffset /= 2;
                break;
            default:
                break;
        }

        if (unstringPtr != null) {
            unstringPtr.setInt(unstringOffset + 1);
        }
    }
}
