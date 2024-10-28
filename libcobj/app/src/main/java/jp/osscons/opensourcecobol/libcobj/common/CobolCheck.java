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
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolExceptionId;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolRuntimeException;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolStopRunException;

/** TODO: 準備中 */
public class CobolCheck {
    /**
     * TODO: 準備中
     *
     * @param i TODO: 準備中
     * @param min TODO: 準備中
     * @param max TODO: 準備中
     * @param name TODO: 準備中
     * @param len TODO: 準備中
     * @throws CobolStopRunException TODO: 準備中
     */
    public static void checkSubscript(int i, int min, int max, byte[] name, int len)
            throws CobolStopRunException {
        if (i < min || max < i) {
            CobolRuntimeException.setException(CobolExceptionId.COB_EC_BOUND_SUBSCRIPT);
            CobolUtil.runtimeError(
                    String.format(
                            "Subscript of '%s' out of bounds: %d",
                            new String(name, AbstractCobolField.charSetSJIS), i));
            CobolStopRunException.stopRunAndThrow(1);
        }
    }

    /**
     * TODO: 準備中
     *
     * @param i TODO: 準備中
     * @param min TODO: 準備中
     * @param max TODO: 準備中
     * @param name TODO: 準備中
     * @param len TODO: 準備中
     * @throws CobolStopRunException TODO: 準備中
     */
    public static void checkSubscript(long i, int min, int max, byte[] name, int len)
            throws CobolStopRunException {
        CobolCheck.checkSubscript((int) i, min, max, name, len);
    }

    /**
     * TODO: 準備中
     *
     * @param i TODO: 準備中
     * @param min TODO: 準備中
     * @param max TODO: 準備中
     * @param name TODO: 準備中
     * @param len TODO: 準備中
     * @throws CobolStopRunException TODO: 準備中
     */
    public static void checkSubscript(long i, int min, int max, CobolDataStorage name, int len)
            throws CobolStopRunException {
        CobolCheck.checkSubscript((int) i, min, max, name.getByteArrayRef(0, len), len);
    }

    /**
     * TODO: 準備中
     *
     * @param i TODO: 準備中
     * @param min TODO: 準備中
     * @param max TODO: 準備中
     * @param name TODO: 準備中
     * @throws CobolStopRunException TODO: 準備中
     */
    public static void checkOdo(int i, int min, int max, String name) throws CobolStopRunException {
        if (i < min || max < i) {
            CobolRuntimeException.setException(CobolExceptionId.COB_EC_BOUND_SUBSCRIPT);
            CobolUtil.runtimeError(
                    String.format("OCCURS DEPENDING ON '%s' out of bounds: %d", name, i));
            CobolStopRunException.stopRunAndThrow(1);
        }
    }

    /**
     * TODO: 準備中
     *
     * @param i TODO: 準備中
     * @param min TODO: 準備中
     * @param max TODO: 準備中
     * @param name TODO: 準備中
     * @throws CobolStopRunException TODO: 準備中
     */
    public static void checkOdo(int i, int min, int max, byte[] name) throws CobolStopRunException {
        CobolCheck.checkOdo(i, min, max, new String(name, AbstractCobolField.charSetSJIS));
    }
}
