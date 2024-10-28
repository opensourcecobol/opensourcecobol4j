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

import java.util.AbstractMap;
import java.util.HashMap;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.file.CobolFile;

/** TODO: 準備中 */
public final class CobolExternal {

    private CobolFile extAllocFile;
    private CobolDataStorage extAllocStorage;

    private static AbstractMap<String, CobolExternal> externalMap =
            new HashMap<String, CobolExternal>();

    private CobolExternal(CobolFile file) {
        this.extAllocFile = file;
    }

    private CobolExternal(CobolDataStorage storage, int size) {
        this.extAllocStorage = storage;
    }

    /**
     * TODO: 準備中
     *
     * @param name TODO: 準備中
     * @return TODO: 準備中
     */
    public static CobolFile getFileAddress(String name) {
        if (externalMap.containsKey(name)) {
            return externalMap.get(name).extAllocFile;
        } else {
            CobolFile ret = new CobolFile();
            CobolExternal ext = new CobolExternal(ret);
            externalMap.put(name, ext);
            return ret;
        }
    }

    /**
     * TODO: 準備中
     *
     * @param name TODO: 準備中
     * @param size TODO: 準備中
     * @return TODO: 準備中
     */
    public static CobolDataStorage getStorageAddress(String name, int size) {
        if (externalMap.containsKey(name)) {
            return externalMap.get(name).extAllocStorage;
        } else {
            CobolDataStorage ret = new CobolDataStorage(size);
            CobolExternal ext = new CobolExternal(ret, size);
            externalMap.put(name, ext);
            return ret;
        }
    }
}
