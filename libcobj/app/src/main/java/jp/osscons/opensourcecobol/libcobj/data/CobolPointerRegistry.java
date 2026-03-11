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
package jp.osscons.opensourcecobol.libcobj.data;

import java.util.HashMap;
import java.util.Map;

/** COBOL USAGE POINTER のアドレス値と CobolDataStorage の対応を管理するレジストリ */
public class CobolPointerRegistry {
    private static long nextId = 1;
    private static final Map<Long, CobolDataStorage> idToStorage = new HashMap<>();
    private static final Map<StorageKey, Long> storageToId = new HashMap<>();

    private static class StorageKey {
        final byte[] data;
        final int index;

        StorageKey(CobolDataStorage s) {
            this.data = s.getRefOfData();
            this.index = s.getIndex();
        }

        @Override
        public boolean equals(Object o) {
            if (!(o instanceof StorageKey)) {
                return false;
            }
            StorageKey k = (StorageKey) o;
            return this.data == k.data && this.index == k.index;
        }

        @Override
        public int hashCode() {
            return System.identityHashCode(data) ^ index;
        }
    }

    /**
     * CobolDataStorage を登録し、対応するアドレス値(long)を返す。 同じバイト配列・同じインデックスの場合は同じアドレス値を返す。
     *
     * @param s 登録する CobolDataStorage (null の場合は 0L を返す)
     * @return アドレス値
     */
    public static long register(CobolDataStorage s) {
        if (s == null) {
            return 0L;
        }
        StorageKey key = new StorageKey(s);
        Long existing = storageToId.get(key);
        if (existing != null) {
            return existing;
        }
        long id = nextId++;
        idToStorage.put(id, s);
        storageToId.put(key, id);
        return id;
    }

    /**
     * アドレス値から CobolDataStorage を取得する。
     *
     * @param id アドレス値 (0L の場合は null を返す)
     * @return 対応する CobolDataStorage
     */
    public static CobolDataStorage resolve(long id) {
        if (id == 0L) {
            return null;
        }
        return idToStorage.get(id);
    }
}
