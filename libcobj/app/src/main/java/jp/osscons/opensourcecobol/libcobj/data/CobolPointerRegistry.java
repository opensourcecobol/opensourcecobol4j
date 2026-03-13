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
import java.util.IdentityHashMap;
import java.util.Map;

/**
 * COBOL USAGE POINTER のアドレス値と CobolDataStorage の対応を管理するレジストリ。
 *
 * <p>ポインタ値は long の上位32ビットにバイト配列ID、下位32ビットにオフセット(インデックス)を
 * エンコードする。これにより SET PTR UP/DOWN BY n のポインタ演算が自然に動作する。
 */
public final class CobolPointerRegistry {
    /** 次に割り当てるバイト配列IDのカウンタ */
    private static int nextId = 1;

    /** バイト配列ID → バイト配列の逆引きマップ (resolveで使用) */
    private static final Map<Integer, byte[]> idToByteArray = new HashMap<>();

    /** バイト配列 → バイト配列IDの正引きマップ (registerで使用、IdentityHashMapで参照同一性を比較) */
    private static final IdentityHashMap<byte[], Integer> byteArrayToId = new IdentityHashMap<>();

    /** バイト配列 → (インデックス → CobolDataStorage) のキャッシュ。同じ配列・同じオフセットのインスタンスを再利用する */
    private static final IdentityHashMap<byte[], Map<Integer, CobolDataStorage>>
            cobolDataStorageCache = new IdentityHashMap<>();

    private CobolPointerRegistry() {}

    /**
     * CobolDataStorage を登録し、対応するアドレス値(long)を返す。
     *
     * <p>アドレス値は上位32ビットにバイト配列ID、下位32ビットにインデックスをエンコードした値。
     * 同じバイト配列・同じインデックスの場合は同じアドレス値を返す。
     *
     * @param s 登録する CobolDataStorage (null の場合は 0L を返す)
     * @return アドレス値
     */
    public static long register(CobolDataStorage s) {
        if (s == null) {
            // NULLポインタは0Lで表現する
            return 0L;
        }

        byte[] data = s.getRefOfData();
        int index = s.getIndex();

        // バイト配列が未登録であれば新しいIDを割り当てて双方向マップに登録する
        Integer byteArrayId = byteArrayToId.get(data);
        if (byteArrayId == null) {
            byteArrayId = nextId++;
            idToByteArray.put(byteArrayId, data);
            byteArrayToId.put(data, byteArrayId);
        }

        // CobolDataStorageインスタンスをキャッシュに登録する (同じ配列・オフセットは初回のみ)
        Map<Integer, CobolDataStorage> indexCache =
                cobolDataStorageCache.computeIfAbsent(data, k -> new HashMap<>());
        indexCache.putIfAbsent(index, s);

        // 上位32ビット: バイト配列ID、下位32ビット: オフセット にエンコードして返す
        return (((long) byteArrayId) << 32) | (0x00000000ffffffffL & index);
    }

    /**
     * アドレス値から CobolDataStorage を取得する。
     *
     * <p>ポインタ演算 (SET PTR UP/DOWN BY n) された値にも対応する。 上位32ビットからバイト配列IDを、下位32ビットからオフセットを取り出し、
     * 対応する CobolDataStorage を返す。キャッシュにヒットした場合は既存インスタンスを再利用する。
     *
     * @param id アドレス値 (0L の場合は null を返す)
     * @return 対応する CobolDataStorage
     * @throws IllegalArgumentException 未登録のバイト配列IDが指定された場合
     */
    public static CobolDataStorage resolve(long id) {
        if (id == 0L) {
            // NULLポインタ
            return null;
        }

        // アドレス値を上位32ビット(バイト配列ID)と下位32ビット(オフセット)に分解する
        int byteArrayId = (int) (id >>> 32);
        int index = (int) (0x00000000ffffffffL & id);

        // バイト配列IDから元のバイト配列を取得する
        byte[] data = idToByteArray.get(byteArrayId);
        if (data == null) {
            throw new IllegalArgumentException(
                    "Invalid pointer id "
                            + id
                            + ": no byte array registered for byteArrayId "
                            + byteArrayId
                            + ".");
        }

        // キャッシュに既存のCobolDataStorageがあればそれを返す
        Map<Integer, CobolDataStorage> indexCache = cobolDataStorageCache.get(data);
        if (indexCache != null) {
            CobolDataStorage cached = indexCache.get(index);
            if (cached != null) {
                return cached;
            }
        }

        // キャッシュにない場合は新規作成してキャッシュに登録する
        // (ポインタ演算でオフセットが変わった場合にここに到達する)
        CobolDataStorage created = new CobolDataStorage(data, index);
        if (indexCache == null) {
            indexCache = new HashMap<>();
            cobolDataStorageCache.put(data, indexCache);
        }
        indexCache.put(index, created);
        return created;
    }

    /**
     * レジストリを初期状態にリセットする。
     *
     * <p>STOP RUN 時に呼び出し、登録済みのポインタ情報をすべて解放してメモリリークを防止する。
     */
    public static void clear() {
        idToByteArray.clear();
        byteArrayToId.clear();
        cobolDataStorageCache.clear();
        nextId = 1;
    }
}
