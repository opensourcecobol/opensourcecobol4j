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

import java.util.HashMap;
import java.util.Map;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.file.CobolFile;

/**
 * EXTERNAL属性を持つデータ項目およびファイルを管理するクラス<br>
 * libcobのcob_externalに対応する。名前をキーとして領域を一元管理し、複数のプログラム間で
 * 同一の実体(ファイルやデータ領域)を共有できるようにする。
 */
public final class CobolExternal {

    /** このインスタンスが保持するEXTERNALファイルの実体 */
    private CobolFile extAllocFile;

    /** このインスタンスが保持するEXTERNALデータ領域の実体 */
    private CobolDataStorage extAllocStorage;

    /**
     * EXTERNAL名から対応する{@link CobolExternal}インスタンスへの対応表。<br>
     * EXTERNAL項目は実行単位内のプログラム間で共有されるものであり、実行単位はスレッドごとに独立しているため、
     * スレッドごとに保持する。
     */
    private static final ThreadLocal<Map<String, CobolExternal>> externalMap =
            ThreadLocal.withInitial(HashMap::new);

    /** 現在のスレッドに紐づくEXTERNAL項目の対応表を破棄する。実行単位の終了時に呼び出す。 */
    public static void resetThreadState() {
        externalMap.remove();
    }

    /**
     * EXTERNALファイルを保持するインスタンスを生成する。
     *
     * @param file 共有するファイルの実体
     */
    private CobolExternal(CobolFile file) {
        this.extAllocFile = file;
    }

    /**
     * EXTERNALデータ領域を保持するインスタンスを生成する。
     *
     * @param storage 共有するデータ領域の実体
     * @param size データ領域のサイズ(バイト数)
     */
    private CobolExternal(CobolDataStorage storage, int size) {
        this.extAllocStorage = storage;
    }

    /**
     * 指定された名前に対応するEXTERNALファイルを取得する。<br>
     * 既に登録されていればその実体を返し、未登録の場合は新たに生成して登録した上で返す。
     *
     * @param name EXTERNALファイルの名前
     * @return 名前に対応する共有ファイル
     */
    public static CobolFile getFileAddress(String name) {
        Map<String, CobolExternal> map = externalMap.get();
        if (map.containsKey(name)) {
            return map.get(name).extAllocFile;
        } else {
            CobolFile ret = new CobolFile();
            CobolExternal ext = new CobolExternal(ret);
            map.put(name, ext);
            return ret;
        }
    }

    /**
     * 指定された名前に対応するEXTERNALデータ領域を取得する。<br>
     * 既に登録されていればその実体を返し、未登録の場合は指定サイズの領域を新たに生成して登録した上で返す。
     *
     * @param name EXTERNALデータ領域の名前
     * @param size 新規生成する場合のデータ領域のサイズ(バイト数)
     * @return 名前に対応する共有データ領域
     */
    public static CobolDataStorage getStorageAddress(String name, int size) {
        Map<String, CobolExternal> map = externalMap.get();
        if (map.containsKey(name)) {
            return map.get(name).extAllocStorage;
        } else {
            CobolDataStorage ret = new CobolDataStorage(size);
            CobolExternal ext = new CobolExternal(ret, size);
            map.put(name, ext);
            return ret;
        }
    }
}
