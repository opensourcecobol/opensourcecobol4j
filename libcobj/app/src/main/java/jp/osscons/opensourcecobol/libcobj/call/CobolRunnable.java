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
package jp.osscons.opensourcecobol.libcobj.call;

import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;

/** COBOLプログラムの実行単位が実装すべきインターフェース */
public interface CobolRunnable {
    /**
     * COBOLプログラムを実行する
     *
     * @param storages プログラムの入力データの可変長引数。CALL等の引数のデータ部分
     * @return プログラムの終了コード
     */
    int run(CobolDataStorage... storages);

    /** CANCELのためのメソッド */
    void cancel();

    /**
     * 取り扱いについては準備中
     *
     * @return 準備中
     */
    boolean isActive();
}
