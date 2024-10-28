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

import java.sql.Connection;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;

/** TODO: 準備中 */
class IndexedFile {
    /** TODO: 準備中 */
    int key_index;

    /** TODO: 準備中 */
    CobolDataStorage last_key;

    /** TODO: 準備中 */
    CobolDataStorage temp_key;

    /** TODO: 準備中 */
    Connection connection;

    /** TODO: 準備中 */
    byte[] key;

    /** TODO: 準備中 */
    byte[] data;

    /** TODO: 準備中 */
    byte[][] last_readkey;

    /** TODO: 準備中 */
    int[] last_dupno;

    /** TODO: 準備中 */
    int[] rewrite_sec_key;

    /** TODO: 準備中 */
    String filename;

    /** TODO: 準備中 */
    Object record_lock;

    /** TODO: 準備中 */
    boolean write_cursor_open;

    /** TODO: 準備中 */
    int lock_id;

    /** TODO: 準備中 */
    boolean record_locked;

    /** TODO: 準備中 */
    int filenamelen;
}
