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
public class IndexedFile {
  /** TODO: 準備中 */
  public int key_index;
  /** TODO: 準備中 */
  public CobolDataStorage last_key;
  /** TODO: 準備中 */
  public CobolDataStorage temp_key;
  /** TODO: 準備中 */
  public Connection connection;
  /** TODO: 準備中 */
  public byte[] key;
  /** TODO: 準備中 */
  public byte[] data;
  /** TODO: 準備中 */
  public byte[][] last_readkey;
  /** TODO: 準備中 */
  public int[] last_dupno;
  /** TODO: 準備中 */
  public int[] rewrite_sec_key;

  /** TODO: 準備中 */
  public String filename;
  /** TODO: 準備中 */
  public Object record_lock;
  /** TODO: 準備中 */
  public boolean write_cursor_open;
  /** TODO: 準備中 */
  public int lock_id;
  /** TODO: 準備中 */
  public boolean record_locked;
  /** TODO: 準備中 */
  public int filenamelen;
}
