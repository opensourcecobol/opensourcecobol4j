/*
 * Copyright (C) 2022-2022 TOKYO SYSTEM HOUSE Co., Ltd.
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
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.Optional;

import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;

public class IndexedFile {
	public int key_index;
	public CobolDataStorage last_key;
	public CobolDataStorage temp_key;
	public Connection connection;
	public byte[] key;
	public byte[] data;
	public byte[][] last_readkey;
	public int[] last_dupno;
	public int[] rewrite_sec_key;

	public String filename;
	public Object record_lock;
	public boolean write_cursor_open;
	public int lock_id;
	public boolean record_locked;
	public int filenamelen;
}
