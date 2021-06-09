/*
 * Copyright (C) 2020 TOKYO SYSTEM HOUSE Co., Ltd.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, 51 Franklin Street, Fifth Floor
 * Boston, MA 02110-1301 USA
 */

package jp.osscons.opensourcecobol.libcobj.file;

import com.sleepycat.je.Cursor;
import com.sleepycat.je.Database;
import com.sleepycat.je.DatabaseEntry;
import com.sleepycat.je.Transaction;

import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;

public class IndexedFile {
	public int key_index;
	public CobolDataStorage last_key;
	public CobolDataStorage temp_key;
	public Database[] db;
	public Database[] sub_db;
	public DatabaseEntry key;
	public DatabaseEntry data;
	public CobolDataStorage[] last_readkey;
	public int[] last_dupno;
	public int[] rewrite_sec_key;

	public Cursor[] cursor;
	public Transaction file_lock;
	public String filename;
	public Object record_lock;
	public boolean write_cursor_open;
	public int lock_id;
	public boolean record_locked;
	public int filenamelen;
}
