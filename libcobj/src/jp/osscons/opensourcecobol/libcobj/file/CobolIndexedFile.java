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

import java.io.File;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;

import org.sqlite.SQLiteErrorCode;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Optional;
import java.util.Properties;

import org.sqlite.SQLiteConfig;
import org.sqlite.SQLiteErrorCode;

import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;

public class CobolIndexedFile extends CobolFile {
	private static int rlo_size = 0;
	private static byte[] record_lock_object;
	private Optional<IndexedCursor> cursor;
	private boolean updateWhileReading = false;
	private boolean indexedFirstRead = true;
	private boolean callStart = false;

	public final static int COB_EQ = 1;
	public final static int COB_LT = 2;
	public final static int COB_LE = 3;
	public final static int COB_GT = 4;
	public final static int COB_GE = 5;
	public final static int COB_NE = 6;

	public CobolIndexedFile(String select_name, byte[] file_status, AbstractCobolField assign,
			AbstractCobolField record, AbstractCobolField record_size, int record_min, int record_max, int nkeys,
			CobolFileKey[] keys, char organization, char access_mode, char lock_mode, char open_mode,
			boolean flag_optional, char last_open_mode, char special, boolean flag_nonexistent,
			boolean flag_end_of_file, boolean flag_begin_of_file, char flag_first_read, boolean flag_read_done,
			char flag_select_features, boolean flag_needs_nl, boolean flag_needs_top, char file_version) {
		super(select_name, file_status, assign, record, record_size, record_min, record_max, nkeys, keys, organization,
				access_mode, lock_mode, open_mode, flag_optional, last_open_mode, special, flag_nonexistent,
				flag_end_of_file,
				flag_begin_of_file, flag_first_read, flag_read_done, flag_select_features, flag_needs_nl,
				flag_needs_top,
				file_version);
	}
	
	private static String getIndexName(int index) {
		return String.format("index%d", index);
	}
	
	private static String getSubIndexName(int index) {
		return String.format("subindex%d", index);
	}
	
	public static String getTableName(int index) {
		return String.format("table%d", index);
	}
	
	public static String getCursorName(int index) {
		return String.format("cursor%d", index);
	}
	
	private static String getConstraintName(int index) {
		return String.format("constraint%d", index);
	}
	
	/**
	 * Equivalent to DBT_SET in libcob/fileio.c
	 */
	private byte[] DBT_SET(AbstractCobolField field) {
		return field.getDataStorage().getByteArray(0, field.getSize());
	}
	
	/**
	 * Equivalent to indexed_open in libcob/fileio.c
	 */
	@Override
	public int open_(String filename, int mode, int sharing) {
		IndexedFile p = new IndexedFile();
		
		SQLiteConfig config = new SQLiteConfig();
		config.setReadOnly(mode == COB_OPEN_INPUT);
		
		if(mode == COB_OPEN_OUTPUT) {
			Path path = Paths.get(filename);
			try {
				Files.deleteIfExists(path);
			} catch (IOException e) {
				return COB_STATUS_30_PERMANENT_ERROR;
			}
		}

		boolean fileExists = new java.io.File(filename).exists();

		p.connection = null;
		try {
			p.connection = DriverManager.getConnection("jdbc:sqlite:"+ filename, config.toProperties());
			p.connection.setAutoCommit(false);
		} catch(SQLException e) {
			int errorCode = e.getErrorCode();
			if(errorCode == SQLiteErrorCode.SQLITE_BUSY.code) {
				return COB_STATUS_61_FILE_SHARING;
			} else {
				return ENOENT;
			}
		} catch (Exception e) {
			return COB_STATUS_30_PERMANENT_ERROR;
		}

		p.filenamelen = filename.length();
		p.last_dupno = new int[this.nkeys];
		p.rewrite_sec_key = new int[this.nkeys];

		int maxsize = 0;
		for (int i = 0; i < this.nkeys; ++i) {
			if (this.keys[i].getField().getSize() > maxsize) {
				maxsize = this.keys[i].getField().getSize();
			}
		}

		for (int i = 0; i < this.nkeys; ++i) {
			String tableName = getTableName(i);

			if (mode == COB_OPEN_OUTPUT || (!fileExists && (mode == COB_OPEN_EXTEND || mode == COB_OPEN_I_O))) {
				try {
					Statement statement = p.connection.createStatement();
					if(i == 0) {
						statement.execute(String.format("create table %s (key blob not null primary key, value blob not null)", tableName));
					} else {
						if(this.keys[i].getFlag() == 0) {
							statement.execute(String.format(
								"create table %s (key blob not null primary key, value blob not null,"
								+ " constraint %s foreign key (value) references %s (key))"
								, tableName, getConstraintName(i), getTableName(0)));
						} else {
							statement.execute(String.format(
								"create table %s (key blob not null, value blob not null, dupNo integer not null,"
								+ " constraint %s foreign key (value) references %s (key))"
								, tableName, getConstraintName(i), getTableName(0)));
						}
						statement.execute(String.format("create index %s on %s(value)", getSubIndexName(i), tableName));
					}
					statement.execute(String.format("create index %s on %s(key)", getIndexName(i), tableName));
				} catch (SQLException e) {
					return COB_STATUS_30_PERMANENT_ERROR;
				}
			}
		}

		p.temp_key = new CobolDataStorage(maxsize + 4);
		this.filei = p;
		p.key_index = 0;
		p.last_key = null;

		p.filename = filename;
		p.write_cursor_open = false;
		p.record_locked = false;

		p.key = DBT_SET(this.keys[0].getField());
		this.updateWhileReading = false;
		this.indexedFirstRead = true;
		this.callStart = false;

		return 0;
	}

	/**
	 * Equivalent to indexed_close in libcob/fileio.c
	 */
	@Override
	public int close_(int opt) {
		IndexedFile p = this.filei;
		
		this.closeCursor();

		try {
			p.connection.close();
		} catch (SQLException e){
			return COB_STATUS_30_PERMANENT_ERROR;
		}
		return COB_STATUS_00_SUCCESS;
	}

	/**
	 * Equivalent to indexed_start_internal in libcob/fileio.c
	 */
	public int indexed_start_internal(int cond, AbstractCobolField key, int read_opts, boolean test_lock) {		
		IndexedFile p = this.filei;
		for (p.key_index = 0; p.key_index < this.nkeys; p.key_index++) {
			int size = this.keys[p.key_index].getField().getSize();
			if (this.keys[p.key_index].getField().getDataStorage().isSame(key.getDataStorage())) {
				break;
			}
		}

		p.key = DBT_SET(key);

		boolean isDuplicate = this.keys[p.key_index].getFlag() != 0;
		boolean isPrimary = p.key_index == 0;

		this.cursor = IndexedCursor.createCursor(p.connection, p.key, p.key_index, isDuplicate, cond);
		if(this.cursor.isEmpty()) {
			return COB_STATUS_30_PERMANENT_ERROR;
		}

		IndexedCursor cursor = this.cursor.get();
		Optional<FetchResult> optionalResult = cursor.next();
		if(optionalResult.isPresent()) {
			FetchResult result = optionalResult.get();
			p.key = result.key;
			p.data = result.value;
			this.indexedFirstRead = false;
			return COB_STATUS_00_SUCCESS;
		} else {
			return COB_STATUS_23_KEY_NOT_EXISTS;
		}
	}

	@Override
	/**
	 * Equivalent to libcob/fileio.c in indexed_start
	 */
	public int start_(int cond, AbstractCobolField key) {
		int ret = indexed_start_internal(cond, key, 0, false);
		if(ret == COB_STATUS_00_SUCCESS) {
			this.callStart = true;
		}
		return ret;
	}

	@Override
	/**
	 * Equivalent to indexed_read in libcob/fileio.c
	 */
	public int read_(AbstractCobolField key, int readOpts) {
		IndexedFile p = this.filei;
		boolean test_lock = false;
		this.callStart = false;
		int ret = this.indexed_start_internal(COB_EQ, key, readOpts, test_lock);
		if (ret != COB_STATUS_00_SUCCESS) {
			return ret;
		}

		this.record.setSize(p.data.length);
		this.record.getDataStorage().memcpy(p.data, p.data.length);

		return COB_STATUS_00_SUCCESS;
	}

	/**
	 * Equivalent to indexed_read_next in libcob/fileio.c
	 */
	@Override
	public int readNext(int readOpts) {
		IndexedFile p = this.filei;
		if(this.callStart) {
			this.callStart = false;
			this.indexedFirstRead = false;
			this.record.setSize(p.data.length);
			this.record.getDataStorage().memcpy(p.data, p.data.length);		
			return COB_STATUS_00_SUCCESS;
		}
		
		boolean isDuplicate = this.keys[p.key_index].getFlag() != 0;
		if(this.indexedFirstRead || this.flag_begin_of_file) {
			this.cursor = IndexedCursor.createCursor(p.connection, p.key, p.key_index, isDuplicate, COB_GE);
			if(this.cursor.isEmpty()) {
				return COB_STATUS_30_PERMANENT_ERROR;
			}
			this.cursor.get().moveToFirst();
		} else if(this.flag_end_of_file) {
			this.cursor = IndexedCursor.createCursor(p.connection, p.key, p.key_index, isDuplicate, COB_LE);
			if(this.cursor.isEmpty()) {
				return COB_STATUS_30_PERMANENT_ERROR;
			}
			this.cursor.get().moveToLast();		
		} else if(this.updateWhileReading) {
			this.updateWhileReading = false;
			if(this.cursor.isEmpty()) {
				return COB_STATUS_30_PERMANENT_ERROR;
			}
			IndexedCursor oldCursor = this.cursor.get();
			Optional<IndexedCursor> newCursor = oldCursor.reloadCursor();
			if(newCursor.isEmpty()) {
				this.cursor = Optional.of(oldCursor);
			} else {
				oldCursor.close();
				this.cursor = newCursor;
			}
		}
		
		if(this.cursor.isEmpty()) {
			return COB_STATUS_30_PERMANENT_ERROR;
		}

		IndexedCursor cursor = this.cursor.get();

		final CursorReadOption cursorOpt;
		if((readOpts & COB_READ_PREVIOUS) != 0) {
			cursorOpt = CursorReadOption.PREV;
			cursor.setComparator(COB_LE);
		} else {
			cursorOpt = CursorReadOption.NEXT;
			cursor.setComparator(COB_GE);
		}

		Optional<FetchResult> optionalResult = cursor.read(cursorOpt);
		

		this.indexedFirstRead = false;

		if(optionalResult.isEmpty()) {
			return COB_STATUS_10_END_OF_FILE;
		} else {
			FetchResult result = optionalResult.get();
			p.key = result.key;
			p.data = result.value;
			
			this.record.setSize(p.data.length);
			this.record.getDataStorage().memcpy(p.data, p.data.length);

			return COB_STATUS_00_SUCCESS;
		}
	}
	
	private void closeCursor() {
		if(this.cursor != null) {
			if(this.cursor.isPresent()) {
				this.cursor.get().close();
			}
		}
	}
	
	private boolean keyExistsInTable(IndexedFile p, int index, byte[] key) {	
		try {
			String query = String.format(
				"select * from %s where key = ?",
				getTableName(index));

			PreparedStatement selectStatement = p.connection.prepareStatement(query);
			selectStatement.setBytes(1, key);
			selectStatement.setFetchSize(0);
			ResultSet rs = selectStatement.executeQuery();
			return rs.next();
		} catch(SQLException e) {
			return false;
		}
	}
	
	private boolean isDuplicateColumn(int index) {
		return this.keys[index].getFlag() != 0;
	}
	
	private int getNextKeyDupNo(Connection conn, int index, byte[] key) {
		try {
			PreparedStatement selectStatement = conn.prepareStatement(
				String.format("select ifnull(max(dupNo), -1) from %s", getTableName(index)));
			ResultSet rs = selectStatement.executeQuery();
			return rs.getInt(1) + 1;
		} catch(SQLException e) {
			return 0;
		}
	}
	
	private int returnWith(IndexedFile p, boolean close_cursor, int index, int returnCode) {
		if (close_cursor) {
			this.closeCursor();
			p.write_cursor_open = false;
		}
		return returnCode;
	}
	
	/**
	 * Equivalent to indexed_write_internal in libcob/fileio.c
	 */
	private int indexed_write_internal(boolean rewrite, int opt) {
		IndexedFile p = this.filei;

		boolean close_cursor;
		p.write_cursor_open = true;
		close_cursor = true;
		
		if (this.nkeys > 1 && !rewrite) {
			if (this.check_alt_keys(false)) {
				return returnWith(p, close_cursor, 0, COB_STATUS_22_KEY_EXISTS);
			}
			p.key = DBT_SET(this.keys[0].getField());
		}

		if(keyExistsInTable(p, 0, p.key)) {
			return COB_STATUS_22_KEY_EXISTS;
		}

		// insert into the primary table
		p.data = DBT_SET(this.record);
		try {
			PreparedStatement insertStatement = p.connection.prepareStatement(
				String.format("insert into %s values (?, ?)", getTableName(0)));
			insertStatement.setBytes(1, p.key);
			insertStatement.setBytes(2, p.data);
			insertStatement.execute();
			p.connection.commit();
		} catch (SQLException e) {
			return returnWith(p, close_cursor, 0, COB_STATUS_51_RECORD_LOCKED);
		}

		p.data = p.key;

		//insert into sub tables
		for (int i = 1; i < this.nkeys; i++) {

			p.key = DBT_SET(this.keys[i].getField());
			try {
				if(!isDuplicateColumn(i) && keyExistsInTable(p, i, p.key)) {
					return returnWith(p, close_cursor, 0, COB_STATUS_22_KEY_EXISTS);
				}

				PreparedStatement insertStatement;
				if(isDuplicateColumn(i)) {
					int dupNo = getNextKeyDupNo(p.connection, i, p.key);
					insertStatement = p.connection.prepareStatement(
							String.format("insert into %s values (?, ?, ?)", getTableName(i)));
					insertStatement.setBytes(1, p.key);
					insertStatement.setBytes(2, p.data);
					insertStatement.setInt(3, dupNo);
				} else {
					insertStatement = p.connection.prepareStatement(
							String.format("insert into %s values (?, ?)", getTableName(i)));
					insertStatement.setBytes(1, p.key);
					insertStatement.setBytes(2, p.data);
				}
				insertStatement.execute();
				p.connection.commit();
			} catch (SQLException e) {
				return returnWith(p, close_cursor, 0, COB_STATUS_51_RECORD_LOCKED);
			}
		}

		this.updateWhileReading = true;

		return returnWith(p, close_cursor, 0, COB_STATUS_00_SUCCESS);
	}

	/**
	 * Equivalent to indexed_write in libcob/fileio.c
	 */
	@Override
	public int write_(int opt) {
		IndexedFile p = this.filei;
		
		p.key = DBT_SET(this.keys[0].getField());
		if (p.last_key == null) {
			p.last_key = new CobolDataStorage(p.key.length);

		} else if (this.access_mode == COB_ACCESS_SEQUENTIAL) {
			byte[] keyBytes = p.key;
			if (p.last_key.memcmp(keyBytes, keyBytes.length) > 0) {
				return COB_STATUS_21_KEY_INVALID;
			}
		}

		byte[] keyBytes = p.key;
		p.last_key.memcpy(keyBytes, keyBytes.length);
		
		return indexed_write_internal(false, opt);
	}

	/**
	 * Equivalent to check_alt_keys in libcob/fileio.c
	 */
	private boolean check_alt_keys(boolean rewrite) {
		IndexedFile p = this.filei;
		
		byte[] primaryKey = DBT_SET(this.keys[0].getField());
		for (int i = 1; i < this.nkeys; ++i) {
			if (this.keys[i].getFlag() == 0) {
				p.key = DBT_SET(this.keys[i].getField());
				if(rewrite) {
					if(checkTable(p, i, p.key, primaryKey)) {
						return true;
					}
				} else {
					if(keyExistsInTable(p, i, p.key)) {
						return true;
					}
				}
			}
		}
		return false;
	}
	
	private static boolean checkTable(IndexedFile p, int index, byte[] key, byte[] primaryKey) {
		try {
			String query = String.format(
				"select key from %s " +
				"where key = ? and value = ?",
				getTableName(index));

			PreparedStatement selectStatement = p.connection.prepareStatement(query);
			selectStatement.setBytes(1, key);
			selectStatement.setBytes(2, primaryKey);
			selectStatement.setFetchSize(0);
			ResultSet rs = selectStatement.executeQuery();
			return rs.next();
		} catch(SQLException e) {
			return false;
		}
	}	

	@Override
	/**
	 * Equivalent to indexed_rewrite in libcob/fileio.c
	 */
	public int rewrite_(int opt) {
		IndexedFile p = this.filei;
		
		p.write_cursor_open = true;
		
		if(this.access_mode == COB_ACCESS_SEQUENTIAL && !IndexedCursor.matchKeyHead(p.key, DBT_SET(this.keys[0].getField()))) {
			return COB_STATUS_21_KEY_INVALID;
		}

		p.key = DBT_SET(this.keys[0].getField());

		int ret = this.indexed_delete_internal(true);

		if (ret != COB_STATUS_00_SUCCESS) {
			p.write_cursor_open = false;
			return ret;
		}

		return this.indexed_write_internal(true, opt);
	}

	/**
	 * Equivalent to indexed_delete_internal in libcob/fileio.c
	 */
	private int indexed_delete_internal(boolean rewrite) {
		IndexedFile p = this.filei;
		boolean close_cursor;

		p.write_cursor_open = true;
		close_cursor = true;
		
		if (this.access_mode != COB_ACCESS_SEQUENTIAL) {
			p.key = DBT_SET(this.keys[0].getField());
		}
		
		if(this.access_mode != COB_ACCESS_SEQUENTIAL && !keyExistsInTable(p, 0, p.key)) {
			return returnWith(p, close_cursor, 0, COB_STATUS_23_KEY_NOT_EXISTS);
		}

		// delete data from the primary table
		try {
			String query = String.format("delete from %s where key = ?", getTableName(0));
			PreparedStatement statement = p.connection.prepareStatement(query);
			statement.setBytes(1, p.key);
			statement.execute();
		} catch(SQLException e) {
			return returnWith(p, close_cursor, 0, COB_STATUS_30_PERMANENT_ERROR);
		}

		// delete data from sub tables
		for(int i=1; i<this.nkeys; ++i) {
			try {
				PreparedStatement statement = p.connection.prepareStatement(
				String.format("delete from %s where value = ?", getTableName(i)));
				statement.setBytes(1, p.key);
				statement.execute();			
			} catch(SQLException e) {
				return returnWith(p, close_cursor, 0, COB_STATUS_30_PERMANENT_ERROR);
			}
		}
		
		try {
			p.connection.commit();
		} catch(SQLException e) {
				return returnWith(p, close_cursor, 0, COB_STATUS_30_PERMANENT_ERROR);
		}
		
		this.updateWhileReading = true;

		return COB_STATUS_00_SUCCESS;
	}

	@Override
	/**
	 * Equivalent to libcob/fileio.c in indexed_delete
	 */
	public int delete_() {
		return this.indexed_delete_internal(false);
	}

	@Override
	public void unlock_() {
		IndexedFile p = this.filei;
	}
}
