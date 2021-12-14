package jp.osscons.opensourcecobol.libcobj.file;

import java.io.File;


import java.nio.ByteBuffer;
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

	private final static int COB_EQ = 1;
	private final static int COB_LT = 2;
	private final static int COB_LE = 3;
	private final static int COB_GT = 4;
	private final static int COB_GE = 5;
	private final static int COB_NE = 6;

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
	private static String getTableName(int index) {
		return String.format("table%d", index);
	}
	
	private static String getCursorName(int index) {
		return String.format("cursor%d", index);
	}
	
	private static void memcpy(byte[] dst, byte[] src, int length) {
		System.arraycopy(src, 0, dst, 0, length);
	}
	
	private static void memcpy(byte[] dst, int dstOffset, byte[] src, int srcOffset, int length) {
		System.arraycopy(src, srcOffset, dst, dstOffset, length);
	}
	
	private static void memset(byte[] src, byte ch, int length) {
		memset(src, 0, ch, length);
	}
	
	private static void memset(byte[] src , int offset, byte ch, int length) {
		for(int i=0; i<length; ++i) {
			src[i + offset] = ch;
		}
	}
	
	private static int memcmp(byte[] a, byte[] b, int length) {
		return memcmp(a, 0, b, 0, length);
	}
	
	private static int memcmp(byte[] a, int offsetA, byte[] b, int offsetB, int length) {
		for(int i=0; i<length; ++i) {
			int cmpResult = a[offsetA + i] - b[offsetB + i];
			if(cmpResult != 0) {
				return cmpResult;
			}
		}
		return 0;
	}
	
	/**
	 * DBT_SETマクロの実装
	 * @param key
	 * @param field
	 */
	private byte[] DBT_SET(AbstractCobolField field) {
		return field.getDataStorage().getByteArray(0, field.getSize());
	}
	
	/**
	 * libcob/fileio.cのindexed_openの実装
	 */
	@Override
	public int open_(String filename, int mode, int sharing) {
		IndexedFile p = new IndexedFile();
		
		SQLiteConfig config = new SQLiteConfig();
		config.setReadOnly(mode == COB_OPEN_INPUT);

		p.connection = null;
		try {
			p.connection = DriverManager.getConnection("jdbc:sqlite:"+ filename, config.toProperties());
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
		p.last_readkey = new byte[2 * this.nkeys][];
		p.last_dupno = new int[this.nkeys];
		p.rewrite_sec_key = new int[this.nkeys];

		p.resultSets = new ArrayList<Optional<ResultSet>>(this.nkeys);
		for(int i=0; i<this.nkeys; ++i) {
			p.resultSets.add(Optional.empty());
		}

		int maxsize = 0;
		for (int i = 0; i < this.nkeys; ++i) {
			if (this.keys[i].getField().getSize() > maxsize) {
				maxsize = this.keys[i].getField().getSize();
			}
		}

		for (int i = 0; i < this.nkeys; ++i) {
			String tableName = getTableName(i);

			if (mode == COB_OPEN_OUTPUT) {
				try {
					Statement statement = p.connection.createStatement();
					statement.execute(String.format("drop table if exists %s", tableName));
					if(i == 0) {
						statement.execute(String.format("create table %s (key blob not null primary key, value blob not null)", tableName));
					} else if(this.keys[i].getFlag() == 0) {
						statement.execute(String.format(
							"create table %s (key blob not null primary key, value blob not null,"
							+ " foreign key (value) references %s (key))"
							, tableName, getTableName(0)));
					} else {
						statement.execute(String.format(
							"create table %s (key blob not null, value blob not null, dupNo integer not null,"
							+ " foreign key (value) references %s (key))"
							, tableName, getTableName(0)));
					}
					statement.execute(String.format("create index %s on %s(key)", getIndexName(i), tableName));
				} catch (SQLException e) {
					return COB_STATUS_30_PERMANENT_ERROR;
				}
			}

			p.last_readkey[i] = new byte[maxsize];
			p.last_readkey[this.nkeys + i] = new byte[maxsize];
		}

		p.temp_key = new CobolDataStorage(maxsize + 4);
		this.filei = p;
		p.key_index = 0;
		p.last_key = null;

		p.filename = filename;
		p.write_cursor_open = false;
		p.record_locked = false;

		//read first record
		p.key = DBT_SET(this.keys[0].getField());
		try {
			Statement statement = p.connection.createStatement();
			statement.execute(String.format("declare %s for select key, value from %s order by key", getCursorName(0), getTableName(0)));
			statement.execute(String.format("open %s", getCursorName(0)));
			ResultSet rs = statement.executeQuery(String.format("fetch first %s", getCursorName(0)));
			p.key = rs.getBytes(1); p.data = rs.getBytes(2);
			statement.execute(String.format("close %s", getCursorName(0)));
			memcpy(p.last_readkey[0], p.key, p.key.length);
		} catch(SQLException e) {
			p.data = new byte[0];
		} finally {
			p.resultSets.set(0, Optional.empty());
		}

		return 0;
	}

	/**
	 * libcob/fileio.cのindexed_closeの実装
	 */
	@Override
	public int close_(int opt) {
		IndexedFile p = this.filei;
		for(int i=0; i<0; ++i) {
			try {
				Statement statement = p.connection.createStatement();
				statement.execute(String.format("CLOSE %s", getCursorName(i)));
				statement.execute(String.format("DEALLOCATE %s", getCursorName(i)));
				Optional<ResultSet> resultSet = p.resultSets.get(i);
				if(resultSet.isPresent()) {
					resultSet.get().close();
				}
			} catch(Exception e) {
			}
		}
		return COB_STATUS_00_SUCCESS;
	}

	/**
	 * libcob/fileio.cのindexed_start_internalの実装
	 * @param cond
	 * @param key
	 * @param read_opts
	 * @param test_lock
	 * @return
	 */
	public int indexed_start_internal(int cond, AbstractCobolField key, int read_opts, boolean test_lock) {
		return 0;
	}

	@Override
	/**
	 * libcob/fileio.cのindexed_startの実装
	 */
	public int start_(int cond, AbstractCobolField key) {
		return indexed_start_internal(cond, key, 0, false);
	}

	@Override
	/**
	 * libcob/fileio.cのindexed_readの実装
	 */
	public int read_(AbstractCobolField key, int readOpts) {
		return COB_STATUS_00_SUCCESS;
	}

	/**
	 * libcob/fileio.cのindexed_read_nextの実装
	 */
	@Override
	public int readNext(int readOpts) {
		return COB_STATUS_00_SUCCESS;
	}


	private void openCursor(int index, CobolFileKey key) {
		openCursor(index, key.getField());
	}
	
	private void openCursor(int index, AbstractCobolField key) {
		openCursor(index, key.getDataStorage().getByteArray(0, key.getSize()));
	}
	
	private void openCursor(int index, byte[] key) {
		IndexedFile p = this.filei;
		try {
			PreparedStatement declareStatement = p.connection.prepareStatement(
				String.format("declare %s for select key, value from %s where key = ? order by key", getCursorName(0), getTableName(0)));
			declareStatement.setBytes(1, key);
			declareStatement.execute();
			Statement openStatement = p.connection.createStatement();
			openStatement.execute(String.format("open %s", getCursorName(0)));
		} catch(SQLException e) {
		}
	}
	
	private void closeCursor(int index) {
		IndexedFile p = this.filei;
		try {
			Statement statement = p.connection.createStatement();
			statement.execute(String.format("close %s", getCursorName(0)));
			p.resultSets.get(index).ifPresent(rs -> {
				try {
					rs.close();
				} catch (SQLException e) {
				}
			});
		} catch(SQLException e) {
		} catch (Exception e) {
		}
	}
	
	/**
	 * libcob/fileio.cのindexed_write_internalの実装
	 * @param rewrite
	 * @param opt
	 * @return
	 */
	private int indexed_write_internal(boolean rewrite, int opt) {
		IndexedFile p = this.filei;

		boolean close_cursor;
		openCursor(0, this.keys[0]);
		p.write_cursor_open = true;
		close_cursor = true;
		
		if (this.nkeys > 1 && !rewrite) {
			if (this.check_alt_keys(false)) {
				if (close_cursor) {
					closeCursor(0);
					p.write_cursor_open = false;
				}
				return COB_STATUS_22_KEY_EXISTS;
			}
			p.key = DBT_SET(this.keys[0].getField());
		}
		
		try {
			PreparedStatement selectStatement = p.connection.prepareStatement(
				String.format(
					"select * from %s where key = ?",
					getTableName(0)));
			selectStatement.setBytes(1, p.key);
			selectStatement.setFetchSize(0);
			ResultSet rs = selectStatement.executeQuery();
			if(rs.next()) {
				if(close_cursor ) {
					closeCursor(0);
					p.write_cursor_open = false;
				}
				return COB_STATUS_22_KEY_EXISTS;
			}
		} catch (SQLException e){
			return COB_STATUS_22_KEY_EXISTS;
		}
		
		p.data = DBT_SET(this.record);
		
		try {
			PreparedStatement insertStatement = p.connection.prepareStatement(
				String.format("insert into %s values (?, ?)", getTableName(0)));
			insertStatement.setBytes(1, p.key);
			insertStatement.setBytes(2, p.data);
		} catch (SQLException e) {
			if(close_cursor ) {
				closeCursor(0);
				p.write_cursor_open = false;
			}
			return COB_STATUS_51_RECORD_LOCKED;	
		}

		return COB_STATUS_00_SUCCESS;
	}

	/**
	 * libcob/fileio.cのindexed_writeの実装
	 */
	@Override
	public int write_(int opt) {
		IndexedFile p = this.filei;
		return indexed_write_internal(false, opt);
	}

	/**
	 * libcob/fileio.cのcheck_alt_keysの実装
	 * @param rewrite
	 * @return
	 */
	private boolean check_alt_keys(boolean rewrite) {
		IndexedFile p = this.filei;
		return false;
	}

	/**
	 * TODO ロック処理実装
	 * libcob/fileio.cのunlock_recordの実装
	 * @return
	 */
	private int unlock_record() {
		IndexedFile p = this.filei;
		if (!p.record_locked) {
			return 0;
		}
		p.record_lock = false;
		return 0;
	}

	/**
	 * libcob/fileio.cのget_dupnoの実装
	 * @param i
	 * @return
	 */
	private int get_dupno(int i) {
		return 0;
	}	

	@Override
	/**
	 * libcob/fileio.cのindexed_rewriteの実装
	 */
	public int rewrite_(int opt) {
		return 0;
	}

	/**
	 * libcob/fileio.cのindexed_delete_internalの実装
	 * @param rewrite
	 * @return
	 */
	private int indexed_delete_internal(boolean rewrite) {
		return COB_STATUS_00_SUCCESS;
	}
	
	private static boolean arrayEquals(byte[] a, byte[] b, int size) {
		for(int i=0; i<size; ++i) {
			if(a[i] != b[i]) {
				return false;
			}
		}
		return true;
	}

	@Override
	/**
	 * libcob/fileio.cのindexed_deleteの実装
	 */
	public int delete_() {
		return this.indexed_delete_internal(false);
	}

	@Override
	public void unlock_() {
		IndexedFile p = this.filei;
	}
}
