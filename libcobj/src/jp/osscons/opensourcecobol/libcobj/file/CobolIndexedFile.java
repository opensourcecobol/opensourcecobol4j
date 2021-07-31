package jp.osscons.opensourcecobol.libcobj.file;

import java.io.File;
import java.nio.ByteBuffer;
import java.util.Arrays;
import java.util.Properties;

import com.sleepycat.bind.ByteArrayBinding;
import com.sleepycat.je.Cursor;
import com.sleepycat.je.CursorConfig;
import com.sleepycat.je.Database;
import com.sleepycat.je.DatabaseConfig;
import com.sleepycat.je.DatabaseEntry;
import com.sleepycat.je.DatabaseNotFoundException;
import com.sleepycat.je.Environment;
import com.sleepycat.je.EnvironmentConfig;
import com.sleepycat.je.EnvironmentFailureException;
import com.sleepycat.je.EnvironmentLockedException;
import com.sleepycat.je.Get;
import com.sleepycat.je.LockConflictException;
import com.sleepycat.je.LockMode;
import com.sleepycat.je.OperationFailureException;
import com.sleepycat.je.OperationResult;
import com.sleepycat.je.OperationStatus;
import com.sleepycat.je.Put;
import com.sleepycat.je.ReadOptions;
import com.sleepycat.je.Transaction;

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

	private Environment env;// bdb_env
	private Transaction txn;

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

	/**
	 * DBT_SETマクロの実装
	 * @param key
	 * @param field
	 */
	private DatabaseEntry DBT_SET(AbstractCobolField field) {
		return new DatabaseEntry(field.getDataStorage().getByteArray(0, field.getSize()));
	}

	/**
	 * libcob/fileio.cのtest_record_lockの実装
	 * @param key
	 * @return
	 */
	private int test_record_lock(DatabaseEntry key) {
		return 0;
	}

	/**
	 * libcob/fileio.cのlock_recordの実装
	 * @param key
	 * @return
	 */
	private int lock_record(DatabaseEntry key) {
		return 0;
	}

	/**
	 * libcob/fileio.cのindexed_openの実装
	 */
	@Override
	public int open_(String filename, int mode, int sharing) {
		IndexedFile p = new IndexedFile();
		
		File dir = new File(filename);
		if(!dir.exists()) {
			if (mode == COB_OPEN_OUTPUT || mode == COB_OPEN_I_O || mode == COB_OPEN_EXTEND) {
				dir.mkdir();
			} else {
				return ENOENT;
			}
		}

		Properties prop = new Properties();
		prop.setProperty("je.freeDisk", "104857600");
		EnvironmentConfig envConf = new EnvironmentConfig(prop);
		envConf.setTransactional(true);

		if (mode == COB_OPEN_INPUT) {
			envConf.setReadOnly(true);
		} else {
			envConf.setAllowCreate(true);
		}

		p.db = new Database[this.nkeys];
		p.sub_db = new Database[this.nkeys - 1];
		p.cursor = new Cursor[this.nkeys];
		p.filenamelen = filename.length();
		p.last_readkey = new CobolDataStorage[2 * this.nkeys];
		p.last_dupno = new int[this.nkeys];
		p.rewrite_sec_key = new int[this.nkeys];

		int maxsize = 0;
		for (int i = 0; i < this.nkeys; ++i) {
			if (this.keys[i].getField().getSize() > maxsize) {
				maxsize = this.keys[i].getField().getSize();
			}
		}

		try {
			env = new Environment(new File(filename), envConf);
		} catch (EnvironmentLockedException e) {
			return COB_STATUS_61_FILE_SHARING;
		} catch (Exception e) {
			return COB_STATUS_30_PERMANENT_ERROR;
		}

		for (int i = 0; i < this.nkeys; ++i) {
			if (i == 0) {
				runtime_buffer = filename;
			} else {
				runtime_buffer = String.format("%s.%d", filename, i);
			}
			String subDbName = String.format("%s.sub.%d", filename, i);

			DatabaseConfig dbConf = new DatabaseConfig();
			if (mode == COB_OPEN_INPUT) {
				dbConf.setReadOnly(true);
			} else {
				dbConf.setAllowCreate(true);
			}
			dbConf.setSortedDuplicates(true);
			dbConf.setTransactional(true);

			if (mode == COB_OPEN_OUTPUT) {
				try {
					env.removeDatabase(null, runtime_buffer);
					if(i > 0) {
						env.removeDatabase(null, subDbName);
					}
				} catch (DatabaseNotFoundException e) {
				}
			}

			try {
				p.db[i] = env.openDatabase(null, runtime_buffer, dbConf);
				if(i > 0) {
					p.sub_db[i-1] = env.openDatabase(null, subDbName, dbConf);
				}
			} catch (OperationFailureException | EnvironmentFailureException | IllegalStateException
					| IllegalArgumentException e) {
				e.printStackTrace();
				for (int j = 0; j<i; ++j) {
					p.db[j].close();
				}
				return COB_STATUS_30_PERMANENT_ERROR;
			}

			p.last_readkey[i] = new CobolDataStorage(maxsize);
			p.last_readkey[this.nkeys + i] = new CobolDataStorage(maxsize);
		}
		p.temp_key = new CobolDataStorage(maxsize + 4);
		this.filei = p;
		p.key_index = 0;
		p.last_key = null;

		p.key = new DatabaseEntry();
		p.data = new DatabaseEntry();

		p.filename = filename;
		p.write_cursor_open = false;
		p.record_locked = false;

		p.key = DBT_SET(this.keys[0].getField());
		p.cursor[0] = p.db[0].openCursor(null, null);
		boolean seqError = false;
		try {
			p.cursor[0].getFirst(p.key, p.data, null);
		} catch (OperationFailureException | EnvironmentFailureException | IllegalStateException
				| IllegalArgumentException e) {
			seqError = true;
		}

		p.cursor[0].close();
		p.cursor[0] = null;

		if (seqError) {
			p.data = new DatabaseEntry(new byte[0]);
		} else {
			byte[] keyData = p.key.getData();
			p.last_readkey[0].memcpy(keyData, keyData.length);
		}

		return 0;
	}

	/**
	 * libcob/fileio.cのindexed_closeの実装
	 */
	@Override
	public int close_(int opt) {
		IndexedFile p = this.filei;

		for (int i = 0; i < this.nkeys; ++i) {
			if (p.cursor[i] != null) {
				p.cursor[i].close();
			}
		}

		for (int i = this.nkeys - 1; i >= 0; i--) {
			if (p.db[i] != null) {
				p.db[i].close();
			}
		}
		
		for(int i=0; i<this.nkeys - 1; ++i) {
			if(p.sub_db[i] != null) {
				p.sub_db[i].close();
			}
		}

		env.close();
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
		IndexedFile p = this.filei;
		for (p.key_index = 0; p.key_index < this.nkeys; p.key_index++) {
			int size = this.keys[p.key_index].getField().getSize();
			if (this.keys[p.key_index].getField().getDataStorage().isSame(key.getDataStorage())) {
				break;
			}
		}

		p.key = DBT_SET(key);

		CursorConfig cursorConfig = new CursorConfig();
		ReadOptions readOptions = new ReadOptions();
		if ((read_opts & (COB_READ_LOCK | COB_READ_WAIT_LOCK)) != 0) {
			readOptions.setLockMode(LockMode.DEFAULT);
		} else {
			cursorConfig.setReadUncommitted(true);
			readOptions.setLockMode(LockMode.READ_UNCOMMITTED);
		}

		if (p.key_index != 0) {
			p.cursor[0] = p.db[0].openCursor(null, cursorConfig);
		}
		p.cursor[p.key_index] = p.db[p.key_index].openCursor(null, cursorConfig);
		try {
			OperationResult result = p.cursor[p.key_index].get(p.key, p.data, Get.SEARCH_GTE, readOptions);
			int ret = result != null ? 0 : 1;

			switch (cond) {
			case COB_EQ:
				if (ret == 0) {
					ret = new CobolDataStorage(p.key.getData()).memcmp(key.getDataStorage(), key.getSize());
				}
				break;
			case COB_LT:
				if (ret != 0) {
					result = p.cursor[p.key_index].get(p.key, p.data, Get.LAST, readOptions);
				} else {
					result = p.cursor[p.key_index].get(p.key, p.data, Get.PREV, readOptions);
				}
				ret = result != null ? 0 : 1;
				break;
			case COB_LE:
				if (ret != 0) {
					result = p.cursor[p.key_index].get(p.key, p.data, Get.LAST, readOptions);
				} else if (new CobolDataStorage(p.key.getData()).memcmp(key.getDataStorage(), key.getSize()) != 0) {
					result = p.cursor[p.key_index].get(p.key, p.data, Get.PREV, readOptions);
				} else if (this.keys[p.key_index].getFlag() != 0) {
					result = p.cursor[p.key_index].get(p.key, p.data, Get.NEXT_NO_DUP, readOptions);
					if (result == null) {
						result = p.cursor[p.key_index].get(p.key, p.data, Get.LAST, readOptions);
					} else {
						result = p.cursor[p.key_index].get(p.key, p.data, Get.PREV, readOptions);
					}
				}
				ret = result != null ? 0 : 1;
				break;
			case COB_GT:
				while (ret == 0
						&& new CobolDataStorage(p.key.getData()).memcmp(key.getDataStorage(), key.getSize()) == 0) {
					result = p.cursor[p.key_index].get(p.key, p.data, Get.NEXT, readOptions);
					ret = result != null ? 0 : 1;
				}
				break;
			case COB_GE:
				break;
			}

			int dupno = 0;

			if (ret == 0 && p.key_index > 0) {
				p.temp_key.memcpy(p.key.getData(), this.keys[p.key_index].getField().getSize());
				if (this.keys[p.key_index].getFlag() != 0) {
					dupno = ByteBuffer.wrap(p.data.getData(), this.keys[0].getField().getSize(), 4).getInt();
				}
				p.key = new DatabaseEntry(p.data.getData());
				p.key.setSize(this.keys[0].getField().getSize());
				result = p.db[0].get(null, p.key, p.data, Get.SEARCH, readOptions);
				ret = result != null ? 0 : 1;
			}

			//TODO 注意 ロック処理を削除した

			if (ret == 0) {
				if (p.key_index == 0) {
					p.last_readkey[0].memcpy(p.key.getData(), this.keys[0].getField().getSize());
				} else {
					p.last_readkey[p.key_index].memcpy(p.temp_key, this.keys[p.key_index].getField().getSize());
					p.last_readkey[p.key_index + this.nkeys].memcpy(p.key.getData(), this.keys[0].getField().getSize());
					if (this.keys[p.key_index].getFlag() != 0) {
						p.last_dupno[p.key_index] = dupno;
					}
				}
			}

			this.discardCursors(p);
			return ret == 0 ? COB_STATUS_00_SUCCESS : COB_STATUS_23_KEY_NOT_EXISTS;
		} catch (LockConflictException e) {
			this.discardCursors(p);
			return COB_STATUS_51_RECORD_LOCKED;
		}
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
		IndexedFile p = this.filei;
		boolean test_lock = false;

		if (env != null) {
			this.unlock_record();
			test_lock = true;
		}

		int ret = this.indexed_start_internal(COB_EQ, key, readOpts, test_lock);
		if (ret != COB_STATUS_00_SUCCESS) {
			return ret;
		}

		this.record.setSize(p.data.getSize());
		this.record.getDataStorage().memcpy(p.data.getData(), p.data.getSize());

		return COB_STATUS_00_SUCCESS;
	}

	/**
	 * libcob/fileio.cのindexed_read_nextの実装
	 */
	@Override
	public int readNext(int readOpts) {
		IndexedFile p = this.filei;
		int ret = 0;
		OperationResult result;
		int dupno = 0;
		int file_changed = 0;
		boolean read_nextprev = false;

		if (env != null) {
			this.unlock_record();
		}

		Get nextprev = Get.NEXT;
		if ((readOpts & COB_READ_PREVIOUS) != 0) {
			if (this.flag_end_of_file) {
				nextprev = Get.LAST;
			} else {
				nextprev = Get.PREV;
			}
		} else if (this.flag_begin_of_file) {
			nextprev = Get.FIRST;
		}

		CursorConfig cursorConfig = new CursorConfig();
		ReadOptions readOptions = new ReadOptions();
		LockMode lockMode;
		if ((readOpts & (COB_READ_LOCK | COB_READ_WAIT_LOCK)) != 0) {
			readOptions.setLockMode(LockMode.DEFAULT);
			lockMode = LockMode.DEFAULT;
		} else {
			cursorConfig.setReadUncommitted(true);
			readOptions.setLockMode(LockMode.READ_UNCOMMITTED);
			lockMode = LockMode.READ_UNCOMMITTED;
		}

		if (p.key_index != 0) {
			p.cursor[0] = p.db[0].openCursor(null, cursorConfig);
		}
		p.cursor[p.key_index] = p.db[p.key_index].openCursor(null, cursorConfig);

		if (this.flag_first_read != 0) {
			if (p.data == null || p.data.getSize() == 0 || (this.flag_first_read == 2 && nextprev == Get.PREV)) {
				this.discardCursors(p);
				return COB_STATUS_10_END_OF_FILE;
			}

			p.key = new DatabaseEntry(
					p.last_readkey[p.key_index].getByteArray(0, this.keys[p.key_index].getField().getSize()));
			try {
				result = p.cursor[p.key_index].get(p.key, p.data, Get.SEARCH, readOptions);
				ret = result != null ? 0 : 1;
			} catch(LockConflictException e) {
				this.discardCursors(p);
				return COB_STATUS_51_RECORD_LOCKED;
			}

			if (ret == 0 && p.key_index > 0) {
				if (this.keys[p.key_index].getFlag() != 0) {
					dupno = ByteBuffer.wrap(p.data.getData(), this.keys[0].getField().getSize(), 4).getInt();
					while (ret == 0 &&
							p.last_readkey[p.key_index].memcmp(p.key.getData(), p.key.getSize()) == 0 &&
							dupno < p.last_dupno[p.key_index]) {
						try {
							result = p.cursor[p.key_index].get(p.key, p.data, Get.NEXT, readOptions);
							ret = result != null ? 0 : 1;
						} catch(LockConflictException e) {
							this.discardCursors(p);
							return COB_STATUS_51_RECORD_LOCKED;
						}
						dupno = ByteBuffer.wrap(p.data.getData(), this.keys[0].getField().getSize(), 4).getInt();
					}
					if (ret == 0 &&
							p.last_readkey[p.key_index].memcmp(p.key.getData(), p.key.getSize()) == 0 &&
							dupno == p.last_dupno[p.key_index]) {
						ret = p.last_readkey[p.key_index + this.nkeys].memcmp(p.data.getData(),
								this.keys[0].getField().getSize());
					} else {
						ret = 1;
					}
				} else {
					ret = p.last_readkey[p.key_index + this.nkeys].memcmp(p.data.getData(),
							this.keys[0].getField().getSize());
				}
				if (ret == 0) {
					p.key = new DatabaseEntry(p.last_readkey[p.key_index + this.nkeys].getByteArray(0,
							this.keys[0].getField().getSize()));
					try {
						result = p.db[0].get(null, p.key, p.data, Get.SEARCH, readOptions);
						ret = result != null ? 0 : 1;
					} catch(LockConflictException e) {
						this.discardCursors(p);
						return COB_STATUS_51_RECORD_LOCKED;
					}
				}
			}
			file_changed = ret;
		}
		if (this.flag_first_read == 0 || file_changed != 0) {
			if (nextprev == Get.FIRST || nextprev == Get.LAST) {
				read_nextprev = true;
			} else {
				p.key = new DatabaseEntry(
						p.last_readkey[p.key_index].getByteArray(0, this.keys[p.key_index].getField().getSize()));
				try {
					result = p.cursor[p.key_index].get(p.key, p.data, Get.SEARCH_GTE, readOptions);
				} catch(LockConflictException e) {
					this.discardCursors(p);
					return COB_STATUS_51_RECORD_LOCKED;
				}

				ret = result != null ? 0 : 1;

				if (ret != 0) {
					if (nextprev == Get.PREV) {
						nextprev = Get.LAST;
						read_nextprev = true;
					} else {
						this.discardCursors(p);
						return COB_STATUS_10_END_OF_FILE;
					}
				} else {
					if (p.last_readkey[p.key_index].memcmp(p.key.getData(), p.key.getSize()) == 0) {
						if (p.key_index > 0 && this.keys[p.key_index].getFlag() != 0) {
							dupno = ByteBuffer.wrap(p.data.getData(), this.keys[0].getField().getSize(), 4)
									.getInt();
							while (ret == 0 &&
									p.last_readkey[p.key_index].memcmp(p.key.getData(), p.key.getSize()) == 0 &&
									dupno < p.last_dupno[p.key_index]) {

								try {
									result = p.cursor[p.key_index].get(p.key, p.data, Get.NEXT, readOptions);
								} catch(LockConflictException e) {
									this.discardCursors(p);
									return COB_STATUS_51_RECORD_LOCKED;
								}

								ret = result != null ? 0 : 1;
								dupno = ByteBuffer.wrap(p.data.getData(), this.keys[0].getField().getSize(), 4)
										.getInt();
							}
							if (ret != 0) {
								if (nextprev == Get.PREV) {
									nextprev = Get.LAST;
									read_nextprev = true;
								} else {
									this.discardCursors(p);
									return COB_STATUS_10_END_OF_FILE;
								}
							} else {
								if (p.last_readkey[p.key_index].memcmp(p.key.getData(), p.key.getSize()) == 0 &&
										dupno == p.last_dupno[p.key_index]) {
									read_nextprev = true;
								} else {
									read_nextprev = nextprev == Get.PREV;
								}
							}
						} else {
							read_nextprev = true;
						}
					} else {
						read_nextprev = nextprev == Get.PREV;
					}
				}
			}
			if (read_nextprev) {
				try {
					result = p.cursor[p.key_index].get(p.key, p.data, nextprev, readOptions);
				} catch(LockConflictException e) {
					this.discardCursors(p);
					return COB_STATUS_51_RECORD_LOCKED;
				}

				ret = result != null ? 0 : 1;
				if (ret != 0) {
					this.discardCursors(p);
					return COB_STATUS_10_END_OF_FILE;
				}
			}

			if (p.key_index > 0) {
				p.temp_key.memcpy(p.key.getData(), p.key.getSize());
				if (this.keys[p.key_index].getFlag() != 0) {
					dupno = ByteBuffer.wrap(p.data.getData(), this.keys[0].getField().getSize(), 4).getInt();
				}
				p.key = new DatabaseEntry(p.data.getData());
				p.key.setSize(this.keys[0].getField().getSize());
				try {
					if (p.db[0].get(null, p.key, p.data, Get.SEARCH, readOptions) == null) {
						p.cursor[p.key_index].close();
						p.cursor[p.key_index] = null;
						p.cursor[0].close();
						p.cursor[0] = null;
						return COB_STATUS_23_KEY_NOT_EXISTS;
					}
				} catch(LockConflictException e) {
					p.cursor[p.key_index].close();
					p.cursor[p.key_index] = null;
					p.cursor[0].close();
					p.cursor[0] = null;
					return COB_STATUS_51_RECORD_LOCKED;
				}
			}
			if (p.key_index == 0) {
				p.last_readkey[0].memcpy(p.key.getData(), p.key.getSize());
			} else {
				p.last_readkey[p.key_index].memcpy(p.temp_key, this.keys[p.key_index].getField().getSize());
				p.last_readkey[p.key_index + this.nkeys].memcpy(p.key.getData(),
						this.keys[0].getField().getSize());
				if (this.keys[p.key_index].getFlag() != 0) {
					p.last_dupno[p.key_index] = dupno;
				}
			}
		}

		this.discardCursors(p);

		this.record.setSize(p.data.getSize());
		this.record.getDataStorage().memcpy(p.data.getData(), p.data.getSize());
		return COB_STATUS_00_SUCCESS;
	}


	/**
	 * libcob/fileio.cにはない関数
	 * 複数回出現するカーソルを閉じる処理をまとめた
	 */
	private void discardCursors(IndexedFile p) {
		p.cursor[p.key_index].close();
		p.cursor[p.key_index] = null;
		if (p.key_index != 0) {
			p.cursor[0].close();
			p.cursor[0] = null;
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
		this.txn = env.beginTransaction(null, null);
		p.cursor[0] = p.db[0].openCursor(this.txn, null);
		p.write_cursor_open = true;
		close_cursor = true;
		
		if (this.nkeys > 1 && !rewrite) {
			if (this.check_alt_keys(false)) {
				if (close_cursor) {
					p.cursor[0].close();
					p.cursor[0] = null;
					p.write_cursor_open = false;
				}
				this.txn.abort();
				return COB_STATUS_22_KEY_EXISTS;
			}
			p.key = DBT_SET(this.keys[0].getField());
		}

		if (p.cursor[0].get(p.key, p.data, Get.SEARCH, null) != null) {
			if (close_cursor) {
				p.cursor[0].close();
				p.cursor[0] = null;
				p.write_cursor_open = false;
			}
			this.txn.abort();
			return COB_STATUS_22_KEY_EXISTS;
		}

		p.data = new DatabaseEntry(this.record.getDataStorage().getByteArray(0, this.record.getSize()));
		try {
			p.cursor[0].put(p.key, p.data);
		} catch (LockConflictException e) {
			if (close_cursor) {
				p.cursor[0].close();
				p.cursor[0] = null;
				p.write_cursor_open = false;
			}		
			this.txn.abort();
			return COB_STATUS_51_RECORD_LOCKED;
		}		

		p.data = p.key;
		Put flags;
		DatabaseEntry subKey = DBT_SET(this.keys[0].getField());
		for (int i = 1; i < this.nkeys; i++) {

			flags = Put.OVERWRITE;
			if (this.keys[i].getFlag() != 0) {
				int dupno = this.get_dupno(i);
				p.temp_key.memcpy(this.keys[0].getField().getDataStorage(), this.keys[0].getField().getFieldSize());
				p.temp_key.getSubDataStorage(this.keys[0].getField().getSize()).set(dupno);
				p.data = new DatabaseEntry(p.temp_key.getByteArray(0, this.keys[0].getField().getSize() + 4));
			}

			p.key = DBT_SET(this.keys[i].getField());
			try {
				DatabaseEntry subData = DBT_SET(this.keys[i].getField());
				boolean writeFail = p.db[i].put(this.txn, p.key, p.data, flags, null) == null;
				byte[] b = subKey.getData();
				OperationResult result = p.sub_db[i-1].put(this.txn, subKey, subData, Put.OVERWRITE, null);
				writeFail |= result == null;
				if(writeFail) {
					if (close_cursor) {
						p.cursor[0].close();
						p.cursor[0] = null;
						p.write_cursor_open = false;
					}
					this.txn.abort();
					return COB_STATUS_22_KEY_EXISTS;
				}
			} catch (LockConflictException e) {
				if (close_cursor) {
					p.cursor[0].close();
					p.cursor[0] = null;
					p.write_cursor_open = false;
				}				
				this.txn.abort();
				return COB_STATUS_51_RECORD_LOCKED;
			} catch (OperationFailureException e) {
				if (close_cursor) {
					p.cursor[0].close();
					p.cursor[0] = null;
					p.write_cursor_open = false;
				}
				this.txn.abort();
				return COB_STATUS_22_KEY_EXISTS;
			}
		}

		if (close_cursor) {
			p.cursor[0].close();
			p.cursor[0] = null;
			p.write_cursor_open = false;
		}
		this.txn.commit();

		return COB_STATUS_00_SUCCESS;
	}

	/**
	 * libcob/fileio.cのindexed_writeの実装
	 */
	@Override
	public int write_(int opt) {
		IndexedFile p = this.filei;

		p.key = DBT_SET(this.keys[0].getField());
		if (p.last_key == null) {
			p.last_key = new CobolDataStorage(p.key.getSize());

		} else if (this.access_mode == COB_ACCESS_SEQUENTIAL) {
			byte[] keyBytes = p.key.getData();
			if (p.last_key.memcmp(keyBytes, keyBytes.length) > 0) {
				return COB_STATUS_21_KEY_INVALID;
			}
		}

		byte[] keyBytes = p.key.getData();
		p.last_key.memcpy(keyBytes, keyBytes.length);

		return indexed_write_internal(false, opt);
	}

	/**
	 * libcob/fileio.cのcheck_alt_keysの実装
	 * @param rewrite
	 * @return
	 */
	private boolean check_alt_keys(boolean rewrite) {
		IndexedFile p = this.filei;
		for (int i = 1; i < this.nkeys; ++i) {
			if (this.keys[i].getFlag() == 0) {
				p.key = DBT_SET(this.keys[i].getField());
				if (p.db[i].get(null, p.key, p.data, Get.SEARCH, null) != null) {
					if (rewrite) {
						byte[] dataBytes = p.data.getData();
						if (this.keys[0].getField().getDataStorage().memcmp(dataBytes,
								this.keys[0].getField().getSize()) != 0) {
							return true;
						}
					} else {
						return true;
					}
				}
			}
		}
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
		IndexedFile p = this.filei;
		p.key = DBT_SET(this.keys[i].getField());
		p.temp_key.memcpy(p.key.getData());
		p.cursor[i] = p.db[i].openCursor(null, null);
		OperationResult ret = p.cursor[i].get(p.key, p.data, Get.SEARCH, null);

		int dupno = 0;
		while (ret != null && p.temp_key.memcmp(p.key.getData(), p.key.getSize()) == 0) {
			int tempdupno = ByteBuffer.wrap(p.data.getData(), this.keys[0].getField().getSize(), 4).getInt();
			if(dupno < tempdupno) {
				dupno = tempdupno;
			}
			ret = p.cursor[i].get(p.key, p.data, Get.NEXT_DUP, null);
		}

		p.cursor[i].close();
		p.cursor[i] = null;

		return ++dupno;
	}	

	@Override
	/**
	 * libcob/fileio.cのindexed_rewriteの実装
	 */
	public int rewrite_(int opt) {
		IndexedFile p = this.filei;

		p.write_cursor_open = true;
		if (env != null) {
			this.unlock_record();
		}

		if (this.check_alt_keys(true)) {
			p.write_cursor_open = false;
			return COB_STATUS_22_KEY_EXISTS;
		}
		
		int ret = this.indexed_delete_internal(true);

		if (ret != COB_STATUS_00_SUCCESS) {
			p.write_cursor_open = false;
			return ret;
		}

		p.key = DBT_SET(this.keys[0].getField());
		ret = this.indexed_write_internal(true, opt);

		return ret;
	}

	/**
	 * libcob/fileio.cのindexed_delete_internalの実装
	 * @param rewrite
	 * @return
	 */
	private int indexed_delete_internal(boolean rewrite) {
		IndexedFile p = this.filei;
		boolean close_cursor;

		this.txn = this.env.beginTransaction(null, null);
		p.cursor[0] = p.db[0].openCursor(this.txn, null);
		p.write_cursor_open = true;
		close_cursor = true;

		if (env != null) {
			this.unlock_record();
		}

		if (this.access_mode != COB_ACCESS_SEQUENTIAL) {
			p.key = DBT_SET(this.keys[0].getField());
		}

		OperationResult result = p.cursor[0].get(p.key, p.data, Get.SEARCH, null);
		int ret = result != null ? 0 : 1;
		if (ret != 0 && this.access_mode != COB_ACCESS_SEQUENTIAL) {
			if (close_cursor) {
				p.cursor[0].close();
				p.cursor[0] = null;
				p.write_cursor_open = false;
			}
			this.txn.abort();
			return COB_STATUS_23_KEY_NOT_EXISTS;
		}

		DatabaseEntry prim_key = p.key;

		for (int i = 1; i < this.nkeys; ++i) {
			p.key = DBT_SET(this.keys[i].getField());
			try {				
				DatabaseEntry subKey = DBT_SET(this.keys[0].getField());
				DatabaseEntry subData = new DatabaseEntry();
				Cursor subCursor = p.sub_db[i-1].openCursor(this.txn, null);
				if(subCursor.get(subKey, subData, Get.SEARCH, null) != null) {
					do {
						p.cursor[i] = p.db[i].openCursor(this.txn, null);
						byte[] alternateKey = subData.getData();
						if(p.cursor[i].get(subData, p.data, Get.SEARCH, null) != null) {
							do {
								if(this.keys[0].getField().getDataStorage().memcmp(p.data.getData(), prim_key.getSize()) == 0) {
									p.cursor[i].delete();
								}
							} while(p.cursor[i].get(subData, p.data, Get.NEXT, null) != null
									&& Arrays.equals(subData.getData(), alternateKey));
						}
						p.cursor[i].close();
						p.cursor[i] = null;
						subCursor.delete();
					} while(subCursor.get(subKey, subData, Get.NEXT, null) != null &&
							this.keys[0].getField().getDataStorage().memcmp(subKey.getData(), this.keys[0].getField().getSize()) == 0);
				}
				subCursor.close();
			} catch (LockConflictException e) {
				if(p.cursor[i] != null) {
					p.cursor[i].close();
					p.cursor[i] = null;
				}
				this.txn.abort();
				return COB_STATUS_51_RECORD_LOCKED;
			}
		}

		try {
			p.cursor[0].delete();
		} catch(LockConflictException e) {
			if(close_cursor) {
				p.cursor[0].close();
				p.cursor[0] = null;
				p.write_cursor_open = false;
			}
			this.txn.abort();
			return COB_STATUS_51_RECORD_LOCKED;
		}
		if(close_cursor) {
			p.cursor[0].close();
			p.cursor[0] = null;
			p.write_cursor_open = false;
		}
		this.txn.commit();
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
		if (this.open_mode != COB_OPEN_CLOSED && this.open_mode != COB_OPEN_LOCKED) {
			return;
		}

		if (env != null) {
			this.unlock_record();
		}
	}
}
