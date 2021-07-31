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
