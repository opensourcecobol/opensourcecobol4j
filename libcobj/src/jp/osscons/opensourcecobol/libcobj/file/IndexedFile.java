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
