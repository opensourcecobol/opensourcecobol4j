package jp.osscons.opensourcecobol.libcobj.common;

import java.util.HashMap;

import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.file.CobolFile;

public class CobolExternal {
	
	private CobolFile extAllocFile;
	private CobolDataStorage extAllocStorage;
	private int status;
	private int size;
	
	public static boolean initialExternal = false;
	
	private static HashMap<String, CobolExternal> externalMap = new HashMap<String, CobolExternal>();
	
	private CobolExternal(CobolFile file) {
		this.extAllocFile = file;
	}
	
	private CobolExternal(CobolDataStorage storage, int size) {
		this.extAllocStorage = storage;
		this.size = size;
	}
	
	private CobolExternal(int status, int size) {
		this.status = status;
		this.size = size;
	}
	
	public static CobolFile getFileAddress(String name) {
		if(externalMap.containsKey(name)) {
			return externalMap.get(name).extAllocFile;
		} else {
			CobolFile ret = new CobolFile();
			CobolExternal ext = new CobolExternal(ret);
			externalMap.put(name, ext);
			return ret;
		}
	}
	
	public static CobolDataStorage getStorageAddress(String name, int size) {
		if(externalMap.containsKey(name)) {
			return externalMap.get(name).extAllocStorage;
		} else {
			CobolDataStorage ret = new CobolDataStorage(size);
			CobolExternal ext = new CobolExternal(ret, size);
			externalMap.put(name, ext);
			return ret;
		}
	}	
}