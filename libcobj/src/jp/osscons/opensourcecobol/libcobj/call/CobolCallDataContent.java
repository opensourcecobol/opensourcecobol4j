package jp.osscons.opensourcecobol.libcobj.call;

import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;

public class CobolCallDataContent {
	public CobolDataStorage data;
	public long datall;
	public int dataint;
	
	public CobolCallDataContent(int size) {
		this.data = new CobolDataStorage(size);
	}
}
