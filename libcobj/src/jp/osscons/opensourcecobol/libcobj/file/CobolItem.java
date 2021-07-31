package jp.osscons.opensourcecobol.libcobj.file;

import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;

public class CobolItem {
	private CobolItem next;
	private int endOfBlock;
	private int recordSize;
	private byte blockByte = 0;
	private CobolDataStorage unique = new CobolDataStorage(new byte[8]);
	private CobolDataStorage item;

	public CobolItem() {
		this.next = null;
		this.endOfBlock = 0;
	}

	public CobolItem getNext() {
		return next;
	}
	public void setNext(CobolItem next) {
		this.next = next;
	}
	public int getEndOfBlock() {
		return endOfBlock;
	}
	public void setEndOfBlock(int endOfBlock) {
		this.endOfBlock = endOfBlock;
	}
	public int getRecordSize() {
		return recordSize;
	}
	public void setRecordSize(int recordSize) {
		this.recordSize = recordSize;
	}
	public byte getBlockByte() {
		return blockByte;
	}
	public void setBlockByte(byte blockByte) {
		this.blockByte = blockByte;
	}
	public CobolDataStorage getUnique() {
		return unique;
	}
	public void setUnique(CobolDataStorage unique) {
		this.unique = unique;
	}
	public CobolDataStorage getItem() {
		return item;
	}
	public void setItem(CobolDataStorage item) {
		this.item = item;
	}


}
