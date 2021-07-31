package jp.osscons.opensourcecobol.libcobj.file;

public class MemoryStruct {
	private CobolItem first;
	private CobolItem last;
	private int count;

	public MemoryStruct() {
		this.first = null;
		this.last = null;
	}

	public CobolItem getFirst() {
		return first;
	}
	public void setFirst(CobolItem first) {
		this.first = first;
	}
	public CobolItem getLast() {
		return last;
	}
	public void setLast(CobolItem last) {
		this.last = last;
	}
	public int getCount() {
		return count;
	}
	public void setCount(int count) {
		this.count = count;
	}
}
