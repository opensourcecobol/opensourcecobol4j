package jp.osscons.opensourcecobol.libcobj.file;

public class FileStruct {
	private FileIO fp;
	private int count;

	public FileIO getFp() {
		return fp;
	}
	public void setFp(FileIO fp) {
		this.fp = fp;
	}
	public int getCount() {
		return count;
	}
	public void setCount(int count) {
		this.count = count;
	}
}
