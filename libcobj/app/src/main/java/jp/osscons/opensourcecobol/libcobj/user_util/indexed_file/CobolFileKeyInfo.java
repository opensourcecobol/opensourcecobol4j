package jp.osscons.opensourcecobol.libcobj.user_util.indexed_file;

class CobolFileKeyInfo {
    public int offset;
    public int size;
    public boolean duplicate;

    public CobolFileKeyInfo(int offset, int size, boolean duplicate) {
        this.offset = offset;
        this.size = size;
        this.duplicate = duplicate;
    }
}
