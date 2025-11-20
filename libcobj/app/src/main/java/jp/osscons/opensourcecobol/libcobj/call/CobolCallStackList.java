package jp.osscons.opensourcecobol.libcobj.call;

public class CobolCallStackList {
    private CobolCallStackList parent;
    private CobolCallStackList children;
    private CobolCallStackList sister;
    private String name;

    /** コンストラクタ */
    public CobolCallStackList() {
        this.parent = null;
        this.children = null;
        this.sister = null;
        this.name = null;
    }

    /** コンストラクタ */
    public CobolCallStackList(String name) {
        this.parent = null;
        this.children = null;
        this.sister = null;
        this.name = name;
    }

    public CobolCallStackList getParent() {
        return parent;
    }

    public void setParent(CobolCallStackList parent) {
        this.parent = parent;
    }

    public CobolCallStackList getChildren() {
        return children;
    }

    public void setChildren(CobolCallStackList children) {
        this.children = children;
    }

    public CobolCallStackList getSister() {
        return sister;
    }

    public void setSister(CobolCallStackList sister) {
        this.sister = sister;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }
}
