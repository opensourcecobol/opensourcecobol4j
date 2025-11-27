package jp.osscons.opensourcecobol.libcobj.call;

/** CALLしたプログラムを階層構造で管理するためのクラス */
public class CobolCallStackList {
    private CobolCallStackList parent;
    private CobolCallStackList children;
    private CobolCallStackList sister;
    private String name;

    /** コンストラクタ */
    protected CobolCallStackList() {
        this.parent = null;
        this.children = null;
        this.sister = null;
        this.name = null;
    }

    /**
     * コンストラクタ
     *
     * @param name プログラム名
     */
    protected CobolCallStackList(String name) {
        this.parent = null;
        this.children = null;
        this.sister = null;
        this.name = name;
    }

    /**
     * 親ノードを取得する
     *
     * @return parent 親ノード
     */
    protected CobolCallStackList getParent() {
        return parent;
    }

    /**
     * 親ノードを設定する
     *
     * @param parent 親ノード
     */
    protected void setParent(CobolCallStackList parent) {
        this.parent = parent;
    }

    /**
     * 子ノードを取得する
     *
     * @return children 子ノード
     */
    protected CobolCallStackList getChildren() {
        return children;
    }

    /**
     * 子ノードを設定する
     *
     * @param children 子ノード
     */
    protected void setChildren(CobolCallStackList children) {
        this.children = children;
    }

    /**
     * 兄弟ノードを取得する
     *
     * @return sister 兄弟ノード
     */
    protected CobolCallStackList getSister() {
        return sister;
    }

    /**
     * 兄弟ノードを設定する
     *
     * @param sister 兄弟ノード
     */
    protected void setSister(CobolCallStackList sister) {
        this.sister = sister;
    }

    /**
     * プログラム名を取得する
     *
     * @return name プログラム名
     */
    protected String getName() {
        return name;
    }
}
