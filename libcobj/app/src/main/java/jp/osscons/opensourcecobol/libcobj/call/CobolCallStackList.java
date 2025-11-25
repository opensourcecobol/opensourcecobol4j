package jp.osscons.opensourcecobol.libcobj.call;

/** CALLしたプログラムを階層構造で管理するためのクラス */
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

    /** 
     * コンストラクタ 
     * 
     * @param name プログラム名
     * */
    public CobolCallStackList(String name) {
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
    public CobolCallStackList getParent() {
        return parent;
    }

    /**
     * 親ノードを設定する
     * 
     * @param parent 親ノード
     */
    public void setParent(CobolCallStackList parent) {
        this.parent = parent;
    }

    /**
     * 子ノードを取得する
     * 
     * @return children 子ノード
     */
    public CobolCallStackList getChildren() {
        return children;
    }

    /**
     * 子ノードを設定する
     * 
     * @param children 子ノード
     */
    public void setChildren(CobolCallStackList children) {
        this.children = children;
    }

    /**
     * 兄弟ノードを取得する
     * 
     * @return sister 兄弟ノード
     */
    public CobolCallStackList getSister() {
        return sister;
    }

    /**
     * 兄弟ノードを設定する
     * 
     * @param sister 兄弟ノード
     */
    public void setSister(CobolCallStackList sister) {
        this.sister = sister;
    }

    /**
     * プログラム名を取得する
     * 
     * @return name プログラム名
     */
    public String getName() {
        return name;
    }

    // /**
    //  * プログラム名を設定する
    //  * 
    //  * @param name
    //  */
    // public void setName(String name) {
    //     this.name = name;
    // }
}
