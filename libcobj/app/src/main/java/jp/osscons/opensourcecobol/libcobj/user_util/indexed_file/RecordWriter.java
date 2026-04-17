package jp.osscons.opensourcecobol.libcobj.user_util.indexed_file;

/**
 * 出力先(ファイル/標準出力)へ1レコードずつ書き込むためのインタフェース。<br>
 * {@code cobj-idx unload}コマンドで、インデックスファイルから読み出したレコードを
 * 指定された形式で外部へ書き出すために使用される。
 */
interface RecordWriter {
    /** 出力先をオープンし、書き込み可能な状態にする。 */
    void open();

    /**
     * 1レコードを出力先に書き込む。
     *
     * @param record 書き込むレコードのバイト列
     * @return 書き込みに成功した場合は{@code true}、失敗した場合は{@code false}
     */
    boolean write(byte[] record);

    /** 出力先をクローズし、保持しているリソースを解放する。 */
    void close();
}
