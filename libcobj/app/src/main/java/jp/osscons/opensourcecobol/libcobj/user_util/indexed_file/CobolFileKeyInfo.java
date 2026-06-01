package jp.osscons.opensourcecobol.libcobj.user_util.indexed_file;

/**
 * インデックスファイルのキー情報を保持するクラス。<br>
 * 1つのキーをレコード先頭からのオフセット(1始まり)、バイト単位のサイズ、
 * および重複キーを許可するかどうかの3つの情報で表現する。
 */
class CobolFileKeyInfo {
    /** レコード先頭からのキーのオフセット(1始まり) */
    public int offset;

    /** キーのサイズ(バイト数) */
    public int size;

    /** 重複キーを許可する場合は{@code true}、そうでない場合は{@code false} */
    public boolean duplicate;

    /**
     * 新しい{@code CobolFileKeyInfo}インスタンスを生成する。
     *
     * @param offset レコード先頭からのキーのオフセット(1始まり)
     * @param size キーのサイズ(バイト数)
     * @param duplicate 重複キーを許可する場合は{@code true}
     */
    public CobolFileKeyInfo(int offset, int size, boolean duplicate) {
        this.offset = offset;
        this.size = size;
        this.duplicate = duplicate;
    }
}
