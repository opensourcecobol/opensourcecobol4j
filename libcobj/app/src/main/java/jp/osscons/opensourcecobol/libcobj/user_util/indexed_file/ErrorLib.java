package jp.osscons.opensourcecobol.libcobj.user_util.indexed_file;

/** cobj-idxコマンドで発生するエラーのメッセージ出力と終了コードの生成をまとめたクラス */
class ErrorLib {
    /**
     * 指定されたインデックスファイルが存在しない場合のエラーメッセージを標準エラー出力へ出力する。
     *
     * @param indexedFilePath 存在が確認できなかったインデックスファイルのパス
     * @return 終了コードとして用いる{@code 1}
     */
    static int errorFileDoesNotExist(String indexedFilePath) {
        System.err.println("error: '" + indexedFilePath + "' does not exist.");
        return 1;
    }

    /**
     * 指定されたファイルが正しいインデックスファイルでない場合のエラーメッセージを標準エラー出力へ出力する。
     *
     * @param indexedFilePath 正しいインデックスファイルとして認識できなかったファイルのパス
     * @return 終了コードとして用いる{@code 1}
     */
    static int errorInvalidIndexedFile(String indexedFilePath) {
        System.err.println("error: '" + indexedFilePath + "' is not a valid indexed file.");
        return 1;
    }

    /**
     * インデックスファイルに対する入出力処理でエラーが発生した場合のエラーメッセージを標準エラー出力へ出力する。
     *
     * @return 終了コードとして用いる{@code 1}
     */
    static int errorIO() {
        System.err.println("error: IO error.");
        return 1;
    }

    /**
     * 入力データの中でキーの重複があるためにロードに失敗した場合のエラーメッセージを標準エラー出力へ出力する。
     *
     * @return 終了コードとして用いる{@code 1}
     */
    static int errorDuplicateKeys() {
        System.err.println("error: loading fails because of duplicate keys.");
        return 1;
    }

    /**
     * 入力データに想定と異なる長さのレコードが含まれる場合のエラーメッセージを標準エラー出力へ出力する。
     *
     * @param correctSize インデックスファイルに定義されている正しいレコードサイズ(バイト数)
     * @return 終了コードとして用いる{@code 1}
     */
    static int errorDataSizeMismatch(int correctSize) {
        System.err.println("error: all record must have the length of " + correctSize + " bytes.");
        return 1;
    }
}
