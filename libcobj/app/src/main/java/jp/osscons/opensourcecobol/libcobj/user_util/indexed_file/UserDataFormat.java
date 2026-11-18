package jp.osscons.opensourcecobol.libcobj.user_util.indexed_file;

/** 入出力データの形式を表す列挙型。 */
enum UserDataFormat {
    /** COBOLの行順（line-sequential）形式を表す。各レコードは改行文字（0x20）で区切られる。 */
    LINE_SEQUENTIAL,
    /** COBOLの順（sequential）形式を表す。各レコードは区切り文字なしで連結される。 */
    SEQUENTIAL,
}
