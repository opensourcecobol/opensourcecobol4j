package jp.osscons.opensourcecobol.libcobj.user_util.indexed_file;

/** ロード操作の結果を表す列挙型。 */
enum LoadResult {
    /** ロード操作が成功した。 */
    LoadResultSuccess,
    /** サイズが不正なレコードが存在するため、ロード操作が失敗した。 */
    LoadResultDataSizeMismatch,
    /** キーの重複が検出された、またはその他のエラーが発生したため、ロード操作が失敗した。 */
    LoadResultOther,
    /** 入力データの終端に達した。 */
    AtEnd,
};
