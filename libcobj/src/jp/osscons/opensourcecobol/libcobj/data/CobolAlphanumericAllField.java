package jp.osscons.opensourcecobol.libcobj.data;

public class CobolAlphanumericAllField extends CobolAlphanumericField {
	/**
	   * コンストラクタ
	 * @param size データを格納するバイト配列の長さ
	 * @param dataStorage データを格納するバイト配列を扱うオブジェクト
	 * @param attribute 変数に関する様々な情報を保持するオブジェクト
	 */
	public CobolAlphanumericAllField(int size, CobolDataStorage dataStorage, CobolFieldAttribute attribute) {
		super(size, dataStorage, attribute);
	}
}
