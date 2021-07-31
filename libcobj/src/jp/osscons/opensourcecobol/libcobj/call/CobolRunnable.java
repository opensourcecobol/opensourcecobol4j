package jp.osscons.opensourcecobol.libcobj.call;

import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;

/**
 * COBOLプログラムの実行単位が実装すべきインターフェース
 */
public interface CobolRunnable {
	public int run(CobolDataStorage ...storages);
	public void cancel();
	public boolean isActive();
}
