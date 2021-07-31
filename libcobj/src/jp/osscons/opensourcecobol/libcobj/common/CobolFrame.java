package jp.osscons.opensourcecobol.libcobj.common;
import jp.osscons.opensourcecobol.libcobj.file.CobolFile;

/**
 * opensource COBOLのcob_frameに対応するクラス
 */
public class CobolFrame {
	private int performThrough;
	private int returnAddress;
	private CobolFile currentSortMergeFile;

	/**
	 * performThroughのgetter
	 * @return this.performThrough
	 */
	public int getPerformThrough() {
		return this.performThrough;
	}

	/**
	 * performThroughのsetter
	 * @param performThrough this.performThroughに設定する値
	 */	
	public void setPerformThrough(int performThrough) {
		this.performThrough = performThrough;
	}
	
	/**
	 * returnAddressのgetter
	 * @return this.performThrough
	 */
	public int getReturnAddress() {
		return this.returnAddress;
	}

	/**
	 * returnAddressのsetter
	 * @param returnAddress this.returnAddressに設定する値
	 */	
	public void setReturnAddress(int returnAddress) {
		this.returnAddress = returnAddress;
	}	

	
	/**
	 * returnAddressのgetter
	 * @return this.performThrough
	 */
	public CobolFile getCurrentSortMergeFile() {
		return this.currentSortMergeFile;
	}

	/**
	 * returnAddressのsetter
	 * @param returnAddress this.returnAddressに設定する値
	 */	
	public void setCurrentSortMergeFile(CobolFile currentSortMergeFile) {
		this.currentSortMergeFile = currentSortMergeFile;
	}
}
