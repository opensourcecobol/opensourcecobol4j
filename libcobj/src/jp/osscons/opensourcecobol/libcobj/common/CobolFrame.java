/*
 * Copyright (C) 2020 TOKYO SYSTEM HOUSE Co., Ltd.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, 51 Franklin Street, Fifth Floor
 * Boston, MA 02110-1301 USA
 */

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
