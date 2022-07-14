/*
 * Copyright (C) 2021-2022 TOKYO SYSTEM HOUSE Co., Ltd.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 3.0,
 * or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; see the file COPYING.LIB.  If
 * not, write to the Free Software Foundation, 51 Franklin Street, Fifth Floor
 * Boston, MA 02110-1301 USA
 */
package jp.osscons.opensourcecobol.libcobj.file;

import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;

public class CobolItem {
	private CobolItem next;
	private int endOfBlock;
	private int recordSize;
	private byte blockByte = 0;
	private CobolDataStorage unique = new CobolDataStorage(new byte[8]);
	private CobolDataStorage item;

	public CobolItem() {
		this.next = null;
		this.endOfBlock = 0;
	}

	public CobolItem getNext() {
		return next;
	}
	public void setNext(CobolItem next) {
		this.next = next;
	}
	public int getEndOfBlock() {
		return endOfBlock;
	}
	public void setEndOfBlock(int endOfBlock) {
		this.endOfBlock = endOfBlock;
	}
	public int getRecordSize() {
		return recordSize;
	}
	public void setRecordSize(int recordSize) {
		this.recordSize = recordSize;
	}
	public byte getBlockByte() {
		return blockByte;
	}
	public void setBlockByte(byte blockByte) {
		this.blockByte = blockByte;
	}
	public CobolDataStorage getUnique() {
		return unique;
	}
	public void setUnique(CobolDataStorage unique) {
		this.unique = unique;
	}
	public CobolDataStorage getItem() {
		return item;
	}
	public void setItem(CobolDataStorage item) {
		this.item = item;
	}


}
