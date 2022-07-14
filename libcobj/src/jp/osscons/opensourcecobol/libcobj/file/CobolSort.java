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

import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;

public class CobolSort {
	private CobolFile pointer;
	private CobolItem empty;
	private CobolDataStorage sortReturn;
	private AbstractCobolField fnstatus;
	private int unique;
	private int retrieving;
	private int filesUsed;
	private int size;
	private int rSize;
	private int wSize;
	private int memory;
	private int destinationFile;
	private int retrievalQueue;
	private MemoryStruct[] queue = new MemoryStruct[4];
	private FileStruct[] file = new FileStruct[4];

	CobolSort() {
		for(int i=0; i<4; ++i) {
			this.queue[i] = new MemoryStruct();
			this.file[i] = new FileStruct();
		}
	}

	public CobolFile getPointer() {
		return pointer;
	}
	public void setPointer(CobolFile pointer) {
		this.pointer = pointer;
	}
	public CobolItem getEmpty() {
		return empty;
	}
	public void setEmpty(CobolItem empty) {
		this.empty = empty;
	}
	public CobolDataStorage getSortReturn() {
		return sortReturn;
	}
	public void setSortReturn(CobolDataStorage sortReturn) {
		this.sortReturn = sortReturn;
	}
	public AbstractCobolField getFnstatus() {
		return fnstatus;
	}
	public void setFnstatus(AbstractCobolField fnstatus) {
		this.fnstatus = fnstatus;
	}
	public int getUnique() {
		return unique;
	}
	public void setUnique(int unique) {
		this.unique = unique;
	}
	public int getRetrieving() {
		return retrieving;
	}
	public void setRetrieving(int retrieving) {
		this.retrieving = retrieving;
	}
	public int getFilesUsed() {
		return filesUsed;
	}
	public void setFilesUsed(int filesUsed) {
		this.filesUsed = filesUsed;
	}
	public int getSize() {
		return size;
	}
	public void setSize(int size) {
		this.size = size;
	}
	public int getrSize() {
		return rSize;
	}
	public void setrSize(int rSize) {
		this.rSize = rSize;
	}
	public int getwSize() {
		return wSize;
	}
	public void setwSize(int wSize) {
		this.wSize = wSize;
	}
	public int getMemory() {
		return memory;
	}
	public void setMemory(int memory) {
		this.memory = memory;
	}
	public int getDestinationFile() {
		return destinationFile;
	}
	public void setDestinationFile(int destinationFile) {
		this.destinationFile = destinationFile;
	}
	public int getRetrievalQueue() {
		return retrievalQueue;
	}
	public void setRetrievalQueue(int retrievalQueue) {
		this.retrievalQueue = retrievalQueue;
	}
	public MemoryStruct[] getQueue() {
		return queue;
	}
	public void setQueue(MemoryStruct[] queue) {
		this.queue = queue;
	}
	public FileStruct[] getFile() {
		return file;
	}
	public void setFile(FileStruct[] file) {
		this.file = file;
	}
}
