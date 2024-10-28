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

/** TODO: 準備中 */
class CobolSort {
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

    /** TODO: 準備中 */
    CobolSort() {
        for (int i = 0; i < 4; ++i) {
            this.queue[i] = new MemoryStruct();
            this.file[i] = new FileStruct();
        }
    }

    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     */
    CobolFile getPointer() {
        return pointer;
    }

    /**
     * TODO: 準備中
     *
     * @param pointer TODO: 準備中
     */
    void setPointer(CobolFile pointer) {
        this.pointer = pointer;
    }

    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     */
    CobolItem getEmpty() {
        return empty;
    }

    /**
     * TODO: 準備中
     *
     * @param empty TODO: 準備中
     */
    void setEmpty(CobolItem empty) {
        this.empty = empty;
    }

    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     */
    CobolDataStorage getSortReturn() {
        return sortReturn;
    }

    /**
     * TODO: 準備中
     *
     * @param sortReturn TODO: 準備中
     */
    void setSortReturn(CobolDataStorage sortReturn) {
        this.sortReturn = sortReturn;
    }

    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     */
    AbstractCobolField getFnstatus() {
        return fnstatus;
    }

    /**
     * TODO: 準備中
     *
     * @param fnstatus TODO: 準備中
     */
    void setFnstatus(AbstractCobolField fnstatus) {
        this.fnstatus = fnstatus;
    }

    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     */
    int getUnique() {
        return unique;
    }

    /**
     * TODO: 準備中
     *
     * @param unique TODO: 準備中
     */
    void setUnique(int unique) {
        this.unique = unique;
    }

    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     */
    int getRetrieving() {
        return retrieving;
    }

    /**
     * TODO: 準備中
     *
     * @param retrieving TODO: 準備中
     */
    void setRetrieving(int retrieving) {
        this.retrieving = retrieving;
    }

    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     */
    int getFilesUsed() {
        return filesUsed;
    }

    /**
     * TODO: 準備中
     *
     * @param filesUsed TODO: 準備中
     */
    void setFilesUsed(int filesUsed) {
        this.filesUsed = filesUsed;
    }

    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     */
    int getSize() {
        return size;
    }

    /**
     * TODO: 準備中
     *
     * @param size TODO: 準備中
     */
    void setSize(int size) {
        this.size = size;
    }

    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     */
    int getrSize() {
        return rSize;
    }

    /**
     * TODO: 準備中
     *
     * @param rSize TODO: 準備中
     */
    void setrSize(int rSize) {
        this.rSize = rSize;
    }

    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     */
    int getwSize() {
        return wSize;
    }

    /**
     * TODO: 準備中
     *
     * @param wSize TODO: 準備中
     */
    void setwSize(int wSize) {
        this.wSize = wSize;
    }

    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     */
    int getMemory() {
        return memory;
    }

    /**
     * TODO: 準備中
     *
     * @param memory TODO: 準備中
     */
    void setMemory(int memory) {
        this.memory = memory;
    }

    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     */
    int getDestinationFile() {
        return destinationFile;
    }

    /**
     * TODO: 準備中
     *
     * @param destinationFile TODO: 準備中
     */
    void setDestinationFile(int destinationFile) {
        this.destinationFile = destinationFile;
    }

    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     */
    int getRetrievalQueue() {
        return retrievalQueue;
    }

    /**
     * TODO: 準備中
     *
     * @param retrievalQueue TODO: 準備中
     */
    void setRetrievalQueue(int retrievalQueue) {
        this.retrievalQueue = retrievalQueue;
    }

    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     */
    MemoryStruct[] getQueue() {
        return queue;
    }

    /**
     * TODO: 準備中
     *
     * @param queue TODO: 準備中
     */
    void setQueue(MemoryStruct[] queue) {
        this.queue = queue;
    }

    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     */
    FileStruct[] getFile() {
        return file;
    }

    /**
     * TODO: 準備中
     *
     * @param file TODO: 準備中
     */
    void setFile(FileStruct[] file) {
        this.file = file;
    }
}
