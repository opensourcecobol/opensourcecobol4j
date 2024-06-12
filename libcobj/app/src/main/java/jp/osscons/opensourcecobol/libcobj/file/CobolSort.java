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
  public CobolFile getPointer() {
    return pointer;
  }

  /**
   * TODO: 準備中
   *
   * @param pointer TODO: 準備中
   */
  public void setPointer(CobolFile pointer) {
    this.pointer = pointer;
  }

  /**
   * TODO: 準備中
   *
   * @return TODO: 準備中
   */
  public CobolItem getEmpty() {
    return empty;
  }

  /**
   * TODO: 準備中
   *
   * @param empty TODO: 準備中
   */
  public void setEmpty(CobolItem empty) {
    this.empty = empty;
  }

  /**
   * TODO: 準備中
   *
   * @return TODO: 準備中
   */
  public CobolDataStorage getSortReturn() {
    return sortReturn;
  }

  /**
   * TODO: 準備中
   *
   * @param sortReturn TODO: 準備中
   */
  public void setSortReturn(CobolDataStorage sortReturn) {
    this.sortReturn = sortReturn;
  }

  /**
   * TODO: 準備中
   *
   * @return TODO: 準備中
   */
  public AbstractCobolField getFnstatus() {
    return fnstatus;
  }

  /**
   * TODO: 準備中
   *
   * @param fnstatus TODO: 準備中
   */
  public void setFnstatus(AbstractCobolField fnstatus) {
    this.fnstatus = fnstatus;
  }

  /**
   * TODO: 準備中
   *
   * @return TODO: 準備中
   */
  public int getUnique() {
    return unique;
  }

  /**
   * TODO: 準備中
   *
   * @param unique TODO: 準備中
   */
  public void setUnique(int unique) {
    this.unique = unique;
  }

  /**
   * TODO: 準備中
   *
   * @return TODO: 準備中
   */
  public int getRetrieving() {
    return retrieving;
  }

  /**
   * TODO: 準備中
   *
   * @param retrieving TODO: 準備中
   */
  public void setRetrieving(int retrieving) {
    this.retrieving = retrieving;
  }

  /**
   * TODO: 準備中
   *
   * @return TODO: 準備中
   */
  public int getFilesUsed() {
    return filesUsed;
  }

  /**
   * TODO: 準備中
   *
   * @param filesUsed TODO: 準備中
   */
  public void setFilesUsed(int filesUsed) {
    this.filesUsed = filesUsed;
  }

  /**
   * TODO: 準備中
   *
   * @return TODO: 準備中
   */
  public int getSize() {
    return size;
  }

  /**
   * TODO: 準備中
   *
   * @param size TODO: 準備中
   */
  public void setSize(int size) {
    this.size = size;
  }

  /**
   * TODO: 準備中
   *
   * @return TODO: 準備中
   */
  public int getrSize() {
    return rSize;
  }

  /**
   * TODO: 準備中
   *
   * @param rSize TODO: 準備中
   */
  public void setrSize(int rSize) {
    this.rSize = rSize;
  }

  /**
   * TODO: 準備中
   *
   * @return TODO: 準備中
   */
  public int getwSize() {
    return wSize;
  }

  /**
   * TODO: 準備中
   *
   * @param wSize TODO: 準備中
   */
  public void setwSize(int wSize) {
    this.wSize = wSize;
  }

  /**
   * TODO: 準備中
   *
   * @return TODO: 準備中
   */
  public int getMemory() {
    return memory;
  }

  /**
   * TODO: 準備中
   *
   * @param memory TODO: 準備中
   */
  public void setMemory(int memory) {
    this.memory = memory;
  }

  /**
   * TODO: 準備中
   *
   * @return TODO: 準備中
   */
  public int getDestinationFile() {
    return destinationFile;
  }

  /**
   * TODO: 準備中
   *
   * @param destinationFile TODO: 準備中
   */
  public void setDestinationFile(int destinationFile) {
    this.destinationFile = destinationFile;
  }

  /**
   * TODO: 準備中
   *
   * @return TODO: 準備中
   */
  public int getRetrievalQueue() {
    return retrievalQueue;
  }

  /**
   * TODO: 準備中
   *
   * @param retrievalQueue TODO: 準備中
   */
  public void setRetrievalQueue(int retrievalQueue) {
    this.retrievalQueue = retrievalQueue;
  }

  /**
   * TODO: 準備中
   *
   * @return TODO: 準備中
   */
  public MemoryStruct[] getQueue() {
    return queue;
  }

  /**
   * TODO: 準備中
   *
   * @param queue TODO: 準備中
   */
  public void setQueue(MemoryStruct[] queue) {
    this.queue = queue;
  }

  /**
   * TODO: 準備中
   *
   * @return TODO: 準備中
   */
  public FileStruct[] getFile() {
    return file;
  }

  /**
   * TODO: 準備中
   *
   * @param file TODO: 準備中
   */
  public void setFile(FileStruct[] file) {
    this.file = file;
  }
}
