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

/** TODO: 調査中 */
public class CobolItem {
  private CobolItem next;
  private int endOfBlock;
  private int recordSize;
  private byte blockByte = 0;
  private CobolDataStorage unique = new CobolDataStorage(new byte[8]);
  private CobolDataStorage item;

  /** TODO: 調査中 */
  public CobolItem() {
    this.next = null;
    this.endOfBlock = 0;
  }

  /**
   * TODO: 調査中
   *
   * @return TODO: 調査中
   */
  public CobolItem getNext() {
    return next;
  }

  /**
   * TODO: 調査中
   *
   * @param next TODO: 調査中
   */
  public void setNext(CobolItem next) {
    this.next = next;
  }

  /**
   * TODO: 調査中
   *
   * @return TODO: 調査中
   */
  public int getEndOfBlock() {
    return endOfBlock;
  }

  /**
   * TODO: 調査中
   *
   * @param endOfBlock TODO: 調査中
   */
  public void setEndOfBlock(int endOfBlock) {
    this.endOfBlock = endOfBlock;
  }

  /**
   * TODO: 調査中
   *
   * @return TODO: 調査中
   */
  public int getRecordSize() {
    return recordSize;
  }

  /**
   * TODO: 調査中
   *
   * @param recordSize TODO: 調査中
   */
  public void setRecordSize(int recordSize) {
    this.recordSize = recordSize;
  }

  /**
   * TODO: 調査中
   *
   * @return TODO: 調査中
   */
  public byte getBlockByte() {
    return blockByte;
  }

  /**
   * TODO: 調査中
   *
   * @param blockByte TODO: 調査中
   */
  public void setBlockByte(byte blockByte) {
    this.blockByte = blockByte;
  }

  /**
   * TODO: 調査中
   *
   * @return TODO: 調査中
   */
  public CobolDataStorage getUnique() {
    return unique;
  }

  /**
   * TODO: 調査中
   *
   * @param unique TODO: 調査中
   */
  public void setUnique(CobolDataStorage unique) {
    this.unique = unique;
  }

  /**
   * TODO: 調査中
   *
   * @return TODO: 調査中
   */
  public CobolDataStorage getItem() {
    return item;
  }

  /**
   * TODO: 調査中
   *
   * @param item TODO: 調査中
   */
  public void setItem(CobolDataStorage item) {
    this.item = item;
  }
}
