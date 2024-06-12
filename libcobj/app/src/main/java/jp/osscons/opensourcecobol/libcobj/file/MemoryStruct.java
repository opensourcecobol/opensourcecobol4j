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

/** TODO: 準備中 */
public class MemoryStruct {
  private CobolItem first;
  private CobolItem last;
  private int count;

  /** TODO: 準備中 */
  public MemoryStruct() {
    this.first = null;
    this.last = null;
  }

  /**
   * TODO: 準備中
   *
   * @return TODO: 準備中
   */
  public CobolItem getFirst() {
    return first;
  }

  /**
   * TODO: 準備中
   *
   * @param first TODO: 準備中
   */
  public void setFirst(CobolItem first) {
    this.first = first;
  }

  /**
   * TODO: 準備中
   *
   * @return TODO: 準備中
   */
  public CobolItem getLast() {
    return last;
  }

  /**
   * TODO: 準備中
   *
   * @param last TODO: 準備中
   */
  public void setLast(CobolItem last) {
    this.last = last;
  }

  /**
   * TODO: 準備中
   *
   * @return TODO: 準備中
   */
  public int getCount() {
    return count;
  }

  /**
   * TODO: 準備中
   *
   * @param count TODO: 準備中
   */
  public void setCount(int count) {
    this.count = count;
  }
}
