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
package jp.osscons.opensourcecobol.libcobj.call;

import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;

/** CALLに関連するクラス。TODO: 調査中 */
public class CobolCallDataContent {
  /** TODO: 調査中 */
  public CobolDataStorage data;
  /** TODO: 調査中 */
  public long datall;
  /** TODO: 調査中 */
  public int dataint;

  /**
   * TODO: 調査中
   *
   * @param size
   */
  public CobolCallDataContent(int size) {
    this.data = new CobolDataStorage(size);
  }
}
