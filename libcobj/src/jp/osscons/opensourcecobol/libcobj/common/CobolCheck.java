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
package jp.osscons.opensourcecobol.libcobj.common;

import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolExceptionId;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolRuntimeException;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolStopRunException;

/** TODO 暫定的に作ったクラス ここに実装されたメソッドは別の適切なクラスに実装する、または適切なクラス名に変更する */
public class CobolCheck {
  public static void checkSubscript(int i, int min, int max, String name) {
    // TODO 実装 (libcob/common.c)
  }

  public static void checkSubscript(long i, int min, int max, String name) {
    CobolCheck.checkSubscript((int) i, min, max, name);
  }

  public static void checkSubscript(long i, int min, int max, CobolDataStorage name) {
    // TODO 実装
  }

  public static void checkSubscript(CobolDataStorage i, int min, int max, String name) {
    // TODO 実装 (libcob/common.c)
  }

  /**
   * libcob/common.cのcob_check_odoの実装
   *
   * @param i
   * @param min
   * @param max
   * @param name
   * @throws CobolStopRunException
   */
  public static void checkOdo(int i, int min, int max, String name) throws CobolStopRunException {
    if (i < min || max < i) {
      CobolRuntimeException.setException(CobolExceptionId.COB_EC_BOUND_SUBSCRIPT);
      // TODO ここを正しく実装する
      System.err.println("check odo error");
      CobolStopRunException.stopRunAndThrow(1);
    }
  }
}
