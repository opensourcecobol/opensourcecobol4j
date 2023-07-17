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

import java.io.IOException;
import java.lang.ProcessBuilder.Redirect;
import java.util.List;
import java.util.concurrent.TimeUnit;
import jp.osscons.opensourcecobol.libcobj.common.CobolModule;
import jp.osscons.opensourcecobol.libcobj.common.CobolUtil;
import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolRuntimeException;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolStopRunException;

public class CobolSystemRoutine {
  private static final boolean runsOnWindows = "\\".equals(System.getProperty("file.separator"));

  public static int CBL_EXIT_PROC(CobolDataStorage x, CobolDataStorage pptr) {
    // TODO 実装
    return 0;
  }

  public static int CBL_ERROR_PROC(CobolDataStorage x, CobolDataStorage pptr) {
    // TODO 実装
    return 0;
  }

  public static int SYSTEM(CobolDataStorage cmd) throws CobolStopRunException {
    int size = SYSTEM_getParameterSize();
    String cmdStr = new String(cmd.getByteArray(0, size));
    return SYSTEM_main(cmdStr);
  }

  /**
   * libcob/common.cのSYSTEMの実装
   *
   * @param cmd
   * @return
   * @throws CobolStopRunException
   */
  public static int SYSTEM(String cmd) throws CobolStopRunException {
    return SYSTEM_main(cmd);
  }

  private static int SYSTEM_getParameterSize() throws CobolStopRunException {
    CobolUtil.COB_CHK_PARMS("SYSTEM", 1);
    List<AbstractCobolField> paramaters = CobolModule.getCurrentModule().cob_procedure_parameters;
    if (!paramaters.isEmpty() && paramaters.get(0) != null) {
      AbstractCobolField paramater = paramaters.get(0);
      int size = paramater.getSize();
      if (size <= 0) {
        CobolUtil.runtimeError(
            "The size of the paramater to SYSTEM call is less than 1");
        CobolStopRunException.stopRunAndThrow(1);
      }
      return size;
    } else {
      CobolUtil.runtimeError("The size of the paramater is not specified");
      CobolStopRunException.stopRunAndThrow(1);
      // not reached
      return -1;
    }
  }

  private static int SYSTEM_main(String cmd) throws CobolStopRunException {
    ProcessBuilder pb;
    if (runsOnWindows) {
      pb = new ProcessBuilder("cmd", "/c", cmd);
    } else {
      pb = new ProcessBuilder("sh", "-c", cmd);
    }
    pb.redirectInput(Redirect.INHERIT)
        .redirectOutput(Redirect.INHERIT)
        .redirectError(Redirect.INHERIT);
    try {
      Process process = pb.start();
      return process.waitFor();
    } catch (InterruptedException | IOException e) {
      e.printStackTrace();
      return 1;
    }
  }

  /**
   * libcob/common.cのSYSTEMの実装
   *
   * @param cmd
   * @return
   * @throws CobolStopRunException
   */
  public static int SYSTEM(AbstractCobolField cmd) throws CobolStopRunException {
    return SYSTEM(cmd.getString());
  }

  private interface Calculater {
    byte calc(byte b1, byte b2);
  }

  public static int CBL_COMMON_OPERATION(
      String funcName, CobolDataStorage data1, CobolDataStorage data2, int length, Calculater c)
      throws CobolStopRunException {
    CobolUtil.COB_CHK_PARMS(funcName, 3);
    if (length <= 0) {
      return 0;
    }
    for (int n = 0; n < length; ++n) {
      byte b1 = data1.getByte(n);
      byte b2 = data2.getByte(n);
      data2.setByte(n, c.calc(b1, b2));
    }
    return 0;
  }

  /**
   * libcob/common.cのCBL_ANDの実装
   *
   * @param data1
   * @param data2
   * @param length
   * @return
   * @throws CobolStopRunException
   */
  public static int CBL_AND(CobolDataStorage data1, CobolDataStorage data2, int length)
      throws CobolStopRunException {
    return CBL_COMMON_OPERATION(
        "CBL_AND",
        data1,
        data2,
        length,
        new Calculater() {
          public byte calc(byte b1, byte b2) {
            return (byte) (b1 & b2);
          }
        });
  }

  public static int CBL_AND(AbstractCobolField data1, CobolDataStorage data2, int length)
      throws CobolStopRunException {
    return CBL_AND(data1.getDataStorage(), data2, length);
  }

  public static int CBL_AND(CobolDataStorage data1, AbstractCobolField data2, int length)
      throws CobolStopRunException {
    return CBL_AND(data1, data2.getDataStorage(), length);
  }

  public static int CBL_AND(AbstractCobolField data1, AbstractCobolField data2, int length)
      throws CobolStopRunException {
    return CBL_AND(data1.getDataStorage(), data2.getDataStorage(), length);
  }

  /**
   * libcob/common.cのCBL_ORの実装
   *
   * @param data1
   * @param data2
   * @param length
   * @return
   * @throws CobolStopRunException
   */
  public static int CBL_OR(CobolDataStorage data1, CobolDataStorage data2, int length)
      throws CobolStopRunException {
    return CBL_COMMON_OPERATION(
        "CBL_OR",
        data1,
        data2,
        length,
        new Calculater() {
          public byte calc(byte b1, byte b2) {
            return (byte) (b1 | b2);
          }
        });
  }

  public static int CBL_OR(AbstractCobolField data1, CobolDataStorage data2, int length)
      throws CobolStopRunException {
    return CBL_OR(data1.getDataStorage(), data2, length);
  }

  public static int CBL_OR(CobolDataStorage data1, AbstractCobolField data2, int length)
      throws CobolStopRunException {
    return CBL_OR(data1, data2.getDataStorage(), length);
  }

  public static int CBL_OR(AbstractCobolField data1, AbstractCobolField data2, int length)
      throws CobolStopRunException {
    return CBL_OR(data1.getDataStorage(), data2.getDataStorage(), length);
  }

  private static int byte2Int(byte x) {
    return x & 0xFF;
  }

  private static byte int2Byte(int x) {
    return (byte) (x & 0xFF);
  }

  /**
   * libcob/common.cのCBL_NORの実装
   *
   * @param data1
   * @param data2
   * @param length
   * @return
   * @throws CobolStopRunException
   */
  public static int CBL_NOR(CobolDataStorage data1, CobolDataStorage data2, int length)
      throws CobolStopRunException {
    return CBL_COMMON_OPERATION(
        "CBL_NOR",
        data1,
        data2,
        length,
        new Calculater() {
          public byte calc(byte b1, byte b2) {
            return int2Byte(~(byte2Int(b1) | int2Byte(b2)));
          }
        });
  }

  public static int CBL_NOR(AbstractCobolField data1, CobolDataStorage data2, int length)
      throws CobolStopRunException {
    return CBL_NOR(data1.getDataStorage(), data2, length);
  }

  public static int CBL_NOR(CobolDataStorage data1, AbstractCobolField data2, int length)
      throws CobolStopRunException {
    return CBL_NOR(data1, data2.getDataStorage(), length);
  }

  public static int CBL_NOR(AbstractCobolField data1, AbstractCobolField data2, int length)
      throws CobolStopRunException {
    return CBL_NOR(data1.getDataStorage(), data2.getDataStorage(), length);
  }

  /**
   * libcob/common.cのCBL_XORの実装
   *
   * @param data1
   * @param data2
   * @param length
   * @return
   * @throws CobolStopRunException
   */
  public static int CBL_XOR(CobolDataStorage data1, CobolDataStorage data2, int length)
      throws CobolStopRunException {
    return CBL_COMMON_OPERATION(
        "CBL_XOR",
        data1,
        data2,
        length,
        new Calculater() {
          public byte calc(byte b1, byte b2) {
            return (byte) (b1 ^ b2);
          }
        });
  }

  public static int CBL_XOR(AbstractCobolField data1, CobolDataStorage data2, int length)
      throws CobolStopRunException {
    return CBL_XOR(data1.getDataStorage(), data2, length);
  }

  public static int CBL_XOR(CobolDataStorage data1, AbstractCobolField data2, int length)
      throws CobolStopRunException {
    return CBL_XOR(data1, data2.getDataStorage(), length);
  }

  public static int CBL_XOR(AbstractCobolField data1, AbstractCobolField data2, int length)
      throws CobolStopRunException {
    return CBL_XOR(data1.getDataStorage(), data2.getDataStorage(), length);
  }

  /**
   * libcob/common.cのCBL_NIMPの実装
   *
   * @param data1
   * @param data2
   * @param length
   * @return
   * @throws CobolStopRunException
   */
  public static int CBL_NIMP(CobolDataStorage data1, CobolDataStorage data2, int length)
      throws CobolStopRunException {
    return CBL_COMMON_OPERATION(
        "CBL_NIMP",
        data1,
        data2,
        length,
        new Calculater() {
          public byte calc(byte b1, byte b2) {
            return (byte) (b1 & (~b2));
          }
        });
  }

  public static int CBL_NIMP(AbstractCobolField data1, CobolDataStorage data2, int length)
      throws CobolStopRunException {
    return CBL_NIMP(data1.getDataStorage(), data2, length);
  }

  public static int CBL_NIMP(CobolDataStorage data1, AbstractCobolField data2, int length)
      throws CobolStopRunException {
    return CBL_NIMP(data1, data2.getDataStorage(), length);
  }

  public static int CBL_NIMP(AbstractCobolField data1, AbstractCobolField data2, int length)
      throws CobolStopRunException {
    return CBL_NIMP(data1.getDataStorage(), data2.getDataStorage(), length);
  }

  /**
   * libcob/common.cのCBL_EQの実装
   *
   * @param data1
   * @param data2
   * @param length
   * @return
   * @throws CobolStopRunException
   */
  public static int CBL_EQ(CobolDataStorage data1, CobolDataStorage data2, int length)
      throws CobolStopRunException {
    return CBL_COMMON_OPERATION(
        "CBL_EQ",
        data1,
        data2,
        length,
        new Calculater() {
          public byte calc(byte b1, byte b2) {
            return (byte) ~(b1 ^ b2);
          }
        });
  }

  public static int CBL_EQ(AbstractCobolField data1, CobolDataStorage data2, int length)
      throws CobolStopRunException {
    return CBL_EQ(data1.getDataStorage(), data2, length);
  }

  public static int CBL_EQ(CobolDataStorage data1, AbstractCobolField data2, int length)
      throws CobolStopRunException {
    return CBL_EQ(data1, data2.getDataStorage(), length);
  }

  public static int CBL_EQ(AbstractCobolField data1, AbstractCobolField data2, int length)
      throws CobolStopRunException {
    return CBL_EQ(data1.getDataStorage(), data2.getDataStorage(), length);
  }

  /**
   * TODO libcob/common.cのCBL_NOTの実装
   *
   * @param data
   * @param length
   * @return
   * @throws CobolStopRunException
   */
  public static int CBL_NOT(CobolDataStorage data, int length) throws CobolStopRunException {
    CobolUtil.COB_CHK_PARMS("CBL_NOT", 2);
    if (length <= 0) {
      return 0;
    }
    for (int n = 0; n < length; ++n) {
      byte b = data.getByte(n);
      data.setByte(n, (byte) ~b);
    }
    return 0;
  }

  public static int CBL_NOT(AbstractCobolField data, int length) throws CobolStopRunException {
    return CBL_NOT(data.getDataStorage(), length);
  }

  /**
   * libcob/common.cのCBL_XF4の実装
   *
   * @param data1
   * @param data2
   * @return
   * @throws CobolStopRunException
   */
  public static int CBL_XF4(CobolDataStorage data1, CobolDataStorage data2)
      throws CobolStopRunException {
    CobolUtil.COB_CHK_PARMS("CBL_XF4", 2);
    for (int n = 0; n < 8; ++n) {
      byte b1 = data1.getByte(n);
      byte b2 = data2.getByte(n);
      data1.setByte(n, (byte) (b1 | (b2 & 1) << (7 - n)));
    }
    return 0;
  }

  public static int CBL_XF4(AbstractCobolField data1, CobolDataStorage data2)
      throws CobolStopRunException {
    return CBL_XF4(data1.getDataStorage(), data2);
  }

  public static int CBL_XF4(CobolDataStorage data1, AbstractCobolField data2)
      throws CobolStopRunException {
    return CBL_XF4(data1, data2.getDataStorage());
  }

  public static int CBL_XF4(AbstractCobolField data1, AbstractCobolField data2)
      throws CobolStopRunException {
    return CBL_XF4(data1.getDataStorage(), data2.getDataStorage());
  }

  /**
   * libcob/common.cのCBL_XF5の実装
   *
   * @param data1
   * @param data2
   * @return
   * @throws CobolStopRunException
   */
  public static int CBL_XF5(CobolDataStorage data1, CobolDataStorage data2)
      throws CobolStopRunException {
    CobolUtil.COB_CHK_PARMS("CBL_XF5", 2);
    byte b1 = data1.getByte(0);
    for (int n = 0; n < 8; ++n) {
      data2.setByte(n, (byte) ((b1 & (1 << (7 - n))) != 0 ? 1 : 0));
    }
    return 0;
  }

  public static int CBL_XF5(AbstractCobolField data1, CobolDataStorage data2)
      throws CobolStopRunException {
    return CBL_XF5(data1.getDataStorage(), data2);
  }

  public static int CBL_XF5(CobolDataStorage data1, AbstractCobolField data2)
      throws CobolStopRunException {
    return CBL_XF5(data1, data2.getDataStorage());
  }

  public static int CBL_XF5(AbstractCobolField data1, AbstractCobolField data2)
      throws CobolStopRunException {
    return CBL_XF5(data1.getDataStorage(), data2.getDataStorage());
  }

  /**
   * libcob/common.cのCBL_X91の実装
   *
   * @param result
   * @param func
   * @param parm
   * @return
   */
  public static int CBL_X91(CobolDataStorage result, CobolDataStorage func, CobolDataStorage parm) {
    switch (func.getByte(0)) {
      case 11:
        for (int i = 0; i < 8; ++i) {
          if (parm.getByte(i) == 0) {
            CobolUtil.cobSwitch[i] = false;
          } else if (parm.getByte(i) == 1) {
            CobolUtil.cobSwitch[i] = true;
          }
        }
        result.setByte(0, (byte) 0);
        break;
      case 12:
        for (int i = 0; i < 8; ++i) {
          parm.setByte(i, (byte) (CobolUtil.cobSwitch[i] ? 1 : 0));
        }
        result.setByte(0, (byte) 0);
        break;
      case 16:
        parm.setByte(0, (byte) CobolUtil.cobSaveCallParams);
        result.setByte(0, (byte) 0);
        break;
      default:
        result.setByte(0, (byte) 1);
        break;
    }
    return 0;
  }

  /**
   * TODO libcob/common.cのCBL_TOLOWERの実装
   *
   * @param data
   * @param length
   * @return
   * @throws CobolStopRunException
   */
  public static int CBL_TOLOWER(CobolDataStorage data, int length) throws CobolStopRunException {
    CobolUtil.COB_CHK_PARMS("CBL_TOLOWER", 2);
    if (length <= 0) {
      return 0;
    }

    for (int n = 0; n < length; ++n) {
      byte b = data.getByte(n);
      byte[] bytes = { b };
      byte result = new String(bytes).toLowerCase().getBytes()[0];
      data.setByte(n, result);
    }
    return 0;
  }

  public static int CBL_TOLOWER(AbstractCobolField data, int length) throws CobolStopRunException {
    return CBL_TOLOWER(data.getDataStorage(), length);
  }

  /**
   * TODO libcob/common.cのCBL_TOUPPERの実装
   *
   * @param data
   * @param length
   * @return
   * @throws CobolStopRunException
   */
  public static int CBL_TOUPPER(CobolDataStorage data, int length) throws CobolStopRunException {
    CobolUtil.COB_CHK_PARMS("CBL_TOUPPER", 2);
    if (length <= 0) {
      return 0;
    }

    for (int n = 0; n < length; ++n) {
      byte b = data.getByte(n);
      byte[] bytes = { b };
      byte result = new String(bytes).toUpperCase().getBytes()[0];
      data.setByte(n, result);
    }
    return 0;
  }

  public static int CBL_TOUPPER(AbstractCobolField data, int length) throws CobolStopRunException {
    return CBL_TOUPPER(data.getDataStorage(), length);
  }

  public static int CBL_OC_NANOSLEEP(CobolDataStorage data) throws CobolStopRunException {
    CobolUtil.COB_CHK_PARMS("CBL_OC_NANOSLEEP", 1);

    AbstractCobolField param = CobolModule.getCurrentModule().cob_procedure_parameters.get(0);
    if (param != null) {
      long nsecs = param.getLong();
      System.out.println(nsecs);
      if (nsecs > 0) {
        try {
          TimeUnit.NANOSECONDS.sleep(nsecs);
        } catch (InterruptedException e) {
        }
      }
    }
    return 0;
  }

  public static int CBL_OC_NANOSLEEP(AbstractCobolField field) throws CobolStopRunException {
    CobolSystemRoutine.CBL_OC_NANOSLEEP(field.getDataStorage());
    return 0;
  }
}
