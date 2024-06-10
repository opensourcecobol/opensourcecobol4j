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

import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldAttribute;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldFactory;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolExceptionId;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolRuntimeException;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolStopRunException;

public class CobolInspect {
  private static final int INSPECT_ALL = 0;
  private static final int INSPECT_LEADING = 1;
  private static final int INSPECT_FIRST = 2;
  private static final int INSPECT_TRAILING = 3;

  private static AbstractCobolField inspectVar;
  private static CobolDataStorage inspectData;
  private static int inspectStart;
  private static int inspectEnd;
  private static int[] inspectMark = null;
  private static int lastsize = 0;
  private static int inspectSize;
  private static int inspectReplacing;
  private static int inspectSign;
  private static AbstractCobolField inspectVarCopy;

  private static AbstractCobolField figurative(AbstractCobolField f1, AbstractCobolField f2) {
    int size1 = 0;
    int size2 = f2.getSize();
    byte[] figptr = new byte[size2];
    int s = 0;
    for (int n = 0; n < size2; n++, s++) {
      figptr[s] = f1.getDataStorage().getByte(size1);
      size1++;
      if (size1 >= f1.getSize()) {
        size1 = 0;
      }
    }
    return CobolFieldFactory.makeCobolField(
        size2,
        new CobolDataStorage(figptr),
        new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null));
  }

  private static void common(AbstractCobolField f1, AbstractCobolField f2, int type)
      throws CobolStopRunException {
    if (f1 == null) {
      f1 = CobolConstant.low;
    }
    if (f2 == null) {
      f2 = CobolConstant.low;
    }

    int type1 = f1.getAttribute().getType();
    int type2 = f2.getAttribute().getType();

    if (type2 == CobolFieldAttribute.COB_TYPE_NATIONAL
        || type2 == CobolFieldAttribute.COB_TYPE_NATIONAL_EDITED) {
      if (f1 == CobolConstant.quote) {
        f1 = CobolConstant.zenQuote;
      } else if (f1 == CobolConstant.space) {
        f1 = CobolConstant.zenSpace;
      } else if (f1 == CobolConstant.zero) {
        f1 = CobolConstant.zenZero;
      }
    }
    if (type1 == CobolFieldAttribute.COB_TYPE_NATIONAL
        || type1 == CobolFieldAttribute.COB_TYPE_NATIONAL_EDITED) {
      if (f2 == CobolConstant.quote) {
        f2 = CobolConstant.zenQuote;
      } else if (f2 == CobolConstant.space) {
        f2 = CobolConstant.zenSpace;
      } else if (f2 == CobolConstant.zero) {
        f2 = CobolConstant.zenZero;
      }
    }
    if (inspectReplacing != 0 && f1.getSize() != f2.getSize()) {
      if (type1 == CobolFieldAttribute.COB_TYPE_ALPHANUMERIC_ALL) {
        f1 = figurative(f1, f2);
      } else {
        CobolRuntimeException.setException(CobolExceptionId.COB_EC_RANGE_INSPECT_SIZE);
        return;
      }
    }

    int mark = inspectStart;
    int len = inspectEnd - inspectStart;
    int n = 0;
    if (type == INSPECT_TRAILING) {
      for (int i = len - f2.getSize(); i >= 0; i--) {
        if (inspectData.memcmp(inspectStart + i, f2.getDataStorage(), f2.getSize()) == 0) {
          int j;
          for (j = 0; j < f2.getSize(); ++j) {
            if (inspectMark[mark + i + j] != -1) {
              break;
            }
          }
          if (j == f2.getSize()) {
            CobolDataStorage f1Storage = f1.getDataStorage();
            for (j = 0; j < f2.getSize(); ++j) {
              inspectMark[mark + i + j] = inspectReplacing != 0 ? f1Storage.getByte(j) : 1;
            }
            i -= f2.getSize() - 1;
            n++;
          }
        } else {
          break;
        }
      }
    } else {
      for (int i = 0; i < (len - f2.getSize() + 1); ++i) {
        if (inspectData.memcmp(inspectStart + i, f2.getDataStorage(), f2.getSize()) == 0) {
          int j;
          for (j = 0; j < f2.getSize(); ++j) {
            if (inspectMark[mark + i + j] != -1) {
              break;
            }
          }
          if (j == f2.getSize()) {
            CobolDataStorage f1Storage = f1.getDataStorage();
            for (j = 0; j < f2.getSize(); ++j) {
              inspectMark[mark + i + j] = inspectReplacing != 0 ? f1Storage.getByte(j) : 1;
            }
            i += f2.getSize() - 1;
            n++;
            if (type == INSPECT_FIRST) {
              break;
            }
          }
        } else if (type == INSPECT_LEADING) {
          break;
        }
      }
    }

    if (n > 0 && inspectReplacing == 0) {
      f1.addInt(n);
    }
  }

  /**
   * libcob/strings.cのcob_inspect_initの実装
   *
   * @param var TODO: 調査中
   * @param replacing TODO: 調査中
   */
  public static void init(AbstractCobolField var, int replacing) {
    CobolInspect.inspectVarCopy = var;
    CobolInspect.inspectVar = CobolInspect.inspectVarCopy;
    if (inspectVar.getAttribute().isTypeNumericDisplay()) {
      inspectVar.putSign(1);
    }
    CobolInspect.inspectReplacing = replacing;
    CobolInspect.inspectSign = var.getSign();
    CobolInspect.inspectSize = var.getFieldSize();
    CobolInspect.inspectData = var.getDataStorage();
    CobolInspect.inspectStart = -1;
    CobolInspect.inspectEnd = -1;
    if (inspectSize > lastsize) {
      inspectMark = new int[inspectSize];
      lastsize = inspectSize;
    }
    for (int i = 0; i < inspectSize; ++i) {
      inspectMark[i] = -1;
    }
    CobolRuntimeException.setException(0);
  }

  /** libcob/strings.cのcob_inspect_startの実装 */
  public static void start() {
    inspectStart = 0;
    inspectEnd = inspectSize;
  }

  /**
   * libcob/strings.cのcob_inspect_beforeの実装
   *
   * @param str TODO: 調査中
   */
  public static void before(AbstractCobolField str) {
    CobolDataStorage p2 = null;
    int fig = 0;

    switch (str.getAttribute().getType()) {
      case CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY:
        CobolDataStorage data = str.getDataStorage();
        int firstIndex = str.getFirstDataIndex();
        int size = str.getFieldSize();
        int n = 0;
        int i = 0;
        while (size > 1
            && (data.getByte(firstIndex + i) == (byte) '0'
                || data.getByte(firstIndex + i) == (byte) 0x70)) {
          size--;
          i++;
        }
        while (size-- > 0) {
          int b = data.getByte(firstIndex + i++);
          n = n * 10 + (b >= 0x70 ? b - 0x70 : b - 0x30);
          fig++;
        }
        p2 = new CobolDataStorage(String.format("%d", n).getBytes());
        break;
      default:
        fig = str.getSize();
        p2 = str.getDataStorage();
        break;
    }

    for (int p = inspectStart; p < inspectEnd - fig + 1; ++p) {
      if (inspectData.getSubDataStorage(p).memcmp(p2, fig) == 0) {
        inspectEnd = p;
        break;
      }
    }
  }

  /**
   * libcob/strings.cのcob_inspect_afterの実装
   *
   * @param str TODO: 調査中
   */
  public static void after(AbstractCobolField str) {
    CobolDataStorage data = str.getDataStorage();
    int size = str.getSize();
    for (int p = inspectStart; p < inspectEnd - str.getSize() + 1; ++p) {
      if (inspectData.getSubDataStorage(p).memcmp(data, size) == 0) {
        inspectStart = p + size;
        return;
      }
    }
    inspectStart = inspectEnd;
  }

  /**
   * libcob/strings.cのcob_inspect_charactersの実装
   *
   * @param f1 TODO: 調査中
   * @throws CobolStopRunException TODO: 調査中
   */
  public static void characters(AbstractCobolField f1) throws CobolStopRunException {
    int mark = inspectStart;
    int len = inspectEnd - inspectStart;
    if (inspectReplacing != 0) {
      for (int i = 0; i < len; ++i) {
        if (inspectMark[mark + i] == -1) {
          CobolDataStorage data = f1.getDataStorage();
          for (int j = 0; j < f1.getSize(); ++j) {
            inspectMark[mark + i + j] = data.getByte(j);
          }
          i += f1.getSize() - 1;
        }
      }
    } else {
      int n = 0;
      for (int i = 0; i < len; ++i) {
        if (inspectMark[mark + i] == -1) {
          inspectMark[mark + i] = 1;
          n++;
        }
      }
      if (n > 0) {
        int type = inspectVar.getAttribute().getType();
        if (type == CobolFieldAttribute.COB_TYPE_NATIONAL
            || type == CobolFieldAttribute.COB_TYPE_NATIONAL_EDITED) {
          n = n / 2;
        }
        f1.addInt(n);
      }
    }
  }

  /**
   * libcob/strings.cのcob_inspect_allの実装
   *
   * @param f1 TODO: 調査中
   * @param f2 TODO: 調査中
   * @throws CobolStopRunException TODO: 調査中
   */
  public static void all(AbstractCobolField f1, AbstractCobolField f2)
      throws CobolStopRunException {
    common(f1, f2, INSPECT_ALL);
  }

  /**
   * libcob/strings.cのcob_inspect_leadingの実装
   *
   * @param f1 TODO: 調査中
   * @param f2 TODO: 調査中
   * @throws CobolStopRunException TODO: 調査中
   */
  public static void leading(AbstractCobolField f1, AbstractCobolField f2)
      throws CobolStopRunException {
    common(f1, f2, INSPECT_LEADING);
  }

  /**
   * libcob/strings.cのcob_inspect_firstの実装
   *
   * @param f1 TODO: 調査中
   * @param f2 TODO: 調査中
   * @throws CobolStopRunException TODO: 調査中
   */
  public static void first(AbstractCobolField f1, AbstractCobolField f2)
      throws CobolStopRunException {
    common(f1, f2, INSPECT_FIRST);
  }

  /**
   * libcob/strings.cのcob_inspect_trailingの実装
   *
   * @param f1 TODO: 調査中
   * @param f2 TODO: 調査中
   * @throws CobolStopRunException TODO: 調査中
   */
  public static void trailing(AbstractCobolField f1, AbstractCobolField f2)
      throws CobolStopRunException {
    common(f1, f2, INSPECT_TRAILING);
  }

  /**
   * libcob/strings.cのcob_inspect_convertingの実装
   *
   * @param f1 TODO: 調査中
   * @param f2 TODO: 調査中
   */
  public static void converting(AbstractCobolField f1, AbstractCobolField f2) {
    int type1 = f1.getAttribute().getType();
    int len = inspectEnd - inspectStart;
    CobolDataStorage data = f2.getDataStorage();
    if (type1 == CobolFieldAttribute.COB_TYPE_NATIONAL
        || type1 == CobolFieldAttribute.COB_TYPE_NATIONAL_EDITED) {
      if (f2 == CobolConstant.quote) {
        f2 = CobolConstant.zenQuote;
      } else if (f2 == CobolConstant.space) {
        f2 = CobolConstant.zenSpace;
      } else if (f2 == CobolConstant.zero) {
        f2 = CobolConstant.zenZero;
      }
      for (int j = 0; j < f1.getSize(); j += 2) {
        for (int i = 0; i < len; i += 2) {
          if (inspectMark[i] == -1
              && inspectMark[i + 1] == -1
              && f1.getDataStorage()
                      .getSubDataStorage(j)
                      .memcmp(inspectData.getSubDataStorage(inspectStart + i), 2)
                  == 0) {
            inspectData.setByte(inspectStart + i, data.getByte(0));
            inspectData.setByte(inspectStart + i + 1, data.getByte(1));
          }
          inspectMark[i] = 1;
          inspectMark[i + 1] = 1;
        }
      }
    } else {
      for (int j = 0; j < f1.getSize(); ++j) {
        for (int i = 0; i < len; ++i) {
          if (inspectMark[i] == -1
              && inspectData.getByte(inspectStart + i) == f1.getDataStorage().getByte(j)) {
            if (f2 == CobolConstant.quote
                || f2 == CobolConstant.space
                || f2 == CobolConstant.zero) {
              inspectData.setByte(inspectStart + i, data.getByte(0));
            } else {
              inspectData.setByte(inspectStart + i, data.getByte(j));
            }
            inspectMark[i] = 1;
          }
        }
      }
    }
  }

  /** libcob/strings.cのcob_inspect_finishの実装 */
  public static void finish() {
    if (inspectReplacing != 0) {
      for (int i = 0; i < inspectSize; ++i) {
        if (inspectMark[i] != -1) {
          inspectData.setByte(i, (byte) inspectMark[i]);
        }
      }
    }
    inspectVar.putSign(inspectSign);
  }

  public static void initString() {
    CobolInspect.inspectMark = new int[CobolConstant.COB_MEDIUM_BUFF];
    lastsize = CobolConstant.COB_MEDIUM_BUFF;
  }
}
