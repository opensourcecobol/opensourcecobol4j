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

import java.math.BigDecimal;
import java.text.DateFormat;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Locale;
import java.util.Random;
import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.data.CobolDecimal;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldAttribute;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldFactory;
import jp.osscons.opensourcecobol.libcobj.data.CobolNationalField;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolExceptionId;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolRuntimeException;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolStopRunException;
import jp.osscons.opensourcecobol.libcobj.file.CobolFile;

public class CobolIntrinsic {

  private static int[] normalDays = {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365};
  private static int[] leapDays = {0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366};
  private static int[] normalMonthDays = {0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
  private static int[] leapMonthDays = {0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
  private static final int DEPTH_LEVEL = 8;
  private static final int sizeOfDouble = 8;
  private static int currEntry = 0;
  private static AbstractCobolField currField = null;
  private static AbstractCobolField[] calcField = new AbstractCobolField[DEPTH_LEVEL];
  private static Random random = new Random();
  private static byte[] localeBuff;
  private static final byte[] byteArray00 = "00".getBytes();

  /** libcob/intrinsicのmake_double_entryの実装 */
  private static void makeDoubleEntry() {
    CobolDataStorage s = new CobolDataStorage(sizeOfDouble + 1);

    CobolFieldAttribute newAttr =
        new CobolFieldAttribute(
            CobolFieldAttribute.COB_TYPE_NUMERIC_DOUBLE,
            18,
            9,
            CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
            null);
    AbstractCobolField newField = CobolFieldFactory.makeCobolField(sizeOfDouble, s, newAttr);

    calcField[currEntry] = newField;
    currField = newField;
    ++currEntry;
    if (currEntry >= DEPTH_LEVEL) {
      currEntry = 0;
    }
  }

  /** libcob/intrinsicのmake_field_entryの実装 */
  private static void makeFieldEntry(AbstractCobolField f) {
    AbstractCobolField newField =
        CobolFieldFactory.makeCobolField(
            f.getSize(), new CobolDataStorage(f.getSize() + 1), f.getAttribute());
    calcField[currEntry] = newField;
    currField = calcField[currEntry];

    ++currEntry;
    if (currEntry >= DEPTH_LEVEL) {
      currEntry = 0;
    }
  }

  /**
   * libcob/intrinsicのcob_intr_ordの実装
   *
   * @param year
   * @return
   */
  private static boolean isLeapYear(int year) {
    return ((year % 4 == 0 && year % 100 != 0) || (year % 400 == 0));
  }

  /** libcob/intrinsicのcob_init_intrinsicの実装 */
  public static void init() {
    CobolFieldAttribute attr =
        new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null);
    for (int i = 0; i < DEPTH_LEVEL; ++i) {
      calcField[i] = CobolFieldFactory.makeCobolField(256, new CobolDataStorage(256), attr);
    }
  }

  /** libcob/intrinsicのcob_intr_get_doubleの実装 */
  private static double intrGetDouble(CobolDecimal d) {
    double v = d.getValue().doubleValue();
    int n = d.getScale();
    for (int i = 0; i < Math.abs(n); ++i) {
      if (n > 0) {
        v /= 10;
      } else {
        v *= 10;
      }
    }
    return v;
  }

  /**
   * libcob/intrinsicのcalc_ref_modの実装
   *
   * @param f
   * @param offset
   * @param length
   */
  private static void calcRefMod(AbstractCobolField f, int offset, int length) {
    if (offset <= f.getSize()) {
      int calcoff = offset - 1;
      int size = f.getSize() - calcoff;
      if (length > 0 && length < size) {
        size = length;
      }
      f.setSize(size);
      if (calcoff > 0) {
        CobolDataStorage tmp = new CobolDataStorage(size);
        tmp.memcpy(f.getDataStorage().getSubDataStorage(calcoff), size);
        f.getDataStorage().memcpy(tmp, size);
      }
    }
  }

  /**
   * libcob/intrinsicのcob_intr_get_binopの実装
   *
   * @param f1
   * @param op
   * @param f2
   * @return
   * @throws CobolStopRunException
   */
  public static AbstractCobolField intrBinop(AbstractCobolField f1, int op, AbstractCobolField f2)
      throws CobolStopRunException {
    CobolDecimal d1 = new CobolDecimal();
    CobolDecimal d2 = new CobolDecimal();
    d1.setField(f1);
    d2.setField(f2);

    switch ((char) op) {
      case '+':
        d1.add(d2);
        break;
      case '-':
        d1.sub(d2);
        break;
      case '*':
        d1.mul(d2);
        break;
      case '/':
        d1.div(d2);
        break;
      case '^':
        d1.pow(d2);
        break;
      default:
        break;
    }

    int attrsign = 0;
    if (d1.getValue().signum() < 0) {
      attrsign = CobolFieldAttribute.COB_FLAG_HAVE_SIGN;
    } else {
      attrsign = 0;
    }

    int size = sizeInBase10(d1.getValue());
    if (d1.getScale() > size) {
      size = d1.getScale();
    }
    CobolFieldAttribute attr =
        new CobolFieldAttribute(
            CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY, size, d1.getScale(), attrsign, null);
    AbstractCobolField field =
        CobolFieldFactory.makeCobolField(size, (CobolDataStorage) null, attr);
    makeFieldEntry(field);
    d1.getDisplayField(currField, 0);
    return currField;
  }

  /**
   * libcob/intrinsicのcob_intr_lengthの実装
   *
   * @param srcfield
   * @return
   */
  public static AbstractCobolField funcLength(AbstractCobolField srcfield) {
    CobolFieldAttribute attr =
        new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY, 8, 0, 0, null);
    AbstractCobolField field = CobolFieldFactory.makeCobolField(4, (CobolDataStorage) null, attr);
    makeFieldEntry(field);
    currField.setInt(srcfield.getSize());
    return currField;
  }

  /**
   * libcob/intrinsicのcob_intr_integerの実装
   *
   * @param srcfield
   * @return
   */
  public static AbstractCobolField funcInteger(AbstractCobolField srcfield) {
    CobolFieldAttribute attr =
        new CobolFieldAttribute(
            CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY,
            18,
            0,
            CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
            null);
    AbstractCobolField field = CobolFieldFactory.makeCobolField(8, (CobolDataStorage) null, attr);
    makeFieldEntry(field);

    CobolDecimal d1 = new CobolDecimal();
    d1.setField(srcfield);
    if (d1.getValue().signum() >= 0) {
      try {
        d1.getField(currField, 0);
      } catch (CobolStopRunException e) {
        return null;
      }
      return currField;
    }

    boolean isScalePositive = d1.getScale() > 0;
    BigDecimal val = d1.getValue();
    for (int i = 0; i < Math.abs(d1.getScale()); ++i) {
      if (isScalePositive) {
        val = val.divide(BigDecimal.TEN);
      } else {
        val = val.multiply(BigDecimal.TEN);
      }
    }

    // Rouding to negative infinity
    BigDecimal[] vals = val.divideAndRemainder(BigDecimal.ONE);
    if (vals[1].signum() != 0) {
      vals[0] = vals[0].subtract(BigDecimal.ONE);
    }

    try {
      new CobolDecimal(vals[0], 0).getField(currField, 0);
    } catch (CobolStopRunException e) {
      return null;
    }
    return currField;
  }

  /**
   * libcob/intrinsicのcob_intr_integer_partの実装
   *
   * @param srcfield
   * @return
   */
  public static AbstractCobolField funcIntegerPart(AbstractCobolField srcfield) {
    CobolFieldAttribute attr =
        new CobolFieldAttribute(
            CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY,
            18,
            0,
            CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
            null);
    AbstractCobolField field = CobolFieldFactory.makeCobolField(8, (CobolDataStorage) null, attr);

    makeFieldEntry(field);
    currField.moveFrom(srcfield);
    return currField;
  }

  /**
   * libcob/intrinsicのcob_intr_upper_caseの実装
   *
   * @param srcfield
   * @return
   */
  public static AbstractCobolField funcUpperCase(
      int offset, int length, AbstractCobolField srcfield) {
    makeFieldEntry(srcfield);
    int size = srcfield.getSize();
    CobolDataStorage currStorage = currField.getDataStorage();
    CobolDataStorage srcStorage = srcfield.getDataStorage();
    for (int i = 0; i < size; ++i) {
      currStorage.setByte(i, (byte) Character.toUpperCase(srcStorage.getByte(i)));
    }
    if (offset > 0) {
      calcRefMod(currField, offset, length);
    }
    return currField;
  }

  /**
   * libcob/intrinsicのcob_intr_lower_caseの実装
   *
   * @param srcfield
   * @return
   */
  public static AbstractCobolField funcLowerCase(
      int offset, int length, AbstractCobolField srcfield) {
    makeFieldEntry(srcfield);
    int size = srcfield.getSize();
    CobolDataStorage currStorage = currField.getDataStorage();
    CobolDataStorage srcStorage = srcfield.getDataStorage();
    for (int i = 0; i < size; ++i) {
      currStorage.setByte(i, (byte) Character.toLowerCase(srcStorage.getByte(i)));
    }
    if (offset > 0) {
      calcRefMod(currField, offset, length);
    }
    return currField;
  }

  /**
   * libcob/intrinsicのcob_intr_reverseの実装
   *
   * @param srcfield
   * @return
   */
  public static AbstractCobolField funcReverse(
      int offset, int length, AbstractCobolField srcfield) {
    makeFieldEntry(srcfield);
    int size = srcfield.getSize();
    CobolDataStorage currStorage = currField.getDataStorage();
    CobolDataStorage srcStorage = srcfield.getDataStorage();
    for (int i = 0; i < size; ++i) {
      currStorage.setByte(i, srcStorage.getByte(srcfield.getSize() - i - 1));
    }
    if (offset > 0) {
      calcRefMod(currField, offset, length);
    }
    return currField;
  }

  /**
   * libcob/intrinsicのcob_intr_when_compiledの実装
   *
   * @param offset
   * @param length
   * @param f
   * @return
   */
  public static AbstractCobolField funcWhenCompiled(int offset, int length, AbstractCobolField f) {
    makeFieldEntry(f);
    currField.getDataStorage().memcpy(f.getDataStorage(), f.getSize());
    if (offset > 0) {
      calcRefMod(currField, offset, length);
    }
    return currField;
  }

  /**
   * libcob/intrinsicのcob_intr_current_dateの実装
   *
   * @param offset
   * @param length
   * @return
   */
  public static AbstractCobolField funcCurrentDate(int offset, int length) {
    CobolFieldAttribute attr =
        new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null);
    AbstractCobolField field = CobolFieldFactory.makeCobolField(21, (CobolDataStorage) null, attr);
    makeFieldEntry(field);
    // TODO Time Zoneを表示する機能を取り入れる

    String dateString =
        String.format(
            "%4d%02d%02d%02d%02d%02d%02d00000",
            CobolUtil.cal.get(Calendar.YEAR),
            CobolUtil.cal.get(Calendar.MONTH) + 1,
            CobolUtil.cal.get(Calendar.DAY_OF_MONTH),
            CobolUtil.cal.get(Calendar.HOUR_OF_DAY),
            CobolUtil.cal.get(Calendar.MINUTE),
            CobolUtil.cal.get(Calendar.SECOND),
            CobolUtil.cal.get(Calendar.MILLISECOND) / 10);
    currField.getDataStorage().memcpy(dateString.getBytes());

    if (offset > 0) {
      calcRefMod(currField, offset, length);
    }
    return currField;
  }

  /**
   * libcob/intrinsicのcob_intr_charの実装
   *
   * @param srcfield
   * @return
   */
  public static AbstractCobolField funcChar(AbstractCobolField srcfield) {
    CobolFieldAttribute attr =
        new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null);
    AbstractCobolField field = CobolFieldFactory.makeCobolField(1, (CobolDataStorage) null, attr);
    makeFieldEntry(field);

    int i = srcfield.getInt();
    if (i < 1 || i > 256) {
      currField.getDataStorage().setByte(0, (byte) 0);
    } else {
      currField.getDataStorage().setByte(0, (byte) (i - 1));
    }
    return currField;
  }

  /**
   * libcob/intrinsicのcob_intr_ordの実装
   *
   * @param srcfield
   * @return
   */
  public static AbstractCobolField funcOrd(AbstractCobolField srcfield) {
    CobolFieldAttribute attr =
        new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY, 8, 0, 0, null);
    AbstractCobolField field = CobolFieldFactory.makeCobolField(4, (CobolDataStorage) null, attr);
    makeFieldEntry(field);

    currField.setInt(srcfield.getDataStorage().getByte(0) + 1);
    return currField;
  }

  /**
   * libcob/intrinsicのcob_intr_date_of_integerの実装
   *
   * @param srcdays
   * @return
   */
  public static AbstractCobolField funcDateOfInteger(AbstractCobolField srcdays) {
    CobolFieldAttribute attr =
        new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY, 8, 0, 0, null);
    AbstractCobolField field = CobolFieldFactory.makeCobolField(8, (CobolDataStorage) null, attr);
    makeFieldEntry(field);

    CobolRuntimeException.setException(0);
    int days = srcdays.getInt();

    if (days < 1 || days > 3067671) {
      CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
      currField.getDataStorage().memset((byte) '0', 8);
      return currField;
    }

    int leapyear = 365;
    int baseyear = 1601;
    while (days > leapyear) {
      days -= leapyear;
      ++baseyear;
      if (isLeapYear(baseyear)) {
        leapyear = 366;
      } else {
        leapyear = 365;
      }
    }
    int i;
    for (i = 0; i < 13; ++i) {
      if (isLeapYear(baseyear)) {
        if (days <= leapDays[i]) {
          days -= leapDays[i - 1];
          break;
        }
      } else {
        if (days <= normalDays[i]) {
          days -= normalDays[i - 1];
          break;
        }
      }
    }
    String dateString = String.format("%04d%02d%02d", baseyear, i, days);
    currField.getDataStorage().memcpy(dateString.getBytes());
    return currField;
  }

  /**
   * libcob/intrinsicのcob_intr_day_of_integerの実装
   *
   * @param srcdays
   * @return
   */
  public static AbstractCobolField funcDayOfInteger(AbstractCobolField srcdays) {
    CobolFieldAttribute attr =
        new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY, 7, 0, 0, null);
    AbstractCobolField field = CobolFieldFactory.makeCobolField(7, (CobolDataStorage) null, attr);
    makeFieldEntry(field);

    CobolRuntimeException.setException(0);
    int days = srcdays.getInt();

    if (days < 1 || days > 3067671) {
      CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
      currField.getDataStorage().memset((byte) '0', 8);
      return currField;
    }

    int leapyear = 365;
    int baseyear = 1601;
    while (days > leapyear) {
      days -= leapyear;
      ++baseyear;
      if (isLeapYear((baseyear))) {
        leapyear = 366;
      } else {
        leapyear = 365;
      }
    }
    String dateString = String.format("%04d%03d", baseyear, days);
    currField.getDataStorage().memcpy(dateString.getBytes());
    return currField;
  }

  /**
   * libcob/intrinsicのcob_intr_integer_of_dateの実装
   *
   * @param srcfield
   * @return
   */
  public static AbstractCobolField funcIntegerOfDate(AbstractCobolField srcfield) {
    CobolFieldAttribute attr =
        new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY, 8, 0, 0, null);
    AbstractCobolField field = CobolFieldFactory.makeCobolField(4, (CobolDataStorage) null, attr);
    makeFieldEntry(field);

    CobolRuntimeException.setException(0);
    int indate = srcfield.getInt();
    int year = indate / 10000;
    if (year < 1601 || year > 9999) {
      CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
      currField.setInt(0);
      return currField;
    }
    indate %= 10000;
    int month = indate / 100;
    if (month < 1 || month > 12) {
      CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
      currField.setInt(0);
      return currField;
    }
    int days = indate % 100;
    if (days < 1 || days > 31) {
      CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
      currField.setInt(0);
      return currField;
    }
    if (isLeapYear(year)) {
      if (days > leapMonthDays[month]) {
        CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
        currField.setInt(0);
        return currField;
      }
    } else {
      if (days > normalMonthDays[month]) {
        CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
        currField.setInt(0);
        return currField;
      }
    }

    int totaldays = 0;
    int baseyear = 1601;
    while (baseyear != year) {
      if (isLeapYear(baseyear)) {
        totaldays += 366;
      } else {
        totaldays += 365;
      }
      ++baseyear;
    }

    if (isLeapYear(baseyear)) {
      totaldays += leapDays[month - 1];
    } else {
      totaldays += normalDays[month - 1];
    }

    totaldays += days;
    currField.setInt(totaldays);
    return currField;
  }

  /**
   * libcob/intrinsicのcob_intr_integer_of_dayの実装
   *
   * @param srcfield
   * @return
   */
  public static AbstractCobolField funcIntegerOfDay(AbstractCobolField srcfield) {
    CobolFieldAttribute attr =
        new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY, 8, 0, 0, null);
    AbstractCobolField field = CobolFieldFactory.makeCobolField(4, (CobolDataStorage) null, attr);
    makeFieldEntry(field);

    CobolRuntimeException.setException(0);
    int indate = srcfield.getInt();
    int year = indate / 1000;
    if (year < 1601 || year > 9999) {
      CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
      currField.setInt(0);
      return currField;
    }
    int days = indate % 1000;
    if (days < 1 || days > 365 + (isLeapYear(year) ? 1 : 0)) {
      CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
      currField.setInt(0);
      return currField;
    }
    int totaldays = 0;
    int baseyear = 1601;
    while (baseyear != year) {
      if (isLeapYear(baseyear)) {
        totaldays += 366;
      } else {
        totaldays += 365;
      }
      ++baseyear;
    }
    totaldays += days;
    currField.setInt(totaldays);
    return currField;
  }

  /**
   * libcob/intrinsicのcob_intr_factorialの実装
   *
   * @param srcfield
   * @return
   */
  public static AbstractCobolField funcFactorial(AbstractCobolField srcfield) {
    CobolFieldAttribute attr =
        new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY, 18, 0, 0, null);
    AbstractCobolField field = CobolFieldFactory.makeCobolField(8, (CobolDataStorage) null, attr);
    makeFieldEntry(field);

    CobolRuntimeException.setException(0);
    int srcval = srcfield.getInt();
    if (srcval < 0) {
      CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
      currField.setInt(0);
      return currField;
    }
    BigDecimal d = BigDecimal.ONE;
    for (int i = 2; i <= srcval; ++i) {
      d = d.multiply(new BigDecimal(i));
    }
    try {
      new CobolDecimal(d, 0).getField(currField, 0);
    } catch (CobolStopRunException e) {
      return null;
    }
    return currField;
  }

  private static CobolDecimal mathFunctionBefore1(AbstractCobolField srcfield) {
    CobolDecimal d1 = new CobolDecimal();
    CobolFieldAttribute attr =
        new CobolFieldAttribute(
            CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY,
            18,
            17,
            CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
            null);
    AbstractCobolField field = CobolFieldFactory.makeCobolField(8, (CobolDataStorage) null, attr);
    d1.setField(srcfield);
    makeFieldEntry(field);
    return d1;
  }

  private static CobolDecimal mathFunctionBefore2(AbstractCobolField srcfield) {
    CobolDecimal d1 = new CobolDecimal();
    d1.setField(srcfield);
    makeDoubleEntry();
    return d1;
  }

  private static AbstractCobolField mathFunctionAfter1(double mathd2) {
    if (Double.isNaN(mathd2)
        || mathd2 == Double.POSITIVE_INFINITY
        || mathd2 == Double.NEGATIVE_INFINITY) {
      currField.setInt(0);
      return currField;
    }
    long result = (long) mathd2;
    mathd2 -= result;
    for (int i = 0; i < 17; ++i) {
      mathd2 *= 10;
      int tempres = (int) mathd2;
      result *= 10;
      result += tempres;
      mathd2 -= tempres;
    }
    currField.getDataStorage().set(result);
    return currField;
  }

  private static AbstractCobolField mathFunctionAfter2(double mathd2) {
    if (Double.isNaN(mathd2)
        || mathd2 == Double.POSITIVE_INFINITY
        || mathd2 == Double.NEGATIVE_INFINITY) {
      currField.setInt(0);
      return currField;
    }
    currField.getDataStorage().set(mathd2);
    return currField;
  }

  /** libcob/intrinsicのcob_intr_expの実装 */
  public static AbstractCobolField funcExp(AbstractCobolField srcfield) {
    CobolDecimal d1 = mathFunctionBefore2(srcfield);
    double mathd2 = Math.pow(2.7182818284590452354, intrGetDouble(d1));
    return mathFunctionAfter2(mathd2);
  }

  /** libcob/intrinsicのcob_intr_exp10の実装 */
  public static AbstractCobolField funcExp10(AbstractCobolField srcfield) {
    CobolDecimal d1 = mathFunctionBefore2(srcfield);
    double mathd2 = Math.pow(10, intrGetDouble(d1));
    return mathFunctionAfter2(mathd2);
  }

  /** libcob/intrinsicのcob_intr_absの実装 */
  public static AbstractCobolField funcAbs(AbstractCobolField srcfield) {
    makeFieldEntry(srcfield);
    CobolDecimal d1 = srcfield.getDecimal();
    d1.setValue(d1.getValue().abs());
    try {
      d1.getField(currField, 0);
    } catch (CobolStopRunException e) {
      return null;
    }
    return currField;
  }

  /** libcob/intrinsicのcob_intr_acosの実装 */
  public static AbstractCobolField funcAcos(AbstractCobolField srcfield) {
    CobolDecimal d1 = mathFunctionBefore1(srcfield);
    double mathd2 = Math.acos(intrGetDouble(d1));
    return mathFunctionAfter1(mathd2);
  }

  /** libcob/intrinsicのcob_intr_asinの実装 */
  public static AbstractCobolField funcAsin(AbstractCobolField srcfield) {
    CobolDecimal d1 = mathFunctionBefore1(srcfield);
    double mathd2 = Math.asin(intrGetDouble(d1));
    return mathFunctionAfter1(mathd2);
  }

  /** libcob/intrinsicのcob_intr_atanの実装 */
  public static AbstractCobolField funcAtan(AbstractCobolField srcfield) {
    CobolDecimal d1 = mathFunctionBefore1(srcfield);
    double mathd2 = Math.atan(intrGetDouble(d1));
    return mathFunctionAfter1(mathd2);
  }

  /** libcob/intrinsicのcob_intr_cosの実装 */
  public static AbstractCobolField funcCos(AbstractCobolField srcfield) {
    CobolDecimal d1 = mathFunctionBefore1(srcfield);
    double mathd2 = Math.cos(intrGetDouble(d1));
    return mathFunctionAfter1(mathd2);
  }

  /** libcob/intrinsicのcob_intr_logの実装 */
  public static AbstractCobolField funcLog(AbstractCobolField srcfield) {
    CobolDecimal d1 = mathFunctionBefore2(srcfield);
    double mathd2 = Math.log(intrGetDouble(d1));
    return mathFunctionAfter2(mathd2);
  }

  /** libcob/intrinsicのcob_intr_log10の実装 */
  public static AbstractCobolField funcLog10(AbstractCobolField srcfield) {
    CobolDecimal d1 = mathFunctionBefore2(srcfield);
    double mathd2 = Math.log10(intrGetDouble(d1));
    return mathFunctionAfter2(mathd2);
  }

  /** libcob/intrinsicのcob_intr_sinの実装 */
  public static AbstractCobolField funcSin(AbstractCobolField srcfield) {
    CobolDecimal d1 = mathFunctionBefore1(srcfield);
    double mathd2 = Math.sin(intrGetDouble(d1));
    return mathFunctionAfter1(mathd2);
  }

  /** libcob/intrinsicのcob_intr_sqrtの実装 */
  public static AbstractCobolField funcSqrt(AbstractCobolField srcfield) {
    CobolDecimal d1 = mathFunctionBefore2(srcfield);
    double mathd2 = Math.sqrt(intrGetDouble(d1));
    return mathFunctionAfter2(mathd2);
  }

  /** libcob/intrinsicのcob_intr_tanの実装 */
  public static AbstractCobolField funcTan(AbstractCobolField srcfield) {
    CobolDecimal d1 = mathFunctionBefore2(srcfield);
    double mathd2 = Math.tan(intrGetDouble(d1));
    return mathFunctionAfter2(mathd2);
  }

  /**
   * libcob/intrinsicのcob_intr_numvalの実装
   *
   * @param srcfield
   * @return
   */
  public static AbstractCobolField funcNumval(AbstractCobolField srcfield) {
    CobolFieldAttribute attr =
        new CobolFieldAttribute(
            CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY,
            18,
            0,
            CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
            null);
    AbstractCobolField field = CobolFieldFactory.makeCobolField(8, (CobolDataStorage) null, attr);

    CobolDataStorage s = srcfield.getDataStorage();
    boolean sign = false;
    boolean decimalSeen = false;
    long llval = 0;
    int integerDigits = 0;
    int decimalDigits = 0;
    StringBuilder integerBuff = new StringBuilder();
    StringBuilder decimalBuff = new StringBuilder();
    for (int i = 0; i < srcfield.getSize(); ++i) {
      if (i < srcfield.getSize() - 1) {
        if ((Character.toUpperCase(s.getByte(i)) == 'C'
                && Character.toUpperCase(s.getByte(i + 1)) == 'R')
            || (Character.toUpperCase(s.getByte(i)) == 'D'
                && Character.toUpperCase(s.getByte(i + 1)) == 'B')) {
          sign = true;
          break;
        }
      }
      char c = (char) s.getByte(i);
      if (c == ' ' || c == '+') {
        continue;
      }
      if (c == '-') {
        sign = true;
        continue;
      }
      if (c == CobolModule.getCurrentModule().decimal_point) {
        decimalSeen = true;
        continue;
      }
      if (c >= '0' && c <= '9') {
        llval *= 10;
        llval += c - '0';
        if (decimalSeen) {
          decimalBuff.append(c);
          decimalDigits++;
        } else {
          integerBuff.append(c);
          integerDigits++;
        }
      }
      if (integerDigits + decimalDigits > 30) {
        break;
      }
    }
    if (integerDigits > 0) {
      integerBuff.setCharAt(0, '0');
    }
    if (decimalDigits > 0) {
      decimalBuff.setCharAt(0, '0');
    }
    if (sign) {
      llval = -llval;
    }
    if (integerDigits + decimalDigits <= 18) {
      attr.setScale(decimalDigits);
      makeFieldEntry(field);
      currField.getDataStorage().set(llval);
    } else {
      String dataString =
          String.format("%s%s.%s", sign ? "-" : "", integerBuff.toString(), decimalBuff.toString());
      double val = Double.parseDouble(dataString);
      makeDoubleEntry();
      currField.getDataStorage().set(val);
    }
    return currField;
  }

  /**
   * libcob/intrinsicのcob_intr_numval_cの実装
   *
   * @param srcfield
   * @return
   */
  public static AbstractCobolField funcNumvalC(
      AbstractCobolField srcfield, AbstractCobolField currency) {
    CobolFieldAttribute attr =
        new CobolFieldAttribute(
            CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY,
            18,
            0,
            CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
            null);
    AbstractCobolField field = CobolFieldFactory.makeCobolField(8, (CobolDataStorage) null, attr);

    boolean sign = false;
    boolean decimalSeen = false;
    long llval = 0;
    int integerDigits = 0;
    int decimalDigits = 0;
    StringBuilder integerBuff = new StringBuilder();
    StringBuilder decimalBuff = new StringBuilder();

    CobolDataStorage currencyData = null;
    if (currency != null) {
      if (currency.getSize() < srcfield.getSize()) {
        currencyData = currency.getDataStorage();
      }
    }
    for (int i = 0; i < srcfield.getSize(); ++i) {
      char c = (char) srcfield.getDataStorage().getByte(i);
      if (i < srcfield.getSize() - 1) {
        char cc = (char) srcfield.getDataStorage().getByte(i + 1);
        if ((Character.toUpperCase(c) == 'C' && Character.toUpperCase(cc) == 'R')
            || (Character.toUpperCase(c) == 'D' && Character.toUpperCase(cc) == 'B')) {
          sign = true;
          break;
        }
      }
      if (currencyData != null) {
        if (i < srcfield.getSize() - currency.getSize()) {
          if (currencyData.memcmp(
                  srcfield.getDataStorage().getSubDataStorage(i), currency.getSize())
              == 0) {
            i += (currency.getSize() - 1);
            continue;
          }
        }
      }
      if (c == ' ' || c == '+') {
        continue;
      }
      if (c == '-') {
        sign = true;
        continue;
      }
      if (c == CobolModule.getCurrentModule().decimal_point) {
        decimalSeen = true;
        continue;
      }
      if (c == CobolModule.getCurrentModule().currency_symbol) {
        continue;
      }
      if (c >= '0' && c <= '9') {
        llval *= 10;
        llval += c - '0';
        if (decimalSeen) {
          decimalBuff.append(c);
          decimalDigits++;
        } else {
          integerBuff.append(c);
          integerDigits++;
        }
      }
      if (integerDigits + decimalDigits > 30) {
        break;
      }
    }
    if (integerDigits > 0) {
      integerBuff.setCharAt(0, '0');
    }
    if (decimalDigits > 0) {
      decimalBuff.setCharAt(0, '0');
    }
    if (sign) {
      llval = -llval;
    }
    if (integerDigits + decimalDigits <= 18) {
      attr.setScale(decimalDigits);
      makeFieldEntry(field);
      currField.getDataStorage().set(llval);
    } else {
      String dataString =
          String.format("%s%s.%s", sign ? "-" : "", integerBuff.toString(), decimalBuff.toString());
      double val = Double.parseDouble(dataString);
      makeDoubleEntry();
      currField.getDataStorage().set(val);
    }
    return currField;
  }

  public static AbstractCobolField funcNumvalC(int n, AbstractCobolField currency) {
    // TODO
    return null;
  }

  public static AbstractCobolField funcNumvalC(AbstractCobolField srcfield, int n) {
    return funcNumvalC(srcfield, null);
  }

  public static AbstractCobolField funcNumvalC(int n, int m) {
    // TODO
    return null;
  }

  /**
   * libcob/intrinsicのcob_intr_annuityの実装
   *
   * @param srcfield1
   * @param srcfield2
   * @return
   */
  public static AbstractCobolField funcAnnuity(
      AbstractCobolField srcfield1, AbstractCobolField srcfield2) {
    makeDoubleEntry();
    CobolDecimal d1 = new CobolDecimal();
    CobolDecimal d2 = new CobolDecimal();
    d1.setField(srcfield1);
    d2.setField(srcfield2);

    double mathd1 = intrGetDouble(d1);
    double mathd2 = intrGetDouble(d2);
    if (mathd1 == 0) {
      mathd1 = 1.0 / mathd2;
      currField.getDataStorage().set(mathd1);
      return currField;
    }

    mathd1 /= (1.0 - Math.pow(mathd1 + 1.0, 0.0 - mathd2));
    currField.getDataStorage().set(mathd1);
    return currField;
  }

  private static int sizeInBase10(BigDecimal d1) {
    String s = d1.toPlainString();
    int begin = s.charAt(0) == '-' ? 0 : -1;
    int pointIndex = s.indexOf('.');
    int end = pointIndex < 0 ? s.length() : pointIndex;
    return end - begin - 1;
  }

  /**
   * libcob/intrinsicのcob_intr_sumの実装
   *
   * @param params
   * @param fields
   * @return
   */
  public static AbstractCobolField funcSum(int params, AbstractCobolField... fields) {
    CobolDecimal d1 = new CobolDecimal();
    CobolDecimal d2 = new CobolDecimal();
    d1.setValue(BigDecimal.ZERO);

    int scale = 0;
    for (AbstractCobolField f : fields) {
      if (f.getAttribute().getScale() > scale) {
        scale = f.getAttribute().getScale();
      }
      d2.setField(f);
      d1.add(d2);
    }

    int size = sizeInBase10(d1.getValue());
    AbstractCobolField field;
    if (size < 19) {
      CobolFieldAttribute attr =
          new CobolFieldAttribute(
              CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY,
              18,
              scale,
              CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
              null);
      field = CobolFieldFactory.makeCobolField(8, (CobolDataStorage) null, attr);
    } else {
      if (d1.getScale() > size) {
        size = d1.getScale();
      }
      if (scale > size) {
        size = scale;
      }
      CobolFieldAttribute attr =
          new CobolFieldAttribute(
              CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY,
              size,
              scale,
              CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
              null);
      field = CobolFieldFactory.makeCobolField(size, (CobolDataStorage) null, attr);
    }
    makeFieldEntry(field);
    try {
      d1.getField(currField, 0);
    } catch (CobolStopRunException e) {
      return null;
    }
    return currField;
  }

  /**
   * libcob/intrinsicのcob_intr_ord_minの実装
   *
   * @param params
   * @param fields
   * @return
   */
  public static AbstractCobolField funcOrdMin(int params, AbstractCobolField... fields) {
    CobolFieldAttribute attr =
        new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY, 8, 0, 0, null);
    AbstractCobolField field = CobolFieldFactory.makeCobolField(8, (CobolDataStorage) null, attr);
    makeFieldEntry(field);

    if (fields.length <= 1) {
      currField.setInt(0);
      return currField;
    }

    AbstractCobolField basef = fields[0];
    int ordmin = 0;
    for (int i = 1; i < fields.length; ++i) {
      AbstractCobolField f = fields[i];
      if (f.compareTo(basef) < 0) {
        basef = f;
        ordmin = i;
      }
    }

    currField.setLong((long) ordmin + 1);
    return currField;
  }

  /**
   * libcob/intrinsicのcob_intr_ord_maxの実装
   *
   * @param params
   * @param fields
   * @return
   */
  public static AbstractCobolField funcOrdMax(int params, AbstractCobolField... fields) {
    CobolFieldAttribute attr =
        new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY, 8, 0, 0, null);
    AbstractCobolField field = CobolFieldFactory.makeCobolField(8, (CobolDataStorage) null, attr);
    makeFieldEntry(field);

    if (fields.length <= 1) {
      currField.setInt(0);
      return currField;
    }

    AbstractCobolField basef = fields[0];
    int ordmax = 0;
    for (int i = 1; i < fields.length; ++i) {
      AbstractCobolField f = fields[i];
      if (f.compareTo(basef) > 0) {
        basef = f;
        ordmax = i;
      }
    }

    currField.setLong((long) ordmax + 1);
    return currField;
  }

  /**
   * libcob/intrinsicのcob_intr_minの実装
   *
   * @param params
   * @param fields
   * @return
   */
  public static AbstractCobolField funcMin(int params, AbstractCobolField... fields) {
    AbstractCobolField beasef = fields[0];
    for (int i = 1; i < fields.length; ++i) {
      AbstractCobolField f = fields[i];
      if (f.compareTo(beasef) < 0) {
        beasef = f;
      }
    }

    return beasef;
  }

  /**
   * libcob/intrinsicのcob_intr_maxの実装
   *
   * @param params
   * @param fields
   * @return
   */
  public static AbstractCobolField funcMax(int params, AbstractCobolField... fields) {
    AbstractCobolField beasef = fields[0];
    for (int i = 1; i < fields.length; ++i) {
      AbstractCobolField f = fields[i];
      if (f.compareTo(beasef) > 0) {
        beasef = f;
      }
    }

    return beasef;
  }

  /**
   * libcob/intrinsicのcob_intr_midrangeの実装
   *
   * @param params
   * @param fields
   * @return
   */
  public static AbstractCobolField funcMidrange(int params, AbstractCobolField... fields) {
    makeDoubleEntry();
    AbstractCobolField basemin = fields[0];
    AbstractCobolField basemax = fields[0];
    for (int i = 1; i < params; ++i) {
      AbstractCobolField f = fields[i];
      if (f.compareTo(basemin) < 0) {
        basemin = f;
      }
      if (f.compareTo(basemax) > 0) {
        basemax = f;
      }
    }

    CobolDecimal d1 = new CobolDecimal();
    CobolDecimal d2 = new CobolDecimal();
    d1.setField(basemin);
    d2.setField(basemax);
    d1.add(d2);
    d2 = new CobolDecimal(new BigDecimal(2), 0);
    try {
      d1.div(d2);
      d1.getField(currField, 0);
    } catch (CobolStopRunException e) {
      return null;
    }
    return currField;
  }

  /**
   * libcob/intrinsicのcob_intr_medianの実装
   *
   * @param params
   * @param fields
   * @return
   */
  public static AbstractCobolField funcMedian(int params, AbstractCobolField... fields) {
    if (fields.length == 1) {
      return fields[0];
    }

    AbstractCobolField[] fieldAlloc = new AbstractCobolField[fields.length];

    for (int i = 0; i < params; ++i) {
      fieldAlloc[i] = fields[i];
    }

    Arrays.sort(fieldAlloc, (a, b) -> a.compareTo(b));
    int i = params / 2;
    if (params % 2 != 0) {
      return fieldAlloc[i];
    } else {
      makeDoubleEntry();
      CobolDecimal d1 = new CobolDecimal();
      CobolDecimal d2 = new CobolDecimal();
      d1.setField(fieldAlloc[i]);
      d2.setField(fieldAlloc[i - 1]);
      d1.add(d2);
      d2 = new CobolDecimal(new BigDecimal(2), 0);
      try {
        d1.div(d2);
        d1.getField(currField, 0);
      } catch (CobolStopRunException e) {
        return null;
      }
      return currField;
    }
  }

  /**
   * libcob/intrinsicのcob_intr_medianの実装
   *
   * @param pramas
   * @param fields
   * @return
   */
  public static AbstractCobolField funcMean(int pramas, AbstractCobolField... fields) {
    CobolFieldAttribute attr =
        new CobolFieldAttribute(
            CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY,
            18,
            0,
            CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
            null);
    AbstractCobolField field = CobolFieldFactory.makeCobolField(8, (CobolDataStorage) null, attr);
    CobolDecimal d1 = new CobolDecimal(BigDecimal.ZERO);
    CobolDecimal d2 = new CobolDecimal();

    for (AbstractCobolField f : fields) {
      d2.setField(f);
      d1.add(d2);
    }

    d2 = new CobolDecimal(new BigDecimal(fields.length), 0);
    try {
      d1.div(d2);
    } catch (CobolStopRunException e) {
      return null;
    }

    CobolDataStorage storage = new CobolDataStorage(8);
    field.setDataStorage(storage);
    try {
      d1.getField(field, 0);
    } catch (CobolStopRunException e) {
      return null;
    }
    long n = storage.longValue();
    int i = 0;
    while (n != 0) {
      n /= 10;
      ++i;
    }
    field.setDataStorage(null);
    if (i <= 18) {
      attr.setScale(18 - i);
    }
    makeFieldEntry(field);
    try {
      d1.getField(currField, 0);
    } catch (CobolStopRunException e) {
      return null;
    }
    return currField;
  }

  /**
   * libcob/intrinsicのcob_intr_modの実装
   *
   * @param srcfield1
   * @param srcfield2
   * @return
   * @throws CobolStopRunException
   */
  public static AbstractCobolField funcMod(
      AbstractCobolField srcfield1, AbstractCobolField srcfield2) throws CobolStopRunException {
    CobolFieldAttribute attr =
        new CobolFieldAttribute(
            CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY,
            18,
            0,
            CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
            null);
    AbstractCobolField field = CobolFieldFactory.makeCobolField(8, (CobolDataStorage) null, attr);
    CobolDecimal d1 = new CobolDecimal();
    CobolDecimal d2 = new CobolDecimal();
    makeFieldEntry(field);

    AbstractCobolField f1 = funcInteger(intrBinop(srcfield1, '/', srcfield2));
    d1.setField(srcfield2);
    d2.setField(f1);
    d2.mul(d1);
    d1.setField(srcfield1);
    d1.sub(d2);
    try {
      d1.getField(currField, 0);
    } catch (CobolStopRunException e) {
      return null;
    }
    return currField;
  }

  /**
   * libcob/intrinsicのcob_intr_rangeの実装
   *
   * @param params
   * @param fields
   * @return
   * @throws CobolStopRunException
   */
  public static AbstractCobolField funcRange(int params, AbstractCobolField... fields)
      throws CobolStopRunException {
    CobolFieldAttribute attr =
        new CobolFieldAttribute(
            CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY,
            18,
            0,
            CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
            null);
    AbstractCobolField field = CobolFieldFactory.makeCobolField(8, (CobolDataStorage) null, attr);
    CobolDecimal d1 = new CobolDecimal();
    CobolDecimal d2 = new CobolDecimal();

    AbstractCobolField basemin = fields[0];
    AbstractCobolField basemax = fields[0];
    for (int i = 1; i < fields.length; ++i) {
      AbstractCobolField f = fields[i];
      if (f.compareTo(basemin) < 0) {
        basemin = f;
      }
      if (f.compareTo(basemax) > 0) {
        basemax = f;
      }
    }

    attr.setScale(basemin.getAttribute().getScale());
    if (basemax.getAttribute().getScale() > attr.getScale()) {
      attr.setScale(basemax.getAttribute().getScale());
    }
    makeFieldEntry(field);
    d1.setField(basemax);
    d2.setField(basemin);
    d1.sub(d2);
    d1.getField(currField, 0);
    return currField;
  }

  /**
   * libcob/intrinsicのcob_intr_remの実装
   *
   * @param srcfield1
   * @param srcfield2
   * @return
   * @throws CobolStopRunException
   */
  public static AbstractCobolField funcRem(
      AbstractCobolField srcfield1, AbstractCobolField srcfield2) throws CobolStopRunException {
    CobolFieldAttribute attr =
        new CobolFieldAttribute(
            CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY,
            18,
            0,
            CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
            null);
    AbstractCobolField field = CobolFieldFactory.makeCobolField(8, (CobolDataStorage) null, attr);
    AbstractCobolField f1 = funcIntegerPart(intrBinop(srcfield1, '/', srcfield2));
    CobolDecimal d1 = new CobolDecimal();
    CobolDecimal d2 = new CobolDecimal();

    d1.setField(srcfield2);
    d2.setField(f1);
    d2.mul(d1);
    d1.setField(srcfield1);
    d1.sub(d2);

    attr.setScale(srcfield1.getAttribute().getScale());
    if (srcfield2.getAttribute().getScale() > attr.getScale()) {
      attr.setScale(srcfield2.getAttribute().getScale());
    }
    makeFieldEntry(field);
    d1.getField(currField, 0);
    return currField;
  }

  /**
   * libcob/intrinsicのcob_intr_randomの実装
   *
   * @param prams
   * @param fields
   * @return
   */
  public static AbstractCobolField funcRandom(int prams, AbstractCobolField... fields) {
    CobolFieldAttribute attr =
        new CobolFieldAttribute(
            CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY,
            18,
            0,
            CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
            null);
    AbstractCobolField field = CobolFieldFactory.makeCobolField(8, (CobolDataStorage) null, attr);

    if (fields.length > 0) {
      AbstractCobolField f = fields[0];
      int seed = f.getInt();
      if (seed < 0) {
        seed = 0;
      }
      random.setSeed(seed);
    }

    int r = random.nextInt(1000000001);

    int exp10 = 1;
    int i = 0;
    for (i = 0; i < 10; ++i) {
      if (r / exp10 == 0) {
        break;
      }
      exp10 *= 10;
    }
    if (i == 0) {
      i = 1;
    }
    attr.setScale(i);
    makeFieldEntry(field);
    currField.getDataStorage().set((long) r);
    return currField;
  }

  /**
   * libcob/intrinsicのcob_intr_varianceの実装
   *
   * @param prams
   * @param fields
   * @return
   * @throws CobolStopRunException
   */
  public static AbstractCobolField funcVariance(int prams, AbstractCobolField... fields)
      throws CobolStopRunException {
    CobolFieldAttribute attr =
        new CobolFieldAttribute(
            CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY,
            18,
            0,
            CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
            null);
    AbstractCobolField field = CobolFieldFactory.makeCobolField(8, (CobolDataStorage) null, attr);

    if (fields.length == 1) {
      makeFieldEntry(field);
      currField.setInt(0);
      return currField;
    }

    CobolDecimal d1 = new CobolDecimal(BigDecimal.ZERO, 0);
    CobolDecimal d2 = new CobolDecimal();

    for (AbstractCobolField f : fields) {
      d2.setField(f);
      d1.add(d2);
    }

    d2.setValue(new BigDecimal(fields.length));
    d2.setScale(0);
    try {
      d1.div(d2);
    } catch (CobolStopRunException e) {
      return null;
    }

    CobolDecimal d4 = new CobolDecimal(BigDecimal.ZERO, 0);

    for (AbstractCobolField f : fields) {
      d2.setField(f);
      d2.sub(d1);
      d2.mul(d2);
      d4.add(d2);
    }

    CobolDecimal d3 = new CobolDecimal(new BigDecimal(fields.length), 0);
    try {
      d4.div(d3);
    } catch (CobolStopRunException e) {
      return null;
    }
    CobolDataStorage data = new CobolDataStorage(8);
    field.setDataStorage(data);
    d4.getField(field, 0);
    long n = field.getLong();
    int i = 0;
    while (n != 0) {
      n /= 10;
      ++i;
    }
    makeFieldEntry(field);
    if (i <= 18) {
      attr.setScale(18 - i);
    }
    d4.getField(currField, 0);
    return currField;
  }

  /**
   * libcob/intrinsicのcob_intr_standard_deviationの実装
   *
   * @param prams
   * @param fields
   * @return
   * @throws CobolStopRunException
   */
  public static AbstractCobolField funcStandardDeviation(int prams, AbstractCobolField... fields)
      throws CobolStopRunException {
    CobolFieldAttribute attr =
        new CobolFieldAttribute(
            CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY,
            18,
            0,
            CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
            null);
    AbstractCobolField field = CobolFieldFactory.makeCobolField(8, (CobolDataStorage) null, attr);

    makeDoubleEntry();

    if (fields.length == 1) {
      makeFieldEntry(field);
      currField.setInt(0);
      return currField;
    }

    CobolDecimal d1 = new CobolDecimal(BigDecimal.ZERO, 0);
    CobolDecimal d2 = new CobolDecimal();

    for (AbstractCobolField f : fields) {
      d2.setField(f);
      d1.add(d2);
    }

    d2.setValue(new BigDecimal(fields.length));
    d2.setScale(0);
    try {
      d1.div(d2);
    } catch (CobolStopRunException e) {
      return null;
    }

    CobolDecimal d4 = new CobolDecimal(BigDecimal.ZERO, 0);

    for (AbstractCobolField f : fields) {
      d2.setField(f);
      d2.sub(d1);
      d2.mul(d2);
      d4.add(d2);
    }

    CobolDecimal d3 = new CobolDecimal(new BigDecimal(fields.length), 0);
    try {
      d4.div(d3);
    } catch (CobolStopRunException e) {
      return null;
    }
    d4.getField(currField, 0);
    return funcSqrt(currField);
  }

  /**
   * libcob/intrinsicのcob_intr_present_valueの実装
   *
   * @param prams
   * @param fields
   * @return
   * @throws CobolStopRunException
   */
  public static AbstractCobolField funcPresentValue(int prams, AbstractCobolField... fields)
      throws CobolStopRunException {
    makeDoubleEntry();
    if (fields.length < 2) {
      System.err.println("Wrong number of parameters for FUNCTION PRESENT-VALUE");
      System.err.flush();
      currField.setInt(0);
      return currField;
    }
    AbstractCobolField f = fields[0];
    CobolDecimal d1 = new CobolDecimal();
    d1.setField(f);
    CobolDecimal d2 = new CobolDecimal(BigDecimal.ONE, 0);
    d1.add(d2);
    CobolDecimal d4 = new CobolDecimal(BigDecimal.ZERO, 0);

    for (int i = 1; i < fields.length; ++i) {
      f = fields[i];
      d2.setField(f);
      CobolDecimal d3 = new CobolDecimal(d1.getValue().add(BigDecimal.ZERO), d1.getScale());
      if (i > 1) {
        CobolDecimal d5 = new CobolDecimal(new BigDecimal(i), 0);
        d3.pow(d5);
      }
      d2.div(d3);
      d4.add(d2);
    }

    d4.getField(currField, 0);
    return currField;
  }

  /**
   * libcob/intrinsicのcob_intr_nationalの実装
   *
   * @param srcfield
   * @return
   */
  public static AbstractCobolField funcNational(AbstractCobolField srcfield) {
    int size = srcfield.getSize();
    byte[] pdata =
        CobolNationalField.han2zen(srcfield.getDataStorage().getByteBuffer(size).array(), size);
    int ndata = CobolNationalField.workReturnSize;
    CobolFieldAttribute attr =
        new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_NATIONAL, 0, 0, 0, null);
    AbstractCobolField field =
        CobolFieldFactory.makeCobolField(ndata, (CobolDataStorage) null, attr);
    makeFieldEntry(field);
    currField.getDataStorage().memcpy(pdata, ndata);
    return currField;
  }

  /**
   * cob_intr_combined_datetimeの実装
   *
   * @param srcdays
   * @param srctime
   * @return
   */
  public static AbstractCobolField funcCombinedDatetime(
      AbstractCobolField srcdays, AbstractCobolField srctime) {
    int srdays;
    int srtime;
    String str;

    CobolFieldAttribute attr =
        new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY, 12, 5, 0, null);
    AbstractCobolField field = CobolFieldFactory.makeCobolField(12, (CobolDataStorage) null, attr);
    makeFieldEntry(field);
    CobolRuntimeException.setException(0);
    srdays = srcdays.getInt();
    if (srdays < 1 || srdays > 3067671) {
      CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
      currField.getDataStorage().memset(0, 12);
      return currField;
    }
    srtime = srctime.getInt();
    if (srtime < 1 || srtime > 86400) {
      CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
      currField.getDataStorage().memset(0, 12);
      return currField;
    }
    str = String.format("%7d%5d", srdays, srtime);
    byte[] buff = str.getBytes();
    for (int i = 0; i < buff.length; i++) {
      if (buff[i] == ' ') {
        buff[i] = '0';
      }
    }
    currField.getDataStorage().memcpy(buff);
    return currField;
  }

  /**
   * cob_intr_concatenateの実装
   *
   * @param offset
   * @param length
   * @param params
   * @param fields
   * @return
   */
  public static AbstractCobolField funcConcatenate(
      int offset, int length, int params, AbstractCobolField... fields) {
    int calcsize = 0;
    int i;
    int index = 0;
    int size;
    byte[] data;

    for (i = 0; i < params; i++) {
      calcsize += fields[i].getSize();
    }
    CobolFieldAttribute attr =
        new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null);
    AbstractCobolField field =
        CobolFieldFactory.makeCobolField(calcsize, (CobolDataStorage) null, attr);
    makeFieldEntry(field);
    data = new byte[calcsize];
    for (i = 0; i < params; i++) {
      size = fields[i].getSize();
      System.arraycopy(
          fields[i].getDataStorage().getByteBuffer(size).array(), 0, data, index, size);
      index += size;
    }
    currField.setDataStorage(new CobolDataStorage(data));
    if (offset > 0) {
      calcRefMod(currField, offset, length);
    }
    return currField;
  }

  /**
   * cob_intr_date_to_yyyymmddの実装
   *
   * @param params
   * @param fields
   * @return
   */
  public static AbstractCobolField funcDateToYyyymmdd(int params, AbstractCobolField... fields) {
    int year;
    int mmdd;
    int interval;
    int xqtyear;
    int maxyear;
    LocalDateTime timeptr;

    CobolFieldAttribute attr =
        new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY, 8, 0, 0, null);
    AbstractCobolField field = CobolFieldFactory.makeCobolField(4, (CobolDataStorage) null, attr);
    makeFieldEntry(field);
    year = fields[0].getInt();
    mmdd = year % 10000;
    year /= 10000;
    if (params > 1) {
      interval = fields[1].getInt();
    } else {
      interval = 50;
    }
    if (params > 2) {
      xqtyear = fields[2].getInt();
    } else {
      timeptr = CobolUtil.localtime();
      xqtyear = 1900 + timeptr.getDayOfYear();
    }
    if (year < 0 || year > 999999) {
      CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
      currField.setInt(0);
      return currField;
    }
    if (xqtyear < 1601 || xqtyear > 9999) {
      CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
      currField.setInt(0);
      return currField;
    }
    maxyear = xqtyear + interval;
    if (maxyear < 1700 || maxyear > 9999) {
      CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
      currField.setInt(0);
      return currField;
    }
    if (maxyear % 100 >= year) {
      year += 100 * (maxyear / 100);
    } else {
      year += 100 * ((maxyear / 100) - 1);
    }
    year *= 10000;
    year += mmdd;
    currField.setInt(year);
    return currField;
  }

  /**
   * cob_intr_day_to_yyyydddの実装
   *
   * @param params
   * @param fields
   * @return
   */
  public static AbstractCobolField funcDayToYyyyddd(int params, AbstractCobolField... fields) {
    int year;
    int days;
    int interval;
    int xqtyear;
    int maxyear;
    LocalDateTime timeptr;

    CobolFieldAttribute attr =
        new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY, 8, 0, 0, null);
    AbstractCobolField field = CobolFieldFactory.makeCobolField(4, (CobolDataStorage) null, attr);
    makeFieldEntry(field);
    year = fields[0].getInt();
    days = year % 1000;
    year /= 1000;
    if (params > 1) {
      interval = fields[1].getInt();
    } else {
      interval = 50;
    }
    if (params > 2) {
      xqtyear = fields[2].getInt();
    } else {
      timeptr = CobolUtil.localtime();
      xqtyear = 1900 + timeptr.getDayOfYear();
    }

    if (year < 0 || year > 999999) {
      CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
      currField.setInt(0);
      return currField;
    }
    if (xqtyear < 1601 || xqtyear > 9999) {
      CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
      currField.setInt(0);
      return currField;
    }
    maxyear = xqtyear + interval;
    if (maxyear < 1700 || maxyear > 9999) {
      CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
      currField.setInt(0);
      return currField;
    }
    if (maxyear % 100 >= year) {
      year += 100 * (maxyear / 100);
    } else {
      year += 100 * ((maxyear / 100) - 1);
    }
    year *= 1000;
    year += days;
    currField.setInt(year);
    return currField;
  }

  /**
   * cob_intr_exception_fileの実装
   *
   * @return
   */
  public static AbstractCobolField funcExceptionFile() {
    int flen;
    byte[] data;

    CobolFieldAttribute attr =
        new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null);
    AbstractCobolField field = CobolFieldFactory.makeCobolField(0, (CobolDataStorage) null, attr);
    if (CobolRuntimeException.getException() == 0
        || (CobolRuntimeException.getExceptionCode() & 0x0500) != 0x0500) {
      field.setSize(2);
      makeFieldEntry(field);
      currField.memcpy(byteArray00, 2);
    } else {
      flen = CobolFile.errorFile.getSelectName().length();
      field.setSize(flen + 2);
      makeFieldEntry(field);
      data = new byte[2 + flen];
      System.arraycopy(CobolFile.errorFile.getFileStatus(), 0, data, 0, 2);
      System.arraycopy(CobolFile.errorFile.getSelectName().getBytes(), 0, data, 2, flen);
      currField.setDataStorage(new CobolDataStorage(data));
    }
    return currField;
  }

  /**
   * cob_intr_exception_locationの実装
   *
   * @return
   */
  public static AbstractCobolField funcExceptionLocation() {
    String buff;

    CobolFieldAttribute attr =
        new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null);
    AbstractCobolField field = CobolFieldFactory.makeCobolField(0, (CobolDataStorage) null, attr);
    currField = field;
    if (CobolRuntimeException.getException() != 1
        || CobolRuntimeException.getOrigProgramId() == null) {
      field.setSize(1);
      makeFieldEntry(field);
      currField.getDataStorage().setByte(0, ' ');
      return currField;
    }
    if (CobolRuntimeException.getOrigSection() != null
        && CobolRuntimeException.getOrigParagragh() != null) {
      buff =
          String.format(
              "%s; %s OF %s; %d",
              CobolRuntimeException.getOrigProgramId(),
              CobolRuntimeException.getOrigParagragh(),
              CobolRuntimeException.getOrigSection(),
              CobolRuntimeException.getOrigLine());
    } else if (CobolRuntimeException.getOrigSection() != null) {
      buff =
          String.format(
              "%s; %s; %d",
              CobolRuntimeException.getOrigProgramId(),
              CobolRuntimeException.getOrigSection(),
              CobolRuntimeException.getOrigLine());
    } else if (CobolRuntimeException.getOrigParagragh() != null) {
      buff =
          String.format(
              "%s; %s; %d",
              CobolRuntimeException.getOrigProgramId(),
              CobolRuntimeException.getOrigParagragh(),
              CobolRuntimeException.getOrigLine());
    } else {
      buff =
          String.format(
              "%s; ; %d",
              CobolRuntimeException.getOrigProgramId(), CobolRuntimeException.getOrigLine());
    }
    localeBuff = buff.getBytes();
    field.setSize(localeBuff.length);
    currField.setDataStorage(new CobolDataStorage(localeBuff));
    return currField;
  }

  /**
   * cob_intr_exception_statementの実装
   *
   * @return
   */
  public static AbstractCobolField funcExceptionStatement() {
    CobolFieldAttribute attr =
        new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null);
    AbstractCobolField field = CobolFieldFactory.makeCobolField(31, (CobolDataStorage) null, attr);
    makeFieldEntry(field);
    byte[] data;
    if (CobolRuntimeException.getExceptionCode() != 0
        && CobolRuntimeException.getOrigStatement() != null) {
      data = String.format("%-31s", CobolRuntimeException.getOrigStatement()).getBytes();
    } else {
      data = String.format("%-31s", "").getBytes();
    }
    currField.setDataStorage(new CobolDataStorage(data));
    return currField;
  }

  private static final byte[] CONST_STRING_EXCEPTION_OBJECT = "EXCEPTION-OBJECT".getBytes();

  /**
   * cob_intr_exception_statusの実装
   *
   * @return
   */
  public static AbstractCobolField funcExceptionStatus() {
    byte[] exceptName;

    CobolFieldAttribute attr =
        new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null);
    AbstractCobolField field = CobolFieldFactory.makeCobolField(31, (CobolDataStorage) null, attr);
    makeFieldEntry(field);
    byte[] data = String.format("%-31s", "").getBytes();
    currField.setDataStorage(new CobolDataStorage(data));
    if (CobolRuntimeException.getExceptionCode() != 0) {
      try {
        exceptName =
            CobolRuntimeException.getExceptionName(CobolRuntimeException.getExceptionCode())
                .getBytes();
      } catch (Exception e) {
        exceptName = CONST_STRING_EXCEPTION_OBJECT;
      }
      currField.memcpy(exceptName, exceptName.length);
    }
    return currField;
  }

  /**
   * cob_intr_fraction_partの実装
   *
   * @param srcfield
   * @return
   */
  public static AbstractCobolField funcFractionPart(AbstractCobolField srcfield) {
    CobolFieldAttribute attr =
        new CobolFieldAttribute(
            CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY,
            18,
            18,
            CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
            null);
    AbstractCobolField field = CobolFieldFactory.makeCobolField(8, (CobolDataStorage) null, attr);
    makeFieldEntry(field);

    currField.moveFrom(srcfield);
    return currField;
  }

  /**
   * cob_intr_seconds_from_formatted_timeの実装
   *
   * @param format
   * @param value
   * @return
   */
  public static AbstractCobolField funcSecondsFromFormattedTime(
      AbstractCobolField format, AbstractCobolField value) {
    int n;
    int seconds = 0;
    int minutes = 0;
    int hours = 0;
    boolean secondsSeen = false;
    boolean minutesSeen = false;
    boolean hoursSeen = false;
    String p1;
    int p2;

    CobolFieldAttribute attr =
        new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY, 8, 0, 0, null);
    AbstractCobolField field = CobolFieldFactory.makeCobolField(4, (CobolDataStorage) null, attr);
    makeFieldEntry(field);

    if (value.getSize() < format.getSize()) {
      CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
      currField.setInt(0);
      return currField;
    }

    CobolDataStorage formatData = format.getDataStorage();
    CobolDataStorage valueData = value.getDataStorage();

    for (n = 0; n < format.getSize() - 1; n++) {
      p1 = new String(formatData.getByteArray(n, 2));

      if ("hh".equals(p1) && !hoursSeen) {
        p2 = Integer.parseInt(new String(valueData.getByteArray(n, 2)));
        hours = p2;
        hoursSeen = true;
        continue;
      }
      if ("mm".equals(p1) && !minutesSeen) {
        p2 = Integer.parseInt(new String(valueData.getByteArray(n, 2)));
        minutes = p2;
        minutesSeen = true;
        continue;
      }
      if ("ss".equals(p1) && !secondsSeen) {
        p2 = Integer.parseInt(new String(valueData.getByteArray(n, 2)));
        seconds = p2;
        secondsSeen = true;
        continue;
      }
    }

    if (hoursSeen && minutesSeen && secondsSeen) {
      seconds += hours * 3600 + minutes * 60;
    } else {
      CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
      seconds = 0;
    }
    currField.setInt(seconds);
    return currField;
  }

  /**
   * cob_intr_seconds_past_midnightの実装
   *
   * @return
   */
  public static AbstractCobolField funcSecondsPastMidnight() {
    int seconds;
    CobolFieldAttribute attr =
        new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY, 8, 0, 0, null);
    AbstractCobolField field = CobolFieldFactory.makeCobolField(4, (CobolDataStorage) null, attr);
    makeFieldEntry(field);
    LocalDateTime currDate = LocalDateTime.now();
    seconds = currDate.getHour() * 3600 + currDate.getMinute() * 60 + currDate.getSecond();
    currField.setInt(seconds);
    return currField;
  }

  /**
   * cob_intr_signの実装
   *
   * @param srcfield
   * @return
   */
  public static AbstractCobolField funcSign(AbstractCobolField srcfield) {
    CobolFieldAttribute attr =
        new CobolFieldAttribute(
            CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY,
            8,
            0,
            CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
            null);
    AbstractCobolField field = CobolFieldFactory.makeCobolField(4, (CobolDataStorage) null, attr);
    makeFieldEntry(field);

    currField.setInt(0);
    int n = srcfield.compareTo(currField);
    if (n < 0) {
      currField.setInt(-1);
    } else if (n > 0) {
      currField.setInt(1);
    }
    return currField;
  }

  /**
   * cob_intr_stored_char_lengthの実装
   *
   * @param srcfield
   * @return
   */
  public static AbstractCobolField funcStoredCharLength(AbstractCobolField srcfield) {
    int count;

    CobolFieldAttribute attr =
        new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY, 8, 0, 0, null);
    AbstractCobolField field = CobolFieldFactory.makeCobolField(4, (CobolDataStorage) null, attr);
    makeFieldEntry(field);

    CobolDataStorage storage = srcfield.getDataStorage();
    for (count = srcfield.getSize(); count > 0; count--) {
      if (storage.getByte(count - 1) != ' ') {
        break;
      }
    }

    currField.setInt(count);
    return currField;
  }

  public static AbstractCobolField funcSubstitute(
      int offset, int length, int params, AbstractCobolField... fields) {
    int i, j, k;
    int numreps = params / 2;
    AbstractCobolField[] f1 = new AbstractCobolField[numreps];
    AbstractCobolField[] f2 = new AbstractCobolField[numreps];
    CobolDataStorage src = fields[0].getDataStorage();
    CobolDataStorage fData1;
    int srcSize = fields[0].getSize();
    int fSize1;
    StringBuilder rtn = new StringBuilder();

    for (i = 0; i < params - 1; i++) {
      if ((i % 2) == 0) {
        f1[i / 2] = fields[i + 1];
      } else {
        f2[i / 2] = fields[i + 1];
      }
    }

    for (i = 0; i < srcSize; ) {
      for (j = 0; j < numreps; j++) {
        fData1 = f1[j].getDataStorage();
        fSize1 = f1[j].getSize();
        for (k = fSize1 - 1; k >= 0; k--) {
          if (i + k >= srcSize || src.getByte(i + k) != fData1.getByte(k)) {
            break;
          }
        }
        if (k < 0) {
          rtn.append(f2[j].getString());
          i += fSize1;
          break;
        }
      }
      if (j == numreps) {
        rtn.append((char) src.getByte(i));
        i++;
      }
    }

    CobolFieldAttribute attr =
        new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null);
    AbstractCobolField field =
        CobolFieldFactory.makeCobolField(rtn.length(), (CobolDataStorage) null, attr);
    makeFieldEntry(field);
    currField.setDataStorage(new CobolDataStorage(rtn.toString()));

    if (offset > 0) {
      calcRefMod(currField, offset, length);
    }
    return currField;
  }

  public static AbstractCobolField funcSubstituteCase(
      int offset, int length, int params, AbstractCobolField... fields) {
    int i, j, k;
    int numreps = params / 2;
    AbstractCobolField[] f1 = new AbstractCobolField[numreps];
    AbstractCobolField[] f2 = new AbstractCobolField[numreps];
    CobolDataStorage src = fields[0].getDataStorage();
    CobolDataStorage fData1;
    int srcSize = fields[0].getSize();
    int fSize1;
    StringBuilder rtn = new StringBuilder();

    for (i = 0; i < params - 1; i++) {
      if (i % 2 == 0) {
        f1[i / 2] = fields[i + 1];
      } else {
        f2[i / 2] = fields[i + 1];
      }
    }

    for (i = 0; i < srcSize; ) {
      for (j = 0; j < numreps; j++) {
        fData1 = f1[j].getDataStorage();
        fSize1 = f1[j].getSize();
        for (k = fSize1 - 1; k >= 0; k--) {
          if (i + k >= srcSize
              || Character.toLowerCase((char) fData1.getByte(k))
                  != Character.toLowerCase((char) src.getByte(i + k))) {
            break;
          }
        }
        if (k < 0) {
          rtn.append(f2[j].getString());
          i += fSize1;
          break;
        }
      }
      if (j == numreps) {
        rtn.append((char) src.getByte(i));
        i++;
      }
    }

    CobolFieldAttribute attr =
        new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null);
    AbstractCobolField field =
        CobolFieldFactory.makeCobolField(rtn.length(), (CobolDataStorage) null, attr);
    makeFieldEntry(field);

    currField.setDataStorage(new CobolDataStorage(rtn.toString()));
    if (offset > 0) {
      calcRefMod(currField, offset, length);
    }
    return currField;
  }

  /** Equivalent to cob_intr_trim */
  public static AbstractCobolField funcTrim(
      int offset, int length, AbstractCobolField srcField, int direction) {
    makeFieldEntry(srcField);
    int i;
    int srcFieldSize = srcField.getSize();
    CobolDataStorage srcStorage = srcField.getDataStorage();
    for (i = 0; i < srcFieldSize; ++i) {
      if (srcStorage.getByte(i) != ' ') {
        break;
      }
    }
    if (i == srcFieldSize) {
      currField.setSize(1);
      currField.getDataStorage().setByte(0, (byte) ' ');
      return currField;
    }
    int beginIndex = 0;
    if (direction != 2) {
      while (srcStorage.getByte(beginIndex) == ' ') {
        ++beginIndex;
      }
    }
    int endIndex = srcFieldSize - 1;
    if (direction != 1) {
      while (srcStorage.getByte(endIndex) == ' ') {
        --endIndex;
      }
    }
    CobolDataStorage currStorage = currField.getDataStorage();
    currField.setSize(endIndex - beginIndex + 1);
    for (i = 0; i <= endIndex - beginIndex; ++i) {
      currStorage.setByte(i, srcStorage.getByte(beginIndex + i));
    }
    if (offset > 0) {
      calcRefMod(currField, offset, length);
    }
    return currField;
  }

  public static AbstractCobolField funcLocaleDate(
      int offset, int length, AbstractCobolField srcField, int localeField) {
    return funcLocaleDate(offset, length, srcField, null);
  }

  public static AbstractCobolField funcLocaleDate(
      int offset, int length, AbstractCobolField srcField, AbstractCobolField localeField) {
    AbstractCobolField field =
        CobolFieldFactory.makeCobolField(
            0,
            (CobolDataStorage) null,
            new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 10, 0, 0, null));
    int inDate;

    // Convert the input field to an integer
    if (srcField.getAttribute().isTypeNumeric()) {
      inDate = srcField.getInt();
    } else {
      if (srcField.getSize() < 8) {
        return errorFuncLocaleDate(field);
      }
      int p = 0;
      inDate = 0;
      for (int len = 0; len < 8; ++len, ++p) {
        char c = (char) srcField.getDataStorage().getByte(p);
        if ('0' <= c && c <= '9') {
          inDate = inDate * 10 + (c - '0');
        } else {
          return errorFuncLocaleDate(field);
        }
      }
    }

    // Calculate the year, month, and days
    int year = inDate / 10000;
    if (year < 1601 || year > 9999) {
      return errorFuncLocaleDate(field);
    }
    inDate %= 10000;

    int month = inDate / 100;
    if (month < 1 || month > 12) {
      return errorFuncLocaleDate(field);
    }

    int days = inDate % 100;
    if (days < 1 || days > 31) {
      return errorFuncLocaleDate(field);
    }

    if (isLeapYear(year)) {
      if (days > leapMonthDays[month]) {
        return errorFuncLocaleDate(field);
      }
    } else {
      if (days > normalMonthDays[month]) {
        return errorFuncLocaleDate(field);
      }
    }

    // Create the date string
    Calendar cal = Calendar.getInstance();
    cal.set(year, month - 1, days);

    DateFormat df;
    if (localeField != null) {
      Locale locale = new Locale(localeField.getString());
      df = DateFormat.getDateInstance(DateFormat.SHORT, locale);
    } else {
      df = DateFormat.getDateInstance(DateFormat.SHORT);
    }
    String dateString = df.format(cal.getTime());

    // Return the result
    field.setSize(dateString.length());
    makeFieldEntry(field);
    currField.getDataStorage().memcpy(dateString.getBytes());
    if (offset > 0) {
      calcRefMod(field, offset, length);
    }
    return currField;
  }

  private static AbstractCobolField errorFuncLocaleDate(AbstractCobolField field) {
    field.setSize(10);
    makeFieldEntry(field);
    currField.getDataStorage().memset((byte) '0', 10);
    CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
    return currField;
  }

  public static AbstractCobolField funcLocaleTime(
      int offset, int length, AbstractCobolField srcField, int localeField) {
    return funcLocaleTime(offset, length, srcField, null);
  }

  public static AbstractCobolField funcLocaleTime(
      int offset, int length, AbstractCobolField srcField, AbstractCobolField localeField) {
    AbstractCobolField field =
        CobolFieldFactory.makeCobolField(
            0,
            (CobolDataStorage) null,
            new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 10, 0, 0, null));
    int inTime;

    // Convert the input field to an integer
    if (srcField.getAttribute().isTypeNumeric()) {
      inTime = srcField.getInt();
    } else {
      if (srcField.getSize() < 6) {
        return errorFuncLocaleDate(field);
      }
      int p = 0;
      inTime = 0;
      for (int len = 0; len < 6; ++len, ++p) {
        char c = (char) srcField.getDataStorage().getByte(p);
        if ('0' <= c && c <= '9') {
          inTime = inTime * 10 + (c - '0');
        } else {
          return errorFuncLocaleDate(field);
        }
      }
    }

    // Calculate the hours, minutes, and seconds
    int hours = inTime / 10000;
    if (hours < 0 || hours > 24) {
      return errorFuncLocaleDate(field);
    }
    inTime %= 10000;

    int minutes = inTime / 100;
    if (minutes < 0 || minutes > 59) {
      return errorFuncLocaleDate(field);
    }

    int seconds = inTime % 100;
    if (seconds < 0 || seconds > 59) {
      return errorFuncLocaleDate(field);
    }

    // Create the time string
    LocalTime time = LocalTime.of(hours, minutes, seconds);

    DateTimeFormatter formatter;
    String pattern = "HH:mm:ss";
    if (localeField != null) {
      Locale locale = new Locale(localeField.getString());
      formatter = DateTimeFormatter.ofPattern(pattern, locale);
    } else {
      formatter = DateTimeFormatter.ofPattern(pattern);
    }
    String timeString = time.format(formatter);

    // Return the result
    field.setSize(timeString.length());
    makeFieldEntry(field);
    currField.getDataStorage().memcpy(timeString.getBytes());
    if (offset > 0) {
      calcRefMod(field, offset, length);
    }
    return currField;
  }

  public static AbstractCobolField funcLocaleTimeFromSeconds(
      int offset, int length, AbstractCobolField srcField, int localeField) {
    return funcLocaleTime(offset, length, srcField, null);
  }

  public static AbstractCobolField funcLocaleTimeFromSeconds(
      int offset, int length, AbstractCobolField srcField, AbstractCobolField localeField) {
    AbstractCobolField field =
        CobolFieldFactory.makeCobolField(
            0,
            (CobolDataStorage) null,
            new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 10, 0, 0, null));
    int inTime;

    // Convert the input field to an integer
    if (srcField.getAttribute().isTypeNumeric()) {
      inTime = srcField.getInt();
    } else {
      return errorFuncLocaleDate(field);
    }

    // Calculate the hours, minutes, and seconds
    int hours = inTime / 3600;
    inTime %= 3600;
    int minutes = inTime / 60;
    int seconds = inTime % 60;

    // Create the time string
    LocalTime time = LocalTime.of(hours, minutes, seconds);

    DateTimeFormatter formatter;
    String pattern = "HH:mm:ss";
    if (localeField != null) {
      Locale locale = new Locale(localeField.getString());
      formatter = DateTimeFormatter.ofPattern(pattern, locale);
    } else {
      formatter = DateTimeFormatter.ofPattern(pattern);
    }
    String timeString = time.format(formatter);

    // Return the result
    field.setSize(timeString.length());
    makeFieldEntry(field);
    currField.getDataStorage().memcpy(timeString.getBytes());
    if (offset > 0) {
      calcRefMod(field, offset, length);
    }
    return currField;
  }
}
