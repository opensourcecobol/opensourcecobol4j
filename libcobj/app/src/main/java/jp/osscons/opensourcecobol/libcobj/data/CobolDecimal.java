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
package jp.osscons.opensourcecobol.libcobj.data;

import java.math.BigDecimal;
import java.math.RoundingMode;
import jp.osscons.opensourcecobol.libcobj.common.CobolModule;
import jp.osscons.opensourcecobol.libcobj.common.CobolUtil;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolExceptionId;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolExceptionInfo;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolRuntimeException;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolStopRunException;

/**
 * BigDecimalを扱うクラス<br>
 * COMPUTE等で計算をするときに使用する
 */
public class CobolDecimal {
  /** TODO: 準備中 */
  public static final int DECIMAL_NAN = -128;
  /** TODO: 準備中 */
  public static final int COB_STORE_ROUND = 0x01;
  /** TODO: 準備中 */
  public static final int COB_STORE_KEEP_ON_OVERFLOW = 0x02;
  /** TODO: 準備中 */
  public static final int COB_STORE_TRUNC_ON_OVERFLOW = 0x04;

  /** TODO: 準備中 */
  public static final int COB_MAX_BINARY = 36;

  private static BigDecimal cobMexp = BigDecimal.ZERO;
  /** TODO: 準備中 */
  static CobolDecimal cobD1 = new CobolDecimal();
  /** TODO: 準備中 */
  static CobolDecimal cobD2 = new CobolDecimal();
  /** TODO: 準備中 */
  static CobolDecimal cobD3 = new CobolDecimal();
  /** TODO: 準備中 */
  static CobolDecimal cobD4 = new CobolDecimal();
  /** TODO: 準備中 */
  private static BigDecimal[] cobMpze10 = new BigDecimal[COB_MAX_BINARY];
  /** TODO: 準備中 */
  static byte[] packedValue = new byte[20];
  /** TODO: 準備中 */
  static int packedValueInt = 0;

  /** TODO: 準備中 */
  public static void cobInitNumeric() {
    cobD1 = new CobolDecimal();
    cobD2 = new CobolDecimal();
    cobD3 = new CobolDecimal();
    cobD4 = new CobolDecimal();
    cobMexp = BigDecimal.ZERO;
    for (int i = 0; i < COB_MAX_BINARY; ++i) {
      cobMpze10[i] = BigDecimal.ZERO;
      cobMpze10[i] = BigDecimal.TEN.pow(i);
    }
    for (int i = 0; i < packedValue.length; ++i) {
      packedValue[i] = 0;
    }
    packedValueInt = 0;
  }

  // TODO cob_init_numeric周辺の初期化処理を正しく実装出来たら消す。
  static {
    for (int i = 0; i < COB_MAX_BINARY; ++i) {
      cobMpze10[i] = BigDecimal.ZERO;
      cobMpze10[i] = BigDecimal.TEN.pow(i);
    }
    for (int i = 0; i < packedValue.length; ++i) {
      packedValue[i] = 0;
    }
    packedValueInt = 0;
  }

  /** 保持する数値データ */
  BigDecimal value;

  /** TODO: 準備中 */
  int scale;

  /** 値とスケールは0に設定する */
  public CobolDecimal() {
    this.value = BigDecimal.ZERO;
    this.setScale(0);
  }

  /**
   * 値はvalueを指定し,スケールは0に設定する
   *
   * @param value 設定する値
   */
  public CobolDecimal(BigDecimal value) {
    this.setValue(value);
    this.setScale(0);
  }

  /**
   * 値とスケールを指定する
   *
   * @param value 設定する値
   * @param scale 設定するスケール
   */
  public CobolDecimal(BigDecimal value, int scale) {
    this(value);
    this.setScale(scale);
  }

  /**
   * 値を指定し,スケールは0に設定する
   *
   * @param n TODO: 準備中
   */
  public CobolDecimal(long n) {
    if (n == 0L) {
      this.setValue(BigDecimal.ZERO);
    } else if (n == 1L) {
      this.setValue(BigDecimal.ONE);
    } else if (n == 10L) {
      this.setValue(BigDecimal.TEN);
    } else {
      this.setValue(new BigDecimal(n));
    }
    this.setScale(0);
  }

  /**
   * 値を設定し,スケールは0に設定する
   *
   * @param n TODO: 準備中
   */
  public CobolDecimal(int n) {
    if (n == 0) {
      this.setValue(BigDecimal.ZERO);
    } else if (n == 1) {
      this.setValue(BigDecimal.ONE);
    } else if (n == 10) {
      this.setValue(BigDecimal.TEN);
    } else {
      this.setValue(new BigDecimal(n));
    }
    this.setScale(0);
  }

  /**
   * コピーコンストラクタ
   *
   * @param other TODO: 準備中
   */
  public CobolDecimal(CobolDecimal other) {
    this.setValue(other.getValue());
    this.setScale(other.getScale());
  }

  /**
   * このオブジェクトの値を取得する
   *
   * @return このオブジェクトの値
   */
  public BigDecimal getValue() {
    return value;
  }

  /** TODO: 準備中 */
  public void decimalInit() {
    this.value = BigDecimal.ZERO;
  }

  /**
   * このオブジェクトの値を設定する
   *
   * @param value このオブジェクトに設定する値
   */
  public void setValue(BigDecimal value) {
    this.value = value;
  }

  /** this.valueの値を0にする */
  public void clear() {
    this.value = BigDecimal.ZERO;
  }

  /**
   * このオブジェクトのスケールを設定する
   *
   * @param scale このオブジェクトに設定するスケール
   */
  public void setScale(int scale) {
    this.scale = scale;
  }

  /**
   * このオブジェクトのスケールを取得する
   *
   * @return このオブジェクトのスケール
   */
  public int getScale() {
    return this.scale;
  }

  /**
   * このオブジェクトの値を設定し、スケールは0に設定する
   *
   * @param n このオブジェクトに設定する値
   */
  public void set(int n) {
    this.value = new BigDecimal(n);
    this.scale = 0;
  }

  /**
   * このオブジェクトの値を設定し、スケールは0に設定する
   *
   * @param n このオブジェクトに設定する値
   */
  public void set(long n) {
    this.value = new BigDecimal(n);
    this.scale = 0;
  }

  /**
   * TODO: 準備中
   *
   * @param decimal TODO: 準備中
   */
  public void set(CobolDecimal decimal) {
    // TODO よりよいコピーの方法を考える
    this.value = decimal.value.add(BigDecimal.ZERO);
    this.setScale(decimal.getScale());
  }

  /**
   * TODO: 準備中
   *
   * @param f TODO: 準備中
   */
  public void setField(AbstractCobolField f) {
    CobolDecimal decimal = f.getDecimal();
    this.setValue(decimal.getValue());
    this.setScale(decimal.getScale());
  }

  /**
   * TODO: 準備中
   *
   * @param d1 TODO: 準備中
   * @param d2 TODO: 準備中
   */
  private static boolean DECIMAL_CHECK(CobolDecimal d1, CobolDecimal d2) {
    if (d1.getScale() == DECIMAL_NAN || d2.getScale() == DECIMAL_NAN) {
      d1.setScale(DECIMAL_NAN);
      return true;
    }
    return false;
  }

  /**
   * このオブジェクトの示す数値に対して加算を行う
   *
   * @param n このオブジェクトに加算する値
   */
  public void add(int n) {
    this.value = this.value.add(new BigDecimal(n));
  }

  /**
   * このオブジェクトの示す数値に対して加算を行う
   *
   * @param decimal このオブジェクトに加算する値
   */
  public void add(CobolDecimal decimal) {
    if (DECIMAL_CHECK(this, decimal)) {
      return;
    }
    alignDecimal(this, decimal);
    this.setValue(this.getValue().add(decimal.getValue()));
  }

  /**
   * このオブジェクトの示す数値に対して減算を行う
   *
   * @param decimal このオブジェクトの示す数値から減算する値
   */
  public void sub(CobolDecimal decimal) {
    if (DECIMAL_CHECK(this, decimal)) {
      return;
    }
    alignDecimal(this, decimal);
    this.setValue(this.getValue().subtract(decimal.getValue()));
  }

  /**
   * このオブジェクトの示す数値に対して減算を行う
   *
   * @param decimal このオブジェクトの示す数値から減算する値
   */
  public void sub(int n) {
    this.value = this.value.subtract(new BigDecimal(n));
  }

  /**
   * このオブジェクトの示す数値に対して乗算を行う
   *
   * @param decimal このオブジェクトの示す数値に乗算する値
   */
  public void mul(CobolDecimal decimal) {
    if (DECIMAL_CHECK(this, decimal)) {
      return;
    }
    this.setScale(this.getScale() + decimal.getScale());
    this.setValue(this.getValue().multiply(decimal.getValue()));
  }

  /**
   * このオブジェクトの示す数値に対して乗算を行う
   *
   * @param n このオブジェクトの示す数値に乗算する値
   */
  public void mul(int n) {
    this.value = this.value.multiply(new BigDecimal(n));
  }

  /**
   * このオブジェクトの示す数値に対してmod演算を行う
   *
   * @param decimal このオブジェクトの示す数値にmod演算する値
   * @throws CobolStopRunException TODO: 準備中
   */
  public void div(CobolDecimal decimal) throws CobolStopRunException {
    if (DECIMAL_CHECK(this, decimal)) {
      return;
    }
    if (decimal.getValue().signum() == 0) {
      this.setScale(DECIMAL_NAN);
      if (CobolUtil.cobErrorOnExitFlag) {
        CobolUtil.runtimeError("Detected division by zero.");
        CobolStopRunException.stopRunAndThrow(1);
        ;
      }
      return;
    }
    if (this.getValue().signum() == 0) {
      this.setScale(0);
      return;
    }
    this.setScale(this.getScale() - decimal.getScale());
    int shift = 37 + ((this.getScale() < 0) ? -this.getScale() : 0);
    this.shiftDecimal(shift);
    this.setValue(this.getValue().divide(decimal.getValue(), RoundingMode.DOWN));
  }

  /**
   * このオブジェクトの示す数値に対して除算を行う
   *
   * @param n このオブジェクトの示す数値を除算する値
   */
  public void div(int n) {
    this.value = this.value.divide(new BigDecimal(n), RoundingMode.DOWN);
  }

  /**
   * このオブジェクトの示す数値に対して累乗を行う
   *
   * @param decimal このオブジェクトの示す数値を累乗する値
   */
  // TODO 残りの実装
  public void pow(CobolDecimal decimal) {
    if (DECIMAL_CHECK(this, decimal)) {
      return;
    }

    if (decimal.getScale() == 0 && decimal.getValue().compareTo(new BigDecimal(2147483647)) <= 0) {
      int n = decimal.getValue().intValue();
      this.value = this.value.pow(n);
      this.setScale(this.getScale() * n);
    } else {
      this.decimalSetDouble(Math.pow(this.decimalGetDouble(), decimal.decimalGetDouble()));
    }
  }

  /**
   * TODO: 準備中
   *
   * @param v TODO: 準備中
   */
  private void decimalSetDouble(double v) {
    this.setValue(new BigDecimal(v * 1.0e9));
    this.setScale(9);
  }

  private double decimalGetDouble() {
    double v = this.getValue().doubleValue();
    int n = this.getScale();
    for (; n > 0; n--) {
      v /= 10;
    }
    for (; n < 0; n++) {
      v *= 10;
    }
    return v;
  }

  /**
   * TODO: 準備中
   *
   * @param f TODO: 準備中
   * @param opt TODO: 準備中
   * @return TODO: 準備中
   * @throws CobolStopRunException TODO: 準備中
   */
  public int getField(AbstractCobolField f, int opt) throws CobolStopRunException {
    if (this.getScale() == CobolDecimal.DECIMAL_NAN) {
      CobolRuntimeException.setException(CobolExceptionId.COB_EC_SIZE_OVERFLOW);
      return CobolRuntimeException.code;
    }

    CobolDecimal d = new CobolDecimal(this);

    /* rounding */
    if ((opt & CobolDecimal.COB_STORE_ROUND) != 0) {
      if (f.getAttribute().getScale() < d.getScale()) {
        int sign = d.value.signum();
        if (sign != 0) {
          d.shiftDecimal(f.getAttribute().getScale() - d.getScale() + 1);
          if (sign > 0) {
            d.add(5);
          } else {
            d.sub(5);
          }
        }
      }
    }

    d.shiftDecimal(f.getAttribute().getScale() - d.getScale());

    // TODO 残りのパターンも実装
    switch (f.getAttribute().getType()) {
      case CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY:
        return d.getDisplayField(f, opt);
      case CobolFieldAttribute.COB_TYPE_NUMERIC_PACKED:
        return d.getPackedField(f, opt);
      case CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY:
        return d.getBinaryField(f, opt);
      case CobolFieldAttribute.COB_TYPE_NUMERIC_FLOAT:
        System.out.println("getField: Float not implemented");
        throw new CobolRuntimeException(0, "getField: Float not implemented");
      case CobolFieldAttribute.COB_TYPE_NUMERIC_DOUBLE:
        return d.getDoubleField(f, opt);
      default:
        int digits = f.getAttribute().getDigits();
        CobolFieldAttribute attr = f.getAttribute();
        CobolFieldAttribute newAttr =
            new CobolFieldAttribute(
                CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY,
                digits,
                attr.getScale(),
                CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
                null);
        AbstractCobolField displayField =
            CobolFieldFactory.makeCobolField(digits, new CobolDataStorage(digits), newAttr);
        if (d.getField(displayField, opt) == 0) {
          f.moveFrom(displayField);
        }
        return CobolExceptionInfo.code;
    }
  }

  /**
   * TODO: 準備中
   *
   * @param f TODO: 準備中
   * @param opt TODO: 準備中
   * @return TODO: 準備中
   */
  public int getDoubleField(AbstractCobolField f, int opt) {
    CobolDataStorage storage = new CobolDataStorage(8);
    double val = this.value.doubleValue();
    int scale = this.scale;
    for (int i = 0; i < Math.abs(scale); ++i) {
      if (scale > 0) {
        val /= 10;
      } else {
        val *= 10;
      }
    }
    storage.set(val);
    f.setDataStorage(storage);
    return 0;
  }

  /**
   * TODO: 準備中
   *
   * @param n TODO: 準備中
   */
  public void shiftDecimal(int n) {
    if (n == 0) {
      return;
    }
    if (n > 0) {
      cobMexp = BigDecimal.TEN.pow(n);
      this.value = this.value.multiply(cobMexp);
    } else {
      cobMexp = BigDecimal.TEN.pow(-n);
      this.value = this.value.divide(cobMexp, RoundingMode.DOWN);
    }
    this.setScale(this.getScale() + n);
  }

  // libcob/numeric.cのalign_decimalの実装
  /**
   * TODO: 準備中
   *
   * @param d1 TODO: 準備中
   * @param d2 TODO: 準備中
   */
  public void alignDecimal(CobolDecimal d1, CobolDecimal d2) {
    if (d1.getScale() < d2.getScale()) {
      d1.shiftDecimal(d2.getScale() - d1.getScale());
    } else if (d1.getScale() > d2.getScale()) {
      d2.shiftDecimal(d1.getScale() - d2.getScale());
    }
  }

  // libcob/numeric.cのcob_decimal_cmpの実装 引数で与えられたCobolDecimal型のインスタンスとの比較をする.
  /**
   * TODO: 準備中
   *
   * @param decimal TODO: 準備中
   * @return TODO: 準備中
   */
  public int compareTo(CobolDecimal decimal) {
    alignDecimal(this, decimal);
    BigDecimal v1 = this.getValue().movePointLeft(this.getScale());
    BigDecimal v2 = decimal.getValue().movePointLeft(decimal.getScale());
    return v1.compareTo(v2);
  }

  // libcob/numeric.cのcob_decimal_get_displayの実装
  /**
   * TODO: 準備中
   *
   * @param f TODO: 準備中
   * @param opt TODO: 準備中
   * @return TODO: 準備中
   * @throws CobolStopRunException TODO: 準備中
   */
  public int getDisplayField(AbstractCobolField f, int opt) throws CobolStopRunException {
    int sign = this.value.signum();
    this.value = this.value.abs();
    String numString = this.value.toPlainString();
    int dPointIndex = numString.indexOf('.');
    numString = numString.replace(".", "");
    byte[] numBuffPtr = numString.getBytes();
    if (dPointIndex < 0) {
      dPointIndex = numBuffPtr.length;
    }
    dPointIndex -= this.scale;
    int size = numBuffPtr.length;

    CobolDataStorage data = f.getDataStorage();
    int diff = f.getFieldSize() - size;

    if (diff < 0) {
      CobolRuntimeException.setException(CobolExceptionId.COB_EC_SIZE_OVERFLOW);
      if ((opt & CobolDecimal.COB_STORE_KEEP_ON_OVERFLOW) > 0) {
        return CobolRuntimeException.code;
      }
    }

    int fFirstIndex = f.getFirstDataIndex();
    int fFieldSize = f.getFieldSize();
    int fPointIndex = fFieldSize - f.getAttribute().getScale();
    for (int i = 0; i < fFieldSize; i++) {
      int fIndex = fFirstIndex + i;
      int dIndex = i + dPointIndex - fPointIndex;
      if (0 <= dIndex && dIndex < numBuffPtr.length) {
        data.setByte(fIndex, numBuffPtr[dIndex]);
      } else {
        data.setByte(fIndex, (byte) '0');
      }
    }
    f.putSign(sign);
    return 0;
  }

  // libcob/numeric.cのcob_decimal_get_packedの実装
  /**
   * TODO: 準備中
   *
   * @param f TODO: 準備中
   * @param opt TODO: 準備中
   * @return TODO: 準備中
   */
  public int getPackedField(AbstractCobolField f, int opt) {
    int sign = this.value.signum();
    this.value = this.value.abs();
    String numString = this.value.toPlainString();
    int dPointIndex = numString.indexOf('.');
    numString = numString.replace(".", "");
    byte[] numBuffPtr = numString.getBytes();
    if (dPointIndex < 0) {
      dPointIndex = numBuffPtr.length;
    }
    dPointIndex -= this.scale;
    int size = numBuffPtr.length;
    int fPointIndex = f.getAttribute().getDigits() - f.getAttribute().getScale();

    CobolDataStorage data = f.getDataStorage();
    int digits = f.getAttribute().getDigits();
    int diff = digits - size;
    if (diff < 0) {
      CobolRuntimeException.setException(CobolExceptionId.COB_EC_SIZE_OVERFLOW);
      if ((opt & CobolDecimal.COB_STORE_KEEP_ON_OVERFLOW) > 0) {
        return CobolRuntimeException.code;
      }
    }
    data.fillBytes(0, f.getSize());
    for (int i = 0; i < digits; ++i) {
      int j = i - fPointIndex + dPointIndex;
      if (j < 0 || numBuffPtr.length <= j) {
        continue;
      }
      byte val = (byte) ((int) numBuffPtr[j] - '0');
      int index;
      if (digits % 2 == 0) {
        index = (i + 1) / 2;
      } else {
        index = i / 2;
      }
      byte b = data.getByte(index);
      if ((digits + i) % 2 == 0) {
        data.setByte(index, (b & 0xF0) | val);
      } else {
        data.setByte(index, (val << 4) | (b & 0x0F));
      }
    }

    int p = f.getSize() - 1;
    byte x = data.getByte(p);
    if (!f.getAttribute().isFlagHaveSign()) {
      data.setByte(p, (byte) ((x & 0xF0) | 0x0F));
    } else if (sign < 0) {
      data.setByte(p, (byte) ((x & 0xF0) | 0x0D));
    } else {
      data.setByte(p, (byte) ((x & 0xF0) | 0x0C));
    }

    return 0;
  }

  // libcob/numeric.cのcob_decimal_get_binaryの実装
  /**
   * TODO: 準備中
   *
   * @param f TODO: 準備中
   * @param opt TODO: 準備中
   * @return TODO: 準備中
   */
  private int getBinaryField(AbstractCobolField f, int opt) {
    CobolDataStorage data = f.getDataStorage();
    CobolFieldAttribute attr = f.getAttribute();
    if (this.getValue().signum() == 0) {
      data.fillBytes(0, f.getSize());
      return 0;
    }
    int overflow = 0;
    int digits = attr.getDigits();
    int sign;
    if (attr.isFlagHaveSign()) {
      sign = 1;
    } else {
      sign = 0;
      if (this.value.signum() < 0) {
        this.value = this.value.abs();
      }
    }
    int bitnum = f.getSize() * 8 - sign;
    outer:
    {
      if (this.getValue().compareTo(new BigDecimal(2).pow(bitnum).subtract(BigDecimal.ONE)) > 0) {
        if ((opt & COB_STORE_KEEP_ON_OVERFLOW) != 0) {
          break outer;
        }
        overflow = 1;
        if ((opt & COB_STORE_TRUNC_ON_OVERFLOW) != 0) {
          this.setValue(this.getValue().remainder(cobMpze10[digits]));
        } else {
          this.setValue(this.getValue().remainder(new BigDecimal(2).pow(f.getSize() * 8)));
        }
      } else if ((opt != 0) && CobolModule.getCurrentModule().flag_binary_truncate != 0) {
        if (this.getValue().abs().compareTo(cobMpze10[digits].abs()) >= 0) {
          if ((opt & COB_STORE_KEEP_ON_OVERFLOW) != 0) {
            break outer;
          }
          overflow = 1;
          if ((opt & COB_STORE_TRUNC_ON_OVERFLOW) != 0) {
            this.setValue(this.getValue().remainder(cobMpze10[digits]));
          } else {
            this.setValue(this.getValue().remainder(new BigDecimal(2).pow(f.getFieldSize() * 8)));
          }
        }
      }
      ((CobolNumericBinaryField) f).setLongValue(this.getValue().longValue());
      if (overflow == 0) {
        return 0;
      }
    }
    CobolRuntimeException.setException(CobolExceptionId.COB_EC_SIZE_OVERFLOW);
    return CobolExceptionInfo.code;
  }

  // libcob/numeric.cのnum_byte_memcpyの実装
  /**
   * TODO: 準備中
   *
   * @param s1 TODO: 準備中
   * @param s1StartIndex TODO: 準備中
   * @param s2 TODO: 準備中
   * @param s2StartIndex TODO: 準備中
   * @param size TODO: 準備中
   */
  public static void numByteMemcpy(
      CobolDataStorage s1, int s1StartIndex, CobolDataStorage s2, int s2StartIndex, int size) {
    int i1 = s1StartIndex;
    int i2 = s2StartIndex;
    do {
      s1.setByte(i1++, s2.getByte(i2++));
    } while (--size != 0);
  }
}
