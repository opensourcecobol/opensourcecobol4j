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

/** BigDecimalを扱うクラス COMPUTE等で計算をするときに使用する */
public class CobolDecimal {
  public static final int DECIMAL_NAN = -128;
  public static final int COB_STORE_ROUND = 0x01;
  public static final int COB_STORE_KEEP_ON_OVERFLOW = 0x02;
  public static final int COB_STORE_TRUNC_ON_OVERFLOW = 0x04;

  public static final int COB_MAX_BINARY = 36;

  private static BigDecimal cobMexp = BigDecimal.ZERO;
  public static CobolDecimal cobD1 = new CobolDecimal();
  public static CobolDecimal cobD2 = new CobolDecimal();
  public static CobolDecimal cobD3 = new CobolDecimal();
  public static CobolDecimal cobD4 = new CobolDecimal();
  public static BigDecimal[] cobMpze10 = new BigDecimal[COB_MAX_BINARY];
  public static byte[] packedValue = new byte[20];
  public static int packedValueInt = 0;

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
  public BigDecimal value;

  public int scale;

  /** コンストラクタ this.valueは0に設定する */
  public CobolDecimal() {
    this.value = BigDecimal.ZERO;
    this.setScale(0);
  }

  /**
   * コンストラクタ this.valueを引数で指定された値に設定する.
   *
   * @param value
   */
  public CobolDecimal(BigDecimal value) {
    this.setValue(value);
    this.setScale(0);
  }

  /**
   * コンストラクタ this.valueを指定された値に設定し,scaleも指定された値に設定する.
   *
   * @param value
   * @param scale
   */
  public CobolDecimal(BigDecimal value, int scale) {
    this(value);
    this.setScale(scale);
  }

  /**
   * コンストラクタ. this.valueを指定されたint型の値に対応する値に設定する
   *
   * @param n
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
   * コンストラクタ. this.valueを指定されたint型の値に対応する値に設定する
   *
   * @param n
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
   * @param other
   */
  public CobolDecimal(CobolDecimal other) {
    this.setValue(other.getValue());
    this.setScale(other.getScale());
  }

  /**
   * this.valueのgetter
   *
   * @return this.value
   */
  public BigDecimal getValue() {
    return value;
  }

  /** libcob/numeric.cのcob_decimal_initの実装 */
  public void decimalInit() {
    this.value = BigDecimal.ZERO;
    this.value.setScale(0);
  }

  /**
   * this.valueのsetter
   *
   * @param value this.valueに設定する値
   */
  public void setValue(BigDecimal value) {
    this.value = value;
  }

  /** this.valueの値を0にする */
  public void clear() {
    this.value = BigDecimal.ZERO;
  }

  /**
   * this.valueのスケールを設定する
   *
   * @param scale スケール値
   */
  public void setScale(int scale) {
    this.scale = scale;
  }

  /**
   * this.valueのスケールを取得する
   *
   * @return this.valueのスケール
   */
  public int getScale() {
    return this.scale;
  }

  /**
   * this.valueを引数で指定されたint型変数の値に対応する値に設定する
   *
   * @param n this.valueに設定する値
   */
  public void set(int n) {
    this.value = new BigDecimal(n);
    this.scale = 0;
  }

  /**
   * this.valueを引数で指定されたint型変数の値に対応する値に設定する
   *
   * @param n this.valueに設定する値
   */
  public void set(long n) {
    this.value = new BigDecimal(n);
    this.scale = 0;
  }

  /** libcob/numeric.cのcob_decimal_setの実装 */
  public void set(CobolDecimal decimal) {
    // TODO よりよいコピーの方法を考える
    this.value = decimal.value.add(BigDecimal.ZERO);
    this.setScale(decimal.getScale());
  }

  /**
   * libcob/numeric.cのcob_decimal_set_fieldの実装
   *
   * @param f
   */
  public void setField(AbstractCobolField f) {
    CobolDecimal decimal = f.getDecimal();
    this.setValue(decimal.getValue());
    this.setScale(decimal.getScale());
  }

  /**
   * libcob/numeric.cのcob_decimal_set_displayの実装
   *
   * @param f
   */
  /*
   * public void decimalSetDisplay(AbstractCobolField f) {
   * int firstIndex = f.getFirstDataIndex();
   * int p = 0;
   * int size = f.getFieldSize();
   *
   * CobolDataStorage data = f.getDataStorage();
   * if(data.getByte(firstIndex + p) == 255) {
   * this.value = BigDecimal.TEN.pow(size);
   * this.setScale(f.getAttribute().getScale());
   * return;
   * }
   * if(data.getByte(firstIndex + p) == 0) {
   * this.value = BigDecimal.TEN.pow(size).negate();
   * this.setScale(f.getAttribute().getScale());
   * return;
   * }
   * int sign = f.getSign();
   * // skip leading zeros
   * while(size > 1 && data.getByte(p) == '0') {
   * size--;
   * p++;
   * }
   *
   * // set value
   * if(size < 10) {
   * int n = 10;
   * while(size-- != 0) {
   * n = n * 10 + data.getByte(firstIndex + p++) - '0';
   * }
   * this.set(n);
   * } else {
   * byte[] numBuffPtr = new byte[size];
   * for(int i=0; i<size; ++i) {
   * numBuffPtr[i] = data.getByte(firstIndex + i);
   * }
   * this.value = new BigDecimal(new String(numBuffPtr));
   * }
   *
   * if(sign < 0) {
   * this.value = this.value.negate();
   * }
   * this.setScale(f.getAttribute().getScale());
   * f.putSign(sign);
   * }
   */

  /**
   * libcob/numeric.cのDECIMAL_CHECKマクロの代替
   *
   * @param d1
   * @param d2
   */
  private static boolean DECIMAL_CHECK(CobolDecimal d1, CobolDecimal d2) {
    if (d1.getScale() == DECIMAL_NAN || d2.getScale() == DECIMAL_NAN) {
      d1.setScale(DECIMAL_NAN);
      return true;
    }
    return false;
  }

  /**
   * this.valueの値をthis.valueとnを加算した値にする
   *
   * @param n other this.valueに加算される値
   */
  public void add(int n) {
    this.value = this.value.add(new BigDecimal(n));
  }

  /**
   * this.valueの値をthis.valueとotherを加算した値にする
   *
   * @param other this.valueに加算される値
   */
  public void add(CobolDecimal decimal) {
    if (DECIMAL_CHECK(this, decimal)) {
      return;
    }
    alignDecimal(this, decimal);
    this.setValue(this.getValue().add(decimal.getValue()));
  }

  /**
   * this.valueの値をthis.valueからotherを減算した値にする
   *
   * @param other this.valueから減算される値
   */
  public void sub(CobolDecimal decimal) {
    if (DECIMAL_CHECK(this, decimal)) {
      return;
    }
    alignDecimal(this, decimal);
    this.setValue(this.getValue().subtract(decimal.getValue()));
  }

  /**
   * this.valueの値をthis.valueからnを減算した値にする
   *
   * @param n this.valueから減算される値
   */
  public void sub(int n) {
    this.value = this.value.subtract(new BigDecimal(n));
  }

  /**
   * this.valueの値をthis.valueとotherを乗算した値にする
   *
   * @param other this.valueに乗算される値
   */
  public void mul(CobolDecimal decimal) {
    if (DECIMAL_CHECK(this, decimal)) {
      return;
    }
    this.setScale(this.getScale() + decimal.getScale());
    this.setValue(this.getValue().multiply(decimal.getValue()));
  }

  /**
   * this.valueの値をthis.valueとnを乗算した値にする
   *
   * @param n this.valueに乗算される値
   */
  public void mul(int n) {
    this.value = this.value.multiply(new BigDecimal(n));
  }

  /**
   * this.valueの値をthis.valueでotherを割った値にする
   *
   * @param other this.valueを割る数
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
   * this.valueの値をthis.valueでnを割った値にする
   *
   * @param n this.valueを割る数
   */
  public void div(int n) {
    this.value = this.value.divide(new BigDecimal(n), RoundingMode.DOWN);
  }

  /**
   * this.valueの値をthis.valueをdecimal乗した値にする
   *
   * @param other this.valueを割る数
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
   * libcob/numeric.cのcob_decimal_set_doubleの実装
   *
   * @param v
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
   * this.valueの値をthis.valueをn乗した値にする
   *
   * @param other this.valueを割る数
   */

  /**
   * libcob/numeric.c cob_decimal_get_fieldの実装 AbstractCobolFieldの保持する値をthisに設定する
   *
   * @param f
   * @param opt
   * @return
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
   * libcob/numeric.c shift_decimalの実装 this.valueを値10^n,スケールをn増加させる
   *
   * @param n
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

  /** libcob/numeric.cのalign_decimalの実装 */
  public void alignDecimal(CobolDecimal d1, CobolDecimal d2) {
    if (d1.getScale() < d2.getScale()) {
      d1.shiftDecimal(d2.getScale() - d1.getScale());
    } else if (d1.getScale() > d2.getScale()) {
      d2.shiftDecimal(d1.getScale() - d2.getScale());
    }
  }

  /**
   * libcob/numeric.cのcob_decimal_cmpの実装 引数で与えられたCobolDecimal型のインスタンスとの比較をする.
   *
   * @param decimal thisと比較する対象
   * @return thisのほうが大きいときは正の値,thisのほうが小さいときあ負の値,それ以外は0
   */
  public int compareTo(CobolDecimal decimal) {
    alignDecimal(this, decimal);
    BigDecimal v1 = this.getValue().movePointLeft(this.getScale());
    BigDecimal v2 = decimal.getValue().movePointLeft(decimal.getScale());
    return v1.compareTo(v2);
  }

  /**
   * libcob/numeric.cのcob_decimal_get_displayの実装
   *
   * @param f
   * @param opt
   * @return
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

  /**
   * libcob/numeric.cのcob_decimal_get_packedの実装
   *
   * @param f
   * @param opt
   * @return
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

  /**
   * libcob/numeric.cのcob_decimal_get_binaryの実装
   *
   * @param f
   * @param opt
   * @return
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

  /**
   * libcob/numeric.cのnum_byte_memcpyの実装
   *
   * @param s1
   * @param s1StartIndex s1のコピー開始位置
   * @param s2
   * @param s2StartIndex s1のコピー開始位置
   * @param size
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
