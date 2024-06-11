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

import java.io.UnsupportedEncodingException;
import java.math.BigDecimal;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolRuntimeException;

/** PIC 文字列が9(5) COMP-3などの変数を表現するクラス. */
public class CobolNumericPackedField extends AbstractCobolField {
  private static final byte[] packed_bytes = {
    0x00,
    0x01,
    0x02,
    0x03,
    0x04,
    0x05,
    0x06,
    0x07,
    0x08,
    0x09,
    0x10,
    0x11,
    0x12,
    0x13,
    0x14,
    0x15,
    0x16,
    0x17,
    0x18,
    0x19,
    0x20,
    0x21,
    0x22,
    0x23,
    0x24,
    0x25,
    0x26,
    0x27,
    0x28,
    0x29,
    0x30,
    0x31,
    0x32,
    0x33,
    0x34,
    0x35,
    0x36,
    0x37,
    0x38,
    0x39,
    0x40,
    0x41,
    0x42,
    0x43,
    0x44,
    0x45,
    0x46,
    0x47,
    0x48,
    0x49,
    0x50,
    0x51,
    0x52,
    0x53,
    0x54,
    0x55,
    0x56,
    0x57,
    0x58,
    0x59,
    0x60,
    0x61,
    0x62,
    0x63,
    0x64,
    0x65,
    0x66,
    0x67,
    0x68,
    0x69,
    0x70,
    0x71,
    0x72,
    0x73,
    0x74,
    0x75,
    0x76,
    0x77,
    0x78,
    0x79,
    (byte) 0x80,
    (byte) 0x81,
    (byte) 0x82,
    (byte) 0x83,
    (byte) 0x84,
    (byte) 0x85,
    (byte) 0x86,
    (byte) 0x87,
    (byte) 0x88,
    (byte) 0x89,
    (byte) 0x90,
    (byte) 0x91,
    (byte) 0x92,
    (byte) 0x93,
    (byte) 0x94,
    (byte) 0x95,
    (byte) 0x96,
    (byte) 0x97,
    (byte) 0x98,
    (byte) 0x99
  };

  /**
   * コンストラクタ
   *
   * @param size データを格納するバイト配列の長さ
   * @param dataStorage データを格納するバイト配列を扱うオブジェクト
   * @param attribute 変数に関する様々な情報を保持するオブジェクト(符号付か,COMP-3指定かなど)
   */
  public CobolNumericPackedField(
      int size, CobolDataStorage dataStorage, CobolFieldAttribute attribute) {
    super(size, dataStorage, attribute);
  }

  /**
   * TODO: 準備中
   *
   * @param s TODO: 準備中
   */
  public void checkNumeric(String s) {}

  /** this.dataの保持するバイト配列のコピーを返す */
  @Override
  public byte[] getBytes() {
    return dataStorage.getData();
  }

  /**
   * thisの文字列表現をかえす.(toStringだけで十分か?)
   *
   * @return thisの文字列表現
   */
  @Override
  public String getString() {
    StringBuilder sb = new StringBuilder();
    int digits = this.getAttribute().getDigits();
    int scale = this.getAttribute().getScale();
    int counter = digits - scale;
    int len;
    if (scale < 0) {
      len = digits + scale;
    } else {
      len = digits;
    }
    for (int i = 0; i < len; ++i, --counter) {
      if (counter == 0) {
        sb.append('.');
      }
      sb.append((char) (this.getDigit(i) + 0x30));
    }
    for (int i = 0; i < -scale; ++i) {
      sb.append('0');
    }

    if (this.getAttribute().isFlagHaveSign()) {
      if (this.getSign() < 0) {
        return "-" + sb;
      } else {
        return "+" + sb;
      }
    } else {
      return sb.toString();
    }
  }

  @Override
  public void setDecimal(BigDecimal decimal) {
    byte[] decimalBytes = decimal.toPlainString().getBytes();
    int length = Math.min(this.size, decimalBytes.length);

    for (int i = 0; i < length; ++i) {
      this.dataStorage.setByte(this.size - 1 - i, decimalBytes[decimalBytes.length - 1 - i]);
    }
  }

  /**
   * 引数で与えらえられたデータからthisへの代入を行う
   *
   * @param src 代入元のデータ(AbstractCobolField型)
   */
  @Override
  public void moveFrom(AbstractCobolField src) {
    AbstractCobolField src1 = this.preprocessOfMoving(src);
    if (src1 == null) {
      return;
    }

    switch (src1.getAttribute().getType()) {
      case CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY:
        this.moveDisplayToPacked(src1);
        break;
      case CobolFieldAttribute.COB_TYPE_NUMERIC_PACKED:
      case CobolFieldAttribute.COB_TYPE_ALPHANUMERIC:
      case CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY:
      case CobolFieldAttribute.COB_TYPE_NATIONAL:
      case CobolFieldAttribute.COB_TYPE_NUMERIC_EDITED:
      case CobolFieldAttribute.COB_TYPE_ALPHANUMERIC_EDITED:
      case CobolFieldAttribute.COB_TYPE_NATIONAL_EDITED:
        this.moveFrom(src1.getNumericField());
        break;
      case CobolFieldAttribute.COB_TYPE_GROUP:
        CobolAlphanumericField.moveAlphanumToAlphanum(this, src1);
        break;
      default:
        throw new CobolRuntimeException(0, "未実装");
    }
  }

  /**
   * CobolNumericFieldからthisへの代入を行う
   *
   * @param field 代入元のデータ(AbstractCobolField型)
   */
  private void moveDisplayToPacked(AbstractCobolField field) {
    int sign = field.getSign();
    CobolDataStorage data1 = field.getDataStorage();
    int data1FirstIndex = field.getFirstDataIndex();
    int digits1 = field.getAttribute().getDigits();
    int scale1 = field.getAttribute().getScale();
    CobolDataStorage data2 = this.getDataStorage();
    int digits2 = this.getAttribute().getDigits();
    int scale2 = this.getAttribute().getScale();
    int packedRealDigits;
    if (scale2 < 0) {
      packedRealDigits = digits2 + scale2;
    } else {
      packedRealDigits = digits2;
    }

    /* null check */
    boolean flagNull = true;
    for (int i = 0; i < digits1; ++i) {
      if (field.getDataStorage().getByte(i) != 0) {
        flagNull = false;
      }
    }
    if (flagNull) {
      this.getDataStorage().fillBytes((byte) 0, this.getSize());
      return;
    }

    /* pack string */
    this.getDataStorage().fillBytes((byte) 0, this.getSize());
    int offset = 1 - (digits2 % 2);
    int p;
    if (scale2 < 0) {
      p = (digits1 - scale1) - digits2;
    } else {
      p = (digits1 - scale1) - (digits2 - scale2);
    }

    boolean isZero = true;
    for (int i = offset; i < offset + packedRealDigits; ++i, ++p) {
      byte n;
      int index1 = data1FirstIndex + p;
      if (index1 >= field.getSize() || index1 < data1FirstIndex) {
        n = 0;
      } else {
        byte ch = data1.getByte(index1);
        if (ch == (byte) 0x20) {
          n = 0;
        } else if (ch >= 0x70) {
          n = (byte) (ch - 0x70);
        } else {
          n = (byte) (ch - 0x30);
        }
      }
      isZero &= n == 0;
      if (i % 2 == 0) {
        data2.setByte(i / 2, (byte) (n << 4));
      } else {
        byte value = data2.getByte(i / 2);
        data2.setByte(i / 2, (byte) (value | n));
      }
    }

    p = this.size - 1;
    byte value = this.getDataStorage().getByte(p);
    if ((this.getAttribute().getFlags() & CobolFieldAttribute.COB_FLAG_HAVE_SIGN) == 0) {
      this.getDataStorage().setByte(p, (byte) ((value & 0xf0) | 0x0f));
    } else if (sign < 0 && !isZero) {
      this.getDataStorage().setByte(p, (byte) ((value & 0xf0) | 0x0d));
    } else {
      this.getDataStorage().setByte(p, (byte) ((value & 0xf0) | 0x0c));
    }
  }

  /**
   * 引数で与えらえられたデータからthisへの代入を行う
   *
   * @param bytes 代入元のデータ(byte[]型)
   */
  @Override
  public void moveFrom(byte[] bytes) {
    this.dataStorage.setData(bytes);
  }

  /**
   * 引数で与えらえられたデータからthisへの代入を行う
   *
   * @param string 代入元のデータ(String型)
   */
  @Override
  public void moveFrom(String string) {
    try {
      this.dataStorage.setData(string.getBytes("SJIS"));
    } catch (UnsupportedEncodingException e) {
      e.printStackTrace();
    }
  }

  /**
   * libcob/numeric.cのcob_set_packed_intの実装 引数で与えらえられたデータからthisへの代入を行う
   *
   * @param val 代入元のデータ(int型)
   */
  @Override
  public void moveFrom(int val) {
    int n;
    int sign = 0;
    CobolDataStorage data = this.getDataStorage();

    if (val == Integer.MIN_VALUE) {
      // Integer.MIN_VALUE == -2147483647
      int[] bytes = {2, 1, 4, 7, 4, 8, 3, 6, 4, 8};
      int digits = this.getAttribute().getDigits();
      for (int i = 0; i < digits; ++i) {
        if (i < bytes.length) {
          this.setDigit(digits - 1 - i, bytes[bytes.length - 1 - i]);
        } else {
          this.setDigit(digits - 1 - i, 0);
        }
      }
      this.putSign(-1);
      return;
    } else if (val < 0) {
      n = -val;
      sign = 1;
    } else {
      n = val;
    }
    this.getDataStorage().fillBytes(0, this.getSize());
    int p = this.getSize() - 1;
    byte b = data.getByte(p);
    b = (byte) ((n % 10) << 4);
    if (!this.getAttribute().isFlagHaveSign()) {
      b |= 0x0f;
    } else if (sign != 0) {
      b |= 0x0d;
    } else {
      b |= 0x0c;
    }
    data.setByte(p, b);
    n /= 10;
    p--;

    for (; n != 0 && p >= 0; n /= 100, p--) {
      data.setByte(p, packed_bytes[n % 100]);
    }

    /* Fixme */
    if (this.getAttribute().getDigits() % 2 == 0) {
      data.setByte(0, (byte) (data.getByte(0) & 0x0F));
    }
  }

  /**
   * 指定の桁に指定の値を設定する
   *
   * @param index 桁の
   * @param value 設定する値
   */
  private void setDigit(int index, int value) {
    // 符号を格納
    byte lastByte = this.dataStorage.getByte(this.size - 1);
    if (value < 0) {
      this.dataStorage.setByte(this.size - 1, (byte) ((lastByte & 0xF0) | 0x0D));
    } else {
      this.dataStorage.setByte(this.size - 1, (byte) ((lastByte & 0xF0) | 0x0C));
    }

    int i = this.getByteIndex(index);

    if ((this.getAttribute().getDigits() + index) % 2 == 0) {
      byte x = this.dataStorage.getByte(i);
      x = (byte) ((x & (0x0F << 4)) | value);
      this.dataStorage.setByte(i, x);
    } else {
      byte x = this.dataStorage.getByte(i);
      x = (byte) ((x & 0x0F) | (value << 4));
      this.dataStorage.setByte(i, x);
    }
  }

  /**
   * 指定のけたの値を取得する
   *
   * @param index 桁
   * @return index桁目の値
   */
  private int getDigit(int index) {
    int i = this.getByteIndex(index);

    if ((this.getAttribute().getDigits() + index) % 2 == 0) {
      return (byte) (this.dataStorage.getByte(i) & 0x0F);
    } else {
      return (byte) ((this.dataStorage.getByte(i) >>> 4) & 0x0F);
    }
  }

  /**
   * 指定した桁の値を取得する
   *
   * @param index 取得する桁の位置
   * @return index番目の桁の値
   */
  private int getByteIndex(int index) {
    if (this.getAttribute().getDigits() % 2 == 0) {
      return (index + 1) / 2;
    } else {
      return index / 2;
    }
  }

  /**
   * thisをCobolNumericFieldに変換する. indirect moveをするときに使用されることを想定している.
   *
   * @return thisからCobolNumericField型へ変換した値
   */
  @Override
  public CobolNumericField getNumericField() {
    int size = this.getAttribute().getDigits();
    int scale = this.getAttribute().getScale();
    CobolFieldAttribute attr =
        new CobolFieldAttribute(
            CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY,
            size,
            scale,
            CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
            null);
    CobolDataStorage data = new CobolDataStorage(this.getAttribute().getDigits());
    CobolNumericField field = new CobolNumericField(size, data, attr);
    field.moveFrom(this);
    return field;
  }

  /**
   * 引数で与えらえられたデータからthisへの代入を行う
   *
   * @param number 代入元のデータ(BigDecimal型)
   */
  @Override
  public void moveFrom(BigDecimal number) {
    // TODO 自動生成されたメソッド・スタブ
  }

  /**
   * 引数で与えらえられたデータからthisへの代入を行う
   *
   * @param dataStorage 代入元のデータ(CobolDataStorage型)
   */
  @Override
  public void moveFrom(CobolDataStorage dataStorage) {
    // TODO 自動生成されたメソッド・スタブ
  }

  /**
   * libcob/numeric.cのcob_packed_get_signの実装 thisの保持する数値データの符号を返す
   *
   * @return thisの保持する数値データが負ならば負数,0なら0,正なら正数を返す
   */
  @Override
  public int getSign() {
    if (!this.getAttribute().isFlagHaveSign()) {
      return 0;
    }
    int index = this.size - 1;
    return (this.dataStorage.getByte(index) & 0x0f) == 0x0d ? -1 : 1;
  }

  /**
   * thisの保持する数値データの符号を設定する
   *
   * @param sign 正符号を設定するときは正数,負符号を設定するときは負数,それ以外は0
   */
  @Override
  public void putSign(int sign) {
    int index = this.getSize() - 1;
    byte value = this.dataStorage.getByte(index);
    if (sign < 0) {
      this.getDataStorage().setByte(index, (byte) ((value & 0xf0) | 0x0d));
    } else {
      this.getDataStorage().setByte(index, (byte) ((value & 0xf0) | 0x0c));
    }
  }

  /**
   * 数値を表すデータが実装すべきメソッド. 保持する数値データをCobolDecimal型に変換する.
   *
   * @return 保持する数値データをCobolDecimal型に変換した値
   */
  @Override
  public CobolDecimal getDecimal() {
    CobolDataStorage data = this.getDataStorage();
    int p = 0;
    int digits = this.getAttribute().getDigits();
    int sign = this.getSign();
    int val;
    BigDecimal value;

    if (digits % 2 == 0) {
      val = data.getByte(p) & 0x0F;
      digits--;
      p++;
    } else {
      val = 0;
    }

    if (this.getAttribute().getDigits() < 10) {
      while (digits > 1) {
        if (val != 0) {
          val *= 100;
        }
        if (data.getByte(p) != 0) {
          val += (upper4bits(data.getByte(p)) * 10) + (data.getByte(p) & 0x0F);
        }
        digits -= 2;
        p++;
      }
      if (val != 0) {
        val *= 10;
      }
      val += upper4bits(data.getByte(p));
      value = new BigDecimal(val);
    } else {
      int valseen = 0;
      value = new BigDecimal(val);
      if (val != 0) {
        valseen = 1;
      }
      while (digits > 1) {
        if (valseen != 0) {
          value = value.multiply(new BigDecimal(100));
        }
        if (data.getByte(p) != 0) {
          int n = (upper4bits(data.getByte(p))) * 10 + (data.getByte(p) & 0x0F);
          value = value.add(new BigDecimal(n));
          valseen = 1;
        }
        digits -= 2;
        p++;
      }
      if (valseen != 0) {
        value = value.multiply(BigDecimal.TEN);
      }
      value = value.add(new BigDecimal(upper4bits(data.getByte(p))));
    }

    if (sign < 0) {
      value = value.negate();
    }
    return new CobolDecimal(value, this.getAttribute().getScale());
  }

  /**
   * byte型データを右に4回シフトした値を返す CとJavaのビット演算の仕様に差異があるため実装した
   *
   * @param b TODO: 準備中
   * @return b>>4
   */
  private byte upper4bits(byte b) {
    return (byte) ((b >>> 4) & 0x0F);
  }

  /**
   * thisの保持する数値データをint型で返す
   *
   * @return thisの保持する数値データをintに変換した値
   */
  @Override
  public int getInt() {
    int p = 0;
    CobolDataStorage data = this.getDataStorage();
    int val = 0;

    for (int size = 0; size < this.getSize() - 1; ++size, ++p) {
      val *= 10;
      val += (data.getByte(p) >>> 4) & 0x0F;
      val *= 10;
      val += data.getByte(p) & 0x0F;
    }
    val *= 10;
    val += (data.getByte(p) >>> 4) & 0x0F;
    if ((data.getByte(p) & 0x0F) == 0x0d) {
      val = -val;
    }
    return val;
  }

  /** thisの保持する数値データを0に設定する */
  @Override
  public void setZero() {
    CobolDataStorage data = this.getDataStorage();
    int size = this.getSize();
    for (int i = 0; i < size; ++i) {
      data.setByte(i, (byte) 0);
    }
    if (this.getAttribute().isFlagHaveSign()) {
      data.setByte(size - 1, (byte) 0x0C);
    } else {
      data.setByte(size - 1, (byte) 0x0F);
    }
  }

  /**
   * libcob/codegen.hのcob_add_packed_intの実装 thisの保持する数値データに加算する
   *
   * @param val thisの保持する数値データに加算する値
   * @return TODO: 準備中
   */
  public int addPackedInt(int val) {
    if (val == 0) {
      return 0;
    }

    int p = this.getSize() - 1;
    CobolDataStorage data = this.getDataStorage();
    int n;
    if ((data.getByte(p) & 0x0F) == 0x0d) {
      if (val > 0) {
        return this.addInt(val);
      }
      n = -val;
    } else {
      if (val < 0) {
        return this.addInt(val);
      }
      n = val;
    }
    int inc = upper4bits(data.getByte(p)) + (n % 10);
    n /= 10;
    int carry = inc / 10;
    data.setByte(p, (byte) (((inc % 10) << 4) | data.getByte(p) & 0x0F));
    p--;

    for (int size = 0; size < this.getSize() - 1; ++size, --p) {
      if (carry == 0 && n == 0) {
        break;
      }
      byte x = data.getByte(p);
      inc = (upper4bits(x) * 10) + (x & 0x0F) + carry + (n % 100);
      carry = inc / 100;
      n /= 100;
      inc %= 100;
      data.setByte(p, (byte) (((inc / 10) << 4) | (inc % 10)));
    }
    return 0;
  }

  /**
   * libcob/numeric.cのcob_add_intおよびcob_add_packedの実装
   *
   * @param ival thisに 加算する値
   * @return TODO: 準備中
   */
  @Override
  public int addInt(int ival) {
    if (ival == 0) {
      return 0;
    }

    long val = ival;
    int ndigs = this.getAttribute().getScale();
    while (ndigs > 0) {
      --ndigs;
      val *= 10;
    }
    ndigs = this.getAttribute().getDigits();
    if (ndigs <= 0) {
      return 0;
    }
    int sign = this.getSign();
    int msn = 1;
    int emptydig = 1 - (this.getAttribute().getDigits() % 2);
    int subtr = 0;

    if (sign < 0) {
      val = -val;
    }
    if (val < 0) {
      val = -val;
      subtr = 1;
    }

    int p = ((ndigs + emptydig) / 2) - (1 - msn);
    CobolDataStorage data = this.getDataStorage();
    int origdigs = ndigs;
    int carry = 0;
    int zeroes = 0;

    while (ndigs-- != 0) {
      int tval;
      if (msn == 0) {
        tval = data.getByte(p) & 0x0F;
      } else {
        tval = upper4bits((byte) (data.getByte(p) & 0xF0));
      }
      if (val != 0) {
        carry += (val % 10);
        val /= 10;
      }
      if (subtr != 0) {
        tval -= carry;
        if (tval < 0) {
          tval += 10;
          carry = 1;
        } else {
          carry = 0;
        }
      } else {
        tval += carry;
        if (tval > 9) {
          tval %= 10;
          carry = 1;
        } else {
          carry = 0;
        }
      }
      if (tval == 0) {
        zeroes++;
      }
      if (msn == 0) {
        data.setByte(p, (byte) ((data.getByte(p) & 0xF0) | tval));
        msn = 1;
      } else {
        data.setByte(p, (byte) ((data.getByte(p) & 0x0F) | (tval << 4)));
        msn = 0;
        p--;
      }
    }

    if (sign != 0) {
      p = this.getSize() - 1;
      byte x = data.getByte(p);
      if (origdigs == zeroes) {
        data.setByte(p, (byte) ((x & 0xF0) | 0x0C));
      } else if (subtr != 0 && carry != 0) {
        complementPacked();
        sign = -sign;
        if (sign < 0) {
          data.setByte(p, (byte) ((x & 0xF0) | 0x0D));
        } else {
          data.setByte(p, (byte) ((x & 0xF0) | 0x0C));
        }
      }
    } else if (subtr != 0 && carry != 0) {
      complementPacked();
    }
    return 0;
  }

  /** libcob/numeric.cのcob_complement_packedの実装 */
  private void complementPacked() {
    int ndigs = this.getAttribute().getDigits();
    int msn = 1;
    int emptydig = 1 - (this.getAttribute().getDigits() % 2);
    int p = ((ndigs + emptydig) / 2) - (1 - msn);
    CobolDataStorage data = this.getDataStorage();
    int tval;
    int carry = 0;

    while (ndigs-- != 0) {
      if (msn == 0) {
        tval = data.getByte(p) & 0x0F;
      } else {
        tval = upper4bits((byte) (data.getByte(p) & 0xF0));
      }
      tval += carry;
      if (tval > 0) {
        carry = 1;
        tval = 10 - tval;
      } else {
        carry = 0;
      }
      byte x = data.getByte(p);
      if (msn == 0) {
        data.setByte(p, (byte) (x & 0xF0 | tval));
        msn = 1;
      } else {
        data.setByte(p, (byte) (x & 0x0F | (tval << 4)));
        msn = 0;
        p--;
      }
    }
  }

  /** */
  @Override
  public int cmpInt(int n) {
    if (this.getAttribute().getDigits() < 10) {
      return this.cmpPackedInt(n);
    } else {
      return this.cmpPacked(n);
    }
  }

  /**
   * libcob/codegen.hのcob_cmp_packed_intの実装
   *
   * @param n TODO: 準備中
   * @return TODO: 準備中
   */
  private int cmpPackedInt(int n) {
    CobolDataStorage data = this.getDataStorage();
    int val = 0;
    int p = 0;

    for (int size = 0; size < this.getSize() - 1; ++size, ++p) {
      val *= 10;
      val += (data.getByte(p) >>> 4) & 0x0F;
      val *= 10;
      val += data.getByte(p) & 0x0F;
    }
    val *= 10;
    val += (data.getByte(p) >> 4) & 0x0F;
    if ((data.getByte(p) & 0x0F) == 0x0d) {
      val = -val;
    }

    return val < n ? -1 : val > n ? 1 : 0;
  }

  /**
   * libcob/numeric.cのcob_cmp_packedの実装
   *
   * @param n TODO: 準備中
   * @return TODO: 準備中
   */
  private int cmpPacked(int n) {
    int sign = this.getSign();
    if (sign >= 0 && n < 0) {
      return 1;
    }
    if (sign < 0 && n >= 0) {
      return -1;
    }

    int p = 0;
    CobolDataStorage data = this.getDataStorage();
    byte[] val1 = new byte[20];
    int inc = 0;
    int size;
    for (size = 0; size < 20; size++) {
      if (size < 20 - this.getSize()) {
        val1[size] = 0;
      } else {
        val1[size] = data.getByte(inc++);
      }
    }
    val1[19] &= 0xF0;
    if (this.getAttribute().getDigits() % 2 == 0) {
      val1[20 - this.getSize()] &= 0x0F;
    }
    if (n != CobolDecimal.packedValueInt) {
      CobolDecimal.packedValueInt = n;
      if (n < 0) {
        n = -n;
      }
      for (int i = 0; i < 6; ++i) {
        CobolDecimal.packedValue[14 + i] = 0;
      }
      if (n != 0) {
        p = 19;
        CobolDecimal.packedValue[p] = (byte) ((n % 10) << 4);
        p--;
        n /= 10;
        while (n != 0) {
          size = n % 100;
          CobolDecimal.packedValue[p] = (byte) ((size % 10) | ((size / 10) << 4));
          n /= 100;
          p--;
        }
      }
    }
    for (size = 0; size < 20; size++) {
      if (val1[size] != CobolDecimal.packedValue[size]) {
        if (sign < 0) {
          return Byte.toUnsignedInt(CobolDecimal.packedValue[size])
              - Byte.toUnsignedInt(val1[size]);
        } else {
          return Byte.toUnsignedInt(val1[size])
              - Byte.toUnsignedInt(CobolDecimal.packedValue[size]);
        }
      }
    }
    return 0;
  }

  /** libcob/move.cのcob_packed_get_long_longの実装 */
  @Override
  public long getLong() {
    int digits = this.getAttribute().getDigits();
    int scale = this.getAttribute().getScale();
    CobolDataStorage data = this.getDataStorage();
    int sign = this.getSign();
    int offset = 1 - (this.getAttribute().getDigits() % 2);
    int val = 0;

    for (int i = offset; i < digits - scale + offset; ++i) {
      val *= 10;
      if (i % 2 == 0) {
        val += (0xFF & data.getByte(i / 2)) >> 4;
      } else {
        val += data.getByte(i / 2) & 0x0F;
      }
    }

    if (sign < 0) {
      val = -val;
    }
    return val;
  }
}
