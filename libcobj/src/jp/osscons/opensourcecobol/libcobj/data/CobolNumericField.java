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
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import jp.osscons.opensourcecobol.libcobj.common.CobolConstant;
import jp.osscons.opensourcecobol.libcobj.common.CobolModule;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolExceptionId;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolRuntimeException;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolStopRunException;

/** PIC 文字列が9(5)や9(9)の変数を表現するクラス. */
public class CobolNumericField extends AbstractCobolField {

  /**
   * コンストラクタ
   *
   * @param size データを格納するバイト配列の長さ
   * @param dataStorage データを格納するバイト配列を扱うオブジェクト
   * @param attribute 変数に関する様々な情報を保持するオブジェクト
   */
  public CobolNumericField(int size, CobolDataStorage dataStorage, CobolFieldAttribute attribute) {
    super(size, dataStorage, attribute);
  }

  /** TODO実装 */
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
    CobolDataStorage data = this.getDataStorage();
    CobolFieldAttribute attr = this.getAttribute();
    int digits = attr.getDigits();

    StringBuilder sb = new StringBuilder();
    if (attr.isFlagHaveSign()) {
      if (this.getSign() < 0) {
        sb.append('-');
      } else {
        sb.append('+');
      }
    }

    int scale = attr.getScale();
    int fieldSize = this.getFieldSize();
    if (scale >= fieldSize) {
      sb.append(".");
      for (int i = 0; i < scale - fieldSize; ++i) {
        sb.append('0');
      }
      int firstIndex = this.getFirstDataIndex();
      int signIndex = attr.isFlagSignLeading() ? 0 : this.getSize() - 1;
      for (int i = 0; i < fieldSize; ++i) {
        char c = (char) data.getByte(firstIndex + i);
        if (firstIndex + i == signIndex && c >= 0x70) {
          c -= 0x40;
        }
        sb.append(c);
      }
      return sb.toString();
    }

    int pointIndex = scale > 0 ? digits - scale - 1 : digits - 1;

    int signIndex = attr.isFlagSignLeading() ? 0 : this.getSize() - 1;
    int i = 0;
    int dataLastIndex =
        attr.isFlagHaveSign() && !attr.isFlagSignLeading() && attr.isFlagSignSeparate()
            ? this.getSize() - 2
            : this.getSize() - 1;

    for (; i + getFirstDataIndex() <= dataLastIndex; ++i) {
      if (scale > 0 && i - 1 == pointIndex) {
        sb.append('.');
      }
      char c = (char) data.getByte(this.getFirstDataIndex() + i);
      if (attr.isFlagHaveSign() && !attr.isFlagSignSeparate() && i == signIndex && c >= 0x70) {
        c -= 0x40;
      }
      sb.append(c);
    }

    for (; i < digits; ++i) {
      if (scale > 0 && i - 1 == pointIndex) {
        sb.append('.');
      }
      sb.append('0');
    }

    return sb.toString();
  }

  /**
   * thisの保持する数値データをint型で返す
   *
   * @return thisの保持する数値データをintに変換した値
   */
  @Override
  public int getInt() {
    int size = this.getFieldSize();
    CobolDataStorage data = this.getDataStorage();
    int firstDataIndex = this.getFirstDataIndex();
    int sign = this.getSign();

    int i;
    for (i = 0; i < size; ++i) {
      if (data.getByte(firstDataIndex + i) - 0x30 != 0) {
        break;
      }
    }

    int val = 0;
    int scale = this.getAttribute().getScale();
    if (scale < 0) {
      for (; i < size; ++i) {
        byte x = data.getByte(firstDataIndex + i);
        x -= (x >= 0x70) ? 0x70 : 0x30;
        val = val * 10 + x;
      }
      val *= AbstractCobolField.cobExp10[-scale];
    } else {
      size -= scale;
      for (; i < size; ++i) {
        byte x = data.getByte(firstDataIndex + i);
        x -= (x >= 0x70) ? 0x70 : 0x30;
        val = val * 10 + x;
      }
    }

    if (sign < 0) {
      val = -val;
    }

    this.putSign(sign);
    return val;
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
   * @param field 代入元のデータ(AbstractCobolField型)
   */
  @Override
  public void moveFrom(AbstractCobolField src) {
    AbstractCobolField src1 = this.preprocessOfMoving(src);
    if (src1 == null) {
      return;
    }

    switch (src1.getAttribute().getType()) {
      case CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY:
        this.moveDisplayToDisplay(src1);
        break;
      case CobolFieldAttribute.COB_TYPE_NUMERIC_PACKED:
        this.movePackedToDisplay(src1);
        break;
      case CobolFieldAttribute.COB_TYPE_NATIONAL:
      case CobolFieldAttribute.COB_TYPE_ALPHANUMERIC:
      case CobolFieldAttribute.COB_TYPE_ALPHANUMERIC_EDITED:
      case CobolFieldAttribute.COB_TYPE_NATIONAL_EDITED:
        this.moveAlphanumericToDisplay(src1);
        break;
      case CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY:
        this.moveBinaryToDisplay(src1);
        break;
      case CobolFieldAttribute.COB_TYPE_NUMERIC_DOUBLE:
      case CobolFieldAttribute.COB_TYPE_NUMERIC_FLOAT:
        this.moveDoubleToDisplay(src1);
        break;
      case CobolFieldAttribute.COB_TYPE_NUMERIC_EDITED:
        this.moveEditedToDisplay(src1);
        break;
      case CobolFieldAttribute.COB_TYPE_GROUP:
        CobolAlphanumericField.moveAlphanumToAlphanum(this, src1);
        break;
      default:
        throw new CobolRuntimeException(0, "未実装");
    }
  }

  /**
   * CobolNumericDisplayからthisへの代入を行う
   *
   * @param field 代入元のデータ(AbstractCobolField型)
   */
  private void moveDisplayToDisplay(AbstractCobolField field) {
    int sign = field.getSign();
    field.putSign(1);

    this.storeCommonRegion(
        this,
        field.getDataStorage().getSubDataStorage(field.getFirstDataIndex()),
        field.getFieldSize(),
        field.getAttribute().getScale());

    field.putSign(sign);
    this.putSign(sign);
  }

  /**
   * CobolNumericPackedFieldからthisへの代入を行う
   *
   * @param field 代入元のデータ(AbstractCobolField型)
   */
  private void movePackedToDisplay(AbstractCobolField field) {
    int sign = field.getSign();
    int offset = 1 - (field.getAttribute().getDigits() % 2);
    CobolDataStorage buff = new CobolDataStorage(64);
    int packedDigits = field.getAttribute().getDigits();
    int packedScale = field.getAttribute().getScale();
    int end;
    if (packedScale < 0) {
      end = offset + packedDigits + packedScale;
    } else {
      end = offset + packedDigits;
    }

    for (int i = offset; i < end; ++i) {
      if (i % 2 == 0) {
        buff.setByte(
            i - offset, (byte) (((field.getDataStorage().getByte(i / 2) >> 4) & 0x0f) + 0x30));
      } else {
        buff.setByte(i - offset, (byte) ((field.getDataStorage().getByte(i / 2) & 0x0f) + 0x30));
      }
    }
    this.storeCommonRegion(
        this, buff, field.getAttribute().getDigits(), field.getAttribute().getScale());

    this.putSign(sign);
  }

  /**
   * CobolAlphanumericFieldからthisへの代入を行う
   *
   * @param field 代入元のデータ(AbstractCobolField型)
   */
  private void moveAlphanumericToDisplay(AbstractCobolField field) {
    int s1 = 0;
    int e1 = s1 + field.getSize();
    int s2 = this.getFirstDataIndex();
    int e2 = this.getFieldSize();

    for (int i = 0; i < this.getSize(); ++i) {
      this.getDataStorage().setByte(i, (byte) 0x30);
    }

    /* skip white space */
    for (; s1 < e1; ++s1) {
      char ch = (char) field.getDataStorage().getByte(s1);
      if (!Character.isWhitespace(ch)) {
        break;
      }
    }

    /* check for sign */
    int sign = 0;
    if (s1 != e1) {
      byte ch = field.getDataStorage().getByte(s1);
      byte plus = 0x2b;
      byte minus = 0x2d;
      if (ch == plus || ch == minus) {
        sign = (ch == plus) ? 1 : -1;
        s1++;
      }
    }

    /* count the number of digits before decimal point */
    int count = 0;
    // TODO Moduleの情報を参照するコードに編集する
    for (int p = s1; p < e1 && field.getDataStorage().getByte(p) != '.'; ++p) {
      if (Character.isDigit(field.getDataStorage().getByte(p))) {
        ++count;
      }
    }

    /* find the start position */
    int size = this.getFieldSize() - this.getAttribute().getScale();
    if (count < size) {
      s2 += size - count;
    } else {
      while (count-- > size) {
        while (!Character.isDigit(field.getDataStorage().getByte(s1++))) {
          ;
        }
      }
    }

    /* move */
    count = 0;
    outer:
    {
      for (; s1 < e1 && s2 < e2; ++s1) {
        byte c = field.getDataStorage().getByte(s1);
        if (Character.isDigit(c)) {
          this.getDataStorage().setByte(s2++, c);
          // TODO Moduleの情報を参照するコードに編集する
        } else if (c == (byte) '.') {
          ++count;
          if (count > 0) {
            break outer;
          }

          // TODO Moduleの情報を参照するコードに編集する
        } else if (!Character.isWhitespace(c) || c == ',') {
          break outer;
        }
      }
      this.putSign(sign);
      return;
    }
    for (int i = 0; i < this.getSize(); ++i) {
      this.getDataStorage().setByte(i, (byte) 0x30);
    }
    this.putSign(0);
  }

  /**
   * CobolNumericBinaryからthisへの代入を行う
   *
   * @param field 代入元のデータ(AbstractCobolField型)
   */
  private void moveBinaryToDisplay(AbstractCobolField field) {
    int sign = 1;
    long val = field.getLongValue();

    if (this.getAttribute().isFlagHaveSign() && val < 0) {
      sign = -1;
      val = -val;
    }

    int i = 20;
    byte[] buff = new byte[64];
    while (val > 0) {
      buff[--i] = (byte) (val % 10 + 0x30);
      val /= 10;
    }

    this.storeCommonRegion(
        this, new CobolDataStorage(buff, 0), i, (20 - i), field.getAttribute().getScale());
    this.putSign(sign);
  }

  private void moveDoubleToDisplay(AbstractCobolField field) {
    CobolFieldAttribute thisAttr = this.getAttribute();
    double val = Math.abs(field.getDouble() * thisAttr.getScale());
    String formatter = "%0" + thisAttr.getDigits() + "." + thisAttr.getScale() + "f";
    String valString = String.format(formatter, val).replace(".", "");
    int startIndex = 0;
    if (thisAttr.isFlagHaveSign()
        && thisAttr.isFlagSignLeading()
        && thisAttr.isFlagSignSeparate()) {
      startIndex = 1;
    }
    CobolDataStorage dstStorage = this.getDataStorage().getSubDataStorage(startIndex);
    dstStorage.memcpy(valString, thisAttr.getDigits());
    this.putSign(field.getSign() >= 0 ? 1 : -1);
  }

  private void moveEditedToDisplay(AbstractCobolField field) {
    byte[] buff = new byte[64];
    int p = 0;
    boolean havePoint = false;
    int scale = 0;
    int sign = 0;

    for (int i = 0; i < field.getSize(); ++i) {
      int cp = field.getDataStorage().getByte(i);
      switch (cp) {
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
          buff[p++] = (byte) cp;
          if (havePoint) {
            ++scale;
          }
          break;
        case '.':
        case ',':
          if (cp == CobolModule.getCurrentModule().decimal_point) {
            havePoint = true;
          }
          break;
        case '-':
        case 'C':
          sign = -1;
          break;
        default:
          break;
      }
    }

    byte[] picBytes = field.getAttribute().getPic().getBytes();
    int count = 0;
    if (scale == 0) {
      for (int p1 = 0; p1 < picBytes.length; p1 += 5) {
        byte c = picBytes[p1];
        ByteBuffer buf = ByteBuffer.wrap(picBytes, p1 + 1, 4);
        buf.order(ByteOrder.LITTLE_ENDIAN);
        int n = buf.getInt();
        if (c == '9' || c == '0' || c == 'Z' || c == '*') {
          if (havePoint) {
            scale += n;
          } else {
            count += n;
          }
        } else if (c == 'P') {
          if (count == 0) {
            havePoint = true;
            scale += n;
          } else {
            scale -= n;
          }
        } else if (c == 'V') {
          havePoint = true;
        }
      }
    }

    storeCommonRegion(this, new CobolDataStorage(buff), p, scale);
    this.putSign(sign);
  }

  /**
   * thisの保持する数値データの符号を返す
   *
   * @return thisの保持する数値データが負ならば負数,0なら0,正なら正数を返す
   */
  @Override
  public int getSign() {
    CobolFieldAttribute attr = this.getAttribute();
    if (!attr.isFlagHaveSign()) {
      return 0;
    }

    int p;
    if (attr.isFlagSignLeading()) {
      p = 0;
    } else {
      p = this.getSize() - 1;
    }
    byte value = this.getDataStorage().getByte(p);
    if (attr.isFlagSignSeparate()) {
      return value == 0x2b ? 1 : -1;
    } else {
      if (0x30 <= value && value <= 0x39) {
        return 1;
      }
      if (value == 0x20) {
        this.getDataStorage().setByte(p, (byte) 0x30);
        return 1;
      }

      // TODO 以下のコメントアウトを外して,他の部分も修正してテストする
      // if(CobolModule.getCurrentModule().display_sign != 0) {
      // return 1;
      // return getSignEbcdic(p);
      // } else {
      // return -1;
      // }

      // this.getDataStorage().setByte(p, (byte) (this.getDataStorage().getByte(p) -
      // 0x40));
      return -1;
    }
  }

  /**
   * thisの保持する数値データの符号を設定する
   *
   * @param sign 正符号を設定するときは正数,負符号を設定するときは負数,それ以外は0
   */
  @Override
  public void putSign(int sign) {
    CobolFieldAttribute attr = this.getAttribute();
    int p;

    if (!attr.isFlagHaveSign()) {
      return;
    }

    if (attr.isFlagSignLeading()) {
      p = 0;
    } else {
      p = this.getSize() - 1;
    }

    byte value = this.getDataStorage().getByte(p);
    if (attr.isFlagSignSeparate()) {
      // 0x2dは'-', 0x2bは'+'
      byte c = (byte) ((sign < 0) ? 0x2d : 0x2b);
      if (value != c) {
        this.getDataStorage().setByte(p, c);
      }
      // TODO 以下のコメントを外して他の部分も修正してテストする
    } // else if (CobolModule.getCurrentModule().display_sign != 0) {
    /// this.putSignEbcdic(p, sign);
    // }

    else {
      value = (byte) (value >= 0x70 ? value - 0x40 : value);
      this.getDataStorage().setByte(p, (byte) (sign < 0 ? value + 0x40 : value));
    }
  }

  /**
   * libcob/move.cのstore_common_regionの実装
   *
   * @param field
   * @param data
   * @param size
   * @param scale
   */
  public void storeCommonRegion(
      AbstractCobolField field, CobolDataStorage data, int size, int scale) {
    this.storeCommonRegion(field, data, 0, size, scale);
  }

  /**
   * libcob/move.cのstore_common_regionの実装
   *
   * @param field
   * @param data
   * @param size
   * @param scale
   */
  public void storeCommonRegion(
      AbstractCobolField field, CobolDataStorage data, int dataStartIndex, int size, int scale) {
    int lf1 = -scale;
    int lf2 = -field.getAttribute().getScale();
    int hf1 = size + lf1;
    int hf2 = field.getFieldSize() + lf2;

    int lcf = Math.max(lf1, lf2);
    int gcf = Math.min(hf1, hf2);

    for (int i = 0; i < field.getFieldSize(); ++i) {
      field.getDataStorage().setByte(i + field.getFirstDataIndex(), (byte) 0x30);
    }

    if (gcf > lcf) {
      int csize = gcf - lcf;
      int p = hf1 - gcf;
      int q = field.getFirstDataIndex() + hf2 - gcf;
      for (int cinc = 0; cinc < csize; ++cinc, ++p, ++q) {
        if (data.getByte(dataStartIndex + p) == (byte) 0x20) {
          field.getDataStorage().setByte(q, (byte) 0x30);
        } else {
          byte value = data.getByte(dataStartIndex + p);
          field.getDataStorage().setByte(q, value);
        }
      }
    }
  }

  /**
   * 引数で与えらえられたデータからthisへの代入を行う
   *
   * @param field 代入元のデータ(byte[]型)
   */
  @Override
  public void moveFrom(byte[] bytes) {
    this.dataStorage.setData(bytes);
  }

  /**
   * 引数で与えらえられたデータからthisへの代入を行う
   *
   * @param field 代入元のデータ(String型)
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
   * 引数で与えらえられたデータからthisへの代入を行う
   *
   * @param field 代入元のデータ(int型)
   */
  @Override
  public void moveFrom(int number) {
    int n = Math.abs(number);
    for (int i = 0; i < this.size; ++i) {
      this.dataStorage.setByte(i, (byte) 0x30);
    }

    for (int i = this.size - 1; i >= 0; --i) {
      this.dataStorage.setByte(i, (byte) (0x30 + n % 10));
      n /= 10;
    }

    if (number < 0 && this.getAttribute().isFlagHaveSign()) {
      this.putSign(-1);
    }
  }

  @Override
  public int subInt(int in) {
    return this.addInt(-in, CobolDecimal.COB_STORE_KEEP_ON_OVERFLOW);
  }

  public int subInt(int in, int opt) {
    return this.addInt(-in, opt);
  }

  @Override
  public int addInt(int in) {
    return this.addInt(in, CobolDecimal.COB_STORE_KEEP_ON_OVERFLOW);
  }

  /**
   * thisの保持する数値データに加算する
   *
   * @param in thisの保持する数値データに加算する値
   */
  public int addInt(int in, int opt) {
    if (in == 0) {
      return 0;
    }

    int n = in;
    CobolDataStorage data = this.getDataStorage();
    int firstDataIndex = this.getFirstDataIndex();
    int size = this.getFieldSize();
    int scale = this.getAttribute().getScale();
    int sign = this.getSign();
    int osize = size;
    byte[] tfield = new byte[64];

    for (int i = 0; i < osize; ++i) {
      tfield[i] = data.getByte(firstDataIndex + i);
    }

    if (sign < 0) {
      n = -n;
    }
    while (scale > 0) {
      --scale;
      n *= 10;
    }

    if (scale < 0) {
      if (-scale < 10) {
        while (scale != 0) {
          ++scale;
          n /= 10;
        }
      } else {
        n = 0;
      }
    } else {
      size -= scale;
    }

    if (n > 0) {
      if (displayAddInt(data, firstDataIndex, size, n) != 0) {
        for (int i = 0; i < osize; ++i) {
          data.setByte(firstDataIndex + i, tfield[i]);
        }
        CobolRuntimeException.setException(CobolExceptionId.COB_EC_SIZE_OVERFLOW);
        if ((opt & CobolDecimal.COB_STORE_KEEP_ON_OVERFLOW) > 0) {
          this.putSign(sign);
          return CobolRuntimeException.code;
        }
      }
    } else if (n < 0) {
      if (displaySubInt(data, firstDataIndex, size, -n) != 0) {
        for (int i = 0; i < size; ++i) {
          byte val = data.getByte(firstDataIndex + i);
          data.setByte(firstDataIndex + i, (byte) (9 - (val - 0x30) + 0x30));
        }
        displayAddInt(data, firstDataIndex, size, 1);
        sign = -sign;
      }
    }

    this.putSign(sign);
    return 0;
  }

  /**
   * libcob/numeric.cのdisplay_add_intの実装
   *
   * @param data
   * @param firstDataIndex dataにアクセスするときの開始位置
   * @param size
   * @param n
   * @return
   */
  private int displayAddInt(CobolDataStorage data, int firstDataIndex, int size, long n) {
    int carry = 0;
    int sp = firstDataIndex + size;
    int i;
    while (n > 0) {
      i = (int) (n % 10L);
      n /= 10;

      /* check for overflow */
      --sp;
      if (sp < firstDataIndex) {
        if (CobolModule.getCurrentModule().flag_binary_truncate == 0) {
          return 0;
        }
        return 1;
      }

      /* perform addtion */
      int is = (data.getByte(sp) & 0x0F) + i + carry;
      if (is > 9) {
        carry = 1;
        data.setByte(sp, (byte) (0x30 + (is % 10)));
      } else {
        carry = 0;
        data.setByte(sp, (byte) (0x30 + is));
      }
    }
    if (carry == 0) {
      return 0;
    }

    /* carry up */
    while (--sp >= firstDataIndex) {
      byte val = data.getByte(sp);
      data.setByte(sp, (byte) (val + 1));
      if (val + 1 <= '9') {
        return 0;
      }
      data.setByte(sp, (byte) '0');
    }

    if (CobolModule.getCurrentModule().flag_binary_truncate == 0) {
      return 0;
    }
    return 1;
  }

  /**
   * libcob/numeric.cのdisplay_sub_intの実装
   *
   * @param data
   * @param firstDataIndex dataにアクセスするときの開始位置
   * @param size
   * @param n
   * @return
   */
  public static int displaySubInt(CobolDataStorage data, int firstDataIndex, int size, long n) {
    int carry = 0;
    int sp = firstDataIndex + size;
    int i;
    while (n > 0) {
      i = (int) (n % 10L);
      n /= 10;

      /* check for overflow */
      --sp;
      if (sp < firstDataIndex) {
        return 1;
      }

      /* perform subtraction */
      byte val = data.getByte(sp);
      data.setByte(sp, (byte) (val - (i + carry)));
      if (val - (i + carry) < '0') {
        carry = 1;
        data.setByte(sp, (byte) (data.getByte(sp) + 10));
      } else {
        carry = 0;
      }
    }
    if (carry == 0) {
      return 0;
    }

    /* carry up */
    while (--sp >= firstDataIndex) {
      byte val = data.getByte(sp);
      data.setByte(sp, (byte) (val - 1));
      if (val - 1 >= '0') {
        return 0;
      }
      data.setByte(sp, (byte) '9');
    }
    return 1;
  }

  @Override
  public int add(AbstractCobolField field, int opt) throws CobolStopRunException {
    CobolFieldAttribute attr = field.getAttribute();
    if (attr.isTypeNumeric()
        && attr.getDigits() <= 9
        && attr.getScale() == 0
        && this.getAttribute().getScale() == 0) {
      return this.addInt(field.getInt(), opt);
    } else {
      return super.add(field, opt);
    }
  }

  @Override
  public int sub(AbstractCobolField field, int opt) throws CobolStopRunException {
    CobolFieldAttribute attr = field.getAttribute();
    if (attr.isTypeNumeric()
        && attr.getDigits() <= 9
        && attr.getScale() == 0
        && this.getAttribute().getScale() == 0) {
      return this.subInt(field.getInt(), opt);
    } else {
      return super.sub(field, opt);
    }
  }

  /**
   * thisをCobolNumericFieldに変換する. indirect moveをするときに使用されることを想定している.
   *
   * @return thisからCobolNumericField型へ変換した値
   */
  @Override
  public CobolNumericField getNumericField() {
    return this;
  }

  /**
   * 引数で与えらえられたデータからthisへの代入を行う
   *
   * @param field 代入元のデータ(double型)
   */
  /*
   * @Override
   * public void moveFrom(double number) {
   * this.moveFrom((int) number);
   * }
   */

  /**
   * 引数で与えらえられたデータからthisへの代入を行う
   *
   * @param field 代入元のデータ(BigDecimal型)
   */
  @Override
  public void moveFrom(BigDecimal number) {}

  /**
   * 引数で与えらえられたデータからthisへの代入を行う
   *
   * @param field 代入元のデータ(CobolDataStorage型)
   */
  @Override
  public void moveFrom(CobolDataStorage dataStrage) {}

  /** 実装しないメソッド */
  public int addPackedInt(int n) {
    throw new CobolRuntimeException(0, "実装しないコード");
  }

  /** libcob/move.cのcob_display_get_long_longの実装 */
  @Override
  public long getLong() {
    int size = this.getSize();
    CobolDataStorage data = this.getDataStorage();
    int sign = this.getSign();
    int i = 0;

    for (i = 0; i < size; ++i) {
      if (data.getByte(i) != '0') {
        break;
      }
    }

    long val = 0;
    int scale = this.getAttribute().getScale();
    if (scale < 0) {
      for (; i < size; ++i) {
        val = val * 10 + (data.getByte(i) - '0');
      }
      val *= CobolConstant.exp10LL[-scale];
    } else {
      size -= scale;
      for (; i < size; ++i) {
        val = val * 10 + (data.getByte(i) - '0');
      }
    }
    if (sign < 0) {
      val = -val;
    }
    this.putSign(sign);
    return val;
  }

  public int numericCompareTo(AbstractCobolField field) {
    CobolFieldAttribute attr1 = this.getAttribute();
    CobolFieldAttribute attr2 = field.getAttribute();
    if (attr2.isTypeNumericDisplay()) {
      final int scale1 = attr1.getScale();
      final int scale2 = attr2.getScale();
      final int firstIndex1 = this.getFirstDataIndex();
      final int firstIndex2 = field.getFirstDataIndex();
      final int fieldSize1 = this.getFieldSize();
      final int fieldSize2 = field.getFieldSize();
      final int size1 = this.getSize();
      final int size2 = field.getSize();
      final int pointIndex1 = fieldSize1 - scale1;
      final int pointIndex2 = fieldSize2 - scale2;
      int sign1 = this.getSign();
      int sign2 = field.getSign();
      sign1 = sign1 > 0 ? 1 : sign1 < 0 ? -1 : 1;
      sign2 = sign2 > 0 ? 1 : sign2 < 0 ? -1 : 1;

      final int signIndex1 =
          attr1.isFlagHaveSign() && !attr1.isFlagSignLeading() ? size1 - 1 : firstIndex1;
      final int signIndex2 =
          attr2.isFlagHaveSign() && !attr2.isFlagSignLeading() ? size2 - 1 : firstIndex2;
      final int l1 = -pointIndex1;
      final int l2 = -pointIndex2;
      final int r1 = fieldSize1 - pointIndex1;
      final int r2 = fieldSize2 - pointIndex2;
      final int left = Math.min(l1, l2);
      final int right = Math.max(r1, r2);
      CobolDataStorage d1 = this.getDataStorage();
      CobolDataStorage d2 = field.getDataStorage();
      for (int i = left; i < right; ++i) {
        final int i1 = firstIndex1 + i + pointIndex1;
        final int i2 = firstIndex2 + i + pointIndex2;
        byte b1;
        if (i1 < 0 || i1 >= size1) {
          b1 = (byte) '0';
        } else {
          b1 = d1.getByte(i1);
          if (i1 == signIndex1 && b1 >= 0x70) {
            b1 -= 0x40;
          }
        }
        byte b2;
        if (i2 < 0 || i2 >= size2) {
          b2 = (byte) '0';
        } else {
          b2 = d2.getByte(i2);
          if (i2 == signIndex2 && b2 >= 0x70) {
            b2 -= 0x40;
          }
        }
        if (b1 != b2) {
          return (sign1 * (int) b1) - (sign2 * (int) b2);
        }
      }

      if (sign1 * sign2 >= 0) {
        return 0;
      } else if (sign1 > 0) {
        return 1;
      } else {
        return -1;
      }
    } else {
      return super.numericCompareTo(field);
    }
  }
}
