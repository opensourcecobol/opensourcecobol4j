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
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import jp.osscons.opensourcecobol.libcobj.common.CobolConstant;
import jp.osscons.opensourcecobol.libcobj.common.CobolModule;
import jp.osscons.opensourcecobol.libcobj.common.CobolUtil;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolRuntimeException;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolStopRunException;

/** COBOLで使用する変数を表現するクラス。 PIC文字列の種類に応じて,このクラスを継承したクラスを作成する */
public abstract class AbstractCobolField {
  /** データを格納に使用するバイト配列の長さ */
  protected int size;
  /** データを格納するバイト配列を扱うオブジェクト */
  protected CobolDataStorage dataStorage;
  /** 変数に関する様々な情報を保持するオブジェクト(符号付か,COMP-3指定かなど) */
  protected CobolFieldAttribute attribute;

  static int lastsize = 0;
  static CobolDataStorage lastdata = null;

  static final int[] cobExp10 = {
    1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000
  };

  /**
   * moveAlphanumericToNumericで使用 C言語のgotoの代替機能を提供する
   *
   * @author y-sakamoto
   */
  protected class GotoException extends Exception {}

  /**
   * コンストラクタ
   *
   * @param size データを格納するバイト配列の長さ
   * @param dataStorage データを格納するバイト配列を扱うオブジェクト
   * @param attribute 変数に関する様々な情報を保持するオブジェクト(符号付か,COMP-3指定かなど)
   */
  public AbstractCobolField(int size, CobolDataStorage dataStorage, CobolFieldAttribute attribute) {
    this.size = size;
    this.dataStorage = dataStorage;
    this.attribute = attribute;
  }

  /**
   * メンバ変数dataStorageのgetter
   *
   * @return this.dataStorage
   */
  public CobolDataStorage getDataStorage() {
    return dataStorage;
  }

  /** メンバ変数dataStorageのsetter */
  public void setDataStorage(CobolDataStorage dataStorage) {
    this.dataStorage = dataStorage;
  }

  /** メンバ変数attirbuteのsetter */
  public void setAttribute(CobolFieldAttribute attribute) {
    this.attribute = attribute;
  }

  /**
   * メンバ変数attributeのgetter
   *
   * @return this.attribute
   */
  public CobolFieldAttribute getAttribute() {
    return attribute;
  }

  /**
   * メンバ変数sizeのsetter
   *
   * @param size
   */
  public void setSize(int size) {
    this.size = size;
  }

  /**
   * メンバ変数sizeのgetter
   *
   * @return this.size
   */
  public int getSize() {
    return size;
  }

  /**
   * opensource COBOLのCOB_FIELD_SIZEマクロに相当するメソッド
   *
   * @return 符号付で符号が分離している場合はthis.size-1,そうでなければthis.size
   */
  public int getFieldSize() {
    return this.size - (this.attribute.isFlagSignSeparate() ? 1 : 0);
  }

  public CobolDataStorage getFieldData() {
    if (this.attribute.isFlagSignSeparate() && this.attribute.isFlagSignLeading()) {
      return new CobolDataStorage(this.dataStorage.getRefOfData(), this.dataStorage.getIndex() + 1);
    } else {
      return this.dataStorage;
    }
  }

  /**
   * opensource COBOLのCOB_FIELD_DATAに相当するメソッド バイト配列の中で(符号データではなく)数値データの格納されている最小の添え字を返す opensource
   * COBOLではポインタを返しているが,このメソッドは添え字を返す
   *
   * @return SIGN_LEADINGかつSIGN_SEPARATEなら1,それ以外は0
   */
  public int getFirstDataIndex() {
    return (this.attribute.isFlagSignSeparate() && this.attribute.isFlagSignLeading()) ? 1 : 0;
  }

  public byte[] getBytes() {
    CobolFieldAttribute attr =
        new CobolFieldAttribute(
            CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY,
            9,
            0,
            CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
            null);
    CobolDataStorage n = new CobolDataStorage(new byte[4], 0);
    AbstractCobolField temp = CobolFieldFactory.makeCobolField(4, n, attr);
    temp.moveFrom(this);
    return ByteBuffer.wrap(n.getByteArray(0, 4)).array();
  }

  /**
   * thisの文字列表現をかえす.(toStringだけで十分か?)
   *
   * @return thisの文字列表現
   */
  public abstract String getString();

  /**
   * @return
   */
  public int getInt() {
    CobolFieldAttribute attr =
        new CobolFieldAttribute(
            CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY,
            9,
            0,
            CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
            null);
    CobolDataStorage n = new CobolDataStorage(new byte[4], 0);
    AbstractCobolField temp = CobolFieldFactory.makeCobolField(4, n, attr);
    temp.moveFrom(this);
    return ByteBuffer.wrap(n.getByteArray(0, 4)).getInt();
  }

  /**
   * @return
   */
  public double getDouble() {
    try {
      return Double.parseDouble(this.getString());
    } catch (Exception e) {
      return 0;
    }
  }

  /**
   * 数値を表すデータが実装すべきメソッド. 保持する数値データをCobolDecimal型に変換する.
   *
   * @return 保持する数値データをCobolDecimal型に変換した値
   */
  public CobolDecimal getDecimal() {
    CobolDataStorage data = this.getDataStorage();
    int firstDataIndex = this.getFirstDataIndex();
    int size = this.getFieldSize();

    if (data.getByte(firstDataIndex) == 255) {
      CobolDecimal decimal = new CobolDecimal(BigDecimal.TEN.pow(size));
      decimal.setScale(this.getAttribute().getScale());
      return decimal;
    }

    if (data.getByte(firstDataIndex) == 0) {
      CobolDecimal decimal = new CobolDecimal(BigDecimal.TEN.pow(size).negate());
      decimal.setScale(this.getAttribute().getScale());
      return decimal;
    }

    char[] buf = new char[size];
    for (int i = 0; i < size; ++i) {
      byte val = data.getByte(firstDataIndex + i);
      if (val >= 0x70) {
        buf[i] = (char) (val - 0x40);
      } else {
        buf[i] = (char) val;
      }
    }

    CobolFieldAttribute attr = this.getAttribute();
    int sign = 1;
    if (attr.isFlagHaveSign()) {
      if (attr.isFlagSignSeparate()) {
        int signIndex = attr.isFlagSignLeading() ? 0 : this.getSize() - 1;
        if (data.getByte(signIndex) == '-') {
          sign = -1;
        }
      } else {
        int signIndex = attr.isFlagSignLeading() ? 0 : this.getSize() - 1;
        if (data.getByte(signIndex) >= 0x70) {
          sign = -1;
        }
      }
    }

    BigDecimal decimal = new BigDecimal(buf);
    if (sign < 0) {
      decimal = decimal.negate();
    }
    CobolDecimal ret = new CobolDecimal(decimal);
    ret.setScale(this.getAttribute().getScale());
    return ret;
  }

  /**
   * TODO 確認 未使用?
   *
   * @param decimal
   */
  public abstract void setDecimal(BigDecimal decimal);

  /**
   * thisの保持する数値データをint型で返す
   *
   * @return thisの保持する数値データをintに変換した値
   */
  public int getInt(int size) {
    int retval = 0;
    int p = 0;
    CobolDataStorage data = this.getDataStorage();
    for (int n = 0; n < size; ++n, ++p) {
      retval *= 10;
      if (data.getByte(p) > (byte) '9') {
        retval += 10;
      } else {
        retval += data.getByte(p) - (byte) '0';
      }
    }
    return retval;
  }

  /** thisの保持する数値データを0に設定するメソッド. */
  public void setZero() {
    throw new CobolRuntimeException(0, "未実装");
  }

  /**
   * libcob/numeric.cのcob_addの実装 thisの保持する数値データに,引数で与えられたフィールドの保持する数値データを加算する
   *
   * @param field 加算する数値を保持するフィールド
   * @param opt 加算に関するオプション.詳しくはopensourceCOBOLを参照
   * @return 加算後のthisの保持する数値データ
   */
  public int add(AbstractCobolField field, int opt) throws CobolStopRunException {
    CobolDecimal d1 = this.getDecimal();
    CobolDecimal d2 = field.getDecimal();
    d1.add(d2);
    return d1.getField(this, opt);
  }

  /**
   * libcob/numeric.cのcob_subの実装 thisの保持する数値データに,引数で与えられたフィールドの保持する数値データを減算する
   *
   * @param field 減算する数値を保持するフィールド
   * @param opt 減算に関するオプション.詳しくはopensourceCOBOLを参照
   * @return 減算後のthisの保持する数値データ
   */
  public int sub(AbstractCobolField field, int opt) throws CobolStopRunException {
    CobolDecimal d1 = this.getDecimal();
    CobolDecimal d2 = field.getDecimal();
    d1.sub(d2);
    return d1.getField(this, opt);
  }

  /**
   * libcob/numeric.cのcob_add_intの実装? 保持する数値データに指定された値を加算する
   *
   * @param n thisの保持する数値データから加算する数値
   * @return 基本的に0が返される.詳しくはopensource COBOLを参照
   */
  public int addInt(int n) throws CobolStopRunException {
    if (n == 0) {
      return 0;
    }
    CobolDecimal d1 = this.getDecimal();
    CobolDecimal d2 = new CobolDecimal(n);
    d2.setScale(0);
    if (d1.getScale() != 0) {
      BigDecimal cobMexp = BigDecimal.TEN.pow(d1.getScale());
      d2.setValue(d2.getValue().multiply(cobMexp));
      d2.setScale(d1.getScale());
    }
    d1.setValue(d1.getValue().add(d2.getValue()));
    return d1.getField(this, 0);
  }

  /**
   * libcob/numeric.cのcob_add_packed
   *
   * @param n
   * @return
   */
  public abstract int addPackedInt(int n);

  /**
   * thisの保持する数値データに指定された値を減算する
   *
   * @param n thisの保持する数値データから減算する数値
   * @return 基本的に0が返される.詳しくはopensource COBOLを参照
   */
  public int subInt(int n) throws CobolStopRunException {
    return n == 0 ? 0 : this.addInt(-n);
  }

  /** libcob/numeric.cのcob_div_quotientの実装 */
  public int divQuotient(AbstractCobolField divisor, AbstractCobolField quotient, int opt)
      throws CobolStopRunException {
    AbstractCobolField dividend = this;
    CobolDecimal d1 = dividend.getDecimal();
    CobolDecimal d2 = divisor.getDecimal();
    CobolDecimal.cobD3.set(d1);

    d1.div(d2);
    if (d1.getScale() == CobolDecimal.DECIMAL_NAN) {
      CobolDecimal.cobD3.setScale(CobolDecimal.DECIMAL_NAN);
      // TODO 例外を投げるべきか?
      return 0;
    }
    CobolDecimal d4 = new CobolDecimal();
    d4.set(d1);
    int ret = d1.getField(quotient, opt);

    d4.shiftDecimal(quotient.getAttribute().getScale() - d4.getScale());

    d4.mul(d2);
    CobolDecimal.cobD3.sub(d4);

    return ret;
  }

  /**
   * libcob/numeric.cのcob_div_remainderの実装
   *
   * @param opt
   * @return
   */
  public int divRemainder(int opt) throws CobolStopRunException {
    return CobolDecimal.cobD3.getField(this, opt);
  }

  /**
   * libcob/numeric.cのcob_cmp_intの実装
   *
   * @param n
   * @return
   */
  public int cmpInt(int n) {
    CobolDecimal d1 = this.getDecimal();
    CobolDecimal d2 = new CobolDecimal(n);
    d2.setScale(0);
    return d1.compareTo(d2);
  }

  /**
   * libcob/numeric.cのcob_cmp_intの実装
   *
   * @param n
   * @return
   */
  public int cmpInt(long n) {
    return this.cmpInt((int) n);
  }

  /**
   * libcob/numeric.cのcob_cmp_uintの実装
   *
   * @param n
   * @return
   */
  public int cmpUint(int n) {
    return this.cmpInt(n);
  }

  /**
   * libcob/numeric.cのcob_cmp_uintの実装
   *
   * @param n
   * @return
   */
  public int cmpUint(long n) {
    return this.cmpUint((int) n);
  }

  /**
   * libcob/numeric.cのcob_numeric_cmpの実装
   *
   * @param field
   * @return
   */
  public int numericCompareTo(AbstractCobolField field) {
    CobolDecimal d1 = this.getDecimal();
    CobolDecimal d2 = field.getDecimal();
    return d1.compareTo(d2);
  }

  /**
   * thisの保持する数値データの符号を返す
   *
   * @return thisの保持する数値データが負ならば負数,0なら0,正なら正数を返す
   */
  public int getSign() {
    return this.getAttribute().isFlagHaveSign() ? this.realGetSign() : 0;
  }

  /**
   * thisの保持する数値データの符号を設定する
   *
   * @param sign 正符号を設定するときは正数,負符号を設定するときは負数,それ以外は0
   */
  public void putSign(int sign) {
    if (this.getAttribute().isFlagHaveSign()) {
      this.realPutSign(sign);
    }
  }

  protected AbstractCobolField preprocessOfMoving(AbstractCobolField src) {
    AbstractCobolField src1;

    // TODO 以下の分岐は不要?
    if (src == CobolConstant.quote
        || src == CobolConstant.zenQuote
        || src == CobolConstant.space
        || src == CobolConstant.zenSpace
        || src == CobolConstant.blank
        || src == CobolConstant.zenBlank
        || src == CobolConstant.zero
        || src == CobolConstant.zenZero) {
      src1 = src;
    } else {
      src1 = src;
    }

    if (src1.getAttribute().isTypeAlphanumAll() || src1.getAttribute().isTypeNationalAll()) {
      this.moveFromAll(src);
      return null;
    }

    if (this.getSize() == 0) {
      return null;
    }

    CobolFieldAttribute srcAttr = src1.getAttribute();
    CobolFieldAttribute dstAttr = this.getAttribute();
    if (!srcAttr.isTypeGroup()) {
      if ((srcAttr.isTypeNumeric() || srcAttr.isTypeAlphanum() || srcAttr.isTypeAlphanumEdited())
          && (dstAttr.isTypeNational() || dstAttr.isTypeNationalEdited())) {
        byte[] pTmp = null;
        int size = 0;
        if (srcAttr.isTypeNumericDisplay()
            || srcAttr.isTypeAlphanum()
            || srcAttr.isTypeAlphanumEdited()) {
          pTmp = CobolNationalField.judge_hankakujpn_exist(src1);
          size = CobolNationalField.workReturnSize;
        }
        if (pTmp != null) {
          src1.setDataStorage(new CobolDataStorage(pTmp));
          src1.setSize(size);
        }
        if (src1.size == 0) {
          src1 = CobolConstant.zenSpace;
        }
      }
    }

    if (src1.getSize() == 0) {
      src1 = CobolConstant.space;
    }

    return src;
  }

  public static class TmpTuple {
    public CobolDataStorage storage;
    public int size;

    TmpTuple(CobolDataStorage storage, int size) {
      this.storage = storage;
      this.size = size;
    }
  }

  protected void moveFromAll(AbstractCobolField src) {
    int size = 0;
    CobolDataStorage tmpSrcStorage = null;
    int tmpSrcSize = 0;
    boolean xToN = false;
    CobolFieldAttribute attr;
    int digcount;

    if (!(src.getAttribute().isTypeNational() || src.getAttribute().isTypeNationalEdited())
        && (this.attribute.isTypeNational() || this.attribute.isTypeNationalEdited())) {
      CobolDataStorage pTmp;
      byte[] pBytes = CobolNationalField.judge_hankakujpn_exist(src);
      pTmp = new CobolDataStorage(pBytes);
      size = CobolNationalField.workReturnSize;
      if (pTmp != null) {
        tmpSrcStorage = pTmp;
        tmpSrcSize = size;
      } else {
        tmpSrcSize = 0;
      }
      xToN = true;
    }

    if (xToN) {
      attr = new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_NATIONAL, 0, 0, 0, null);
    } else {
      attr = new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null);
    }
    if (this.attribute.isTypeNumeric()) {
      digcount = 18;
      attr.setType(CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY);
      attr.setDigits(18);
    } else {
      digcount = this.size;
    }
    if (digcount > AbstractCobolField.lastsize) {
      AbstractCobolField.lastdata = new CobolDataStorage(digcount);
      AbstractCobolField.lastsize = digcount;
    }

    AbstractCobolField temp = CobolFieldFactory.makeCobolField(digcount, lastdata, attr);

    if (xToN && tmpSrcSize > 1) {
      for (int i = 0; i < digcount; ++i) {
        lastdata.setByte(i, tmpSrcStorage.getByte(i % tmpSrcSize));
      }
    } else {
      if (src.getSize() == 1) {
        lastdata.memset(src.getDataStorage().getByte(0), digcount);
      } else {
        int i;
        for (i = 0; i < digcount; ++i) {
          lastdata.setByte(i, src.getDataStorage().getByte(i % src.getSize()));
        }

        int b = Byte.toUnsignedInt(lastdata.getByte(i - 1));
        if ((0x81 <= b && b <= 0x9F) || (0xE0 <= b && b <= 0xFC)) {
          lastdata.setByte(i - 1, (byte) ' ');
        }
      }
    }
    this.moveFrom(temp);
  }

  /**
   * 引数で与えらえられたデータからthisへの代入を行う
   *
   * @param field 代入元のデータ(AbstractCobolField型)
   */
  public abstract void moveFrom(AbstractCobolField field);

  /**
   * 引数で与えらえられたデータからthisへの代入を行う
   *
   * @param field 代入元のデータ(CobolDataStorage型)
   */
  public abstract void moveFrom(CobolDataStorage dataStrage);

  /**
   * 引数で与えらえられたデータからthisへの代入を行う
   *
   * @param field 代入元のデータ(byte[]型)
   */
  public abstract void moveFrom(byte[] bytes);

  /**
   * 引数で与えらえられたデータからthisへの代入を行う
   *
   * @param field 代入元のデータ(String型)
   */
  public void moveFrom(String s) {
    // The maximum number of digits of int type in decimal is 10

    byte[] bytes = s.getBytes(Charset.forName("SJIS"));

    CobolDataStorage storage = new CobolDataStorage(bytes.length);
    storage.memcpy(bytes);

    CobolFieldAttribute attr =
        new CobolFieldAttribute(
            CobolFieldAttribute.COB_TYPE_ALPHANUMERIC,
            bytes.length,
            0,
            0,
            String.format("X(%d)", bytes.length));

    AbstractCobolField tmp = CobolFieldFactory.makeCobolField(bytes.length, storage, attr);
    this.moveFrom(tmp);
  }

  /**
   * 引数で与えらえられたデータからthisへの代入を行う
   *
   * @param field 代入元のデータ(int型)
   */
  public void moveFrom(int number) {
    // The maximum number of digits of int type in decimal is 10
    final int length = 10;

    CobolDataStorage storage = new CobolDataStorage(length);
    String formatted_number_string = String.format("%10d", Math.abs(number));
    storage.memcpy(formatted_number_string, length);
    if (number < 0) {
      storage.setByte(length - 1, (byte) (storage.getByte(length - 1) + 0x40));
    }

    CobolFieldAttribute attr =
        new CobolFieldAttribute(
            CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY,
            length,
            0,
            CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
            "S9(10)");

    AbstractCobolField tmp = CobolFieldFactory.makeCobolField(length, storage, attr);
    this.moveFrom(tmp);
  }

  /**
   * 引数で与えらえられたデータからthisへの代入を行う
   *
   * @param field 代入元のデータ(double型)
   */
  public void moveFrom(double number) {
    String s = Double.toString(Math.abs(number));
    String ss;
    int scale;
    ss = s.replace("+", "").replace("-", "");
    int pointIndex = ss.indexOf('.');
    if (pointIndex < 0) {
      scale = 0;
    } else {
      scale = ss.length() - 1 - pointIndex;
      ss = ss.replace(".", "");
    }

    CobolDataStorage storage = new CobolDataStorage(ss.length());
    storage.memcpy(ss, ss.length());

    CobolFieldAttribute attr =
        new CobolFieldAttribute(
            CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY,
            ss.length(),
            scale,
            CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
            "");

    AbstractCobolField tmp = CobolFieldFactory.makeCobolField(ss.length(), storage, attr);
    if (number < 0) {
      tmp.putSign(-1);
    }
    this.moveFrom(tmp);
  }

  /**
   * 引数で与えらえられたデータからthisへの代入を行う
   *
   * @param field 代入元のデータ(BigDecimal型)
   */
  public abstract void moveFrom(BigDecimal number);

  /**
   * opensourceCOBOLのcob_check_numericの実装
   *
   * @param s
   */
  public void checkNumeric(String s) {}

  // TODO abstract指定
  /**
   * thisと引数で与えられたデータとの数値比較を行う
   *
   * @param field thisと比較するfield
   * @return 保持する数値データの比較を行い,this<fieldなら負の値,this==fieldなら0,this>fieldなら正の値
   */
  public int compareTo(AbstractCobolField other) {
    AbstractCobolField f1 = this;
    AbstractCobolField f2 = other;
    CobolFieldAttribute attr1 = f1.getAttribute();
    CobolFieldAttribute attr2 = f2.getAttribute();

    if (attr1.isTypeNational() || attr1.isTypeNationalAll() || attr1.isTypeNationalEdited()) {
      if (f2 == CobolConstant.quote) {
        f2 = CobolConstant.zenQuote;
      } else if (f2 == CobolConstant.space) {
        f2 = CobolConstant.zenSpace;
      } else if (f2 == CobolConstant.zero) {
        f2 = CobolConstant.zenZero;
      }
    }

    if (attr2.isTypeNational() || attr2.isTypeNationalAll() || attr2.isTypeNationalEdited()) {
      if (f1 == CobolConstant.quote) {
        f1 = CobolConstant.zenQuote;
      } else if (f1 == CobolConstant.space) {
        f1 = CobolConstant.zenSpace;
      } else if (f1 == CobolConstant.zero) {
        f1 = CobolConstant.zenZero;
      }
    }

    attr1 = f1.getAttribute();
    attr2 = f2.getAttribute();

    if (attr1.isTypeNumeric() && attr2.isTypeNumeric()) {
      return f1.numericCompareTo(f2);
    }
    if (attr2.isTypeAlphanumAll()) {
      if (f2 == CobolConstant.zero && attr1.isTypeNumeric()) {
        return f1.cmpInt(0);
      } else if (f2.getSize() == 1) {
        return f1.cmpChar(f2.getDataStorage().getByte(0));
      } else {
        return f1.cmpAll(f2);
      }
    } else if (attr1.isTypeAlphanumAll()) {
      if (f1 == CobolConstant.zero && attr2.isTypeNumeric()) {
        return -f2.cmpInt(0);
      } else if (f1.getSize() == 1) {
        return -f2.cmpChar(f1.getDataStorage().getByte(0));
      } else {
        return -f2.cmpAll(f1);
      }
    } else if (attr2.isTypeNationalAll()) {
      if (f2 == CobolConstant.zero && attr1.isTypeNumeric()) {
        return f1.cmpInt(0);
      } else if (f2.getSize() == 1) {
        return f1.cmpChar(f2.getDataStorage().getByte(0));
      } else {
        return f1.cmpAll(f2);
      }
    } else if (attr1.isTypeNationalAll()) {
      if (f1 == CobolConstant.zero && attr2.isTypeNumeric()) {
        return -f2.cmpInt(0);
      } else if (f1.getSize() == 1) {
        return -f2.cmpChar(f1.getDataStorage().getByte(0));
      } else {
        return -f2.cmpAll(f1);
      }
    } else if (attr1.isTypeGroup() || attr2.isTypeGroup()) {
      return f1.cmpSimpleStr(f2);
    } else {
      if (attr1.isTypeNumeric()) {
        if (attr1.getType() != CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY) {
          int tmpSize = attr1.getDigits();
          CobolDataStorage tmpBuff = new CobolDataStorage(48);
          CobolFieldAttribute tmpAttr = new CobolFieldAttribute(attr1);
          tmpAttr.setType(CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY);
          tmpAttr.setFlags(tmpAttr.getFlags() & ~CobolFieldAttribute.COB_FLAG_HAVE_SIGN);
          AbstractCobolField tmpField = CobolFieldFactory.makeCobolField(tmpSize, tmpBuff, tmpAttr);
          tmpField.moveFrom(f1);
          f1 = tmpField;
        } else if (attr1.isFlagSignSeparate()) {
          int tmpSize = attr1.getDigits();
          CobolDataStorage tmpBuff = new CobolDataStorage(48);
          CobolFieldAttribute tmpAttr = new CobolFieldAttribute(attr1);
          tmpAttr.setType(CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY);
          tmpAttr.setFlags(CobolFieldAttribute.COB_FLAG_HAVE_SIGN);
          AbstractCobolField tmpField = CobolFieldFactory.makeCobolField(tmpSize, tmpBuff, tmpAttr);
          tmpField.moveFrom(f1);
          f1 = tmpField;
        }
      }
      if (attr2.isTypeNumeric()) {
        if (attr2.getType() != CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY) {
          int tmpSize = attr2.getDigits();
          CobolDataStorage tmpBuff = new CobolDataStorage(48);
          CobolFieldAttribute tmpAttr = new CobolFieldAttribute(attr2);
          tmpAttr.setType(CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY);
          tmpAttr.setFlags(tmpAttr.getFlags() & ~CobolFieldAttribute.COB_FLAG_HAVE_SIGN);
          AbstractCobolField tmpField = CobolFieldFactory.makeCobolField(tmpSize, tmpBuff, tmpAttr);
          tmpField.moveFrom(f2);
          f2 = tmpField;
        } else if (attr2.isFlagSignSeparate()) {
          int tmpSize = attr2.getDigits();
          CobolDataStorage tmpBuff = new CobolDataStorage(48);
          CobolFieldAttribute tmpAttr = new CobolFieldAttribute(attr2);
          tmpAttr.setType(CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY);
          tmpAttr.setFlags(CobolFieldAttribute.COB_FLAG_HAVE_SIGN);
          AbstractCobolField tmpField = CobolFieldFactory.makeCobolField(tmpSize, tmpBuff, tmpAttr);
          tmpField.moveFrom(f2);
          f2 = tmpField;
        }
      }
    }
    return f1.cmpAlnum(f2);
  }

  /**
   * 引数で与えられた数値データを保持するフィールドと同じ数値を保持するCobolNumericFieldに変換する
   *
   * @param field 変換するクラス
   * @return 引数で与えられた数値データを保持するフィールドを同じ数値を保持するCobolNumericField型のフィールド
   */
  private AbstractCobolField numericFieldToNumericDisplayField(AbstractCobolField field) {
    CobolFieldAttribute attr = field.getAttribute();
    CobolDataStorage data = new CobolDataStorage(48);
    if (attr.isTypeNumeric()) {
      if (!attr.isTypeNumericDisplay()) {
        CobolFieldAttribute newAttr =
            new CobolFieldAttribute(
                CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY,
                attr.getDigits(),
                attr.getScale(),
                attr.getFlags() & (~CobolFieldAttribute.COB_FLAG_HAVE_SIGN),
                attr.getPic());
        CobolNumericField temp = new CobolNumericField(attr.getDigits(), data, newAttr);
        temp.moveFrom(field);
        field = temp;
      } else if (attr.isFlagHaveSign()) {
        CobolFieldAttribute newAttr =
            new CobolFieldAttribute(
                CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY,
                attr.getDigits(),
                attr.getScale(),
                CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
                attr.getPic());
        CobolNumericField temp = new CobolNumericField(attr.getDigits(), data, newAttr);
        temp.moveFrom(field);
        field = temp;
      }
    }
    return field;
  }

  /**
   * libcob/common.cのcommon_cmpcの実装
   *
   * @param s1
   * @param s1StartIndex s1のバイトデータにアクセスるするときの最小の添え字の相対位置
   * @param c
   * @param size
   * @return
   */
  protected int commonCmpc(CobolDataStorage s1, int s1StartIndex, int c, int size) {
    // TODO moduleを参照するコードを書く
    int ret;
    for (int i = 0; i < size; ++i) {
      if ((ret = s1.getByte(s1StartIndex + i) - c) != 0) {
        return ret;
      }
    }
    return 0;
  }

  /**
   * libcob/common.cのcob_cmp_allの実装
   *
   * @param field thisと比較するフィールド
   * @return
   */
  protected int compareAll(AbstractCobolField field) {
    int size = this.getSize();
    CobolDataStorage data = this.getDataStorage();
    int sign = this.getSign();
    int ret = 0;
    int p = 0;
    try {
      while (size >= field.getSize()) {
        // TODO moduleを参照するコードにする
        if ((ret = alnumCmps(data, p, field.getDataStorage(), 0, this.getSize(), null)) != 0) {
          throw new GotoException();
        }
        size -= field.getSize();
        p += field.getSize();
      }
      if (size > 0) {
        // TODO moduleを参照するコードにする
        ret = alnumCmps(data, 0, field.getDataStorage(), 0, this.getSize(), null);
      }
    } catch (Exception e) {
    }

    this.putSign(sign);
    return ret;
  }

  /**
   * libcob/common.cのalnum_cmpsの実装
   *
   * @param s1
   * @param s1Start s1のバイトデータにアクセスるするときの最初の添え字の相対位置
   * @param s2
   * @param s2Start s2のバイトデータにアクセスるするときの最初の添え字の相対位置
   * @param size
   * @param col
   * @return
   */
  protected int alnumCmps(
      CobolDataStorage s1,
      int s1Start,
      CobolDataStorage s2,
      int s2Start,
      int size,
      CobolDataStorage col) {
    int ret;
    if (col != null) {
      // TODO 実装
      throw new CobolRuntimeException(0, "未実装");
    } else {
      for (int i = 0; i < size; ++i) {
        if ((ret = s1.getByte(i + s1Start) - s2.getByte(i + s2Start)) != 0) {
          return ret;
        }
      }
    }
    return 0;
  }

  // TODO abstract指定
  public BigDecimal getBigDecimal() {
    return BigDecimal.ZERO;
  }

  /**
   * thisをCobolNumericFieldに変換する. indirect moveをするときに使用されることを想定している.
   *
   * @return thisからCobolNumericField型へ変換した値
   */
  public CobolNumericField getNumericField() {
    return new CobolNumericField(this.getSize(), this.getDataStorage(), this.getAttribute());
  }

  /**
   * libcob/common.cのcob_check_mvstrnumの実装
   *
   * @param field
   */
  public void checkMoveStrNum(AbstractCobolField field) {
    switch (this.getAttribute().getType()) {
      case CobolFieldAttribute.COB_TYPE_ALPHANUMERIC:
      case CobolFieldAttribute.COB_TYPE_ALPHANUMERIC_ALL:
      case CobolFieldAttribute.COB_TYPE_ALPHANUMERIC_EDITED:
        switch (field.getAttribute().getType()) {
          case CobolFieldAttribute.COB_TYPE_NUMERIC:
            /* case COB_TYPE_NUMERIC_DISPLAY: */
          case CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY:
          case CobolFieldAttribute.COB_TYPE_NUMERIC_PACKED:
          case CobolFieldAttribute.COB_TYPE_NUMERIC_FLOAT:
          case CobolFieldAttribute.COB_TYPE_NUMERIC_DOUBLE:
          case CobolFieldAttribute.COB_TYPE_NUMERIC_EDITED:
            CobolDataStorage data = this.getDataStorage();
            int firstIndex = this.getFirstDataIndex();
            for (int i = 0; i < this.getSize(); i++) {
              byte val = data.getByte(firstIndex + i);
              if (val < 0x30 || 0x39 < val) {
                System.out.println("Numeric value is expected");
                // TODO STOP RUNを呼ぶ
              }
            }
            break;
        }
        break;
    }
  }

  /**
   * libcob/move.c own_byte_memcpyの実装
   *
   * @param s1
   * @param s1StartIndex s1のバイトデータにアクセスるするときの最初の添え字の相対位置
   * @param s2
   * @param s2StartIndex s2のバイトデータにアクセスるするときの最初の添え字の相対位置
   * @param size
   */
  protected void ownByteMemcpy(
      CobolDataStorage s1, int s1StartIndex, CobolDataStorage s2, int s2StartIndex, int size) {
    int i = 0;
    do {
      s1.setByte(s1StartIndex + i, s2.getByte(s2StartIndex + i));
      i++;
    } while (--size > 0);
  }

  @Override
  public String toString() {
    return this.getString();
  }

  /**
   * libcob/common.cのcob_field_to_stringの実装 TODO CobolNationalFieldでオーバーライドしなくても済むように修正する.
   *
   * @return this.dataの保持するデータを文字列にして返す.
   */
  public String fieldToString() {
    CobolDataStorage data = this.getDataStorage();
    int i;
    for (i = this.getSize() - 1; i >= 0; --i) {
      if (data.getByte(i) != ' ' && data.getByte(i) != 0) {
        break;
      }
    }
    return new String(data.getByteArray(0, i + 1));
  }

  /** libcob/move.cのcob_set_intの実装 */
  public void setInt(int n) {
    CobolFieldAttribute attr =
        new CobolFieldAttribute(
            CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY,
            9,
            0,
            CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
            null);
    CobolDataStorage data = new CobolDataStorage(ByteBuffer.allocate(4).putInt(n).array());
    AbstractCobolField temp = CobolFieldFactory.makeCobolField(4, data, attr);
    this.moveFrom(temp);
  }

  /** libcob/move.cのcob_set_intの実装 */
  public void setInt(CobolDataStorage data) {
    this.setInt((int) data.intValue());
  }

  /**
   * libcob/common.cのcob_memcpyの実装
   *
   * @param src
   * @param size
   */
  public void memcpy(byte[] src, int size) {
    CobolFieldAttribute attr =
        new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null);
    AbstractCobolField temp =
        CobolFieldFactory.makeCobolField(size, new CobolDataStorage(src), attr);
    this.moveFrom(temp);
  }

  /**
   * libcob/common.cのcob_memcpyの実装
   *
   * @param src
   */
  public void memcpy(byte[] src) {
    this.memcpy(src, src.length);
  }

  /**
   * libcob/common.cのcob_memcpyの実装
   *
   * @param src
   * @param size
   */
  public void memcpy(String src, int size) {
    byte[] bytes = src.getBytes();
    this.memcpy(bytes, size);
  }

  /**
   * libcob/common.cのcob_memcpyの実装
   *
   * @param src
   */
  public void memcpy(String src) {
    this.memcpy(src.getBytes());
  }

  /**
   * libcob/common.cのcob_is_omittedの実装
   *
   * @return
   */
  public boolean isOmitted() {
    return this.dataStorage == null;
  }

  /**
   * libcob/common.cのcob_is_numericの実装
   *
   * @return
   */
  public boolean isNumeric() {
    int i;
    char c = 0;
    int sign = 0;
    switch (this.getAttribute().getType()) {
      case CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY:
      case CobolFieldAttribute.COB_TYPE_NUMERIC_FLOAT:
      case CobolFieldAttribute.COB_TYPE_NUMERIC_DOUBLE:
        return true;
      case CobolFieldAttribute.COB_TYPE_NUMERIC_PACKED:
        byte b = 0;
        for (i = 0; i < this.size - 1; ++i) {
          b = this.getDataStorage().getByte(i);
          if ((b & 0xF0) > 0x90 || (b & 0x0f) > 0x09) {
            return false;
          }
        }
        b = this.getDataStorage().getByte(i);
        if ((b & 0xf0) > 0x90) {
          return false;
        }
        sign = b & 0x0f;
        if (sign == 0x0f) {
          return true;
        }
        if (CobolUtil.nibbleCForUnsigned) {
          if (sign == 0x0c) {
            return true;
          }
        }
        return sign == 0x0c || sign == 0x0d;
      case CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY:
        int size = this.getFieldSize();
        int firstIndex = this.getFirstDataIndex();
        sign = this.getSign();
        this.putSign(1);
        for (i = 0; i < size; ++i) {
          c = (char) this.getDataStorage().getByte(i + firstIndex);
          if (!Character.isDigit(c)) {
            this.putSign(sign);
            return false;
          }
        }
        this.putSign(sign);
        return true;
      default:
        for (i = 0; i < this.size; ++i) {
          c = (char) this.getDataStorage().getByte(i);
          if (!Character.isDigit(c)) {
            return false;
          }
        }
        return true;
    }
  }

  /**
   * libcob/common.cのcob_is_alphaの実装
   *
   * @return
   */
  public boolean isAlpha() {
    for (int i = 0; i < this.size; ++i) {
      char c = (char) this.getDataStorage().getByte(i);
      if (!Character.isWhitespace(c) && !Character.isAlphabetic(c)) {
        return false;
      }
    }
    return true;
  }

  /**
   * libcob/common.cのcob_is_upperの実装
   *
   * @return
   */
  public boolean isUpper() {
    for (int i = 0; i < this.size; ++i) {
      char c = (char) this.getDataStorage().getByte(i);
      if (!Character.isWhitespace(c) && !Character.isUpperCase(c)) {
        return false;
      }
    }
    return true;
  }

  /**
   * libcob/common.cのcob_is_lowerの実装
   *
   * @return
   */
  public boolean isLower() {
    for (int i = 0; i < this.size; ++i) {
      char c = (char) this.getDataStorage().getByte(i);
      if (!Character.isWhitespace(c) && !Character.isLowerCase(c)) {
        return false;
      }
    }
    return true;
  }

  /**
   * libcob/common.cのcob_cmp_charの実装
   *
   * @param c
   * @return
   */
  public int cmpChar(byte c) {
    int sign = this.getSign();
    int ret = CobolUtil.commonCmpc(this.getDataStorage(), c, this.getSize());
    if (this.getAttribute().getType() != CobolFieldAttribute.COB_TYPE_NUMERIC_PACKED) {
      this.putSign(sign);
    }
    return ret;
  }

  private interface DataComparator {
    public int compare(CobolDataStorage s1, CobolDataStorage s2, int size, CobolDataStorage col);
  }

  private static DataComparator getComparator(AbstractCobolField f) {
    if (f.getAttribute().isTypeNational()) {
      return new DataComparator() {
        public int compare(
            CobolDataStorage s1, CobolDataStorage s2, int size, CobolDataStorage col) {
          return CobolUtil.nationalCmps(s1, s2, size, col);
        }
      };
    } else {
      return new DataComparator() {
        public int compare(
            CobolDataStorage s1, CobolDataStorage s2, int size, CobolDataStorage col) {
          return CobolUtil.alnumCmps(s1, s2, size, col);
        }
      };
    }
  }

  /**
   * libcob/common.cのcob_cmp_allの実装
   *
   * @param other
   * @return
   */
  public int cmpAll(AbstractCobolField other) {
    int ret = 0;
    DataComparator comparator = getComparator(this);
    int sign = 0;

    if ((this.getAttribute().getType() == CobolFieldAttribute.COB_TYPE_ALPHANUMERIC_ALL
            || this.getAttribute().getType() == CobolFieldAttribute.COB_TYPE_NATIONAL_ALL)
        && this.getSize() < other.getSize()) {
      int size = other.getSize();
      CobolDataStorage data = other.getDataStorage();
      sign = other.getSign();
      CobolDataStorage s = CobolModule.getCurrentModule().collating_sequence;
      OUTSIDE:
      do {
        while (size >= this.getSize()) {
          ret = comparator.compare(this.getDataStorage(), data, this.getSize(), s);
          if (ret != 0) {
            break OUTSIDE;
          }
          size -= this.getSize();
          data = data.getSubDataStorage(this.getSize());
        }
        if (size > 0) {
          comparator.compare(this.getDataStorage(), data, size, s);
        }
      } while (false);
    } else {
      int size = this.getSize();
      CobolDataStorage data = this.getDataStorage();
      sign = this.getSign();
      CobolDataStorage s = CobolModule.getCurrentModule().collating_sequence;
      OUTSIDE:
      do {
        while (size >= other.getSize()) {
          ret = comparator.compare(data, other.getDataStorage(), other.getSize(), s);
          if (ret != 0) {
            break OUTSIDE;
          }
          size -= other.getSize();
          data = data.getSubDataStorage(other.getSize());
        }
        if (size > 0) {
          comparator.compare(data, other.getDataStorage(), size, s);
        }
      } while (false);
    }

    if (this.getAttribute().getType() != CobolFieldAttribute.COB_TYPE_NUMERIC_PACKED) {
      this.putSign(sign);
    }
    return ret;
  }

  /**
   * libcob/common.cのcob_cmp_simple_strの実装
   *
   * @param other
   * @return
   */
  public int cmpSimpleStr(AbstractCobolField other) {
    AbstractCobolField lf, sf;
    DataComparator comparator = getComparator(this);
    if (this.getSize() < other.getSize()) {
      lf = other;
      sf = this;
    } else {
      lf = this;
      sf = other;
    }
    CobolDataStorage s = CobolModule.getCurrentModule().collating_sequence;
    int ret = comparator.compare(this.getDataStorage(), other.getDataStorage(), sf.getSize(), s);
    if (ret == 0) {
      if (lf.getSize() > sf.getSize()) {
        if ((lf.getAttribute().getType() & CobolFieldAttribute.COB_TYPE_NATIONAL) != 0) {
          ret =
              CobolUtil.isNationalPadding(
                  lf.getDataStorage().getSubDataStorage(sf.getSize()), lf.getSize() - sf.getSize());
        } else {
          ret =
              CobolUtil.commonCmpc(
                  lf.getDataStorage().getSubDataStorage(sf.getSize()),
                  (byte) ' ',
                  lf.getSize() - sf.getSize());
        }
        if (this.getSize() < other.getSize()) {
          ret = -ret;
        }
      }
    }
    return ret;
  }

  /**
   * libcob/common.cのcob_alnum_cmpsの実装
   *
   * @param other
   * @return
   */
  public int cmpAlnum(AbstractCobolField other) {
    int sign1 = this.getSign();
    int sign2 = other.getSign();

    if (this.getAttribute().isTypeNumericDisplay()) {
      this.putSign(1);
    }
    if (other.getAttribute().isTypeNumericDisplay()) {
      other.putSign(1);
    }

    int ret = this.cmpSimpleStr(other);

    if (this.getAttribute().getType() != CobolFieldAttribute.COB_TYPE_NUMERIC_PACKED) {
      this.putSign(sign1);
    }
    if (other.getAttribute().getType() != CobolFieldAttribute.COB_TYPE_NUMERIC_PACKED) {
      other.putSign(sign2);
    }
    return ret;
  }

  /**
   * libcob/common.cのcob_real_get_signの実装
   *
   * @return
   */
  public int realGetSign() {
    CobolDataStorage p;
    CobolFieldAttribute attr = this.getAttribute();
    byte b;

    switch (this.getAttribute().getType()) {
      case CobolFieldAttribute.COB_TYPE_NUMERIC:
        if (attr.isFlagSignLeading()) {
          p = this.getDataStorage();
        } else {
          p = this.getDataStorage().getSubDataStorage(this.getSize() - 1);
        }

        b = p.getByte(0);
        if (attr.isFlagSignSeparate()) {
          return b == '+' ? 1 : -1;
        } else {
          if ('0' <= b && b <= '9') {
            return 1;
          }
          if (b == ' ') {
            p.setByte(0, (byte) '0');
            return 1;
          }
          if (CobolModule.getCurrentModule().display_sign != 0) {
            return CobolUtil.getSignEbcdic(p);
          } else {
            // TODO マクロの分岐に関して調査
            // #ifdef COB_EBCDIC_MACHINE
            // CobolUtil.getSignAscii(p);
            // #else
            p.setByte(0, (byte) (b - 0x40));
            // #endif
            return -1;
          }
        }
      case CobolFieldAttribute.COB_TYPE_NUMERIC_PACKED:
        p = this.getDataStorage().getSubDataStorage(this.size - 1);
        return ((p.getByte(0) & 0x0f) == 0x0d) ? -1 : 1;
      default:
        return 0;
    }
  }

  /**
   * libcob/common.cのcob_real_put_signの実装
   *
   * @param sign
   */
  public void realPutSign(int sign) {
    CobolDataStorage p;
    CobolFieldAttribute attr = this.getAttribute();
    byte b;

    switch (this.getAttribute().getType()) {
      case CobolFieldAttribute.COB_TYPE_NUMERIC:
        if (attr.isFlagSignLeading()) {
          p = this.getDataStorage();
        } else {
          p = this.getDataStorage().getSubDataStorage(this.getSize() - 1);
        }

        b = p.getByte(0);
        if (attr.isFlagSignSeparate()) {
          char c = (sign < 0) ? '-' : '+';
          if (b != c) {
            p.setByte(0, (byte) c);
          }
        } else if (CobolModule.getCurrentModule().display_sign != 0) {
          CobolUtil.putSignEbcdic(p, sign);
        } else if (sign < 0) {
          p.setByte(0, (byte) (b + 0x40));
        }
      case CobolFieldAttribute.COB_TYPE_NUMERIC_PACKED:
        p = this.getDataStorage().getSubDataStorage(this.size - 1);
        if (sign < 0) {
          p.setByte(0, (byte) ((p.getByte(0) & 0xf0) | 0x0d));
        } else {
          p.setByte(0, (byte) ((p.getByte(0) & 0xf0) | 0x0c));
        }
        return;
      default:
        return;
    }
  }

  /**
   * libcob/move.cのcob_get_long_longの実装
   *
   * @return
   */
  public long getLong() {
    long n;
    CobolFieldAttribute attr =
        new CobolFieldAttribute(
            CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY,
            18,
            0,
            CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
            null);
    byte[] data = new byte[8];
    CobolDataStorage storage = new CobolDataStorage(data);
    AbstractCobolField field = CobolFieldFactory.makeCobolField(8, storage, attr);
    field.moveFrom(this);
    return ByteBuffer.wrap(data).getLong();
  }

  public long getLongValue() {
    return 0;
  }

  public void setLongValue(long n) {}

  /**
   * libcob/move.cのcob_hankaku_moveの実装
   *
   * @param src
   */
  public void hankakuMoveFrom(AbstractCobolField src) {
    // TODO 暫定実装
    this.moveFrom(src);
  }
}
