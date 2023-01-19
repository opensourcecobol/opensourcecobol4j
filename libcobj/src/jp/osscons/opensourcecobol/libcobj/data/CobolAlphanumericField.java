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

/** PIC 文字列がX(5)やX(9)の変数を表現するクラス. */
public class CobolAlphanumericField extends AbstractCobolField {

  /**
   * コンストラクタ
   *
   * @param size データを格納するバイト配列の長さ
   * @param dataStorage データを格納するバイト配列を扱うオブジェクト
   * @param attribute 変数に関する様々な情報を保持するオブジェクト
   */
  public CobolAlphanumericField(
      int size, CobolDataStorage dataStorage, CobolFieldAttribute attribute) {
    super(size, dataStorage, attribute);
  }

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
    try {
      return new String(dataStorage.getByteArray(0, this.getSize()), "SJIS");
    } catch (UnsupportedEncodingException e) {
      // TODO ログの対応
      e.printStackTrace();
      throw new CobolRuntimeException(CobolRuntimeException.COBOL_FITAL_ERROR, "エンコードエラー");
    }
  }

  @Override
  public int getInt() {
    throw new CobolRuntimeException(CobolRuntimeException.COBOL_FITAL_ERROR, "未対応");
  }

  @Override
  public void setDecimal(BigDecimal decimal) {
    // TODO 自動生成されたメソッド・スタブ

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
        this.moveDisplayToAlnum(src1);
        return;
      case CobolFieldAttribute.COB_TYPE_NUMERIC_PACKED:
      case CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY:
      case CobolFieldAttribute.COB_TYPE_NUMERIC_DOUBLE:
      case CobolFieldAttribute.COB_TYPE_NUMERIC_FLOAT:
        this.moveFrom(src1.getNumericField());
        return;
      case CobolFieldAttribute.COB_TYPE_ALPHANUMERIC:
      case CobolFieldAttribute.COB_TYPE_NATIONAL:
      case CobolFieldAttribute.COB_TYPE_NUMERIC_EDITED:
      case CobolFieldAttribute.COB_TYPE_ALPHANUMERIC_EDITED:
      case CobolFieldAttribute.COB_TYPE_NATIONAL_EDITED:
      case CobolFieldAttribute.COB_TYPE_GROUP:
        this.moveAlphanumToAlphanum(src1);
        return;
      default:
        throw new CobolRuntimeException(0, "未実装");
    }
  }

  /**
   * CobolNumericDisplayからCobolAlphanumericFieldへのMOVE
   *
   * @param field 転記元のCobolNumericDisplay型のフィールド
   */
  public void moveDisplayToAlnum(AbstractCobolField field) {
    CobolDataStorage data1 = field.getDataStorage();
    int data1FirstIndex = field.getFirstDataIndex();
    int size1 = field.getFieldSize();
    int sign = field.getSign();
    CobolDataStorage data2 = this.getDataStorage();
    int size2 = this.getSize();

    if (size1 >= size2) {
      for (int i = 0; i < size2 && data1FirstIndex + i < size1; ++i) {
        byte val = data1.getByte(data1FirstIndex + i);
        data2.setByte(i, (byte) (val >= 0x70 ? val - 0x40 : val));
      }
    } else {
      int diff = size2 - size1;
      int zeroSize = 0;
      int i = 0;
      for (; i < size1; ++i) {
        byte val = data1.getByte(data1FirstIndex + i);
        data2.setByte(i, (byte) (val >= 0x70 ? val - 0x40 : val));
      }
      if (field.getAttribute().getScale() < 0) {
        for (; i < field.getAttribute().getDigits(); ++i) {
          data2.setByte(i, (byte) '0');
        }
      }
      if (diff - zeroSize > 0) {
        for (; i < size2; ++i) {
          data2.setByte(i, (byte) ' ');
        }
      }
    }
    field.putSign(sign);
  }

  /**
   * CobolNumericDisplayからCobolAlphanumericFieldへのMOVE
   *
   * @param field 転記元のCobolNumericDisplay型のフィールド
   */
  public void moveAlphanumToAlphanum(AbstractCobolField field) {
    CobolAlphanumericField.moveAlphanumToAlphanum(this, field);
  }

  public static void moveAlphanumToAlphanum(AbstractCobolField dst, AbstractCobolField src) {
    CobolDataStorage data1 = src.getDataStorage();
    int size1 = src.getSize();
    CobolDataStorage data2 = dst.getDataStorage();
    int size2 = dst.getSize();
    if (size1 >= size2) {
      if (dst.getAttribute().isFlagJustified()) {
        for (int i = 0; i < size2; ++i) {
          data2.setByte(i, data1.getByte(size1 - size2 + i));
        }
      } else {
        for (int i = 0; i < size2; ++i) {
          data2.setByte(i, data1.getByte(i));
        }
      }
    } else {
      if (dst.getAttribute().isFlagJustified()) {
        for (int i = 0; i < size2 - size1; ++i) {
          data2.setByte(i, (byte) 0x20);
        }
        for (int i = 0; i < size1; ++i) {
          data2.setByte(size2 - size1 + i, data1.getByte(i));
        }
      } else {
        for (int i = 0; i < size1; ++i) {
          data2.setByte(i, data1.getByte(i));
        }
        for (int i = 0; i < size2 - size1; ++i) {
          data2.setByte(size1 + i, (byte) 0x20);
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
      byte[] bytes = string.getBytes("SJIS");
      int length = Math.min(bytes.length, this.getSize());
      CobolDataStorage data = this.getDataStorage();
      // ' '埋め
      for (int i = 0; i < this.getSize(); ++i) {
        data.setByte(i, (byte) 0x20);
      }
      for (int i = 0; i < length; ++i) {
        data.setByte(i, bytes[i]);
      }
    } catch (UnsupportedEncodingException e) {
      // TODO ログの対応
      e.printStackTrace();
      throw new CobolRuntimeException(CobolRuntimeException.COBOL_FITAL_ERROR, "エンコードエラー");
    }
  }

  /**
   * thisをCobolNumericFieldに変換する. indirect moveをするときに使用されることを想定している.
   *
   * @return thisからCobolNumericField型へ変換した値
   */
  @Override
  public CobolNumericField getNumericField() {
    int size = 36;
    int scale = 18;
    CobolFieldAttribute attr =
        new CobolFieldAttribute(
            CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY,
            size,
            scale,
            CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
            null);
    CobolDataStorage data = new CobolDataStorage(64);
    CobolNumericField field = new CobolNumericField(size, data, attr);
    field.moveFrom(this);
    return field;
  }

  /**
   * 引数で与えらえられたデータからthisへの代入を行う
   *
   * @param field 代入元のデータ(int型)
   */
  @Override
  public void moveFrom(int number) {
    // TODO 自動生成されたメソッド・スタブ
  }

  /**
   * 引数で与えらえられたデータからthisへの代入を行う
   *
   * @param field 代入元のデータ(double型)
   */
  @Override
  public void moveFrom(double number) {
    // TODO 自動生成されたメソッド・スタブ
    this.moveFrom((int) number);
  }

  /**
   * 引数で与えらえられたデータからthisへの代入を行う
   *
   * @param field 代入元のデータ(BigDecimal型)
   */
  @Override
  public void moveFrom(BigDecimal number) {
    // TODO 自動生成されたメソッド・スタブ

  }

  /**
   * 引数で与えらえられたデータからthisへの代入を行う
   *
   * @param field 代入元のデータ(CobolDataStorage]型)
   */
  @Override
  public void moveFrom(CobolDataStorage dataStrage) {
    // TODO 自動生成されたメソッド・スタブ

  }

  /** 実装しないメソッド */
  public int addPackedInt(int n) {
    throw new CobolRuntimeException(0, "実装しないコード");
  }
}
