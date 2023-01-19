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

/** 集団項目を扱う */
public class CobolGroupField extends AbstractCobolField {

  /**
   * コンストラクタ
   *
   * @param size データを格納するバイト配列の長さ
   * @param dataStorage データを格納するバイト配列を扱うオブジェクト
   * @param attribute 変数に関する様々な情報を保持するオブジェクト
   */
  public CobolGroupField(int size, CobolDataStorage dataStorage, CobolFieldAttribute attribute) {
    super(size, dataStorage, attribute);
  }

  /** this.dataの保持するバイト配列のコピーを返す */
  @Override
  public byte[] getBytes() {
    return dataStorage.getData(size);
  }

  /**
   * thisの文字列表現をかえす.(toStringだけで十分か?)
   *
   * @return thisの文字列表現
   */
  @Override
  public String getString() {
    try {
      return new String(dataStorage.getData(), "SJIS");
    } catch (UnsupportedEncodingException e) {
      e.printStackTrace();
      throw new CobolRuntimeException(CobolRuntimeException.COBOL_FITAL_ERROR, "エンコードエラー");
    }
  }

  /** TODO */
  @Override
  public int getInt() {
    return 0;
  }

  /** TODO */
  @Override
  public CobolDecimal getDecimal() {
    return null;
  }

  /** TODO */
  @Override
  public void setDecimal(BigDecimal decimal) {}

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

    CobolAlphanumericField.moveAlphanumToAlphanum(this, src1);
  }

  /**
   * 引数で与えらえられたデータからthisへの代入を行う
   *
   * @param field 代入元のデータ(CobolDataStorage型)
   */
  @Override
  public void moveFrom(CobolDataStorage dataStrage) {}

  /**
   * 引数で与えらえられたデータからthisへの代入を行う
   *
   * @param field 代入元のデータ(byte[]型)
   */
  @Override
  public void moveFrom(byte[] bytes) {
    if (bytes.length >= this.size) {
      this.dataStorage.setBytes(bytes, this.size);
    } else {
      this.dataStorage.setBytes(bytes, bytes.length);
      this.dataStorage.fillBytes(bytes.length, (byte) 0x20, this.size);
    }
  }

  /**
   * 引数で与えらえられたデータからthisへの代入を行う
   *
   * @param field 代入元のデータ(String型)
   */
  @Override
  public void moveFrom(String string) {
    byte[] bytes;
    try {
      bytes = string.getBytes("SJIS");
    } catch (UnsupportedEncodingException e) {
      e.printStackTrace();
      throw new CobolRuntimeException(CobolRuntimeException.COBOL_FITAL_ERROR, "エンコードエラー");
    }

    this.moveFrom(bytes);
  }

  /**
   * 引数で与えらえられたデータからthisへの代入を行う
   *
   * @param field 代入元のデータ(int型)
   */
  @Override
  public void moveFrom(int number) {}

  /**
   * 引数で与えらえられたデータからthisへの代入を行う
   *
   * @param field 代入元のデータ(double型)
   */
  @Override
  public void moveFrom(double number) {}

  /**
   * 引数で与えらえられたデータからthisへの代入を行う
   *
   * @param field 代入元のデータ(BigDecimal型)
   */
  @Override
  public void moveFrom(BigDecimal number) {}

  /** 実装しないメソッド */
  public int addPackedInt(int n) {
    throw new CobolRuntimeException(0, "実装しないコード");
  }
}
