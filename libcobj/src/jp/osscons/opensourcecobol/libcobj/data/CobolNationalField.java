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

/** PIC 文字列がN(5)やN(9)の変数を表現するクラス. */
public class CobolNationalField extends AbstractCobolField {

  public static int workReturnSize;

  /**
   * コンストラクタ
   *
   * @param size データを格納するバイト配列の長さ
   * @param dataStorage データを格納するバイト配列を扱うオブジェクト
   * @param attribute 変数に関する様々な情報を保持するオブジェクト
   */
  public CobolNationalField(int size, CobolDataStorage dataStorage, CobolFieldAttribute attribute) {
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

  @Override
  public int getInt() {
    return 0;
  }

  @Override
  public CobolDecimal getDecimal() {
    return null;
  }

  /** 実装しないメソッド */
  public int addPackedInt(int n) {
    throw new CobolRuntimeException(0, "実装しないコード");
  }

  /**
   * 引数で与えらえられたデータからthisへの代入を行う
   *
   * @param field 代入元のデータ(AbstractCobolField型)
   */
  @Override
  public void moveFrom(AbstractCobolField src) {

    byte[] pTmp;
    int size;
    AbstractCobolField src2;

    AbstractCobolField src1 = this.preprocessOfMoving(src);
    if (src1 == null) {
      return;
    }

    // Convert Numeric
    switch (src1.getAttribute().getType()) {
      case CobolFieldAttribute.COB_TYPE_NUMERIC_PACKED:
      case CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY:
      case CobolFieldAttribute.COB_TYPE_NUMERIC_DOUBLE:
      case CobolFieldAttribute.COB_TYPE_NUMERIC_FLOAT:
        src1 = src1.getNumericField();
        break;
    }

    // HANKAKU TO ZENKAKU
    if (!src1.getAttribute().isTypeNational() && !src1.getAttribute().isTypeNationalEdited()) {
      workReturnSize = 0;
      pTmp = judge_hankakujpn_exist(src1);
      size = workReturnSize;
      CobolDataStorage pTmpStorage = new CobolDataStorage(size);
      pTmpStorage.setBytes(pTmp, size);
      CobolFieldAttribute attr =
          new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null);
      src2 = new CobolAlphanumericField(size, pTmpStorage, attr);
    } else {
      src2 = src1;
    }

    cob_move_from_alphanum(src2);
  }

  /**
   * CobolAlphanumericFieldからthisへの代入を行う
   *
   * @param field 代入元のデータ(AbstractCobolField型)
   */
  private void cob_move_from_alphanum(AbstractCobolField field) {
    // TODO UTF8対応
    CobolDataStorage data1;
    CobolDataStorage data2;
    int size1;
    int size2;
    int len;
    // TODO
    final int cobZenCSiz = 2;
    final byte[] cobZenBlk = {(byte) 0x81, (byte) 0x40};

    data1 = field.getDataStorage();
    size1 = field.getSize();
    data2 = this.getDataStorage();
    size2 = this.getSize();

    len = size2 - size1;

    if (size1 >= size2) {
      /* move string with truncation */
      if (this.getAttribute().isFlagJustified()) {
        data2.setBytes(data1, size2, 0, size1 - size2);
      } else {
        data2.setBytes(data1, size2);
      }
    } else {
      data2.fillBytes((byte) 0x20, size2);
      if (this.getAttribute().isFlagJustified()) {
        for (int i = 0; i < len; i += cobZenCSiz) {
          if (len - i >= cobZenCSiz) {
            data2.setBytes(i, cobZenBlk, cobZenCSiz);
          }
        }
        data2.getSubDataStorage(len).memcpy(data1, size1);
      } else {
        for (int i = 0; i < len; i += cobZenCSiz) {
          if (len - i >= cobZenCSiz) {
            data2.setBytes(i + size1, cobZenBlk, cobZenCSiz);
          }
        }
        data2.setBytes(data1, size1);
      }
    }
  }

  /**
   * libcob/move.cのjudge_hankakujpn_existの実装
   *
   * @param src
   * @return
   */
  public static byte[] judge_hankakujpn_exist(AbstractCobolField src) {
    byte[] tmpZenJpnWord = null;

    if (src.getSize() <= 0) {
      return null;
    }
    // TODO データのStrlenの差し替え
    if (src.getDataStorage().getByte(0) != 0) {
      tmpZenJpnWord = han2zen(src.getDataStorage().getData(), src.getSize());
    }
    return tmpZenJpnWord;
  }

  /**
   * libcob/move.c han2zenの実装
   *
   * @param str
   * @param size
   * @return
   */
  public static byte[] han2zen(byte[] str, int size) {
    byte[] buf;

    int c;
    int d = (byte) 0x00;
    int i, buIndex;

    buf = new byte[size * 2 + 1];

    for (i = 0, buIndex = 0; i < size; i++) {
      c = Byte.toUnsignedInt(str[i]);
      switch (c) {
        case 0x20:
          buf[buIndex] = (byte) 0x81;
          buf[buIndex + 1] = (byte) 0x40;
          buIndex += 2;
          break;
        case 0x21:
          buf[buIndex] = (byte) 0x81;
          buf[buIndex + 1] = (byte) 0x49;
          buIndex += 2;
          break;
        case 0x22:
          buf[buIndex] = (byte) 0x81;
          buf[buIndex + 1] = (byte) 0x68;
          buIndex += 2;
          break;
        case 0x23:
          buf[buIndex] = (byte) 0x81;
          buf[buIndex + 1] = (byte) 0x94;
          buIndex += 2;
          break;
        case 0x24:
          buf[buIndex] = (byte) 0x81;
          buf[buIndex + 1] = (byte) 0x90;
          buIndex += 2;
          break;
        case 0x25:
          buf[buIndex] = (byte) 0x81;
          buf[buIndex + 1] = (byte) 0x93;
          buIndex += 2;
          break;
        case 0x26:
          buf[buIndex] = (byte) 0x81;
          buf[buIndex + 1] = (byte) 0x95;
          buIndex += 2;
          break;
        case 0x27:
          buf[buIndex] = (byte) 0x81;
          buf[buIndex + 1] = (byte) 0x66;
          buIndex += 2;
          break;
        case 0x28:
          buf[buIndex] = (byte) 0x81;
          buf[buIndex + 1] = (byte) 0x69;
          buIndex += 2;
          break;
        case 0x29:
          buf[buIndex] = (byte) 0x81;
          buf[buIndex + 1] = (byte) 0x6A;
          buIndex += 2;
          break;
        case 0x2A:
          buf[buIndex] = (byte) 0x81;
          buf[buIndex + 1] = (byte) 0x96;
          buIndex += 2;
          break;
        case 0x2B:
          buf[buIndex] = (byte) 0x81;
          buf[buIndex + 1] = (byte) 0x7B;
          buIndex += 2;
          break;
        case 0x2C:
          buf[buIndex] = (byte) 0x81;
          buf[buIndex + 1] = (byte) 0x43;
          buIndex += 2;
          break;
        case 0x2D:
          buf[buIndex] = (byte) 0x81;
          buf[buIndex + 1] = (byte) 0x7C;
          buIndex += 2;
          break;
        case 0x2E:
          buf[buIndex] = (byte) 0x81;
          buf[buIndex + 1] = (byte) 0x44;
          buIndex += 2;
          break;
        case 0x2F:
          buf[buIndex] = (byte) 0x81;
          buf[buIndex + 1] = (byte) 0x5E;
          buIndex += 2;
          break;
        case 0x30:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x4F;
          buIndex += 2;
          break;
        case 0x31:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x50;
          buIndex += 2;
          break;
        case 0x32:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x51;
          buIndex += 2;
          break;
        case 0x33:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x52;
          buIndex += 2;
          break;
        case 0x34:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x53;
          buIndex += 2;
          break;
        case 0x35:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x54;
          buIndex += 2;
          break;
        case 0x36:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x55;
          buIndex += 2;
          break;
        case 0x37:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x56;
          buIndex += 2;
          break;
        case 0x38:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x57;
          buIndex += 2;
          break;
        case 0x39:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x58;
          buIndex += 2;
          break;
        case 0x3A:
          buf[buIndex] = (byte) 0x81;
          buf[buIndex + 1] = (byte) 0x46;
          buIndex += 2;
          break;
        case 0x3B:
          buf[buIndex] = (byte) 0x81;
          buf[buIndex + 1] = (byte) 0x47;
          buIndex += 2;
          break;
        case 0x3C:
          buf[buIndex] = (byte) 0x81;
          buf[buIndex + 1] = (byte) 0x83;
          buIndex += 2;
          break;
        case 0x3D:
          buf[buIndex] = (byte) 0x81;
          buf[buIndex + 1] = (byte) 0x81;
          buIndex += 2;
          break;
        case 0x3E:
          buf[buIndex] = (byte) 0x81;
          buf[buIndex + 1] = (byte) 0x84;
          buIndex += 2;
          break;
        case 0x3F:
          buf[buIndex] = (byte) 0x81;
          buf[buIndex + 1] = (byte) 0x48;
          buIndex += 2;
          break;
        case 0x40:
          buf[buIndex] = (byte) 0x81;
          buf[buIndex + 1] = (byte) 0x97;
          buIndex += 2;
          break;
        case 0x41:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x60;
          buIndex += 2;
          break;
        case 0x42:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x61;
          buIndex += 2;
          break;
        case 0x43:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x62;
          buIndex += 2;
          break;
        case 0x44:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x63;
          buIndex += 2;
          break;
        case 0x45:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x64;
          buIndex += 2;
          break;
        case 0x46:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x65;
          buIndex += 2;
          break;
        case 0x47:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x66;
          buIndex += 2;
          break;
        case 0x48:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x67;
          buIndex += 2;
          break;
        case 0x49:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x68;
          buIndex += 2;
          break;
        case 0x4A:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x69;
          buIndex += 2;
          break;
        case 0x4B:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x6A;
          buIndex += 2;
          break;
        case 0x4C:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x6B;
          buIndex += 2;
          break;
        case 0x4D:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x6C;
          buIndex += 2;
          break;
        case 0x4E:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x6D;
          buIndex += 2;
          break;
        case 0x4F:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x6E;
          buIndex += 2;
          break;
        case 0x50:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x6F;
          buIndex += 2;
          break;
        case 0x51:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x70;
          buIndex += 2;
          break;
        case 0x52:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x71;
          buIndex += 2;
          break;
        case 0x53:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x72;
          buIndex += 2;
          break;
        case 0x54:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x73;
          buIndex += 2;
          break;
        case 0x55:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x74;
          buIndex += 2;
          break;
        case 0x56:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x75;
          buIndex += 2;
          break;
        case 0x57:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x76;
          buIndex += 2;
          break;
        case 0x58:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x77;
          buIndex += 2;
          break;
        case 0x59:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x78;
          buIndex += 2;
          break;
        case 0x5A:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x79;
          buIndex += 2;
          break;
        case 0x5B:
          buf[buIndex] = (byte) 0x81;
          buf[buIndex + 1] = (byte) 0x6D;
          buIndex += 2;
          break;
        case 0x5C:
          buf[buIndex] = (byte) 0x81;
          buf[buIndex + 1] = (byte) 0x8F;
          buIndex += 2;
          break;
        case 0x5D:
          buf[buIndex] = (byte) 0x81;
          buf[buIndex + 1] = (byte) 0x6E;
          buIndex += 2;
          break;
        case 0x5E:
          buf[buIndex] = (byte) 0x81;
          buf[buIndex + 1] = (byte) 0x4F;
          buIndex += 2;
          break;
        case 0x5F:
          buf[buIndex] = (byte) 0x81;
          buf[buIndex + 1] = (byte) 0x51;
          buIndex += 2;
          break;
        case 0x60:
          buf[buIndex] = (byte) 0x81;
          buf[buIndex + 1] = (byte) 0x65;
          buIndex += 2;
          break;
        case 0x61:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x81;
          buIndex += 2;
          break;
        case 0x62:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x82;
          buIndex += 2;
          break;
        case 0x63:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x83;
          buIndex += 2;
          break;
        case 0x64:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x84;
          buIndex += 2;
          break;
        case 0x65:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x85;
          buIndex += 2;
          break;
        case 0x66:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x86;
          buIndex += 2;
          break;
        case 0x67:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x87;
          buIndex += 2;
          break;
        case 0x68:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x88;
          buIndex += 2;
          break;
        case 0x69:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x89;
          buIndex += 2;
          break;
        case 0x6A:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x8A;
          buIndex += 2;
          break;
        case 0x6B:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x8B;
          buIndex += 2;
          break;
        case 0x6C:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x8C;
          buIndex += 2;
          break;
        case 0x6D:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x8D;
          buIndex += 2;
          break;
        case 0x6E:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x8E;
          buIndex += 2;
          break;
        case 0x6F:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x8F;
          buIndex += 2;
          break;
        case 0x70:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x90;
          buIndex += 2;
          break;
        case 0x71:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x91;
          buIndex += 2;
          break;
        case 0x72:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x92;
          buIndex += 2;
          break;
        case 0x73:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x93;
          buIndex += 2;
          break;
        case 0x74:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x94;
          buIndex += 2;
          break;
        case 0x75:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x95;
          buIndex += 2;
          break;
        case 0x76:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x96;
          buIndex += 2;
          break;
        case 0x77:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x97;
          buIndex += 2;
          break;
        case 0x78:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x98;
          buIndex += 2;
          break;
        case 0x79:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x99;
          buIndex += 2;
          break;
        case 0x7A:
          buf[buIndex] = (byte) 0x82;
          buf[buIndex + 1] = (byte) 0x9A;
          buIndex += 2;
          break;
        case 0x7B:
          buf[buIndex] = (byte) 0x81;
          buf[buIndex + 1] = (byte) 0x6F;
          buIndex += 2;
          break;
        case 0x7C:
          buf[buIndex] = (byte) 0x81;
          buf[buIndex + 1] = (byte) 0x62;
          buIndex += 2;
          break;
        case 0x7D:
          buf[buIndex] = (byte) 0x81;
          buf[buIndex + 1] = (byte) 0x70;
          buIndex += 2;
          break;
        case 0x7E:
          buf[buIndex] = (byte) 0x81;
          buf[buIndex + 1] = (byte) 0x60;
          buIndex += 2;
          break;
        case 0xA1:
          buf[buIndex] = (byte) 0x81;
          buf[buIndex + 1] = (byte) 0x42;
          buIndex += 2;
          break;
        case 0xA2:
          buf[buIndex] = (byte) 0x81;
          buf[buIndex + 1] = (byte) 0x75;
          buIndex += 2;
          break;
        case 0xA3:
          buf[buIndex] = (byte) 0x81;
          buf[buIndex + 1] = (byte) 0x76;
          buIndex += 2;
          break;
        case 0xA4:
          buf[buIndex] = (byte) 0x81;
          buf[buIndex + 1] = (byte) 0x41;
          buIndex += 2;
          break;
        case 0xA5:
          buf[buIndex] = (byte) 0x81;
          buf[buIndex + 1] = (byte) 0x45;
          buIndex += 2;
          break;
        case 0xA6:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x92;
          buIndex += 2;
          break;
        case 0xA7:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x40;
          buIndex += 2;
          break;
        case 0xA8:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x42;
          buIndex += 2;
          break;
        case 0xA9:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x44;
          buIndex += 2;
          break;
        case 0xAA:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x46;
          buIndex += 2;
          break;
        case 0xAB:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x48;
          buIndex += 2;
          break;
        case 0xAC:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x83;
          buIndex += 2;
          break;
        case 0xAD:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x85;
          buIndex += 2;
          break;
        case 0xAE:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x87;
          buIndex += 2;
          break;
        case 0xAF:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x62;
          buIndex += 2;
          break;
        case 0xB0:
          buf[buIndex] = (byte) 0x81;
          buf[buIndex + 1] = (byte) 0x5B;
          buIndex += 2;
          break;
        case 0xB1:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x41;
          buIndex += 2;
          break;
        case 0xB2:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x43;
          buIndex += 2;
          break;
        case 0xB3:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x45;
          buIndex += 2;
          break;
        case 0xB4:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x47;
          buIndex += 2;
          break;
        case 0xB5:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x49;
          buIndex += 2;
          break;
        case 0xB6:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x4A;
          buIndex += 2;
          break;
        case 0xB7:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x4C;
          buIndex += 2;
          break;
        case 0xB8:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x4E;
          buIndex += 2;
          break;
        case 0xB9:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x50;
          buIndex += 2;
          break;
        case 0xBA:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x52;
          buIndex += 2;
          break;
        case 0xBB:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x54;
          buIndex += 2;
          break;
        case 0xBC:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x56;
          buIndex += 2;
          break;
        case 0xBD:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x58;
          buIndex += 2;
          break;
        case 0xBE:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x5A;
          buIndex += 2;
          break;
        case 0xBF:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x5C;
          buIndex += 2;
          break;
        case 0xC0:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x5E;
          buIndex += 2;
          break;
        case 0xC1:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x60;
          buIndex += 2;
          break;
        case 0xC2:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x63;
          buIndex += 2;
          break;
        case 0xC3:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x65;
          buIndex += 2;
          break;
        case 0xC4:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x67;
          buIndex += 2;
          break;
        case 0xC5:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x69;
          buIndex += 2;
          break;
        case 0xC6:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x6A;
          buIndex += 2;
          break;
        case 0xC7:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x6B;
          buIndex += 2;
          break;
        case 0xC8:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x6C;
          buIndex += 2;
          break;
        case 0xC9:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x6D;
          buIndex += 2;
          break;
        case 0xCA:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x6E;
          buIndex += 2;
          break;
        case 0xCB:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x71;
          buIndex += 2;
          break;
        case 0xCC:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x74;
          buIndex += 2;
          break;
        case 0xCD:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x77;
          buIndex += 2;
          break;
        case 0xCE:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x7A;
          buIndex += 2;
          break;
        case 0xCF:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x7D;
          buIndex += 2;
          break;
        case 0xD0:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x7E;
          buIndex += 2;
          break;
        case 0xD1:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x80;
          buIndex += 2;
          break;
        case 0xD2:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x81;
          buIndex += 2;
          break;
        case 0xD3:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x82;
          buIndex += 2;
          break;
        case 0xD4:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x84;
          buIndex += 2;
          break;
        case 0xD5:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x86;
          buIndex += 2;
          break;
        case 0xD6:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x88;
          buIndex += 2;
          break;
        case 0xD7:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x89;
          buIndex += 2;
          break;
        case 0xD8:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x8A;
          buIndex += 2;
          break;
        case 0xD9:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x8B;
          buIndex += 2;
          break;
        case 0xDA:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x8C;
          buIndex += 2;
          break;
        case 0xDB:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x8D;
          buIndex += 2;
          break;
        case 0xDC:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x8F;
          buIndex += 2;
          break;
        case 0xDD:
          buf[buIndex] = (byte) 0x83;
          buf[buIndex + 1] = (byte) 0x93;
          buIndex += 2;
          break;
        case 0xDE:
          /* Dakuten */
          if ((d >= 0xB6 && d <= 0xC4) || (d >= 0xCA && d <= 0xCE)) {
            buf[buIndex - 1]++;
          } else if (d == 0xB3) {
            buf[buIndex - 1] = (byte) 0x94;
          } else {
            buf[buIndex] = (byte) 0x81;
            buf[buIndex + 1] = (byte) 0x4A;
            buIndex += 2;
          }
          break;
        case 0xDF:
          /* Han-dakuten */
          if (d >= 0xCA && d <= 0xCE) {
            buf[buIndex - 1] += 2;
          } else {
            buf[buIndex] = (byte) 0x81;
            buf[buIndex + 1] = (byte) 0x4B;
            buIndex += 2;
          }
          break;
        case 0:
        case 255:
          buf[buIndex] = (byte) c;
          buIndex++;
          buf[buIndex] = (byte) c;
          buIndex++;
          if (i + 1 < size) {
            if (str[i + 1] == str[i]) {
              i++;
            }
          }
          break;
        default:
          if ((byte) 0 < c && c < (byte) 0x20) {
            // #define COB_SJSPC "\x81\x40"
            buf[buIndex] = (byte) 0x81;
            buf[buIndex + 1] = (byte) 0x40;
            buIndex += 2;
          } else {
            buf[buIndex] = str[i];
            buIndex++;
            i++;
            buf[buIndex] = str[i];
            buIndex++;
          }
          break;
      }
      d = c;
    }
    buf[buIndex] = (byte) 0x00;
    workReturnSize = buIndex;
    return buf;
  }

  /**
   * 引数で与えらえられたデータからthisへの代入を行う
   *
   * @param field 代入元のデータ(dataStorage型)
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
    dataStorage.setBytes(bytes);
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
      CobolDataStorage data = new CobolDataStorage(bytes);
      CobolFieldAttribute a =
          new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null);
      CobolAlphanumericField f = new CobolAlphanumericField(bytes.length, data, a);
      this.moveFrom(f);
    } catch (UnsupportedEncodingException e) {
      e.printStackTrace();
      throw new CobolRuntimeException(CobolRuntimeException.COBOL_FITAL_ERROR, "エンコードエラー");
    }
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
   * @param field 代入元のデータ(int型)
   */
  @Override
  public void moveFrom(double number) {}

  /**
   * 引数で与えらえられたデータからthisへの代入を行う
   *
   * @param field 代入元のデータ(int型)
   */
  @Override
  public void moveFrom(BigDecimal number) {}

  /**
   * 引数で与えらえられたデータからthisへの代入を行う
   *
   * @param field 代入元のデータ(int型)
   */
  @Override
  public void setDecimal(BigDecimal decimal) {}

  /**
   * thisをCobolNumericFieldに変換する. indirect moveをするときに使用されることを想定している.
   *
   * @return thisからCobolNumericField型へ変換した値
   */
  @Override
  public CobolNumericField getNumericField() {
    int size = this.getSize() / 2;
    int scale = this.getAttribute().getScale();
    CobolFieldAttribute attr =
        new CobolFieldAttribute(
            CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY,
            size,
            scale,
            CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
            null);
    CobolDataStorage data = new CobolDataStorage(size);
    CobolNumericField field = new CobolNumericField(size, data, attr);
    field.moveFrom(this);
    return field;
  }
}
