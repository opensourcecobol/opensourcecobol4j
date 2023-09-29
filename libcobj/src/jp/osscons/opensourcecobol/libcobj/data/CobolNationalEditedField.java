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
import java.nio.ByteOrder;
import jp.osscons.opensourcecobol.libcobj.common.CobolConstant;

public class CobolNationalEditedField extends AbstractCobolField {
  /**
   * コンストラクタ
   *
   * @param size データを格納するバイト配列の長さ
   * @param dataStorage データを格納するバイト配列を扱うオブジェクト
   * @param attribute 変数に関する様々な情報を保持するオブジェクト
   */
  public CobolNationalEditedField(
      int size, CobolDataStorage dataStorage, CobolFieldAttribute attribute) {
    super(size, dataStorage, attribute);
  }

  @Override
  public byte[] getBytes() {
    return new byte[0];
  }

  @Override
  public String getString() {
    return null;
  }

  @Override
  public CobolDecimal getDecimal() {
    return null;
  }

  @Override
  public void setDecimal(BigDecimal decimal) {}

  @Override
  public int addPackedInt(int n) {
    return 0;
  }

  @Override
  public void moveFrom(AbstractCobolField src) {
    AbstractCobolField src1 = this.preprocessOfMoving(src);
    if (src1 == null) {
      return;
    }

    switch (src1.getAttribute().getType()) {
      case CobolFieldAttribute.COB_TYPE_NUMERIC_PACKED:
      case CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY:
      case CobolFieldAttribute.COB_TYPE_NUMERIC_FLOAT:
      case CobolFieldAttribute.COB_TYPE_NUMERIC_DOUBLE:
        this.moveFrom(src1.getNumericField());
        break;
      case CobolFieldAttribute.COB_TYPE_GROUP:
        CobolAlphanumericField.moveAlphanumToAlphanum(this, src1);
        break;
      default:
        this.moveAlphanumToNationalEdited(src1);
        break;
    }
  }

  private void moveAlphanumToNationalEdited(AbstractCobolField src) {
    CobolDataStorage srcd = src.getDataStorage();
    CobolDataStorage dstd = this.getDataStorage();
    int srcp = src.getFirstDataIndex();
    int dstp = 0;
    int max = src.getFieldSize();
    dstd.memset((byte) ' ', this.getSize());

    final int sizeOfInt = 4;

    byte[] picBytes = this.getAttribute().getPic().getBytes();
    for (int p = 0; p < picBytes.length; ) {
      byte c = picBytes[p++];
      ByteBuffer buf = ByteBuffer.wrap(picBytes, p, sizeOfInt);
      buf.order(ByteOrder.LITTLE_ENDIAN);
      int n = buf.getInt();
      p += sizeOfInt;
      for (; n > 0; --n) {
        switch (c) {
          case 'N':
            if (srcp < max) {
              dstd.setByte(dstp++, srcd.getByte(srcp++));
              dstd.setByte(dstp++, srcd.getByte(srcp++));
            } else {
              dstd.memcpy(dstp, CobolConstant.ZENBLK, CobolConstant.ZENCSIZ);
              dstp += CobolConstant.ZENCSIZ;
            }
            break;
          case '/':
            dstd.memcpy(dstp, CobolConstant.ZENSLAS, CobolConstant.ZENCSIZ);
            dstp += CobolConstant.ZENCSIZ;
            break;
          case 'B':
            dstd.memcpy(dstp, CobolConstant.ZENSPC, CobolConstant.ZENCSIZ);
            dstp += CobolConstant.ZENCSIZ;
            break;
          case '0':
            dstd.memcpy(dstp, CobolConstant.ZENZERO, CobolConstant.ZENCSIZ);
            dstp += CobolConstant.ZENCSIZ;
            break;
          default:
            dstd.setByte(dstp++, (byte) '?');
            break;
        }
      }
    }
  }

  @Override
  public void moveFrom(CobolDataStorage dataStrage) {}

  @Override
  public void moveFrom(byte[] bytes) {}

  @Override
  public void moveFrom(String string) {}

  @Override
  public void moveFrom(int number) {}

  @Override
  public void moveFrom(double number) {}

  @Override
  public void moveFrom(BigDecimal number) {}
}
