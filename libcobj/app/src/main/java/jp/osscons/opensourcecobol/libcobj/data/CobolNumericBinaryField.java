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
import jp.osscons.opensourcecobol.libcobj.common.CobolConstant;
import jp.osscons.opensourcecobol.libcobj.common.CobolModule;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolRuntimeException;

/** TODO: 準備中 */
public class CobolNumericBinaryField extends AbstractCobolField {

    /**
     * コンストラクタ
     *
     * @param size データを格納するバイト配列の長さ
     * @param dataStorage データを格納するバイト配列を扱うオブジェクト
     * @param attribute 変数に関する様々な情報を保持するオブジェクト
     */
    public CobolNumericBinaryField(
            int size, CobolDataStorage dataStorage, CobolFieldAttribute attribute) {
        super(size, dataStorage, attribute);
    }

    @Override
    public byte[] getBytes() {
        return new byte[0];
    }

    @Override
    public int addPackedInt(int n) {
        throw new CobolRuntimeException(0, "実装しないコード");
    }

    private long getBinaryValue() {
        CobolDataStorage storage = this.getDataStorage();
        if (this.size == 1) {
            return storage.getByte(0);
        } else if (this.size == 2) {
            return storage.shortValue();
        } else if (this.size == 4) {
            return storage.intValue();
        } else {
            return storage.longValue();
        }
    }

    private void setBinaryValue(long n) {
        CobolDataStorage storage = this.getDataStorage();
        if (this.size == 1) {
            storage.setByte(0, (byte) n);
        } else if (this.size == 2) {
            storage.set((short) n);
        } else if (this.size == 4) {
            storage.set((int) n);
        } else {
            storage.set((long) n);
        }
    }

    @Override
    public long getLongValue() {
        return this.getBinaryValue();
    }

    /**
     * TODO: 準備中
     *
     * @param n TODO: 準備中
     */
    public void setLongValue(long n) {
        this.setBinaryValue(n);
    }

    @Override
    public String getString() {
        CobolFieldAttribute thisAttr = this.getAttribute();
        int flag = thisAttr.isFlagHaveSign() ? CobolFieldAttribute.COB_FLAG_HAVE_SIGN : 0;
        CobolFieldAttribute attr =
                new CobolFieldAttribute(
                        CobolFieldAttribute.COB_TYPE_NUMERIC,
                        thisAttr.getDigits(),
                        thisAttr.getScale(),
                        flag,
                        thisAttr.getPic());
        CobolDataStorage storage = new CobolDataStorage(thisAttr.getDigits());
        CobolNumericField numericField = new CobolNumericField(thisAttr.getDigits(), storage, attr);
        numericField.moveFrom(this);
        return numericField.getString();
    }

    @Override
    public int getSign() {
        long n = this.getBinaryValue();

        if (n < 0) {
            return -1;
        } else if (n > 0) {
            return 1;
        } else {
            return 0;
        }
    }

    @Override
    public void setDecimal(BigDecimal decimal) {}

    @Override
    public void moveFrom(AbstractCobolField src) {
        AbstractCobolField src1 = this.preprocessOfMoving(src);
        if (src1 == null) {
            return;
        }

        switch (src1.getAttribute().getType()) {
            case CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY:
                this.moveDisplayToBinary(src1);
                break;
            case CobolFieldAttribute.COB_TYPE_NUMERIC_PACKED:
            case CobolFieldAttribute.COB_TYPE_ALPHANUMERIC:
            case CobolFieldAttribute.COB_TYPE_ALPHANUMERIC_EDITED:
            case CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY:
            case CobolFieldAttribute.COB_TYPE_NUMERIC_DOUBLE:
            case CobolFieldAttribute.COB_TYPE_NUMERIC_FLOAT:
            case CobolFieldAttribute.COB_TYPE_NATIONAL:
            case CobolFieldAttribute.COB_TYPE_NUMERIC_EDITED:
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
     * CobolNumericFieldからからthisへの代入
     *
     * @param field 代入元のデータ(AbstractCobolField型)
     */
    private void moveDisplayToBinary(AbstractCobolField field) {
        int size1 = field.getFieldSize();
        int data1Index = field.getFirstDataIndex();
        int sign = field.getSign();

        CobolDataStorage data1 = field.getDataStorage();
        CobolDataStorage data2 = this.getDataStorage();

        boolean flagNull = true;
        for (int i = 0; i < size1; ++i) {
            if (data1.getByte(data1Index + i) != 0) {
                flagNull = false;
            }
        }
        if (flagNull) {
            for (int i = 0; i < this.getSize(); ++i) {
                data2.setByte(i, (byte) 0);
            }
        }

        int size = size1 - field.getAttribute().getScale() + this.getAttribute().getScale();
        long val = 0;
        for (int i = 0; i < size; ++i) {
            if (i < size1) {
                byte x = data1.getByte(data1Index + i);
                x = (byte) (x >= (byte) 0x70 ? x - (byte) 0x70 : x - (byte) 0x30);
                val = val * 10 + x;
            } else {
                val = val * 10;
            }
        }

        if (sign < 0 && this.getAttribute().isFlagHaveSign()) {
            val = -val;
        }

        if (CobolModule.getCurrentModule().flag_binary_truncate != 0
                && !this.getAttribute().isFlagRealBinary()) {
            val %= CobolConstant.exp10LL[this.getAttribute().getDigits()];
        }

        this.setBinaryValue(val);
        field.putSign(sign);
    }

    @Override
    public void moveFrom(CobolDataStorage dataStorage) {}

    @Override
    public void moveFrom(byte[] bytes) {}

    @Override
    public void moveFrom(String string) {}

    @Override
    public void moveFrom(int number) {
        this.getDataStorage().setSwpU32Binary(number);
    }

    @Override
    public void moveFrom(double number) {}

    @Override
    public void moveFrom(BigDecimal number) {}

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

    @Override
    public CobolDecimal getDecimal() {
        CobolDecimal decimal = new CobolDecimal();
        decimal.setValue(new BigDecimal(this.getBinaryValue()));
        decimal.setScale(this.getAttribute().getScale());
        return decimal;
    }

    // libcob/numeric.cのcob_binary_get_int64の実装
    private long binaryGetInt64() {
        int fsiz = 8 - this.getSize();
        long n = 0;
        byte[] nBytes = ByteBuffer.allocate(8).putLong(n).array();
        CobolDataStorage nStorage = new CobolDataStorage(nBytes);
        if (this.getAttribute().isFlagHaveSign()) {
            CobolDecimal.numByteMemcpy(nStorage, 0, this.getDataStorage(), 0, this.getSize());
            n = ByteBuffer.wrap(nStorage.getByteArray(0, 8)).getLong();
            n >>>= 8 * fsiz;
        } else {
            CobolDecimal.numByteMemcpy(nStorage, fsiz, this.getDataStorage(), 0, this.getSize());
            n = ByteBuffer.wrap(nStorage.getByteArray(0, 8)).getLong();
        }
        return n;
    }

    @Override
    public long getLong() {
        return this.binaryGetInt64();
    }
}
