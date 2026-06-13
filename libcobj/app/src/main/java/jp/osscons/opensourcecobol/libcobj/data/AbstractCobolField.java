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

/** COBOLで使用する変数を表現するクラス。 */
public abstract class AbstractCobolField {
    /** データを格納に使用するバイト配列の長さ */
    protected int size;

    /** データを格納するバイト配列を扱うオブジェクト */
    protected CobolDataStorage dataStorage;

    /** 変数に関する様々な情報を保持するオブジェクト(符号付か,COMP-3指定かなど) */
    protected CobolFieldAttribute attribute;

    static int lastsize = 0;
    static CobolDataStorage lastdata = null;

    /** 文字データの符号化に使用するShift_JIS文字セット */
    public static Charset charSetSJIS = Charset.forName("SHIFT-JIS");

    static final int[] cobExp10 = {
        1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000
    };

    /**
     * コンストラクタ
     *
     * @param size データを格納するバイト配列の長さ
     * @param dataStorage データを格納するバイト配列を扱うオブジェクト
     * @param attribute 変数に関する様々な情報を保持するオブジェクト(符号付か,COMP-3指定かなど)
     */
    public AbstractCobolField(
            int size, CobolDataStorage dataStorage, CobolFieldAttribute attribute) {
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

    /**
     * メンバ変数dataStorageのsetter
     *
     * @param dataStorage 設定するデータを格納するバイト配列を扱うオブジェクト
     */
    public void setDataStorage(CobolDataStorage dataStorage) {
        this.dataStorage = dataStorage;
    }

    /**
     * メンバ変数attributeのsetter
     *
     * @param attribute 設定する変数に関する様々な情報を保持するオブジェクト(符号付か,COMP-3指定かなど)
     */
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
     * @param size 設定するデータを格納するバイト配列の長さ
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
     * 符号部分を除いた数値部分の長さを返す
     *
     * @return 符号付で符号が分離している場合はthis.size-1,そうでなければthis.size
     */
    public int getFieldSize() {
        return this.size - (this.attribute.isFlagSignSeparate() ? 1 : 0);
    }

    /**
     * バイト配列の中で(符号データではなく)数値データの格納されている最小の添え字を返す
     *
     * @return SIGN LEADINGかつSIGN SEPARATEな変数なら1,それ以外は0
     */
    public int getFirstDataIndex() {
        return (this.attribute.isFlagSignSeparate() && this.attribute.isFlagSignLeading()) ? 1 : 0;
    }

    /**
     * thisの保持する数値データを4バイトの2進数(符号付き)に変換し,そのバイト配列を返す
     *
     * @return thisの保持する数値データを4バイトの2進数に変換したバイト配列
     */
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
     * thisの文字列表現をかえす.
     *
     * @return thisの文字列表現
     */
    public abstract String getString();

    /**
     * thisの保持する数値データを4バイトの2進数に変換し,int値として返す
     *
     * @return thisの保持する数値データをintに変換した値
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
     * thisの保持するデータをdouble型の値に変換して返す
     *
     * @return thisの文字列表現をdoubleに変換した値.変換できない場合は0
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

        if (Byte.toUnsignedInt(data.getByte(firstDataIndex)) == 0xFF) {
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
     * 数値を表すデータが実装すべきメソッド. 引数で与えられた値をthisの保持する数値データに設定する.
     *
     * @param decimal thisに設定する数値
     */
    public abstract void setDecimal(BigDecimal decimal);

    /**
     * thisの保持する数値データをint型で返す
     *
     * @param size 変換に使用するバイトデータの桁数
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
     * thisの保持する数値データに,引数で与えられたフィールドの保持する数値データを加算する
     *
     * @param field 加算する数値を保持するフィールド
     * @param opt 桁あふれ時の丸めや切り捨てなど,結果をthisに格納する際の動作を指定するオプション
     * @return 加算後のthisの保持する数値データ
     * @throws CobolStopRunException 実行を停止する状況が発生した場合にスローされる
     */
    public int add(AbstractCobolField field, int opt) throws CobolStopRunException {
        CobolDecimal d1 = this.getDecimal();
        CobolDecimal d2 = field.getDecimal();
        d1.add(d2);
        return d1.getField(this, opt);
    }

    /**
     * thisの保持する数値データに,引数で与えられたフィールドの保持する数値データを減算する
     *
     * @param field 減算する数値を保持するフィールド
     * @param opt 桁あふれ時の丸めや切り捨てなど,結果をthisに格納する際の動作を指定するオプション
     * @return 減算後のthisの保持する数値データ
     * @throws CobolStopRunException 実行を停止する状況が発生した場合にスローされる
     */
    public int sub(AbstractCobolField field, int opt) throws CobolStopRunException {
        CobolDecimal d1 = this.getDecimal();
        CobolDecimal d2 = field.getDecimal();
        d1.sub(d2);
        return d1.getField(this, opt);
    }

    /**
     * 保持する数値データに指定された値を加算する
     *
     * @param n thisの保持する数値データに加算する数値
     * @return 基本的に0が返される
     * @throws CobolStopRunException 実行を停止する状況が発生した場合にスローされる
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
     * thisの保持するパック10進数(COMP-3)形式の数値データに指定された整数値を加算する
     *
     * @param n thisの保持する数値データに加算する整数値
     * @return 基本的に0が返される
     */
    public abstract int addPackedInt(int n);

    /**
     * thisの保持する数値データに指定された値を減算する
     *
     * @param n thisの保持する数値データから減算する数値
     * @return 基本的に0が返される
     * @throws CobolStopRunException 実行を停止する状況が発生した場合にスローされる
     */
    public int subInt(int n) throws CobolStopRunException {
        return n == 0 ? 0 : this.addInt(-n);
    }

    /**
     * thisを被除数として除算を行い,商をquotientに格納する.剰余は後続のdivRemainderで取得できるよう内部に保持される.
     *
     * @param divisor 除数を保持するフィールド
     * @param quotient 商を格納するフィールド
     * @param opt 桁あふれ時の丸めや切り捨てなど,結果を格納する際の動作を指定するオプション
     * @return 基本的に0が返される
     * @throws CobolStopRunException 実行を停止する状況が発生した場合にスローされる
     */
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
     * 直前のdivQuotientで保持された剰余をthisに格納する
     *
     * @param opt 桁あふれ時の丸めや切り捨てなど,結果をthisに格納する際の動作を指定するオプション
     * @return 基本的に0が返される
     * @throws CobolStopRunException 実行を停止する状況が発生した場合にスローされる
     */
    public int divRemainder(int opt) throws CobolStopRunException {
        return CobolDecimal.cobD3.getField(this, opt);
    }

    /**
     * 整数値との比較を行う
     * @deprecated 代わりにcmpIntegerを使用してください
     * @param n 比較対象の整数値
     * @return 保持する数値データの比較を行い,this&lt;nなら負の値,this==nなら0,this&gt;nなら正の値
     */
    public int cmpInt(int n) {
        CobolDecimal d1 = this.getDecimal();
        CobolDecimal d2 = new CobolDecimal(n);
        d2.setScale(0);
        return d1.compareTo(d2);
    }

    /**
     * 整数値との比較を行う
     * @deprecated 代わりにcmpIntegerを使用してください
     * @param n 比較対象の整数値
     * @return 保持する数値データの比較を行い,this&lt;nなら負の値,this==nなら0,this&gt;nなら正の値
     */
    public int cmpInt(long n) {
        return this.cmpInt((int) n);
    }

    /**
     * 整数値との比較を行う
     * @param n 比較対象の整数値
     * @return 保持する数値データの比較を行い,this&lt;nなら負の値,this==nなら0,this&gt;nなら正の値
     */
    public int cmpInteger(long n) {
        CobolDecimal d1 = this.getDecimal();
        CobolDecimal d2 = new CobolDecimal(n);
        d2.setScale(0);
        return d1.compareTo(d2);
    }

    /**
     * 整数値との比較を行う
     * @param n 比較対象の整数値
     * @return 保持する数値データの比較を行い,this&lt;nなら負の値,this==nなら0,this&gt;nなら正の値
     */
    public int cmpInteger(int n) {
        return this.cmpInteger((long) n);
    }

    /**
     * 整数値との比較を行う
     * @deprecated 代わりにcmpIntegerを使用してください
     * @param n 比較対象の整数値
     * @return 保持する数値データの比較を行い,this&lt;nなら負の値,this==nなら0,this&gt;nなら正の値
     */
    public int cmpUint(int n) {
        return this.cmpInt(n);
    }

    /**
     * 整数値との比較を行う
     * @deprecated 代わりにcmpIntegerを使用してください
     * @param n 比較対象の整数値
     * @return 保持する数値データの比較を行い,this&lt;nなら負の値,this==nなら0,this&gt;nなら正の値
     */
    public int cmpUint(long n) {
        return this.cmpUint((int) n);
    }

    /**
     * thisと引数で与えられたフィールドの保持する数値データを数値として比較する
     *
     * @param field thisと比較するフィールド
     * @return 保持する数値データの比較を行い,this&lt;fieldなら負の値,this==fieldなら0,this&gt;fieldなら正の値
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

    /**
     * 代入処理(moveFrom)の前処理を行う. 全体集団項目(ALL)の代入や,英数字から日本語項目への変換などを処理する.
     *
     * @param src 代入元のフィールド
     * @return 後続の代入処理を行うべき代入元フィールド.前処理内で代入が完結した場合や代入する必要がない場合はnull
     */
    protected AbstractCobolField preprocessOfMoving(AbstractCobolField src) {
        AbstractCobolField src1 = src;

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
            if ((srcAttr.isTypeNumeric()
                            || srcAttr.isTypeAlphanum()
                            || srcAttr.isTypeAlphanumEdited())
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
                    return src1;
                }
            }
        }

        if (src1.getSize() == 0) {
            return src1;
        }

        return src;
    }

    /**
     * 全体集団項目(ALL定数)からthisへの代入を行う. 代入元のデータをthisのサイズいっぱいに繰り返し展開してから代入する.
     *
     * @param src 代入元のフィールド
     */
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
            tmpSrcStorage = pTmp;
            tmpSrcSize = size;
            xToN = true;
        }

        if (xToN) {
            attr = new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_NATIONAL, 0, 0, 0, null);
        } else {
            attr =
                    new CobolFieldAttribute(
                            CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null);
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
     * @param field 代入元のデータ
     */
    public abstract void moveFrom(AbstractCobolField field);

    /**
     * 引数で与えらえられたデータからthisへの代入を行う
     *
     * @param dataStorage 代入元のデータ
     */
    public abstract void moveFrom(CobolDataStorage dataStorage);

    /**
     * 引数で与えらえられたデータからthisへの代入を行う
     *
     * @param bytes 代入元のデータ
     */
    public abstract void moveFrom(byte[] bytes);

    /**
     * 引数で与えらえられたデータからthisへの代入を行う
     *
     * @param s 代入元のデータ
     */
    public void moveFrom(String s) {
        // The maximum number of digits of int type in decimal is 10

        byte[] bytes = s.getBytes(charSetSJIS);

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
     * @param number 代入元のデータ
     */
    public void moveFrom(int number) {
        // The maximum number of digits of int type in decimal is 10
        final int length = 10;

        CobolDataStorage storage = new CobolDataStorage(length);
        String formattedNumberString = String.format("%10d", Math.abs(number));
        storage.memcpy(formattedNumberString, length);
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
     * @param number 代入元のデータ
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
     * @param number 代入元のデータ
     */
    public abstract void moveFrom(BigDecimal number);

    /**
     * thisの保持するデータが数値でない場合に実行時エラーを発生させ,実行を停止する
     *
     * @param s エラーメッセージに表示する変数名(Shift_JISでエンコードされたバイト配列)
     * @throws CobolStopRunException thisの保持するデータが数値でない場合にスローされる
     */
    public void checkNumeric(byte[] s) throws CobolStopRunException {
        if (!this.isNumeric()) {
            byte[] buff = this.getDataStorage().getByteArrayRef(0, this.getSize());
            String name = new String(s, charSetSJIS);
            String content = new String(buff, charSetSJIS);
            CobolUtil.runtimeError("'" + name + "' not numeric: '" + content + "'");
            CobolStopRunException.stopRunAndThrow(1);
        }
    }

    // TODO abstract指定
    /**
     * thisと引数で与えられたデータとの数値比較を行う
     *
     * @param other thisと比較するfield
     * @return 保持する数値データの比較を行い,this&lt;fieldなら負の値,this==fieldなら0,this&gt;fieldなら正の値
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
                    AbstractCobolField tmpField =
                            CobolFieldFactory.makeCobolField(tmpSize, tmpBuff, tmpAttr);
                    tmpField.moveFrom(f1);
                    f1 = tmpField;
                } else if (attr1.isFlagSignSeparate()) {
                    int tmpSize = attr1.getDigits();
                    CobolDataStorage tmpBuff = new CobolDataStorage(48);
                    CobolFieldAttribute tmpAttr = new CobolFieldAttribute(attr1);
                    tmpAttr.setType(CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY);
                    tmpAttr.setFlags(CobolFieldAttribute.COB_FLAG_HAVE_SIGN);
                    AbstractCobolField tmpField =
                            CobolFieldFactory.makeCobolField(tmpSize, tmpBuff, tmpAttr);
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
                    AbstractCobolField tmpField =
                            CobolFieldFactory.makeCobolField(tmpSize, tmpBuff, tmpAttr);
                    tmpField.moveFrom(f2);
                    f2 = tmpField;
                } else if (attr2.isFlagSignSeparate()) {
                    int tmpSize = attr2.getDigits();
                    CobolDataStorage tmpBuff = new CobolDataStorage(48);
                    CobolFieldAttribute tmpAttr = new CobolFieldAttribute(attr2);
                    tmpAttr.setType(CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY);
                    tmpAttr.setFlags(CobolFieldAttribute.COB_FLAG_HAVE_SIGN);
                    AbstractCobolField tmpField =
                            CobolFieldFactory.makeCobolField(tmpSize, tmpBuff, tmpAttr);
                    tmpField.moveFrom(f2);
                    f2 = tmpField;
                }
            }
        }
        return f1.cmpAlnum(f2);
    }

    /**
     * thisのデータを,引数で与えられたフィールドのデータを繰り返し並べたものと比較する
     *
     * @param field thisと比較するフィールド
     * @return 比較を行い,this&lt;fieldなら負の値,this==fieldなら0,this&gt;fieldなら正の値
     */
    protected int compareAll(AbstractCobolField field) {
        int size = this.getSize();
        CobolDataStorage data = this.getDataStorage();
        int sign = this.getSign();
        int ret = 0;
        int p = 0;
        outer:
        {
            while (size >= field.getSize()) {
                // TODO moduleを参照するコードにする
                ret = alnumCmps(data, p, field.getDataStorage(), 0, this.getSize(), null);
                if (ret != 0) {
                    break outer;
                }
                size -= field.getSize();
                p += field.getSize();
            }
            if (size > 0) {
                // TODO moduleを参照するコードにする
                ret = alnumCmps(data, 0, field.getDataStorage(), 0, this.getSize(), null);
            }
        }
        this.putSign(sign);
        return ret;
    }

    /**
     * 2つのバイトデータを英数字として1バイトずつ比較する
     *
     * @param s1 比較対象の一方のバイトデータ
     * @param s1Start s1のバイトデータにアクセスるするときの最初の添え字の相対位置
     * @param s2 比較対象のもう一方のバイトデータ
     * @param s2Start s2のバイトデータにアクセスるするときの最初の添え字の相対位置
     * @param size 比較するバイト数
     * @param col 文字の照合順序(コレーティングシーケンス).nullの場合はバイト値で比較する
     * @return 比較を行い,s1&lt;s2なら負の値,s1==s2なら0,s1&gt;s2なら正の値
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
                ret = s1.getByte(i + s1Start) - s2.getByte(i + s2Start);
                if (ret != 0) {
                    return ret;
                }
            }
        }
        return 0;
    }

    /**
     * thisと同じサイズ・データ・属性を持つCobolNumericField型のフィールドを生成して返す
     *
     * @return thisからCobolNumericField型へ変換した値
     */
    public CobolNumericField getNumericField() {
        return new CobolNumericField(this.getSize(), this.getDataStorage(), this.getAttribute());
    }

    /**
     * thisが英数字項目で,かつ引数のfieldが数値項目の場合に,thisの保持するデータがすべて数字であるか検査する
     *
     * @param field 代入元など対象となる数値項目のフィールド
     * @throws CobolStopRunException thisに数字以外のバイトが含まれている場合にスローされる
     */
    public void checkMoveStrNum(AbstractCobolField field) throws CobolStopRunException {
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
                                CobolUtil.runtimeError("Numeric value is expected");
                                CobolStopRunException.stopRunAndThrow(1);
                            }
                        }
                        break;
                    default:
                        break;
                }
                break;
            default:
                break;
        }
    }

    /**
     * s2のバイトデータをs1へ指定したバイト数だけコピーする
     *
     * @param s1 コピー先のバイトデータ
     * @param s1StartIndex s1のバイトデータにアクセスるするときの最初の添え字の相対位置
     * @param s2 コピー元のバイトデータ
     * @param s2StartIndex s2のバイトデータにアクセスるするときの最初の添え字の相対位置
     * @param size コピーするバイト数
     */
    protected void ownByteMemcpy(
            CobolDataStorage s1,
            int s1StartIndex,
            CobolDataStorage s2,
            int s2StartIndex,
            int size) {
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
     * thisの保持するデータを,末尾の空白および0バイトを取り除いた文字列にして返す
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

    /**
     * 引数で与えられたint値をthisの保持する数値データに設定する
     *
     * @param n thisに設定するint値
     */
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

    /**
     * 引数で与えられたバイトデータをint値として読み取り,thisの保持する数値データに設定する
     *
     * @param data thisに設定する値を保持するバイトデータ
     */
    public void setInt(CobolDataStorage data) {
        this.setInt((int) data.intValue());
    }

    /**
     * 引数で与えられたlong値をthisの保持する数値データに設定する
     *
     * @param n thisに設定するlong値
     */
    public void setLong(Long n) {
        CobolFieldAttribute attr =
                new CobolFieldAttribute(
                        CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY,
                        9,
                        0,
                        CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
                        null);
        CobolDataStorage data = new CobolDataStorage(ByteBuffer.allocate(8).putLong(n).array());
        AbstractCobolField temp = CobolFieldFactory.makeCobolField(8, data, attr);
        this.moveFrom(temp);
    }

    /**
     * 引数で与えられたバイト配列を英数字データとしてthisへ代入する
     *
     * @param src 代入元のバイト配列
     * @param size 代入元として使用するバイト数
     */
    public void memcpy(byte[] src, int size) {
        CobolFieldAttribute attr =
                new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null);
        AbstractCobolField temp =
                CobolFieldFactory.makeCobolField(size, new CobolDataStorage(src), attr);
        this.moveFrom(temp);
    }

    /**
     * 引数で与えられたバイト配列全体を英数字データとしてthisへ代入する
     *
     * @param src 代入元のバイト配列
     */
    public void memcpy(byte[] src) {
        this.memcpy(src, src.length);
    }

    /**
     * 引数で与えられた文字列をShift_JISでエンコードし,英数字データとしてthisへ代入する
     *
     * @param src 代入元の文字列
     * @param size 代入元として使用するバイト数
     */
    public void memcpy(String src, int size) {
        byte[] bytes = src.getBytes(AbstractCobolField.charSetSJIS);
        this.memcpy(bytes, size);
    }

    /**
     * 引数で与えられた文字列全体をShift_JISでエンコードし,英数字データとしてthisへ代入する
     *
     * @param src 代入元の文字列
     */
    public void memcpy(String src) {
        this.memcpy(src.getBytes(AbstractCobolField.charSetSJIS));
    }

    /**
     * thisが省略された引数(OMITTED)であるかどうかを返す
     *
     * @return データを格納するバイト配列が存在しない(省略されている)場合はtrue,そうでなければfalse
     */
    public boolean isOmitted() {
        return this.dataStorage == null;
    }

    /**
     * thisの保持するデータが数値として有効であるかどうかを判定する(CLASS NUMERIC判定)
     *
     * @return 保持するデータが数値として有効であればtrue,そうでなければfalse
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
                if (this.getAttribute().isFlagHaveSign()) {
                    if (sign == 0x0c || sign == 0x0d) {
                        return true;
                    }
                } else if (CobolUtil.nibbleCForUnsigned) {
                    if (sign == 0x0c) {
                        return true;
                    }
                }
                return false;
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
     * thisの保持するデータが英字および空白のみで構成されているかどうかを判定する(CLASS ALPHABETIC判定)
     *
     * @return 保持するデータが英字または空白のみであればtrue,そうでなければfalse
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
     * thisの保持するデータが大文字および空白のみで構成されているかどうかを判定する(CLASS ALPHABETIC-UPPER判定)
     *
     * @return 保持するデータが大文字または空白のみであればtrue,そうでなければfalse
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
     * thisの保持するデータが小文字および空白のみで構成されているかどうかを判定する(CLASS ALPHABETIC-LOWER判定)
     *
     * @return 保持するデータが小文字または空白のみであればtrue,そうでなければfalse
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
     * thisの保持するデータを,指定した1文字で全体を埋めたデータと比較する
     *
     * @param c 比較対象の文字(thisのサイズ分だけ繰り返されるとみなす)
     * @return 比較を行い,this&lt;cの並びなら負の値,等しければ0,this&gt;cの並びなら正の値
     */
    public int cmpChar(byte c) {
        int sign = this.getSign();
        int ret = AbstractCobolField.commonCmpc(this.getDataStorage(), c, this.getSize());
        if (this.getAttribute().getType() != CobolFieldAttribute.COB_TYPE_NUMERIC_PACKED) {
            this.putSign(sign);
        }
        return ret;
    }

    private interface DataComparator {
        int compare(CobolDataStorage s1, CobolDataStorage s2, int size, CobolDataStorage col);
    }

    private static DataComparator getComparator(AbstractCobolField f) {
        if (f.getAttribute().isTypeNational()) {
            return new DataComparator() {
                @Override
                public int compare(
                        CobolDataStorage s1, CobolDataStorage s2, int size, CobolDataStorage col) {
                    return CobolUtil.nationalCmps(s1, s2, size, col);
                }
            };
        } else {
            return new DataComparator() {
                @Override
                public int compare(
                        CobolDataStorage s1, CobolDataStorage s2, int size, CobolDataStorage col) {
                    return CobolUtil.alnumCmps(s1, s2, size, col);
                }
            };
        }
    }

    /**
     * thisのデータをotherのデータに対して繰り返し並べ,照合順序を考慮して比較する(ALL定数の比較)
     *
     * @param other thisと比較するフィールド
     * @return 比較を行い,this&lt;otherなら負の値,this==otherなら0,this&gt;otherなら正の値
     */
    public int cmpAll(AbstractCobolField other) {
        int ret = 0;
        DataComparator comparator = getComparator(this);
        int sign = 0;

        if ((this.getAttribute().getType() == CobolFieldAttribute.COB_TYPE_ALPHANUMERIC_ALL
                        || this.getAttribute().getType()
                                == CobolFieldAttribute.COB_TYPE_NATIONAL_ALL)
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
                    ret = comparator.compare(this.getDataStorage(), data, size, s);
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
                    ret = comparator.compare(data, other.getDataStorage(), size, s);
                }
            } while (false);
        }

        if (this.getAttribute().getType() != CobolFieldAttribute.COB_TYPE_NUMERIC_PACKED) {
            this.putSign(sign);
        }
        return ret;
    }

    /**
     * thisとotherを文字列として比較する. サイズが異なる場合は短い方を空白(日本語項目の場合は全角空白)で埋めて比較する.
     *
     * @param other thisと比較するフィールド
     * @return 比較を行い,this&lt;otherなら負の値,this==otherなら0,this&gt;otherなら正の値
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
        int ret =
                comparator.compare(this.getDataStorage(), other.getDataStorage(), sf.getSize(), s);
        if (ret == 0) {
            if (lf.getSize() > sf.getSize()) {
                if ((lf.getAttribute().getType() & CobolFieldAttribute.COB_TYPE_NATIONAL) != 0) {
                    int cmpResult =
                            AbstractCobolField.isNationalPadding(
                                    sf.getSize(), lf.getDataStorage(), lf.getSize() - sf.getSize());
                    return cmpResult == 0 ? 1 : 0;
                } else {
                    ret =
                            AbstractCobolField.commonCmpc(
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
     * thisとotherを英数字として比較する. 数値表示項目(DISPLAY)の場合は符号を一時的に正にしてから比較し,比較後に符号を元に戻す.
     *
     * @param other thisと比較するフィールド
     * @return 比較を行い,this&lt;otherなら負の値,this==otherなら0,this&gt;otherなら正の値
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
     * 符号の有無にかかわらず,thisの保持する数値データの符号を実際に取得する. 必要に応じて符号桁を数字に戻す処理も行う.
     *
     * @return 数値データが正(または符号なし)なら1,負なら-1.数値型でない場合は0
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
                        return AbstractCobolField.getSignEbcdic(p);
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
     * 符号の有無にかかわらず,thisの保持する数値データに符号を実際に設定する
     *
     * @param sign 正符号を設定するときは0以上の値,負符号を設定するときは負の値
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
                    AbstractCobolField.putSignEbcdic(p, sign);
                } else if (sign < 0) {
                    p.setByte(0, (byte) (b + 0x40));
                }
                return;
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
     * thisの保持する数値データを8バイトの2進数に変換し,long値として返す
     *
     * @return thisの保持する数値データをlongに変換した値
     */
    public long getLong() {
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

    /**
     * thisの保持する数値データをlong値として返す. 基底クラスでは0を返し,各サブクラスでオーバーライドされる.
     *
     * @return thisの保持する数値データをlongに変換した値.基底クラスでは0
     */
    public long getLongValue() {
        return 0;
    }

    /**
     * 半角文字への変換を伴う代入を行う(暫定実装として通常の代入を行う)
     *
     * @param src 代入元のフィールド
     */
    public void hankakuMoveFrom(AbstractCobolField src) {
        // TODO 暫定実装
        this.moveFrom(src);
    }

    // libcob/common.cのcob_get_sign_ebcdicの実装
    /**
     * EBCDIC形式で符号が重ねられた符号桁から符号を取得し,その桁を対応する数字に戻す
     *
     * @param p 符号桁を先頭に持つバイトデータ
     * @return 符号が正なら1,負なら-1
     */
    private static int getSignEbcdic(CobolDataStorage p) {
        switch (p.getByte(0)) {
            case '{':
                p.setByte(0, (byte) '0');
                return 1;
            case 'A':
                p.setByte(0, (byte) '1');
                return 1;
            case 'B':
                p.setByte(0, (byte) '2');
                return 1;
            case 'C':
                p.setByte(0, (byte) '3');
                return 1;
            case 'D':
                p.setByte(0, (byte) '4');
                return 1;
            case 'E':
                p.setByte(0, (byte) '5');
                return 1;
            case 'F':
                p.setByte(0, (byte) '6');
                return 1;
            case 'G':
                p.setByte(0, (byte) '7');
                return 1;
            case 'H':
                p.setByte(0, (byte) '8');
                return 1;
            case 'I':
                p.setByte(0, (byte) '9');
                return 1;
            case '}':
                p.setByte(0, (byte) '0');
                return -1;
            case 'J':
                p.setByte(0, (byte) '1');
                return -1;
            case 'K':
                p.setByte(0, (byte) '2');
                return -1;
            case 'L':
                p.setByte(0, (byte) '3');
                return -1;
            case 'M':
                p.setByte(0, (byte) '4');
                return -1;
            case 'N':
                p.setByte(0, (byte) '5');
                return -1;
            case 'O':
                p.setByte(0, (byte) '6');
                return -1;
            case 'P':
                p.setByte(0, (byte) '7');
                return -1;
            case 'Q':
                p.setByte(0, (byte) '8');
                return -1;
            case 'R':
                p.setByte(0, (byte) '9');
                return -1;
            default:
                /* What to do here */
                p.setByte(0, (byte) '0');
                return 1;
        }
    }

    // libcob/common.cのcob_put_sign_ebcdicの実装
    /**
     * 符号桁の数字に対し,指定された符号をEBCDIC形式で重ねて設定する
     *
     * @param p 符号桁を先頭に持つバイトデータ
     * @param sign 負符号を設定するときは負の値,正符号を設定するときはそれ以外の値
     */
    private static void putSignEbcdic(CobolDataStorage p, int sign) {
        if (sign < 0) {
            switch (p.getByte(0)) {
                case '0':
                    p.setByte(0, (byte) '}');
                    return;
                case '1':
                    p.setByte(0, (byte) 'J');
                    return;
                case '2':
                    p.setByte(0, (byte) 'K');
                    return;
                case '3':
                    p.setByte(0, (byte) 'L');
                    return;
                case '4':
                    p.setByte(0, (byte) 'M');
                    return;
                case '5':
                    p.setByte(0, (byte) 'N');
                    return;
                case '6':
                    p.setByte(0, (byte) 'O');
                    return;
                case '7':
                    p.setByte(0, (byte) 'P');
                    return;
                case '8':
                    p.setByte(0, (byte) 'Q');
                    return;
                case '9':
                    p.setByte(0, (byte) 'R');
                    return;
                default:
                    /* What to do here */
                    p.setByte(0, (byte) '}');
                    return;
            }
        }
        switch (p.getByte(0)) {
            case '0':
                p.setByte(0, (byte) '{');
                return;
            case '1':
                p.setByte(0, (byte) 'A');
                return;
            case '2':
                p.setByte(0, (byte) 'B');
                return;
            case '3':
                p.setByte(0, (byte) 'C');
                return;
            case '4':
                p.setByte(0, (byte) 'D');
                return;
            case '5':
                p.setByte(0, (byte) 'E');
                return;
            case '6':
                p.setByte(0, (byte) 'F');
                return;
            case '7':
                p.setByte(0, (byte) 'G');
                return;
            case '8':
                p.setByte(0, (byte) 'H');
                return;
            case '9':
                p.setByte(0, (byte) 'I');
                return;
            default:
                /* What to do here */
                p.setByte(0, (byte) '{');
                return;
        }
    }

    // libcob/common.cのcommon_compcの実装
    /**
     * バイトデータを,指定した1文字で全体を埋めたデータと,照合順序を考慮して比較する
     *
     * @param s1 比較対象のバイトデータ
     * @param c 比較対象の文字(size分だけ繰り返されるとみなす)
     * @param size 比較するバイト数
     * @return 比較を行い,s1&lt;cの並びなら負の値,等しければ0,s1&gt;cの並びなら正の値
     */
    private static int commonCmpc(CobolDataStorage s1, byte c, int size) {
        CobolDataStorage s = CobolModule.getCurrentModule().collating_sequence;
        int uc = c & 0xFF;
        if (s != null) {
            for (int i = 0; i < size; ++i) {
                // int ret = s.getByte((s1.getByte(i) & 0xFF) - (s.getByte(uc) & 0xFF));
                int ret = (s.getByte(s1.getByte(i) & 0xFF) & 0xFF) - (s.getByte(uc) & 0xFF);
                if (ret != 0) {
                    return ret;
                }
            }
        } else {
            for (int i = 0; i < size; ++i) {
                int ret = (s1.getByte(i) & 0xFF) - uc;
                if (ret != 0) {
                    return ret;
                }
            }
        }
        return 0;
    }

    // libcob/common.cのis_national_paddingの実装
    /**
     * バイトデータの指定範囲が,日本語項目の余白(半角空白または全角空白)のみで構成されているかどうかを判定する
     *
     * @param offset 判定を開始するバイトデータの添え字
     * @param s 判定対象のバイトデータ
     * @param size 判定するバイト数
     * @return 余白(半角空白または全角空白)のみで構成されていれば1,そうでなければ0
     */
    private static int isNationalPadding(int offset, CobolDataStorage s, int size) {
        int ret = 1;
        int i = 0;
        while (i < size && ret != 0) {
            if (s.getByte(offset + i) == ' ') {
                i++;
            } else if (size - i >= CobolConstant.ZENCSIZ) {
                for (int j = 0; j < CobolConstant.ZENCSIZ; ++j) {
                    if (s.getByte(offset + i + j) != CobolConstant.ZENSPC[j]) {
                        return 0;
                    }
                }
                i += CobolConstant.ZENCSIZ;
            } else {
                ret = 0;
            }
        }
        return ret;
    }
}
