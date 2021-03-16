/*
 * Copyright (C) 2020 TOKYO SYSTEM HOUSE Co., Ltd.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, 51 Franklin Street, Fifth Floor
 * Boston, MA 02110-1301 USA
 */

package jp.osscons.opensourcecobol.libcobj.data;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.ByteBuffer;

import jp.osscons.opensourcecobol.libcobj.common.CobolModule;
import jp.osscons.opensourcecobol.libcobj.common.CobolUtil;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolException;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolExceptionId;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolRuntimeException;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolStopRunException;

/**
 * BigDecimalを扱うクラス
 * COMPUTE等で計算をするときに使用する
 */
public class CobolDecimal {
	final static public int DECIMAL_NAN = -128;
	final static public int COB_STORE_ROUND = 0x01;
	final static public int COB_STORE_KEEP_ON_OVERFLOW = 0x02;
	final static public int COB_STORE_TRUNC_ON_OVERFLOW = 0x04;

	final static public int COB_MAX_BINARY = 36;

	private static byte[] numBuffPtr = new byte[2048];

	static private BigDecimal cobMexp = new BigDecimal(0);
	static private BigDecimal cobMpzt = new BigDecimal(0);
	public static CobolDecimal cobD1 = new CobolDecimal();
	public static CobolDecimal cobD2 = new CobolDecimal();
	public static CobolDecimal cobD3 = new CobolDecimal();
	public static CobolDecimal cobD4 = new CobolDecimal();
	public static BigDecimal[] cobMpze10 = new BigDecimal[COB_MAX_BINARY];
	public static byte[] packedValue = new byte[20];

	public static void cobInitNumeric() {
		cobD1 = new CobolDecimal();
		cobD2 = new CobolDecimal();
		cobD3 = new CobolDecimal();
		cobD4 = new CobolDecimal();
		cobMexp = new BigDecimal(0);
		cobMpzt = new BigDecimal(0);
		for(int i=0; i<COB_MAX_BINARY; ++i) {
			cobMpze10[i] = BigDecimal.ZERO;
			cobMpze10[i] = BigDecimal.TEN.pow(i);
		}
		numBuffPtr = new byte[2048];
		for(int i=0; i<packedValue.length; ++i) {
			packedValue[i] = 0;
		}
	}

	//TODO cob_init_numeric周辺の初期化処理を正しく実装出来たら消す。
	static {
		for(int i=0; i<COB_MAX_BINARY; ++i) {
			cobMpze10[i] = BigDecimal.ZERO;
			cobMpze10[i] = BigDecimal.TEN.pow(i);
		}
		for(int i=0; i<packedValue.length; ++i) {
			packedValue[i] = 0;
		}
	 }

	/**
	 * 保持する数値データ
	 */
	public BigDecimal value;
	public int scale;

	/**
	 * コンストラクタ
	 * this.valueは0に設定する
	 */
	public CobolDecimal() {
		this.value = BigDecimal.ZERO;
		this.setScale(0);
	}

	/**
	 * コンストラクタ
	 * this.valueを引数で指定された値に設定する.
	 * @param value
	 */
	public CobolDecimal(BigDecimal value) {
		this.setValue(value);
		this.setScale(0);
	}

	/**
	 * コンストラクタ
	 * this.valueを指定された値に設定し,scaleも指定された値に設定する.
	 * @param value
	 * @param scale
	 */
	public CobolDecimal(BigDecimal value, int scale) {
		this(value);
		this.setScale(scale);
	}

	/**
	 * コンストラクタ.
	 * this.valueを指定されたint型の値に対応する値に設定する
	 * @param n
	 */
	public CobolDecimal(long n) {
		this(new BigDecimal(n));
		this.setScale(0);
	}
	/**
	 * コンストラクタ.
	 * this.valueを指定されたint型の値に対応する値に設定する
	 * @param n
	 */
	public CobolDecimal(int n) {
		this(new BigDecimal(n));
		this.setScale(0);
	}

	/**
	 * this.valueのgetter
	 * @return this.value
	 */
	public BigDecimal getValue() {
		return value;
	}

	/**
	 * libcob/numeric.cのcob_decimal_initの実装
	 */
	public void decimalInit() {
		this.value = BigDecimal.ZERO;
		this.value.setScale(0);
	}

	/**
	 * this.valueのsetter
	 * @param value this.valueに設定する値
	 */
	public void setValue(BigDecimal value) {
		this.value = value;
	}

	/**
	 * this.valueの値を0にする
	 */
	public void clear() {
		this.value = BigDecimal.ZERO;
	}

	/**
	 * this.valueのスケールを設定する
	 * @param scale スケール値
	 */
	public void setScale(int scale) {
		this.value.setScale(scale, RoundingMode.DOWN);
		this.scale = scale;
	}

	/**
	 * this.valueのスケールを取得する
	 * @return this.valueのスケール
	 */
	public int getScale() {
		//return this.value.scale();
		return this.scale;
	}

	/**
	 * this.valueを引数で指定されたint型変数の値に対応する値に設定する
	 * @param n this.valueに設定する値
	 */
	public void set(int n) {
		this.value = new BigDecimal(n);
		this.scale = 0;
	}

	/**
	 * this.valueを引数で指定されたint型変数の値に対応する値に設定する
	 * @param n this.valueに設定する値
	 */
	public void set(long n) {
		this.value = new BigDecimal(n);
		this.scale = 0;
	}

	/**
	 * libcob/numeric.cのcob_decimal_setの実装
	 */
	public void set(CobolDecimal decimal) {
		//TODO よりよいコピーの方法を考える
		this.value = decimal.value.add(BigDecimal.ZERO);
		this.setScale(decimal.getScale());
	}

	/**
	 * libcob/numeric.cのcob_decimal_set_fieldの実装
	 * @param f
	 */
	public void setField(AbstractCobolField f) {
		CobolDecimal decimal = f.getDecimal();
		this.setValue(decimal.getValue());
		this.setScale(decimal.getScale());
	}

	/**
	 *
	 * libcob/numeric.cのcob_decimal_set_displayの実装
	 * @param f
	 */
	/*public void decimalSetDisplay(AbstractCobolField f) {
		int firstIndex = f.getFirstDataIndex();
		int p = 0;
		int size = f.getFieldSize();

		CobolDataStorage data = f.getDataStorage();
		if(data.getByte(firstIndex + p) == 255) {
			this.value = BigDecimal.TEN.pow(size);
			this.setScale(f.getAttribute().getScale());
			return;
		}
		if(data.getByte(firstIndex + p) == 0) {
			this.value = BigDecimal.TEN.pow(size).negate();
			this.setScale(f.getAttribute().getScale());
			return;
		}
		int sign = f.getSign();
		// skip leading zeros
		while(size > 1 && data.getByte(p) == '0') {
			size--;
			p++;
		}

		// set value
		if(size < 10) {
			int n = 10;
			while(size-- != 0) {
				n = n * 10 + data.getByte(firstIndex + p++) - '0';
			}
			this.set(n);
		} else {
			byte[] numBuffPtr = new byte[size];
			for(int i=0; i<size; ++i) {
				numBuffPtr[i] = data.getByte(firstIndex + i);
			}
			this.value = new BigDecimal(new String(numBuffPtr));
		}

		if(sign < 0) {
			this.value = this.value.negate();
		}
		this.setScale(f.getAttribute().getScale());
		f.putSign(sign);
	}*/

	/**
	 * libcob/numeric.cのDECIMAL_CHECKマクロの代替
	 * @param d1
	 * @param d2
	 */
	private static boolean DECIMAL_CHECK(CobolDecimal d1, CobolDecimal d2) {
		if(d1.getScale() == DECIMAL_NAN || d2.getScale() == DECIMAL_NAN) {
			d1.setScale(DECIMAL_NAN);
			return true;
		}
		return false;
	}

	private void decimalAdd(CobolDecimal d1, CobolDecimal d2) {
	}

	/**
	 * this.valueの値をthis.valueとnを加算した値にする
	 * @param n other this.valueに加算される値
	 */
	public void add(int n) {
		this.value = this.value.add(new BigDecimal(n));
	}

	/**
	 * this.valueの値をthis.valueとotherを加算した値にする
	 * @param other this.valueに加算される値
	 */
	public void add(CobolDecimal decimal) {
		//this.value = this.value.add(other.getValue());
		if(DECIMAL_CHECK(this, decimal)) {
			return;
		}
		alignDecimal(this, decimal);
		this.setValue(this.getValue().add(decimal.getValue()));
	}

	/**
	 * this.valueの値をthis.valueからotherを減算した値にする
	 * @param other this.valueから減算される値
	 */
	public void sub(CobolDecimal decimal) {
		//this.value = this.value.subtract(other.getValue());
		if(DECIMAL_CHECK(this, decimal)) {
			return;
		}
		alignDecimal(this, decimal);
		this.setValue(this.getValue().subtract(decimal.getValue()));
	}

	/**
	 * this.valueの値をthis.valueからnを減算した値にする
	 * @param n this.valueから減算される値
	 */
	public void sub(int n) {
		this.value = this.value.subtract(new BigDecimal(n));
	}

	/**
	 * this.valueの値をthis.valueとotherを乗算した値にする
	 * @param other this.valueに乗算される値
	 */
	public void mul(CobolDecimal decimal) {
		//this.value = this.value.multiply(other.getValue());
		if(DECIMAL_CHECK(this, decimal)) {
			return;
		}
		this.setScale(this.getScale() + decimal.getScale());
		this.setValue(this.getValue().multiply(decimal.getValue()));
	}

	/**
	 * this.valueの値をthis.valueとnを乗算した値にする
	 * @param n this.valueに乗算される値
	 */
	public void mul(int n) {
		this.value = this.value.multiply(new BigDecimal(n));
	}

	/**
	 * this.valueの値をthis.valueでotherを割った値にする
	 * @param other this.valueを割る数
	 */
	public void div(CobolDecimal decimal) throws CobolStopRunException {
		if(DECIMAL_CHECK(this, decimal)) {
			return;
		}
		//System.out.println("decimal: " + decimal.getValue());
		if (decimal.getValue().signum() == 0) {
			this.setScale(DECIMAL_NAN);
			if(CobolUtil.cobErrorOnExitFlag) {
				//TODO より正確な実装に変更
				System.err.println("Detected division by zero");
				CobolStopRunException.throwException(1);
			}
			return;
		}
		if(this.getValue().signum() == 0) {
			this.setScale(0);
			return;
		}
		this.setScale(this.getScale() - decimal.getScale());
		int shift = 37 + ((this.getScale() < 0) ? -this.getScale() : 0);
		this.shiftDecimal(shift);
		this.setValue(this.getValue().divide(decimal.getValue(), RoundingMode.DOWN));
		//System.out.println(">>> value: " + this.value + ", scale: " + this.scale);
	}

	/**
	 * this.valueの値をthis.valueでnを割った値にする
	 * @param n this.valueを割る数
	 */
	public void div(int n) {
		this.value = this.value.divide(new BigDecimal(n), RoundingMode.DOWN);
	}

		/**
	 * this.valueの値をthis.valueをdecimal乗した値にする
	 * @param other this.valueを割る数
	 */
	//TODO 残りの実装
	public void pow(CobolDecimal decimal) {
		if(DECIMAL_CHECK(this, decimal)) {
			return;
		}

		if(decimal.getScale() == 0 && decimal.getValue().compareTo(new BigDecimal(2147483647)) <= 0) {
			int n = decimal.getValue().intValue();
			this.value = this.value.pow(n);
			this.setScale(this.getScale() * n);
		} else {
			this.decimalSetDouble(Math.pow(this.decimalGetDouble(), decimal.decimalGetDouble()));
		}
	}

	/**
	 * libcob/numeric.cのcob_decimal_set_doubleの実装
	 * @param v
	 */
	private void decimalSetDouble(double v) {
		this.setValue(new BigDecimal(v * 1.0e9));
		this.setScale(9);
	}

	private double decimalGetDouble() {
		double v = this.getValue().doubleValue();
		int n = this.getScale();
		for(; n>0; n--) {
			v /= 10;
		}
		for(; n<0; n++) {
			v *= 10;
		}
		return v;
	}

	/**
	 * this.valueの値をthis.valueをn乗した値にする
	 * @param other this.valueを割る数
	 */

	/**
	 * libcob/numeric.c cob_decimal_get_fieldの実装
	 * AbstractCobolFieldの保持する値をthisに設定する
	 * @param f
	 * @param opt
	 * @return
	 */
	public int getField(AbstractCobolField f, int opt) throws CobolStopRunException {
		if(this.getScale() == CobolDecimal.DECIMAL_NAN) {
			//throw new CobolRuntimeException(0, "getFieldのエラー");
			CobolException.setException(CobolExceptionId.COB_EC_SIZE_OVERFLOW);
			return CobolException.code;
		}

		/* rounding */
		if((opt & CobolDecimal.COB_STORE_ROUND) > 0) {
			if(f.getAttribute().getScale() < this.getScale()) {
				int sign = this.value.signum();
				if(sign != 0) {
					this.shiftDecimal(f.getAttribute().getScale() - this.getScale() + 1);
					if(sign > 0) {
						this.add(5);
					} else {
						this.sub(5);
					}
				}
			}
		}

		this.shiftDecimal(f.getAttribute().getScale() - this.getScale());
		//TODO 残りのパターンも実装
		switch(f.getAttribute().getType()) {
		case CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY:
			return this.getDisplayField(f, opt);
		case CobolFieldAttribute.COB_TYPE_NUMERIC_PACKED:
			return this.getPackedField(f, opt);
		case CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY:
			return this.getBinaryField(f, opt);
		case CobolFieldAttribute.COB_TYPE_NUMERIC_FLOAT:
			System.out.println("getField: Float not implemented");
			throw new CobolRuntimeException(0, "getField: Float not implemented");
		case CobolFieldAttribute.COB_TYPE_NUMERIC_DOUBLE:
			System.out.println("getField: Double not implemented");
			throw new CobolRuntimeException(0, "getField: Double not implemented");
		default:
			int digits = f.getAttribute().getDigits();
			CobolFieldAttribute attr = f.getAttribute();
			CobolFieldAttribute newAttr = new CobolFieldAttribute(
				CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY,
				digits,
				attr.getScale(),
				CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
				null);
			AbstractCobolField displayField = CobolFieldFactory.makeCobolField(digits, new CobolDataStorage(digits), newAttr);
			if(this.getField(displayField, opt) == 0) {
				f.moveFrom(displayField);
			}
			return CobolException.code;
		}
	}

	/**
	 * libcob/numeric.c shift_decimalの実装
	 * this.valueを値10^n,スケールをn増加させる
	 * @param n
	 */
	public void shiftDecimal(int n) {
		if(n == 0) {
			return;
		}
		if(n > 0) {
			cobMexp = BigDecimal.TEN.pow(n);
			this.value = this.value.multiply(cobMexp);
		} else {
			cobMexp = BigDecimal.TEN.pow(-n);
			this.value = this.value.divide(cobMexp, RoundingMode.DOWN);
		}
		this.setScale(this.getScale() + n);
	}

	/**
	 * libcob/numeric.cのalign_decimalの実装
	 */
	public void  alignDecimal(CobolDecimal d1, CobolDecimal d2) {
		if(d1.getScale() < d2.getScale()) {
			this.shiftDecimal(d2.getScale() - d1.getScale());
		} else if(d1.getScale() < d2.getScale()) {
			d2.shiftDecimal(d1.getScale() - d2.getScale());
		}
	}

	/**
	 * libcob/numeric.cのcob_decimal_cmpの実装
	 * 引数で与えられたCobolDecimal型のインスタンスとの比較をする.
	 * @param decimal thisと比較する対象
	 * @return thisのほうが大きいときは正の値,thisのほうが小さいときあ負の値,それ以外は0
	 */
	public int compareTo(CobolDecimal decimal) {
		alignDecimal(this, decimal);
		return this.value.compareTo(decimal.getValue());
	}

	/**
	 * libcob/numeric.cのcob_decimal_get_displayの実装
	 * @param f
	 * @param opt
	 * @return
	 */
	public int getDisplayField(AbstractCobolField f, int opt) throws CobolStopRunException {
		int sign = this.value.signum();
		this.value = this.value.abs();
		byte[] numBuffPtr = this.value.toPlainString().getBytes();
		int size = numBuffPtr.length;

		CobolDataStorage data = f.getDataStorage();
		int firstDataIndex = f.getFirstDataIndex();
		int diff = f.getFieldSize() - size;
		if(diff < 0) {
			CobolRuntimeException.setException(CobolExceptionId.COB_EC_SIZE_OVERFLOW);
			if((opt & CobolDecimal.COB_STORE_KEEP_ON_OVERFLOW) > 0) {
				return CobolException.code;
			}
			for(int i=0; i<f.getFieldSize(); ++i) {
				data.setByte(i, numBuffPtr[i - diff]);
			}
		} else {
			for(int i=0; i<diff; ++i) {
				data.setByte(firstDataIndex + i, (byte)'0');
			}
			for(int i=0; i<size; ++i) {
				data.setByte(firstDataIndex + i + diff,  numBuffPtr[i]);
			}
		}
		f.putSign(sign);
		return 0;
	}

	/**
	 * libcob/numeric.cのcob_decimal_get_packedの実装
	 * @param f
	 * @param opt
	 * @return
	 */
	public int getPackedField(AbstractCobolField f, int opt ) {
		int sign = this.value.signum();
		this.value = this.value.abs();
		String numBuffPtr = this.value.toPlainString();
		int size = numBuffPtr.length();

		CobolDataStorage data = f.getDataStorage();
		int digits = f.getAttribute().getDigits();
		int q = 0;
		int diff = digits - size;
		if(diff < 0) {
			//TODO  実装
			throw new CobolRuntimeException(0, "未実装のエラー");
		}
		data.fillBytes(0, f.getSize());
		int p = (digits / 2) - (size / 2);
		diff = 1 - (size % 2);
		for(int i=diff, n=0; i<size+diff; i++, n++) {
			byte x = (byte)(numBuffPtr.charAt(q + n) - '0');
			if(i % 2 == 0) {
				data.setByte(p, (byte)(x << 4));
			} else {
				data.setByte(p, (byte)(data.getByte(p) | x));
				p++;
			}
		}

		p = f.getSize() - 1;
		byte x = data.getByte(p);
		if(!f.getAttribute().isFlagHaveSign()) {
			data.setByte(p, (byte) ((x & 0xF0) | 0x0F));
		} else if(sign < 0) {
			data.setByte(p, (byte) ((x & 0xF0) | 0x0D));
		} else {
			data.setByte(p, (byte) ((x & 0xF0) | 0x0C));
		}

		return 0;
	}

	class OverflowException extends Exception {
	}
	/**
	 * libcob/numeric.cのcob_decimal_get_binaryの実装
	 * @param f
	 * @param opt
	 * @return
	 */
	private int getBinaryField(AbstractCobolField f, int opt) {
		CobolDataStorage data = f.getDataStorage();
		CobolFieldAttribute attr = f.getAttribute();
		if(this.getValue().signum() == 0) {
			data.fillBytes(0, f.getSize());
			return 0;
		}
		int overflow = 0;
		int digits = attr.getDigits();
		int sign;
		if(attr.isFlagHaveSign()) {
			sign = 1;
		} else {
			sign = 0;
			if(this.value.signum() < 0) {
				this.value = this.value.abs();
			}
		}
		int bitnum = f.getSize() * 8 - sign;
		try {
			if(this.getValue().compareTo(new BigDecimal(2).pow(bitnum).subtract(BigDecimal.ONE)) > 0) {
				if((opt & COB_STORE_KEEP_ON_OVERFLOW) != 0) {
					throw new OverflowException();
				}
				overflow = 1;
				if((opt & COB_STORE_TRUNC_ON_OVERFLOW) != 0) {
					this.setValue(this.getValue().divide(cobMpze10[digits],  RoundingMode.DOWN));
				} else {
					this.setValue(this.getValue().divide(new BigDecimal(2).pow(f.getSize()*8), RoundingMode.DOWN));
				}
			} else if((opt != 0) && CobolModule.getCurrentModule().flag_binary_truncate !=0) {
				if(this.getValue().abs().compareTo(cobMpze10[digits].abs()) >= 0) {
					if((opt & COB_STORE_KEEP_ON_OVERFLOW) != 0) {
						throw new OverflowException();
					}
					overflow = 1;
					if((opt & COB_STORE_TRUNC_ON_OVERFLOW) != 0) {
						this.setValue(this.getValue().divide(cobMpze10[digits], RoundingMode.DOWN));
					} else {
						this.setValue(this.getValue().divide(new BigDecimal(2).pow(f.getFieldSize() * 8), RoundingMode.DOWN));
					}
				}
			}
			if(sign == 0 || overflow != 0) {
				this.binarySetUint64(f, this.getValue().longValue());
			} else {
				this.binarySetInt64(f, this.getValue().longValue());
			}
			if(overflow == 0) {
				return 0;
			}
		} catch (OverflowException e) {

		} finally {
			// TODO 例外処理
			return 0;
		}
	}

	/**
	 * libcob/numeric.cのcob_binary_set_uint64の実装
	 * @param f
	 * @param n
	 */
	private void binarySetUint64(AbstractCobolField f, long n) {
		byte[] nBytes = ByteBuffer.allocate(8).putLong(n).array();
		CobolDataStorage nStorage = new CobolDataStorage(nBytes);
		this.numByteMemcpy(f.getDataStorage(), 0, nStorage, 8 - f.getSize(), f.getSize());
	}

	/**
	 * libcob/numeric.cのcob_binary_set_int64の実装
	 * @param f
	 * @param n
	 */
	private void binarySetInt64(AbstractCobolField f, long n) {
		byte[] nBytes = ByteBuffer.allocate(8).putLong(n).array();
		CobolDataStorage nStorage = new CobolDataStorage(nBytes);
		this.numByteMemcpy(f.getDataStorage(), 0, nStorage, 8 - f.getSize(), f.getSize());
	}

	/**
	 * libcob/numeric.cのnum_byte_memcpyの実装
	 * @param s1
	 * @param s1StartIndex s1のコピー開始位置
	 * @param s2
	 * @param s2StartIndex s1のコピー開始位置
	 * @param size
	 */
	public static void numByteMemcpy(CobolDataStorage s1, int s1StartIndex, CobolDataStorage s2, int s2StartIndex, int size) {
		int i1 = s1StartIndex;
		int i2 = s2StartIndex;
		do {
			s1.setByte(i1++, s2.getByte(i2++));
		} while(--size != 0);
	}

	/*
		CobolDataStorage data = f.getDataStorage();
		int firstDataIndex = f.getFirstDataIndex();
		int size = f.getFieldSize();

		if(data.getByte(firstDataIndex) == 255) {
			this.value = BigDecimal.TEN.pow(size);
			this.setScale(f.getAttribute().getScale());
			return;
		}

		if(data.getByte(firstDataIndex) == 255) {
			this.value = BigDecimal.TEN.pow(size);
			this.value = this.value.negate();
			this.setScale(f.getAttribute().getScale());
			return;
		}

		int sign = f.getSign();
		int i = 0;
		while(size > 1 && data.getByte(firstDataIndex + i) == '0') {
			size--;
			i++;
		}
	 */
}
