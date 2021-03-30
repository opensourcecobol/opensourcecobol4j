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

import java.io.UnsupportedEncodingException;
import java.math.BigDecimal;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.charset.StandardCharsets;

import jp.osscons.opensourcecobol.libcobj.common.CobolConstant;
import jp.osscons.opensourcecobol.libcobj.common.CobolModule;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolRuntimeException;

/**
 * PIC 文字列が9(5)や9(9)の変数を表現するクラス.
 */
public class CobolNumericField extends AbstractCobolField {

	/**
	 * コンストラクタ
	 * @param size データを格納するバイト配列の長さ
	 * @param dataStorage データを格納するバイト配列を扱うオブジェクト
	 * @param attribute 変数に関する様々な情報を保持するオブジェクト
	 */
	public CobolNumericField(int size, CobolDataStorage dataStorage, CobolFieldAttribute attribute) {
		super(size, dataStorage, attribute);
	}

	/**
	 * TODO実装
	 */
	public void checkNumeric(String s) {
		//TODO 実装
	}

	/**
	 * this.dataの保持するバイト配列のコピーを返す
	 */
	@Override
	public byte[] getBytes() {
		return dataStorage.getData();
	}

	//削除予定

	private char removeSign(byte v) {
		return (char) (v >= 0x70 ? v - 0x40 : v);
	}

	/**
	 * thisの文字列表現をかえす.(toStringだけで十分か?)
	 * @return thisの文字列表現
	 */
	@Override
	public String getString() {
		CobolDataStorage data = this.getDataStorage();
		CobolFieldAttribute attr = this.getAttribute();
		int scale = attr.getScale();
		int pointIndex = scale > 0 ? attr.getDigits() - scale - 1 : attr.getDigits() - 1;
		StringBuilder sb = new StringBuilder();
		if(attr.isFlagHaveSign()) {
			if(this.getSign() < 0) {
				sb.append('-');
			} else {
				sb.append('+');
			}
		}
		
		int signIndex = attr.isFlagSignLeading() ? 0 : this.getSize() - 1;
		int i=0;
		
		for(; i + getFirstDataIndex()<this.getSize(); ++i) {
			if(scale > 0 && i - 1 == pointIndex) {
				sb.append('.');
			}
			char c = (char)data.getByte(this.getFirstDataIndex() + i);
			if(attr.isFlagHaveSign() && !attr.isFlagSignSeparate() && i == signIndex && c >= 0x70) {
				c -= 0x40;
			}
			sb.append(c);
		}
		
		for(;i < attr.getDigits(); ++i) {
			if(scale > 0 && i - 1 == pointIndex) {
				sb.append('.');
			}
			sb.append('0');
		}
		
		return sb.toString();
	}

	/**
	 * thisの保持する数値データをint型で返す
	 * @return thisの保持する数値データをintに変換した値
	 */
	@Override
	public int getInt() {
		int size = this.getFieldSize();
		CobolDataStorage data = this.getDataStorage();
		int firstDataIndex = this.getFirstDataIndex();
		int sign = this.getSign();

		int i;
		for(i=0; i< size; ++i) {
			if(data.getByte(firstDataIndex + i) - 0x30 != 0) {
				break;
			}
		}

		int val = 0;
		int scale = this.getAttribute().getScale();
		if(scale < 0) {
			for(; i<size; ++i) {
				byte x = data.getByte(firstDataIndex + i);
				x -= (x >= 0x70) ? 0x70 : 0x30;
				val = val * 10 +  x;
			}
			val *= AbstractCobolField.cobExp10[-scale];
		} else {
			size -= scale;
			for(; i<size; ++i) {
				byte x = data.getByte(firstDataIndex + i);
				x -= (x >= 0x70) ? 0x70 : 0x30;
				val = val * 10 + x;
			}
		}

		if(sign < 0) {
			val = -val;
		}

		this.putSign(sign);
		return val;
	}

	/**
	 * TODO
	 */
	@Override
	public double getDouble() {
		throw new CobolRuntimeException(CobolRuntimeException.COBOL_FITAL_ERROR, "未対応");
	}

	@Override
	public void setDecimal(BigDecimal decimal) {
		byte[] decimalBytes = decimal.toPlainString().getBytes();
		int length = Math.min(this.size, decimalBytes.length);

		//末尾からコピー
		for(int i=0; i<length; ++i) {
			this.dataStorage.setByte(this.size - 1 - i, decimalBytes[decimalBytes.length - 1 - i]);
		}
	}

	/**
	 * 引数で与えらえられたデータからthisへの代入を行う
	 * @param field 代入元のデータ(AbstractCobolField型)
	 */
	@Override
	public void moveFrom(AbstractCobolField src) {
		AbstractCobolField src1 = this.preprocessOfMoving(src);
		if(src1 == null) {
			return;
		}

		switch(src1.getAttribute().getType()) {
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
	 * @param field 代入元のデータ(AbstractCobolField型)
	 */
	private void moveDisplayToDisplay(AbstractCobolField field) {
		int sign = field.getSign();
		field.putSign(1);

		this.storeCommonRegion(this, field.getDataStorage().getSubDataStorage(field.getFirstDataIndex()), field.getFieldSize(), field.getAttribute().getScale());

		field.putSign(sign);
		this.putSign(sign);
	}

	/**
	 * CobolNumericPackedFieldからthisへの代入を行う
	 * @param field 代入元のデータ(AbstractCobolField型)
	 */
	private void movePackedToDisplay(AbstractCobolField field) {
		int sign = field.getSign();
		int offset = 1 - (field.getAttribute().getDigits() % 2);
		CobolDataStorage buff = new CobolDataStorage(64);

		for(int i=offset; i< field.getAttribute().getDigits() + offset; ++i) {
			if(i % 2 == 0) {
				buff.setByte(i - offset, (byte) (((field.getDataStorage().getByte(i / 2) >> 4) & 0x0f) + 0x30));
			} else {
				buff.setByte(i -offset, (byte)((field.getDataStorage().getByte(i / 2) & 0x0f) + 0x30));
			}
		}
		this.storeCommonRegion(this, buff, field.getAttribute().getDigits(), field.getAttribute().getScale());

		this.putSign(sign);
	}

	/**
	 * CobolAlphanumericFieldからthisへの代入を行う
	 * @param field 代入元のデータ(AbstractCobolField型)
	 */
	private void moveAlphanumericToDisplay(AbstractCobolField field) {
		int s1 = 0;
		int e1 = s1 + field.getSize();
		int s2 = this.getFirstDataIndex();
		int e2 = this.getFieldSize();

		for(int i=0; i<this.getSize(); ++i) {
			this.getDataStorage().setByte(i, (byte)0x30);
		}

		/* skip white space */
		for(; s1 < e1; ++s1) {
			char ch = (char) field.getDataStorage().getByte(s1);
			if(!Character.isWhitespace(ch)) {
				break;
			}
		}

		/* check for sign */
		int sign = 0;
		if(s1 != e1) {
			byte ch = field.getDataStorage().getByte(s1);
			byte plus = 0x2b; byte minus = 0x2d;
			if(ch == plus || ch == minus) {
				sign = (ch == plus) ?  1 : -1;
				s1++;
			}
		}

		/* count the number of digits before decimal point */
		int count = 0;
		//TODO Moduleの情報を参照するコードに編集する
		for(int p=s1; p<e1 && field.getDataStorage().getByte(p) != '.'; ++p) {
			if(Character.isDigit(field.getDataStorage().getByte(p))) {
				++count;
			}
		}

		/* find the start position */
		int size = this.getFieldSize() - this.getAttribute().getScale();
		if(count < size) {
			s2 += size - count;
		} else {
			while(count-- > size) {
				while(!Character.isDigit(field.getDataStorage().getByte(s1++))) {
					;
				}
			}
		}

		/* move */
		count = 0;
		try {
			for(; s1 < e1 && s2 < e2; ++s1) {
				byte c = field.getDataStorage().getByte(s1);
				if(Character.isDigit(c)) {
					this.getDataStorage().setByte(s2++, c);
				//TODO Moduleの情報を参照するコードに編集する
				} else if (c == '.') {
					if(count++ > 0) {
						throw new GotoException();
					}

				//TODO Moduleの情報を参照するコードに編集する
				} else if (!Character.isWhitespace(c) || c == ','){
					throw new GotoException();
				}
			}
			this.putSign(sign);
			return;

		} catch(GotoException e) {
			for(int i=0; i<this.getSize(); ++i) {
				this.getDataStorage().setByte(i, (byte)0x30);
			}
			this.putSign(0);
		}
	}

	/**
	 * CobolNumericBinaryからthisへの代入を行う
	 * @param field 代入元のデータ(AbstractCobolField型)
	 */
	private void moveBinaryToDisplay(AbstractCobolField field) {
		int sign = 1;
		long val = field.getLongValue();

		if(this.getAttribute().isFlagHaveSign() && val < 0) {
			sign = -1;
			val = -val;
		}

		int i = 20;
		byte[] buff = new byte[64];
		while(val > 0) {
			buff[--i] = (byte) (val % 10 + 0x30);
			val /= 10;
		}

		this.storeCommonRegion(this, new CobolDataStorage(buff, 0), i, (20 - i), field.getAttribute().getScale());
		this.putSign(sign);
	}
	
	private void moveEditedToDisplay(AbstractCobolField field) {
		byte[] buff = new byte[64];
		int p = 0;
		boolean havePoint = false;
		int scale = 0;
		int sign = 0;
		
		for(int i=0; i<field.getSize(); ++i) {
			int cp = field.getDataStorage().getByte(i);
			switch(cp) {
			case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
				buff[p++] = (byte)cp;
				if(havePoint) {
					++scale;
				}
				break;
			case '.': case ',':
				if(cp == CobolModule.getCurrentModule().decimal_point) {
					havePoint = true;
				}
				break;
			case '-': case 'C':
				sign = -1;
				break;
			}
		}
		
		byte[] picBytes = field.getAttribute().getPic().getBytes();
		int count = 0;
		if(scale == 0) {
			for(int p1 = 0; p1 < picBytes.length; p1 += 5) {
				byte c = picBytes[p1];
				ByteBuffer buf = ByteBuffer.wrap(picBytes, p1 + 1, 4);
				buf.order(ByteOrder.LITTLE_ENDIAN);
				int n = buf.getInt();
				if(c == '9' || c == '0' || c == 'Z' || c == '*') {
					if(havePoint) {
						scale += n;
					} else {
						count += n;
					}
				} else if(c == 'P') {
					if(count == 0) {
						havePoint = true;
						scale += n;
					} else {
						scale -= n;
					}
				} else if(c == 'V') {
					havePoint = true;
				}
			}
		}
		
		storeCommonRegion(this, new CobolDataStorage(buff), p, scale);
		this.putSign(sign);
	}

	/**
	 * libcob/move.cのcob_binary_mget_int64の実装
	 * @param field
	 */
	private long binaryMgetInt64(AbstractCobolField field) {
		long n = 0;
		int fsiz = 8 - field.getSize();

		//TODO Javaの内部エンディアンの調査
		//JavaはBIG ENDIANのためopensource COBOLにある条件分岐は削除した
		byte bytes[] = new byte[8];
		for(int i=0; i<8; ++i) {
			bytes[i] = 0;
		}
		CobolDataStorage nData = new CobolDataStorage(bytes, 0);

		if(field.getAttribute().isFlagHaveSign()) {
			this.ownByteMemcpy(nData, 0, field.getDataStorage(), 0, field.getSize());
			n = this.dataStorageToLong(nData);
			n = (n >> (8 * fsiz));
		} else {
			this.ownByteMemcpy(nData, fsiz, field.getDataStorage(), 0, field.getSize());
			n = this.dataStorageToLong(nData);
		}

		return n;
	}

	/**
	 * CobolDataStorageに格納された値をlongに変換する
	 * @param storage 変換元データ
	 * @return lっ変換後のlong型の値
	 */
	private long dataStorageToLong(CobolDataStorage storage) {
		long n = 0;
		for(int i=0; i<8; ++i) {
			long x = storage.getByte(i);

			if(x == 0) {
				n <<= 8;
			} else if(x < 0) {
				n = (n << 8) | (0xFFL - (~x));
			} else {
				n = (n << 8) | x;
			}
		}

		return n;
	}

	/**
	 * thisの保持する数値データの符号を返す
	 * @return thisの保持する数値データが負ならば負数,0なら0,正なら正数を返す
	 */
	@Override
	public int getSign() {
		CobolFieldAttribute attr = this.getAttribute();
		if(!attr.isFlagHaveSign()) {
			return 0;
		}

		int p;
		if(attr.isFlagSignLeading()) {
			p = 0;
		} else {
			p = this.getSize() - 1;
		}
		byte value = this.getDataStorage().getByte(p);
		if(attr.isFlagSignSeparate()) {
			//0x2b == '+'
			return value == 0x2b ? 1 : -1;
		} else {
			if(0x30 <= value && value <= 0x39) {
				return 1;
			}
			if(value == 0x20) {
				this.getDataStorage().setByte(p, (byte)0x30);
				return 1;
			}

			//TODO 以下のコメントアウトを外して,他の部分も修正してテストする
			//if(CobolModule.getCurrentModule().display_sign != 0) {
				//return 1;
				//return getSignEbcdic(p);
			//} else {
				//return -1;
			//}

			//this.getDataStorage().setByte(p, (byte) (this.getDataStorage().getByte(p) - 0x40));
			return -1;
		}
	}

	private int getSignEbcdic(int p) {
		CobolDataStorage data = this.getDataStorage();
		switch(data.getByte(p)) {
		case '{': data.setByte(p, (byte)'0'); return 1;
		case 'A': data.setByte(p, (byte)'1'); return 1;
		case 'B': data.setByte(p, (byte)'2'); return 1;
		case 'C': data.setByte(p, (byte)'3'); return 1;
		case 'D': data.setByte(p, (byte)'4'); return 1;
		case 'E': data.setByte(p, (byte)'5'); return 1;
		case 'F': data.setByte(p, (byte)'6'); return 1;
		case 'G': data.setByte(p, (byte)'7'); return 1;
		case 'H': data.setByte(p, (byte)'8'); return 1;
		case 'I': data.setByte(p, (byte)'9'); return 1;
		case '}': data.setByte(p, (byte)'0'); return 1;
		case 'J': data.setByte(p, (byte)'1'); return 1;
		case 'K': data.setByte(p, (byte)'2'); return 1;
		case 'L': data.setByte(p, (byte)'3'); return 1;
		case 'M': data.setByte(p, (byte)'4'); return 1;
		case 'N': data.setByte(p, (byte)'5'); return 1;
		case 'O': data.setByte(p, (byte)'6'); return 1;
		case 'P': data.setByte(p, (byte)'7'); return 1;
		case 'Q': data.setByte(p, (byte)'8'); return 1;
		case 'R': data.setByte(p, (byte)'9'); return 1;
		default: data.setByte(p, (byte)'0'); return 1;
		}
	}

	/**
	 * thisの保持する数値データの符号を設定する
	 * @param sign 正符号を設定するときは正数,負符号を設定するときは負数,それ以外は0
	 */

	@Override
	public void putSign(int sign) {
		CobolFieldAttribute attr = this.getAttribute();
		int p;

		if(!attr.isFlagHaveSign()) {
			return;
		}

		if(attr.isFlagSignLeading()) {
			p = 0;
		} else {
			p = this.getSize() - 1;
		}

		byte value = this.getDataStorage().getByte(p);
		if(attr.isFlagSignSeparate()) {
			//0x2dは'-', 0x2bは'+'
			byte c = (byte) ((sign < 0) ? 0x2d : 0x2b);
			if(value != c) {
				this.getDataStorage().setByte(p, c);
			}
		//TODO 以下のコメントを外して他の部分も修正してテストする
		} //else if (CobolModule.getCurrentModule().display_sign != 0) {
			///this.putSignEbcdic(p, sign);
		//}

		else {
			value = (byte) (value >= 0x70 ? value - 0x40 : value);
			this.getDataStorage().setByte(p, (byte) (sign < 0 ? value + 0x40 : value));
		}
	}

	private void putSignEbcdic(int p, int sign) {
		CobolDataStorage data = this.getDataStorage();
		if(sign < 0) {
			switch(data.getByte(p)) {
			case '0': data.setByte(p, (byte)'}'); return;
			case '1': data.setByte(p, (byte)'J'); return;
			case '2': data.setByte(p, (byte)'K'); return;
			case '3': data.setByte(p, (byte)'L'); return;
			case '4': data.setByte(p, (byte)'M'); return;
			case '5': data.setByte(p, (byte)'N'); return;
			case '6': data.setByte(p, (byte)'O'); return;
			case '7': data.setByte(p, (byte)'P'); return;
			case '8': data.setByte(p, (byte)'Q'); return;
			case '9': data.setByte(p, (byte)'R'); return;
			default: data.setByte(p, (byte)'}'); return;
			}
		}
		switch(data.getByte(p)) {
		case '0': data.setByte(p, (byte)'{'); return;
		case '1': data.setByte(p, (byte)'A'); return;
		case '2': data.setByte(p, (byte)'B'); return;
		case '3': data.setByte(p, (byte)'C'); return;
		case '4': data.setByte(p, (byte)'D'); return;
		case '5': data.setByte(p, (byte)'E'); return;
		case '6': data.setByte(p, (byte)'F'); return;
		case '7': data.setByte(p, (byte)'G'); return;
		case '8': data.setByte(p, (byte)'H'); return;
		case '9': data.setByte(p, (byte)'I'); return;
		default: data.setByte(p, (byte)'{'); return;
		}
	}

	/**
	 * libcob/move.cのstore_common_regionの実装
	 * @param field
	 * @param data
	 * @param size
	 * @param scale
	 */
	public void storeCommonRegion(AbstractCobolField field, CobolDataStorage data, int size, int scale) {
		this.storeCommonRegion(field, data, 0, size, scale);
	}

	/**
	 * libcob/move.cのstore_common_regionの実装
	 * @param field
	 * @param data
	 * @param size
	 * @param scale
	 */
	public void storeCommonRegion(AbstractCobolField field, CobolDataStorage data, int dataStartIndex, int size, int scale) {
		int lf1 = -scale;
		int lf2 = -field.getAttribute().getScale();
		int hf1 = size + lf1;
		int hf2 = field.getFieldSize() + lf2;

		int  lcf = Math.max(lf1, lf2);
		int  gcf = Math.min(hf1, hf2);

		for(int i=0; i<field.getFieldSize(); ++i) {
			field.getDataStorage().setByte(i + field.getFirstDataIndex(), (byte)0x30);
		}

		if(gcf > lcf) {
			int csize = gcf - lcf;
			int p = hf1 - gcf;
			int q = field.getFirstDataIndex() + hf2 - gcf;
			for(int cinc=0; cinc < csize; ++cinc, ++p, ++q) {
				if(data.getByte(dataStartIndex + p) == (byte)0x20) {
					field.getDataStorage().setByte(q, (byte)0x30);
				} else {
					byte value = data.getByte(dataStartIndex + p);
					field.getDataStorage().setByte(q, value);
				}
			}
		}
	}

	/**
	 * 引数で与えらえられたデータからthisへの代入を行う
	 * @param field 代入元のデータ(byte[]型)
	 */
	@Override
	public void moveFrom(byte[] bytes) {
		this.dataStorage.setData(bytes);
	}

	/**
	 * 引数で与えらえられたデータからthisへの代入を行う
	 * @param field 代入元のデータ(String型)
	 */
	@Override
	public void moveFrom(String string) {
		try {
			this.dataStorage.setData(string.getBytes("SJIS"));
		} catch (UnsupportedEncodingException e) {
			// TODO ログの対応
			e.printStackTrace();
			throw new CobolRuntimeException(CobolRuntimeException.COBOL_FITAL_ERROR, "エンコードエラー");
		}
	}

	/**
	 * 引数で与えらえられたデータからthisへの代入を行う
	 * @param field 代入元のデータ(int型)
	 */
	@Override
	public void moveFrom(int number) {
		for(int i=0; i<this.size; ++i) {
			this.dataStorage.setByte(i, (byte)0x30);
		}

		for(int i=this.size-1; i>=0; --i) {
			this.dataStorage.setByte(i, (byte)(0x30 + number % 10));
			number /= 10;
		}
	}

	/**
	 * thisとCobolNumericFieldを比較する
	 * @param field 比較するデータ
	 * @return thisのほうが大きいときは正数,thisのほうが小さいときは負数,それ以外は0を返す
	 */
	private int compareToNumeric(AbstractCobolField field) {
		CobolDecimal d1 = this.getDecimal();
		CobolDecimal d2 = field.getDecimal();
		return d1.compareTo(d2);
	}

	/**
	 * CobolDecimalに変換する
	 * @return thisの保持する数値データをCobolDecimalに変換した値
	 */
	@Override
	public CobolDecimal getDecimal() {
		CobolDataStorage data = this.getDataStorage();
		int firstDataIndex = this.getFirstDataIndex();
		int size = this.getFieldSize();

		if(data.getByte(firstDataIndex) == 255) {
			//TODO 実装
			throw new CobolRuntimeException(0, "未実装");
		}

		if(data.getByte(firstDataIndex) == 0) {
			//TODO  実装
			throw new CobolRuntimeException(0, "未実装");
		}

		char[] buf = new char[size];
		for(int i=0; i<size; ++i) {
			byte val = data.getByte(firstDataIndex + i);
			if(val >= 0x70) {
				buf[i] = (char)(val - 0x40);
			} else {
				buf[i] = (char)val;
			}
		}
		
		CobolFieldAttribute attr = this.getAttribute();
		int sign = 1;
		if(attr.isFlagHaveSign()) {
			if(attr.isFlagSignSeparate()) {
				int signIndex = attr.isFlagSignLeading() ? 0 : this.getSize() - 1;
				if(data.getByte(signIndex) == '-') {
					sign = -1;
				}
			} else {
				int signIndex = attr.isFlagSignLeading() ? 0 : size - 1;
				if(buf[signIndex] >= 0x70) {
					buf[signIndex] -= 0x40;
					sign = -1;
				}
			}
		}

		BigDecimal decimal = new BigDecimal(buf);
		if(sign < 0) {
			decimal = decimal.negate();
		}
		CobolDecimal ret = new CobolDecimal(decimal);
		ret.setScale(this.getAttribute().getScale());
		return ret;
	}

	//addInt内のgotoの代替として使用する
	class OverflowException extends Exception {}

	/**
	 * thisの保持する数値データに加算する
	 * @param in thisの保持する数値データに加算する値
	 */
	@Override
	public int addInt(int in) {
		if(in == 0) {
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

		for(int i=0; i<osize; ++i) {
			tfield[i] = data.getByte(firstDataIndex + i);
		}

		if(sign < 0) {
			n = -n;
		}
		while(scale > 0) {
			--scale;
			n *= 10;
		}

		if(scale < 0) {
			if(-scale < 10) {
				while(scale != 0) {
					++scale;
					n /= 10;
				}
			} else {
				n = 0;
			}
		} else {
			size -= scale;
		}

		try {
			if(n > 0) {
				if(displayAddInt(data, firstDataIndex, size, n) != 0) {
					for(int i=0; i<osize; ++i) {
						data.setByte(firstDataIndex + i, tfield[i]);
					}
					throw new OverflowException();
				}
			} else if(n < 0) {
				if(displaySubInt(data, firstDataIndex, size, -n) != 0) {
					for(int i=0; i<size; ++i) {
						byte val = data.getByte(firstDataIndex + i);
						data.setByte(firstDataIndex + i, (byte)(9 - (val - 0x30) +0x30));
					}
					displayAddInt(data, firstDataIndex, size, 1);
					sign = -sign;
				}
			}

			this.putSign(sign);
			return 0;

		} catch(OverflowException e) {
			this.putSign(sign);
			//TODO set exception code
			return 0;
		}
	}

	/**
	 * libcob/numeric.cのdisplay_add_intの実装
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
		while(n > 0) {
			i = (int) (n % 10L);
			n /= 10;

			/* check for overflow */
			if(--sp < firstDataIndex) {
				if(CobolModule.getCurrentModule().flag_binary_truncate == 0) {
					return 0;
				}
				return 1;
			}

			/* perform addtion */
			int is = (data.getByte(sp) & 0x0F) + i + carry;
			if(is > 9) {
				carry = 1;
				data.setByte(sp, (byte) (0x30 + (is % 10)));
			} else {
				carry = 0;
				data.setByte(sp, (byte)(0x30 + is));
			}
		}
		if(carry == 0) {
			return 0;
		}

		/* carry up */
		while(--sp >= firstDataIndex) {
			byte val = data.getByte(sp);
			data.setByte(sp, (byte)(val + 1));
			if(val + 1 <= '9') {
				return 0;
			}
			data.setByte(sp, (byte)'0');
		}

		if(CobolModule.getCurrentModule().flag_binary_truncate == 0) {
			return 0;
		}
		return 1;
	}

	/**
	 * libcob/numeric.cのdisplay_sub_intの実装
	 * @param data
	 * @param firstDataIndex dataにアクセスするときの開始位置
	 * @param size
	 * @param n
	 * @return
	 */
	private int displaySubInt(CobolDataStorage data, int firstDataIndex, int size , long n) {
			int carry = 0;
		int sp = firstDataIndex + size;
		int i;
		while(n > 0) {
			i = (int) (n % 10L);
			n /= 10;

			/* check for overflow */
			if(--sp < firstDataIndex) {
				return 1;
			}

			/* perform subtraction */
			byte val = data.getByte(sp);
			data.setByte(sp, (byte) (val - (i + carry)));
			if(val - (i + carry) < '0') {
				carry = 1;
				data.setByte(sp, (byte) (data.getByte(sp) + 10));
			} else {
				carry = 0;
			}
		}
		if(carry == 0) {
			return 0;
		}

		/* carry up */
		while(--sp >= firstDataIndex) {
			byte val = data.getByte(sp);
			data.setByte(sp, (byte)(val - 1));
			if(val - 1 >= '0') {
				return 0;
			}
			data.setByte(sp, (byte)'9');
		}
		return 1;
	}

	/**
	 * thisをCobolNumericFieldに変換する.
	 * indirect moveをするときに使用されることを想定している.
	 * @return thisからCobolNumericField型へ変換した値
	 */
	@Override
	public CobolNumericField getNumericField() {
		return this;
	}

	/**
	 * 引数で与えらえられたデータからthisへの代入を行う
	 * @param field 代入元のデータ(double型)
	 */
	@Override
	public void moveFrom(double number) {
		// TODO 自動生成されたメソッド・スタブ
		this.moveFrom((int)number);
	}

	/**
	 * 引数で与えらえられたデータからthisへの代入を行う
	 * @param field 代入元のデータ(BigDecimal型)
	 */
	@Override
	public void moveFrom(BigDecimal number) {
		// TODO 自動生成されたメソッド・スタブ
	}

	/**
	 * 引数で与えらえられたデータからthisへの代入を行う
	 * @param field 代入元のデータ(CobolDataStorage型)
	 */
	@Override
	public void moveFrom(CobolDataStorage dataStrage) {
		// TODO 自動生成されたメソッド・スタブ
	}

	/**
	 * 実装しないメソッド
	 */
	public int addPackedInt(int n) {
		throw new CobolRuntimeException(0, "実装しないコード");
	}

	/**
	 * libcob/move.cのcob_display_get_long_longの実装
	 */
	@Override
	public long getLong() {
		int size = this.getSize();
		CobolDataStorage data = this.getDataStorage();
		int sign = this.getSign();
		int i = 0;
		
		for(i=0; i<size; ++i) {
			if(data.getByte(i) != '0') {
				break;
			}
		}
	
		long val = 0;
		int scale = this.getAttribute().getScale();
		if(scale < 0) {
			for(; i<size; ++i) {
				val = val * 10 + (data.getByte(i) - '0');
			}
			val *= CobolConstant.exp10LL[-scale];
		} else {
			size -= scale;
			for(; i<size; ++i) {
				val = val * 10 + (data.getByte(i) - '0');
			}
		}
		if(sign < 0) {
			val = -val;
		}
		this.putSign(sign);
		return val;
	}
}