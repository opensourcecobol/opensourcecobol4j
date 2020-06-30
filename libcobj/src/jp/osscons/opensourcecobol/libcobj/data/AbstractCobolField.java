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
import java.nio.ByteBuffer;

import jp.osscons.opensourcecobol.libcobj.exceptions.CobolRuntimeException;

/**
 * COBOLで使用する変数を表現するクラス。
 * PIC文字列の種類に応じて,このクラスを継承したクラスを作成する
 */
public abstract class AbstractCobolField {
	/**
	 * データを格納に使用するバイト配列の長さ
	 */
	protected int size;
	/**
	 * データを格納するバイト配列を扱うオブジェクト
	 */
	protected CobolDataStorage dataStorage;
	/**
	 * 変数に関する様々な情報を保持するオブジェクト(符号付か,COMP-3指定かなど)
	 */
	protected CobolFieldAttribute attribute;

	final static int[] cobExp10 = {
		1,
		10,
		100,
		1000,
		10000,
		100000,
		1000000,
		10000000,
		100000000,
		1000000000
	};

	/**
	 * moveAlphanumericToNumericで使用
	 * C言語のgotoの代替機能を提供する
	 * @author y-sakamoto
	 */
	protected class GotoException extends Exception {
	}

	/**
	 * コンストラクタ
	 * @param size データを格納するバイト配列の長さ
	 * @param dataStorage データを格納するバイト配列を扱うオブジェクト
	 * @param attribute 変数に関する様々な情報を保持するオブジェクト(符号付か,COMP-3指定かなど)
	 */
	public AbstractCobolField (int size, CobolDataStorage dataStorage, CobolFieldAttribute attribute) {
		this.size = size;
		this.dataStorage = dataStorage;
		this.attribute = attribute;
	}

	/**
	 * メンバ変数dataStorageのgetter
	 * @return  this.dataStorage
	 */
	public CobolDataStorage getDataStorage() {
		return dataStorage;
	}
	/**
	 * メンバ変数dataStorageのsetter
	 * @return  this.dataStorage
	 */
	public void setDataStorage(CobolDataStorage dataStorage) {
		this.dataStorage = dataStorage;
	}

	/**
	 * メンバ変数attributeのgetter
	 * @return this.attribute
	 */
	public CobolFieldAttribute getAttribute() {
		return attribute;
	}

	/**
	 * メンバ変数sizeのsetter
	 * @param size
	 */
	public void setSize(int size) {
		this.size = size;
	}

	/**
	 * メンバ変数sizeのgetter
	 * @return this.size
	 */
	public int getSize() {
		return size;
	}

	/**
	 * opensource COBOLのCOB_FIELD_SIZEマクロに相当するメソッド
	 * @return 符号付で符号が分離している場合はthis.size-1,そうでなければthis.size
	 */
	public int getFieldSize() {
		return this.size - (this.attribute.isFlagSignSeparate() ? 1 : 0);
	}

	/**
	 * opensource COBOLのCOB_FIELD_DATAに相当するメソッド
	 * バイト配列の中で(符号データではなく)数値データの格納されている最小の添え字を返す
	 * opensource COBOLではポインタを返しているが,このメソッドは添え字を返す
	 * @return SIGN_LEADINGかつSIGN_SEPARATEなら1,それ以外は0
	 */
	public int getFirstDataIndex() {
		return (this.attribute.isFlagSignSeparate() && this.attribute.isFlagSignLeading()) ? 1 : 0;
	}

	abstract public byte[] getBytes();

	/**
	 * thisの文字列表現をかえす.(toStringだけで十分か?)
	 * @return thisの文字列表現
	 */
	abstract public String getString();

	/**
	 *
	 * @return
	 */
	public int getInt() {
		CobolFieldAttribute attr = new CobolFieldAttribute(
			CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY, 9, 0,
			CobolFieldAttribute.COB_FLAG_HAVE_SIGN, null);
		CobolDataStorage n = new CobolDataStorage(new byte[4], 0);
		AbstractCobolField temp = CobolFieldFactory.makeCobolField(4, n, attr);
		temp.moveFrom(this);
		return ByteBuffer.wrap(n.getByteArray(0, 4)).getInt();
	}
	/**
	 *
	 * @return
	 */
	abstract public double getDouble();
	/**
	 * 数値を表すデータが実装すべきメソッド.
	 * 保持する数値データをCobolDecimal型に変換する.
	 * @return 保持する数値データをCobolDecimal型に変換した値
	 */
	abstract public CobolDecimal getDecimal();
	/**
	 * TODO 確認 未使用?
	 * @param decimal
	 */
	abstract public void setDecimal(BigDecimal decimal);

	/**
	 * thisの保持する数値データをint型で返す
	 * @return thisの保持する数値データをintに変換した値
	 */
	public int getInt(int size) {
		int retval = 0;
		int p = 0;
		CobolDataStorage data = this.getDataStorage();
		for(int n = 0; n < size; ++n, ++p) {
			retval *= 10;
			if(data.getByte(p) > (byte)'9') {
				retval += 10;
			} else {
				retval += data.getByte(p) - (byte)'0';
			}
		}
		return retval;
	}

	/**
	 * thisの保持する数値データを0に設定するメソッド.
	 */
	public void setZero() {
		throw new CobolRuntimeException(0, "未実装");
	}

	/**
	 * libcob/numeric.cのcob_addの実装
	 * thisの保持する数値データに,引数で与えられたフィールドの保持する数値データを加算する
	 * @param field 加算する数値を保持するフィールド
	 * @param opt 加算に関するオプション.詳しくはopensourceCOBOLを参照
	 * @return 加算後のthisの保持する数値データ
	 */
	public int add(AbstractCobolField field, int opt) {
		CobolDecimal d1 = this.getDecimal();
		CobolDecimal d2 = field.getDecimal();
		d1.add(d2);
		return d1.getField(this, opt);
	}

	/**
	 * libcob/numeric.cのcob_subの実装
	 * thisの保持する数値データに,引数で与えられたフィールドの保持する数値データを減算する
	 * @param field 減算する数値を保持するフィールド
	 * @param opt 減算に関するオプション.詳しくはopensourceCOBOLを参照
	 * @return 減算後のthisの保持する数値データ
	 */
	public int sub(AbstractCobolField field, int opt) {
		CobolDecimal d1 = this.getDecimal();
		CobolDecimal d2 = field.getDecimal();
		d1.sub(d2);
		return d1.getField(this, opt);
	}

	/**
	 * libcob/numeric.cのcob_add_intの実装?
	 * 保持する数値データに指定された値を加算する
	 * @param n thisの保持する数値データから加算する数値
	 * @return 基本的に0が返される.詳しくはopensource COBOLを参照
	 */
	public int addInt(int n) {
		if(n == 0) {
			return 0;
		}
		CobolDecimal d1 = this.getDecimal();
		CobolDecimal d2 = new CobolDecimal(n);
		d2.setScale(0);
		if(d1.getScale() != 0) {
			BigDecimal cobMexp = BigDecimal.TEN.pow(d1.getScale());
			d2.setValue(d2.getValue().multiply(cobMexp));
			d2.setScale(d1.getScale());
		}
		d1.setValue(d1.getValue().add(d2.getValue()));
		return d1.getField(this, 0);
	}

	/**
	 * libcob/numeric.cのcob_add_packed
	 * @param n
	 * @return
	 */
	abstract public int addPackedInt(int n);

	/**
	 * thisの保持する数値データに指定された値を減算する
	 * @param n thisの保持する数値データから減算する数値
	 * @return 基本的に0が返される.詳しくはopensource COBOLを参照
	 */
	public int subInt(int n) {
		return n == 0 ? 0 : this.addInt(-n);
	}

	/**
	 * libcob/numeric.cのcob_div_quotientの実装
	 */
	public int divQuotient(AbstractCobolField divisor, AbstractCobolField quotient, int opt) {
		AbstractCobolField dividend = this;
		CobolDecimal d1 = dividend.getDecimal();
		CobolDecimal d2 = divisor.getDecimal();
		CobolDecimal.cobD3.set(d1);

		d1.div(d2);
		if(d1.getScale() == CobolDecimal.DECIMAL_NAN) {
			CobolDecimal.cobD3.setScale(CobolDecimal.DECIMAL_NAN);
			//TODO 例外を投げるべきか?
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
	 * @param opt
	 * @return
	 */
	public int divRemainder(int opt) {
		return CobolDecimal.cobD3.getField(this, opt);
	}

	/**
	 * libcob/numeric.cのcob_cmp_intの実装
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
	 * libcob/numeric.cのcob_cmp_uintの実装
	 * @param n
	 * @return
	 */
	public int cmpUint(int n) {
		return this.cmpInt(n);
	}

	/**
	 * libcob/numeric.cのcob_numeric_cmpの実装
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
	 * @return thisの保持する数値データが負ならば負数,0なら0,正なら正数を返す
	 */
	public int getSign() {
		return 0;
	}

	/**
	 * thisの保持する数値データの符号を設定する
	 * @param sign 正符号を設定するときは正数,負符号を設定するときは負数,それ以外は0
	 */
	public void putSign(int sign) {
	}

	/**
	 * 引数で与えらえられたデータからthisへの代入を行う
	 * @param field 代入元のデータ(AbstractCobolField型)
	 */
	abstract public void moveFrom(AbstractCobolField field);
	/**
	 * 引数で与えらえられたデータからthisへの代入を行う
	 * @param field 代入元のデータ(CobolDataStorage型)
	 */
	abstract public void moveFrom(CobolDataStorage dataStrage);
	/**
	 * 引数で与えらえられたデータからthisへの代入を行う
	 * @param field 代入元のデータ(byte[]型)
	 */
	abstract public void moveFrom(byte[] bytes);
	/**
	 * 引数で与えらえられたデータからthisへの代入を行う
	 * @param field 代入元のデータ(String型)
	 */
	abstract public void moveFrom(String string);
	/**
	 * 引数で与えらえられたデータからthisへの代入を行う
	 * @param field 代入元のデータ(int型)
	 */
	abstract public void moveFrom(int number);
	/**
	 * 引数で与えらえられたデータからthisへの代入を行う
	 * @param field 代入元のデータ(double型)
	 */
	abstract public void moveFrom(double number);
	/**
	 * 引数で与えらえられたデータからthisへの代入を行う
	 * @param field 代入元のデータ(BigDecimal型)
	 */
	abstract public void moveFrom(BigDecimal number);

	/**
	 * opensourceCOBOLのcob_check_numericの実装
	 * @param s
	 */
	public void checkNumeric(String s) {
	}

	//TODO abstract指定
	/**
	 * thisと引数で与えられたデータとの数値比較を行う
	 * @param field thisと比較するfield
	 * @return 保持する数値データの比較を行い,this<fieldなら負の値,this==fieldなら0,this>fieldなら正の値
	 */
	public int compareTo(AbstractCobolField field) {
		AbstractCobolField f1 = numericFieldToNumericDisplayField(this);
		AbstractCobolField f2 = numericFieldToNumericDisplayField(field);
		return f1.cmpAlnum(f2);
	}

	/**
	 * 引数で与えられた数値データを保持するフィールドと同じ数値を保持するCobolNumericFieldに変換する
	 * @param field 変換するクラス
	 * @return 引数で与えられた数値データを保持するフィールドを同じ数値を保持するCobolNumericField型のフィールド
	 */
	private AbstractCobolField numericFieldToNumericDisplayField(AbstractCobolField field) {
		CobolFieldAttribute attr = field.getAttribute();
		CobolDataStorage data = new CobolDataStorage(48);
		if(attr.isTypeNumeric()) {
			if(!attr.isTypeNumericDisplay()) {
				CobolFieldAttribute newAttr = new CobolFieldAttribute(
						CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY,
						attr.getDigits(),
						attr.getScale(),
						attr.getFlags() & (~CobolFieldAttribute.COB_FLAG_HAVE_SIGN),
						attr.getPic());
				CobolNumericField temp = new CobolNumericField(attr.getDigits(), data, newAttr);
				temp.moveFrom(field);
				field = temp;
			} else if(attr.isFlagHaveSign()) {
				CobolFieldAttribute newAttr = new CobolFieldAttribute(
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
	 * thisの保持するデータと与えられたフィールドの保持するデータを比較する
	 * @param field thisと比較するfield
	 * @return
	 */
	public int cmpAlnum(AbstractCobolField field) {
		int sign1 = this.getSign();
		int sign2 =field.getSign();
		int ret = this.cmpSimpleStr(field);
		if(!this.getAttribute().isTypeNumericPacked()) {
			this.putSign(sign1);
		}
		if(!field.getAttribute().isTypeNumericPacked()) {
			field.putSign(sign1);
		}
		return ret;
	}

	/**
	 * thisの保持するデータと与えられたフィールドの保持するデータを比較する
	 * @param field thisと比較するfield
	 * @return
	 */
	public int cmpSimpleStr(AbstractCobolField field) {
		//TODO  条件分岐
		AbstractCobolField lf, sf;
		if(this.size < field.size) {
			lf = field;
			sf = this;;
		} else {
			lf = this;
			sf = field;
		}
		//TODO moduleを参照するコードにする
		CobolDataStorage s = null;
		int ret;
		if((ret = alnumCmps(this.getDataStorage(), 0, field.getDataStorage(), 0, sf.getSize(), s)) == 0) {
			if(lf.getSize() > sf.getSize()) {
				ret = (lf.getAttribute().isTypeNational()) ?
						//TODO 実装
						0
						: commonCmpc(lf.getDataStorage(), sf.getSize(), ' ', lf.getSize() - sf.getSize());
				//TODO 確認
				if(this.getSize() < field.getSize()) {
					ret = -ret;
				}
			}
		}
		return ret;
	}

	/*public int compareInt(int n) {
		CobolDecimal cobD1 = this.getDecimal();
		CobolDecimal cobD2 = new CobolDecimal(n);
		cobD2.setScale(0);
		return cobD1.compareTo(cobD2);
	}

	protected int compareChar(int c) {
		int sign = this.getSign();
		int ret = commonCmpc(this.getDataStorage(), c, this.getSize());
		if(this.getAttribute().isTypeNumericPacked()) {
			this.putSign(sign);
		}
		return ret;
	}*/

	/**
	 * libcob/common.cのcommon_cmpcの実装
	 * @param s1
	 * @param s1StartIndex s1のバイトデータにアクセスるするときの最小の添え字の相対位置
	 * @param c
	 * @param size
	 * @return
	 */
	protected int commonCmpc(CobolDataStorage s1, int s1StartIndex, int c, int size) {
		//TODO moduleを参照するコードを書く
		int ret;
		for(int i=0; i<size; ++i) {
			if((ret = s1.getByte(s1StartIndex + i) - c) != 0) {
				return ret;
			}
		}
		return 0;
	}

	/**
	 * libcob/common.cのcob_cmp_allの実装
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
			while(size >= field.getSize()) {
				//TODO moduleを参照するコードにする
				if((ret = alnumCmps(data, p, field.getDataStorage(), 0, this.getSize(), null)) != 0) {
					throw new GotoException();
				}
				size -= field.getSize();
				p += field.getSize();
			}
			if(size > 0) {
				//TODO moduleを参照するコードにする
				ret = alnumCmps(data, 0, field.getDataStorage(), 0, this.getSize(), null);
			}
		} catch(Exception e) {
		}

		this.putSign(sign);
		return ret;
	}

	/**
	 * libcob/common.cのalnum_cmpsの実装
	 * @param s1
	 * @param s1Start s1のバイトデータにアクセスるするときの最初の添え字の相対位置
	 * @param s2
	 * @param s2Start s2のバイトデータにアクセスるするときの最初の添え字の相対位置
	 * @param size
	 * @param col
	 * @return
	 */
	protected int alnumCmps(CobolDataStorage s1, int s1Start, CobolDataStorage s2, int s2Start, int size, CobolDataStorage col) {
		int ret;
		if(col != null) {
			//TODO 実装
			throw new CobolRuntimeException(0, "未実装");
		} else {
			for(int i=0; i<size; ++i) {
				if((ret = s1.getByte(i + s1Start) - s2.getByte(i + s2Start)) != 0) {
					return ret;
				}
			}
		}
		return 0;
	}

	//TODO abstract指定
	public BigDecimal getBigDecimal() {
		return BigDecimal.ZERO;
	}

	/**
	 * thisをCobolNumericFieldに変換する.
	 * indirect moveをするときに使用されることを想定している.
	 * @return thisからCobolNumericField型へ変換した値
	 */
	public CobolNumericField getNumericField() {
		return new CobolNumericField(this.getSize(), this.getDataStorage(), this.getAttribute());
	}

	/**
	 * libcob/common.cのcob_check_mvstrnumの実装
	 * @param field
	 */
	public void checkMoveStrNum(AbstractCobolField field) {
		switch (this.getAttribute().getType()) {
		case CobolFieldAttribute.COB_TYPE_ALPHANUMERIC:
		case CobolFieldAttribute.COB_TYPE_ALPHANUMERIC_ALL:
		case CobolFieldAttribute.COB_TYPE_ALPHANUMERIC_EDITED:
			switch (field.getAttribute().getType()) {
			case CobolFieldAttribute.COB_TYPE_NUMERIC:
			/*case COB_TYPE_NUMERIC_DISPLAY:*/
			case CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY:
			case CobolFieldAttribute.COB_TYPE_NUMERIC_PACKED:
			case CobolFieldAttribute.COB_TYPE_NUMERIC_FLOAT:
			case CobolFieldAttribute.COB_TYPE_NUMERIC_DOUBLE:
			case CobolFieldAttribute.COB_TYPE_NUMERIC_EDITED:
				CobolDataStorage data = this.getDataStorage();
				int firstIndex = this.getFirstDataIndex();
				for (int i=0; i<this.getSize(); i++) {
					byte val = data.getByte(firstIndex + i);
					if (val < 0x30 || 0x39 < val) {
						System.out.println("Numeric value is expected");
						//TODO STOP RUNを呼ぶ
					}
				}
				break;
			}
			break;
	    }
	}


	/**
	 * libcob/move.c own_byte_memcpyの実装
	 * @param s1
	 * @param s1StartIndex s1のバイトデータにアクセスるするときの最初の添え字の相対位置
	 * @param s2
	 * @param s2StartIndex s2のバイトデータにアクセスるするときの最初の添え字の相対位置
	 * @param size
	 */
	protected void ownByteMemcpy(CobolDataStorage s1 , int s1StartIndex, CobolDataStorage s2, int s2StartIndex, int size) {
		int i=0;
		do {
			s1.setByte(s1StartIndex +i, s2.getByte(s2StartIndex + i));
			i++;
		} while(--size > 0);
	}

	@Override
	public String toString() {
		return this.getString();
	}

	/**
	 * libcob/common.cのcob_field_to_stringの実装
	 * TODO CobolNationalFieldでオーバーライドしなくても済むように修正する.
	 * @return this.dataの保持するデータを文字列にして返す.
	 */
	public String fieldToString() {
		CobolDataStorage data = this.getDataStorage();
		int i;
		for(i=this.getSize() -1; i >= 0; --i) {
			if(data.getByte(i) != ' ' && data.getByte(i) != 0) {
				break;
			}
		}
		return new String(data.getByteArray(0, i+1));
	}

	/**
	 * libcob/move.cのcob_set_intの実装
	 */
	public void setInt(int n) {
		CobolFieldAttribute attr = new CobolFieldAttribute(
				CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY,
				9,
				0,
				CobolFieldAttribute. COB_FLAG_HAVE_SIGN,
				null);
		CobolDataStorage data = new CobolDataStorage(ByteBuffer.allocate(4).putInt(n).array());
		AbstractCobolField temp = CobolFieldFactory.makeCobolField(4, data, attr);
		this.moveFrom(temp);
	}
}