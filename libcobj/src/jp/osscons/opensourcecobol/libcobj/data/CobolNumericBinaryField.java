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

public class CobolNumericBinaryField extends AbstractCobolField {

	/**
	 * コンストラクタ
	 * @param size データを格納するバイト配列の長さ
	 * @param dataStorage データを格納するバイト配列を扱うオブジェクト
	 * @param attribute 変数に関する様々な情報を保持するオブジェクト
	 */
	public CobolNumericBinaryField(int size, CobolDataStorage dataStorage, CobolFieldAttribute attribute) {
		super(size, dataStorage, attribute);
	}

	/**
	 * TODO
	 */
	@Override
	public byte[] getBytes() {
		// TODO 自動生成されたメソッド・スタブ
		return null;
	}

	/**
	 * 実装しないメソッド
	 */
	public int addPackedInt(int n) {
		throw new CobolRuntimeException(0, "実装しないコード");
	}

	/**
	 * thisの文字列表現をかえす.(toStringだけで十分か?)
	 * @return thisの文字列表現
	 */
	@Override
	public String getString() {
		//TODO 以下のコメントアウトしたコードを消去する.
		/*int n = 0;
		for(int i=0; i<this.getSize(); ++i) {
			System.out.println(String.format("%02x", this.getDataStorage().getByte(i)));
		}
		for(int i=0; i<4; ++i) {
			if(i < this.getSize()) {
				n = ((n << 8) & -255) | (this.getDataStorage().getByte(i) & 0xFF);
			} else {
				n <<= 8;
			}
		}

		CobolFieldAttribute attr = this.getAttribute();
		String format = String.format("%%0%dd", attr.getDigits());
		String body = String.format(format, Math.abs(n));
		//System.out.println("n: " + n);
		int scale = this.getAttribute().getScale();
		if(scale > 0) {
			int len = body.length();
			body = body.substring(0, len - scale) + "." + body.substring(len-scale, len);
		}

		if(this.getAttribute().isFlagHaveSign()) {
			if(this.getSign() < 0) {
				return "-" + body;
			} else {
				return "+" + body;
			}
		} else {
			return body;
		}*/
		CobolFieldAttribute thisAttr = this.getAttribute();
		int flag = thisAttr.isFlagHaveSign() ? CobolFieldAttribute.COB_FLAG_HAVE_SIGN : 0;
		CobolFieldAttribute attr = new CobolFieldAttribute(
				CobolFieldAttribute.COB_TYPE_NUMERIC,
				thisAttr.getDigits(),
				thisAttr.getScale(),
				flag,
				"");
		CobolDataStorage storage = new CobolDataStorage(thisAttr.getDigits());
		CobolNumericField numericField = new CobolNumericField(thisAttr.getDigits(), storage, attr);
		numericField.moveFrom(this);
		return numericField.getString();
	}

	/**
	 * thisの保持する数値データの符号を返す
	 * @return thisの保持する数値データが負ならば負数,0なら0,正なら正数を返す
	 */
	@Override
	public int getSign() {
		int n = 0;
		for(int i=0; i<4; ++i) {
			if(i < this.getSize()) {
				n = ((n << 8) & -255) | (this.getDataStorage().getByte(i) & 0xFF);
			} else {
				n <<= 8;
			}
		}

		if(n < 0) {
			return -1;
		} else if(n > 0) {
			return 1;
		} else {
			return 0;
		}
	}

	/**
	 * TODO
	 */
	@Override
	public double getDouble() {
		// TODO 自動生成されたメソッド・スタブ
		return 0;
	}

	/**
	 * TODO
	 */
	@Override
	public void setDecimal(BigDecimal decimal) {
		// TODO 自動生成されたメソッド・スタブ

	}

	/**
	 * 引数で与えらえられたデータからthisへの代入を行う
	 * @param field 代入元のデータ(AbstractCobolField型)
	 */
	@Override
	public void moveFrom(AbstractCobolField field) {
		switch(field.getAttribute().getType()) {
		case CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY:
			this.moveDisplayToBinary(field);
			return;
		case CobolFieldAttribute.COB_TYPE_NUMERIC_PACKED:
		case CobolFieldAttribute.COB_TYPE_ALPHANUMERIC:
		case CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY:
		case CobolFieldAttribute.COB_TYPE_NATIONAL:
			this.moveFrom(field.getNumericField());
			return;
		default:
			throw new CobolRuntimeException(0, "未実装");
		}
	}

	/**
	 * CobolNumericFieldからからthisへの代入
	 * @param field 代入元のデータ(AbstractCobolField型)
	 */
	private void moveDisplayToBinary(AbstractCobolField field) {
		int size1 = field.getFieldSize();
		int data1Index = field.getFirstDataIndex();
		int sign = field.getSign();

		CobolDataStorage data1 = field.getDataStorage();
		CobolDataStorage data2 = this.getDataStorage();

		boolean flagNull = true;
		for(int i=0; i<size1; ++i) {
			if(data1.getByte(data1Index + i) != 0) {
				flagNull = false;
			}
		}
		if(flagNull) {
			for(int i=0; i<this.getSize(); ++i) {
				data2.setByte(i, (byte)0);
			}
		}

		int size = size1 - field.getAttribute().getScale() + this.getAttribute().getScale();
		int val = 0;
		for(int i=0; i < size; ++i) {
			if(i < size1) {
				//TODO 確認
				//バグを修正するためにopensource COBOLにはない処理を書き加えた
				//なぜopensource COBOLはこの処理を書き加えずに正しく動作しているか不明
				byte x = data1.getByte(data1Index + i);
				x = (byte) (x >= (byte)0x70 ? x - (byte)0x70 : x - (byte)0x30);
				val = val * 10 + x;
			} else {
				val = val * 10;
			}
		}

		if(sign < 0 && this.getAttribute().isFlagHaveSign()) {
			val = -val;
		}

		//TODO moduleに関する処理を追加
		// libcob/move.c 636
		this.binaryMsetInt64(val);
		field.putSign(sign);
	}

	/**
	 * libcob/move.cのcob_binary_mset_int64の実装
	 * @param n
	 */
	public void binaryMsetInt64(long n) {
		//TODO Javaの内部エンディアンの調査
		//JavaはBIG ENDIANのためopensource COBOLにある条件分岐は削除した
		byte bytes[] = new byte[8];
		for(int i=0;  i<8; ++i) {
			bytes[i] = (byte) ((byte) (n >> ((7 - i) * 8)) & 0x00000000000000FFL);
		}
		CobolDataStorage nData = new CobolDataStorage(bytes, 0);
		this.ownByteMemcpy(this.getDataStorage(), 0, nData, 8 - this.size, this.size);
	}

	/**
	 * CobolNumericFieldからからthisへの代入
	 * @param field 代入元のデータ(CobolDataStorage型)
	 */
	@Override
	public void moveFrom(CobolDataStorage dataStrage) {
		// TODO 自動生成されたメソッド・スタブ

	}

	/**
	 * CobolNumericFieldからからthisへの代入
	 * @param field 代入元のデータ(byte[]型)
	 */
	@Override
	public void moveFrom(byte[] bytes) {
		// TODO 自動生成されたメソッド・スタブ

	}

	/**
	 * CobolNumericFieldからからthisへの代入
	 * @param field 代入元のデータ(String型)
	 */
	@Override
	public void moveFrom(String string) {
		// TODO 自動生成されたメソッド・スタブ

	}

	/**
	 * CobolNumericFieldからからthisへの代入
	 * @param field 代入元のデータ(int型)
	 */
	@Override
	public void moveFrom(int number) {
		this.getDataStorage().setSwpU32Binary(number);
	}

	/**
	 * CobolNumericFieldからからthisへの代入
	 * @param field 代入元のデータ(double型)
	 */
	@Override
	public void moveFrom(double number) {
		// TODO 自動生成されたメソッド・スタブ

	}

	/**
	 * CobolNumericFieldからからthisへの代入
	 * @param field 代入元のデータ(BigDecimal型)
	 */
	@Override
	public void moveFrom(BigDecimal number) {
		// TODO 自動生成されたメソッド・スタブ

	}

	/**
	 * thisをCobolNumericFieldに変換する.
	 * indirect moveをするときに使用されることを想定している.
	 * @return thisからCobolNumericField型へ変換した値
	 */
	public CobolNumericField getNumericField() {
		int size = this.getAttribute().getDigits();
		int scale = this.getAttribute().getScale();
		CobolFieldAttribute attr = new CobolFieldAttribute(
			CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY,
			size,
			scale,
			CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
			null
		);
		CobolDataStorage data = new CobolDataStorage(this.getAttribute().getDigits());
		CobolNumericField field  =new CobolNumericField(size, data, attr);
		field.moveFrom(this);
		return field;
	}

		/**
	 * libcob/numeric.cのcob_decimal_set_binaryの実装
	 * @param f
	 */
	@Override
	public CobolDecimal getDecimal() {
		CobolDecimal decimal = new CobolDecimal();
		if(this.getAttribute().isFlagHaveSign()) {
			decimal.setValue(new BigDecimal(this.binaryGetInt64()));
		} else {
			decimal.setValue(new BigDecimal(this.binaryGetUint64()));
		}
		decimal.setScale(this.getAttribute().getScale());
		return decimal;
	}

	/**
	 * libcob/numeric.cのcob_binary_get_int64の実装
	 * @param f
	 */
	private long binaryGetInt64() {
		int fsiz = 8 - this.getSize();
		long n = 0;
		byte[] nBytes = ByteBuffer.allocate(8).putLong(n).array();
		CobolDataStorage nStorage = new CobolDataStorage(nBytes);
		if(this.getAttribute().isFlagHaveSign()) {
			CobolDecimal.numByteMemcpy(nStorage, 0, this.getDataStorage(), 0, this.getSize());
			n = ByteBuffer.wrap(nStorage.getByteArray(0, this.getSize())).getLong();
			//TODO ビット演算に誤りがいないか確認
			n >>>= 8 * fsiz;
		} else {
			CobolDecimal.numByteMemcpy(nStorage, fsiz, this.getDataStorage(), 0,  this.getSize());
			n = ByteBuffer.wrap(nStorage.getByteArray(0, this.getSize())).getLong();
		}
		return n;
	}


	private long binaryGetUint64() {
		int fsiz = 8 - this.getSize();
		long n = 0;
		byte[] nBytes = ByteBuffer.allocate(8).putLong(n).array();
		CobolDataStorage nStorage = new CobolDataStorage(nBytes);
		CobolDecimal.numByteMemcpy(nStorage, fsiz, this.getDataStorage(), 0,  this.getSize());
		n = ByteBuffer.wrap(nStorage.getByteArray(0, 8), 0, 8).getLong();
		return n;
	}
}
