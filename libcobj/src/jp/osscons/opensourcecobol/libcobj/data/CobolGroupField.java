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

import jp.osscons.opensourcecobol.libcobj.exceptions.CobolRuntimeException;

/**
 * 集団項目を扱う
 */
public class CobolGroupField extends AbstractCobolField {

	/**
	 * コンストラクタ
	 * @param size データを格納するバイト配列の長さ
	 * @param dataStorage データを格納するバイト配列を扱うオブジェクト
	 * @param attribute 変数に関する様々な情報を保持するオブジェクト
	 */
	public CobolGroupField(int size, CobolDataStorage dataStorage, CobolFieldAttribute attribute) {
		super(size, dataStorage, attribute);
		// TODO 自動生成されたコンストラクター・スタブ
	}

	/**
	 * this.dataの保持するバイト配列のコピーを返す
	 */
	@Override
	public byte[] getBytes() {
		return dataStorage.getData(size);
	}

	/**
	 * thisの文字列表現をかえす.(toStringだけで十分か?)
	 * @return thisの文字列表現
	 */
	@Override
	public String getString() {
		// TODO 自動生成されたメソッ:/ド・スタブ
		try {
			return new String (dataStorage.getData(), "SJIS");
		} catch (UnsupportedEncodingException e) {
			// TODO 自動生成された catch ブロック
			// TODO ログの対応
			e.printStackTrace();
			throw new CobolRuntimeException(CobolRuntimeException.COBOL_FITAL_ERROR, "エンコードエラー");
		}
	}

	/**
	 * TODO
	 */
	@Override
	public int getInt() {
		// TODO 自動生成されたメソッド・スタブ
		return 0;
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
	public CobolDecimal getDecimal() {
		// TODO 自動生成されたメソッド・スタブ
		return null;
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
		this.moveFrom(field.getBytes());
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
	 * 引数で与えらえられたデータからthisへの代入を行う
	 * @param field 代入元のデータ(byte[]型)
	 */
	@Override
	public void moveFrom(byte[] bytes) {
		if(bytes.length >= this.size) {
			this.dataStorage.setBytes(bytes, this.size);
		} else {
			this.dataStorage.setBytes(bytes, bytes.length);
			this.dataStorage.fillBytes(bytes.length, (byte) 0x20, this.size);
		}
	}

	/**
	 * 引数で与えらえられたデータからthisへの代入を行う
	 * @param field 代入元のデータ(String型)
	 */
	@Override
	public void moveFrom(String string) {
		// TODO 自動生成されたメソッド・スタブ
		byte[] bytes;
		// TODO 自動生成されたメソッ:/ド・スタブ
		try {
			bytes = string.getBytes("SJIS");
		} catch (UnsupportedEncodingException e) {
			// TODO 自動生成された catch ブロック
			// TODO ログの対応
			e.printStackTrace();
			throw new CobolRuntimeException(CobolRuntimeException.COBOL_FITAL_ERROR, "エンコードエラー");
		}

		this.moveFrom(bytes);
	}

	/**
	 * 引数で与えらえられたデータからthisへの代入を行う
	 * @param field 代入元のデータ(int型)
	 */
	@Override
	public void moveFrom(int number) {
		// TODO 自動生成されたメソッド・スタブ

	}

	/**
	 * 引数で与えらえられたデータからthisへの代入を行う
	 * @param field 代入元のデータ(double型)
	 */
	@Override
	public void moveFrom(double number) {
		// TODO 自動生成されたメソッド・スタブ

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
	 * 実装しないメソッド
	 */
	public int addPackedInt(int n) {
		throw new CobolRuntimeException(0, "実装しないコード");
	}
}
