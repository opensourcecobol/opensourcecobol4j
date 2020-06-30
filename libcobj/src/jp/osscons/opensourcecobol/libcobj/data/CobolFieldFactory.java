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

import jp.osscons.opensourcecobol.libcobj.exceptions.CobolRuntimeException;

/**
 * CobolFieldAttributeに設定された値に基づいて適切なAbstractCobolFieldの
 * サブクラスのコンストラクタを呼び出すmakeCobolFieldを実装するクラス
 */
public class CobolFieldFactory {
	/**
	 * attrに設定された値に応じて適切なAbstractCobolFieldクラスのサブクラスの
	 * コンストラクタを呼びだす。
	 * @param size データを保存するバイト領域の大きさ
	 * @param str データを保存する領域に書き込まれる初期データ
	 * @param attr 変数の様々な情報を格納するインスタンス
	 * @return attrの値に応じてインスタンス化されたAbstractCobolField型のデータ
	 */
	public static AbstractCobolField makeCobolField(int size, String str, CobolFieldAttribute attr) {
		return CobolFieldFactory.makeCobolField(size, new CobolDataStorage(str), attr);
	}

	/**
	 * attrに設定された値に応じて適切なAbstractCobolFieldクラスのサブクラスの
	 * コンストラクタを呼びだす。
	 * @param size データを保存するバイト領域の大きさ
	 * @param data データを保存する領域
	 * @param attr 変数の様々な情報を格納するインスタンス
	 * @return attrの値に応じてインスタンス化されたAbstractCobolField型のデータ
	 */
	public static AbstractCobolField makeCobolField(int size, CobolDataStorage data, CobolFieldAttribute attr) {
		switch(attr.getType()) {
		case CobolFieldAttribute.COB_TYPE_NUMERIC:
			return new CobolNumericField(size, data, attr);
		case CobolFieldAttribute.COB_TYPE_ALPHANUMERIC:
			return new CobolAlphanumericField(size, data, attr);
		case CobolFieldAttribute.COB_TYPE_NUMERIC_PACKED:
			return new CobolNumericPackedField(size, data, attr);
		case CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY:
			return new CobolNumericBinaryField(size, data, attr);
		case CobolFieldAttribute.COB_TYPE_NATIONAL:
			return new CobolNationalField(size, data, attr);
		case CobolFieldAttribute.COB_TYPE_GROUP:
			return new CobolGroupField(size, data, attr);
		default:
			throw new CobolRuntimeException(0, "未実装");
		}
	}

	/**
	 * 文字列型のデータ(CobolAlphanumericField)
	 * @param str 文字列データ
	 * @return strの値を保持するCobolAlphanumericFieldのインスタンス
	 */
	public static AbstractCobolField makeCobolField(String str) {
		byte[] bytes;
		try {
			bytes = str.getBytes("SJIS");
		} catch (UnsupportedEncodingException e) {
			// TODO ログの対応
			e.printStackTrace();
			throw new CobolRuntimeException(CobolRuntimeException.COBOL_FITAL_ERROR, "エンコードエラー");
		}
		CobolFieldAttribute attr =
				new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null);
		return new CobolAlphanumericField(bytes.length, new CobolDataStorage(bytes), attr);
	}

	/**
	 * 整数値型のデータ(CobolNumericField)
	 * @param n int型整数値
	 * @return nの値を保持するCobolNumericFieldのインスタンス
	 */
	public static AbstractCobolField makeCobolField(int n) {
		//対数にて数値の桁数を検出する
		int size = (int) (Math.log10(n) + 1);
		byte[] bytes = new byte[size];
		CobolFieldAttribute attr =
				new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_NUMERIC, size, 0, 0, null);
		AbstractCobolField field =  
				new CobolNumericField(size, new CobolDataStorage(bytes), attr);
		field.setInt(n);
		return field;
	}
}
