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

/**
 * opensource COBOLのCOB_DATA_ATTRに対応するクラス
 *
 */
public class CobolFieldAttribute {
	//libcob/common.hに定義されている定数群

	/* field types */

	final public static int COB_TYPE_UNKNOWN = 0x00;
	final public static int COB_TYPE_GROUP = 0x01;
	final public static int COB_TYPE_BOOLEAN = 0x02;

	final public static int COB_TYPE_NUMERIC = 0x10;
	final public static int COB_TYPE_NUMERIC_DISPLAY = 0x10;
	final public static int COB_TYPE_NUMERIC_BINARY = 0x11;
	final public static int COB_TYPE_NUMERIC_PACKED = 0x12;
	final public static int COB_TYPE_NUMERIC_FLOAT = 0x13;
	final public static int COB_TYPE_NUMERIC_DOUBLE = 0x14;
	final public static int COB_TYPE_NUMERIC_EDITED = 0x24;

	final public static int COB_TYPE_ALPHANUMERIC = 0x21;
	final public static int COB_TYPE_ALPHANUMERIC_ALL = 0x22;
	final public static int COB_TYPE_ALPHANUMERIC_EDITED = 0x23;

	final public static int COB_TYPE_NATIONAL = 0x40;
	final public static int COB_TYPE_NATIONAL_EDITED = 0x41;
	final public static int COB_TYPE_NATIONAL_ALL = 0x42;

	/* field flags */

	final public static int COB_FLAG_HAVE_SIGN = 0x01;
	final public static int COB_FLAG_SIGN_SEPARATE = 0x02;
	final public static int COB_FLAG_SIGN_LEADING = 0x04;
	final public static int COB_FLAG_BLANK_ZERO = 0x08;
	final public static int COB_FLAG_JUSTIFIED = 0x10;
	final public static int COB_FLAG_BINARY_SWAP = 0x20;
	final public static int COB_FLAG_REAL_BINARY = 0x40;
	final public static int COB_FLAG_IS_POINTER = 0x80;


	/**
	 * 変数の種類
	 */
	private int type;
	/**
	 * 数値の時,桁数を示す
	 */
	private int digits;
	/**
	 * 数値の時,スケールを示す
	 */
	private int scale;
	/**
	 * 様々なフラグ
	 */
	private int flags;
	/**
	 * PICTURE句の文字列
	 */
	private String pic;

	/**
	 * コンストラクタ
	 * @param type フィールドの種別
	 * @param digits 数値型のとき,桁数を表す
	 * @param scale 数値型の時,スケールを表す
	 * @param flags 各種フラグ
	 * @param pic PIC文字列
	 */
	public CobolFieldAttribute(int type, int digits, int scale, int flags, String pic) {
		this.type = type;
		this.digits = digits;
		this.scale = scale;
		this.flags = flags;
		this.pic = pic;
	}

	/**
	 * this.typeのgetter
	 * @return this.type
	 */
	public int getType() {
		return type;
	}

	/**
	 * this.typeのsetter
	 * @param type this.typeに設定する値
	 */
	public void setType(int type) {
		this.type = type;
	}

	/**
	 * this.digitsのgetter
	 * @return this.digits
	 */
	public int getDigits() {
		return digits;
	}

	/**
	 * this.digitsのsetter
	 * @param digits this.digitsに設定する値
	 */
	public void setDigits(int digits) {
		this.digits = digits;
	}

	/**
	 * this.scaleの getter
	 * @return this.scale
	 */
	public int getScale() {
		return scale;
	}

	/**
	 * this.scaleのsetter
	 * @param scale this.scaleに設定する値
	 */
	public void setScale(int scale) {
		this.scale = scale;
	}

	/**
	 * this.flagのgetter
	 * @return this.flag
	 */
	public int getFlags() {
		return this.flags;
	}

	/**
	 * this.flagのsetter
	 * @param flags this.flagに設定する値
	 */
	public void setFlags(int flags) {
		this.flags = flags;
	}

	/**
	 * this.picのgetter
	 * @return this.pic
	 */
	public String getPic() {
		return pic;
	}

	/**
	 * this.picのsetter
	 * @param pic this.picに設定する値
	 */
	public void setPic(String pic) {
		this.pic = pic;
	}

	public boolean isTypeUnknown() {
		return type == COB_TYPE_UNKNOWN;
	}

	public boolean isTypeGroup() {
		return type == COB_TYPE_GROUP;
	}

	public boolean isTypeBoolean() {
		return type == COB_TYPE_BOOLEAN;
	}

	/**
	 * 数値型のデータかを判定する
	 * @return DISPLAY,COMP-3等の時true,それ以外はfalse
	 */
	public boolean isTypeNumeric() {
		return (type & COB_TYPE_NUMERIC) > 0;
	}

	public boolean isTypeNumericDisplay() {
		return type == COB_TYPE_NUMERIC_DISPLAY;
	}

	public boolean isTypeNumericBinary() {
		return type == COB_TYPE_NUMERIC_BINARY;
	}

	public boolean isTypeNumericPacked() {
		return type == COB_TYPE_NUMERIC_PACKED;
	}

	public boolean isTypeNumericFloat() {
		return type == COB_TYPE_NUMERIC_FLOAT;
	}

	public boolean isTypeNumericDouble() {
		return type == COB_TYPE_NUMERIC_DOUBLE;
	}

	public boolean isTypeNumericEdited() {
		return type == COB_TYPE_NUMERIC_EDITED;
	}

	public boolean isTypeAlphanum() {
		return type == COB_TYPE_ALPHANUMERIC;
	}

	public boolean isTypeAlphanumAll() {
		return type == COB_TYPE_ALPHANUMERIC_ALL;
	}

	public boolean isTypeAlphanumEdited() {
		return type == COB_TYPE_ALPHANUMERIC_EDITED;
	}

	public boolean isTypeNational() {
		return type == COB_TYPE_NATIONAL;
	}

	public boolean isTypeNationalEdited() {
		return type == COB_TYPE_NATIONAL_EDITED;
	}

	public boolean isTypeNationalAll() {
		return type == COB_TYPE_NATIONAL_ALL;
	}

	public boolean isFlagHaveSign() {
		return (flags & COB_FLAG_HAVE_SIGN) != 0;
	}

	public boolean isFlagSignSeparate() {
		return (flags & COB_FLAG_SIGN_SEPARATE) != 0;
	}

	public boolean isFlagSignLeading() {
		return (flags & COB_FLAG_SIGN_LEADING) != 0;
	}

	public boolean isFlagBlankZero() {
		return (flags & COB_FLAG_BLANK_ZERO) != 0;
	}

	public boolean isFlagJustified() {
		return (flags & COB_FLAG_JUSTIFIED) != 0;
	}

	public boolean isFlagBinarySwap() {
		return (flags & COB_FLAG_BINARY_SWAP) != 0;
	}

	public boolean isFlagRealBinary() {
		return (flags & COB_FLAG_REAL_BINARY) != 0;
	}

	public boolean isFlagIsPointer() {
		return (flags & COB_FLAG_IS_POINTER) != 0;
	}
}