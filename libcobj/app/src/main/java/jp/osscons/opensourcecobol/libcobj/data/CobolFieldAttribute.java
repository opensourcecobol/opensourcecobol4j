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

/**
 * COBOL変数の属性情報を保持するクラス.<br>
 * 変数の種別(数値・英数字・日本語など)・桁数・スケール・各種フラグ・PICTURE句の文字列を保持する.
 * 種別とフラグは{@code COB_TYPE_*}・{@code COB_FLAG_*}定数で表現される.
 */
public class CobolFieldAttribute {

    /* field types */

    /** 不明な種別を表す変数種別. */
    public static final int COB_TYPE_UNKNOWN = 0x00;

    /** 集団項目を表す変数種別. */
    public static final int COB_TYPE_GROUP = 0x01;

    /** 真理値項目を表す変数種別. */
    public static final int COB_TYPE_BOOLEAN = 0x02;

    /** 数値項目を表す変数種別(数値種別共通のビットを含む). */
    public static final int COB_TYPE_NUMERIC = 0x10;

    /** 表示用数値項目(USAGE DISPLAY)を表す変数種別. */
    public static final int COB_TYPE_NUMERIC_DISPLAY = 0x10;

    /** 2進数値項目(USAGE COMP/BINARY)を表す変数種別. */
    public static final int COB_TYPE_NUMERIC_BINARY = 0x11;

    /** パック10進数値項目(USAGE COMP-3)を表す変数種別. */
    public static final int COB_TYPE_NUMERIC_PACKED = 0x12;

    /** 単精度浮動小数点数値項目(USAGE COMP-1)を表す変数種別. */
    public static final int COB_TYPE_NUMERIC_FLOAT = 0x13;

    /** 倍精度浮動小数点数値項目(USAGE COMP-2)を表す変数種別. */
    public static final int COB_TYPE_NUMERIC_DOUBLE = 0x14;

    /** 編集数値項目を表す変数種別. */
    public static final int COB_TYPE_NUMERIC_EDITED = 0x24;

    /** 英数字項目(PIC X)を表す変数種別. */
    public static final int COB_TYPE_ALPHANUMERIC = 0x21;

    /** ALL指定の英数字項目を表す変数種別. */
    public static final int COB_TYPE_ALPHANUMERIC_ALL = 0x22;

    /** 英数字編集項目を表す変数種別. */
    public static final int COB_TYPE_ALPHANUMERIC_EDITED = 0x23;

    /** 日本語項目(PIC N)を表す変数種別. */
    public static final int COB_TYPE_NATIONAL = 0x40;

    /** 日本語編集項目を表す変数種別. */
    public static final int COB_TYPE_NATIONAL_EDITED = 0x41;

    /** ALL指定の日本語項目を表す変数種別. */
    public static final int COB_TYPE_NATIONAL_ALL = 0x42;

    /* field flags */

    /** フラグが何も指定されていないことを表す値. */
    public static final int COB_FLAG_NOT_SPECIFIED = 0x00;

    /** 符号を持つ数値であることを表すフラグ. */
    public static final int COB_FLAG_HAVE_SIGN = 0x01;

    /** 符号を独立した桁として保持する(SIGN SEPARATE)ことを表すフラグ. */
    public static final int COB_FLAG_SIGN_SEPARATE = 0x02;

    /** 符号を先頭桁に保持する(SIGN LEADING)ことを表すフラグ. */
    public static final int COB_FLAG_SIGN_LEADING = 0x04;

    /** 値が0のとき空白で表示する(BLANK WHEN ZERO)ことを表すフラグ. */
    public static final int COB_FLAG_BLANK_ZERO = 0x08;

    /** 右寄せ(JUSTIFIED RIGHT)であることを表すフラグ. */
    public static final int COB_FLAG_JUSTIFIED = 0x10;

    /** 2進数値のバイト順を入れ替える(バイトスワップする)ことを表すフラグ. */
    public static final int COB_FLAG_BINARY_SWAP = 0x20;

    /** 2進数値を実際のバイト幅(BINARY-C-LONG等)で扱うことを表すフラグ. */
    public static final int COB_FLAG_REAL_BINARY = 0x40;

    /** ポインタ項目であることを表すフラグ. */
    public static final int COB_FLAG_IS_POINTER = 0x80;

    /** 変数種別 */
    private int type;

    /** 数値の時,桁数を示す */
    private int digits;

    /** 数値の時,スケールを示す */
    private int scale;

    /** 様々なフラグ */
    private int flags;

    /** PICTURE句の文字列 */
    private String pic;

    /**
     * コンストラクタ
     *
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
     * コピーコンストラクタ.別の属性オブジェクトの各値をコピーして新しいオブジェクトを生成する.
     *
     * @param other コピー元の属性オブジェクト
     */
    public CobolFieldAttribute(CobolFieldAttribute other) {
        this.type = other.type;
        this.digits = other.digits;
        this.scale = other.scale;
        this.flags = other.flags;
        this.pic = other.pic;
    }

    /**
     * 変数種別を取得する
     *
     * @return このオブジェクトの変数種別
     */
    public int getType() {
        return type;
    }

    /**
     * 変数種別を取得する
     *
     * @param type このオブジェクトに設定する変数種別
     */
    public void setType(int type) {
        this.type = type;
    }

    /**
     * 桁数を取得する
     *
     * @return このオブジェクトの桁数
     */
    public int getDigits() {
        return digits;
    }

    /**
     * 桁数を設定する
     *
     * @param digits このオブジェクトに設定する桁数
     */
    public void setDigits(int digits) {
        this.digits = digits;
    }

    /**
     * このオブジェクトのスケールを取得する
     *
     * @return このオブジェクトのスケール
     */
    public int getScale() {
        return scale;
    }

    /**
     * このオブジェクトのスケールを設定する
     *
     * @param scale このオブジェクトに設定するスケール
     */
    public void setScale(int scale) {
        this.scale = scale;
    }

    /**
     * このオブジェクトのフラグを設定する
     *
     * @return このオブジェクトのフラグ
     */
    public int getFlags() {
        return this.flags;
    }

    /**
     * このオブジェクトのフラグを設定する
     *
     * @param flags このオブジェクトに設定するフラグ
     */
    public void setFlags(int flags) {
        this.flags = flags;
    }

    /**
     * このオブジェクトのPICTURE文字列を取得する
     *
     * @return このオブジェクトのPICTURE文字列
     */
    public String getPic() {
        return pic;
    }

    /**
     * このオブジェクトのPICTURE文字列を設定する
     *
     * @param pic このオブジェクトに設定するPICTURE文字列
     */
    public void setPic(String pic) {
        this.pic = pic;
    }

    /**
     * 変数種別が不明な種別かどうかを判定する.
     *
     * @return 不明な種別であればtrue
     */
    public boolean isTypeUnknown() {
        return type == COB_TYPE_UNKNOWN;
    }

    /**
     * 変数種別が集団項目かどうかを判定する.
     *
     * @return 集団項目であればtrue
     */
    public boolean isTypeGroup() {
        return type == COB_TYPE_GROUP;
    }

    /**
     * 変数種別が真理値項目かどうかを判定する.
     *
     * @return 真理値項目であればtrue
     */
    public boolean isTypeBoolean() {
        return type == COB_TYPE_BOOLEAN;
    }

    /**
     * 変数種別が数値項目かどうかを判定する.
     *
     * @return 数値項目であればtrue
     */
    public boolean isTypeNumeric() {
        return (type & COB_TYPE_NUMERIC) > 0;
    }

    /**
     * 変数種別が表示用数値項目(USAGE DISPLAY)かどうかを判定する.
     *
     * @return 表示用数値項目であればtrue
     */
    public boolean isTypeNumericDisplay() {
        return type == COB_TYPE_NUMERIC_DISPLAY;
    }

    /**
     * 変数種別が2進数値項目(USAGE COMP/BINARY)かどうかを判定する.
     *
     * @return 2進数値項目であればtrue
     */
    public boolean isTypeNumericBinary() {
        return type == COB_TYPE_NUMERIC_BINARY;
    }

    /**
     * 変数種別がパック10進数値項目(USAGE COMP-3)かどうかを判定する.
     *
     * @return パック10進数値項目であればtrue
     */
    public boolean isTypeNumericPacked() {
        return type == COB_TYPE_NUMERIC_PACKED;
    }

    /**
     * 変数種別が単精度浮動小数点数値項目(USAGE COMP-1)かどうかを判定する.
     *
     * @return 単精度浮動小数点数値項目であればtrue
     */
    public boolean isTypeNumericFloat() {
        return type == COB_TYPE_NUMERIC_FLOAT;
    }

    /**
     * 変数種別が倍精度浮動小数点数値項目(USAGE COMP-2)かどうかを判定する.
     *
     * @return 倍精度浮動小数点数値項目であればtrue
     */
    public boolean isTypeNumericDouble() {
        return type == COB_TYPE_NUMERIC_DOUBLE;
    }

    /**
     * 変数種別が編集数値項目かどうかを判定する.
     *
     * @return 編集数値項目であればtrue
     */
    public boolean isTypeNumericEdited() {
        return type == COB_TYPE_NUMERIC_EDITED;
    }

    /**
     * 変数種別が英数字項目(PIC X)かどうかを判定する.
     *
     * @return 英数字項目であればtrue
     */
    public boolean isTypeAlphanum() {
        return type == COB_TYPE_ALPHANUMERIC;
    }

    /**
     * 変数種別がALL指定の英数字項目かどうかを判定する.
     *
     * @return ALL指定の英数字項目であればtrue
     */
    public boolean isTypeAlphanumAll() {
        return type == COB_TYPE_ALPHANUMERIC_ALL;
    }

    /**
     * 変数種別が英数字編集項目かどうかを判定する.
     *
     * @return 英数字編集項目であればtrue
     */
    public boolean isTypeAlphanumEdited() {
        return type == COB_TYPE_ALPHANUMERIC_EDITED;
    }

    /**
     * 変数種別が日本語項目(PIC N)かどうかを判定する.
     *
     * @return 日本語項目であればtrue
     */
    public boolean isTypeNational() {
        return type == COB_TYPE_NATIONAL;
    }

    /**
     * 変数種別が日本語編集項目かどうかを判定する.
     *
     * @return 日本語編集項目であればtrue
     */
    public boolean isTypeNationalEdited() {
        return type == COB_TYPE_NATIONAL_EDITED;
    }

    /**
     * 変数種別がALL指定の日本語項目かどうかを判定する.
     *
     * @return ALL指定の日本語項目であればtrue
     */
    public boolean isTypeNationalAll() {
        return type == COB_TYPE_NATIONAL_ALL;
    }

    /**
     * 符号を持つ数値であるフラグが立っているかどうかを判定する.
     *
     * @return 符号を持つ数値であればtrue
     */
    public boolean isFlagHaveSign() {
        return (flags & COB_FLAG_HAVE_SIGN) != 0;
    }

    /**
     * 符号を独立した桁として保持する(SIGN SEPARATE)フラグが立っているかどうかを判定する.
     *
     * @return SIGN SEPARATEであればtrue
     */
    public boolean isFlagSignSeparate() {
        return (flags & COB_FLAG_SIGN_SEPARATE) != 0;
    }

    /**
     * 符号を先頭桁に保持する(SIGN LEADING)フラグが立っているかどうかを判定する.
     *
     * @return SIGN LEADINGであればtrue
     */
    public boolean isFlagSignLeading() {
        return (flags & COB_FLAG_SIGN_LEADING) != 0;
    }

    /**
     * 値が0のとき空白で表示する(BLANK WHEN ZERO)フラグが立っているかどうかを判定する.
     *
     * @return BLANK WHEN ZEROであればtrue
     */
    public boolean isFlagBlankZero() {
        return (flags & COB_FLAG_BLANK_ZERO) != 0;
    }

    /**
     * 右寄せ(JUSTIFIED RIGHT)フラグが立っているかどうかを判定する.
     *
     * @return 右寄せであればtrue
     */
    public boolean isFlagJustified() {
        return (flags & COB_FLAG_JUSTIFIED) != 0;
    }

    /**
     * 2進数値のバイト順を入れ替える(バイトスワップする)フラグが立っているかどうかを判定する.
     *
     * @return バイトスワップするのであればtrue
     */
    public boolean isFlagBinarySwap() {
        return (flags & COB_FLAG_BINARY_SWAP) != 0;
    }

    /**
     * 2進数値を実際のバイト幅で扱う(REAL BINARY)フラグが立っているかどうかを判定する.
     *
     * @return REAL BINARYであればtrue
     */
    public boolean isFlagRealBinary() {
        return (flags & COB_FLAG_REAL_BINARY) != 0;
    }

    /**
     * ポインタ項目であるフラグが立っているかどうかを判定する.
     *
     * @return ポインタ項目であればtrue
     */
    public boolean isFlagIsPointer() {
        return (flags & COB_FLAG_IS_POINTER) != 0;
    }
}
