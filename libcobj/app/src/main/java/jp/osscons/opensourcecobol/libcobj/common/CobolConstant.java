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
package jp.osscons.opensourcecobol.libcobj.common;

import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldAttribute;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldFactory;

/**
 * COBOLの定数値(figurative constant)および実行時に用いる各種定数を保持するクラス<br>
 * ZERO・SPACE・HIGH-VALUE・LOW-VALUE・QUOTEなどの形象定数や、それらの全角(ZEN)版、
 * 各種バッファサイズ、10のべき乗表などをまとめて定義する。
 */
public class CobolConstant {
    /** 英数字のALL定数(SPACE・ZEROなどの形象定数)に用いる属性 */
    public static final CobolFieldAttribute allAttr =
            new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_ALPHANUMERIC_ALL, 0, 0, 0, null);

    /** 数字定数1に用いる属性(1桁の数字項目) */
    public static final CobolFieldAttribute oneAttr =
            new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_NUMERIC, 1, 0, 0, null);

    /** Shift_JISの全角ゼロ「０」のバイト列 */
    public static final byte[] SJZERO = {(byte) 0x82, (byte) 0x4f};

    /** Shift_JISの全角空白「　」のバイト列 */
    public static final byte[] SJSPC = {(byte) 0x81, (byte) 0x40};

    /** Shift_JISの全角空白(BLANK)のバイト列 */
    public static final byte[] SJBLK = {(byte) 0x81, (byte) 0x40};

    /** Shift_JISの全角引用符「”」のバイト列 */
    public static final byte[] SJQUOT = {(byte) 0x81, (byte) 0x68};

    /** Shift_JISの全角スラッシュ「／」のバイト列 */
    public static final byte[] SJSLAS = {(byte) 0x81, (byte) 0x5e};

    /** Shift_JISの全角文字1文字あたりのバイト数 */
    public static final int SJCSIZ = 2;

    /** 全角ゼロのバイト列({@link #SJZERO}の別名) */
    public static final byte[] ZENZERO = SJZERO;

    /** 全角空白のバイト列({@link #SJSPC}の別名) */
    public static final byte[] ZENSPC = SJSPC;

    /** 全角空白(BLANK)のバイト列({@link #SJBLK}の別名) */
    public static final byte[] ZENBLK = SJBLK;

    /** 全角引用符のバイト列({@link #SJQUOT}の別名) */
    public static final byte[] ZENQUOT = SJQUOT;

    /** 全角スラッシュのバイト列({@link #SJSLAS}の別名) */
    public static final byte[] ZENSLAS = SJSLAS;

    /** 全角文字1文字あたりのバイト数({@link #SJCSIZ}の別名) */
    public static final int ZENCSIZ = SJCSIZ;

    /** 形象定数ZEROを表すフィールド */
    public static final AbstractCobolField zero = CobolFieldFactory.makeCobolField(1, "0", allAttr);

    /** 形象定数SPACEを表すフィールド */
    public static final AbstractCobolField space =
            CobolFieldFactory.makeCobolField(1, " ", allAttr);

    /** 形象定数BLANKを表すフィールド */
    public static final AbstractCobolField blank =
            CobolFieldFactory.makeCobolField(1, " ", allAttr);

    /** 形象定数HIGH-VALUE(0xFF)を表すフィールド */
    public static final AbstractCobolField high =
            CobolFieldFactory.makeCobolField(1, CobolConstant.get0xFFStorage(), allAttr);

    /** 形象定数LOW-VALUE(0x00)を表すフィールド */
    public static final AbstractCobolField low = CobolFieldFactory.makeCobolField(1, "\0", allAttr);

    /** 形象定数QUOTEを表すフィールド */
    public static final AbstractCobolField quote =
            CobolFieldFactory.makeCobolField(1, "\"", allAttr);

    /** 数字定数1を表すフィールド */
    public static final AbstractCobolField one = CobolFieldFactory.makeCobolField(1, "1", oneAttr);

    /** 全角の形象定数ZEROを表すフィールド */
    public static final AbstractCobolField zenZero =
            CobolFieldFactory.makeCobolField(ZENCSIZ, new CobolDataStorage(ZENZERO), allAttr);

    /** 全角の形象定数SPACEを表すフィールド */
    public static final AbstractCobolField zenSpace =
            CobolFieldFactory.makeCobolField(ZENCSIZ, new CobolDataStorage(ZENSPC), allAttr);

    /** 全角の形象定数BLANKを表すフィールド */
    public static final AbstractCobolField zenBlank =
            CobolFieldFactory.makeCobolField(ZENCSIZ, new CobolDataStorage(ZENBLK), allAttr);

    /** 全角の形象定数QUOTEを表すフィールド */
    public static final AbstractCobolField zenQuote =
            CobolFieldFactory.makeCobolField(ZENCSIZ, new CobolDataStorage(ZENQUOT), allAttr);

    private static CobolDataStorage get0xFFStorage() {
        byte[] bytes = new byte[1];
        bytes[0] = (byte) 0xff;
        return new CobolDataStorage(bytes);
    }

    /** 10のべき乗の表(添字が指数に対応する。10^0〜10^18) */
    public static final long[] exp10LL = {
        1L,
        10L,
        100L,
        1000L,
        10000L,
        100000L,
        1000000L,
        10000000L,
        100000000L,
        1000000000L,
        10000000000L,
        100000000000L,
        1000000000000L,
        10000000000000L,
        100000000000000L,
        1000000000000000L,
        10000000000000000L,
        100000000000000000L,
        1000000000000000000L
    };

    /** 最小サイズのバッファのバイト数 */
    static final int COB_MINI_BUFF = 256;

    /** 小サイズのバッファのバイト数 */
    static final int COB_SMALL_BUFF = 1024;

    /** 標準サイズのバッファのバイト数 */
    static final int COB_NORMAL_BUFF = 2048;

    /** 中サイズのバッファのバイト数 */
    static final int COB_MEDIUM_BUFF = 8192;

    /** 大サイズのバッファのバイト数 */
    static final int COB_LARGE_BUFF = 16384;

    /** 最小サイズのバッファに格納できる最大文字数(終端分を除いた値) */
    static final int COB_MINI_MAX = COB_MINI_BUFF - 1;

    /** 小サイズのバッファに格納できる最大文字数(終端分を除いた値) */
    static final int COB_SMALL_MAX = COB_SMALL_BUFF - 1;

    /** 標準サイズのバッファに格納できる最大文字数(終端分を除いた値) */
    static final int COB_NORMAL_MAX = COB_NORMAL_BUFF - 1;

    /** 中サイズのバッファに格納できる最大文字数(終端分を除いた値) */
    static final int COB_MEDIUM_MAX = COB_MEDIUM_BUFF - 1;

    /** 大サイズのバッファに格納できる最大文字数(終端分を除いた値) */
    static final int COB_LARGE_MAX = COB_LARGE_BUFF - 1;

    /** 1つのフィールドに指定できる引数の最大数 */
    static final int COB_MAX_FIELD_PARAMS = 64;

    /** 初期化に関する致命的エラーを表すエラーコード */
    static final int COB_FERROR_INITIALIZED = 0;

    /** コンパイル対象のソースファイル名 */
    static final String COB_SOURCE_FILE = null;

    /** パッケージのバージョン番号 */
    static final int COB_PACKAGE_VERSION = 0;

    /** パッチレベル */
    static final int COB_PATCH_LEVEL = 0;

    // TODO 標準パスの設定
    /** ライブラリの検索パス */
    public static final String COB_LIBRARY_PATH = "";
}
