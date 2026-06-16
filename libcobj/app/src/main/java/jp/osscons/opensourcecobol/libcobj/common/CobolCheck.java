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
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolExceptionId;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolRuntimeException;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolStopRunException;

/**
 * COBOLの実行時境界チェックを行うクラス<br>
 * 表(OCCURS)の添字や、OCCURS DEPENDING ONの可変回数が範囲内にあるかを検査する。<br>
 * 範囲外の場合は{@link CobolExceptionId#COB_EC_BOUND_SUBSCRIPT}を設定し、実行時エラーを出力して実行を中止する。
 */
public class CobolCheck {
    /**
     * 表の添字が指定された範囲内にあるかを検査する。<br>
     * 範囲外の場合は例外を設定し、実行時エラーメッセージを出力して実行を中止する。
     *
     * @param i 検査対象の添字の値
     * @param min 添字の下限値
     * @param max 添字の上限値
     * @param name 表として参照しているデータ項目の名前(SJISのバイト列)
     * @param len データ項目名のバイト数
     * @throws CobolStopRunException 添字が範囲外で実行が中止された場合
     */
    public static void checkSubscript(int i, int min, int max, byte[] name, int len)
            throws CobolStopRunException {
        if (i < min || max < i) {
            CobolRuntimeException.setException(CobolExceptionId.COB_EC_BOUND_SUBSCRIPT);
            CobolUtil.runtimeError(
                    String.format(
                            "Subscript of '%s' out of bounds: %d",
                            new String(name, AbstractCobolField.charSetSJIS), i));
            CobolStopRunException.stopRunAndThrow(1);
        }
    }

    /**
     * 表の添字が指定された範囲内にあるかを検査する({@code long}型の添字を受け取るオーバーロード)。<br>
     * 添字をint型に変換した上で{@link #checkSubscript(int, int, int, byte[], int)}に委譲する。
     *
     * @param i 検査対象の添字の値
     * @param min 添字の下限値
     * @param max 添字の上限値
     * @param name 表として参照しているデータ項目の名前(SJISのバイト列)
     * @param len データ項目名のバイト数
     * @throws CobolStopRunException 添字が範囲外で実行が中止された場合
     */
    public static void checkSubscript(long i, int min, int max, byte[] name, int len)
            throws CobolStopRunException {
        CobolCheck.checkSubscript((int) i, min, max, name, len);
    }

    /**
     * 表の添字が指定された範囲内にあるかを検査する({@link CobolDataStorage}で名前を受け取るオーバーロード)。<br>
     * データ項目名をバイト列として取り出した上で{@link #checkSubscript(int, int, int, byte[], int)}に委譲する。
     *
     * @param i 検査対象の添字の値
     * @param min 添字の下限値
     * @param max 添字の上限値
     * @param name 表として参照しているデータ項目の名前を保持する記憶領域
     * @param len データ項目名のバイト数
     * @throws CobolStopRunException 添字が範囲外で実行が中止された場合
     */
    public static void checkSubscript(long i, int min, int max, CobolDataStorage name, int len)
            throws CobolStopRunException {
        CobolCheck.checkSubscript((int) i, min, max, name.getByteArrayRef(0, len), len);
    }

    /**
     * OCCURS DEPENDING ONの可変回数が指定された範囲内にあるかを検査する。<br>
     * 範囲外の場合は例外を設定し、実行時エラーメッセージを出力して実行を中止する。
     *
     * @param i 検査対象の可変回数の値
     * @param min 可変回数の下限値
     * @param max 可変回数の上限値
     * @param name OCCURS DEPENDING ONの対象となるデータ項目の名前
     * @throws CobolStopRunException 可変回数が範囲外で実行が中止された場合
     */
    public static void checkOdo(int i, int min, int max, String name) throws CobolStopRunException {
        if (i < min || max < i) {
            CobolRuntimeException.setException(CobolExceptionId.COB_EC_BOUND_SUBSCRIPT);
            CobolUtil.runtimeError(
                    String.format("OCCURS DEPENDING ON '%s' out of bounds: %d", name, i));
            CobolStopRunException.stopRunAndThrow(1);
        }
    }

    /**
     * OCCURS DEPENDING ONの可変回数が指定された範囲内にあるかを検査する(名前をバイト列で受け取るオーバーロード)。<br>
     * データ項目名をSJISの文字列に変換した上で{@link #checkOdo(int, int, int, String)}に委譲する。
     *
     * @param i 検査対象の可変回数の値
     * @param min 可変回数の下限値
     * @param max 可変回数の上限値
     * @param name OCCURS DEPENDING ONの対象となるデータ項目の名前(SJISのバイト列)
     * @throws CobolStopRunException 可変回数が範囲外で実行が中止された場合
     */
    public static void checkOdo(int i, int min, int max, byte[] name) throws CobolStopRunException {
        CobolCheck.checkOdo(i, min, max, new String(name, AbstractCobolField.charSetSJIS));
    }
}
