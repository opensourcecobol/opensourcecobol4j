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
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolExceptionId;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolExceptionInfo;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolRuntimeException;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolStopRunException;

/** UNSTRING文で使用される1個の区切り文字(DELIMITED BY指定)を保持するクラス。 */
class Dlm {
    /** 区切り文字を表すフィールド。 */
    public AbstractCobolField dlm;

    /** DELIMITED BY ALL指定の有無を表す値(0以外でALL指定あり)。 */
    public int all;
}

/**
 * COBOLのSTRING文およびUNSTRING文の処理を実装するクラス。<br>
 * libcobのstrings.cに対応する。<br>
 * STRING文は{@code stringInit}→{@code stringDelimited}/{@code stringAppend}を繰り返し→{@code stringFinish}の順に、
 * UNSTRING文は{@code unstringInit}→{@code unstringDelimited}/{@code unstringInto}を繰り返し→{@code unstringTallying}→{@code unstringFinish}の順に、
 * それぞれ静的メソッドを呼び出すことで実行される。<br>
 * 処理途中の状態(対象フィールド・オフセット・区切り文字など)はクラスの静的フィールドに保持される。
 *
 * @see AbstractCobolField
 */
public class CobolString {
    /** UNSTRING文の区切り文字リストの既定の確保数。 */
    private static final int DLM_DEFAULT_NUM = 8;

    /** 現在確保している区切り文字リスト({@link #dlmList})の要素数。 */
    private static int udlmCount = 0;

    /** STRING文の連結先(INTO)フィールド。 */
    private static AbstractCobolField stringDst;

    /** STRING文の文字位置(WITH POINTER)フィールド。指定されない場合はnull。 */
    private static AbstractCobolField stringPtr;

    /** STRING文の現在の区切り文字(DELIMITED BY)フィールド。指定されない場合はnull。 */
    private static AbstractCobolField stringDlm;

    /** STRING文の連結先フィールドの保持用コピー。 */
    private static AbstractCobolField stringDstCopy;

    /** STRING文の文字位置フィールドの保持用コピー。 */
    private static AbstractCobolField stringPtrCopy;

    /** STRING文の区切り文字フィールドの保持用コピー。 */
    private static AbstractCobolField stringDlmCopy;

    /** STRING文で次に書き込む連結先フィールド内のオフセット(バイト単位)。 */
    private static int stringOffset;

    /** UNSTRING文で使用する区切り文字(DELIMITED BY)のリスト。 */
    private static Dlm[] dlmList;

    /** UNSTRING文の分解対象(送り出し元)フィールド。 */
    private static AbstractCobolField unstringSrc;

    /** UNSTRING文の文字位置(WITH POINTER)フィールド。指定されない場合はnull。 */
    private static AbstractCobolField unstringPtr;

    /** UNSTRING文の分解対象フィールドの保持用コピー。 */
    private static AbstractCobolField unstringSrcCopy;

    /** UNSTRING文の文字位置フィールドの保持用コピー。 */
    private static AbstractCobolField unstringPtrCopy;

    /** UNSTRING文で次に読み込む分解対象フィールド内のオフセット(バイト単位)。 */
    private static int unstringOffset;

    /** UNSTRING文で分解した部分文字列の個数(TALLYING IN用のカウンタ)。 */
    private static int unstringCount;

    /** UNSTRING文で現在登録されている区切り文字の個数。 */
    private static int unstringNdlms;

    /**
     * STRING文の処理を開始する。連結先フィールドが省略された場合のオーバーロード。<br>
     * 連結先(dst)が指定されない場合に、整数のダミー引数を受け取って{@link #stringInit(AbstractCobolField,
     * AbstractCobolField)}へ委譲する。
     *
     * @param dst 連結先フィールドが省略されたことを表すダミー引数
     * @param ptr 文字位置(WITH POINTER)フィールド
     */
    public static void stringInit(int dst, AbstractCobolField ptr) {
        stringInit(null, ptr);
    }

    /**
     * STRING文の処理を開始する。文字位置フィールドが省略された場合のオーバーロード。<br>
     * 文字位置(ptr)が指定されない場合に、整数のダミー引数を受け取って{@link #stringInit(AbstractCobolField,
     * AbstractCobolField)}へ委譲する。
     *
     * @param dst 連結先(INTO)フィールド
     * @param ptr 文字位置フィールドが省略されたことを表すダミー引数
     */
    public static void stringInit(AbstractCobolField dst, int ptr) {
        stringInit(dst, null);
    }

    /**
     * STRING文の処理を開始する。連結先・文字位置の両フィールドが省略された場合のオーバーロード。<br>
     * いずれも整数のダミー引数を受け取って{@link #stringInit(AbstractCobolField, AbstractCobolField)}へ委譲する。
     *
     * @param dst 連結先フィールドが省略されたことを表すダミー引数
     * @param ptr 文字位置フィールドが省略されたことを表すダミー引数
     */
    public static void stringInit(int dst, int ptr) {
        stringInit(null, null);
    }

    /**
     * STRING文の処理を開始する。<br>
     * 連結先フィールドと文字位置フィールドを保持して内部状態を初期化し、文字位置が指定されている場合は
     * その値から書き込み開始オフセットを設定する。開始位置が連結先の範囲外であればオーバーフロー例外を設定する。<br>
     * 連結先が日本語項目の場合はオフセットを2倍(バイト単位)に補正する。
     *
     * @param dst 連結先(INTO)フィールド
     * @param ptr 文字位置(WITH POINTER)フィールド。省略時はnull
     */
    public static void stringInit(AbstractCobolField dst, AbstractCobolField ptr) {
        stringDstCopy = dst;
        stringDst = stringDstCopy;
        stringPtr = null;
        if (ptr != null) {
            stringPtrCopy = ptr;
            stringPtr = stringPtrCopy;
        }
        stringOffset = 0;
        CobolRuntimeException.setException(0);

        if (stringPtr != null) {
            stringOffset = stringPtr.getInt() - 1;
            if (stringOffset < 0 || stringOffset >= stringDst.getSize()) {
                CobolRuntimeException.setException(CobolExceptionId.COB_EC_OVERFLOW_STRING);
            }
        }

        switch (stringDst.getAttribute().getType()) {
            case CobolFieldAttribute.COB_TYPE_NATIONAL:
            case CobolFieldAttribute.COB_TYPE_NATIONAL_EDITED:
                stringOffset *= 2;
                break;
            default:
                break;
        }
    }

    /**
     * STRING文の区切り文字を設定する。区切り文字が省略された場合(DELIMITED BY SIZE相当)のオーバーロード。<br>
     * 整数のダミー引数を受け取って{@link #stringDelimited(AbstractCobolField)}へ委譲する。
     *
     * @param dlm 区切り文字が省略されたことを表すダミー引数
     */
    public static void stringDelimited(int dlm) {
        stringDelimited(null);
    }

    /**
     * STRING文の区切り文字(DELIMITED BY)を設定する。<br>
     * 以降の{@link #stringAppend(AbstractCobolField)}で連結する送り出し元から、この区切り文字までの部分を連結対象とする。<br>
     * 連結先が日本語項目の場合は、引用符・空白・ゼロの定数を対応する全角の定数に置き換える。
     *
     * @param dlm 区切り文字フィールド。区切り文字を指定しない(全体を連結する)場合はnull
     */
    public static void stringDelimited(AbstractCobolField dlm) {
        switch (stringDst.getAttribute().getType()) {
            case CobolFieldAttribute.COB_TYPE_NATIONAL:
            case CobolFieldAttribute.COB_TYPE_NATIONAL_EDITED:
                if (dlm == CobolConstant.quote) {
                    dlm = CobolConstant.zenQuote;
                } else if (dlm == CobolConstant.space) {
                    dlm = CobolConstant.zenSpace;
                } else if (dlm == CobolConstant.zero) {
                    dlm = CobolConstant.zenZero;
                }
                break;
            default:
                break;
        }
        stringDlm = null;
        if (dlm != null) {
            stringDlmCopy = dlm;
            stringDlm = stringDlmCopy;
        }
    }

    /**
     * STRING文の送り出し元を連結する。送り出し元が省略された場合のオーバーロード。<br>
     * 整数のダミー引数を受け取って{@link #stringAppend(AbstractCobolField)}へ委譲する。
     *
     * @param src 送り出し元フィールドが省略されたことを表すダミー引数
     */
    public static void stringAppend(int src) {
        stringAppend(null);
    }

    /**
     * STRING文の送り出し元フィールドを連結先へ連結する。<br>
     * 区切り文字({@link #stringDlm})が設定されている場合は、送り出し元の先頭から区切り文字が現れる手前までを連結対象とする。<br>
     * 連結先の残り領域に収まらない場合は収まる分だけ連結し、オーバーフロー例外を設定する。<br>
     * すでに例外が発生している場合は何も行わない。
     *
     * @param src 連結する送り出し元フィールド
     */
    public static void stringAppend(AbstractCobolField src) {
        if (CobolRuntimeException.code != 0) {
            return;
        }

        int srcSize = src.getSize();
        if (stringDlm != null) {
            int size = srcSize - stringDlm.getSize() + 1;
            CobolDataStorage srcData = src.getDataStorage();
            CobolDataStorage dlmData = stringDlm.getDataStorage();
            int dlmSize = stringDlm.getSize();
            for (int i = 0; i < size; ++i) {
                if (srcData.getSubDataStorage(i).memcmp(dlmData, dlmSize) == 0) {
                    srcSize = i;
                    break;
                }
            }
        }

        if (srcSize <= stringDst.getSize() - stringOffset) {
            stringDst
                    .getDataStorage()
                    .getSubDataStorage(stringOffset)
                    .memcpy(src.getDataStorage(), srcSize);
            stringOffset += srcSize;
        } else {
            // 連結先が日本語項目の場合、stringInitがバイト数との比較を2倍補正の前に行うため、
            // stringOffsetが連結先のバイト数を超えている(=残り領域が負になる)ことがある。
            // その場合は何も連結せずオーバーフローとして扱う。
            int size = stringDst.getSize() - stringOffset;
            if (size > 0) {
                stringDst
                        .getDataStorage()
                        .getSubDataStorage(stringOffset)
                        .memcpy(src.getDataStorage(), size);
            }
            stringOffset = stringDst.getSize();
            CobolRuntimeException.setException(CobolExceptionId.COB_EC_OVERFLOW_STRING);
        }
    }

    /**
     * STRING文の処理を終了する。<br>
     * 連結先が日本語項目の場合は最終オフセットをバイト単位から文字単位に戻し、文字位置(WITH POINTER)フィールドが
     * 指定されていれば、次の文字位置(オフセット+1)を書き戻す。
     */
    public static void stringFinish() {
        int type = stringDst.getAttribute().getType();
        if (type == CobolFieldAttribute.COB_TYPE_NATIONAL
                || type == CobolFieldAttribute.COB_TYPE_NATIONAL_EDITED) {

            stringOffset /= 2;
        }
        if (stringPtr != null) {
            stringPtr.setInt(stringOffset + 1);
        }
    }

    /**
     * UNSTRING文の処理を開始する。分解対象・文字位置の両フィールドが省略された場合のオーバーロード。<br>
     * いずれも整数のダミー引数を受け取って{@link #unstringInit(AbstractCobolField, AbstractCobolField, int)}へ委譲する。
     *
     * @param src 分解対象フィールドが省略されたことを表すダミー引数
     * @param ptr 文字位置フィールドが省略されたことを表すダミー引数
     * @param numDlm 区切り文字の個数
     */
    public static void unstringInit(int src, int ptr, int numDlm) {
        unstringInit(null, null, numDlm);
    }

    /**
     * UNSTRING文の処理を開始する。文字位置フィールドが省略された場合のオーバーロード。<br>
     * 文字位置(ptr)に整数のダミー引数を受け取って{@link #unstringInit(AbstractCobolField, AbstractCobolField,
     * int)}へ委譲する。
     *
     * @param src 分解対象(送り出し元)フィールド
     * @param ptr 文字位置フィールドが省略されたことを表すダミー引数
     * @param numDlm 区切り文字の個数
     */
    public static void unstringInit(AbstractCobolField src, int ptr, int numDlm) {
        unstringInit(src, null, numDlm);
    }

    /**
     * UNSTRING文の処理を開始する。分解対象フィールドが省略された場合のオーバーロード。<br>
     * 分解対象(src)に整数のダミー引数を受け取って{@link #unstringInit(AbstractCobolField, AbstractCobolField,
     * int)}へ委譲する。
     *
     * @param src 分解対象フィールドが省略されたことを表すダミー引数
     * @param ptr 文字位置(WITH POINTER)フィールド
     * @param numDlm 区切り文字の個数
     */
    public static void unstringInit(int src, AbstractCobolField ptr, int numDlm) {
        unstringInit(null, ptr, numDlm);
    }

    /**
     * UNSTRING文の処理を開始する。<br>
     * 分解対象フィールドと文字位置フィールドを保持して内部状態を初期化し、区切り文字を格納するリスト({@link #dlmList})を
     * 必要な個数だけ確保する。文字位置が指定されている場合はその値から読み込み開始オフセットを設定し、
     * 開始位置が分解対象の範囲外であればオーバーフロー例外を設定する。<br>
     * 分解対象が日本語項目の場合はオフセットを2倍(バイト単位)に補正する。
     *
     * @param src 分解対象(送り出し元)フィールド
     * @param ptr 文字位置(WITH POINTER)フィールド。省略時はnull
     * @param numDlm 区切り文字(DELIMITED BY)の個数
     */
    public static void unstringInit(AbstractCobolField src, AbstractCobolField ptr, int numDlm) {
        unstringSrcCopy = src;
        unstringSrc = unstringSrcCopy;
        unstringPtr = null;
        if (ptr != null) {
            unstringPtrCopy = ptr;
            unstringPtr = unstringPtrCopy;
        }

        unstringOffset = 0;
        unstringCount = 0;
        unstringNdlms = 0;
        CobolRuntimeException.setException(0);

        if (dlmList == null) {
            if (numDlm <= DLM_DEFAULT_NUM) {
                dlmList = new Dlm[DLM_DEFAULT_NUM];
                udlmCount = DLM_DEFAULT_NUM;
            } else {
                dlmList = new Dlm[numDlm];
                udlmCount = numDlm;
            }
        } else {
            if (numDlm > udlmCount) {
                dlmList = new Dlm[numDlm];
                udlmCount = numDlm;
            }
        }

        for (int i = 0; i < dlmList.length; ++i) {
            dlmList[i] = new Dlm();
        }

        if (unstringPtr != null) {
            unstringOffset = unstringPtr.getInt() - 1;
            if (unstringOffset < 0 || unstringOffset >= unstringSrc.getSize()) {
                CobolRuntimeException.setException(CobolExceptionId.COB_EC_OVERFLOW_UNSTRING);
            }
        }

        switch (unstringSrc.getAttribute().getType()) {
            case CobolFieldAttribute.COB_TYPE_NATIONAL:
            case CobolFieldAttribute.COB_TYPE_NATIONAL_EDITED:
                unstringOffset *= 2;
                break;
            default:
                break;
        }
    }

    /**
     * UNSTRING文の区切り文字を追加する。区切り文字が省略された場合のオーバーロード。<br>
     * 整数のダミー引数を受け取って{@link #unstringDelimited(AbstractCobolField, int)}へ委譲する。
     *
     * @param dlm 区切り文字が省略されたことを表すダミー引数
     * @param all DELIMITED BY ALL指定の有無を表す値
     */
    public static void unstringDelimited(int dlm, int all) {
        unstringDelimited(null, all);
    }

    /**
     * UNSTRING文の区切り文字(DELIMITED BY)を1個、区切り文字リストへ追加する。<br>
     * 分解対象が日本語項目の場合は、引用符・空白・ゼロの定数を対応する全角の定数に置き換える。
     * 全角空白の場合はさらに全角ブランクの区切り文字も追加する。<br>
     * allが0以外の場合はDELIMITED BY ALL指定であり、連続する区切り文字をまとめて1個の区切りとして扱う。
     *
     * @param dlm 区切り文字フィールド
     * @param all DELIMITED BY ALL指定の有無を表す値
     */
    public static void unstringDelimited(AbstractCobolField dlm, int all) {
        AbstractCobolField addDlm = null;

        switch (unstringSrc.getAttribute().getType()) {
            case CobolFieldAttribute.COB_TYPE_NATIONAL:
            case CobolFieldAttribute.COB_TYPE_NATIONAL_EDITED:
                if (dlm == CobolConstant.quote) {
                    dlm = CobolConstant.zenQuote;
                } else if (dlm == CobolConstant.space) {
                    dlm = CobolConstant.zenSpace;
                    addDlm = CobolConstant.zenBlank;
                } else if (dlm == CobolConstant.zero) {
                    dlm = CobolConstant.zenZero;
                }
                break;
            default:
                break;
        }

        dlmList[unstringNdlms].dlm = dlm;
        dlmList[unstringNdlms].all = all;
        unstringNdlms++;

        if (addDlm != null) {
            dlmList[unstringNdlms].dlm = addDlm;
            dlmList[unstringNdlms].all = all;
            unstringNdlms++;
        }
    }

    /**
     * UNSTRING文の分解結果を受け取り側へ格納する。文字数(COUNT IN)が省略された場合のオーバーロード。<br>
     * 整数のダミー引数を受け取って{@link #unstringInto(AbstractCobolField, AbstractCobolField,
     * AbstractCobolField)}へ委譲する。
     *
     * @param dst 受け取り側(INTO)フィールド
     * @param dlm 区切り文字を格納する(DELIMITER IN)フィールド
     * @param cnt 文字数フィールドが省略されたことを表すダミー引数
     */
    public static void unstringInto(AbstractCobolField dst, AbstractCobolField dlm, int cnt) {
        unstringInto(dst, dlm, null);
    }

    /**
     * UNSTRING文の分解結果を受け取り側へ格納する。区切り文字格納フィールドが省略された場合のオーバーロード。<br>
     * 整数のダミー引数を受け取って{@link #unstringInto(AbstractCobolField, AbstractCobolField,
     * AbstractCobolField)}へ委譲する。
     *
     * @param dst 受け取り側(INTO)フィールド
     * @param dlm 区切り文字格納フィールドが省略されたことを表すダミー引数
     * @param cnt 文字数を格納する(COUNT IN)フィールド
     */
    public static void unstringInto(AbstractCobolField dst, int dlm, AbstractCobolField cnt) {
        unstringInto(dst, null, cnt);
    }

    /**
     * UNSTRING文の分解結果を受け取り側へ格納する。区切り文字格納・文字数の両フィールドが省略された場合のオーバーロード。<br>
     * いずれも整数のダミー引数を受け取って{@link #unstringInto(AbstractCobolField, AbstractCobolField,
     * AbstractCobolField)}へ委譲する。
     *
     * @param dst 受け取り側(INTO)フィールド
     * @param dlm 区切り文字格納フィールドが省略されたことを表すダミー引数
     * @param cnt 文字数フィールドが省略されたことを表すダミー引数
     */
    public static void unstringInto(AbstractCobolField dst, int dlm, int cnt) {
        unstringInto(dst, null, null);
    }

    /**
     * UNSTRING文の分解結果を受け取り側へ格納する。受け取り側・区切り文字格納・文字数のすべてが省略された場合のオーバーロード。<br>
     * いずれも整数のダミー引数を受け取って{@link #unstringInto(AbstractCobolField, AbstractCobolField,
     * AbstractCobolField)}へ委譲する。
     *
     * @param dst 受け取り側フィールドが省略されたことを表すダミー引数
     * @param dlm 区切り文字格納フィールドが省略されたことを表すダミー引数
     * @param cnt 文字数フィールドが省略されたことを表すダミー引数
     */
    public static void unstringInto(int dst, int dlm, int cnt) {
        unstringInto(null, null, null);
    }

    /**
     * UNSTRING文の分解結果を受け取り側へ格納する。受け取り側・文字数フィールドが省略された場合のオーバーロード。<br>
     * いずれも整数のダミー引数を受け取って{@link #unstringInto(AbstractCobolField, AbstractCobolField,
     * AbstractCobolField)}へ委譲する。
     *
     * @param dst 受け取り側フィールドが省略されたことを表すダミー引数
     * @param dlm 区切り文字を格納する(DELIMITER IN)フィールド
     * @param cnt 文字数フィールドが省略されたことを表すダミー引数
     */
    public static void unstringInto(int dst, AbstractCobolField dlm, int cnt) {
        unstringInto(null, dlm, null);
    }

    /**
     * UNSTRING文の分解結果を受け取り側へ格納する。受け取り側・区切り文字格納フィールドが省略された場合のオーバーロード。<br>
     * いずれも整数のダミー引数を受け取って{@link #unstringInto(AbstractCobolField, AbstractCobolField,
     * AbstractCobolField)}へ委譲する。
     *
     * @param dst 受け取り側フィールドが省略されたことを表すダミー引数
     * @param dlm 区切り文字格納フィールドが省略されたことを表すダミー引数
     * @param cnt 文字数を格納する(COUNT IN)フィールド
     */
    public static void unstringInto(int dst, int dlm, AbstractCobolField cnt) {
        unstringInto(null, null, cnt);
    }

    /**
     * UNSTRING文の分解結果を受け取り側へ格納する。受け取り側フィールドが省略された場合のオーバーロード。<br>
     * 整数のダミー引数を受け取って{@link #unstringInto(AbstractCobolField, AbstractCobolField,
     * AbstractCobolField)}へ委譲する。
     *
     * @param dst 受け取り側フィールドが省略されたことを表すダミー引数
     * @param dlm 区切り文字を格納する(DELIMITER IN)フィールド
     * @param cnt 文字数を格納する(COUNT IN)フィールド
     */
    public static void unstringInto(int dst, AbstractCobolField dlm, AbstractCobolField cnt) {
        unstringInto(null, dlm, cnt);
    }

    /**
     * UNSTRING文の分解結果を1個の受け取り側フィールドへ格納する。<br>
     * 現在のオフセットから分解対象を走査し、登録された区切り文字のいずれかが見つかればその手前までを、
     * 見つからなければ残り全体を受け取り側へ転記する。区切り文字が登録されていない場合は受け取り側の桁数分を転記する。<br>
     * ALL指定の区切り文字では、連続する同一の区切り文字をまとめて読み飛ばす。<br>
     * 区切り文字格納フィールドが指定されていれば、一致した区切り文字(なければ数字項目はゼロ、それ以外は空白)を転記する。<br>
     * 文字数フィールドが指定されていれば、転記した文字数(日本語項目では文字単位に換算)を設定する。<br>
     * すでに例外が発生している場合、または読み込み位置が分解対象の範囲外の場合は何も行わない。
     *
     * @param dst 受け取り側(INTO)フィールド
     * @param dlm 区切り文字を格納する(DELIMITER IN)フィールド。省略時はnull
     * @param cnt 転記した文字数を格納する(COUNT IN)フィールド。省略時はnull
     */
    public static void unstringInto(
            AbstractCobolField dst, AbstractCobolField dlm, AbstractCobolField cnt) {
        if (CobolExceptionInfo.code != 0) {
            return;
        }
        if (unstringOffset >= unstringSrc.getSize()) {
            return;
        }

        CobolDataStorage srcData = unstringSrc.getDataStorage();
        int start = unstringOffset;
        int matchSize = 0;
        int dlmSize = 0;
        CobolDataStorage dlmData = null;
        if (unstringNdlms == 0) {
            matchSize = Math.min(dst.getFieldSize(), unstringSrc.getSize() - unstringOffset);
            dst.moveFrom(
                    CobolFieldFactory.makeCobolField(
                            matchSize,
                            srcData.getSubDataStorage(unstringOffset),
                            new CobolFieldAttribute(
                                    CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null)));
            unstringOffset += matchSize;
        } else {
            int srSize = unstringSrc.getSize();
            int s = srSize;
            boolean brkpt = false;
            for (int p = start; p < s; ++p) {
                for (int i = 0; i < unstringNdlms; ++i) {
                    int dlsize = dlmList[i].dlm.getSize();
                    CobolDataStorage dp = dlmList[i].dlm.getDataStorage();
                    if (p + dlsize > s) {
                        continue;
                    }
                    if (srcData.getSubDataStorage(p).memcmp(dp, dlsize) == 0) {
                        matchSize = (p - start);
                        dst.moveFrom(
                                CobolFieldFactory.makeCobolField(
                                        matchSize,
                                        srcData.getSubDataStorage(start),
                                        new CobolFieldAttribute(
                                                CobolFieldAttribute.COB_TYPE_ALPHANUMERIC,
                                                0,
                                                0,
                                                0,
                                                null)));
                        unstringOffset += matchSize + dlsize;
                        dlmData = dp;
                        dlmSize = dlsize;
                        if (dlmList[i].all != 0) {
                            for (p += dlsize; p < s; p += dlsize) {
                                if (p + dlsize > s) {
                                    break;
                                }
                                if (srcData.getSubDataStorage(p).memcmp(dp, dlsize) != 0) {
                                    break;
                                }
                                unstringOffset += dlsize;
                            }
                        }
                        brkpt = true;
                        break;
                    }
                }
                switch (unstringSrc.getAttribute().getType()) {
                    case CobolFieldAttribute.COB_TYPE_NATIONAL:
                    case CobolFieldAttribute.COB_TYPE_NATIONAL_EDITED:
                        p++;
                        break;
                    default:
                        break;
                }

                if (brkpt) {
                    break;
                }
            }
            if (!brkpt) {
                matchSize = unstringSrc.getSize() - unstringOffset;
                dst.moveFrom(
                        CobolFieldFactory.makeCobolField(
                                matchSize,
                                srcData.getSubDataStorage(start),
                                new CobolFieldAttribute(
                                        CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null)));
                unstringOffset = unstringSrc.getSize();
                dlmData = null;
            }
        }
        unstringCount++;

        if (dlm != null) {
            if (dlmData != null) {
                dlm.moveFrom(
                        CobolFieldFactory.makeCobolField(
                                dlmSize,
                                dlmData,
                                new CobolFieldAttribute(
                                        CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null)));
            } else if (dlm.getAttribute().isTypeNumeric()) {
                dlm.moveFrom(CobolConstant.zero);
            } else {
                dlm.moveFrom(CobolConstant.space);
            }
        }

        switch (unstringSrc.getAttribute().getType()) {
            case CobolFieldAttribute.COB_TYPE_NATIONAL:
            case CobolFieldAttribute.COB_TYPE_NATIONAL_EDITED:
                matchSize /= 2;
                break;
            default:
                break;
        }

        if (cnt != null) {
            cnt.setInt(matchSize);
        }
    }

    /**
     * UNSTRING文のTALLYING IN処理。集計先フィールドが省略された場合のオーバーロード。<br>
     * 整数のダミー引数を受け取り、何も行わない。
     *
     * @param f 集計先フィールドが省略されたことを表すダミー引数
     * @throws CobolStopRunException 実行が停止された場合
     */
    public static void unstringTallying(int f) throws CobolStopRunException {}

    /**
     * UNSTRING文のTALLYING IN処理を行う。<br>
     * これまでに分解した部分文字列の個数({@link #unstringCount})を、集計先フィールドへ加算する。
     *
     * @param f 分解した部分文字列の個数を加算する(TALLYING IN)フィールド
     * @throws CobolStopRunException 実行が停止された場合
     */
    public static void unstringTallying(AbstractCobolField f) throws CobolStopRunException {
        f.addInt(unstringCount);
    }

    /**
     * UNSTRING文の処理を終了する。<br>
     * 分解対象にまだ読み込んでいない部分が残っている場合はオーバーフロー例外を設定する。<br>
     * 分解対象が日本語項目の場合は最終オフセットをバイト単位から文字単位に戻し、文字位置(WITH POINTER)フィールドが
     * 指定されていれば、次の文字位置(オフセット+1)を書き戻す。
     */
    public static void unstringFinish() {
        if (unstringOffset < unstringSrc.getSize()) {
            CobolRuntimeException.setException(CobolExceptionId.COB_EC_OVERFLOW_UNSTRING);
        }

        switch (unstringSrc.getAttribute().getType()) {
            case CobolFieldAttribute.COB_TYPE_NATIONAL:
            case CobolFieldAttribute.COB_TYPE_NATIONAL_EDITED:
                unstringOffset /= 2;
                break;
            default:
                break;
        }

        if (unstringPtr != null) {
            unstringPtr.setInt(unstringOffset + 1);
        }
    }
}
