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
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolRuntimeException;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolStopRunException;

/**
 * COBOLのINSPECT文を実装するクラス<br>
 * libcob/strings.cのcob_inspect_*系の関数に対応する。<br>
 * TALLYING(文字の計数)・REPLACING(文字の置換)・CONVERTING(文字の変換)の各機能を提供する。<br>
 * INSPECT文は{@link #init(AbstractCobolField, int)}で対象を設定し、{@link #start()}や
 * {@link #before(AbstractCobolField)}・{@link #after(AbstractCobolField)}で走査範囲(BEFORE/AFTER)を限定した上で、
 * {@link #all(AbstractCobolField, AbstractCobolField)}などで計数・置換を行い、最後に{@link #finish()}で確定する
 * という手順で実行する。
 */
public class CobolInspect {
    /** 一致する文字をすべて対象とする(INSPECT ... ALLに対応) */
    private static final int INSPECT_ALL = 0;

    /** 先頭から連続する一致のみを対象とする(INSPECT ... LEADINGに対応) */
    private static final int INSPECT_LEADING = 1;

    /** 最初に一致した1箇所のみを対象とする(INSPECT ... FIRSTに対応) */
    private static final int INSPECT_FIRST = 2;

    /** 末尾から連続する一致のみを対象とする(INSPECT ... TRAILINGに対応) */
    private static final int INSPECT_TRAILING = 3;

    /**
     * 実行単位(スレッド)ごとに保持する作業状態。<br>
     * 文の処理は複数のメソッド呼び出しにまたがって状態を共有するため、スレッドごとに独立させる。
     */
    private static final class State {
        /** INSPECTの対象となるフィールド */
        AbstractCobolField inspectVar;

        /** INSPECTの対象フィールドの記憶領域 */
        CobolDataStorage inspectData;

        /** 走査範囲の開始位置(0始まり) */
        int inspectStart;

        /** 走査範囲の終了位置(この位置は含まない) */
        int inspectEnd;

        /** 各バイトの処理状況を保持する配列(-1:未処理、1:計数済み、その他:置換後のバイト値) */
        int[] inspectMark = null;

        /** inspectMarkとして確保済みの領域サイズ */
        int lastsize = 0;

        /** INSPECTの対象フィールドのサイズ(バイト数) */
        int inspectSize;

        /** REPLACING(置換)モードかどうかを表す(0以外で置換、0で計数) */
        int inspectReplacing;

        /** 対象フィールドの元の符号(処理後に復元するために退避する) */
        int inspectSign;

        /** INSPECTの対象フィールドの控え */
        AbstractCobolField inspectVarCopy;
    }

    /** 現在のスレッドの作業状態。実行単位はスレッドごとに独立しているため、スレッドごとに保持する。 */
    private static final ThreadLocal<State> state = ThreadLocal.withInitial(State::new);

    /** 現在のスレッドに紐づく作業状態を破棄する。実行単位の終了時に呼び出す。 */
    public static void resetThreadState() {
        state.remove();
    }

    /**
     * 形象定数f1を、対象f2のサイズに合わせて繰り返し展開したフィールドを生成する。<br>
     * REPLACINGでALL形象定数を置換文字として用いる場合に、長さを一致させるために使用する。
     *
     * @param f1 展開元となる形象定数のフィールド
     * @param f2 展開後の長さの基準となるフィールド
     * @return f2と同じサイズに展開された英数字フィールド
     */
    private static AbstractCobolField figurative(AbstractCobolField f1, AbstractCobolField f2) {
        int size1 = 0;
        int size2 = f2.getSize();
        byte[] figptr = new byte[size2];
        int s = 0;
        for (int n = 0; n < size2; n++, s++) {
            figptr[s] = f1.getDataStorage().getByte(size1);
            size1++;
            if (size1 >= f1.getSize()) {
                size1 = 0;
            }
        }
        return CobolFieldFactory.makeCobolField(
                size2,
                new CobolDataStorage(figptr),
                new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null));
    }

    /**
     * ALL/LEADING/FIRST/TRAILINGに共通する、文字列の照合・計数・置換処理を行う。<br>
     * 指定された種別に応じて走査範囲内でf2を検索し、計数モードでは出現回数をf1に加算し、
     * 置換モードではinspectMarkに置換後のバイト値を記録する。
     *
     * @param f1 計数モードでは計数結果を加算するフィールド、置換モードでは置換後の文字を表すフィールド
     * @param f2 検索対象となる文字または文字列を表すフィールド
     * @param type 照合の種別({@link #INSPECT_ALL}・{@link #INSPECT_LEADING}・{@link #INSPECT_FIRST}・{@link #INSPECT_TRAILING})
     * @throws CobolStopRunException 処理中にSTOP RUNが実行された場合
     */
    private static void common(AbstractCobolField f1, AbstractCobolField f2, int type)
            throws CobolStopRunException {
        State st = state.get();
        if (f1 == null) {
            f1 = CobolConstant.low;
        }
        if (f2 == null) {
            f2 = CobolConstant.low;
        }

        int type1 = f1.getAttribute().getType();
        int type2 = f2.getAttribute().getType();

        if (type2 == CobolFieldAttribute.COB_TYPE_NATIONAL
                || type2 == CobolFieldAttribute.COB_TYPE_NATIONAL_EDITED) {
            if (f1 == CobolConstant.quote) {
                f1 = CobolConstant.zenQuote;
            } else if (f1 == CobolConstant.space) {
                f1 = CobolConstant.zenSpace;
            } else if (f1 == CobolConstant.zero) {
                f1 = CobolConstant.zenZero;
            }
        }
        if (type1 == CobolFieldAttribute.COB_TYPE_NATIONAL
                || type1 == CobolFieldAttribute.COB_TYPE_NATIONAL_EDITED) {
            if (f2 == CobolConstant.quote) {
                f2 = CobolConstant.zenQuote;
            } else if (f2 == CobolConstant.space) {
                f2 = CobolConstant.zenSpace;
            } else if (f2 == CobolConstant.zero) {
                f2 = CobolConstant.zenZero;
            }
        }
        if (st.inspectReplacing != 0 && f1.getSize() != f2.getSize()) {
            if (type1 == CobolFieldAttribute.COB_TYPE_ALPHANUMERIC_ALL) {
                f1 = figurative(f1, f2);
            } else {
                CobolRuntimeException.setException(CobolExceptionId.COB_EC_RANGE_INSPECT_SIZE);
                return;
            }
        }

        int mark = st.inspectStart;
        int len = st.inspectEnd - st.inspectStart;
        int n = 0;
        if (type == INSPECT_TRAILING) {
            for (int i = len - f2.getSize(); i >= 0; i--) {
                if (st.inspectData.memcmp(st.inspectStart + i, f2.getDataStorage(), f2.getSize())
                        == 0) {
                    int j;
                    for (j = 0; j < f2.getSize(); ++j) {
                        if (st.inspectMark[mark + i + j] != -1) {
                            break;
                        }
                    }
                    if (j == f2.getSize()) {
                        CobolDataStorage f1Storage = f1.getDataStorage();
                        for (j = 0; j < f2.getSize(); ++j) {
                            st.inspectMark[mark + i + j] =
                                    st.inspectReplacing != 0 ? f1Storage.getByte(j) : 1;
                        }
                        i -= f2.getSize() - 1;
                        n++;
                    }
                } else {
                    break;
                }
            }
        } else {
            for (int i = 0; i < (len - f2.getSize() + 1); ++i) {
                if (st.inspectData.memcmp(st.inspectStart + i, f2.getDataStorage(), f2.getSize())
                        == 0) {
                    int j;
                    for (j = 0; j < f2.getSize(); ++j) {
                        if (st.inspectMark[mark + i + j] != -1) {
                            break;
                        }
                    }
                    if (j == f2.getSize()) {
                        CobolDataStorage f1Storage = f1.getDataStorage();
                        for (j = 0; j < f2.getSize(); ++j) {
                            st.inspectMark[mark + i + j] =
                                    st.inspectReplacing != 0 ? f1Storage.getByte(j) : 1;
                        }
                        i += f2.getSize() - 1;
                        n++;
                        if (type == INSPECT_FIRST) {
                            break;
                        }
                    }
                } else if (type == INSPECT_LEADING) {
                    break;
                }
            }
        }

        if (n > 0 && st.inspectReplacing == 0) {
            f1.addInt(n);
        }
    }

    /**
     * libcob/strings.cのcob_inspect_initの実装。<br>
     * INSPECT文の処理を開始し、対象フィールドや処理状況を記録する各種状態を初期化する。
     *
     * @param var INSPECTの対象となるフィールド
     * @param replacing REPLACING(置換)を行う場合は0以外、TALLYING(計数)の場合は0
     */
    public static void init(AbstractCobolField var, int replacing) {
        State st = state.get();
        st.inspectVarCopy = var;
        st.inspectVar = st.inspectVarCopy;
        st.inspectSign = var.getSign();

        if (var.getAttribute().isTypeNumericDisplay()) {
            st.inspectVar.putSign(1);
        }

        st.inspectReplacing = replacing;
        st.inspectSize = var.getFieldSize();
        st.inspectData = var.getDataStorage();
        st.inspectStart = -1;
        st.inspectEnd = -1;
        if (st.inspectSize > st.lastsize) {
            st.inspectMark = new int[st.inspectSize];
            st.lastsize = st.inspectSize;
        }
        for (int i = 0; i < st.inspectSize; ++i) {
            st.inspectMark[i] = -1;
        }
        CobolRuntimeException.setException(0);
    }

    /**
     * libcob/strings.cのcob_inspect_startの実装。<br>
     * 走査範囲を対象フィールドの全体(先頭から末尾まで)に設定する。
     */
    public static void start() {
        State st = state.get();
        st.inspectStart = 0;
        st.inspectEnd = st.inspectSize;
    }

    /**
     * libcob/strings.cのcob_inspect_beforeの実装。<br>
     * BEFORE INITIAL句に対応し、指定された区切り文字が最初に現れる位置で走査範囲の終了位置を制限する。
     *
     * @param str 走査範囲の終端を定める区切り文字(BEFORE INITIALの基準)を表すフィールド
     */
    public static void before(AbstractCobolField str) {
        State st = state.get();
        CobolDataStorage p2 = null;
        int fig = 0;

        switch (str.getAttribute().getType()) {
            case CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY:
                CobolDataStorage data = str.getDataStorage();
                int firstIndex = str.getFirstDataIndex();
                int size = str.getFieldSize();
                int n = 0;
                int i = 0;
                while (size > 1
                        && (data.getByte(firstIndex + i) == (byte) '0'
                                || data.getByte(firstIndex + i) == (byte) 0x70)) {
                    size--;
                    i++;
                }
                while (size-- > 0) {
                    int b = data.getByte(firstIndex + i++);
                    n = n * 10 + (b >= 0x70 ? b - 0x70 : b - 0x30);
                    fig++;
                }
                p2 =
                        new CobolDataStorage(
                                String.format("%d", n).getBytes(AbstractCobolField.charSetSJIS));
                break;
            default:
                fig = str.getSize();
                p2 = str.getDataStorage();
                break;
        }

        for (int p = st.inspectStart; p < st.inspectEnd - fig + 1; ++p) {
            if (st.inspectData.getSubDataStorage(p).memcmp(p2, fig) == 0) {
                st.inspectEnd = p;
                break;
            }
        }
    }

    /**
     * libcob/strings.cのcob_inspect_afterの実装。<br>
     * AFTER INITIAL句に対応し、指定された区切り文字が最初に現れる位置の直後から走査範囲の開始位置を制限する。
     *
     * @param str 走査範囲の始端を定める区切り文字(AFTER INITIALの基準)を表すフィールド
     */
    public static void after(AbstractCobolField str) {
        State st = state.get();
        CobolDataStorage data = str.getDataStorage();
        int size = str.getSize();
        for (int p = st.inspectStart; p < st.inspectEnd - str.getSize() + 1; ++p) {
            if (st.inspectData.getSubDataStorage(p).memcmp(data, size) == 0) {
                st.inspectStart = p + size;
                return;
            }
        }
        st.inspectStart = st.inspectEnd;
    }

    /**
     * libcob/strings.cのcob_inspect_charactersの実装。<br>
     * INSPECT ... CHARACTERS句に対応し、走査範囲内の未処理のすべての文字を対象とする。<br>
     * 計数モードでは対象文字数をf1に加算し、置換モードでは対象範囲をf1の内容で置き換える。
     *
     * @param f1 計数モードでは計数結果を加算するフィールド、置換モードでは置換後の文字を表すフィールド
     * @throws CobolStopRunException 処理中にSTOP RUNが実行された場合
     */
    public static void characters(AbstractCobolField f1) throws CobolStopRunException {
        State st = state.get();
        int mark = st.inspectStart;
        int len = st.inspectEnd - st.inspectStart;
        if (st.inspectReplacing != 0) {
            for (int i = 0; i < len; ++i) {
                if (st.inspectMark[mark + i] == -1) {
                    CobolDataStorage data = f1.getDataStorage();
                    for (int j = 0; j < f1.getSize(); ++j) {
                        st.inspectMark[mark + i + j] = data.getByte(j);
                    }
                    i += f1.getSize() - 1;
                }
            }
        } else {
            int n = 0;
            for (int i = 0; i < len; ++i) {
                if (st.inspectMark[mark + i] == -1) {
                    st.inspectMark[mark + i] = 1;
                    n++;
                }
            }
            if (n > 0) {
                int type = st.inspectVar.getAttribute().getType();
                if (type == CobolFieldAttribute.COB_TYPE_NATIONAL
                        || type == CobolFieldAttribute.COB_TYPE_NATIONAL_EDITED) {
                    n = n / 2;
                }
                f1.addInt(n);
            }
        }
    }

    /**
     * libcob/strings.cのcob_inspect_allの実装。<br>
     * INSPECT ... ALL句に対応し、走査範囲内に現れるf2のすべての出現を対象に計数または置換を行う。
     *
     * @param f1 計数モードでは計数結果を加算するフィールド、置換モードでは置換後の文字を表すフィールド
     * @param f2 検索対象となる文字または文字列を表すフィールド
     * @throws CobolStopRunException 処理中にSTOP RUNが実行された場合
     */
    public static void all(AbstractCobolField f1, AbstractCobolField f2)
            throws CobolStopRunException {
        common(f1, f2, INSPECT_ALL);
    }

    /**
     * libcob/strings.cのcob_inspect_leadingの実装。<br>
     * INSPECT ... LEADING句に対応し、走査範囲の先頭から連続して現れるf2のみを対象に計数または置換を行う。
     *
     * @param f1 計数モードでは計数結果を加算するフィールド、置換モードでは置換後の文字を表すフィールド
     * @param f2 検索対象となる文字または文字列を表すフィールド
     * @throws CobolStopRunException 処理中にSTOP RUNが実行された場合
     */
    public static void leading(AbstractCobolField f1, AbstractCobolField f2)
            throws CobolStopRunException {
        common(f1, f2, INSPECT_LEADING);
    }

    /**
     * libcob/strings.cのcob_inspect_firstの実装。<br>
     * INSPECT ... FIRST句に対応し、走査範囲内で最初に現れたf2の1箇所のみを対象に計数または置換を行う。
     *
     * @param f1 計数モードでは計数結果を加算するフィールド、置換モードでは置換後の文字を表すフィールド
     * @param f2 検索対象となる文字または文字列を表すフィールド
     * @throws CobolStopRunException 処理中にSTOP RUNが実行された場合
     */
    public static void first(AbstractCobolField f1, AbstractCobolField f2)
            throws CobolStopRunException {
        common(f1, f2, INSPECT_FIRST);
    }

    /**
     * libcob/strings.cのcob_inspect_trailingの実装。<br>
     * INSPECT ... TRAILING句に対応し、走査範囲の末尾から連続して現れるf2のみを対象に計数または置換を行う。
     *
     * @param f1 計数モードでは計数結果を加算するフィールド、置換モードでは置換後の文字を表すフィールド
     * @param f2 検索対象となる文字または文字列を表すフィールド
     * @throws CobolStopRunException 処理中にSTOP RUNが実行された場合
     */
    public static void trailing(AbstractCobolField f1, AbstractCobolField f2)
            throws CobolStopRunException {
        common(f1, f2, INSPECT_TRAILING);
    }

    /**
     * libcob/strings.cのcob_inspect_convertingの実装。<br>
     * INSPECT ... CONVERTING句に対応し、走査範囲内でf1に含まれる各文字を、対応するf2の文字に変換する。
     *
     * @param f1 変換元となる文字の集合を表すフィールド
     * @param f2 変換先となる文字の集合を表すフィールド
     */
    public static void converting(AbstractCobolField f1, AbstractCobolField f2) {
        State st = state.get();
        int type1 = f1.getAttribute().getType();
        int len = st.inspectEnd - st.inspectStart;
        CobolDataStorage data = f2.getDataStorage();
        if (type1 == CobolFieldAttribute.COB_TYPE_NATIONAL
                || type1 == CobolFieldAttribute.COB_TYPE_NATIONAL_EDITED) {
            if (f2 == CobolConstant.quote) {
                f2 = CobolConstant.zenQuote;
            } else if (f2 == CobolConstant.space) {
                f2 = CobolConstant.zenSpace;
            } else if (f2 == CobolConstant.zero) {
                f2 = CobolConstant.zenZero;
            }
            for (int j = 0; j < f1.getSize(); j += 2) {
                for (int i = 0; i < len; i += 2) {
                    if (st.inspectMark[i] == -1
                            && st.inspectMark[i + 1] == -1
                            && f1.getDataStorage()
                                            .getSubDataStorage(j)
                                            .memcmp(
                                                    st.inspectData.getSubDataStorage(
                                                            st.inspectStart + i),
                                                    2)
                                    == 0) {
                        st.inspectData.setByte(st.inspectStart + i, data.getByte(0));
                        st.inspectData.setByte(st.inspectStart + i + 1, data.getByte(1));
                    }
                    st.inspectMark[i] = 1;
                    st.inspectMark[i + 1] = 1;
                }
            }
        } else {
            for (int j = 0; j < f1.getSize(); ++j) {
                for (int i = 0; i < len; ++i) {
                    if (st.inspectMark[i] == -1
                            && st.inspectData.getByte(st.inspectStart + i)
                                    == f1.getDataStorage().getByte(j)) {
                        if (f2 == CobolConstant.quote
                                || f2 == CobolConstant.space
                                || f2 == CobolConstant.zero) {
                            st.inspectData.setByte(st.inspectStart + i, data.getByte(0));
                        } else {
                            st.inspectData.setByte(st.inspectStart + i, data.getByte(j));
                        }
                        st.inspectMark[i] = 1;
                    }
                }
            }
        }
    }

    /**
     * libcob/strings.cのcob_inspect_finishの実装。<br>
     * INSPECT文の処理を確定する。置換モードの場合はinspectMarkに記録した置換結果を対象フィールドへ書き戻し、
     * 退避していた符号を復元する。
     */
    public static void finish() {
        State st = state.get();
        if (st.inspectReplacing != 0) {
            for (int i = 0; i < st.inspectSize; ++i) {
                if (st.inspectMark[i] != -1) {
                    st.inspectData.setByte(i, (byte) st.inspectMark[i]);
                }
            }
        }
        st.inspectVar.putSign(st.inspectSign);
    }
}
