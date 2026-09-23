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

import java.math.BigDecimal;
import java.text.DateFormat;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Locale;
import java.util.Random;
import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.data.CobolDecimal;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldAttribute;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldFactory;
import jp.osscons.opensourcecobol.libcobj.data.CobolNationalField;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolExceptionId;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolRuntimeException;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolStopRunException;
import jp.osscons.opensourcecobol.libcobj.file.CobolFile;

/**
 * COBOLの組み込み関数(FUNCTION ...)を実装するクラス。<br>
 * GnuCOBOL/opensource COBOLのランタイムライブラリlibcobのintrinsic.cに対応する。<br>
 * ABS、MOD、FACTORIAL、SUM、MAX、MIN、NUMVAL、CURRENT-DATE、UPPER-CASE、LOWER-CASEなどの
 * 各組み込み関数を、{@code funcXxx}という名前の静的メソッドとして提供する。<br>
 * 各メソッドは計算結果を{@link AbstractCobolField}として返す。
 */
public class CobolIntrinsic {

    /** 各月初日までの通日(非うるう年)。インデックスは月(0〜12)。 */
    private static int[] normalDays = {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365};

    /** 各月初日までの通日(うるう年)。インデックスは月(0〜12)。 */
    private static int[] leapDays = {0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366};

    /** 各月の日数(非うるう年)。インデックスは月(0〜12)。 */
    private static int[] normalMonthDays = {0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

    /** 各月の日数(うるう年)。インデックスは月(0〜12)。 */
    private static int[] leapMonthDays = {0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

    /** 計算結果を保持する内部フィールドの個数(リングバッファの深さ)。 */
    private static final int DEPTH_LEVEL = 8;

    /** double型1個分のバイト数。 */
    private static final int sizeOfDouble = 8;

    /** 次に使用する{@link #calcField}のインデックス。 */
    private static int currEntry = 0;

    /** 直近に生成した計算結果フィールド。 */
    private static AbstractCobolField currField = null;

    /** 計算結果を保持する内部フィールドのリングバッファ。 */
    private static AbstractCobolField[] calcField = new AbstractCobolField[DEPTH_LEVEL];

    /** FUNCTION RANDOMで使用する擬似乱数生成器。 */
    private static Random random = new Random();

    /** ロケール関連の文字列を一時的に保持するバッファ。 */
    private static byte[] localeBuff;

    /** 文字列"00"のSJISバイト列(ファイルステータスの初期値などに使用)。 */
    private static final byte[] byteArray00 = "00".getBytes(AbstractCobolField.charSetSJIS);

    /**
     * libcob/intrinsicのmake_double_entryの実装。<br>
     * double型(COB_TYPE_NUMERIC_DOUBLE)の計算結果フィールドを新たに生成し、
     * {@link #currField}に設定する。
     */
    private static void makeDoubleEntry() {
        CobolDataStorage s = new CobolDataStorage(sizeOfDouble + 1);

        CobolFieldAttribute newAttr =
                new CobolFieldAttribute(
                        CobolFieldAttribute.COB_TYPE_NUMERIC_DOUBLE,
                        18,
                        9,
                        CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
                        null);
        AbstractCobolField newField = CobolFieldFactory.makeCobolField(sizeOfDouble, s, newAttr);

        calcField[currEntry] = newField;
        currField = newField;
        ++currEntry;
        if (currEntry >= DEPTH_LEVEL) {
            currEntry = 0;
        }
    }

    /**
     * libcob/intrinsicのmake_field_entryの実装。<br>
     * 指定したフィールドと同じサイズ・属性を持つ計算結果フィールドを新たに生成し、
     * {@link #currField}に設定する。
     *
     * @param f 生成するフィールドのサイズと属性の基となるフィールド
     */
    private static void makeFieldEntry(AbstractCobolField f) {
        AbstractCobolField newField =
                CobolFieldFactory.makeCobolField(
                        f.getSize(), new CobolDataStorage(f.getSize() + 1), f.getAttribute());
        calcField[currEntry] = newField;
        currField = calcField[currEntry];

        ++currEntry;
        if (currEntry >= DEPTH_LEVEL) {
            currEntry = 0;
        }
    }

    /**
     * 指定した西暦年がうるう年かどうかを判定する。
     *
     * @param year 判定対象の西暦年
     * @return うるう年であればtrue、そうでなければfalse
     */
    private static boolean isLeapYear(int year) {
        return ((year % 4 == 0 && year % 100 != 0) || (year % 400 == 0));
    }

    // libcob/intrinsicのcob_init_intrinsicの実装
    /**
     * 組み込み関数の計算結果フィールド用バッファを初期化する。<br>
     * {@link #calcField}の各要素に英数字型の256バイトフィールドを割り当てる。
     */
    public static void init() {
        CobolFieldAttribute attr =
                new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null);
        for (int i = 0; i < DEPTH_LEVEL; ++i) {
            calcField[i] = CobolFieldFactory.makeCobolField(256, new CobolDataStorage(256), attr);
        }
    }

    // libcob/intrinsicのcob_intr_get_doubleの実装
    /**
     * {@link CobolDecimal}の値を、その位取り(scale)を反映したdouble値に変換する。
     *
     * @param d 変換元の十進数値
     * @return 位取りを反映したdouble値
     */
    private static double intrGetDouble(CobolDecimal d) {
        double v = d.getValue().doubleValue();
        int n = d.getScale();
        for (int i = 0; i < Math.abs(n); ++i) {
            if (n > 0) {
                v /= 10;
            } else {
                v *= 10;
            }
        }
        return v;
    }

    /**
     * 部分参照(reference modification)を計算結果フィールドに適用する。<br>
     * 指定した開始位置offset(1始まり)と長さlengthに従ってフィールドの内容を切り出し、
     * フィールドのサイズとデータを更新する。
     *
     * @param f 部分参照を適用するフィールド
     * @param offset 切り出し開始位置(1始まり)
     * @param length 切り出す長さ。0以下の場合は開始位置以降の全体を対象とする
     */
    private static void calcRefMod(AbstractCobolField f, int offset, int length) {
        if (offset <= f.getSize()) {
            int calcoff = offset - 1;
            int size = f.getSize() - calcoff;
            if (length > 0 && length < size) {
                size = length;
            }
            f.setSize(size);
            if (calcoff > 0) {
                CobolDataStorage tmp = new CobolDataStorage(size);
                tmp.memcpy(f.getDataStorage().getSubDataStorage(calcoff), size);
                f.getDataStorage().memcpy(tmp, size);
            }
        }
    }

    /**
     * 2つのフィールドに対して二項算術演算を行い、その結果をフィールドとして返す。<br>
     * COBOLの算術式の評価に対応する。
     *
     * @param f1 左辺のフィールド
     * @param op 演算子を表す文字コード('+'、'-'、'*'、'/'、'^'のいずれか)
     * @param f2 右辺のフィールド
     * @return 演算結果を保持するフィールド
     * @throws CobolStopRunException 演算処理中にランタイムエラーが発生した場合
     */
    public static AbstractCobolField intrBinop(AbstractCobolField f1, int op, AbstractCobolField f2)
            throws CobolStopRunException {
        CobolDecimal d1 = new CobolDecimal();
        CobolDecimal d2 = new CobolDecimal();
        d1.setField(f1);
        d2.setField(f2);

        switch ((char) op) {
            case '+':
                d1.add(d2);
                break;
            case '-':
                d1.sub(d2);
                break;
            case '*':
                d1.mul(d2);
                break;
            case '/':
                d1.div(d2);
                break;
            case '^':
                d1.pow(d2);
                break;
            default:
                break;
        }

        int attrsign = 0;
        if (d1.getValue().signum() < 0) {
            attrsign = CobolFieldAttribute.COB_FLAG_HAVE_SIGN;
        } else {
            attrsign = 0;
        }

        int size = sizeInBase10(d1.getValue());
        if (d1.getScale() > size) {
            size = d1.getScale();
        }
        CobolFieldAttribute attr =
                new CobolFieldAttribute(
                        CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY,
                        size,
                        d1.getScale(),
                        attrsign,
                        null);
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(size, (CobolDataStorage) null, attr);
        makeFieldEntry(field);
        d1.getDisplayField(currField, 0);
        return currField;
    }

    /**
     * COBOLの組み込み関数FUNCTION LENGTHに対応する。<br>
     * 引数のフィールドのバイト数を返す。
     *
     * @param srcfield 長さを求める対象のフィールド
     * @return フィールドのバイト数を保持する数値フィールド
     */
    public static AbstractCobolField funcLength(AbstractCobolField srcfield) {
        CobolFieldAttribute attr =
                new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY, 8, 0, 0, null);
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(4, (CobolDataStorage) null, attr);
        makeFieldEntry(field);
        currField.setInt(srcfield.getSize());
        return currField;
    }

    /**
     * COBOLの組み込み関数FUNCTION INTEGERに対応する。<br>
     * 引数の値を超えない最大の整数(負方向への切り捨て、すなわち床関数)を返す。
     *
     * @param srcfield 対象の数値フィールド
     * @return 床関数の結果を保持する整数フィールド。処理中にエラーが発生した場合はnull
     */
    public static AbstractCobolField funcInteger(AbstractCobolField srcfield) {
        CobolFieldAttribute attr =
                new CobolFieldAttribute(
                        CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY,
                        18,
                        0,
                        CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
                        null);
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(8, (CobolDataStorage) null, attr);
        makeFieldEntry(field);

        CobolDecimal d1 = new CobolDecimal();
        d1.setField(srcfield);
        if (d1.getValue().signum() >= 0) {
            try {
                d1.getField(currField, 0);
            } catch (CobolStopRunException e) {
                return null;
            }
            return currField;
        }

        boolean isScalePositive = d1.getScale() > 0;
        BigDecimal val = d1.getValue();
        for (int i = 0; i < Math.abs(d1.getScale()); ++i) {
            if (isScalePositive) {
                val = val.divide(BigDecimal.TEN);
            } else {
                val = val.multiply(BigDecimal.TEN);
            }
        }

        // Rouding to negative infinity
        BigDecimal[] vals = val.divideAndRemainder(BigDecimal.ONE);
        if (vals[1].signum() != 0) {
            vals[0] = vals[0].subtract(BigDecimal.ONE);
        }

        try {
            new CobolDecimal(vals[0], 0).getField(currField, 0);
        } catch (CobolStopRunException e) {
            return null;
        }
        return currField;
    }

    /**
     * COBOLの組み込み関数FUNCTION INTEGER-PARTに対応する。<br>
     * 引数の整数部(小数点以下を0方向へ切り捨てた値)を返す。
     *
     * @param srcfield 対象の数値フィールド
     * @return 整数部を保持する整数フィールド
     */
    public static AbstractCobolField funcIntegerPart(AbstractCobolField srcfield) {
        CobolFieldAttribute attr =
                new CobolFieldAttribute(
                        CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY,
                        18,
                        0,
                        CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
                        null);
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(8, (CobolDataStorage) null, attr);

        makeFieldEntry(field);
        currField.moveFrom(srcfield);
        return currField;
    }

    /**
     * COBOLの組み込み関数FUNCTION UPPER-CASEに対応する。<br>
     * 引数の文字列中の小文字を大文字に変換した結果を返す。
     *
     * @param offset 結果に対する部分参照の開始位置(1始まり)。0以下の場合は部分参照を行わない
     * @param length 結果に対する部分参照の長さ
     * @param srcfield 変換対象の文字列フィールド
     * @return 大文字に変換した結果を保持するフィールド
     */
    public static AbstractCobolField funcUpperCase(
            int offset, int length, AbstractCobolField srcfield) {
        makeFieldEntry(srcfield);
        int size = srcfield.getSize();
        CobolDataStorage currStorage = currField.getDataStorage();
        CobolDataStorage srcStorage = srcfield.getDataStorage();
        for (int i = 0; i < size; ++i) {
            currStorage.setByte(i, (byte) Character.toUpperCase(srcStorage.getByte(i)));
        }
        if (offset > 0) {
            calcRefMod(currField, offset, length);
        }
        return currField;
    }

    /**
     * COBOLの組み込み関数FUNCTION LOWER-CASEに対応する。<br>
     * 引数の文字列中の大文字を小文字に変換した結果を返す。
     *
     * @param offset 結果に対する部分参照の開始位置(1始まり)。0以下の場合は部分参照を行わない
     * @param length 結果に対する部分参照の長さ
     * @param srcfield 変換対象の文字列フィールド
     * @return 小文字に変換した結果を保持するフィールド
     */
    public static AbstractCobolField funcLowerCase(
            int offset, int length, AbstractCobolField srcfield) {
        makeFieldEntry(srcfield);
        int size = srcfield.getSize();
        CobolDataStorage currStorage = currField.getDataStorage();
        CobolDataStorage srcStorage = srcfield.getDataStorage();
        for (int i = 0; i < size; ++i) {
            currStorage.setByte(i, (byte) Character.toLowerCase(srcStorage.getByte(i)));
        }
        if (offset > 0) {
            calcRefMod(currField, offset, length);
        }
        return currField;
    }

    /**
     * COBOLの組み込み関数FUNCTION REVERSEに対応する。<br>
     * 引数の文字列を逆順に並べ替えた結果を返す。
     *
     * @param offset 結果に対する部分参照の開始位置(1始まり)。0以下の場合は部分参照を行わない
     * @param length 結果に対する部分参照の長さ
     * @param srcfield 反転対象の文字列フィールド
     * @return 文字順を反転した結果を保持するフィールド
     */
    public static AbstractCobolField funcReverse(
            int offset, int length, AbstractCobolField srcfield) {
        makeFieldEntry(srcfield);
        int size = srcfield.getSize();
        CobolDataStorage currStorage = currField.getDataStorage();
        CobolDataStorage srcStorage = srcfield.getDataStorage();
        for (int i = 0; i < size; ++i) {
            currStorage.setByte(i, srcStorage.getByte(srcfield.getSize() - i - 1));
        }
        if (offset > 0) {
            calcRefMod(currField, offset, length);
        }
        return currField;
    }

    /**
     * COBOLの組み込み関数FUNCTION WHEN-COMPILEDに対応する。<br>
     * コンパイル日時を表すフィールドの内容をそのまま返す。
     *
     * @param offset 結果に対する部分参照の開始位置(1始まり)。0以下の場合は部分参照を行わない
     * @param length 結果に対する部分参照の長さ
     * @param f コンパイル日時を保持するフィールド
     * @return コンパイル日時を保持するフィールド
     */
    public static AbstractCobolField funcWhenCompiled(
            int offset, int length, AbstractCobolField f) {
        makeFieldEntry(f);
        currField.getDataStorage().memcpy(f.getDataStorage(), f.getSize());
        if (offset > 0) {
            calcRefMod(currField, offset, length);
        }
        return currField;
    }

    /**
     * COBOLの組み込み関数FUNCTION CURRENT-DATEに対応する。<br>
     * 現在の日付と時刻を「YYYYMMDDHHMMSSccsHHMM」形式の21文字の文字列として返す。<br>
     * ここでccは1/100秒、sHHMMはグリニッジ標準時からのオフセット(符号+時+分)を表す。<br>
     * 環境変数COB_DATEで固定日付が指定されている場合は、日付部分にその値が反映される。<br>
     * 時刻とオフセットは常に実行時のシステムクロックから取得する。
     *
     * @param offset 結果に対する部分参照の開始位置(1始まり)。0以下の場合は部分参照を行わない
     * @param length 結果に対する部分参照の長さ
     * @return 現在日時を表す文字列フィールド
     */
    public static AbstractCobolField funcCurrentDate(int offset, int length) {
        CobolFieldAttribute attr =
                new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null);
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(21, (CobolDataStorage) null, attr);
        makeFieldEntry(field);

        // COB_DATEで置き換えるのは日付だけであり、時刻もオフセットも実行時のシステムクロックに由来する。
        // これはGnuCOBOL 3.2(OSSC patch)と同じ挙動である。opensource COBOL 1.5は
        // COB_DATEの日付に対するmktimeのtm_gmtoffをそのまま使うため、COB_DATE指定時に限り
        // オフセットが異なりうる(TZ=America/New_York, COB_DATE=2026/01/15を夏に実行すると
        // 1.5は-0500、こちらは-0400)。
        OffsetDateTime now = CobolUtil.localtime();
        int offsetInMinutes = now.getOffset().getTotalSeconds() / 60;
        char offsetSign = offsetInMinutes < 0 ? '-' : '+';
        int absOffsetInMinutes = Math.abs(offsetInMinutes);

        String dateString =
                String.format(
                        Locale.ROOT,
                        "%04d%02d%02d%02d%02d%02d%02d%c%02d%02d",
                        now.getYear(),
                        now.getMonthValue(),
                        now.getDayOfMonth(),
                        now.getHour(),
                        now.getMinute(),
                        now.getSecond(),
                        now.getNano() / 10000000,
                        offsetSign,
                        absOffsetInMinutes / 60,
                        absOffsetInMinutes % 60);
        currField.getDataStorage().memcpy(dateString.getBytes(AbstractCobolField.charSetSJIS));

        if (offset > 0) {
            calcRefMod(currField, offset, length);
        }
        return currField;
    }

    /**
     * COBOLの組み込み関数FUNCTION CHARに対応する。<br>
     * 引数の整数値(1始まりの順序位置)に対応する1文字を返す。
     *
     * @param srcfield 文字の順序位置を表す数値フィールド
     * @return 対応する1文字を保持するフィールド
     */
    public static AbstractCobolField funcChar(AbstractCobolField srcfield) {
        CobolFieldAttribute attr =
                new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null);
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(1, (CobolDataStorage) null, attr);
        makeFieldEntry(field);

        int i = srcfield.getInt();
        if (i < 1 || i > 256) {
            currField.getDataStorage().setByte(0, (byte) 0);
        } else {
            currField.getDataStorage().setByte(0, (byte) (i - 1));
        }
        return currField;
    }

    // libcob/intrinsicのcob_intr_ordの実装
    /**
     * COBOLの組み込み関数FUNCTION ORDに対応する。<br>
     * 引数の文字の照合順序上の位置(1始まり)を返す。
     *
     * @param srcfield 対象の1文字を保持するフィールド
     * @return 文字の順序位置を保持する数値フィールド
     */
    public static AbstractCobolField funcOrd(AbstractCobolField srcfield) {
        CobolFieldAttribute attr =
                new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY, 8, 0, 0, null);
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(4, (CobolDataStorage) null, attr);
        makeFieldEntry(field);

        currField.setInt(srcfield.getDataStorage().getByte(0) + 1);
        return currField;
    }

    // libcob/intrinsicのcob_intr_date_of_integerの実装
    /**
     * COBOLの組み込み関数FUNCTION DATE-OF-INTEGERに対応する。<br>
     * 基準日(1601年1月1日を1とする通日)から、対応する日付を「YYYYMMDD」形式で返す。<br>
     * 引数が有効範囲(1〜3067671)外の場合は例外を設定し、結果を"00000000"とする。
     *
     * @param srcdays 通日(整数)を保持するフィールド
     * @return 日付を表す8桁の数値フィールド
     */
    public static AbstractCobolField funcDateOfInteger(AbstractCobolField srcdays) {
        CobolFieldAttribute attr =
                new CobolFieldAttribute(
                        CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY, 8, 0, 0, null);
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(8, (CobolDataStorage) null, attr);
        makeFieldEntry(field);

        CobolRuntimeException.setException(0);
        int days = srcdays.getInt();

        if (days < 1 || days > 3067671) {
            CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
            currField.getDataStorage().memset((byte) '0', 8);
            return currField;
        }

        int leapyear = 365;
        int baseyear = 1601;
        while (days > leapyear) {
            days -= leapyear;
            ++baseyear;
            if (isLeapYear(baseyear)) {
                leapyear = 366;
            } else {
                leapyear = 365;
            }
        }
        int i;
        for (i = 0; i < 13; ++i) {
            if (isLeapYear(baseyear)) {
                if (days <= leapDays[i]) {
                    days -= leapDays[i - 1];
                    break;
                }
            } else {
                if (days <= normalDays[i]) {
                    days -= normalDays[i - 1];
                    break;
                }
            }
        }
        String dateString = String.format("%04d%02d%02d", baseyear, i, days);
        currField.getDataStorage().memcpy(dateString.getBytes(AbstractCobolField.charSetSJIS));
        return currField;
    }

    // libcob/intrinsicのcob_intr_day_of_integerの実装
    /**
     * COBOLの組み込み関数FUNCTION DAY-OF-INTEGERに対応する。<br>
     * 基準日(1601年1月1日を1とする通日)から、対応する日付を年間通日形式「YYYYDDD」で返す。<br>
     * 引数が有効範囲(1〜3067671)外の場合は例外を設定する。
     *
     * @param srcdays 通日(整数)を保持するフィールド
     * @return 年間通日を表す7桁の数値フィールド
     */
    public static AbstractCobolField funcDayOfInteger(AbstractCobolField srcdays) {
        CobolFieldAttribute attr =
                new CobolFieldAttribute(
                        CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY, 7, 0, 0, null);
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(7, (CobolDataStorage) null, attr);
        makeFieldEntry(field);

        CobolRuntimeException.setException(0);
        int days = srcdays.getInt();

        if (days < 1 || days > 3067671) {
            CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
            currField.getDataStorage().memset((byte) '0', 8);
            return currField;
        }

        int leapyear = 365;
        int baseyear = 1601;
        while (days > leapyear) {
            days -= leapyear;
            ++baseyear;
            if (isLeapYear((baseyear))) {
                leapyear = 366;
            } else {
                leapyear = 365;
            }
        }
        String dateString = String.format("%04d%03d", baseyear, days);
        currField.getDataStorage().memcpy(dateString.getBytes(AbstractCobolField.charSetSJIS));
        return currField;
    }

    // libcob/intrinsicのcob_intr_integer_of_dateの実装
    /**
     * COBOLの組み込み関数FUNCTION INTEGER-OF-DATEに対応する。<br>
     * 「YYYYMMDD」形式の日付を、1601年1月1日を1とする通日(整数)に変換する。<br>
     * 年・月・日のいずれかが有効範囲外の場合は例外を設定し、0を返す。
     *
     * @param srcfield 「YYYYMMDD」形式の日付を保持するフィールド
     * @return 通日を表す数値フィールド
     */
    public static AbstractCobolField funcIntegerOfDate(AbstractCobolField srcfield) {
        CobolFieldAttribute attr =
                new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY, 8, 0, 0, null);
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(4, (CobolDataStorage) null, attr);
        makeFieldEntry(field);

        CobolRuntimeException.setException(0);
        int indate = srcfield.getInt();
        int year = indate / 10000;
        if (year < 1601 || year > 9999) {
            CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
            currField.setInt(0);
            return currField;
        }
        indate %= 10000;
        int month = indate / 100;
        if (month < 1 || month > 12) {
            CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
            currField.setInt(0);
            return currField;
        }
        int days = indate % 100;
        if (days < 1 || days > 31) {
            CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
            currField.setInt(0);
            return currField;
        }
        if (isLeapYear(year)) {
            if (days > leapMonthDays[month]) {
                CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
                currField.setInt(0);
                return currField;
            }
        } else {
            if (days > normalMonthDays[month]) {
                CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
                currField.setInt(0);
                return currField;
            }
        }

        int totaldays = 0;
        int baseyear = 1601;
        while (baseyear != year) {
            if (isLeapYear(baseyear)) {
                totaldays += 366;
            } else {
                totaldays += 365;
            }
            ++baseyear;
        }

        if (isLeapYear(baseyear)) {
            totaldays += leapDays[month - 1];
        } else {
            totaldays += normalDays[month - 1];
        }

        totaldays += days;
        currField.setInt(totaldays);
        return currField;
    }

    // libcob/intrinsicのcob_intr_integer_of_dayの実装
    /**
     * COBOLの組み込み関数FUNCTION INTEGER-OF-DAYに対応する。<br>
     * 年間通日形式「YYYYDDD」の日付を、1601年1月1日を1とする通日(整数)に変換する。<br>
     * 年または日が有効範囲外の場合は例外を設定し、0を返す。
     *
     * @param srcfield 「YYYYDDD」形式の日付を保持するフィールド
     * @return 通日を表す数値フィールド
     */
    public static AbstractCobolField funcIntegerOfDay(AbstractCobolField srcfield) {
        CobolFieldAttribute attr =
                new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY, 8, 0, 0, null);
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(4, (CobolDataStorage) null, attr);
        makeFieldEntry(field);

        CobolRuntimeException.setException(0);
        int indate = srcfield.getInt();
        int year = indate / 1000;
        if (year < 1601 || year > 9999) {
            CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
            currField.setInt(0);
            return currField;
        }
        int days = indate % 1000;
        if (days < 1 || days > 365 + (isLeapYear(year) ? 1 : 0)) {
            CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
            currField.setInt(0);
            return currField;
        }
        int totaldays = 0;
        int baseyear = 1601;
        while (baseyear != year) {
            if (isLeapYear(baseyear)) {
                totaldays += 366;
            } else {
                totaldays += 365;
            }
            ++baseyear;
        }
        totaldays += days;
        currField.setInt(totaldays);
        return currField;
    }

    // libcob/intrinsicのcob_intr_factorialの実装
    /**
     * COBOLの組み込み関数FUNCTION FACTORIALに対応する。<br>
     * 引数の非負整数の階乗を返す。<br>
     * 引数が負の場合は例外を設定し、0を返す。
     *
     * @param srcfield 階乗を求める非負整数を保持するフィールド
     * @return 階乗の値を保持する数値フィールド。処理中にエラーが発生した場合はnull
     */
    public static AbstractCobolField funcFactorial(AbstractCobolField srcfield) {
        CobolFieldAttribute attr =
                new CobolFieldAttribute(
                        CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY, 18, 0, 0, null);
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(8, (CobolDataStorage) null, attr);
        makeFieldEntry(field);

        CobolRuntimeException.setException(0);
        int srcval = srcfield.getInt();
        if (srcval < 0) {
            CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
            currField.setInt(0);
            return currField;
        }
        BigDecimal d = BigDecimal.ONE;
        for (int i = 2; i <= srcval; ++i) {
            d = d.multiply(new BigDecimal(i));
        }
        try {
            new CobolDecimal(d, 0).getField(currField, 0);
        } catch (CobolStopRunException e) {
            return null;
        }
        return currField;
    }

    /**
     * 三角関数・逆三角関数系の組み込み関数の前処理。<br>
     * 引数から{@link CobolDecimal}を生成し、結果格納用の数値フィールドを準備する。
     *
     * @param srcfield 対象の数値フィールド
     * @return 引数の値を保持する十進数値
     */
    private static CobolDecimal mathFunctionBefore1(AbstractCobolField srcfield) {
        CobolDecimal d1 = new CobolDecimal();
        CobolFieldAttribute attr =
                new CobolFieldAttribute(
                        CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY,
                        18,
                        17,
                        CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
                        null);
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(8, (CobolDataStorage) null, attr);
        d1.setField(srcfield);
        makeFieldEntry(field);
        return d1;
    }

    /**
     * 指数・対数・平方根系の組み込み関数の前処理。<br>
     * 引数から{@link CobolDecimal}を生成し、結果格納用のdouble型フィールドを準備する。
     *
     * @param srcfield 対象の数値フィールド
     * @return 引数の値を保持する十進数値
     */
    private static CobolDecimal mathFunctionBefore2(AbstractCobolField srcfield) {
        CobolDecimal d1 = new CobolDecimal();
        d1.setField(srcfield);
        makeDoubleEntry();
        return d1;
    }

    /**
     * {@link #mathFunctionBefore1(AbstractCobolField)}と対になる後処理。<br>
     * 計算したdouble値を、位取り17桁の数値フィールドに格納して返す。<br>
     * 値がNaNまたは無限大の場合は0を格納する。
     *
     * @param mathd2 数学関数の計算結果
     * @return 結果を保持する数値フィールド
     */
    private static AbstractCobolField mathFunctionAfter1(double mathd2) {
        if (Double.isNaN(mathd2)
                || mathd2 == Double.POSITIVE_INFINITY
                || mathd2 == Double.NEGATIVE_INFINITY) {
            currField.setInt(0);
            return currField;
        }
        long result = (long) mathd2;
        mathd2 -= result;
        for (int i = 0; i < 17; ++i) {
            mathd2 *= 10;
            int tempres = (int) mathd2;
            result *= 10;
            result += tempres;
            mathd2 -= tempres;
        }
        currField.getDataStorage().set(result);
        return currField;
    }

    /**
     * {@link #mathFunctionBefore2(AbstractCobolField)}と対になる後処理。<br>
     * 計算したdouble値を、double型のフィールドに格納して返す。<br>
     * 値がNaNまたは無限大の場合は0を格納する。
     *
     * @param mathd2 数学関数の計算結果
     * @return 結果を保持するdouble型フィールド
     */
    private static AbstractCobolField mathFunctionAfter2(double mathd2) {
        if (Double.isNaN(mathd2)
                || mathd2 == Double.POSITIVE_INFINITY
                || mathd2 == Double.NEGATIVE_INFINITY) {
            currField.setInt(0);
            return currField;
        }
        currField.getDataStorage().set(mathd2);
        return currField;
    }

    // libcob/intrinsicのcob_intr_expの実装
    /**
     * COBOLの組み込み関数FUNCTION EXPに対応する。<br>
     * ネイピア数eを底とする指数関数(e のsrcfield乗)を返す。
     *
     * @param srcfield 指数を保持する数値フィールド
     * @return 計算結果を保持するdouble型フィールド
     */
    public static AbstractCobolField funcExp(AbstractCobolField srcfield) {
        CobolDecimal d1 = mathFunctionBefore2(srcfield);
        double mathd2 = Math.pow(2.7182818284590452354, intrGetDouble(d1));
        return mathFunctionAfter2(mathd2);
    }

    // libcob/intrinsicのcob_intr_exp10の実装
    /**
     * COBOLの組み込み関数FUNCTION EXP10に対応する。<br>
     * 10を底とする指数関数(10 のsrcfield乗)を返す。
     *
     * @param srcfield 指数を保持する数値フィールド
     * @return 計算結果を保持するdouble型フィールド
     */
    public static AbstractCobolField funcExp10(AbstractCobolField srcfield) {
        CobolDecimal d1 = mathFunctionBefore2(srcfield);
        double mathd2 = Math.pow(10, intrGetDouble(d1));
        return mathFunctionAfter2(mathd2);
    }

    // libcob/intrinsicのcob_intr_absの実装
    /**
     * COBOLの組み込み関数FUNCTION ABSに対応する。<br>
     * 引数の絶対値を返す。
     *
     * @param srcfield 対象の数値フィールド
     * @return 絶対値を保持するフィールド。処理中にエラーが発生した場合はnull
     */
    public static AbstractCobolField funcAbs(AbstractCobolField srcfield) {
        makeFieldEntry(srcfield);
        CobolDecimal d1 = srcfield.getDecimal();
        d1.setValue(d1.getValue().abs());
        try {
            d1.getField(currField, 0);
        } catch (CobolStopRunException e) {
            return null;
        }
        return currField;
    }

    // libcob/intrinsicのcob_intr_acosの実装
    /**
     * COBOLの組み込み関数FUNCTION ACOSに対応する。<br>
     * 引数の逆余弦(アークコサイン、単位はラジアン)を返す。
     *
     * @param srcfield 対象の数値フィールド
     * @return 逆余弦の値を保持するフィールド
     */
    public static AbstractCobolField funcAcos(AbstractCobolField srcfield) {
        CobolDecimal d1 = mathFunctionBefore1(srcfield);
        double mathd2 = Math.acos(intrGetDouble(d1));
        return mathFunctionAfter1(mathd2);
    }

    // libcob/intrinsicのcob_intr_asinの実装
    /**
     * COBOLの組み込み関数FUNCTION ASINに対応する。<br>
     * 引数の逆正弦(アークサイン、単位はラジアン)を返す。
     *
     * @param srcfield 対象の数値フィールド
     * @return 逆正弦の値を保持するフィールド
     */
    public static AbstractCobolField funcAsin(AbstractCobolField srcfield) {
        CobolDecimal d1 = mathFunctionBefore1(srcfield);
        double mathd2 = Math.asin(intrGetDouble(d1));
        return mathFunctionAfter1(mathd2);
    }

    // libcob/intrinsicのcob_intr_atanの実装
    /**
     * COBOLの組み込み関数FUNCTION ATANに対応する。<br>
     * 引数の逆正接(アークタンジェント、単位はラジアン)を返す。
     *
     * @param srcfield 対象の数値フィールド
     * @return 逆正接の値を保持するフィールド
     */
    public static AbstractCobolField funcAtan(AbstractCobolField srcfield) {
        CobolDecimal d1 = mathFunctionBefore1(srcfield);
        double mathd2 = Math.atan(intrGetDouble(d1));
        return mathFunctionAfter1(mathd2);
    }

    // libcob/intrinsicのcob_intr_cosの実装
    /**
     * COBOLの組み込み関数FUNCTION COSに対応する。<br>
     * 引数(ラジアン)の余弦(コサイン)を返す。
     *
     * @param srcfield 角度(ラジアン)を保持する数値フィールド
     * @return 余弦の値を保持するフィールド
     */
    public static AbstractCobolField funcCos(AbstractCobolField srcfield) {
        CobolDecimal d1 = mathFunctionBefore1(srcfield);
        double mathd2 = Math.cos(intrGetDouble(d1));
        return mathFunctionAfter1(mathd2);
    }

    // libcob/intrinsicのcob_intr_logの実装
    /**
     * COBOLの組み込み関数FUNCTION LOGに対応する。<br>
     * 引数の自然対数(底はネイピア数e)を返す。
     *
     * @param srcfield 対象の数値フィールド
     * @return 自然対数の値を保持するdouble型フィールド
     */
    public static AbstractCobolField funcLog(AbstractCobolField srcfield) {
        CobolDecimal d1 = mathFunctionBefore2(srcfield);
        double mathd2 = Math.log(intrGetDouble(d1));
        return mathFunctionAfter2(mathd2);
    }

    // libcob/intrinsicのcob_intr_log10の実装
    /**
     * COBOLの組み込み関数FUNCTION LOG10に対応する。<br>
     * 引数の常用対数(底は10)を返す。
     *
     * @param srcfield 対象の数値フィールド
     * @return 常用対数の値を保持するdouble型フィールド
     */
    public static AbstractCobolField funcLog10(AbstractCobolField srcfield) {
        CobolDecimal d1 = mathFunctionBefore2(srcfield);
        double mathd2 = Math.log10(intrGetDouble(d1));
        return mathFunctionAfter2(mathd2);
    }

    // libcob/intrinsicのcob_intr_sinの実装
    /**
     * COBOLの組み込み関数FUNCTION SINに対応する。<br>
     * 引数(ラジアン)の正弦(サイン)を返す。
     *
     * @param srcfield 角度(ラジアン)を保持する数値フィールド
     * @return 正弦の値を保持するフィールド
     */
    public static AbstractCobolField funcSin(AbstractCobolField srcfield) {
        CobolDecimal d1 = mathFunctionBefore1(srcfield);
        double mathd2 = Math.sin(intrGetDouble(d1));
        return mathFunctionAfter1(mathd2);
    }

    // libcob/intrinsicのcob_intr_sqrtの実装
    /**
     * COBOLの組み込み関数FUNCTION SQRTに対応する。<br>
     * 引数の平方根を返す。
     *
     * @param srcfield 対象の数値フィールド
     * @return 平方根の値を保持するdouble型フィールド
     */
    public static AbstractCobolField funcSqrt(AbstractCobolField srcfield) {
        CobolDecimal d1 = mathFunctionBefore2(srcfield);
        double mathd2 = Math.sqrt(intrGetDouble(d1));
        return mathFunctionAfter2(mathd2);
    }

    // libcob/intrinsicのcob_intr_tanの実装
    /**
     * COBOLの組み込み関数FUNCTION TANに対応する。<br>
     * 引数(ラジアン)の正接(タンジェント)を返す。
     *
     * @param srcfield 角度(ラジアン)を保持する数値フィールド
     * @return 正接の値を保持するdouble型フィールド
     */
    public static AbstractCobolField funcTan(AbstractCobolField srcfield) {
        CobolDecimal d1 = mathFunctionBefore2(srcfield);
        double mathd2 = Math.tan(intrGetDouble(d1));
        return mathFunctionAfter2(mathd2);
    }

    // libcob/intrinsicのcob_intr_numvalの実装
    /**
     * COBOLの組み込み関数FUNCTION NUMVALに対応する。<br>
     * 数字を表す文字列(符号、小数点、CR/DBなどを含む)を解析し、対応する数値に変換して返す。
     *
     * @param srcfield 数字を表す文字列フィールド
     * @return 変換した数値を保持するフィールド
     */
    public static AbstractCobolField funcNumval(AbstractCobolField srcfield) {
        CobolFieldAttribute attr =
                new CobolFieldAttribute(
                        CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY,
                        18,
                        0,
                        CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
                        null);
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(8, (CobolDataStorage) null, attr);

        CobolDataStorage s = srcfield.getDataStorage();
        boolean sign = false;
        boolean decimalSeen = false;
        long llval = 0;
        int integerDigits = 0;
        int decimalDigits = 0;
        StringBuilder integerBuff = new StringBuilder();
        StringBuilder decimalBuff = new StringBuilder();
        for (int i = 0; i < srcfield.getSize(); ++i) {
            if (i < srcfield.getSize() - 1) {
                if ((Character.toUpperCase(s.getByte(i)) == 'C'
                                && Character.toUpperCase(s.getByte(i + 1)) == 'R')
                        || (Character.toUpperCase(s.getByte(i)) == 'D'
                                && Character.toUpperCase(s.getByte(i + 1)) == 'B')) {
                    sign = true;
                    break;
                }
            }
            char c = (char) s.getByte(i);
            if (c == ' ' || c == '+') {
                continue;
            }
            if (c == '-') {
                sign = true;
                continue;
            }
            if (c == CobolModule.getCurrentModule().decimal_point) {
                decimalSeen = true;
                continue;
            }
            if (c >= '0' && c <= '9') {
                llval *= 10;
                llval += c - '0';
                if (decimalSeen) {
                    decimalBuff.append(c);
                    decimalDigits++;
                } else {
                    integerBuff.append(c);
                    integerDigits++;
                }
            }
            if (integerDigits + decimalDigits > 30) {
                break;
            }
        }
        if (integerDigits > 0) {
            integerBuff.setCharAt(0, '0');
        }
        if (decimalDigits > 0) {
            decimalBuff.setCharAt(0, '0');
        }
        if (sign) {
            llval = -llval;
        }
        if (integerDigits + decimalDigits <= 18) {
            attr.setScale(decimalDigits);
            makeFieldEntry(field);
            currField.getDataStorage().set(llval);
        } else {
            String dataString =
                    String.format(
                            "%s%s.%s",
                            sign ? "-" : "", integerBuff.toString(), decimalBuff.toString());
            double val = Double.parseDouble(dataString);
            makeDoubleEntry();
            currField.getDataStorage().set(val);
        }
        return currField;
    }

    // libcob/intrinsicのcob_intr_numval_cの実装
    /**
     * COBOLの組み込み関数FUNCTION NUMVAL-Cに対応する。<br>
     * 通貨記号やカンマを含む金額文字列を解析し、対応する数値に変換して返す。
     *
     * @param srcfield 金額を表す文字列フィールド
     * @param currency 通貨記号を表すフィールド。nullの場合はモジュールの通貨記号設定を用いる
     * @return 変換した数値を保持するフィールド
     */
    public static AbstractCobolField funcNumvalC(
            AbstractCobolField srcfield, AbstractCobolField currency) {
        CobolFieldAttribute attr =
                new CobolFieldAttribute(
                        CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY,
                        18,
                        0,
                        CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
                        null);
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(8, (CobolDataStorage) null, attr);

        boolean sign = false;
        boolean decimalSeen = false;
        long llval = 0;
        int integerDigits = 0;
        int decimalDigits = 0;
        StringBuilder integerBuff = new StringBuilder();
        StringBuilder decimalBuff = new StringBuilder();

        CobolDataStorage currencyData = null;
        if (currency != null) {
            if (currency.getSize() < srcfield.getSize()) {
                currencyData = currency.getDataStorage();
            }
        }
        for (int i = 0; i < srcfield.getSize(); ++i) {
            char c = (char) srcfield.getDataStorage().getByte(i);
            if (i < srcfield.getSize() - 1) {
                char cc = (char) srcfield.getDataStorage().getByte(i + 1);
                if ((Character.toUpperCase(c) == 'C' && Character.toUpperCase(cc) == 'R')
                        || (Character.toUpperCase(c) == 'D' && Character.toUpperCase(cc) == 'B')) {
                    sign = true;
                    break;
                }
            }
            if (currencyData != null) {
                if (i < srcfield.getSize() - currency.getSize()) {
                    if (currencyData.memcmp(
                                    srcfield.getDataStorage().getSubDataStorage(i),
                                    currency.getSize())
                            == 0) {
                        i += (currency.getSize() - 1);
                        continue;
                    }
                }
            }
            if (c == ' ' || c == '+') {
                continue;
            }
            if (c == '-') {
                sign = true;
                continue;
            }
            if (c == CobolModule.getCurrentModule().decimal_point) {
                decimalSeen = true;
                continue;
            }
            if (c == CobolModule.getCurrentModule().currency_symbol) {
                continue;
            }
            if (c >= '0' && c <= '9') {
                llval *= 10;
                llval += c - '0';
                if (decimalSeen) {
                    decimalBuff.append(c);
                    decimalDigits++;
                } else {
                    integerBuff.append(c);
                    integerDigits++;
                }
            }
            if (integerDigits + decimalDigits > 30) {
                break;
            }
        }
        if (integerDigits > 0) {
            integerBuff.setCharAt(0, '0');
        }
        if (decimalDigits > 0) {
            decimalBuff.setCharAt(0, '0');
        }
        if (sign) {
            llval = -llval;
        }
        if (integerDigits + decimalDigits <= 18) {
            attr.setScale(decimalDigits);
            makeFieldEntry(field);
            currField.getDataStorage().set(llval);
        } else {
            String dataString =
                    String.format(
                            "%s%s.%s",
                            sign ? "-" : "", integerBuff.toString(), decimalBuff.toString());
            double val = Double.parseDouble(dataString);
            makeDoubleEntry();
            currField.getDataStorage().set(val);
        }
        return currField;
    }

    /**
     * COBOLの組み込み関数FUNCTION NUMVAL-Cに対応するオーバーロードのうち、第1引数が数値リテラルとして
     * 渡された呼び出しに対応するスタブ。<br>
     * このメソッドは未実装であり、常にnullを返す。
     *
     * @param n 金額を表す数値(未使用)
     * @param currency 通貨記号を表すフィールド(未使用)
     * @return 常にnull
     */
    public static AbstractCobolField funcNumvalC(int n, AbstractCobolField currency) {
        // TODO
        return null;
    }

    /**
     * COBOLの組み込み関数FUNCTION NUMVAL-Cに対応するオーバーロード。<br>
     * 通貨記号の指定を省略した呼び出しに対応し、通貨記号なしで
     * {@link #funcNumvalC(AbstractCobolField, AbstractCobolField)}を実行する。
     *
     * @param srcfield 金額を表す文字列フィールド
     * @param n 通貨記号フィールドが指定されなかったことを表すダミー引数
     * @return 変換した数値を保持するフィールド
     */
    public static AbstractCobolField funcNumvalC(AbstractCobolField srcfield, int n) {
        return funcNumvalC(srcfield, null);
    }

    /**
     * COBOLの組み込み関数FUNCTION NUMVAL-Cに対応するオーバーロードのうち、両引数が数値リテラルとして
     * 渡された呼び出しに対応するスタブ。<br>
     * このメソッドは未実装であり、常にnullを返す。
     *
     * @param n 金額を表す数値(未使用)
     * @param m 通貨記号を表す数値(未使用)
     * @return 常にnull
     */
    public static AbstractCobolField funcNumvalC(int n, int m) {
        // TODO
        return null;
    }

    // libcob/intrinsicのcob_intr_annuityの実装
    /**
     * COBOLの組み込み関数FUNCTION ANNUITYに対応する。<br>
     * 利率srcfield1と期間数srcfield2から、元本1に対する年金係数(期ごとの返済額)を返す。<br>
     * 利率が0の場合は期間数の逆数を返す。
     *
     * @param srcfield1 期あたりの利率を保持するフィールド
     * @param srcfield2 期間数を保持するフィールド
     * @return 年金係数を保持するdouble型フィールド
     */
    public static AbstractCobolField funcAnnuity(
            AbstractCobolField srcfield1, AbstractCobolField srcfield2) {
        makeDoubleEntry();
        CobolDecimal d1 = new CobolDecimal();
        CobolDecimal d2 = new CobolDecimal();
        d1.setField(srcfield1);
        d2.setField(srcfield2);

        double mathd1 = intrGetDouble(d1);
        double mathd2 = intrGetDouble(d2);
        if (mathd1 == 0) {
            mathd1 = 1.0 / mathd2;
            currField.getDataStorage().set(mathd1);
            return currField;
        }

        mathd1 /= (1.0 - Math.pow(mathd1 + 1.0, 0.0 - mathd2));
        currField.getDataStorage().set(mathd1);
        return currField;
    }

    /**
     * 値の整数部の桁数を返す。<br>
     * 符号と小数部を除いた、10進数表記での整数部の桁数を求める。
     *
     * @param d1 対象の十進数値
     * @return 整数部の桁数
     */
    private static int sizeInBase10(BigDecimal d1) {
        String s = d1.toPlainString();
        int begin = s.charAt(0) == '-' ? 0 : -1;
        int pointIndex = s.indexOf('.');
        int end = pointIndex < 0 ? s.length() : pointIndex;
        return end - begin - 1;
    }

    // libcob/intrinsicのcob_intr_sumの実装
    /**
     * COBOLの組み込み関数FUNCTION SUMに対応する。<br>
     * 引数として与えられたすべての数値の総和を返す。位取りは各引数のうち最大のものに合わせる。
     *
     * @param params 引数の個数
     * @param fields 合計の対象となる数値フィールドの並び
     * @return 総和を保持する数値フィールド。処理中にエラーが発生した場合はnull
     */
    public static AbstractCobolField funcSum(int params, AbstractCobolField... fields) {
        CobolDecimal d1 = new CobolDecimal();
        CobolDecimal d2 = new CobolDecimal();
        d1.setValue(BigDecimal.ZERO);

        int scale = 0;
        for (AbstractCobolField f : fields) {
            if (f.getAttribute().getScale() > scale) {
                scale = f.getAttribute().getScale();
            }
            d2.setField(f);
            d1.add(d2);
        }

        int size = sizeInBase10(d1.getValue());
        AbstractCobolField field;
        if (size < 19) {
            CobolFieldAttribute attr =
                    new CobolFieldAttribute(
                            CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY,
                            18,
                            scale,
                            CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
                            null);
            field = CobolFieldFactory.makeCobolField(8, (CobolDataStorage) null, attr);
        } else {
            if (d1.getScale() > size) {
                size = d1.getScale();
            }
            if (scale > size) {
                size = scale;
            }
            CobolFieldAttribute attr =
                    new CobolFieldAttribute(
                            CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY,
                            size,
                            scale,
                            CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
                            null);
            field = CobolFieldFactory.makeCobolField(size, (CobolDataStorage) null, attr);
        }
        makeFieldEntry(field);
        try {
            d1.getField(currField, 0);
        } catch (CobolStopRunException e) {
            return null;
        }
        return currField;
    }

    // libcob/intrinsicのcob_intr_ord_minの実装
    /**
     * COBOLの組み込み関数FUNCTION ORD-MINに対応する。<br>
     * 引数の並びの中で最小の値を持つ要素の順序位置(1始まり)を返す。<br>
     * 引数が1個以下の場合は0を返す。
     *
     * @param params 引数の個数
     * @param fields 比較対象となるフィールドの並び
     * @return 最小値を持つ要素の順序位置を保持する数値フィールド
     */
    public static AbstractCobolField funcOrdMin(int params, AbstractCobolField... fields) {
        CobolFieldAttribute attr =
                new CobolFieldAttribute(
                        CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY, 8, 0, 0, null);
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(8, (CobolDataStorage) null, attr);
        makeFieldEntry(field);

        if (fields.length <= 1) {
            currField.setInt(0);
            return currField;
        }

        AbstractCobolField basef = fields[0];
        int ordmin = 0;
        for (int i = 1; i < fields.length; ++i) {
            AbstractCobolField f = fields[i];
            if (f.compareTo(basef) < 0) {
                basef = f;
                ordmin = i;
            }
        }

        currField.setLong((long) ordmin + 1);
        return currField;
    }

    // libcob/intrinsicのcob_intr_ord_maxの実装
    /**
     * COBOLの組み込み関数FUNCTION ORD-MAXに対応する。<br>
     * 引数の並びの中で最大の値を持つ要素の順序位置(1始まり)を返す。<br>
     * 引数が1個以下の場合は0を返す。
     *
     * @param params 引数の個数
     * @param fields 比較対象となるフィールドの並び
     * @return 最大値を持つ要素の順序位置を保持する数値フィールド
     */
    public static AbstractCobolField funcOrdMax(int params, AbstractCobolField... fields) {
        CobolFieldAttribute attr =
                new CobolFieldAttribute(
                        CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY, 8, 0, 0, null);
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(8, (CobolDataStorage) null, attr);
        makeFieldEntry(field);

        if (fields.length <= 1) {
            currField.setInt(0);
            return currField;
        }

        AbstractCobolField basef = fields[0];
        int ordmax = 0;
        for (int i = 1; i < fields.length; ++i) {
            AbstractCobolField f = fields[i];
            if (f.compareTo(basef) > 0) {
                basef = f;
                ordmax = i;
            }
        }

        currField.setLong((long) ordmax + 1);
        return currField;
    }

    // libcob/intrinsicのcob_intr_minの実装
    /**
     * COBOLの組み込み関数FUNCTION MINに対応する。<br>
     * 引数の並びの中で最小の値を持つフィールドを返す。
     *
     * @param params 引数の個数
     * @param fields 比較対象となるフィールドの並び
     * @return 最小値を持つフィールド
     */
    public static AbstractCobolField funcMin(int params, AbstractCobolField... fields) {
        AbstractCobolField beasef = fields[0];
        for (int i = 1; i < fields.length; ++i) {
            AbstractCobolField f = fields[i];
            if (f.compareTo(beasef) < 0) {
                beasef = f;
            }
        }

        return beasef;
    }

    // libcob/intrinsicのcob_intr_maxの実装
    /**
     * COBOLの組み込み関数FUNCTION MAXに対応する。<br>
     * 引数の並びの中で最大の値を持つフィールドを返す。
     *
     * @param params 引数の個数
     * @param fields 比較対象となるフィールドの並び
     * @return 最大値を持つフィールド
     */
    public static AbstractCobolField funcMax(int params, AbstractCobolField... fields) {
        AbstractCobolField beasef = fields[0];
        for (int i = 1; i < fields.length; ++i) {
            AbstractCobolField f = fields[i];
            if (f.compareTo(beasef) > 0) {
                beasef = f;
            }
        }

        return beasef;
    }

    // libcob/intrinsicのcob_intr_midrangeの実装
    /**
     * COBOLの組み込み関数FUNCTION MIDRANGEに対応する。<br>
     * 引数の並びの中の最小値と最大値の平均((最小値 + 最大値) / 2)を返す。
     *
     * @param params 引数の個数
     * @param fields 比較対象となる数値フィールドの並び
     * @return 最小値と最大値の中間値を保持するdouble型フィールド。処理中にエラーが発生した場合はnull
     */
    public static AbstractCobolField funcMidrange(int params, AbstractCobolField... fields) {
        makeDoubleEntry();
        AbstractCobolField basemin = fields[0];
        AbstractCobolField basemax = fields[0];
        for (int i = 1; i < params; ++i) {
            AbstractCobolField f = fields[i];
            if (f.compareTo(basemin) < 0) {
                basemin = f;
            }
            if (f.compareTo(basemax) > 0) {
                basemax = f;
            }
        }

        CobolDecimal d1 = new CobolDecimal();
        CobolDecimal d2 = new CobolDecimal();
        d1.setField(basemin);
        d2.setField(basemax);
        d1.add(d2);
        d2 = new CobolDecimal(new BigDecimal(2), 0);
        try {
            d1.div(d2);
            d1.getField(currField, 0);
        } catch (CobolStopRunException e) {
            return null;
        }
        return currField;
    }

    // libcob/intrinsicのcob_intr_medianの実装
    /**
     * COBOLの組み込み関数FUNCTION MEDIANに対応する。<br>
     * 引数を昇順に整列したときの中央値を返す。引数の個数が奇数の場合は中央の要素を、
     * 偶数の場合は中央の2要素の平均を返す。
     *
     * @param params 引数の個数
     * @param fields 中央値を求める対象となる数値フィールドの並び
     * @return 中央値を保持するフィールド。処理中にエラーが発生した場合はnull
     */
    public static AbstractCobolField funcMedian(int params, AbstractCobolField... fields) {
        if (fields.length == 1) {
            return fields[0];
        }

        AbstractCobolField[] fieldAlloc = new AbstractCobolField[fields.length];

        for (int i = 0; i < params; ++i) {
            fieldAlloc[i] = fields[i];
        }

        Arrays.sort(fieldAlloc, (a, b) -> a.compareTo(b));
        int i = params / 2;
        if (params % 2 != 0) {
            return fieldAlloc[i];
        } else {
            makeDoubleEntry();
            CobolDecimal d1 = new CobolDecimal();
            CobolDecimal d2 = new CobolDecimal();
            d1.setField(fieldAlloc[i]);
            d2.setField(fieldAlloc[i - 1]);
            d1.add(d2);
            d2 = new CobolDecimal(new BigDecimal(2), 0);
            try {
                d1.div(d2);
                d1.getField(currField, 0);
            } catch (CobolStopRunException e) {
                return null;
            }
            return currField;
        }
    }

    // libcob/intrinsicのcob_intr_medianの実装
    /**
     * COBOLの組み込み関数FUNCTION MEANに対応する。<br>
     * 引数として与えられたすべての数値の算術平均(総和を要素数で割った値)を返す。
     *
     * @param pramas 引数の個数
     * @param fields 平均を求める対象となる数値フィールドの並び
     * @return 算術平均を保持する数値フィールド。処理中にエラーが発生した場合はnull
     */
    public static AbstractCobolField funcMean(int pramas, AbstractCobolField... fields) {
        CobolFieldAttribute attr =
                new CobolFieldAttribute(
                        CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY,
                        18,
                        0,
                        CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
                        null);
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(8, (CobolDataStorage) null, attr);
        CobolDecimal d1 = new CobolDecimal(BigDecimal.ZERO);
        CobolDecimal d2 = new CobolDecimal();

        for (AbstractCobolField f : fields) {
            d2.setField(f);
            d1.add(d2);
        }

        d2 = new CobolDecimal(new BigDecimal(fields.length), 0);
        try {
            d1.div(d2);
        } catch (CobolStopRunException e) {
            return null;
        }

        CobolDataStorage storage = new CobolDataStorage(8);
        field.setDataStorage(storage);
        try {
            d1.getField(field, 0);
        } catch (CobolStopRunException e) {
            return null;
        }
        long n = storage.longValue();
        int i = 0;
        while (n != 0) {
            n /= 10;
            ++i;
        }
        field.setDataStorage(null);
        if (i <= 18) {
            attr.setScale(18 - i);
        }
        makeFieldEntry(field);
        try {
            d1.getField(currField, 0);
        } catch (CobolStopRunException e) {
            return null;
        }
        return currField;
    }

    // libcob/intrinsicのcob_intr_modの実装
    /**
     * COBOLの組み込み関数FUNCTION MODに対応する。<br>
     * srcfield1をsrcfield2で割った剰余(srcfield1 - srcfield2 * FUNCTION INTEGER(srcfield1 / srcfield2))を返す。
     * 結果の符号は除数srcfield2と一致する。
     *
     * @param srcfield1 被除数を保持するフィールド
     * @param srcfield2 除数を保持するフィールド
     * @return 剰余を保持する数値フィールド。処理中にエラーが発生した場合はnull
     * @throws CobolStopRunException 演算処理中にランタイムエラーが発生した場合
     */
    public static AbstractCobolField funcMod(
            AbstractCobolField srcfield1, AbstractCobolField srcfield2)
            throws CobolStopRunException {
        CobolFieldAttribute attr =
                new CobolFieldAttribute(
                        CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY,
                        18,
                        0,
                        CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
                        null);
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(8, (CobolDataStorage) null, attr);
        CobolDecimal d1 = new CobolDecimal();
        CobolDecimal d2 = new CobolDecimal();
        makeFieldEntry(field);

        AbstractCobolField f1 = funcInteger(intrBinop(srcfield1, '/', srcfield2));
        d1.setField(srcfield2);
        d2.setField(f1);
        d2.mul(d1);
        d1.setField(srcfield1);
        d1.sub(d2);
        try {
            d1.getField(currField, 0);
        } catch (CobolStopRunException e) {
            return null;
        }
        return currField;
    }

    // libcob/intrinsicのcob_intr_rangeの実装
    /**
     * COBOLの組み込み関数FUNCTION RANGEに対応する。<br>
     * 引数の並びの中の最大値から最小値を引いた値(値の範囲)を返す。
     *
     * @param params 引数の個数
     * @param fields 範囲を求める対象となる数値フィールドの並び
     * @return 最大値と最小値の差を保持する数値フィールド
     * @throws CobolStopRunException 演算処理中にランタイムエラーが発生した場合
     */
    public static AbstractCobolField funcRange(int params, AbstractCobolField... fields)
            throws CobolStopRunException {
        CobolFieldAttribute attr =
                new CobolFieldAttribute(
                        CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY,
                        18,
                        0,
                        CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
                        null);
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(8, (CobolDataStorage) null, attr);
        CobolDecimal d1 = new CobolDecimal();
        CobolDecimal d2 = new CobolDecimal();

        AbstractCobolField basemin = fields[0];
        AbstractCobolField basemax = fields[0];
        for (int i = 1; i < fields.length; ++i) {
            AbstractCobolField f = fields[i];
            if (f.compareTo(basemin) < 0) {
                basemin = f;
            }
            if (f.compareTo(basemax) > 0) {
                basemax = f;
            }
        }

        attr.setScale(basemin.getAttribute().getScale());
        if (basemax.getAttribute().getScale() > attr.getScale()) {
            attr.setScale(basemax.getAttribute().getScale());
        }
        makeFieldEntry(field);
        d1.setField(basemax);
        d2.setField(basemin);
        d1.sub(d2);
        d1.getField(currField, 0);
        return currField;
    }

    // libcob/intrinsicのcob_intr_remの実装
    /**
     * COBOLの組み込み関数FUNCTION REMに対応する。<br>
     * srcfield1をsrcfield2で割った剰余(srcfield1 - srcfield2 * FUNCTION INTEGER-PART(srcfield1 / srcfield2))を返す。
     * 商の整数部を0方向へ切り捨てて算出するため、結果の符号は被除数srcfield1と一致する。
     *
     * @param srcfield1 被除数を保持するフィールド
     * @param srcfield2 除数を保持するフィールド
     * @return 剰余を保持する数値フィールド
     * @throws CobolStopRunException 演算処理中にランタイムエラーが発生した場合
     */
    public static AbstractCobolField funcRem(
            AbstractCobolField srcfield1, AbstractCobolField srcfield2)
            throws CobolStopRunException {
        CobolFieldAttribute attr =
                new CobolFieldAttribute(
                        CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY,
                        18,
                        0,
                        CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
                        null);
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(8, (CobolDataStorage) null, attr);
        AbstractCobolField f1 = funcIntegerPart(intrBinop(srcfield1, '/', srcfield2));
        CobolDecimal d1 = new CobolDecimal();
        CobolDecimal d2 = new CobolDecimal();

        d1.setField(srcfield2);
        d2.setField(f1);
        d2.mul(d1);
        d1.setField(srcfield1);
        d1.sub(d2);

        attr.setScale(srcfield1.getAttribute().getScale());
        if (srcfield2.getAttribute().getScale() > attr.getScale()) {
            attr.setScale(srcfield2.getAttribute().getScale());
        }
        makeFieldEntry(field);
        d1.getField(currField, 0);
        return currField;
    }

    // libcob/intrinsicのcob_intr_randomの実装
    /**
     * COBOLの組み込み関数FUNCTION RANDOMに対応する。<br>
     * 0以上1未満の擬似乱数を返す。引数が与えられた場合はその値を乱数の種(シード)として用いる。
     *
     * @param prams 引数の個数
     * @param fields シードを保持するフィールドの並び(省略可能)。指定された場合は先頭要素をシードとして用いる
     * @return 擬似乱数を保持する数値フィールド
     */
    public static AbstractCobolField funcRandom(int prams, AbstractCobolField... fields) {
        CobolFieldAttribute attr =
                new CobolFieldAttribute(
                        CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY,
                        18,
                        0,
                        CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
                        null);
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(8, (CobolDataStorage) null, attr);

        if (fields.length > 0) {
            AbstractCobolField f = fields[0];
            int seed = f.getInt();
            if (seed < 0) {
                seed = 0;
            }
            random.setSeed(seed);
        }

        int r = random.nextInt(1000000001);

        int exp10 = 1;
        int i = 0;
        for (i = 0; i < 10; ++i) {
            if (r / exp10 == 0) {
                break;
            }
            exp10 *= 10;
        }
        if (i == 0) {
            i = 1;
        }
        attr.setScale(i);
        makeFieldEntry(field);
        currField.getDataStorage().set((long) r);
        return currField;
    }

    // libcob/intrinsicのcob_intr_varianceの実装
    /**
     * COBOLの組み込み関数FUNCTION VARIANCEに対応する。<br>
     * 引数として与えられた数値の母分散(平均との差の二乗の平均)を返す。<br>
     * 引数が1個の場合は0を返す。
     *
     * @param prams 引数の個数
     * @param fields 分散を求める対象となる数値フィールドの並び
     * @return 母分散を保持する数値フィールド。処理中にエラーが発生した場合はnull
     * @throws CobolStopRunException 演算処理中にランタイムエラーが発生した場合
     */
    public static AbstractCobolField funcVariance(int prams, AbstractCobolField... fields)
            throws CobolStopRunException {
        CobolFieldAttribute attr =
                new CobolFieldAttribute(
                        CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY,
                        18,
                        0,
                        CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
                        null);
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(8, (CobolDataStorage) null, attr);

        if (fields.length == 1) {
            makeFieldEntry(field);
            currField.setInt(0);
            return currField;
        }

        CobolDecimal d1 = new CobolDecimal(BigDecimal.ZERO, 0);
        CobolDecimal d2 = new CobolDecimal();

        for (AbstractCobolField f : fields) {
            d2.setField(f);
            d1.add(d2);
        }

        d2.setValue(new BigDecimal(fields.length));
        d2.setScale(0);
        try {
            d1.div(d2);
        } catch (CobolStopRunException e) {
            return null;
        }

        CobolDecimal d4 = new CobolDecimal(BigDecimal.ZERO, 0);

        for (AbstractCobolField f : fields) {
            d2.setField(f);
            d2.sub(d1);
            d2.mul(d2);
            d4.add(d2);
        }

        CobolDecimal d3 = new CobolDecimal(new BigDecimal(fields.length), 0);
        try {
            d4.div(d3);
        } catch (CobolStopRunException e) {
            return null;
        }
        CobolDataStorage data = new CobolDataStorage(8);
        field.setDataStorage(data);
        d4.getField(field, 0);
        long n = field.getLong();
        int i = 0;
        while (n != 0) {
            n /= 10;
            ++i;
        }
        makeFieldEntry(field);
        if (i <= 18) {
            attr.setScale(18 - i);
        }
        d4.getField(currField, 0);
        return currField;
    }

    // libcob/intrinsicのcob_intr_standard_deviationの実装
    /**
     * COBOLの組み込み関数FUNCTION STANDARD-DEVIATIONに対応する。<br>
     * 引数として与えられた数値の母標準偏差(母分散の平方根)を返す。<br>
     * 引数が1個の場合は0を返す。
     *
     * @param prams 引数の個数
     * @param fields 標準偏差を求める対象となる数値フィールドの並び
     * @return 母標準偏差を保持する数値フィールド。処理中にエラーが発生した場合はnull
     * @throws CobolStopRunException 演算処理中にランタイムエラーが発生した場合
     */
    public static AbstractCobolField funcStandardDeviation(int prams, AbstractCobolField... fields)
            throws CobolStopRunException {
        CobolFieldAttribute attr =
                new CobolFieldAttribute(
                        CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY,
                        18,
                        0,
                        CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
                        null);
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(8, (CobolDataStorage) null, attr);

        makeDoubleEntry();

        if (fields.length == 1) {
            makeFieldEntry(field);
            currField.setInt(0);
            return currField;
        }

        CobolDecimal d1 = new CobolDecimal(BigDecimal.ZERO, 0);
        CobolDecimal d2 = new CobolDecimal();

        for (AbstractCobolField f : fields) {
            d2.setField(f);
            d1.add(d2);
        }

        d2.setValue(new BigDecimal(fields.length));
        d2.setScale(0);
        try {
            d1.div(d2);
        } catch (CobolStopRunException e) {
            return null;
        }

        CobolDecimal d4 = new CobolDecimal(BigDecimal.ZERO, 0);

        for (AbstractCobolField f : fields) {
            d2.setField(f);
            d2.sub(d1);
            d2.mul(d2);
            d4.add(d2);
        }

        CobolDecimal d3 = new CobolDecimal(new BigDecimal(fields.length), 0);
        try {
            d4.div(d3);
        } catch (CobolStopRunException e) {
            return null;
        }
        d4.getField(currField, 0);
        return funcSqrt(currField);
    }

    // libcob/intrinsicのcob_intr_present_valueの実装
    /**
     * COBOLの組み込み関数FUNCTION PRESENT-VALUEに対応する。<br>
     * 第1引数を割引率とし、第2引数以降を将来の各期の収支とみなして、それらの現在価値の合計を返す。<br>
     * 引数が2個未満の場合はエラーメッセージを出力し0を返す。
     *
     * @param prams 引数の個数
     * @param fields 先頭が割引率、それ以降が各期の収支を表す数値フィールドの並び
     * @return 現在価値の合計を保持するdouble型フィールド
     * @throws CobolStopRunException 演算処理中にランタイムエラーが発生した場合
     */
    public static AbstractCobolField funcPresentValue(int prams, AbstractCobolField... fields)
            throws CobolStopRunException {
        makeDoubleEntry();
        if (fields.length < 2) {
            System.err.println("Wrong number of parameters for FUNCTION PRESENT-VALUE");
            System.err.flush();
            currField.setInt(0);
            return currField;
        }
        AbstractCobolField f = fields[0];
        CobolDecimal d1 = new CobolDecimal();
        d1.setField(f);
        CobolDecimal d2 = new CobolDecimal(BigDecimal.ONE, 0);
        d1.add(d2);
        CobolDecimal d4 = new CobolDecimal(BigDecimal.ZERO, 0);

        for (int i = 1; i < fields.length; ++i) {
            f = fields[i];
            d2.setField(f);
            CobolDecimal d3 = new CobolDecimal(d1.getValue().add(BigDecimal.ZERO), d1.getScale());
            if (i > 1) {
                CobolDecimal d5 = new CobolDecimal(new BigDecimal(i), 0);
                d3.pow(d5);
            }
            d2.div(d3);
            d4.add(d2);
        }

        d4.getField(currField, 0);
        return currField;
    }

    // libcob/intrinsicのcob_intr_nationalの実装
    /**
     * COBOLの組み込み関数FUNCTION NATIONALに対応する。<br>
     * 引数の半角文字(英数字)を全角の日本語文字(各国文字)に変換した結果を返す。
     *
     * @param srcfield 変換対象の文字列フィールド
     * @return 各国文字に変換した結果を保持するフィールド
     */
    public static AbstractCobolField funcNational(AbstractCobolField srcfield) {
        int size = srcfield.getSize();
        byte[] pdata =
                CobolNationalField.han2zen(
                        srcfield.getDataStorage().getByteBuffer(size).array(), size);
        int ndata = CobolNationalField.workReturnSize;
        CobolFieldAttribute attr =
                new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_NATIONAL, 0, 0, 0, null);
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(ndata, (CobolDataStorage) null, attr);
        makeFieldEntry(field);
        currField.getDataStorage().memcpy(pdata, ndata);
        return currField;
    }

    // cob_intr_combined_datetimeの実装
    /**
     * COBOLの組み込み関数FUNCTION COMBINED-DATETIMEに対応する。<br>
     * 通日(整数日付)と時刻(0時からの経過秒数)を結合し、「DDDDDDDttttt」形式の
     * 12桁の数値(整数部7桁が通日、小数部5桁が時刻)を返す。<br>
     * 通日が有効範囲(1〜3067671)外、または時刻が有効範囲(1〜86400)外の場合は
     * 例外を設定し、結果を0とする。
     *
     * @param srcdays 通日(整数)を保持するフィールド
     * @param srctime 0時からの経過秒数を保持するフィールド
     * @return 通日と時刻を結合した数値フィールド
     */
    public static AbstractCobolField funcCombinedDatetime(
            AbstractCobolField srcdays, AbstractCobolField srctime) {
        int srdays;
        int srtime;
        String str;

        CobolFieldAttribute attr =
                new CobolFieldAttribute(
                        CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY, 12, 5, 0, null);
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(12, (CobolDataStorage) null, attr);
        makeFieldEntry(field);
        CobolRuntimeException.setException(0);
        srdays = srcdays.getInt();
        if (srdays < 1 || srdays > 3067671) {
            CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
            currField.getDataStorage().memset(0, 12);
            return currField;
        }
        srtime = srctime.getInt();
        if (srtime < 1 || srtime > 86400) {
            CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
            currField.getDataStorage().memset(0, 12);
            return currField;
        }
        str = String.format("%7d%5d", srdays, srtime);
        byte[] buff = str.getBytes(AbstractCobolField.charSetSJIS);
        for (int i = 0; i < buff.length; i++) {
            if (buff[i] == ' ') {
                buff[i] = '0';
            }
        }
        currField.getDataStorage().memcpy(buff);
        return currField;
    }

    // cob_intr_concatenateの実装
    /**
     * COBOLの組み込み関数FUNCTION CONCATENATEに対応する。<br>
     * 引数として与えられたすべての文字列フィールドを順に連結した結果を返す。
     *
     * @param offset 結果に対する部分参照の開始位置(1始まり)。0以下の場合は部分参照を行わない
     * @param length 結果に対する部分参照の長さ
     * @param params 連結対象の引数の個数
     * @param fields 連結する文字列フィールドの並び
     * @return 連結した結果を保持するフィールド
     */
    public static AbstractCobolField funcConcatenate(
            int offset, int length, int params, AbstractCobolField... fields) {
        int calcsize = 0;
        int i;
        int index = 0;
        int size;
        byte[] data;

        for (i = 0; i < params; i++) {
            calcsize += fields[i].getSize();
        }
        CobolFieldAttribute attr =
                new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null);
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(calcsize, (CobolDataStorage) null, attr);
        makeFieldEntry(field);
        data = new byte[calcsize];
        for (i = 0; i < params; i++) {
            size = fields[i].getSize();
            System.arraycopy(
                    fields[i].getDataStorage().getByteBuffer(size).array(), 0, data, index, size);
            index += size;
        }
        currField.setDataStorage(new CobolDataStorage(data));
        if (offset > 0) {
            calcRefMod(currField, offset, length);
        }
        return currField;
    }

    // cob_intr_date_to_yyyymmddの実装
    /**
     * COBOLの組み込み関数FUNCTION DATE-TO-YYYYMMDDに対応する。<br>
     * 2桁年の日付「YYMMDD」を、ウィンドウ方式(基準年と区間)により4桁年の日付「YYYYMMDD」に変換して返す。<br>
     * 第2引数で区間(省略時は50)、第3引数で実行年(省略時はシステム日付)を指定できる。<br>
     * 引数が有効範囲外の場合は例外を設定し、0を返す。
     *
     * @param params 引数の個数
     * @param fields 先頭が「YYMMDD」形式の日付、第2要素が区間、第3要素が実行年を表す数値フィールドの並び
     * @return 4桁年の日付「YYYYMMDD」を保持する数値フィールド
     */
    public static AbstractCobolField funcDateToYyyymmdd(int params, AbstractCobolField... fields) {
        int year;
        int mmdd;
        int interval;
        int xqtyear;
        int maxyear;
        OffsetDateTime timeptr;

        CobolFieldAttribute attr =
                new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY, 8, 0, 0, null);
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(4, (CobolDataStorage) null, attr);
        makeFieldEntry(field);
        year = fields[0].getInt();
        mmdd = year % 10000;
        year /= 10000;
        if (params > 1) {
            interval = fields[1].getInt();
        } else {
            interval = 50;
        }
        if (params > 2) {
            xqtyear = fields[2].getInt();
        } else {
            timeptr = CobolUtil.localtime();
            xqtyear = 1900 + timeptr.getDayOfYear();
        }
        if (year < 0 || year > 999999) {
            CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
            currField.setInt(0);
            return currField;
        }
        if (xqtyear < 1601 || xqtyear > 9999) {
            CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
            currField.setInt(0);
            return currField;
        }
        maxyear = xqtyear + interval;
        if (maxyear < 1700 || maxyear > 9999) {
            CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
            currField.setInt(0);
            return currField;
        }
        if (maxyear % 100 >= year) {
            year += 100 * (maxyear / 100);
        } else {
            year += 100 * ((maxyear / 100) - 1);
        }
        year *= 10000;
        year += mmdd;
        currField.setInt(year);
        return currField;
    }

    // cob_intr_day_to_yyyydddの実装
    /**
     * COBOLの組み込み関数FUNCTION DAY-TO-YYYYDDDに対応する。<br>
     * 2桁年の年間通日「YYDDD」を、ウィンドウ方式(基準年と区間)により4桁年の年間通日「YYYYDDD」に変換して返す。<br>
     * 第2引数で区間(省略時は50)、第3引数で実行年(省略時はシステム日付)を指定できる。<br>
     * 引数が有効範囲外の場合は例外を設定し、0を返す。
     *
     * @param params 引数の個数
     * @param fields 先頭が「YYDDD」形式の年間通日、第2要素が区間、第3要素が実行年を表す数値フィールドの並び
     * @return 4桁年の年間通日「YYYYDDD」を保持する数値フィールド
     */
    public static AbstractCobolField funcDayToYyyyddd(int params, AbstractCobolField... fields) {
        int year;
        int days;
        int interval;
        int xqtyear;
        int maxyear;
        OffsetDateTime timeptr;

        CobolFieldAttribute attr =
                new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY, 8, 0, 0, null);
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(4, (CobolDataStorage) null, attr);
        makeFieldEntry(field);
        year = fields[0].getInt();
        days = year % 1000;
        year /= 1000;
        if (params > 1) {
            interval = fields[1].getInt();
        } else {
            interval = 50;
        }
        if (params > 2) {
            xqtyear = fields[2].getInt();
        } else {
            timeptr = CobolUtil.localtime();
            xqtyear = 1900 + timeptr.getDayOfYear();
        }

        if (year < 0 || year > 999999) {
            CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
            currField.setInt(0);
            return currField;
        }
        if (xqtyear < 1601 || xqtyear > 9999) {
            CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
            currField.setInt(0);
            return currField;
        }
        maxyear = xqtyear + interval;
        if (maxyear < 1700 || maxyear > 9999) {
            CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
            currField.setInt(0);
            return currField;
        }
        if (maxyear % 100 >= year) {
            year += 100 * (maxyear / 100);
        } else {
            year += 100 * ((maxyear / 100) - 1);
        }
        year *= 1000;
        year += days;
        currField.setInt(year);
        return currField;
    }

    // cob_intr_exception_fileの実装
    /**
     * COBOLの組み込み関数FUNCTION EXCEPTION-FILEに対応する。<br>
     * 直近に発生した入出力例外に関するファイル情報(2桁のファイルステータスと、続くファイル名)を返す。<br>
     * 入出力例外が発生していない場合はファイルステータス"00"のみを返す。
     *
     * @return ファイルステータスとファイル名を保持する文字列フィールド
     */
    public static AbstractCobolField funcExceptionFile() {
        int flen;
        byte[] data;

        CobolFieldAttribute attr =
                new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null);
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(0, (CobolDataStorage) null, attr);
        if (CobolRuntimeException.getException() == 0
                || (CobolRuntimeException.getExceptionCode() & 0x0500) != 0x0500) {
            field.setSize(2);
            makeFieldEntry(field);
            currField.memcpy(byteArray00, 2);
        } else {
            flen = CobolFile.errorFile.getSelectName().length();
            field.setSize(flen + 2);
            makeFieldEntry(field);
            data = new byte[2 + flen];
            System.arraycopy(CobolFile.errorFile.getFileStatus(), 0, data, 0, 2);
            System.arraycopy(
                    CobolFile.errorFile.getSelectName().getBytes(AbstractCobolField.charSetSJIS),
                    0,
                    data,
                    2,
                    flen);
            currField.setDataStorage(new CobolDataStorage(data));
        }
        return currField;
    }

    // cob_intr_exception_locationの実装
    /**
     * COBOLの組み込み関数FUNCTION EXCEPTION-LOCATIONに対応する。<br>
     * 直近に発生した例外の発生箇所(プログラムID、節・段落名、行番号)を
     * 「プログラムID; 段落 OF 節; 行番号」などの形式の文字列として返す。<br>
     * 例外が発生していない場合は空白1文字を返す。
     *
     * @return 例外発生箇所を表す文字列フィールド
     */
    public static AbstractCobolField funcExceptionLocation() {
        String buff;

        CobolFieldAttribute attr =
                new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null);
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(0, (CobolDataStorage) null, attr);
        currField = field;
        if (CobolRuntimeException.getException() != 1
                || CobolRuntimeException.getOrigProgramId() == null) {
            field.setSize(1);
            makeFieldEntry(field);
            currField.getDataStorage().setByte(0, ' ');
            return currField;
        }
        if (CobolRuntimeException.getOrigSection() != null
                && CobolRuntimeException.getOrigParagraph() != null) {
            buff =
                    String.format(
                            "%s; %s OF %s; %d",
                            CobolRuntimeException.getOrigProgramId(),
                            CobolRuntimeException.getOrigParagraph(),
                            CobolRuntimeException.getOrigSection(),
                            CobolRuntimeException.getOrigLine());
        } else if (CobolRuntimeException.getOrigSection() != null) {
            buff =
                    String.format(
                            "%s; %s; %d",
                            CobolRuntimeException.getOrigProgramId(),
                            CobolRuntimeException.getOrigSection(),
                            CobolRuntimeException.getOrigLine());
        } else if (CobolRuntimeException.getOrigParagraph() != null) {
            buff =
                    String.format(
                            "%s; %s; %d",
                            CobolRuntimeException.getOrigProgramId(),
                            CobolRuntimeException.getOrigParagraph(),
                            CobolRuntimeException.getOrigLine());
        } else {
            buff =
                    String.format(
                            "%s; ; %d",
                            CobolRuntimeException.getOrigProgramId(),
                            CobolRuntimeException.getOrigLine());
        }
        localeBuff = buff.getBytes(AbstractCobolField.charSetSJIS);
        field.setSize(localeBuff.length);
        currField.setDataStorage(new CobolDataStorage(localeBuff));
        return currField;
    }

    // cob_intr_exception_statementの実装
    /**
     * COBOLの組み込み関数FUNCTION EXCEPTION-STATEMENTに対応する。<br>
     * 直近に発生した例外を引き起こした文(STATEMENT)の名前を、31桁に左詰めした文字列として返す。<br>
     * 該当する文が無い場合は空白を返す。
     *
     * @return 例外を引き起こした文の名前を表す31桁の文字列フィールド
     */
    public static AbstractCobolField funcExceptionStatement() {
        CobolFieldAttribute attr =
                new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null);
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(31, (CobolDataStorage) null, attr);
        makeFieldEntry(field);
        byte[] data;
        if (CobolRuntimeException.getExceptionCode() != 0
                && CobolRuntimeException.getOrigStatement() != null) {
            data =
                    String.format("%-31s", CobolRuntimeException.getOrigStatement())
                            .getBytes(AbstractCobolField.charSetSJIS);
        } else {
            data = String.format("%-31s", "").getBytes(AbstractCobolField.charSetSJIS);
        }
        currField.setDataStorage(new CobolDataStorage(data));
        return currField;
    }

    /** 例外名が取得できなかった場合に用いる既定の例外名"EXCEPTION-OBJECT"のSJISバイト列。 */
    private static final byte[] CONST_STRING_EXCEPTION_OBJECT =
            "EXCEPTION-OBJECT".getBytes(AbstractCobolField.charSetSJIS);

    // cob_intr_exception_statusの実装
    /**
     * COBOLの組み込み関数FUNCTION EXCEPTION-STATUSに対応する。<br>
     * 直近に発生した例外の名前を、31桁に左詰めした文字列として返す。<br>
     * 例外が発生していない場合は空白を返す。
     *
     * @return 例外の名前を表す31桁の文字列フィールド
     */
    public static AbstractCobolField funcExceptionStatus() {
        byte[] exceptName;

        CobolFieldAttribute attr =
                new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null);
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(31, (CobolDataStorage) null, attr);
        makeFieldEntry(field);
        byte[] data = String.format("%-31s", "").getBytes(AbstractCobolField.charSetSJIS);
        currField.setDataStorage(new CobolDataStorage(data));
        if (CobolRuntimeException.getExceptionCode() != 0) {
            try {
                exceptName =
                        CobolRuntimeException.getExceptionName(
                                        CobolRuntimeException.getExceptionCode())
                                .getBytes(AbstractCobolField.charSetSJIS);
            } catch (Exception e) {
                exceptName = CONST_STRING_EXCEPTION_OBJECT;
            }
            currField.memcpy(exceptName, exceptName.length);
        }
        return currField;
    }

    // cob_intr_fraction_partの実装
    /**
     * COBOLの組み込み関数FUNCTION FRACTION-PARTに対応する。<br>
     * 引数の小数部(整数部を除いた値)を返す。
     *
     * @param srcfield 対象の数値フィールド
     * @return 小数部を保持する数値フィールド
     */
    public static AbstractCobolField funcFractionPart(AbstractCobolField srcfield) {
        CobolFieldAttribute attr =
                new CobolFieldAttribute(
                        CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY,
                        18,
                        18,
                        CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
                        null);
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(8, (CobolDataStorage) null, attr);
        makeFieldEntry(field);

        currField.moveFrom(srcfield);
        return currField;
    }

    // cob_intr_seconds_from_formatted_timeの実装
    /**
     * COBOLの組み込み関数FUNCTION SECONDS-FROM-FORMATTED-TIMEに対応する。<br>
     * 時刻書式文字列(時を"hh"、分を"mm"、秒を"ss"で表す)に従ってvalueを解析し、
     * その時刻を0時からの経過秒数に変換して返す。<br>
     * 時・分・秒のいずれかが書式中に現れない場合は例外を設定し、0を返す。
     *
     * @param format 時刻の書式を表すフィールド("hh"、"mm"、"ss"を含む)
     * @param value 書式に従った時刻文字列を保持するフィールド
     * @return 0時からの経過秒数を保持する数値フィールド
     */
    public static AbstractCobolField funcSecondsFromFormattedTime(
            AbstractCobolField format, AbstractCobolField value) {
        int n;
        int seconds = 0;
        int minutes = 0;
        int hours = 0;
        boolean secondsSeen = false;
        boolean minutesSeen = false;
        boolean hoursSeen = false;
        String p1;
        int p2;

        CobolFieldAttribute attr =
                new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY, 8, 0, 0, null);
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(4, (CobolDataStorage) null, attr);
        makeFieldEntry(field);

        if (value.getSize() < format.getSize()) {
            CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
            currField.setInt(0);
            return currField;
        }

        CobolDataStorage formatData = format.getDataStorage();
        CobolDataStorage valueData = value.getDataStorage();

        for (n = 0; n < format.getSize() - 1; n++) {
            p1 = new String(formatData.getByteArray(n, 2), AbstractCobolField.charSetSJIS);

            if ("hh".equals(p1) && !hoursSeen) {
                p2 =
                        Integer.parseInt(
                                new String(
                                        valueData.getByteArray(n, 2),
                                        AbstractCobolField.charSetSJIS));
                hours = p2;
                hoursSeen = true;
                continue;
            }
            if ("mm".equals(p1) && !minutesSeen) {
                p2 =
                        Integer.parseInt(
                                new String(
                                        valueData.getByteArray(n, 2),
                                        AbstractCobolField.charSetSJIS));
                minutes = p2;
                minutesSeen = true;
                continue;
            }
            if ("ss".equals(p1) && !secondsSeen) {
                p2 =
                        Integer.parseInt(
                                new String(
                                        valueData.getByteArray(n, 2),
                                        AbstractCobolField.charSetSJIS));
                seconds = p2;
                secondsSeen = true;
                continue;
            }
        }

        if (hoursSeen && minutesSeen && secondsSeen) {
            seconds += hours * 3600 + minutes * 60;
        } else {
            CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
            seconds = 0;
        }
        currField.setInt(seconds);
        return currField;
    }

    // cob_intr_seconds_past_midnightの実装
    /**
     * COBOLの組み込み関数FUNCTION SECONDS-PAST-MIDNIGHTに対応する。<br>
     * 現在時刻の、0時からの経過秒数(時 * 3600 + 分 * 60 + 秒)を返す。
     *
     * @return 0時からの経過秒数を保持する数値フィールド
     */
    public static AbstractCobolField funcSecondsPastMidnight() {
        int seconds;
        CobolFieldAttribute attr =
                new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY, 8, 0, 0, null);
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(4, (CobolDataStorage) null, attr);
        makeFieldEntry(field);
        LocalDateTime currDate = LocalDateTime.now();
        seconds = currDate.getHour() * 3600 + currDate.getMinute() * 60 + currDate.getSecond();
        currField.setInt(seconds);
        return currField;
    }

    // cob_intr_signの実装
    /**
     * COBOLの組み込み関数FUNCTION SIGNに対応する。<br>
     * 引数の符号に応じて、負の場合は-1、0の場合は0、正の場合は1を返す。
     *
     * @param srcfield 対象の数値フィールド
     * @return 符号を表す数値フィールド(-1、0、または1)
     */
    public static AbstractCobolField funcSign(AbstractCobolField srcfield) {
        CobolFieldAttribute attr =
                new CobolFieldAttribute(
                        CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY,
                        8,
                        0,
                        CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
                        null);
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(4, (CobolDataStorage) null, attr);
        makeFieldEntry(field);

        currField.setInt(0);
        int n = srcfield.compareTo(currField);
        if (n < 0) {
            currField.setInt(-1);
        } else if (n > 0) {
            currField.setInt(1);
        }
        return currField;
    }

    // cob_intr_stored_char_lengthの実装
    /**
     * COBOLの組み込み関数FUNCTION STORED-CHAR-LENGTHに対応する。<br>
     * 引数の文字列から末尾の空白を取り除いた、実質的な格納文字数を返す。
     *
     * @param srcfield 対象の文字列フィールド
     * @return 末尾の空白を除いた文字数を保持する数値フィールド
     */
    public static AbstractCobolField funcStoredCharLength(AbstractCobolField srcfield) {
        int count;

        CobolFieldAttribute attr =
                new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY, 8, 0, 0, null);
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(4, (CobolDataStorage) null, attr);
        makeFieldEntry(field);

        CobolDataStorage storage = srcfield.getDataStorage();
        for (count = srcfield.getSize(); count > 0; count--) {
            if (storage.getByte(count - 1) != ' ') {
                break;
            }
        }

        currField.setInt(count);
        return currField;
    }

    /**
     * COBOLの組み込み関数FUNCTION SUBSTITUTEに対応する。<br>
     * 先頭の引数を対象文字列とし、続く引数を(検索文字列, 置換文字列)の組として、
     * 対象文字列中に出現する各検索文字列を対応する置換文字列に置き換えた結果を返す。<br>
     * 大文字・小文字を区別して比較する。
     *
     * @param offset 結果に対する部分参照の開始位置(1始まり)。0以下の場合は部分参照を行わない
     * @param length 結果に対する部分参照の長さ
     * @param params 引数の個数(先頭の対象文字列と、それに続く検索・置換の組)
     * @param fields 先頭が対象文字列、それ以降が検索文字列と置換文字列を交互に並べたフィールドの並び
     * @return 置換した結果を保持するフィールド
     */
    public static AbstractCobolField funcSubstitute(
            int offset, int length, int params, AbstractCobolField... fields) {
        int i, j, k;
        int numreps = params / 2;
        AbstractCobolField[] f1 = new AbstractCobolField[numreps];
        AbstractCobolField[] f2 = new AbstractCobolField[numreps];
        CobolDataStorage src = fields[0].getDataStorage();
        CobolDataStorage fData1;
        int srcSize = fields[0].getSize();
        int fSize1;
        StringBuilder rtn = new StringBuilder();

        for (i = 0; i < params - 1; i++) {
            if ((i % 2) == 0) {
                f1[i / 2] = fields[i + 1];
            } else {
                f2[i / 2] = fields[i + 1];
            }
        }

        for (i = 0; i < srcSize; ) {
            for (j = 0; j < numreps; j++) {
                fData1 = f1[j].getDataStorage();
                fSize1 = f1[j].getSize();
                for (k = fSize1 - 1; k >= 0; k--) {
                    if (i + k >= srcSize || src.getByte(i + k) != fData1.getByte(k)) {
                        break;
                    }
                }
                if (k < 0) {
                    rtn.append(f2[j].getString());
                    i += fSize1;
                    break;
                }
            }
            if (j == numreps) {
                rtn.append((char) src.getByte(i));
                i++;
            }
        }

        CobolFieldAttribute attr =
                new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null);
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(rtn.length(), (CobolDataStorage) null, attr);
        makeFieldEntry(field);
        currField.setDataStorage(new CobolDataStorage(rtn.toString()));

        if (offset > 0) {
            calcRefMod(currField, offset, length);
        }
        return currField;
    }

    /**
     * COBOLの組み込み関数FUNCTION SUBSTITUTE-CASEに対応する。<br>
     * 先頭の引数を対象文字列とし、続く引数を(検索文字列, 置換文字列)の組として、
     * 対象文字列中に出現する各検索文字列を対応する置換文字列に置き換えた結果を返す。<br>
     * 大文字・小文字を区別せずに比較する。
     *
     * @param offset 結果に対する部分参照の開始位置(1始まり)。0以下の場合は部分参照を行わない
     * @param length 結果に対する部分参照の長さ
     * @param params 引数の個数(先頭の対象文字列と、それに続く検索・置換の組)
     * @param fields 先頭が対象文字列、それ以降が検索文字列と置換文字列を交互に並べたフィールドの並び
     * @return 置換した結果を保持するフィールド
     */
    public static AbstractCobolField funcSubstituteCase(
            int offset, int length, int params, AbstractCobolField... fields) {
        int i, j, k;
        int numreps = params / 2;
        AbstractCobolField[] f1 = new AbstractCobolField[numreps];
        AbstractCobolField[] f2 = new AbstractCobolField[numreps];
        CobolDataStorage src = fields[0].getDataStorage();
        CobolDataStorage fData1;
        int srcSize = fields[0].getSize();
        int fSize1;
        StringBuilder rtn = new StringBuilder();

        for (i = 0; i < params - 1; i++) {
            if (i % 2 == 0) {
                f1[i / 2] = fields[i + 1];
            } else {
                f2[i / 2] = fields[i + 1];
            }
        }

        for (i = 0; i < srcSize; ) {
            for (j = 0; j < numreps; j++) {
                fData1 = f1[j].getDataStorage();
                fSize1 = f1[j].getSize();
                for (k = fSize1 - 1; k >= 0; k--) {
                    if (i + k >= srcSize
                            || Character.toLowerCase((char) fData1.getByte(k))
                                    != Character.toLowerCase((char) src.getByte(i + k))) {
                        break;
                    }
                }
                if (k < 0) {
                    rtn.append(f2[j].getString());
                    i += fSize1;
                    break;
                }
            }
            if (j == numreps) {
                rtn.append((char) src.getByte(i));
                i++;
            }
        }

        CobolFieldAttribute attr =
                new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null);
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(rtn.length(), (CobolDataStorage) null, attr);
        makeFieldEntry(field);

        currField.setDataStorage(new CobolDataStorage(rtn.toString()));
        if (offset > 0) {
            calcRefMod(currField, offset, length);
        }
        return currField;
    }

    // Equivalent to cob_intr_trim
    /**
     * COBOLの組み込み関数FUNCTION TRIMに対応する。<br>
     * 引数の文字列の前後の空白を取り除いた結果を返す。directionにより、取り除く位置を切り替える。<br>
     * 文字列が空白のみの場合は空白1文字を返す。
     *
     * @param offset 結果に対する部分参照の開始位置(1始まり)。0以下の場合は部分参照を行わない
     * @param length 結果に対する部分参照の長さ
     * @param srcField 空白を取り除く対象の文字列フィールド
     * @param direction 取り除く位置の指定。1は先頭の空白のみ、2は末尾の空白のみ、それ以外は前後両方の空白を取り除く
     * @return 空白を取り除いた結果を保持するフィールド
     */
    public static AbstractCobolField funcTrim(
            int offset, int length, AbstractCobolField srcField, int direction) {
        makeFieldEntry(srcField);
        int i;
        int srcFieldSize = srcField.getSize();
        CobolDataStorage srcStorage = srcField.getDataStorage();
        for (i = 0; i < srcFieldSize; ++i) {
            if (srcStorage.getByte(i) != ' ') {
                break;
            }
        }
        if (i == srcFieldSize) {
            currField.setSize(1);
            currField.getDataStorage().setByte(0, (byte) ' ');
            return currField;
        }
        int beginIndex = 0;
        if (direction != 2) {
            while (srcStorage.getByte(beginIndex) == ' ') {
                ++beginIndex;
            }
        }
        int endIndex = srcFieldSize - 1;
        if (direction != 1) {
            while (srcStorage.getByte(endIndex) == ' ') {
                --endIndex;
            }
        }
        CobolDataStorage currStorage = currField.getDataStorage();
        currField.setSize(endIndex - beginIndex + 1);
        for (i = 0; i <= endIndex - beginIndex; ++i) {
            currStorage.setByte(i, srcStorage.getByte(beginIndex + i));
        }
        if (offset > 0) {
            calcRefMod(currField, offset, length);
        }
        return currField;
    }

    /**
     * COBOLの組み込み関数FUNCTION LOCALE-DATEに対応するオーバーロード。<br>
     * ロケールの指定が省略された呼び出しに対応し、既定のロケールで
     * {@link #funcLocaleDate(int, int, AbstractCobolField, AbstractCobolField)}を実行する。
     *
     * @param offset 結果に対する部分参照の開始位置(1始まり)。0以下の場合は部分参照を行わない
     * @param length 結果に対する部分参照の長さ
     * @param srcField 「YYYYMMDD」形式の日付を保持するフィールド
     * @param localeField ロケールフィールドが指定されなかったことを表すダミー引数
     * @return ロケールに従って整形した日付文字列を保持するフィールド
     */
    public static AbstractCobolField funcLocaleDate(
            int offset, int length, AbstractCobolField srcField, int localeField) {
        return funcLocaleDate(offset, length, srcField, null);
    }

    /**
     * COBOLの組み込み関数FUNCTION LOCALE-DATEに対応する。<br>
     * 「YYYYMMDD」形式の日付を、指定されたロケール(省略時は既定のロケール)の書式に従って整形した
     * 日付文字列に変換して返す。<br>
     * 日付の各要素が有効範囲外の場合は例外を設定し、"0000000000"を返す。
     *
     * @param offset 結果に対する部分参照の開始位置(1始まり)。0以下の場合は部分参照を行わない
     * @param length 結果に対する部分参照の長さ
     * @param srcField 「YYYYMMDD」形式の日付を保持するフィールド(数値・文字列いずれも可)
     * @param localeField ロケール名を保持するフィールド。nullの場合は既定のロケールを用いる
     * @return ロケールに従って整形した日付文字列を保持するフィールド
     */
    public static AbstractCobolField funcLocaleDate(
            int offset, int length, AbstractCobolField srcField, AbstractCobolField localeField) {
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(
                        0,
                        (CobolDataStorage) null,
                        new CobolFieldAttribute(
                                CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 10, 0, 0, null));
        int inDate;

        // Convert the input field to an integer
        if (srcField.getAttribute().isTypeNumeric()) {
            inDate = srcField.getInt();
        } else {
            if (srcField.getSize() < 8) {
                return errorFuncLocaleDate(field);
            }
            int p = 0;
            inDate = 0;
            for (int len = 0; len < 8; ++len, ++p) {
                char c = (char) srcField.getDataStorage().getByte(p);
                if ('0' <= c && c <= '9') {
                    inDate = inDate * 10 + (c - '0');
                } else {
                    return errorFuncLocaleDate(field);
                }
            }
        }

        // Calculate the year, month, and days
        int year = inDate / 10000;
        if (year < 1601 || year > 9999) {
            return errorFuncLocaleDate(field);
        }
        inDate %= 10000;

        int month = inDate / 100;
        if (month < 1 || month > 12) {
            return errorFuncLocaleDate(field);
        }

        int days = inDate % 100;
        if (days < 1 || days > 31) {
            return errorFuncLocaleDate(field);
        }

        if (isLeapYear(year)) {
            if (days > leapMonthDays[month]) {
                return errorFuncLocaleDate(field);
            }
        } else {
            if (days > normalMonthDays[month]) {
                return errorFuncLocaleDate(field);
            }
        }

        // Create the date string
        Calendar cal = Calendar.getInstance();
        cal.set(year, month - 1, days);

        DateFormat df;
        if (localeField != null) {
            Locale locale = new Locale(localeField.getString());
            df = DateFormat.getDateInstance(DateFormat.SHORT, locale);
        } else {
            df = DateFormat.getDateInstance(DateFormat.SHORT);
        }
        String dateString = df.format(cal.getTime());

        // Return the result
        field.setSize(dateString.length());
        makeFieldEntry(field);
        currField.getDataStorage().memcpy(dateString.getBytes(AbstractCobolField.charSetSJIS));
        if (offset > 0) {
            calcRefMod(field, offset, length);
        }
        return currField;
    }

    /**
     * {@link #funcLocaleDate}や{@link #funcLocaleTime}などの引数が不正な場合のエラー処理。<br>
     * 結果フィールドを10桁の"0"で埋め、引数エラーの例外を設定して返す。
     *
     * @param field エラー結果を格納するフィールド
     * @return "0000000000"を保持する結果フィールド
     */
    private static AbstractCobolField errorFuncLocaleDate(AbstractCobolField field) {
        field.setSize(10);
        makeFieldEntry(field);
        currField.getDataStorage().memset((byte) '0', 10);
        CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
        return currField;
    }

    /**
     * COBOLの組み込み関数FUNCTION LOCALE-TIMEに対応するオーバーロード。<br>
     * ロケールの指定が省略された呼び出しに対応し、既定のロケールで
     * {@link #funcLocaleTime(int, int, AbstractCobolField, AbstractCobolField)}を実行する。
     *
     * @param offset 結果に対する部分参照の開始位置(1始まり)。0以下の場合は部分参照を行わない
     * @param length 結果に対する部分参照の長さ
     * @param srcField 「hhmmss」形式の時刻を保持するフィールド
     * @param localeField ロケールフィールドが指定されなかったことを表すダミー引数
     * @return ロケールに従って整形した時刻文字列を保持するフィールド
     */
    public static AbstractCobolField funcLocaleTime(
            int offset, int length, AbstractCobolField srcField, int localeField) {
        return funcLocaleTime(offset, length, srcField, null);
    }

    /**
     * COBOLの組み込み関数FUNCTION LOCALE-TIMEに対応する。<br>
     * 「hhmmss」形式の時刻を、指定されたロケール(省略時は既定のロケール)の書式に従って整形した
     * 時刻文字列に変換して返す。<br>
     * 時・分・秒のいずれかが有効範囲外の場合は例外を設定し、"0000000000"を返す。
     *
     * @param offset 結果に対する部分参照の開始位置(1始まり)。0以下の場合は部分参照を行わない
     * @param length 結果に対する部分参照の長さ
     * @param srcField 「hhmmss」形式の時刻を保持するフィールド(数値・文字列いずれも可)
     * @param localeField ロケール名を保持するフィールド。nullの場合は既定のロケールを用いる
     * @return ロケールに従って整形した時刻文字列を保持するフィールド
     */
    public static AbstractCobolField funcLocaleTime(
            int offset, int length, AbstractCobolField srcField, AbstractCobolField localeField) {
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(
                        0,
                        (CobolDataStorage) null,
                        new CobolFieldAttribute(
                                CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 10, 0, 0, null));
        int inTime;

        // Convert the input field to an integer
        if (srcField.getAttribute().isTypeNumeric()) {
            inTime = srcField.getInt();
        } else {
            if (srcField.getSize() < 6) {
                return errorFuncLocaleDate(field);
            }
            int p = 0;
            inTime = 0;
            for (int len = 0; len < 6; ++len, ++p) {
                char c = (char) srcField.getDataStorage().getByte(p);
                if ('0' <= c && c <= '9') {
                    inTime = inTime * 10 + (c - '0');
                } else {
                    return errorFuncLocaleDate(field);
                }
            }
        }

        // Calculate the hours, minutes, and seconds
        int hours = inTime / 10000;
        if (hours < 0 || hours > 24) {
            return errorFuncLocaleDate(field);
        }
        inTime %= 10000;

        int minutes = inTime / 100;
        if (minutes < 0 || minutes > 59) {
            return errorFuncLocaleDate(field);
        }

        int seconds = inTime % 100;
        if (seconds < 0 || seconds > 59) {
            return errorFuncLocaleDate(field);
        }

        // Create the time string
        LocalTime time = LocalTime.of(hours, minutes, seconds);

        DateTimeFormatter formatter;
        String pattern = "HH:mm:ss";
        if (localeField != null) {
            Locale locale = new Locale(localeField.getString());
            formatter = DateTimeFormatter.ofPattern(pattern, locale);
        } else {
            formatter = DateTimeFormatter.ofPattern(pattern);
        }
        String timeString = time.format(formatter);

        // Return the result
        field.setSize(timeString.length());
        makeFieldEntry(field);
        currField.getDataStorage().memcpy(timeString.getBytes(AbstractCobolField.charSetSJIS));
        if (offset > 0) {
            calcRefMod(field, offset, length);
        }
        return currField;
    }

    /**
     * COBOLの組み込み関数FUNCTION LOCALE-TIME-FROM-SECONDSに対応するオーバーロード。<br>
     * ロケールの指定が省略された呼び出しに対応し、既定のロケールで処理を実行する。
     *
     * @param offset 結果に対する部分参照の開始位置(1始まり)。0以下の場合は部分参照を行わない
     * @param length 結果に対する部分参照の長さ
     * @param srcField 0時からの経過秒数を保持するフィールド
     * @param localeField ロケールフィールドが指定されなかったことを表すダミー引数
     * @return ロケールに従って整形した時刻文字列を保持するフィールド
     */
    public static AbstractCobolField funcLocaleTimeFromSeconds(
            int offset, int length, AbstractCobolField srcField, int localeField) {
        return funcLocaleTime(offset, length, srcField, null);
    }

    /**
     * COBOLの組み込み関数FUNCTION LOCALE-TIME-FROM-SECONDSに対応する。<br>
     * 0時からの経過秒数を時・分・秒に変換し、指定されたロケール(省略時は既定のロケール)の書式に従って
     * 整形した時刻文字列に変換して返す。<br>
     * 引数が数値でない場合は例外を設定し、"0000000000"を返す。
     *
     * @param offset 結果に対する部分参照の開始位置(1始まり)。0以下の場合は部分参照を行わない
     * @param length 結果に対する部分参照の長さ
     * @param srcField 0時からの経過秒数を保持するフィールド
     * @param localeField ロケール名を保持するフィールド。nullの場合は既定のロケールを用いる
     * @return ロケールに従って整形した時刻文字列を保持するフィールド
     */
    public static AbstractCobolField funcLocaleTimeFromSeconds(
            int offset, int length, AbstractCobolField srcField, AbstractCobolField localeField) {
        AbstractCobolField field =
                CobolFieldFactory.makeCobolField(
                        0,
                        (CobolDataStorage) null,
                        new CobolFieldAttribute(
                                CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 10, 0, 0, null));
        int inTime;

        // Convert the input field to an integer
        if (srcField.getAttribute().isTypeNumeric()) {
            inTime = srcField.getInt();
        } else {
            return errorFuncLocaleDate(field);
        }

        // Calculate the hours, minutes, and seconds
        int hours = inTime / 3600;
        inTime %= 3600;
        int minutes = inTime / 60;
        int seconds = inTime % 60;

        // Create the time string
        LocalTime time = LocalTime.of(hours, minutes, seconds);

        DateTimeFormatter formatter;
        String pattern = "HH:mm:ss";
        if (localeField != null) {
            Locale locale = new Locale(localeField.getString());
            formatter = DateTimeFormatter.ofPattern(pattern, locale);
        } else {
            formatter = DateTimeFormatter.ofPattern(pattern);
        }
        String timeString = time.format(formatter);

        // Return the result
        field.setSize(timeString.length());
        makeFieldEntry(field);
        currField.getDataStorage().memcpy(timeString.getBytes(AbstractCobolField.charSetSJIS));
        if (offset > 0) {
            calcRefMod(field, offset, length);
        }
        return currField;
    }
}
