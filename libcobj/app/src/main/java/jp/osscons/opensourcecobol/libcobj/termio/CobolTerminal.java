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
package jp.osscons.opensourcecobol.libcobj.termio;

import java.io.InputStreamReader;
import java.io.PrintStream;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.charset.UnsupportedCharsetException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Scanner;
import jp.osscons.opensourcecobol.libcobj.common.CobolEncoding;
import jp.osscons.opensourcecobol.libcobj.common.CobolModule;
import jp.osscons.opensourcecobol.libcobj.common.CobolUtil;
import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldAttribute;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldFactory;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolExceptionId;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolExceptionInfo;

/** DISPLAY文やACCEPT文に関するメソッドを実装するクラス */
public class CobolTerminal {

    /** DISPLAY文で設定されたコマンドラインデータのバイト数 */
    private static int commlncnt = 0;

    /** DISPLAY文で設定されたコマンドラインデータのバイト配列 */
    private static byte[] commlnptr = null;

    /**
     * 標準出力または標準エラー出力にデータを出力する
     *
     * @param dispStdout trueなら標準出力に,それ以外は標準エラー出力に出力する
     * @param newline trueなら出力後に改行しない,それ以外の場合は改行する
     * @param fields 出力するCOBOL変数(可変長)
     */
    public static void display(boolean dispStdout, boolean newline, AbstractCobolField... fields) {
        PrintStream stream = dispStdout ? System.out : System.err;
        for (AbstractCobolField field : fields) {
            CobolFieldAttribute attr = field.getAttribute();
            if (attr.isTypeNumericBinary()
                    && CobolModule.getCurrentModule().flag_pretty_display == 0) {
                stream.print(field);
            } else if (attr.isTypeNumeric()) {
                stream.print(field);
            } else {
                displayAlnum(field, stream);
            }
        }
        if (newline) {
            stream.println("");
        }
    }

    private static void displayAlnum(AbstractCobolField f, PrintStream stream) {
        CobolDataStorage storage = f.getDataStorage();
        if (CobolUtil.terminalEncoding == CobolEncoding.UTF8) {
            byte[] utf8Bytes =
                    new String(storage.getByteArray(0, f.getSize()), AbstractCobolField.charSetSJIS)
                            .getBytes(StandardCharsets.UTF_8);
            stream.write(utf8Bytes, 0, utf8Bytes.length);
        } else {
            stream.write(storage.getRefOfData(), storage.getIndex(), f.getSize());
        }
    }

    /**
     * 標準出力または標準エラー出力にデータを出力する
     *
     * @param outorerr 0なら標準出力に,それ以外は標準エラー出力に出力する.
     * @param newline 0なら出力後に改行しない,それ以外の場合は改行する
     * @param varcnt 出力するCOBOL変数の数
     * @param fields 出力するCOBOL変数(可変長)
     */
    public static void display(
            int outorerr, int newline, int varcnt, AbstractCobolField... fields) {
        PrintStream stream = outorerr == 0 ? System.out : System.err;
        for (AbstractCobolField field : fields) {
            CobolFieldAttribute attr = field.getAttribute();
            if (attr.isTypeNumericBinary()
                    && CobolModule.getCurrentModule().flag_pretty_display == 0) {
                stream.print(field);
            } else if (attr.isTypeNumeric()) {
                stream.print(field);
            } else {
                displayAlnum(field, stream);
            }
        }
        if (newline == 1) {
            stream.println("");
        }
    }

    private static Scanner scan = null;

    /**
     * 標準入力からデータを受け取る
     *
     * @param f 入力データを格納するCOBOL変数
     */
    public static void accept(AbstractCobolField f) {
        try {
            if (scan == null) {
                if (CobolUtil.terminalEncoding == CobolEncoding.UTF8) {
                    scan = new Scanner(new InputStreamReader(System.in, StandardCharsets.UTF_8));
                } else {
                    try {
                        scan =
                                new Scanner(
                                        new InputStreamReader(
                                                System.in, Charset.forName("Shift_JIS")));
                    } catch (UnsupportedCharsetException e) {
                        scan = new Scanner(System.in);
                    }
                }
            }

            String input = scan.nextLine();

            // PIC X(n)型のデータに変換
            AbstractCobolField field = CobolFieldFactory.makeCobolField(input);

            if (f.getAttribute().isTypeNumericDisplay() && field.getSize() > f.getSize()) {
                field.setSize(f.getSize());
            }

            f.moveFrom(field);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    // Time

    /**
     * COB_DATE環境変数で設定された日時または現在時刻を返す. COB_DATE環境変数で設定された日時({@link
     * CobolUtil#cobLocalTm})が存在する場合はその値を返し, 設定されていない場合は現在時刻を返す.
     *
     * <p>libcob/common.cのjob_or_current_localtimeに対応する.
     *
     * @return COB_DATE環境変数で設定された日時または現在のローカル日時
     */
    private static LocalDateTime jobOrCurrentLocalTime() {
        if (CobolUtil.cobLocalTm != null) {
            return CobolUtil.cobLocalTm;
        } else {
            return LocalDateTime.now();
        }
    }

    /**
     * ACCEPT FROM DATE文の実装. 現在の日付を"yyMMdd"形式(2桁年+月+日)でCOBOL変数に格納する.
     *
     * @param f 日付データを格納するCOBOL変数
     */
    public static void acceptDate(AbstractCobolField f) {
        LocalDateTime date = jobOrCurrentLocalTime();
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyMMdd");
        f.memcpy(date.format(formatter));
    }

    /**
     * ACCEPT FROM DATE YYYYMMDD文の実装. 現在の日付を"yyyyMMdd"形式(4桁年+月+日)でCOBOL変数に格納する.
     *
     * @param f 日付データを格納するCOBOL変数
     */
    public static void acceptDate_yyyymmdd(AbstractCobolField f) {
        LocalDateTime date = jobOrCurrentLocalTime();
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyyMMdd");
        f.memcpy(date.format(formatter));
    }

    /**
     * ACCEPT FROM DAY文の実装. 現在の日付を"yyDDD"形式(2桁年+年間通算日)でCOBOL変数に格納する.
     *
     * @param f 日付データを格納するCOBOL変数
     */
    public static void acceptDay(AbstractCobolField f) {
        LocalDateTime date = jobOrCurrentLocalTime();
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyDDD");
        f.memcpy(date.format(formatter));
    }

    /**
     * ACCEPT FROM DAY YYYYDDD文の実装. 現在の日付を"yyyyDDD"形式(4桁年+年間通算日)でCOBOL変数に格納する.
     *
     * @param f 日付データを格納するCOBOL変数
     */
    public static void acceptDay_yyyyddd(AbstractCobolField f) {
        LocalDateTime date = jobOrCurrentLocalTime();
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyyDDD");
        f.memcpy(date.format(formatter));
    }

    /**
     * ACCEPT FROM DAY-OF-WEEK文の実装. 現在の曜日を1桁の数値(1=月曜日〜7=日曜日)でCOBOL変数に格納する.
     *
     * @param f 曜日データを格納するCOBOL変数
     */
    public static void acceptDayOfWeek(AbstractCobolField f) {
        LocalDateTime date = jobOrCurrentLocalTime();
        f.memcpy(String.format("%d", date.getDayOfWeek().getValue()));
    }

    /**
     * ACCEPT FROM TIME文の実装. 現在の時刻を"HHmmssSS"形式(時+分+秒+1/100秒)でCOBOL変数に格納する.
     *
     * <p>他の日付系メソッド({@link #acceptDate(AbstractCobolField)}等)とは異なり, {@link
     * #jobOrCurrentLocalTime()}を使用せず{@code LocalDateTime.now()}を直接呼び出すため, COB_DATE環境変数の設定は反映されない.
     *
     * @param f 時刻データを格納するCOBOL変数
     */
    public static void acceptTime(AbstractCobolField f) {
        LocalDateTime date = LocalDateTime.now();
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("HHmmssSS");
        f.memcpy(date.format(formatter));
    }

    // Environment

    /**
     * DISPLAY UPON ENVIRONMENT-NAME文の実装. 後続の{@link #displayEnvValue(AbstractCobolField)}や{@link
     * #acceptEnvironment(AbstractCobolField)}で使用する環境変数名を設定する.
     *
     * @param f 環境変数名を保持するCOBOL変数
     */
    public static void displayEnvironment(AbstractCobolField f) {
        CobolUtil.cobLocalEnv = f.fieldToString();
    }

    /**
     * DISPLAY UPON ENVIRONMENT-VALUE文の実装. {@link #displayEnvironment(AbstractCobolField)}
     * で設定された環境変数名に対して値を設定する. 環境変数名が未設定または空文字列の場合は{@link
     * CobolExceptionId#COB_EC_IMP_DISPLAY}例外を設定する.
     *
     * <p>COBOL変数{@code f}の値は{@link AbstractCobolField#fieldToString(java.nio.charset.Charset)}
     * によって末尾の空白とNULバイトが取り除かれてから環境変数値として設定される. これは本家CのopensourceCOBOLが
     * {@code cob_display_env_value}内で{@code cob_field_to_string}を使用する挙動と一致する.
     *
     * @param f 設定する環境変数の値を保持するCOBOL変数
     */
    public static void displayEnvValue(AbstractCobolField f) {
        if (CobolUtil.cobLocalEnv == null || CobolUtil.cobLocalEnv.equals("")) {
            CobolExceptionInfo.setException(CobolExceptionId.COB_EC_IMP_DISPLAY);
            return;
        }
        // Strip trailing spaces and NULs so that PIC X(n) source fields do not
        // leak padding into the environment variable value. Matches libcob's
        // cob_field_to_string used by cob_display_env_value.
        CobolUtil.setEnv(CobolUtil.cobLocalEnv, f.fieldToString(AbstractCobolField.charSetSJIS));
    }

    /**
     * ACCEPT FROM ENVIRONMENT-VALUE文の実装. {@link #displayEnvironment(AbstractCobolField)}
     * で設定された環境変数名の値を取得し,COBOL変数に格納する. 環境変数名が未設定の場合や環境変数が存在しない場合は{@link
     * CobolExceptionId#COB_EC_IMP_ACCEPT}例外を設定し,スペース1文字を格納する.
     *
     * @param f 環境変数の値を格納するCOBOL変数
     */
    public static void acceptEnvironment(AbstractCobolField f) {
        String p = null;
        if (CobolUtil.cobLocalEnv != null) {
            p = CobolUtil.getEnv(CobolUtil.cobLocalEnv);
        }

        if (p == null) {
            // TODO setExceptionは暫定実装
            CobolExceptionInfo.setException(CobolExceptionId.COB_EC_IMP_ACCEPT);
            p = " ";
        }

        AbstractCobolField src = CobolFieldFactory.makeCobolField(p);
        // TODO hankakuMoveFromは暫定実装
        f.hankakuMoveFrom(src);
    }

    /**
     * DISPLAY UPON COMMAND-LINE文の実装. COBOL変数の内容をコマンドラインデータとして内部バッファに保存する.
     * 保存されたデータは{@link #acceptCommandLine(AbstractCobolField)}で取得できる.
     *
     * @param f コマンドラインとして設定するデータを保持するCOBOL変数
     */
    public static void displayCommandLine(AbstractCobolField f) {
        CobolTerminal.commlnptr = new byte[f.getSize()];
        CobolTerminal.commlncnt = f.getSize();
        for (int i = 0; i < CobolTerminal.commlncnt; ++i) {
            CobolTerminal.commlnptr[i] = f.getDataStorage().getByte(i);
        }
    }

    /**
     * ACCEPT FROM COMMAND-LINE文の実装. {@link #displayCommandLine(AbstractCobolField)}
     * で設定されたコマンドラインデータが存在する場合はそのデータを返し,
     * 存在しない場合はプログラム起動時のコマンドライン引数をスペース区切りで結合した文字列をCOBOL変数に格納する.
     *
     * @param f コマンドラインデータを格納するCOBOL変数
     */
    public static void acceptCommandLine(AbstractCobolField f) {
        if (CobolTerminal.commlncnt != 0) {
            f.memcpy(CobolTerminal.commlnptr, CobolTerminal.commlncnt);
            return;
        }

        f.memcpy(String.join(" ", CobolUtil.commandLineArgs));
    }

    /**
     * DISPLAY UPON ARGUMENT-NUMBER文の実装. COBOL変数の値を現在の引数インデックスとして設定する.
     * 値が0未満またはコマンドライン引数の数を超える場合は{@link
     * CobolExceptionId#COB_EC_IMP_DISPLAY}例外を設定する.
     *
     * @param f 引数インデックス(1始まり)を保持するCOBOL変数
     */
    public static void displayArgNumber(AbstractCobolField f) {
        CobolFieldAttribute attr =
                new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY, 9, 0, 0, null);
        byte[] data = new byte[4];
        AbstractCobolField temp =
                CobolFieldFactory.makeCobolField(data.length, new CobolDataStorage(data), attr);
        temp.moveFrom(f);
        int n = ByteBuffer.wrap(data).getInt();
        if (n < 0 || n > CobolUtil.commandLineArgs.length) {
            CobolExceptionInfo.setException(CobolExceptionId.COB_EC_IMP_DISPLAY);
            return;
        }
        CobolUtil.currentArgIndex = n;
    }

    /**
     * ACCEPT FROM ARGUMENT-NUMBER文の実装. コマンドライン引数の総数をCOBOL変数に格納する.
     *
     * @param f 引数の総数を格納するCOBOL変数
     */
    public static void acceptArgNumber(AbstractCobolField f) {
        CobolFieldAttribute attr =
                new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY, 9, 0, 0, null);
        byte[] data = new byte[4];
        ByteBuffer.wrap(data).putInt(CobolUtil.commandLineArgs.length);
        AbstractCobolField temp =
                CobolFieldFactory.makeCobolField(data.length, new CobolDataStorage(data), attr);
        f.moveFrom(temp);
    }

    /**
     * ACCEPT FROM ARGUMENT-VALUE文の実装. {@link #displayArgNumber(AbstractCobolField)}
     * で設定された引数インデックスに対応するコマンドライン引数の値をCOBOL変数に格納し, インデックスを1つ進める.
     * インデックスが引数の総数を超えている場合は{@link CobolExceptionId#COB_EC_IMP_ACCEPT}例外を設定する.
     *
     * @param f 引数の値を格納するCOBOL変数
     */
    public static void acceptArgValue(AbstractCobolField f) {
        if (CobolUtil.currentArgIndex > CobolUtil.commandLineArgs.length) {
            CobolExceptionInfo.setException(CobolExceptionId.COB_EC_IMP_ACCEPT);
            return;
        }
        f.memcpy(CobolUtil.commandLineArgs[CobolUtil.currentArgIndex - 1]);
        CobolUtil.currentArgIndex++;
    }
}
