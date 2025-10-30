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

    /** TDOD: 準備中 */
    private static int commlncnt = 0;

    /** TDOD: 準備中 */
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
     * @param f TODO: 準備中
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
     * libcob/common.c job_or_current_localtime
     *
     * @return TODO: 準備中
     */
    private static LocalDateTime jobOrCurrentLocalTime() {
        if (CobolUtil.cobLocalTm != null) {
            return CobolUtil.cobLocalTm;
        } else {
            return LocalDateTime.now();
        }
    }

    /**
     * TODO: 準備中
     *
     * @param f TODO: 準備中
     */
    public static void acceptDate(AbstractCobolField f) {
        LocalDateTime date = jobOrCurrentLocalTime();
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyMMdd");
        f.memcpy(date.format(formatter));
    }

    /**
     * TODO: 準備中
     *
     * @param f TODO: 準備中
     */
    public static void acceptDate_yyyymmdd(AbstractCobolField f) {
        LocalDateTime date = jobOrCurrentLocalTime();
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyyMMdd");
        f.memcpy(date.format(formatter));
    }

    /**
     * TODO: 準備中
     *
     * @param f TODO: 準備中
     */
    public static void acceptDay(AbstractCobolField f) {
        LocalDateTime date = jobOrCurrentLocalTime();
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyDDD");
        f.memcpy(date.format(formatter));
    }

    /**
     * TODO: 準備中
     *
     * @param f TODO: 準備中
     */
    public static void acceptDay_yyyyddd(AbstractCobolField f) {
        LocalDateTime date = jobOrCurrentLocalTime();
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyyDDD");
        f.memcpy(date.format(formatter));
    }

    /**
     * TODO: 準備中
     *
     * @param f TODO: 準備中
     */
    public static void acceptDayOfWeek(AbstractCobolField f) {
        LocalDateTime date = jobOrCurrentLocalTime();
        f.memcpy(String.format("%d", date.getDayOfWeek().getValue()));
    }

    /**
     * TODO: 準備中
     *
     * @param f TODO: 準備中
     */
    public static void acceptTime(AbstractCobolField f) {
        LocalDateTime date = LocalDateTime.now();
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("HHmmssSS");
        f.memcpy(date.format(formatter));
    }

    // Environment

    /**
     * TODO: 準備中
     *
     * @param f TODO: 準備中
     */
    public static void displayEnvironment(AbstractCobolField f) {
        CobolUtil.cobLocalEnv = f.fieldToString();
    }

    /**
     * TODO: 準備中
     *
     * @param f TODO: 準備中
     */
    public static void displayEnvValue(AbstractCobolField f) {
        if (CobolUtil.cobLocalEnv == null || CobolUtil.cobLocalEnv.equals("")) {
            CobolExceptionInfo.setException(CobolExceptionId.COB_EC_IMP_DISPLAY);
            return;
        }
        CobolUtil.setEnv(CobolUtil.cobLocalEnv, f.getString());
    }

    /**
     * TODO: 準備中
     *
     * @param f TODO: 準備中
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
     * TODO: 準備中
     *
     * @param f TODO: 準備中
     */
    public static void displayCommandLine(AbstractCobolField f) {
        CobolTerminal.commlnptr = new byte[f.getSize()];
        CobolTerminal.commlncnt = f.getSize();
        for (int i = 0; i < CobolTerminal.commlncnt; ++i) {
            CobolTerminal.commlnptr[i] = f.getDataStorage().getByte(i);
        }
    }

    /**
     * TODO: 準備中
     *
     * @param f TODO: 準備中
     */
    public static void acceptCommandLine(AbstractCobolField f) {
        if (CobolTerminal.commlncnt != 0) {
            f.memcpy(CobolTerminal.commlnptr, CobolTerminal.commlncnt);
            return;
        }

        f.memcpy(String.join(" ", CobolUtil.commandLineArgs));
    }

    /**
     * TODO: 準備中
     *
     * @param f TODO: 準備中
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
     * TODO: 準備中
     *
     * @param f TODO: 準備中
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
     * TODO: 準備中
     *
     * @param f TODO: 準備中
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
