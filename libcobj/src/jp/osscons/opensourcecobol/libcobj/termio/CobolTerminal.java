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

import java.io.PrintStream;
import java.nio.ByteBuffer;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Scanner;
import jp.osscons.opensourcecobol.libcobj.common.CobolModule;
import jp.osscons.opensourcecobol.libcobj.common.CobolUtil;
import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldAttribute;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldFactory;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolException;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolExceptionId;

/** 標準出力,標準エラー出力に関するメソッドを実装するクラス */
public class CobolTerminal {

  /**
   * cob_displayの実装 TODO 暫定実装
   *
   * @param outorerr trueなら標準出力に,それ以外は標準エラー出力に出力する.
   * @param newline trueなら出力後に改行しない,それ以外の場合は改行する
   * @param fields 出力する変数(可変長)
   */
  public static void display(boolean dispStdout, boolean newline, AbstractCobolField... fields) {
    PrintStream stream = dispStdout ? System.out : System.err;
    for (AbstractCobolField field : fields) {
      CobolFieldAttribute attr = field.getAttribute();
      if (attr.isTypeNumericBinary() && CobolModule.getCurrentModule().flag_pretty_display == 0) {
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
    stream.write(storage.getRefOfData(), storage.getIndex(), f.getSize());
  }

  /**
   * cob_displayの実装 TODO 暫定実装
   *
   * @param outorerr 0なら標準出力に,それ以外は標準エラー出力に出力する.
   * @param newline 0なら出力後に改行しない,それ以外の場合は改行する
   * @param varcnt 出力する変数の数
   * @param fields 出力する変数(可変長)
   */
  public static void display(int outorerr, int newline, int varcnt, AbstractCobolField... fields) {
    PrintStream stream = outorerr == 0 ? System.out : System.err;
    for (AbstractCobolField field : fields) {
      CobolFieldAttribute attr = field.getAttribute();
      if (attr.isTypeNumericBinary() && CobolModule.getCurrentModule().flag_pretty_display == 0) {
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
   * cob_acceptの実装(暫定)
   *
   * @param f
   */
  public static void accept(AbstractCobolField f) {
    try {
      if (scan == null) {
        scan = new Scanner(System.in);
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
   * @return
   */
  private static LocalDateTime jobOrCurrentLocalTime() {
    if (CobolUtil.cobLocalTm != null) {
      return CobolUtil.cobLocalTm;
    } else {
      return LocalDateTime.now();
    }
  }

  /**
   * libcob/common.c cob_accept_date
   *
   * @param f
   */
  public static void acceptDate(AbstractCobolField f) {
    LocalDateTime date = jobOrCurrentLocalTime();
    DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyMMdd");
    f.memcpy(date.format(formatter));
  }

  /**
   * libcob/common.c cob_accept_date_yyyymmdd
   *
   * @param f
   */
  public static void acceptDate_yyyymmdd(AbstractCobolField f) {
    LocalDateTime date = jobOrCurrentLocalTime();
    DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyyMMdd");
    f.memcpy(date.format(formatter));
  }

  /**
   * libcob/common.c cob_accept_day
   *
   * @param f
   */
  public static void acceptDay(AbstractCobolField f) {
    LocalDateTime date = jobOrCurrentLocalTime();
    DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyDDD");
    f.memcpy(date.format(formatter));
  }

  /**
   * libcob/common.c cob_accept_date_yyyyddd
   *
   * @param f
   */
  public static void acceptDay_yyyyddd(AbstractCobolField f) {
    LocalDateTime date = jobOrCurrentLocalTime();
    DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyyDDD");
    f.memcpy(date.format(formatter));
  }

  /**
   * libcob/common.c cob_accept_day_of_week
   *
   * @param f
   */
  public static void acceptDayOfWeek(AbstractCobolField f) {
    LocalDateTime date = jobOrCurrentLocalTime();
    f.memcpy(String.format("%d", date.getDayOfWeek().getValue()));
  }

  /**
   * libcob/common.c cob_accept_time
   *
   * @param f
   */
  public static void acceptTime(AbstractCobolField f) {
    LocalDateTime date = LocalDateTime.now();
    DateTimeFormatter formatter = DateTimeFormatter.ofPattern("HHmmssSS");
    f.memcpy(date.format(formatter));
  }

  // Environment

  /**
   * libcob/common.c cob_display_environment
   *
   * @param f
   */
  public static void displayEnvironment(AbstractCobolField f) {
    CobolUtil.cobLocalEnv = f.fieldToString();
  }

  /**
   * libcob/common.c cob_display_env_value
   *
   * @param f
   */
  public static void displayEnvValue(AbstractCobolField f) {
    if (CobolUtil.cobLocalEnv == null || CobolUtil.cobLocalEnv == "") {
      CobolException.setException(CobolExceptionId.COB_EC_IMP_DISPLAY);
      return;
    }
    String env2 = f.fieldToString();
    // javaでは環境変数を変更できない？
  }

  /**
   * libcob/common.c cob_accept_environment
   *
   * @param f
   */
  public static void acceptEnvironment(AbstractCobolField f) {
    String p = null;
    if (CobolUtil.cobLocalEnv != null) {
      p = CobolUtil.getEnv(CobolUtil.cobLocalEnv);
    }

    if (p == null) {
      // TODO setExceptionは暫定実装
      CobolException.setException(CobolExceptionId.COB_EC_IMP_ACCEPT);
      p = " ";
    }

    AbstractCobolField src = CobolFieldFactory.makeCobolField(p);
    // TODO hankakuMoveFromは暫定実装
    f.hankakuMoveFrom(src);
  }

  /**
   * libcob/common.c cob_display_command_line
   *
   * @param f
   */
  public static void displayCommandLine(AbstractCobolField f) {
    CobolUtil.commlnptr = new byte[f.getSize()];
    CobolUtil.commlncnt = f.getSize();
    for (int i = 0; i < CobolUtil.commlncnt; ++i) {
      CobolUtil.commlnptr[i] = f.getDataStorage().getByte(i);
    }
  }

  /**
   * libcob/common.c cob_accept_command_line
   *
   * @param f
   */
  public static void acceptCommandLine(AbstractCobolField f) {
    if (CobolUtil.commlncnt != 0) {
      f.memcpy(CobolUtil.commlnptr, CobolUtil.commlncnt);
      return;
    }

    f.memcpy(String.join(" ", CobolUtil.commandLineArgs));
  }

  /**
   * libcob/common.c cob_display_arg_number
   *
   * @param f
   */
  public static void displayArgNumber(AbstractCobolField f) {
    CobolFieldAttribute attr =
        new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY, 9, 0, 0, null);
    byte[] data = new byte[4];
    AbstractCobolField temp =
        CobolFieldFactory.makeCobolField(data.length, new CobolDataStorage(data), attr);
    temp.moveFrom(f);
    int n = ByteBuffer.wrap(data).getInt();
    if (n < 0 || n >= CobolUtil.commandLineArgs.length) {
      CobolException.setException(CobolExceptionId.COB_EC_IMP_DISPLAY);
      return;
    }
    CobolUtil.currentArgIndex = n;
  }

  /**
   * libcob/common.c cob_accept_arg_number
   *
   * @param f
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
   * libcob/common.c cob_accept_arg_value
   *
   * @param f
   */
  public static void acceptArgValue(AbstractCobolField f) {
    if (CobolUtil.currentArgIndex > CobolUtil.commandLineArgs.length) {
      CobolException.setException(CobolExceptionId.COB_EC_IMP_ACCEPT);
      return;
    }
    f.memcpy(CobolUtil.commandLineArgs[CobolUtil.currentArgIndex - 1]);
    CobolUtil.currentArgIndex++;
  }
}
