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
package jp.osscons.opensourcecobol.libcobj.exceptions;

import java.util.List;

/** 実行時エラーを示す例外 */
public class CobolRuntimeException extends RuntimeException {
  public static int code;

  public static final int COBOL_FITAL_ERROR = 9000;

  public static String source_file = null;
  public static int source_line = 0;

  public static List<RuntimeErrorHandler> hdlrs = null;

  /** エラー番号 */
  private int errorCode;
  /** エラーメッセージ */
  private String message;

  /**
   * コンストラクタ
   *
   * @param errorCode this.errorCodeに設定する値
   * @param message   this.messageに設定する値
   */
  public CobolRuntimeException(int errorCode, String message) {
    super();
    this.errorCode = errorCode;
    this.message = message;
  }

  /**
   * コンストラクタ
   *
   * @param e
   */
  public CobolRuntimeException(Throwable e) {
    super(e);
  }

  /** エラーメッセージの文字列表現を返す */
  @Override
  public String getMessage() {
    // TODO エラーメッセージの設計
    return errorCode + " : " + message;
  }

  /** superのprintStackTraceの拡張 */
  @Override
  public void printStackTrace() {
    System.out.println(errorCode + " : " + message);
    super.printStackTrace();
  }

  public static void setException(int id) {
    code = CobolExceptionTabCode.code[id];
    // TODO common.c実装に残りをやる
  }

  /** libcob/common.cのcob_runtime_errorの実装 */
  public static void displayRuntimeError(String message) {
    if (hdlrs != null && !hdlrs.isEmpty()) {
      String runtime_err_str;
      if (source_file != null) {
        runtime_err_str = String.format("%s:%d: %s", source_file, source_line, message);
      } else {
        runtime_err_str = message;
      }
      for (RuntimeErrorHandler h : hdlrs) {
        h.proc(runtime_err_str);
      }
      hdlrs = null;
    }

    if (source_file != null) {
      System.err.print(String.format("%s:%d", source_file, source_line));
    }
    System.err.println("libcobj: " + message);
    System.err.flush();
  }
}
