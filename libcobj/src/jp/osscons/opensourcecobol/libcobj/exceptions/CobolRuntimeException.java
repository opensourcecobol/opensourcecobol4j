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
import jp.osscons.opensourcecobol.libcobj.common.CobolUtil;

/** 実行時エラーを示す例外 */
public class CobolRuntimeException extends RuntimeException {
  public static int code;
  public static int cob_got_exception = 0;
  public static String cob_orig_program_id;
  public static String cob_orig_section;
  public static String cob_orig_paragraph;
  public static int cob_orig_line = 0;

  public static final int COBOL_FITAL_ERROR = 9000;

  public static List<RuntimeErrorHandler> hdlrs = null;

  /** エラー番号 */
  private int errorCode;
  /** エラーメッセージ */
  private String message;

  /**
   * コンストラクタ
   *
   * @param errorCode this.errorCodeに設定する値
   * @param message this.messageに設定する値
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
    cob_got_exception = 1;
    cob_orig_program_id = CobolUtil.getCurrProgramId();
    cob_orig_section = CobolUtil.getCurrSection();
    cob_orig_paragraph = CobolUtil.getCurrParagraph();
    cob_orig_line = CobolUtil.getSourceLine();
    // TODO common.c実装に残りをやる
  }

  public static int getExceptionCode() {
    return code;
  }

  public static int getException() {
    return cob_got_exception;
  }

  public static String getOrigProgramId() {
    return cob_orig_program_id;
  }

  public static String getOrigSection() {
    return cob_orig_section;
  }

  public static String getOrigParagragh() {
    return cob_orig_paragraph;
  }

  public static int getOrigLine() {
    return cob_orig_line;
  }
}
