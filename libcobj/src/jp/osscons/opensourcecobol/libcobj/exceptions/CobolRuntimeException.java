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
import jp.osscons.opensourcecobol.libcobj.call.CobolResolve;
import jp.osscons.opensourcecobol.libcobj.common.CobolUtil;

/** 実行時エラーを示す例外 */
public class CobolRuntimeException extends RuntimeException {
  public static int code;
  public static int cobException = 0;
  public static String origProgramId;
  public static String origSection;
  public static String origParagraph;
  public static int origLine = 0;
  public static String origStatement;

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
    cobException = 1;
    origLine = CobolUtil.getSourceLine();
    origProgramId = CobolUtil.getCurrProgramId();
    origSection = CobolUtil.getCurrSection();
    origParagraph = CobolUtil.getCurrParagraph();
    origStatement = CobolUtil.getSourceStatement();
  }

  public static String getExceptionName(int exceptionCode) {
    return CobolResolve.cobException.get(exceptionCode);
  }

  public static int getExceptionCode() {
    return code;
  }

  public static int getException() {
    return cobException;
  }

  public static String getOrigProgramId() {
    return origProgramId;
  }

  public static String getOrigSection() {
    return origSection;
  }

  public static String getOrigParagragh() {
    return origParagraph;
  }

  public static int getOrigLine() {
    return origLine;
  }

  public static String getOrigStatement() {
    return origStatement;
  }
}
