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

import jp.osscons.opensourcecobol.libcobj.call.CobolResolve;
import jp.osscons.opensourcecobol.libcobj.common.CobolUtil;

/** 実行時エラーを示す例外。エラー番号とエラーメッセージを保持する */
public class CobolRuntimeException extends RuntimeException {
    /** TODO: 準備中 */
    public static int code;

    private static int cobException = 0;
    private static String origProgramId;
    private static String origSection;
    private static String origParagraph;
    private static int origLine = 0;
    private static String origStatement;

    /** 重大なエラーを示すエラーコード */
    public static final int COBOL_FATAL_ERROR = 9000;

    /** エラー番号 */
    private int errorCode;

    /** エラーメッセージ */
    private String message;

    /**
     * コンストラクタ
     *
     * @param errorCode エラーコード
     * @param message エラーメッセージ
     */
    public CobolRuntimeException(int errorCode, String message) {
        super();
        this.errorCode = errorCode;
        this.message = message;
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

    /**
     * 実行時例外を設定する。 エラーIDをベースに、CobolExceptioTabCode.codeテーブルを参照して、対応するエラーコードが設定される。
     * また、エラー発生時のプログラムID、セクション名、パラグラフ名、行番号、ステートメントを取得し、このクラスの静的変数に保持する。
     *
     * @param id エラーID
     */
    public static void setException(int id) {
        code = CobolExceptionTabCode.code[id];
        cobException = 1;
        origLine = CobolUtil.getSourceLine();
        origProgramId = CobolUtil.getCurrProgramId();
        origSection = CobolUtil.getCurrSection();
        origParagraph = CobolUtil.getCurrParagraph();
        origStatement = CobolUtil.getSourceStatement();
    }

    /**
     * エラーコード名を取得する
     *
     * @param exceptionCode エラーコード
     * @return エラーコードに対応するエラーコード名
     */
    public static String getExceptionName(int exceptionCode) {
        return CobolResolve.cobException.get(exceptionCode);
    }

    /**
     * エラーコードを取得する
     *
     * @return エラーコード
     */
    public static int getExceptionCode() {
        return code;
    }

    /**
     * 常に0を返す。TODO: 必要に応じてこのメソッドは削除ないし修正する。
     *
     * @return 0
     */
    public static int getException() {
        return cobException;
    }

    /**
     * エラー発生時のプログラムIDを取得する
     *
     * @return エラー発生時のプログラムID
     */
    public static String getOrigProgramId() {
        return origProgramId;
    }

    /**
     * エラー発生時のセクション名を取得する
     *
     * @return エラー発生時のセクション名
     */
    public static String getOrigSection() {
        return origSection;
    }

    /**
     * エラー発生時のパラグラフ名を取得する
     *
     * @return エラー発生時のパラグラフ名
     */
    public static String getOrigParagragh() {
        return origParagraph;
    }

    /**
     * エラー発生時の行番号を取得する
     *
     * @return エラー発生時の行番号
     */
    public static int getOrigLine() {
        return origLine;
    }

    /**
     * エラー発生時のステートメントを取得する
     *
     * @return エラー発生時のステートメント
     */
    public static String getOrigStatement() {
        return origStatement;
    }
}
