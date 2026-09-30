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
    /**
     * 現在のエラーコード。16進数のエラーコードが格納される。
     * 通常はsetExceptionメソッドによりCobolExceptionTabCode.codeテーブルから設定されるが、
     * CobolFileやCobolLineSequentialFile等からsetExceptionCodeで直接設定される場合もある。
     * 主にファイルI/O操作や数値演算のエラー判定に使用される。 実行単位はスレッドごとに独立しているため、スレッドごとに保持する。
     */
    private static final ThreadLocal<int[]> code = ThreadLocal.withInitial(() -> new int[1]);

    private static final ThreadLocal<Context> context = ThreadLocal.withInitial(Context::new);

    /** スレッドごとに保持する例外の発生箇所情報 */
    private static final class Context {
        int cobException = 0;
        String origProgramId;
        String origSection;
        String origParagraph;
        int origLine = 0;
        String origStatement;
    }

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
     * 実行時例外を設定する。 エラーIDをベースに、CobolExceptionTabCode.codeテーブルを参照して、対応するエラーコードが設定される。
     * また、エラー発生時のプログラムID、セクション名、パラグラフ名、行番号、ステートメントを取得し、このクラスの静的変数に保持する。
     *
     * @param id エラーID
     */
    public static void setException(int id) {
        code.get()[0] = CobolExceptionTabCode.code[id];
        Context c = context.get();
        c.cobException = 1;
        c.origLine = CobolUtil.getSourceLine();
        c.origProgramId = CobolUtil.getCurrProgramId();
        c.origSection = CobolUtil.getCurrSection();
        c.origParagraph = CobolUtil.getCurrParagraph();
        c.origStatement = CobolUtil.getSourceStatement();
    }

    /**
     * 現在のスレッドのエラーコードを直接設定する。
     *
     * @param exceptionCode 設定するエラーコード
     */
    public static void setExceptionCode(int exceptionCode) {
        code.get()[0] = exceptionCode;
    }

    /** 現在のスレッドのエラーコードを0にリセットする。 */
    public static void clearExceptionCode() {
        code.get()[0] = 0;
    }

    /** 現在のスレッドに紐づく例外情報を破棄する。実行単位の終了時に呼び出す。 */
    public static void resetThreadState() {
        code.remove();
        context.remove();
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
        return code.get()[0];
    }

    /**
     * 例外が設定されたことがあるかどうかを返す。 setExceptionが一度でも呼ばれると1を返し、以降0に戻ることはない。
     * setException(0)を呼ぶとcodeは0にリセットされるが、cobExceptionは1のまま変わらない。
     *
     * @return setExceptionが呼ばれたことがある場合は1、一度も呼ばれていない場合は0
     */
    public static int getException() {
        return context.get().cobException;
    }

    /**
     * エラー発生時のプログラムIDを取得する
     *
     * @return エラー発生時のプログラムID
     */
    public static String getOrigProgramId() {
        return context.get().origProgramId;
    }

    /**
     * エラー発生時のセクション名を取得する
     *
     * @return エラー発生時のセクション名
     */
    public static String getOrigSection() {
        return context.get().origSection;
    }

    /**
     * エラー発生時のパラグラフ名を取得する
     *
     * @return エラー発生時のパラグラフ名
     */
    public static String getOrigParagraph() {
        return context.get().origParagraph;
    }

    /**
     * エラー発生時の行番号を取得する
     *
     * @return エラー発生時の行番号
     */
    public static int getOrigLine() {
        return context.get().origLine;
    }

    /**
     * エラー発生時のステートメントを取得する
     *
     * @return エラー発生時のステートメント
     */
    public static String getOrigStatement() {
        return context.get().origStatement;
    }
}
