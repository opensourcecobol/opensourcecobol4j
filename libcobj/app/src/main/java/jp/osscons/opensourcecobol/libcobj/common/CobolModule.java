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

import java.util.ArrayList;
import java.util.List;
import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;

/**
 * libcob/common.hのcob_moduleに対応するクラス<br>
 * 実行中のプログラム1つ分の実行時情報(照合順序、小数点文字、通貨記号、各種フラグ、
 * プログラムID、手続き部の引数など)を保持する。<br>
 * CALLによる呼び出しの入れ子をモジュールスタックで管理し、現在実行中のモジュールを追跡する。
 */
public class CobolModule {

    /**
     * モジュールの呼び出し階層を表すスタック。<br>
     * 実行単位(run unit)はスレッドごとに独立しているため、スタックはスレッドごとに保持する。
     */
    private static final ThreadLocal<List<CobolModule>> moduleStack =
            ThreadLocal.withInitial(ArrayList::new);

    /**
     * 直近にスタックから取り除かれたモジュール。<br>
     * プログラムの実行が終わった後にJavaアプリケーションから結果を参照する場合(CobolResultSetなど)に、
     * 小数点文字などのモジュール情報を引き続き参照できるようにするために保持する。
     */
    private static final ThreadLocal<CobolModule> lastModule = new ThreadLocal<>();

    /**
     * 現在実行中のモジュールを取得する。<br>
     * このスレッドで実行中のモジュールがない場合は、直近に実行を終えたモジュールを返す。
     *
     * @return 現在実行中のモジュール。このスレッドでモジュールが一度も実行されていない場合はnull
     */
    public static CobolModule getCurrentModule() {
        List<CobolModule> stack = moduleStack.get();
        return stack.isEmpty() ? lastModule.get() : stack.get(stack.size() - 1);
    }

    /**
     * モジュールスタックに追加する
     *
     * @param module キューに追加するモジュール
     */
    public static void push(CobolModule module) {
        moduleStack.get().add(module);
    }

    /** モジュールスタックからモジュールを取り除く */
    public static void pop() {
        List<CobolModule> stack = moduleStack.get();
        if (!stack.isEmpty()) {
            lastModule.set(stack.remove(stack.size() - 1));
        }
    }

    /**
     * 現在のスレッドのモジュールスタックの深さを返す。
     *
     * @return スタックに積まれているモジュールの数
     */
    public static int getStackDepth() {
        return moduleStack.get().size();
    }

    /** 現在のスレッドに紐づくモジュールスタックを破棄する。実行単位の終了時に呼び出す。 */
    public static void resetThreadState() {
        moduleStack.remove();
        lastModule.remove();
    }

    /**
     * 呼び出し元プログラムの名前(プログラムID)を指定された記憶領域へ書き込む。<br>
     * C$NARGなどの呼び出し元情報を取得する機能に相当する。
     *
     * @param data 呼び出し元プログラム名の書き込み先となる記憶領域
     * @return 呼び出し元の情報を書き込んだ場合、または第1引数が存在しない場合は1、
     *     呼び出し元が存在しない場合(空白を書き込んだ場合)は0、
     *     呼び出し元のプログラムIDがnullの場合は-1
     */
    public static int calledBy(CobolDataStorage data) {
        AbstractCobolField param = CobolModule.getCurrentModule().cob_procedure_parameters.get(0);
        if (param != null) {
            List<CobolModule> stack = moduleStack.get();
            if (stack.size() >= 2) {
                String calledProgramName = stack.get(stack.size() - 2).program_id;
                if (calledProgramName == null) {
                    return -1;
                }
                int length = Math.min(param.getSize(), calledProgramName.length());
                param.getDataStorage().memcpy(calledProgramName, length);
            } else {
                param.getDataStorage().memset(' ', param.getSize());
                return 0;
            }
        }
        return 1;
    }

    /**
     * モジュールキューが空かどうか
     *
     * @return モジュールスタックが空の場合はtrue、そうでない場合はfalse
     */
    public static boolean isQueueEmpty() {
        return moduleStack.get().isEmpty();
    }

    /** スタック上の次の(呼び出し元の)モジュール */
    // private CobolModule next;
    /** PROGRAM COLLATING SEQUENCEで指定された照合順序 */
    public CobolDataStorage collating_sequence;

    /** 画面入出力時のCRT STATUS */
    // private AbstractCobolField cut_status;
    /** 画面入出力時のカーソル位置 */
    // private AbstractCobolField cursor_pos;
    /** 符号の表示方法(DISPLAY SIGN) */
    public int display_sign;

    /** 小数点として用いる文字 */
    public char decimal_point;

    /** 通貨記号として用いる文字 */
    public char currency_symbol;

    /** 桁区切りとして用いる文字 */
    // private char numeric_separator;
    /** ファイル名のマッピングを行うかどうかのフラグ */
    public int flag_filename_mapping;

    /** 2進数項目の桁あふれを切り捨てるかどうかのフラグ */
    public int flag_binary_truncate;

    /** DISPLAY文の整形出力を行うかどうかのフラグ */
    public int flag_pretty_display;

    /** 予約領域 */
    // private int spare8;
    /** プログラムID(プログラム名) */
    private String program_id;

    /** プログラムのパッケージ名 */
    // private String packageName;

    /** 手続き部に渡された引数(USING句の引数)のリスト */
    public List<AbstractCobolField> cob_procedure_parameters;

    /**
     * コンストラクタ
     *
     * @param next スタック上の次の(呼び出し元の)モジュール
     * @param collatingSequence PROGRAM COLLATING SEQUENCEで指定された照合順序
     * @param cutStatus 画面入出力時のCRT STATUS
     * @param cursorPos 画面入出力時のカーソル位置
     * @param displaySign 符号の表示方法(DISPLAY SIGN)
     * @param decimalPoint 小数点として用いる文字
     * @param currencySymbol 通貨記号として用いる文字
     * @param numericSeparator 桁区切りとして用いる文字
     * @param flagFilenameMapping ファイル名のマッピングを行うかどうかのフラグ
     * @param flagBinaryTruncate 2進数項目の桁あふれを切り捨てるかどうかのフラグ
     * @param flagPrettyDisplay DISPLAY文の整形出力を行うかどうかのフラグ
     * @param spare8 予約領域
     * @param programId プログラムID(プログラム名)
     */
    public CobolModule(
            CobolModule next,
            CobolDataStorage collatingSequence,
            AbstractCobolField cutStatus,
            AbstractCobolField cursorPos,
            int displaySign,
            char decimalPoint,
            char currencySymbol,
            char numericSeparator,
            int flagFilenameMapping,
            int flagBinaryTruncate,
            int flagPrettyDisplay,
            int spare8,
            String programId) {
        // this.next = next;
        this.collating_sequence = collatingSequence;
        // this.cut_status = cutStatus;
        // this.cursor_pos = cursorPos;
        this.display_sign = displaySign;
        this.decimal_point = decimalPoint;
        this.currency_symbol = currencySymbol;
        // this.numeric_separator = numericSeparator;
        this.flag_filename_mapping = flagFilenameMapping;
        this.flag_binary_truncate = flagBinaryTruncate;
        this.flag_pretty_display = flagPrettyDisplay;
        // this.spare8 = spare8;
        this.program_id = programId;

        this.cob_procedure_parameters = new ArrayList<AbstractCobolField>();
    }

    /**
     * このモジュールのプログラムID(プログラム名)を設定する。
     *
     * @param programName 設定するプログラム名
     */
    public void setProgramId(String programName) {
        if (this.program_id != null) {
            this.program_id = null;
        }
        this.program_id = programName;
    }

    /**
     * このモジュールの手続き部の引数(USING句の引数)を設定する。<br>
     * 既存の引数をすべて消去した上で、指定された引数で置き換える。
     *
     * @param field 手続き部に渡す引数の並び
     */
    public void setParameters(AbstractCobolField... field) {
        cob_procedure_parameters.clear();
        for (AbstractCobolField f : field) {
            cob_procedure_parameters.add(f);
        }
    }

    /**
     * 現在実行中のモジュールにおける小数点文字を取得する。
     *
     * @return 現在のモジュールの小数点文字
     */
    public static int getDecimalPoint() {
        CobolModule module = getCurrentModule();
        return module == null ? '.' : module.decimal_point;
    }
}
