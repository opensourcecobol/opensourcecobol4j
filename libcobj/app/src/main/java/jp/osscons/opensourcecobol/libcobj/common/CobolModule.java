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

/** libcob/common.hのcob_moduleに対応するクラス */
public class CobolModule {

    private static List<CobolModule> moduleStack = new ArrayList<CobolModule>();
    private static CobolModule currentModule;

    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     */
    public static CobolModule getCurrentModule() {
        return currentModule;
    }

    /**
     * モジュールスタックに追加する
     *
     * @param module キューに追加するモジュール
     */
    public static void push(CobolModule module) {
        currentModule = module;
        moduleStack.add(module);
    }

    /** モジュールスタックからモジュールを取り除く */
    public static void pop() {
        currentModule = moduleStack.remove(moduleStack.size() - 1);
    }

    /**
     * TODO: 準備中
     *
     * @param data TODO: 準備中
     * @return TODO: 準備中
     */
    public static int calledBy(CobolDataStorage data) {
        AbstractCobolField param = CobolModule.getCurrentModule().cob_procedure_parameters.get(0);
        if (param != null) {
            if (moduleStack.size() >= 2) {
                String calledProgramName = moduleStack.get(moduleStack.size() - 2).program_id;
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
     * @return TODO: 準備中
     */
    public static boolean isQueueEmpty() {
        return moduleStack.isEmpty();
    }

    /** TODO: 準備中 */
    // private CobolModule next;
    /** TODO: 準備中 */
    public CobolDataStorage collating_sequence;

    /** TODO: 準備中 */
    // private AbstractCobolField cut_status;
    /** TODO: 準備中 */
    // private AbstractCobolField cursor_pos;
    /** TODO: 準備中 */
    public int display_sign;

    /** TODO: 準備中 */
    public char decimal_point;

    /** TODO: 準備中 */
    public char currency_symbol;

    /** TODO: 準備中 */
    // private char numeric_separator;
    /** TODO: 準備中 */
    public int flag_filename_mapping;

    /** TODO: 準備中 */
    public int flag_binary_truncate;

    /** TODO: 準備中 */
    public int flag_pretty_display;

    /** TODO: 準備中 */
    // private int spare8;
    /** TODO: 準備中 */
    private String program_id;

    /** TODO: 準備中 */
    // private String packageName;

    /** TODO: 準備中 */
    public List<AbstractCobolField> cob_procedure_parameters;

    /**
     * コンストラクタ
     *
     * @param next TODO: 準備中
     * @param collatingSequence TODO: 準備中
     * @param cutStatus TODO: 準備中
     * @param cursorPos TODO: 準備中
     * @param displaySign TODO: 準備中
     * @param decimalPoint TODO: 準備中
     * @param currencySymbol TODO: 準備中
     * @param numericSeparator TODO: 準備中
     * @param flagFilenameMapping TODO: 準備中
     * @param flagBinaryTruncate TODO: 準備中
     * @param flagPrettyDisplay TODO: 準備中
     * @param spare8 TODO: 準備中
     * @param programId TODO: 準備中
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
     * TODO 実装
     *
     * @param programName TODO: 準備中
     */
    public void setProgramId(String programName) {
        if (this.program_id != null) {
            this.program_id = null;
        }
        this.program_id = programName;
    }

    /**
     * TODO: 準備中
     *
     * @param field TODO: 準備中
     */
    public void setParameters(AbstractCobolField... field) {
        cob_procedure_parameters.clear();
        for (AbstractCobolField f : field) {
            cob_procedure_parameters.add(f);
        }
    }

    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     */
    public static int getDecimalPoint() {
        return currentModule.decimal_point;
    }
}
