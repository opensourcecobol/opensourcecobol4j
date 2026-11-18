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

import jp.osscons.opensourcecobol.libcobj.exceptions.CobolRuntimeException;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolStopRunException;

/**
 * COBOLの手続き部における1つの段落・節を表す抽象クラス<br>
 * 各段落・節を1つのCobolControlとして表し、{@link #run()}でその単位を実行する。<br>
 * {@link #run()}は実行後に続けて実行すべき次の制御を返すことで、段落の連続実行(フォールスルー)を表現する。<br>
 * 続けて実行すべき制御が無い場合は{@code null}を返す。<br>
 * PERFORM文は{@link #perform(CobolControl[], int)}および{@link #performThrough(CobolControl[], int,
 * int)}に変換され、GO TO文は移行先のCobolControlを{@link #run()}の戻り値として返すコードに変換される。
 */
public abstract class CobolControl {
    /** 制御単位の種別(段落か節か)を表す列挙型 */
    public enum LabelType {
        /** 段落(paragraph)を表す */
        label,
        /** 節(section)を表す */
        section,
    }

    /**
     * この制御単位を実行する。
     *
     * @return 続けて実行すべき次の制御。続きがない場合は{@code null}
     * @throws CobolRuntimeException 実行中に実行時例外が発生した場合
     * @throws CobolStopRunException 実行中にSTOP RUNが実行された場合
     */
    public abstract CobolControl run() throws CobolRuntimeException, CobolStopRunException;

    /** この制御単位を識別するID(段落・節の通し番号)。未設定の場合は-1 */
    public int contId = -1;

    /** この制御単位の種別(段落か節か) */
    public LabelType type = LabelType.label;

    /** 既定値(ID:-1、種別:段落)で制御単位を生成する。 */
    public CobolControl() {
        this.contId = -1;
        this.type = LabelType.label;
    }

    /**
     * 指定されたIDと種別で制御単位を生成する。
     *
     * @param contId 制御単位を識別するID
     * @param type 制御単位の種別(段落か節か)
     */
    public CobolControl(int contId, LabelType type) {
        this.contId = contId;
        this.type = type;
    }

    /**
     * PERFORM ... THRU ...文に相当し、begin番目からend番目までの制御単位を順に実行する。<br>
     * 終端が節の場合は、後続の段落も含めて節の終わりまで実行する。
     *
     * @param contList 段落・節を格納した制御単位の配列
     * @param begin 実行を開始する制御単位のインデックス
     * @param end 実行を終了する制御単位のインデックス
     * @throws CobolRuntimeException 実行中に実行時例外が発生した場合
     * @throws CobolStopRunException 実行中にSTOP RUNが実行された場合
     */
    public static void performThrough(CobolControl[] contList, int begin, int end)
            throws CobolRuntimeException, CobolStopRunException {
        CobolControl nextCont = contList[begin];
        final LabelType endType = contList[end].type;
        int executedProgramId;
        do {
            executedProgramId = nextCont.contId;
            nextCont = nextCont.run();
        } while (nextCont != null && executedProgramId != end);

        if (endType == LabelType.section) {
            while (nextCont != null && nextCont.type == LabelType.label) {
                nextCont = nextCont.run();
            }
        }
    }

    /**
     * PERFORM文に相当し、指定された単一の段落・節を実行する。
     *
     * @param contList 段落・節を格納した制御単位の配列
     * @param labelId 実行する制御単位のインデックス
     * @throws CobolRuntimeException 実行中に実行時例外が発生した場合
     * @throws CobolStopRunException 実行中にSTOP RUNが実行された場合
     */
    public static void perform(CobolControl[] contList, int labelId)
            throws CobolRuntimeException, CobolStopRunException {
        performThrough(contList, labelId, labelId);
    }
}
