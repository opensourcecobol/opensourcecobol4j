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

import java.util.Optional;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolRuntimeException;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolStopRunException;

/**
 * COBOLの手続き部における制御の流れ(PERFORM文・GO TO文)を表現する抽象クラス<br>
 * 各段落・節を1つのCobolControlとして表し、{@link #run()}でその単位を実行する。<br>
 * {@link #run()}は実行後に続けて実行すべき次の制御を返すことで、段落の連続実行(フォールスルー)を表現する。
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
     * @return 続けて実行すべき次の制御。続きがない場合は空の{@link Optional}
     * @throws CobolRuntimeException 実行中に実行時例外が発生した場合
     * @throws CobolStopRunException 実行中にSTOP RUNが実行された場合
     */
    public abstract Optional<CobolControl> run()
            throws CobolRuntimeException, CobolStopRunException;

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
     * 何も実行せず、続きの制御も持たない空の制御単位を生成する。
     *
     * @return 何も行わない制御単位
     */
    public static CobolControl pure() {
        return new CobolControl() {
            @Override
            public Optional<CobolControl> run()
                    throws CobolRuntimeException, CobolStopRunException {
                return Optional.empty();
            }
        };
    }

    /**
     * GO TO文に相当し、指定された制御単位へ制御を移す制御単位を生成する。
     *
     * @param cont 制御の移行先となる制御単位
     * @return 移行先を実行する制御単位
     */
    public static CobolControl goTo(CobolControl cont) {
        return new CobolControl() {
            @Override
            public Optional<CobolControl> run()
                    throws CobolRuntimeException, CobolStopRunException {
                return cont.run();
            }
        };
    }

    /**
     * PERFORM ... THRU ...文に相当し、begin番目からend番目までの制御単位を順に実行する制御単位を生成する。<br>
     * 終端が節の場合は、後続の段落も含めて節の終わりまで実行する。
     *
     * @param contList 段落・節を格納した制御単位の配列
     * @param begin 実行を開始する制御単位のインデックス
     * @param end 実行を終了する制御単位のインデックス
     * @return 指定範囲を実行する制御単位
     */
    public static CobolControl performThrough(CobolControl[] contList, int begin, int end) {
        return new CobolControl() {
            @Override
            public Optional<CobolControl> run()
                    throws CobolRuntimeException, CobolStopRunException {
                Optional<CobolControl> nextCont = Optional.of(contList[begin]);
                LabelType endType = contList[end].type;
                int executedProgramId;
                do {
                    CobolControl cont = nextCont.get();
                    executedProgramId = cont.contId;
                    nextCont = cont.run();
                } while (nextCont.isPresent() && executedProgramId != end);

                if (endType == LabelType.section) {
                    while (nextCont.isPresent() && nextCont.get().type == LabelType.label) {
                        CobolControl cont = nextCont.get();
                        nextCont = cont.run();
                    }
                }
                return Optional.of(CobolControl.pure());
            }
        };
    }

    /**
     * PERFORM文に相当し、指定された単一の段落・節を実行する制御単位を生成する。
     *
     * @param contList 段落・節を格納した制御単位の配列
     * @param labelId 実行する制御単位のインデックス
     * @return 指定された制御単位を実行する制御単位
     */
    public static CobolControl perform(CobolControl[] contList, int labelId) {
        return CobolControl.performThrough(contList, labelId, labelId);
    }
}
