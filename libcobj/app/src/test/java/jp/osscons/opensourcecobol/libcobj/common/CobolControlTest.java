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

import static org.junit.jupiter.api.Assertions.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import jp.osscons.opensourcecobol.libcobj.common.CobolControl.LabelType;
import org.junit.jupiter.api.Test;

class CobolControlTest {

    /** 実行された順に自身の名前を記録し、続けて実行すべき制御単位を返す制御単位。 */
    private static class Label extends CobolControl {
        private final String name;
        private final List<String> executed;
        private CobolControl next;

        Label(int contId, LabelType type, String name, List<String> executed) {
            super(contId, type);
            this.name = name;
            this.executed = executed;
        }

        @Override
        public CobolControl run() {
            executed.add(name);
            return next;
        }
    }

    /**
     * 各要素が次の要素へフォールスルーする制御単位の配列を生成する。 最後の要素は続きが無いことを表すnullを返す。
     *
     * @param executed 実行された制御単位の名前が追記されるリスト
     * @param types 生成する制御単位の種別(配列の要素数が制御単位の数になる)
     * @return 生成した制御単位の配列。名前は"L0", "L1", ...となる
     */
    private static Label[] contListOf(List<String> executed, LabelType... types) {
        Label[] contList = new Label[types.length];
        for (int i = 0; i < types.length; i++) {
            contList[i] = new Label(i, types[i], "L" + i, executed);
        }
        for (int i = 0; i < types.length - 1; i++) {
            contList[i].next = contList[i + 1];
        }
        return contList;
    }

    @Test
    void performRunsOnlySpecifiedParagraph() throws Exception {
        List<String> executed = new ArrayList<>();
        CobolControl[] contList =
                contListOf(executed, LabelType.label, LabelType.label, LabelType.label);

        CobolControl.perform(contList, 1);

        assertEquals(Arrays.asList("L1"), executed);
    }

    @Test
    void performThroughRunsRangeInclusive() throws Exception {
        List<String> executed = new ArrayList<>();
        CobolControl[] contList =
                contListOf(
                        executed,
                        LabelType.label,
                        LabelType.label,
                        LabelType.label,
                        LabelType.label);

        CobolControl.performThrough(contList, 1, 2);

        assertEquals(Arrays.asList("L1", "L2"), executed);
    }

    @Test
    void performThroughEndingWithParagraphDoesNotFallThrough() throws Exception {
        List<String> executed = new ArrayList<>();
        CobolControl[] contList =
                contListOf(executed, LabelType.label, LabelType.label, LabelType.label);

        CobolControl.performThrough(contList, 0, 1);

        assertEquals(Arrays.asList("L0", "L1"), executed);
    }

    @Test
    void performThroughStopsWhenControlEndsBeforeEnd() throws Exception {
        List<String> executed = new ArrayList<>();
        Label[] contList = contListOf(executed, LabelType.label, LabelType.label, LabelType.label);
        // L1でGO TOやEXIT PROGRAMにより制御が途切れる状況を再現する
        contList[1].next = null;

        CobolControl.performThrough(contList, 0, 2);

        assertEquals(Arrays.asList("L0", "L1"), executed);
    }

    @Test
    void performThroughEndingWithSectionRunsItsFollowingParagraphs() throws Exception {
        List<String> executed = new ArrayList<>();
        CobolControl[] contList =
                contListOf(
                        executed,
                        LabelType.label, // L0: 範囲外の段落
                        LabelType.label, // L1: 範囲の開始
                        LabelType.section, // L2: 範囲の終端となる節
                        LabelType.label, // L3: L2に属する段落
                        LabelType.label, // L4: L2に属する段落
                        LabelType.section); // L5: 次の節(実行されない)

        CobolControl.performThrough(contList, 1, 2);

        assertEquals(Arrays.asList("L1", "L2", "L3", "L4"), executed);
    }

    @Test
    void performOnSectionRunsItsFollowingParagraphs() throws Exception {
        List<String> executed = new ArrayList<>();
        CobolControl[] contList =
                contListOf(
                        executed,
                        LabelType.section, // L0: 対象の節
                        LabelType.label, // L1: L0に属する段落
                        LabelType.label, // L2: L0に属する段落
                        LabelType.section, // L3: 次の節(実行されない)
                        LabelType.label);

        CobolControl.perform(contList, 0);

        assertEquals(Arrays.asList("L0", "L1", "L2"), executed);
    }

    @Test
    void performOnSectionStopsWhenControlEndsInTheSection() throws Exception {
        List<String> executed = new ArrayList<>();
        Label[] contList =
                contListOf(executed, LabelType.section, LabelType.label, LabelType.label);
        // 節に属する段落の途中で制御が途切れる状況を再現する
        contList[1].next = null;

        CobolControl.perform(contList, 0);

        assertEquals(Arrays.asList("L0", "L1"), executed);
    }
}
