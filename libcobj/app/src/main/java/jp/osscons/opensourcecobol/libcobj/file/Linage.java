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
package jp.osscons.opensourcecobol.libcobj.file;

import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;

/**
 * COBOLのLINAGE句に関する情報を保持するクラス.
 * 帳票の1ページの本体行数,フッタ・上下余白の行数,およびLINAGE-COUNTERの状態を保持する.
 */
public class Linage {
    private AbstractCobolField linage;
    private AbstractCobolField linageCtr;
    private AbstractCobolField latfoot;
    private AbstractCobolField lattop;
    private AbstractCobolField latbot;
    private int linLines;
    private int linFoot;
    private int linTop;
    private int linBot;

    /**
     * 1ページの本体行数(LINAGE句のLINES)を表すCOBOLデータ項目を取得する.
     *
     * @return 1ページの本体行数を表すCOBOLデータ項目
     */
    AbstractCobolField getLinage() {
        return linage;
    }

    /**
     * 1ページの本体行数(LINAGE句のLINES)を表すCOBOLデータ項目を設定する.
     *
     * @param linage 1ページの本体行数を表すCOBOLデータ項目
     */
    public void setLinage(AbstractCobolField linage) {
        this.linage = linage;
    }

    /**
     * 現在のページ内行位置を表すLINAGE-COUNTERのCOBOLデータ項目を取得する.
     *
     * @return LINAGE-COUNTERを表すCOBOLデータ項目
     */
    AbstractCobolField getLinageCtr() {
        return linageCtr;
    }

    /**
     * 現在のページ内行位置を表すLINAGE-COUNTERのCOBOLデータ項目を設定する.
     *
     * @param linageCtr LINAGE-COUNTERを表すCOBOLデータ項目
     */
    public void setLinageCtr(AbstractCobolField linageCtr) {
        this.linageCtr = linageCtr;
    }

    /**
     * フッタ開始行(FOOTING AT)を表すCOBOLデータ項目を取得する.
     *
     * @return フッタ開始行を表すCOBOLデータ項目
     */
    AbstractCobolField getLatfoot() {
        return latfoot;
    }

    /**
     * フッタ開始行(FOOTING AT)を表すCOBOLデータ項目を設定する.
     *
     * @param latfoot フッタ開始行を表すCOBOLデータ項目
     */
    public void setLatfoot(AbstractCobolField latfoot) {
        this.latfoot = latfoot;
    }

    /**
     * ページ上部の余白行数(LINES AT TOP)を表すCOBOLデータ項目を取得する.
     *
     * @return ページ上部の余白行数を表すCOBOLデータ項目
     */
    AbstractCobolField getLattop() {
        return lattop;
    }

    /**
     * ページ上部の余白行数(LINES AT TOP)を表すCOBOLデータ項目を設定する.
     *
     * @param lattop ページ上部の余白行数を表すCOBOLデータ項目
     */
    public void setLattop(AbstractCobolField lattop) {
        this.lattop = lattop;
    }

    /**
     * ページ下部の余白行数(LINES AT BOTTOM)を表すCOBOLデータ項目を取得する.
     *
     * @return ページ下部の余白行数を表すCOBOLデータ項目
     */
    AbstractCobolField getLatbot() {
        return latbot;
    }

    /**
     * ページ下部の余白行数(LINES AT BOTTOM)を表すCOBOLデータ項目を設定する.
     *
     * @param latbot ページ下部の余白行数を表すCOBOLデータ項目
     */
    public void setLatbot(AbstractCobolField latbot) {
        this.latbot = latbot;
    }

    /**
     * 1ページの本体行数の現在値(整数)を取得する.
     *
     * @return 1ページの本体行数
     */
    int getLinLines() {
        return linLines;
    }

    /**
     * 1ページの本体行数の現在値(整数)を設定する.
     *
     * @param linLines 1ページの本体行数
     */
    public void setLinLines(int linLines) {
        this.linLines = linLines;
    }

    /**
     * フッタ開始行の現在値(整数)を取得する.
     *
     * @return フッタ開始行
     */
    int getLinFoot() {
        return linFoot;
    }

    /**
     * フッタ開始行の現在値(整数)を設定する.
     *
     * @param linFoot フッタ開始行
     */
    public void setLinFoot(int linFoot) {
        this.linFoot = linFoot;
    }

    /**
     * ページ上部の余白行数の現在値(整数)を取得する.
     *
     * @return ページ上部の余白行数
     */
    int getLinTop() {
        return linTop;
    }

    /**
     * ページ上部の余白行数の現在値(整数)を設定する.
     *
     * @param linTop ページ上部の余白行数
     */
    public void setLinTop(int linTop) {
        this.linTop = linTop;
    }

    /**
     * ページ下部の余白行数の現在値(整数)を取得する.
     *
     * @return ページ下部の余白行数
     */
    int getLinBot() {
        return linBot;
    }

    /**
     * ページ下部の余白行数の現在値(整数)を設定する.
     *
     * @param linBot ページ下部の余白行数
     */
    public void setLinBot(int linBot) {
        this.linBot = linBot;
    }
}
