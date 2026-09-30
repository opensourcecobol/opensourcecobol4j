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
 * License along with this library; see the file COPYING.LIB; if
 * not, write to the Free Software Foundation, 51 Franklin Street, Fifth Floor
 * Boston, MA 02110-1301 USA
 */
package jp.osscons.opensourcecobol.libcobj.exceptions;

/**
 * CALLされたプログラム内でSTOP RUNが実行されたとき、呼び出し元のCOBOLプログラムへ巻き戻すための非検査例外。<br>
 * {@link jp.osscons.opensourcecobol.libcobj.call.CobolRunnable#run}は検査例外を宣言していないため、
 * 実行単位のルートに到達するまでの巻き戻しにはこの非検査例外を使用する。 ルートのプログラムが
 * {@link CobolStopRunException#handleAtFrame(int)}で受け取り、実行単位の終了処理を行って正常復帰する。
 */
public final class CobolStopRunSignal extends RuntimeException {

    private static final long serialVersionUID = 1L;

    /** STOP RUNの返り値 */
    private final int returnCode;

    /**
     * コンストラクタ
     *
     * @param returnCode STOP RUNの返り値
     */
    CobolStopRunSignal(int returnCode) {
        super("STOP RUN (" + returnCode + ")", null, false, false);
        this.returnCode = returnCode;
    }

    /**
     * STOP RUNの返り値を取得する。
     *
     * @return STOP RUNの返り値
     */
    public int getReturnCode() {
        return returnCode;
    }
}
