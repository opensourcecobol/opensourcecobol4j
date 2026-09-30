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
package jp.osscons.opensourcecobol.libcobj.common;

import jp.osscons.opensourcecobol.libcobj.call.CobolResolve;
import jp.osscons.opensourcecobol.libcobj.call.CobolSystemRoutine;
import jp.osscons.opensourcecobol.libcobj.data.CobolDecimal;
import jp.osscons.opensourcecobol.libcobj.data.CobolPointerRegistry;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolExceptionInfo;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolRuntimeException;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolStopRunException;
import jp.osscons.opensourcecobol.libcobj.file.CobolFile;
import jp.osscons.opensourcecobol.libcobj.file.CobolFileSort;
import jp.osscons.opensourcecobol.libcobj.file.CobolIndexedFile;
import jp.osscons.opensourcecobol.libcobj.sql.CobolEsql;
import jp.osscons.opensourcecobol.libcobj.termio.CobolTerminal;

/**
 * COBOLの実行単位(run unit)のライフサイクルを扱うクラス。<br>
 * libcobjのランタイム状態(モジュールスタック、例外コード、CALLのキャッシュ、オープン中のファイル、
 * EXTERNAL項目、ESQLの接続とカーソルなど)はスレッドごとに独立して保持されている。
 * 実行単位が終了したとき(STOP RUN、または生成されたmainメソッドの終了時)にこれらを破棄することで、
 * スレッドプールで再利用されるスレッドに前の実行単位の状態が残らないようにする。
 *
 * <p>TomcatやSpring Bootのようなマルチスレッド環境からCOBOLプログラムを呼び出す場合、
 * 各スレッドはそれぞれ独立した実行単位として扱われる。STOP RUNを実行しないプログラムを
 * 呼び出した後は、必要に応じて{@link #end()}を呼び出して実行単位の状態を解放すること。
 */
public final class CobolRunUnit {

    private CobolRunUnit() {}

    /**
     * 現在のスレッドの実行単位を終了する。<br>
     * オープンしたままのファイルを暗黙的にクローズし、現在のスレッドに紐づくランタイム状態をすべて破棄する。
     */
    public static void end() {
        CobolFile.exitFileIO();
        resetThreadState();
    }

    /**
     * 現在のスレッドに紐づくランタイム状態をすべて破棄する(ファイルのクローズは行わない)。<br>
     * 注意: libcobjにThreadLocalの状態を持つクラスを追加したときは、そのクラスに
     * resetThreadState()を定義し、必ずここからも呼び出すこと。呼び出しが漏れると、
     * スレッドプールで再利用されるスレッドに前の実行単位の状態が残る。
     */
    public static void resetThreadState() {
        CobolFile.resetThreadState();
        CobolFileSort.resetThreadState();
        CobolIndexedFile.resetRunUnitId();
        CobolPointerRegistry.resetThreadState();
        CobolModule.resetThreadState();
        CobolResolve.resetThreadState();
        CobolCallParams.resetThreadState();
        CobolRuntimeException.resetThreadState();
        CobolExceptionInfo.resetThreadState();
        CobolUtil.resetThreadState();
        CobolIntrinsic.resetThreadState();
        CobolInspect.resetThreadState();
        CobolString.resetThreadState();
        CobolExternal.resetThreadState();
        CobolDecimal.resetThreadState();
        CobolTerminal.resetThreadState();
        CobolSystemRoutine.resetThreadState();
        CobolEsql.resetThreadState();
        CobolStopRunException.resetThreadState();
    }
}
