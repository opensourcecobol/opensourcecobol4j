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
import jp.osscons.opensourcecobol.libcobj.common.CobolModule;
import jp.osscons.opensourcecobol.libcobj.common.CobolRunUnit;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;

/** STOP RUNの呼び出し時にスローされる例外。返り値を保持する。 */
public final class CobolStopRunException extends Exception {

    /** 返り値 */
    private int returnCode;

    /**
     * コンストラクタ
     *
     * @param returnCode 返り値
     */
    private CobolStopRunException(int returnCode) {
        this.returnCode = returnCode;
    }

    /**
     * コンストラクタ
     *
     * @param storage 返り値を格納したCobolDataStorageのインスタンス
     */
    private CobolStopRunException(CobolDataStorage storage) {
        this(storage.intValue());
    }

    /**
     * このクラスが保持するSTOP RUNの返り値を取得する。
     *
     * @return このクラスが保持するSTOP RUNの返り値
     */
    public int getReturnCode() {
        return returnCode;
    }

    /**
     * CobolStopRunExceptionを例外としてスローする。 COBOLプログラム終了時のデフォルトの終了処理は実行されない。
     *
     * @param returnCode STOP RUNの返り値
     * @throws CobolStopRunException 常にスローされる
     */
    public static void throwException(int returnCode) throws CobolStopRunException {
        throw new CobolStopRunException(returnCode);
    }

    /**
     * CobolStopRunExceptionを例外としてスローする。 COBOLプログラム終了時のデフォルトの終了処理は実行されない。
     *
     * @param storage STOP RUNの返り値
     * @throws CobolStopRunException 常にスローされる
     */
    public static void throwException(CobolDataStorage storage) throws CobolStopRunException {
        throw new CobolStopRunException(storage);
    }

    /**
     * CobolStopRunExceptionをスローする。<br>
     * 例外が実行単位のルートのプログラムに到達したとき({@link #handleAtFrame(int)})に、
     * COBOLプログラムによってオープンされたファイルのクローズなどの終了処理が行われる。
     *
     * @param status STOP RUNの返り値
     * @throws CobolStopRunException このメソッドでは必ずCobolStopRunExceptionがスローされる。
     */
    public static void stopRunAndThrow(int status) throws CobolStopRunException {
        // 実行単位の終了処理は、例外が実行単位のルートのプログラムまで巻き戻されたときに
        // handleAtFrameが行う。ここで行うとモジュールスタックが空になり、CALL先で発生した
        // STOP RUNがルートと誤判定されて呼び出し元へ復帰してしまう。
        CobolStopRunException.throwException(status);
    }

    /**
     * デフォルトの終了処理を実行する。CobolStopRunExceptionをスローしない。<br>
     * 現在のスレッドの実行単位を終了し、オープン中のファイルのクローズとランタイム状態の破棄を行う。
     * 他のスレッドで実行中の実行単位には影響しない。
     */
    public static void stopRun() {
        // TODO screen実装時に追加
        // cob_screen_terminate();
        CobolRunUnit.end();
    }

    /**
     * STOP RUNによって実行単位が終了したときの返り値(スレッドごとに保持する)。 STOP RUNが実行されていない場合はnull。
     */
    private static final ThreadLocal<Integer> lastStopRunCode = new ThreadLocal<>();

    /**
     * 生成されたCOBOLプログラムのrun_moduleがCobolStopRunExceptionまたは{@link CobolStopRunSignal}を 捕捉したときに呼び出す。<br>
     * 呼び出し元のモジュールスタックとコールスタックを1段戻し、このプログラムが実行単位のルート
     * (モジュールスタックが空になった)であれば実行単位の終了処理({@link #stopRun()})を行って返り値を返す。
     * ルートでなければ{@link CobolStopRunSignal}をスローして呼び出し元のCOBOLプログラムへ巻き戻す。
     *
     * @param returnCode STOP RUNの返り値
     * @return 実行単位のルートで捕捉した場合のSTOP RUNの返り値
     * @throws CobolStopRunSignal 実行単位のルートでない場合
     */
    public static int handleAtFrame(int returnCode) {
        CobolResolve.popCallStackList();
        CobolModule.pop();
        if (CobolModule.getStackDepth() > 0) {
            throw new CobolStopRunSignal(returnCode);
        }
        stopRun();
        lastStopRunCode.set(returnCode);
        return returnCode;
    }

    /** 現在のスレッドに記録されたSTOP RUNの返り値を破棄する。実行単位の状態を破棄するときに呼び出す。 */
    public static void resetThreadState() {
        lastStopRunCode.remove();
    }

    /**
     * 現在のスレッドでSTOP RUNによって実行単位が終了したかどうかを返す。<br>
     * 生成されたmainメソッドがプロセスの終了コードを決めるために使用する。 呼び出すと記録はクリアされる。
     *
     * @return STOP RUNが実行されていればその返り値、実行されていなければnull
     */
    public static Integer consumeStopRunCode() {
        Integer code = lastStopRunCode.get();
        lastStopRunCode.remove();
        return code;
    }

    /**
     * 生成されたmainメソッドの末尾で呼び出し、実行単位を終了する。<br>
     * STOP RUNで終了していた場合はその返り値で{@link System#exit}する。 それ以外の場合はSystem.exitを呼ばずに復帰する。
     */
    public static void exitMain() {
        Integer stopRunCode = consumeStopRunCode();
        stopRun();
        if (stopRunCode != null) {
            System.exit(stopRunCode);
        }
    }
}
