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
package jp.osscons.opensourcecobol.libcobj.call;

import java.io.IOException;
import java.lang.ProcessBuilder.Redirect;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import jp.osscons.opensourcecobol.libcobj.common.CobolModule;
import jp.osscons.opensourcecobol.libcobj.common.CobolUtil;
import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolStopRunException;

/** 組み込み関数を提供するクラス */
public class CobolSystemRoutine {
    private static final boolean runsOnWindows = "\\".equals(System.getProperty("file.separator"));

    /**
     * 組み込み関数CBL_EXIT_PROCの実装。未実装。
     *
     * @param x 未実装
     * @param pptr 未実装
     * @return 未実装
     */
    public static int CBL_EXIT_PROC(CobolDataStorage x, CobolDataStorage pptr) {
        // TODO 実装
        return 0;
    }

    /**
     * 組み込み関数CBL_ERROR_PROCの実装。未実装。
     *
     * @param x 未実装
     * @param pptr 未実装
     * @return 未実装
     */
    public static int CBL_ERROR_PROC(CobolDataStorage x, CobolDataStorage pptr) {
        // TODO 実装
        return 0;
    }

    /**
     * 組み込み関数SYSTEMの実装。cmdに指定されたコマンドを実行する。
     *
     * @param cmd コマンド文字列。Linux/Unix環境であればシェルコマンド、Windows環境であればコマンドプロンプトのコマンド。
     * @return コマンドの終了コード。
     * @throws CobolStopRunException パラメータのサイズが0以下の場合、またはパラメータが現在のモジュールで指定されていない場合にスローされる
     */
    public static int SYSTEM(CobolDataStorage cmd) throws CobolStopRunException {
        int size = SYSTEM_getParameterSize();
        String cmdStr = new String(cmd.getByteArray(0, size), AbstractCobolField.charSetSJIS);
        return SYSTEM_main(cmdStr);
    }

    /**
     * 組み込み関数SYSTEMの実装。cmdに指定されたコマンドを実行する。
     *
     * @param cmd コマンド文字列。Linux/Unix環境であればシェルコマンド、Windows環境であればコマンドプロンプトのコマンド。
     * @return コマンドの終了コード。
     */
    public static int SYSTEM(String cmd) {
        return SYSTEM_main(cmd);
    }

    private static int SYSTEM_getParameterSize() throws CobolStopRunException {
        CobolUtil.COB_CHK_PARMS("SYSTEM", 1);
        List<AbstractCobolField> paramaters =
                CobolModule.getCurrentModule().cob_procedure_parameters;
        if (!paramaters.isEmpty() && paramaters.get(0) != null) {
            AbstractCobolField paramater = paramaters.get(0);
            int size = paramater.getSize();
            if (size <= 0) {
                CobolUtil.runtimeError("The size of the paramater to SYSTEM call is less than 1");
                CobolStopRunException.stopRunAndThrow(1);
            }
            return size;
        } else {
            CobolUtil.runtimeError("The size of the paramater is not specified");
            CobolStopRunException.stopRunAndThrow(1);
            // not reached
            return -1;
        }
    }

    private static int SYSTEM_main(String cmd) {
        ProcessBuilder pb;
        if (runsOnWindows) {
            pb = new ProcessBuilder("cmd", "/c", cmd);
        } else {
            pb = new ProcessBuilder("sh", "-c", cmd);
        }
        pb.redirectInput(Redirect.INHERIT)
                .redirectOutput(Redirect.INHERIT)
                .redirectError(Redirect.INHERIT);
        try {
            Process process = pb.start();
            return process.waitFor();
        } catch (InterruptedException | IOException e) {
            e.printStackTrace();
            return 1;
        }
    }

    /**
     * 組み込み関数SYSTEMの実装。cmdに指定されたコマンドを実行する。
     *
     * @param cmd コマンド文字列。Linux/Unix環境であればシェルコマンド、Windows環境であればコマンドプロンプトのコマンド。
     * @return コマンドの終了コード。
     */
    public static int SYSTEM(AbstractCobolField cmd) {
        return SYSTEM(cmd.getString());
    }

    /**
     * 組み込み関数すうC$SLEEPの実装
     * opensource COBOLのlibcob/common.cのcob_acuw_sleep関数に相当する
     *
     * @param data C$SLEEPの引数として指定されたCOBOL変数のバイト列。
     */
    @SuppressWarnings("PMD.AvoidDollarSigns")
    public static int C$SLEEP(CobolDataStorage data) {
        CobolUtil.COB_CHK_PARMS("C$SLEEP", 3);
        List<AbstractCobolField> params = CobolModule.getCurrentModule().cob_procedure_parameters;
        if (!params.isEmpty() && params.get(0) != null) {
            int n = params.get(0).getInt();
            if (n > 0 && n < 3600 * 24 * 7) {
                try {
                    Thread.sleep(n * 1000L);
                } catch (InterruptedException e) {
                    return 0;
                }
            }
        }
        return 0;
    }

    private interface Calculater {
        byte calc(byte b1, byte b2);
    }

    private static int CBL_COMMON_OPERATION(
            String funcName,
            CobolDataStorage data1,
            CobolDataStorage data2,
            int length,
            Calculater c) {
        CobolUtil.COB_CHK_PARMS(funcName, 3);
        if (length <= 0) {
            return 0;
        }
        for (int n = 0; n < length; ++n) {
            byte b1 = data1.getByte(n);
            byte b2 = data2.getByte(n);
            data2.setByte(n, c.calc(b1, b2));
        }
        return 0;
    }

    /**
     * 組み込み関数CBL_ANDの実装。1バイトごとに論理積を取る。 先頭lengthバイトのデータについて論理積を計算し、計算結果を2番目の引数の先頭に格納する。
     *
     * @param data1 計算対象の1番目のCOBOL変数のバイト列。
     * @param data2 計算対象の2番目のCOBOL変数のバイト列。先頭lengthバイトに計算結果が格納される。
     * @param length 計算するバイト数。
     * @return 0
     */
    public static int CBL_AND(CobolDataStorage data1, CobolDataStorage data2, int length) {
        return CBL_COMMON_OPERATION(
                "CBL_AND",
                data1,
                data2,
                length,
                new Calculater() {
                    @Override
                    public byte calc(byte b1, byte b2) {
                        return (byte) (b1 & b2);
                    }
                });
    }

    /**
     * 組み込み関数CBL_ANDの実装。1バイトごとに論理積を取る。 先頭lengthバイトのデータについて論理積を計算し、計算結果を2番目の引数の先頭に格納する。
     *
     * @param data1 計算対象の1番目のCOBOL変数。
     * @param data2 計算対象の2番目の引数のバイト列。先頭lengthバイトに計算結果が格納される。
     * @param length 計算するバイト数。
     * @return 0
     */
    public static int CBL_AND(AbstractCobolField data1, CobolDataStorage data2, int length) {
        return CBL_AND(data1.getDataStorage(), data2, length);
    }

    /**
     * 組み込み関数CBL_ANDの実装。1バイトごとに論理積を取る。 先頭lengthバイトのデータについて論理積を計算し、計算結果を2番目の引数の先頭に格納する。
     *
     * @param data1 計算対象の1番目のCOBOL変数のバイト列。
     * @param data2 計算対象の2番目のCOBOL変数のバイト列。その変数のバイト列の先頭lengthバイトに計算結果が格納される。
     * @param length 計算するバイト数。
     * @return 0
     */
    public static int CBL_AND(CobolDataStorage data1, AbstractCobolField data2, int length) {
        return CBL_AND(data1, data2.getDataStorage(), length);
    }

    /**
     * 組み込み関数CBL_ANDの実装。1バイトごとに論理積を取る。 先頭lengthバイトのデータについて論理積を計算し、計算結果を2番目の引数の先頭に格納する。
     *
     * @param data1 計算対象の1番目のCOBOL変数。
     * @param data2 計算対象の2番目のCOBOL変数。その変数のバイト列の先頭lengthバイトに計算結果が格納される。
     * @param length 計算するバイト数。
     * @return 0
     */
    public static int CBL_AND(AbstractCobolField data1, AbstractCobolField data2, int length) {
        return CBL_AND(data1.getDataStorage(), data2.getDataStorage(), length);
    }

    /**
     * 組み込み関数CBL_ORの実装。1バイトごとに論理和を取る。 先頭lengthバイトのデータについて論理和を計算し、計算結果を2番目の引数の先頭に格納する。
     *
     * @param data1 計算対象の1番目のCOBOL変数のバイト列。
     * @param data2 計算対象の2番目のCOBOL変数のバイト列。先頭lengthバイトに計算結果が格納される。
     * @param length 計算するバイト数
     * @return 0
     */
    public static int CBL_OR(CobolDataStorage data1, CobolDataStorage data2, int length) {
        return CBL_COMMON_OPERATION(
                "CBL_OR",
                data1,
                data2,
                length,
                new Calculater() {
                    @Override
                    public byte calc(byte b1, byte b2) {
                        return (byte) (b1 | b2);
                    }
                });
    }

    /**
     * 組み込み関数CBL_ORの実装。1バイトごとに論理和を取る。 先頭lengthバイトのデータについて論理和を計算し、計算結果を2番目の引数の先頭に格納する。
     *
     * @param data1 計算対象の1番目のCOBOL変数のバイト列。
     * @param data2 計算対象の2番目のCOBOL変数。そのバイト列の先頭lengthバイトに計算結果が格納される。
     * @param length 計算するバイト数
     * @return 0
     */
    public static int CBL_OR(AbstractCobolField data1, CobolDataStorage data2, int length) {
        return CBL_OR(data1.getDataStorage(), data2, length);
    }

    /**
     * 組み込み関数CBL_ORの実装。1バイトごとに論理和を取る。 先頭lengthバイトのデータについて論理和を計算し、計算結果を2番目の引数の先頭に格納する。
     *
     * @param data1 計算対象の1番目のCOBOL変数のバイト列。
     * @param data2 計算対象の2番目のCOBOL変数。先頭lengthバイトに計算結果が格納される。
     * @param length 計算するバイト数
     * @return 0
     */
    public static int CBL_OR(CobolDataStorage data1, AbstractCobolField data2, int length) {
        return CBL_OR(data1, data2.getDataStorage(), length);
    }

    /**
     * 組み込み関数CBL_ORの実装。1バイトごとに論理和を取る。 先頭lengthバイトのデータについて論理和を計算し、計算結果を2番目の引数の先頭に格納する。
     *
     * @param data1 計算対象の1番目のCOBOL変数。
     * @param data2 計算対象の2番目のCOBOL変数。その変数のバイト列の先頭lengthバイトに計算結果が格納される。
     * @param length 計算するバイト数
     * @return 0
     */
    public static int CBL_OR(AbstractCobolField data1, AbstractCobolField data2, int length) {
        return CBL_OR(data1.getDataStorage(), data2.getDataStorage(), length);
    }

    private static int byte2Int(byte x) {
        return x & 0xFF;
    }

    private static byte int2Byte(int x) {
        return (byte) (x & 0xFF);
    }

    /**
     * 組み込み関数CBL_NORの実装。1バイトごとに否定論理和(NOR)を取る。 先頭lengthバイトのデータについてNOR演算を計算し、計算結果を2番目の引数の先頭に格納する。
     *
     * @param data1 計算対象の1番目のCOBOL変数のバイト列。
     * @param data2 計算対象の2番目のCOBOL変数のバイト列。先頭lengthバイトに計算結果が格納される。
     * @param length 計算するバイト数
     * @return 0
     */
    public static int CBL_NOR(CobolDataStorage data1, CobolDataStorage data2, int length) {
        return CBL_COMMON_OPERATION(
                "CBL_NOR",
                data1,
                data2,
                length,
                new Calculater() {
                    @Override
                    public byte calc(byte b1, byte b2) {
                        return int2Byte(~(byte2Int(b1) | int2Byte(b2)));
                    }
                });
    }

    /**
     * 組み込み関数CBL_NORの実装。1バイトごとに否定論理和(NOR)を取る。 先頭lengthバイトのデータについてNOR演算を計算し、計算結果を2番目の引数の先頭に格納する。
     *
     * @param data1 計算対象の1番目のCOBOL変数。
     * @param data2 計算対象の2番目のCOBOL変数のバイト列。先頭lengthバイトに計算結果が格納される。
     * @param length 計算するバイト数
     * @return 0
     */
    public static int CBL_NOR(AbstractCobolField data1, CobolDataStorage data2, int length) {
        return CBL_NOR(data1.getDataStorage(), data2, length);
    }

    /**
     * 組み込み関数CBL_NORの実装。1バイトごとに否定論理和(NOR)を取る。 先頭lengthバイトのデータについてNOR演算を計算し、計算結果を2番目の引数の先頭に格納する。
     *
     * @param data1 計算対象の1番目のCOBOL変数のバイト列。
     * @param data2 計算対象の2番目のCOBOL変数。そのバイト列の先頭lengthバイトに計算結果が格納される。
     * @param length 計算するバイト数
     * @return 0
     */
    public static int CBL_NOR(CobolDataStorage data1, AbstractCobolField data2, int length) {
        return CBL_NOR(data1, data2.getDataStorage(), length);
    }

    /**
     * 組み込み関数CBL_NORの実装。1バイトごとに否定論理和(NOR)を取る。 先頭lengthバイトのデータについてNOR演算を計算し、計算結果を2番目の引数の先頭に格納する。
     *
     * @param data1 計算対象の1番目のCOBOL変数。
     * @param data2 計算対象の2番目のCOBOL変数。その変数のバイト列の先頭lengthバイトに計算結果が格納される。
     * @param length 計算するバイト数
     * @return 0
     */
    public static int CBL_NOR(AbstractCobolField data1, AbstractCobolField data2, int length) {
        return CBL_NOR(data1.getDataStorage(), data2.getDataStorage(), length);
    }

    /**
     * 組み込み関数CBL_XORの実装。1バイトごとに排他的論理和を取る。 先頭lengthバイトのデータについて排他的論理和を計算し、計算結果を2番目の引数の先頭に格納する。
     *
     * @param data1 計算対象の1番目のCOBOL変数のバイト列。
     * @param data2 計算対象の2番目のCOBOL変数のバイト列。先頭lengthバイトに計算結果が格納される。
     * @param length 計算するバイト数
     * @return 0
     */
    public static int CBL_XOR(CobolDataStorage data1, CobolDataStorage data2, int length) {
        return CBL_COMMON_OPERATION(
                "CBL_XOR",
                data1,
                data2,
                length,
                new Calculater() {
                    @Override
                    public byte calc(byte b1, byte b2) {
                        return (byte) (b1 ^ b2);
                    }
                });
    }

    /**
     * 組み込み関数CBL_XORの実装。1バイトごとに排他的論理和を取る。 先頭lengthバイトのデータについて排他的論理和を計算し、計算結果を2番目の引数の先頭に格納する。
     *
     * @param data1 計算対象の1番目のCOBOL変数。
     * @param data2 計算対象の2番目のCOBOL変数のバイト列。先頭lengthバイトに計算結果が格納される。
     * @param length 計算するバイト数
     * @return 0
     */
    public static int CBL_XOR(AbstractCobolField data1, CobolDataStorage data2, int length) {
        return CBL_XOR(data1.getDataStorage(), data2, length);
    }

    /**
     * 組み込み関数CBL_XORの実装。1バイトごとに排他的論理和を取る。 先頭lengthバイトのデータについて排他的論理和を計算し、計算結果を2番目の引数の先頭に格納する。
     *
     * @param data1 計算対象の1番目のCOBOL変数のバイト列。
     * @param data2 計算対象の2番目のCOBOL変数。そのバイト列の先頭lengthバイトに計算結果が格納される。
     * @param length 計算するバイト数
     * @return 0
     */
    public static int CBL_XOR(CobolDataStorage data1, AbstractCobolField data2, int length) {
        return CBL_XOR(data1, data2.getDataStorage(), length);
    }

    /**
     * 組み込み関数CBL_XORの実装。1バイトごとに排他的論理和を取る。 先頭lengthバイトのデータについて排他的論理和を計算し、計算結果を2番目の引数の先頭に格納する。
     *
     * @param data1 計算対象の1番目のCOBOL変数。
     * @param data2 計算対象の2番目のCOBOL変数。その変数のバイト列の先頭lengthバイトに計算結果が格納される。
     * @param length 計算するバイト数
     * @return 0
     */
    public static int CBL_XOR(AbstractCobolField data1, AbstractCobolField data2, int length) {
        return CBL_XOR(data1.getDataStorage(), data2.getDataStorage(), length);
    }

    /**
     * 組み込み関数CBL_NIMPの実装。1バイトごとに非含意演算(data1 AND NOT data2)を行う。 先頭lengthバイトのデータについて非含意演算を計算し、計算結果を2番目の引数の先頭に格納する。
     *
     * @param data1 計算対象の1番目のCOBOL変数のバイト列。
     * @param data2 計算対象の2番目のCOBOL変数のバイト列。先頭lengthバイトに計算結果が格納される。
     * @param length 計算するバイト数
     * @return 0
     */
    public static int CBL_NIMP(CobolDataStorage data1, CobolDataStorage data2, int length) {
        return CBL_COMMON_OPERATION(
                "CBL_NIMP",
                data1,
                data2,
                length,
                new Calculater() {
                    @Override
                    public byte calc(byte b1, byte b2) {
                        return (byte) (b1 & (~b2));
                    }
                });
    }

    /**
     * 組み込み関数CBL_NIMPの実装。1バイトごとに非含意演算(data1 AND NOT data2)を行う。 先頭lengthバイトのデータについて非含意演算を計算し、計算結果を2番目の引数の先頭に格納する。
     *
     * @param data1 計算対象の1番目のCOBOL変数。
     * @param data2 計算対象の2番目のCOBOL変数のバイト列。先頭lengthバイトに計算結果が格納される。
     * @param length 計算するバイト数
     * @return 0
     */
    public static int CBL_NIMP(AbstractCobolField data1, CobolDataStorage data2, int length) {
        return CBL_NIMP(data1.getDataStorage(), data2, length);
    }

    /**
     * 組み込み関数CBL_NIMPの実装。1バイトごとに非含意演算(data1 AND NOT data2)を行う。 先頭lengthバイトのデータについて非含意演算を計算し、計算結果を2番目の引数の先頭に格納する。
     *
     * @param data1 計算対象の1番目のCOBOL変数のバイト列。
     * @param data2 計算対象の2番目のCOBOL変数。そのバイト列の先頭lengthバイトに計算結果が格納される。
     * @param length 計算するバイト数
     * @return 0
     */
    public static int CBL_NIMP(CobolDataStorage data1, AbstractCobolField data2, int length) {
        return CBL_NIMP(data1, data2.getDataStorage(), length);
    }

    /**
     * 組み込み関数CBL_NIMPの実装。1バイトごとに非含意演算(data1 AND NOT data2)を行う。 先頭lengthバイトのデータについて非含意演算を計算し、計算結果を2番目の引数の先頭に格納する。
     *
     * @param data1 計算対象の1番目のCOBOL変数。
     * @param data2 計算対象の2番目のCOBOL変数。その変数のバイト列の先頭lengthバイトに計算結果が格納される。
     * @param length 計算するバイト数
     * @return 0
     */
    public static int CBL_NIMP(AbstractCobolField data1, AbstractCobolField data2, int length) {
        return CBL_NIMP(data1.getDataStorage(), data2.getDataStorage(), length);
    }

    /**
     * 組み込み関数CBL_EQの実装。1バイトごとに等価演算(XNOR)を行う。 先頭lengthバイトのデータについて等価演算を計算し、計算結果を2番目の引数の先頭に格納する。
     *
     * @param data1 計算対象の1番目のCOBOL変数のバイト列。
     * @param data2 計算対象の2番目のCOBOL変数のバイト列。先頭lengthバイトに計算結果が格納される。
     * @param length 計算するバイト数
     * @return 0
     */
    public static int CBL_EQ(CobolDataStorage data1, CobolDataStorage data2, int length) {
        return CBL_COMMON_OPERATION(
                "CBL_EQ",
                data1,
                data2,
                length,
                new Calculater() {
                    @Override
                    public byte calc(byte b1, byte b2) {
                        return (byte) ~(b1 ^ b2);
                    }
                });
    }

    /**
     * 組み込み関数CBL_EQの実装。1バイトごとに等価演算(XNOR)を行う。 先頭lengthバイトのデータについて等価演算を計算し、計算結果を2番目の引数の先頭に格納する。
     *
     * @param data1 計算対象の1番目のCOBOL変数。
     * @param data2 計算対象の2番目のCOBOL変数のバイト列。先頭lengthバイトに計算結果が格納される。
     * @param length 計算するバイト数
     * @return 0
     */
    public static int CBL_EQ(AbstractCobolField data1, CobolDataStorage data2, int length) {
        return CBL_EQ(data1.getDataStorage(), data2, length);
    }

    /**
     * 組み込み関数CBL_EQの実装。1バイトごとに等価演算(XNOR)を行う。 先頭lengthバイトのデータについて等価演算を計算し、計算結果を2番目の引数の先頭に格納する。
     *
     * @param data1 計算対象の1番目のCOBOL変数のバイト列。
     * @param data2 計算対象の2番目のCOBOL変数。そのバイト列の先頭lengthバイトに計算結果が格納される。
     * @param length 計算するバイト数
     * @return 0
     */
    public static int CBL_EQ(CobolDataStorage data1, AbstractCobolField data2, int length) {
        return CBL_EQ(data1, data2.getDataStorage(), length);
    }

    /**
     * 組み込み関数CBL_EQの実装。1バイトごとに等価演算(XNOR)を行う。 先頭lengthバイトのデータについて等価演算を計算し、計算結果を2番目の引数の先頭に格納する。
     *
     * @param data1 計算対象の1番目のCOBOL変数。
     * @param data2 計算対象の2番目のCOBOL変数。その変数のバイト列の先頭lengthバイトに計算結果が格納される。
     * @param length 計算するバイト数
     * @return 0
     */
    public static int CBL_EQ(AbstractCobolField data1, AbstractCobolField data2, int length) {
        return CBL_EQ(data1.getDataStorage(), data2.getDataStorage(), length);
    }

    /**
     * 組み込み関数CBL_NOTの実装。1バイトごとに論理否定(NOT)を行う。 先頭lengthバイトのデータについて論理否定を計算し、計算結果をdataに格納する。
     *
     * @param data 計算対象のCOBOL変数のバイト列。先頭lengthバイトに計算結果が格納される。
     * @param length 計算するバイト数
     * @return 0
     */
    public static int CBL_NOT(CobolDataStorage data, int length) {
        CobolUtil.COB_CHK_PARMS("CBL_NOT", 2);
        if (length <= 0) {
            return 0;
        }
        for (int n = 0; n < length; ++n) {
            byte b = data.getByte(n);
            data.setByte(n, (byte) ~b);
        }
        return 0;
    }

    /**
     * 組み込み関数CBL_NOTの実装。1バイトごとに論理否定(NOT)を行う。 先頭lengthバイトのデータについて論理否定を計算し、計算結果をdataに格納する。
     *
     * @param data 計算対象のCOBOL変数。そのバイト列の先頭lengthバイトに計算結果が格納される。
     * @param length 計算するバイト数
     * @return 0
     */
    public static int CBL_NOT(AbstractCobolField data, int length) {
        return CBL_NOT(data.getDataStorage(), length);
    }

    /**
     * 組み込み関数CBL_XF4の実装。data2の各バイトの最下位ビットを取り出し、data1の対応するバイトのビット位置(7-n)にORで設定する。
     *
     * @param data1 ビット設定先のCOBOL変数のバイト列。8バイト分が更新される。
     * @param data2 ビット値を提供するCOBOL変数のバイト列。各バイトの最下位ビットが使用される。
     * @return 0
     */
    public static int CBL_XF4(CobolDataStorage data1, CobolDataStorage data2) {
        CobolUtil.COB_CHK_PARMS("CBL_XF4", 2);
        for (int n = 0; n < 8; ++n) {
            byte b1 = data1.getByte(n);
            byte b2 = data2.getByte(n);
            data1.setByte(n, (byte) (b1 | (b2 & 1) << (7 - n)));
        }
        return 0;
    }

    /**
     * 組み込み関数CBL_XF4の実装。data2の各バイトの最下位ビットを取り出し、data1の対応するバイトのビット位置(7-n)にORで設定する。
     *
     * @param data1 ビット設定先のCOBOL変数。8バイト分が更新される。
     * @param data2 ビット値を提供するCOBOL変数のバイト列。各バイトの最下位ビットが使用される。
     * @return 0
     */
    public static int CBL_XF4(AbstractCobolField data1, CobolDataStorage data2) {
        return CBL_XF4(data1.getDataStorage(), data2);
    }

    /**
     * 組み込み関数CBL_XF4の実装。data2の各バイトの最下位ビットを取り出し、data1の対応するバイトのビット位置(7-n)にORで設定する。
     *
     * @param data1 ビット設定先のCOBOL変数のバイト列。8バイト分が更新される。
     * @param data2 ビット値を提供するCOBOL変数。各バイトの最下位ビットが使用される。
     * @return 0
     */
    public static int CBL_XF4(CobolDataStorage data1, AbstractCobolField data2) {
        return CBL_XF4(data1, data2.getDataStorage());
    }

    /**
     * 組み込み関数CBL_XF4の実装。data2の各バイトの最下位ビットを取り出し、data1の対応するバイトのビット位置(7-n)にORで設定する。
     *
     * @param data1 ビット設定先のCOBOL変数。8バイト分が更新される。
     * @param data2 ビット値を提供するCOBOL変数。各バイトの最下位ビットが使用される。
     * @return 0
     */
    public static int CBL_XF4(AbstractCobolField data1, AbstractCobolField data2) {
        return CBL_XF4(data1.getDataStorage(), data2.getDataStorage());
    }

    /**
     * 組み込み関数CBL_XF5の実装。data1の先頭バイトの各ビットを取り出し、data2の対応するバイトに1または0として格納する。
     *
     * @param data1 ビット値を提供するCOBOL変数のバイト列。先頭1バイトの各ビットが使用される。
     * @param data2 ビット展開先のCOBOL変数のバイト列。8バイト分にビット値(0または1)が格納される。
     * @return 0
     */
    public static int CBL_XF5(CobolDataStorage data1, CobolDataStorage data2) {
        CobolUtil.COB_CHK_PARMS("CBL_XF5", 2);
        byte b1 = data1.getByte(0);
        for (int n = 0; n < 8; ++n) {
            data2.setByte(n, (byte) ((b1 & (1 << (7 - n))) != 0 ? 1 : 0));
        }
        return 0;
    }

    /**
     * 組み込み関数CBL_XF5の実装。data1の先頭バイトの各ビットを取り出し、data2の対応するバイトに1または0として格納する。
     *
     * @param data1 ビット値を提供するCOBOL変数。先頭1バイトの各ビットが使用される。
     * @param data2 ビット展開先のCOBOL変数のバイト列。8バイト分にビット値(0または1)が格納される。
     * @return 0
     */
    public static int CBL_XF5(AbstractCobolField data1, CobolDataStorage data2) {
        return CBL_XF5(data1.getDataStorage(), data2);
    }

    /**
     * 組み込み関数CBL_XF5の実装。data1の先頭バイトの各ビットを取り出し、data2の対応するバイトに1または0として格納する。
     *
     * @param data1 ビット値を提供するCOBOL変数のバイト列。先頭1バイトの各ビットが使用される。
     * @param data2 ビット展開先のCOBOL変数。8バイト分にビット値(0または1)が格納される。
     * @return 0
     */
    public static int CBL_XF5(CobolDataStorage data1, AbstractCobolField data2) {
        return CBL_XF5(data1, data2.getDataStorage());
    }

    /**
     * 組み込み関数CBL_XF5の実装。data1の先頭バイトの各ビットを取り出し、data2の対応するバイトに1または0として格納する。
     *
     * @param data1 ビット値を提供するCOBOL変数。先頭1バイトの各ビットが使用される。
     * @param data2 ビット展開先のCOBOL変数。8バイト分にビット値(0または1)が格納される。
     * @return 0
     */
    public static int CBL_XF5(AbstractCobolField data1, AbstractCobolField data2) {
        return CBL_XF5(data1.getDataStorage(), data2.getDataStorage());
    }

    /**
     * 組み込み関数CBL_X91の実装。COBOLスイッチの設定・取得を行う汎用ルーチン。
     * funcの値により動作が異なる。
     * func=11の場合、parmの各バイト(0~7)に基づきCOBOLスイッチを設定する(0=OFF、1=ON、その他=変更なし)。
     * func=12の場合、現在のCOBOLスイッチの状態をparmの各バイト(0~7)に格納する(OFF=0、ON=1)。
     * func=16の場合、保存済みCALLパラメータ数をparmの先頭バイトに格納する。
     *
     * @param result 結果コード格納先。成功時は0、未知の機能コードの場合は1が格納される。
     * @param func 機能コード。11=スイッチ設定、12=スイッチ取得、16=保存済みCALLパラメータ数の取得。
     * @param parm パラメータ。機能コードにより用途が異なる。
     * @return 0
     */
    public static int CBL_X91(
            CobolDataStorage result, CobolDataStorage func, CobolDataStorage parm) {
        switch (func.getByte(0)) {
            case 11:
                for (int i = 0; i < 8; ++i) {
                    if (parm.getByte(i) == 0) {
                        CobolUtil.getSwitches()[i] = false;
                    } else if (parm.getByte(i) == 1) {
                        CobolUtil.getSwitches()[i] = true;
                    }
                }
                result.setByte(0, (byte) 0);
                break;
            case 12:
                for (int i = 0; i < 8; ++i) {
                    parm.setByte(i, (byte) (CobolUtil.getSwitches()[i] ? 1 : 0));
                }
                result.setByte(0, (byte) 0);
                break;
            case 16:
                parm.setByte(0, (byte) CobolUtil.getSaveCallParams());
                result.setByte(0, (byte) 0);
                break;
            default:
                result.setByte(0, (byte) 1);
                break;
        }
        return 0;
    }

    /**
     * 組み込み関数CBL_TOLOWERの実装。先頭lengthバイトのデータについて、Shift_JISエンコーディングを用いて各バイトを小文字に変換する。
     *
     * @param data 変換対象のCOBOL変数のバイト列。先頭lengthバイトが小文字に変換される。
     * @param length 変換するバイト数
     * @return 0
     */
    public static int CBL_TOLOWER(CobolDataStorage data, int length) {
        CobolUtil.COB_CHK_PARMS("CBL_TOLOWER", 2);
        if (length <= 0) {
            return 0;
        }

        for (int n = 0; n < length; ++n) {
            byte b = data.getByte(n);
            byte[] bytes = {b};
            byte result =
                    new String(bytes, AbstractCobolField.charSetSJIS)
                            .toLowerCase()
                            .getBytes(AbstractCobolField.charSetSJIS)[0];
            data.setByte(n, result);
        }
        return 0;
    }

    /**
     * 組み込み関数CBL_TOLOWERの実装。先頭lengthバイトのデータについて、Shift_JISエンコーディングを用いて各バイトを小文字に変換する。
     *
     * @param data 変換対象のCOBOL変数。そのバイト列の先頭lengthバイトが小文字に変換される。
     * @param length 変換するバイト数
     * @return 0
     */
    public static int CBL_TOLOWER(AbstractCobolField data, int length) {
        return CBL_TOLOWER(data.getDataStorage(), length);
    }

    /**
     * 組み込み関数CBL_TOUPPERの実装。先頭lengthバイトのデータについて、Shift_JISエンコーディングを用いて各バイトを大文字に変換する。
     *
     * @param data 変換対象のCOBOL変数のバイト列。先頭lengthバイトが大文字に変換される。
     * @param length 変換するバイト数
     * @return 0
     */
    public static int CBL_TOUPPER(CobolDataStorage data, int length) {
        CobolUtil.COB_CHK_PARMS("CBL_TOUPPER", 2);
        if (length <= 0) {
            return 0;
        }

        for (int n = 0; n < length; ++n) {
            byte b = data.getByte(n);
            byte[] bytes = {b};
            byte result =
                    new String(bytes, AbstractCobolField.charSetSJIS)
                            .toUpperCase()
                            .getBytes(AbstractCobolField.charSetSJIS)[0];
            data.setByte(n, result);
        }
        return 0;
    }

    /**
     * 組み込み関数CBL_TOUPPERの実装。先頭lengthバイトのデータについて、Shift_JISエンコーディングを用いて各バイトを大文字に変換する。
     *
     * @param data 変換対象のCOBOL変数。そのバイト列の先頭lengthバイトが大文字に変換される。
     * @param length 変換するバイト数
     * @return 0
     */
    public static int CBL_TOUPPER(AbstractCobolField data, int length) {
        return CBL_TOUPPER(data.getDataStorage(), length);
    }

    /**
     * 組み込み関数CBL_OC_NANOSLEEPの実装。指定されたナノ秒数だけスレッドをスリープさせる。
     * 現在のモジュールの最初のプロシージャパラメータから整数値(ナノ秒)を取得し、その値が正の場合にスリープを実行する。
     *
     * @param data CBL_OC_NANOSLEEPの引数として指定されたCOBOL変数のバイト列。
     * @return 0
     */
    public static int CBL_OC_NANOSLEEP(CobolDataStorage data) {
        CobolUtil.COB_CHK_PARMS("CBL_OC_NANOSLEEP", 1);

        AbstractCobolField param = CobolModule.getCurrentModule().cob_procedure_parameters.get(0);
        if (param != null) {
            long nsecs = param.getLong();
            System.out.println(nsecs);
            if (nsecs > 0) {
                try {
                    TimeUnit.NANOSECONDS.sleep(nsecs);
                } catch (InterruptedException e) {
                }
            }
        }
        return 0;
    }

    /**
     * 組み込み関数CBL_OC_NANOSLEEPの実装。指定されたナノ秒数だけスレッドをスリープさせる。
     * 現在のモジュールの最初のプロシージャパラメータから整数値(ナノ秒)を取得し、その値が正の場合にスリープを実行する。
     *
     * @param field CBL_OC_NANOSLEEPの引数として指定されたCOBOL変数。
     * @return 0
     */
    public static int CBL_OC_NANOSLEEP(AbstractCobolField field) {
        CobolSystemRoutine.CBL_OC_NANOSLEEP(field.getDataStorage());
        return 0;
    }

    /**
     * 組み込み関数calledByの実装。呼び出し元プログラムの名前を取得する。
     *
     * @param data 呼び出し元プログラム名の格納先となるCOBOL変数のバイト列。
     * @return CobolModule.calledByの戻り値
     */
    public static int calledBy(CobolDataStorage data) {
        CobolUtil.COB_CHK_PARMS("calledby", 1);
        return CobolModule.calledBy(data);
    }

    /**
     * 組み込み関数listDirectoryの実装。ディレクトリの内容を一覧表示する。
     * 最初のパラメータで操作コードを指定する。
     * 操作コード1の場合、指定されたディレクトリを開き一覧取得を開始する。
     * 操作コード2の場合、次のディレクトリエントリを取得する。
     * 操作コード3の場合、ディレクトリの一覧取得を終了する。
     *
     * @param data 操作に応じたCOBOL変数のバイト列の可変長引数。
     * @return 成功時は0、失敗時は-1
     */
    public static int listDir(CobolDataStorage... data) {
        CobolUtil.COB_CHK_PARMS("listDirectory", 1);
        List<AbstractCobolField> params = CobolModule.getCurrentModule().cob_procedure_parameters;
        if (params == null) {
            return -1;
        }
        int paramsSize = params.size();
        if (paramsSize < 1) {
            return -1;
        }
        AbstractCobolField operationCodeFiled = params.get(0);
        if (operationCodeFiled == null) {
            return -1;
        }

        int operationCode = params.get(0).getInt();
        switch (operationCode) {
            case 1: // LISTDIR-OPEN(value:1)
                if (paramsSize < 3) {
                    return -1;
                }
                return listDirOpen(params.get(1), params.get(2));
            case 2: // LISTDIR-OPEN(value:2)
                if (paramsSize < 3) {
                    return -1;
                }
                return listDirNext(params.get(1), params.get(2));
            case 3: // LISTDIR-OPEN(value:3)
                if (paramsSize < 2) {
                    return -1;
                }
                return lsitDirClose(params.get(1));
            default:
                return -1;
        }
    }

    /** C$LIST-DIRECTORYで走査中のディレクトリエントリ(スレッドごとに保持する) */
    private static final ThreadLocal<List<Path>> dirListHolder = new ThreadLocal<>();

    /** 現在のスレッドに紐づくディレクトリ走査の状態を破棄する。実行単位の終了時に呼び出す。 */
    public static void resetThreadState() {
        dirListHolder.remove();
    }

    private static int listDirOpen(
            AbstractCobolField dirPathField, AbstractCobolField patternField) {
        // FIXME: now not use patternField.
        String dirPath = strFromField(dirPathField);
        List<Path> dirList;
        try {
            dirList =
                    Files.list(Paths.get(dirPath))
                            .map(p -> p.getFileName())
                            .collect(Collectors.toList());
        } catch (IOException e) {
            return -1;
        }
        dirList.add(Paths.get("."));
        dirList.add(Paths.get(".."));
        Collections.sort(dirList);
        dirListHolder.set(dirList);
        return 0;
    }

    private static int listDirNext(
            AbstractCobolField handleField, AbstractCobolField fileNameField) {
        // FIXME: now not use handleField.
        List<Path> dirList = dirListHolder.get();
        if (dirList == null) {
            return -1;
        }

        CobolDataStorage storage = fileNameField.getDataStorage();
        int fieldSize = fileNameField.getSize();
        storage.memset(' ', fieldSize);

        if (dirList.isEmpty()) {
            return -1;
        }

        Path filePath = dirList.get(0);
        dirList.remove(0);
        byte[] filePathBytes = filePath.toString().getBytes(AbstractCobolField.charSetSJIS);
        int filePathStringLength = filePathBytes.length;
        int copySize = Math.min(fieldSize, filePathStringLength);
        storage.memcpy(filePathBytes, copySize);
        return 0;
    }

    private static int lsitDirClose(AbstractCobolField handleField) {
        // FIXME: now not use handleField
        dirListHolder.remove();
        return 0;
    }

    private static String strFromField(AbstractCobolField field) {
        if (field == null) {
            return null;
        }

        int i;
        CobolDataStorage storage = field.getDataStorage();
        for (i = field.getSize() - 1; i >= 0; --i) {
            byte b = storage.getByte(i);
            if (b != ' ' && b != 0) {
                break;
            }
        }

        StringBuilder sb = new StringBuilder();
        boolean quoteSwitch = false;
        for (int n = 0; n <= i; ++n) {
            byte b = storage.getByte(n);
            if (b == '\'') {
                quoteSwitch = !quoteSwitch;
                continue;
            }
            if (quoteSwitch) {
                sb.append((char) b);
                continue;
            } else if (b == ' ' || b == 0) {
                break;
            } else {
                sb.append((char) b);
            }
        }
        return sb.toString();
    }
}
