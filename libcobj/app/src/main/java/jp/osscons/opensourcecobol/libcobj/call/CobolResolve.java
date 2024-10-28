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

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import jp.osscons.opensourcecobol.libcobj.common.CobolConstant;
import jp.osscons.opensourcecobol.libcobj.common.CobolUtil;
import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolExceptionId;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolRuntimeException;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolStopRunException;

/** 動的にクラスファイルを読み込んでCALL文のような機能を実装するためのクラス */
public class CobolResolve {

    /** プログラム名とCobolRunnableインスタンスの対応表 */
    private static Map<String, CobolRunnable> callTable;

    /** 例外コードと例外名の対応を表現する */
    public static Map<Integer, String> cobException;

    /** 名前の変換方法(小文字か大文字)を示す変数 */
    private static int name_convert;

    // TODO resolve_pathsの利用方法
    /** システムで設定された区切り文字で区切られた0個以上のパス 動的に読み込むクラスファイルを検索する場所を示す. */
    private static List<String> resolve_paths;

    /** システムで設定された区切り文字で区切られた0個以上のパス 動的に読み込むクラスファイルを検索するパッケージ名を示す. */
    private static List<String> package_paths;

    static {
        callTable = new HashMap<>();
        name_convert = 0;
        resolve_paths = new ArrayList<String>();
        package_paths = new ArrayList<String>();
        cobException = new HashMap<>();

        cobException.put(0xFFFF, "EC-ALL");
        cobException.put(0x0100, "EC-ARGUMENT");
        cobException.put(0x0101, "EC-ARGUMENT-FUNCTION");
        cobException.put(0x0102, "EC-ARGUMENT-IMP");
        cobException.put(0x0200, "EC-BOUND");
        cobException.put(0x0201, "EC-BOUND-IMP");
        cobException.put(0x0202, "EC-BOUND-ODO");
        cobException.put(0x0203, "EC-BOUND-OVERFLOW");
        cobException.put(0x0204, "EC-BOUND-PTR");
        cobException.put(0x0205, "EC-BOUND-REF-MOD");
        cobException.put(0x0206, "EC-BOUND-SET");
        cobException.put(0x0207, "EC-BOUND-SUBSCRIPT");
        cobException.put(0x0208, "EC-BOUND-TABLE-LIMIT");
        cobException.put(0x0300, "EC-DATA");
        cobException.put(0x0301, "EC-DATA-CONVERSION");
        cobException.put(0x0302, "EC-DATA-IMP");
        cobException.put(0x0303, "EC-DATA-INCOMPATIBLE");
        cobException.put(0x0304, "EC-DATA-INTEGRITY");
        cobException.put(0x0305, "EC-DATA-PTR-NULL");
        cobException.put(0x0306, "EC-DATA-INFINITY");
        cobException.put(0x0307, "EC-DATA-NEGATIVE-INFINITY");
        cobException.put(0x0308, "EC-DATA-NOT_A_NUMBER");
        cobException.put(0x0400, "EC-FLOW");
        cobException.put(0x0401, "EC-FLOW-GLOBAL-EXIT");
        cobException.put(0x0402, "EC-FLOW-GLOBAL-GOBACK");
        cobException.put(0x0403, "EC-FLOW-IMP");
        cobException.put(0x0404, "EC-FLOW-RELEASE");
        cobException.put(0x0405, "EC-FLOW-REPORT");
        cobException.put(0x0406, "EC-FLOW-RETURN");
        cobException.put(0x0407, "EC-FLOW-SEARCH");
        cobException.put(0x0408, "EC-FLOW-USE");
        cobException.put(0x0500, "EC-I-O");
        cobException.put(0x0501, "EC-I-O-AT-END");
        cobException.put(0x0502, "EC-I-O-EOP");
        cobException.put(0x0503, "EC-I-O-EOP-OVERFLOW");
        cobException.put(0x0504, "EC-I-O-FILE-SHARING");
        cobException.put(0x0505, "EC-I-O-IMP");
        cobException.put(0x0506, "EC-I-O-INVALID-KEY");
        cobException.put(0x0507, "EC-I-O-LINAGE");
        cobException.put(0x0508, "EC-I-O-LOGIC-ERROR");
        cobException.put(0x0509, "EC-I-O-PERMANENT-ERROR");
        cobException.put(0x050A, "EC-I-O-RECORD-OPERATION");
        cobException.put(0x0600, "EC-IMP");
        cobException.put(0x0601, "EC-IMP-ACCEPT");
        cobException.put(0x0602, "EC-IMP-DISPLAY");
        cobException.put(0x0700, "EC-LOCALE");
        cobException.put(0x0701, "EC-LOCALE-IMP");
        cobException.put(0x0702, "EC-LOCALE-INCOMPATIBLE");
        cobException.put(0x0703, "EC-LOCALE-INVALID");
        cobException.put(0x0704, "EC-LOCALE-INVALID-PTR");
        cobException.put(0x0705, "EC-LOCALE-MISSING");
        cobException.put(0x0706, "EC-LOCALE-SIZE");
        cobException.put(0x0800, "EC-OO");
        cobException.put(0x0801, "EC-OO-CONFORMANCE");
        cobException.put(0x0802, "EC-OO-EXCEPTION");
        cobException.put(0x0803, "EC-OO-IMP");
        cobException.put(0x0804, "EC-OO-METHOD");
        cobException.put(0x0805, "EC-OO-NULL");
        cobException.put(0x0806, "EC-OO-RESOURCE");
        cobException.put(0x0807, "EC-OO-UNIVERSAL");
        cobException.put(0x0900, "EC-ORDER");
        cobException.put(0x0901, "EC-ORDER-IMP");
        cobException.put(0x0902, "EC-ORDER-NOT-SUPPORTED");
        cobException.put(0x0A00, "EC-OVERFLOW");
        cobException.put(0x0A01, "EC-OVERFLOW-IMP");
        cobException.put(0x0A02, "EC-OVERFLOW-STRING");
        cobException.put(0x0A03, "EC-OVERFLOW-UNSTRING");
        cobException.put(0x0B00, "EC-PROGRAM");
        cobException.put(0x0B01, "EC-PROGRAM-ARG-MISMATCH");
        cobException.put(0x0B02, "EC-PROGRAM-ARG-OMITTED");
        cobException.put(0x0B03, "EC-PROGRAM-CANCEL-ACTIVE");
        cobException.put(0x0B04, "EC-PROGRAM-IMP");
        cobException.put(0x0B05, "EC-PROGRAM-NOT-FOUND");
        cobException.put(0x0B06, "EC-PROGRAM-PTR-NULL");
        cobException.put(0x0B07, "EC-PROGRAM-RECURSIVE-CALL");
        cobException.put(0x0B08, "EC-PROGRAM-RESOURCES");
        cobException.put(0x0C00, "EC-RAISING");
        cobException.put(0x0C01, "EC-RAISING-IMP");
        cobException.put(0x0C02, "EC-RAISING-NOT-SPECIFIED");
        cobException.put(0x0D00, "EC-RANGE");
        cobException.put(0x0D01, "EC-RANGE-IMP");
        cobException.put(0x0D02, "EC-RANGE-INDEX");
        cobException.put(0x0D03, "EC-RANGE-INSPECT-SIZE");
        cobException.put(0x0D04, "EC-RANGE-INVALID");
        cobException.put(0x0D05, "EC-RANGE-PERFORM-VARYING");
        cobException.put(0x0D06, "EC-RANGE-PTR");
        cobException.put(0x0D07, "EC-RANGE-SEARCH-INDEX");
        cobException.put(0x0D08, "EC-RANGE-SEARCH-NO-MATCH");
        cobException.put(0x0E00, "EC-REPORT");
        cobException.put(0x0E01, "EC-REPORT-ACTIVE");
        cobException.put(0x0E02, "EC-REPORT-COLUMN-OVERLAP");
        cobException.put(0x0E03, "EC-REPORT-FILE-MODE");
        cobException.put(0x0E04, "EC-REPORT-IMP");
        cobException.put(0x0E05, "EC-REPORT-INACTIVE");
        cobException.put(0x0E06, "EC-REPORT-LINE-OVERLAP");
        cobException.put(0x0E08, "EC-REPORT-NOT-TERMINATED");
        cobException.put(0x0E09, "EC-REPORT-PAGE-LIMIT");
        cobException.put(0x0E0A, "EC-REPORT-PAGE-WIDTH");
        cobException.put(0x0E0B, "EC-REPORT-SUM-SIZE");
        cobException.put(0x0E0C, "EC-REPORT-VARYING");
        cobException.put(0x0F00, "EC-SCREEN");
        cobException.put(0x0F01, "EC-SCREEN-FIELD-OVERLAP");
        cobException.put(0x0F02, "EC-SCREEN-IMP");
        cobException.put(0x0F03, "EC-SCREEN-ITEM-TRUNCATED");
        cobException.put(0x0F04, "EC-SCREEN-LINE-NUMBER");
        cobException.put(0x0F05, "EC-SCREEN-STARTING-COLUMN");
        cobException.put(0x1000, "EC-SIZE");
        cobException.put(0x1001, "EC-SIZE-ADDRESS");
        cobException.put(0x1002, "EC-SIZE-EXPONENTIATION");
        cobException.put(0x1003, "EC-SIZE-IMP");
        cobException.put(0x1004, "EC-SIZE-OVERFLOW");
        cobException.put(0x1005, "EC-SIZE-TRUNCATION");
        cobException.put(0x1006, "EC-SIZE-UNDERFLOW");
        cobException.put(0x1007, "EC-SIZE-ZERO-DIVIDE");
        cobException.put(0x1100, "EC-SORT-MERGE");
        cobException.put(0x1101, "EC-SORT-MERGE-ACTIVE");
        cobException.put(0x1102, "EC-SORT-MERGE-FILE-OPEN");
        cobException.put(0x1103, "EC-SORT-MERGE-IMP");
        cobException.put(0x1104, "EC-SORT-MERGE-RELEASE");
        cobException.put(0x1105, "EC-SORT-MERGE-RETURN");
        cobException.put(0x1106, "EC-SORT-MERGE-SEQUENCE");
        cobException.put(0x1200, "EC-STORAGE");
        cobException.put(0x1201, "EC-STORAGE-IMP");
        cobException.put(0x1202, "EC-STORAGE-NOT-ALLOC");
        cobException.put(0x1203, "EC-STORAGE-NOT-AVAIL");
        cobException.put(0x1300, "EC-USER");
        cobException.put(0x1400, "EC-VALIDATE");
        cobException.put(0x1401, "EC-VALIDATE-CONTENT");
        cobException.put(0x1402, "EC-VALIDATE-FORMAT");
        cobException.put(0x1403, "EC-VALIDATE-IMP");
        cobException.put(0x1404, "EC-VALIDATE-RELATION");
        cobException.put(0x1405, "EC-VALIDATE-VARYING");
        cobException.put(0x1500, "EC-FUNCTION");
        cobException.put(0x1501, "EC-FUNCTION-NOT-FOUND");
        cobException.put(0x1502, "EC-FUNCTION-PTR-INVALID");
        cobException.put(0x1503, "EC-FUNCTION-PTR-NULL");
        cobException.put(0x1600, "EC-XML");
        cobException.put(0x1601, "EC-XML-CODESET");
        cobException.put(0x1602, "EC-XML-CODESET-CONVERSION");
        cobException.put(0x1603, "EC-XML-COUNT");
        cobException.put(0x1604, "EC-XML-DOCUMENT-TYPE");
        cobException.put(0x1605, "EC-XML-IMPLICIT-CLOSE");
        cobException.put(0x1606, "EC-XML-INVALID");
        cobException.put(0x1607, "EC-XML-NAMESPACE");
        cobException.put(0x1608, "EC-XML-STACKED-OPEN");
        cobException.put(0x1609, "EC-XML-RANGE");
    }

    /**
     * 下記の環境変数を読み込み、CobolResolve内で定義されたメソッドの動作が変わる。<br>
     * 環境変数COB_LOAD_CASEにLOWERが指定されているときは、resolveメソッドに渡された引数を小文字に変換してから処理を開始する。<br>
     * 環境変数COB_LOAD_CASEにUPPERが指定されているときは、resolveメソッドに渡された引数を大文字に変換してから処理を開始する。<br>
     * 環境変数COB_LIBRARY_PATHにディレクトリパスが指定されている場合、 resolveメソッドが検索するモジュールのディレクトリパスのリストにそれを追加する。<br>
     * 環境変数COB_PACKAGE_PATHにディレクトリパスが指定されている場合、 resolveメソッドが検索するモジュールのパッケージパスのリストにそれを追加する。
     * 環境変数COB_PRE_LOADは未実装
     */
    public static void cobolInitCall() {

        String s;
        String buf;

        // //用途不明
        // call_filename_buff = cob_malloc (CALL_FILEBUFF_SIZE);
        // call_entry_buff = cob_malloc (COB_SMALL_BUFF);
        // call_entry2_buff = cob_malloc (COB_SMALL_BUFF);

        s = CobolUtil.getEnv("COB_LOAD_CASE");
        if (s != null) {
            String sU = s.toUpperCase();
            if ("LOWER".equals(sU)) {
                name_convert = 1;
            } else if ("UPPER".equals(sU)) {
                name_convert = 2;
            }
        }

        s = CobolUtil.getEnv("COB_LIBRARY_PATH");
        if (s == null || s.equals("")) {
            buf = "." + System.getProperty("path.separator") + CobolConstant.COB_LIBRARY_PATH;
        } else {
            buf =
                    s
                            + System.getProperty("path.separator")
                            + "."
                            + System.getProperty("path.separator")
                            + CobolConstant.COB_LIBRARY_PATH;
        }
        setLibraryPath(buf);

        s = CobolUtil.getEnv("COB_PACKAGE_PATH");
        setPackagePath(s);

        // TODO プリロードの扱いを検討する
        s = CobolUtil.getEnv("COB_PRE_LOAD");

        // 用途不明
        // call_buffer = cob_malloc (CALL_BUFF_SIZE);
        // call_lastsize = CALL_BUFF_SIZE;
        // for (psyst = (struct system_table *)&system_tab[0]; psyst->syst_name;
        // ++psyst) {
        // insert (psyst->syst_name, psyst->syst_call, NULL);
        // }
    }

    /**
     * ライブラリパスを更新する
     *
     * @param path 区切り文字で区切られた0個以上のパスを示す文字列
     */
    private static void setLibraryPath(String path) {
        if (!resolve_paths.isEmpty()) {
            resolve_paths.clear();
        }

        String[] paths = path.split(System.getProperty("path.separator"));

        for (String path1 : paths) {
            if (path1.length() > 0) {
                resolve_paths.add(path1);
            }
        }
    }

    /**
     * パッケージへパスを更新する
     *
     * @param path 区切り文字で区切られた0個以上のパスを示す文字列
     */
    private static void setPackagePath(String path) {
        if (!package_paths.isEmpty()) {
            package_paths.clear();
        }

        String[] paths = path.split(System.getProperty("path.separator"));

        for (String path1 : paths) {
            if (path1.length() > 0) {
                package_paths.add(path1);
            }
        }
    }

    /**
     * パッケージ名とクラス名から、クラスを動的に読み込む。 このメソッドは以下のように、環境変数の設定値によって動作が変わる。<br>
     * 環境変数COB_LOAD_CASEにLOWERが指定されているときは、cobolFieldの示す文字列全体を小文字に変換したものを検索する。<br>
     * 環境変数COB_LOAD_CASEにUPPERが指定されているときは、cobolFieldの示す文字列全体を小文字に変換したものを検索する。<br>
     * 環境変数COB_LIBRARY_PATHにディレクトリパスが指定されている場合、 このメソッドが検索するモジュールのディレクトリパスのリストにそれを追加する。<br>
     * 環境変数COB_PACKAGE_PATHにディレクトリパスが指定されている場合、 このメソッドが検索するモジュールのパッケージパスのリストにそれを追加する。
     *
     * @param packageName 検索するクラスのパッケージ名
     * @param cobolField 読み込むクラスの名前を格納するCOBOL変数
     * @param runner CobolRunnableを実装したクラス。nullでもよい。
     * @return runnerがnullでない場合はrunnerを返し、そうでないときはクラス名とパッケージ名を元に検索処理を実施する。
     *     検索して動的にクラスの読み込みに成功したら、それを返す。検索に失敗したら nullを返す。
     * @throws CobolRuntimeException TODO: 準備中
     */
    public static CobolRunnable resolve(
            String packageName, AbstractCobolField cobolField, CobolRunnable runner)
            throws CobolRuntimeException {
        if (runner == null) {
            return resolve(packageName, cobolField.fieldToString());
        } else {
            return runner;
        }
    }

    /**
     * パッケージ名とクラス名から、クラスを動的に読み込む。 このメソッドは以下のように、環境変数の設定値によって動作が変わる。<br>
     * 環境変数COB_LOAD_CASEにLOWERが指定されているときは、nameの示す文字列全体を小文字に変換したものを検索する。<br>
     * 環境変数COB_LOAD_CASEにUPPERが指定されているときは、nameの示す文字列全体を小文字に変換したものを検索する。<br>
     * 環境変数COB_LIBRARY_PATHにディレクトリパスが指定されている場合、 このメソッドが検索するモジュールのディレクトリパスのリストにそれを追加する。<br>
     * 環境変数COB_PACKAGE_PATHにディレクトリパスが指定されている場合、 このメソッドが検索するモジュールのパッケージパスのリストにそれを追加する。
     *
     * @param packageName 検索するクラスのパッケージ名
     * @param name 読み込むクラスの名前
     * @param runner CobolRunnableを実装したクラス。nullでもよい。
     * @return runnerがnullでない場合はrunnerを返し、そうでないときはクラス名とパッケージ名を元に検索処理を実施する。
     *     検索して動的にクラスの読み込みに成功したら、それを返す。検索に失敗したら nullを返す。
     * @throws CobolRuntimeException TODO: 準備中
     */
    public static CobolRunnable resolve(String packageName, String name, CobolRunnable runner)
            throws CobolRuntimeException {
        if (runner == null) {
            return resolve(packageName, name);
        } else {
            return runner;
        }
    }

    /**
     * パッケージ名とクラス名から、クラスを動的に読み込む。 このメソッドは以下のように、環境変数の設定値によって動作が変わる。<br>
     * 環境変数COB_LOAD_CASEにLOWERが指定されているときは、cobolFieldの示す文字列全体を小文字に変換したものを検索する。<br>
     * 環境変数COB_LOAD_CASEにUPPERが指定されているときは、cobolFieldの示す文字列全体を小文字に変換したものを検索する。<br>
     * 環境変数COB_LIBRARY_PATHにディレクトリパスが指定されている場合、 このメソッドが検索するモジュールのディレクトリパスのリストにそれを追加する。<br>
     * 環境変数COB_PACKAGE_PATHにディレクトリパスが指定されている場合、 このメソッドが検索するモジュールのパッケージパスのリストにそれを追加する。
     *
     * @param packageName パッケージ名
     * @param cobolField 読み込むクラスの名前
     * @return クラス名とパッケージ名を元に検索処理を実施する。 検索して動的にクラスの読み込みに成功したら、それを返す。検索に失敗したら nullを返す。
     * @throws CobolRuntimeException TODO: 準備中
     */
    public static CobolRunnable resolve(String packageName, AbstractCobolField cobolField)
            throws CobolRuntimeException {
        return resolve(packageName, cobolField.fieldToString());
    }

    /**
     * パッケージ名とクラス名から、クラスを動的に読み込む。 このメソッドは以下のように、環境変数の設定値によって動作が変わる。<br>
     * 環境変数COB_LOAD_CASEにLOWERが指定されているときは、nameの示す文字列全体を小文字に変換したものを検索する。<br>
     * 環境変数COB_LOAD_CASEにUPPERが指定されているときは、nameの示す文字列全体を小文字に変換したものを検索する。<br>
     * 環境変数COB_LIBRARY_PATHにディレクトリパスが指定されている場合、 このメソッドが検索するモジュールのディレクトリパスのリストにそれを追加する。<br>
     * 環境変数COB_PACKAGE_PATHにディレクトリパスが指定されている場合、 このメソッドが検索するモジュールのパッケージパスのリストにそれを追加する。
     *
     * @param packageName パッケージ名
     * @param name 読み込むクラスの名前
     * @return クラス名とパッケージ名を元に検索処理を実施する。 検索して動的にクラスの読み込みに成功したら、それを返す。検索に失敗したら nullを返す。
     * @throws CobolRuntimeException TODO: 準備中
     */
    public static CobolRunnable resolve(String packageName, String name)
            throws CobolRuntimeException {
        String fullName;
        CobolRunnable runnable = null;

        /* encode program name */
        char c1 = name.charAt(0);
        if (c1 >= '0' && c1 <= '9') {
            name = "_" + name;
        }
        name = name.replaceAll("-", "__");

        /* search the cache */
        if (callTable.containsKey(name)) {
            return callTable.get(name);
        }

        if (name_convert == 1) {
            name = name.toLowerCase();
        } else if (name_convert == 2) {
            name = name.toUpperCase();
        }

        if (packageName != null) {
            fullName = packageName + "." + name;
        } else {
            fullName = name;
        }

        /* search the main program */
        runnable = getInstance(fullName);
        if (runnable != null) {
            callTable.put(name, runnable);
            return runnable;
        }

        /* search external modules */
        for (String packagePath : package_paths) {
            fullName = packagePath + "." + name;
            runnable = getInstance(fullName);
            if (runnable != null) {
                callTable.put(name, runnable);
                return runnable;
            }
        }

        // Not found
        String msg = "Program not found: " + name;
        throw new CobolRuntimeException(CobolExceptionId.COB_EC_PROGRAM_NOT_FOUND, msg);
    }

    /**
     * リフレクション機能を用いて動的にクラスファイルを読み込んでCobolRunnableのインスタンスを生成する
     *
     * @param name 読み込むクラスファイルのパス
     * @return 読み込みに失敗したときはnull,其れ以外は読み込んだCobolRunnbableのインスタンス
     */
    private static CobolRunnable getInstance(String name) {
        CobolRunnable runnable = null;

        try {
            Class<?> c = Class.forName(name);
            Constructor<?> cons = c.getConstructor();
            runnable = (CobolRunnable) cons.newInstance();
            runnable = (CobolRunnable) c.getDeclaredConstructor().newInstance();
            callTable.put(name, runnable);
            return runnable;
        } catch (NoSuchMethodException
                | SecurityException
                | InstantiationException
                | IllegalAccessException
                | IllegalArgumentException
                | InvocationTargetException
                | ClassNotFoundException e) {
            // TODO 自動生成された catch ブロック
            return null;
        }
    }

    /**
     * 引数で与えられたプログラム名に対応するCobolRunnableのインスタンスの cancelメソッドを呼び出す
     *
     * @param cobolField プログラム名を示すCOBOL変数
     */
    public static void cancel(AbstractCobolField cobolField) {
        cancel(cobolField.getString());
    }

    /**
     * 引数で与えられたプログラム名に対応するCobolRunnableのインスタンスの cancelメソッドを呼び出す
     *
     * @param name プログラム名
     */
    public static void cancel(String name) {
        if (name == null) {
            throw new CobolRuntimeException(
                    CobolRuntimeException.COBOL_FATAL_ERROR,
                    "NULL name parameter passed to 'cobcancel'");
        }

        CobolRunnable runnable = callTable.get(name);
        if (runnable.isActive() == false) {
            runnable.cancel();
        }
    }

    /**
     * 指定のプログラムのcancelメソッドを呼び出す
     *
     * @param f cancelを呼び出すプログラム名を示すCOBOL変数
     * @throws CobolStopRunException TODO: 準備中
     */
    public static void fieldCancel(AbstractCobolField f) throws CobolStopRunException {
        CobolResolve.cobCancel(f.fieldToString());
    }

    /**
     * 指定のプログラムのcancelメソッドを呼び出す
     *
     * @param name プログラム名
     * @throws CobolStopRunException TODO: 準備中
     */
    public static void cobCancel(String name) throws CobolStopRunException {
        if (name == null || name.equals("")) {
            // TODO cob_runtime_errorの実装
            CobolStopRunException.stopRunAndThrow(1);
        }

        name = name.replaceAll("-", "__");
        CobolRunnable runner = callTable.get(name);
        if (runner != null) {
            runner.cancel();
        }
    }
}
