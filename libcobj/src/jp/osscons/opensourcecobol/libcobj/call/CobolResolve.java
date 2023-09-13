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
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.UUID;
import jp.osscons.opensourcecobol.libcobj.common.CobolConstant;
import jp.osscons.opensourcecobol.libcobj.common.CobolUtil;
import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolExceptionId;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolRuntimeException;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolStopRunException;

/** ポインタ(UUID)やプログラム名とプログラムの実行を担うCobolRunnableのインスタンスを紐づけるクラス */
public class CobolResolve {

  /** プログラム名とCobolRunnableインスタンスの対応表 */
  private static Map<String, CobolRunnable> callTable;

  /** ポインタ(UUID)のCobolRunnableインスタンスの対応表 */
  private static Map<UUID, String> pointerTable;

  public static Map<String, String> cob_exception;

  /** 名前の変換方法(小文字か大文字)を示す変数 */
  private static int name_convert;

  // TODO resolve_pathsの利用方法
  /** システムで設定された区切り文字で区切られた0個以上のパス 動的に読み込むクラスファイルを検索する場所を示す. */
  private static List<String> resolve_paths;

  /** システムで設定された区切り文字で区切られた0個以上のパス 動的に読み込むクラスファイルを検索するパッケージ名を示す. */
  private static List<String> package_paths;

  static {
    callTable = new HashMap<>();
    pointerTable = new HashMap<>();
    name_convert = 0;
    resolve_paths = new ArrayList<String>();
    package_paths = new ArrayList<String>();
    cob_exception = new HashMap<>();

    cob_exception.put("FFFF", "EC-ALL");
    cob_exception.put("0100", "EC-ARGUMENT");
    cob_exception.put("0101", "EC-ARGUMENT-FUNCTION");
    cob_exception.put("0102", "EC-ARGUMENT-IMP");
    cob_exception.put("0200", "EC-BOUND");
    cob_exception.put("0201", "EC-BOUND-IMP");
    cob_exception.put("0202", "EC-BOUND-ODO");
    cob_exception.put("0203", "EC-BOUND-OVERFLOW");
    cob_exception.put("0204", "EC-BOUND-PTR");
    cob_exception.put("0205", "EC-BOUND-REF-MOD");
    cob_exception.put("0206", "EC-BOUND-SET");
    cob_exception.put("0207", "EC-BOUND-SUBSCRIPT");
    cob_exception.put("0208", "EC-BOUND-TABLE-LIMIT");
    cob_exception.put("0300", "EC-DATA");
    cob_exception.put("0301", "EC-DATA-CONVERSION");
    cob_exception.put("0302", "EC-DATA-IMP");
    cob_exception.put("0303", "EC-DATA-INCOMPATIBLE");
    cob_exception.put("0304", "EC-DATA-INTEGRITY");
    cob_exception.put("0305", "EC-DATA-PTR-NULL");
    cob_exception.put("0306", "EC-DATA-INFINITY");
    cob_exception.put("0307", "EC-DATA-NEGATIVE-INFINITY");
    cob_exception.put("0308", "EC-DATA-NOT_A_NUMBER");
    cob_exception.put("0400", "EC-FLOW");
    cob_exception.put("0401", "EC-FLOW-GLOBAL-EXIT");
    cob_exception.put("0402", "EC-FLOW-GLOBAL-GOBACK");
    cob_exception.put("0403", "EC-FLOW-IMP");
    cob_exception.put("0404", "EC-FLOW-RELEASE");
    cob_exception.put("0405", "EC-FLOW-REPORT");
    cob_exception.put("0406", "EC-FLOW-RETURN");
    cob_exception.put("0407", "EC-FLOW-SEARCH");
    cob_exception.put("0408", "EC-FLOW-USE");
    cob_exception.put("0500", "EC-I-O");
    cob_exception.put("0501", "EC-I-O-AT-END");
    cob_exception.put("0502", "EC-I-O-EOP");
    cob_exception.put("0503", "EC-I-O-EOP-OVERFLOW");
    cob_exception.put("0504", "EC-I-O-FILE-SHARING");
    cob_exception.put("0505", "EC-I-O-IMP");
    cob_exception.put("0506", "EC-I-O-INVALID-KEY");
    cob_exception.put("0507", "EC-I-O-LINAGE");
    cob_exception.put("0508", "EC-I-O-LOGIC-ERROR");
    cob_exception.put("0509", "EC-I-O-PERMANENT-ERROR");
    cob_exception.put("050A", "EC-I-O-RECORD-OPERATION");
    cob_exception.put("0600", "EC-IMP");
    cob_exception.put("0601", "EC-IMP-ACCEPT");
    cob_exception.put("0602", "EC-IMP-DISPLAY");
    cob_exception.put("0700", "EC-LOCALE");
    cob_exception.put("0701", "EC-LOCALE-IMP");
    cob_exception.put("0702", "EC-LOCALE-INCOMPATIBLE");
    cob_exception.put("0703", "EC-LOCALE-INVALID");
    cob_exception.put("0704", "EC-LOCALE-INVALID-PTR");
    cob_exception.put("0705", "EC-LOCALE-MISSING");
    cob_exception.put("0706", "EC-LOCALE-SIZE");
    cob_exception.put("0800", "EC-OO");
    cob_exception.put("0801", "EC-OO-CONFORMANCE");
    cob_exception.put("0802", "EC-OO-EXCEPTION");
    cob_exception.put("0803", "EC-OO-IMP");
    cob_exception.put("0804", "EC-OO-METHOD");
    cob_exception.put("0805", "EC-OO-NULL");
    cob_exception.put("0806", "EC-OO-RESOURCE");
    cob_exception.put("0807", "EC-OO-UNIVERSAL");
    cob_exception.put("0900", "EC-ORDER");
    cob_exception.put("0901", "EC-ORDER-IMP");
    cob_exception.put("0902", "EC-ORDER-NOT-SUPPORTED");
    cob_exception.put("0A00", "EC-OVERFLOW");
    cob_exception.put("0A01", "EC-OVERFLOW-IMP");
    cob_exception.put("0A02", "EC-OVERFLOW-STRING");
    cob_exception.put("0A03", "EC-OVERFLOW-UNSTRING");
    cob_exception.put("0B00", "EC-PROGRAM");
    cob_exception.put("0B01", "EC-PROGRAM-ARG-MISMATCH");
    cob_exception.put("0B02", "EC-PROGRAM-ARG-OMITTED");
    cob_exception.put("0B03", "EC-PROGRAM-CANCEL-ACTIVE");
    cob_exception.put("0B04", "EC-PROGRAM-IMP");
    cob_exception.put("0B05", "EC-PROGRAM-NOT-FOUND");
    cob_exception.put("0B06", "EC-PROGRAM-PTR-NULL");
    cob_exception.put("0B07", "EC-PROGRAM-RECURSIVE-CALL");
    cob_exception.put("0B08", "EC-PROGRAM-RESOURCES");
    cob_exception.put("0C00", "EC-RAISING");
    cob_exception.put("0C01", "EC-RAISING-IMP");
    cob_exception.put("0C02", "EC-RAISING-NOT-SPECIFIED");
    cob_exception.put("0D00", "EC-RANGE");
    cob_exception.put("0D01", "EC-RANGE-IMP");
    cob_exception.put("0D02", "EC-RANGE-INDEX");
    cob_exception.put("0D03", "EC-RANGE-INSPECT-SIZE");
    cob_exception.put("0D04", "EC-RANGE-INVALID");
    cob_exception.put("0D05", "EC-RANGE-PERFORM-VARYING");
    cob_exception.put("0D06", "EC-RANGE-PTR");
    cob_exception.put("0D07", "EC-RANGE-SEARCH-INDEX");
    cob_exception.put("0D08", "EC-RANGE-SEARCH-NO-MATCH");
    cob_exception.put("0E00", "EC-REPORT");
    cob_exception.put("0E01", "EC-REPORT-ACTIVE");
    cob_exception.put("0E02", "EC-REPORT-COLUMN-OVERLAP");
    cob_exception.put("0E03", "EC-REPORT-FILE-MODE");
    cob_exception.put("0E04", "EC-REPORT-IMP");
    cob_exception.put("0E05", "EC-REPORT-INACTIVE");
    cob_exception.put("0E06", "EC-REPORT-LINE-OVERLAP");
    cob_exception.put("0E08", "EC-REPORT-NOT-TERMINATED");
    cob_exception.put("0E09", "EC-REPORT-PAGE-LIMIT");
    cob_exception.put("0E0A", "EC-REPORT-PAGE-WIDTH");
    cob_exception.put("0E0B", "EC-REPORT-SUM-SIZE");
    cob_exception.put("0E0C", "EC-REPORT-VARYING");
    cob_exception.put("0F00", "EC-SCREEN");
    cob_exception.put("0F01", "EC-SCREEN-FIELD-OVERLAP");
    cob_exception.put("0F02", "EC-SCREEN-IMP");
    cob_exception.put("0F03", "EC-SCREEN-ITEM-TRUNCATED");
    cob_exception.put("0F04", "EC-SCREEN-LINE-NUMBER");
    cob_exception.put("0F05", "EC-SCREEN-STARTING-COLUMN");
    cob_exception.put("1000", "EC-SIZE");
    cob_exception.put("1001", "EC-SIZE-ADDRESS");
    cob_exception.put("1002", "EC-SIZE-EXPONENTIATION");
    cob_exception.put("1003", "EC-SIZE-IMP");
    cob_exception.put("1004", "EC-SIZE-OVERFLOW");
    cob_exception.put("1005", "EC-SIZE-TRUNCATION");
    cob_exception.put("1006", "EC-SIZE-UNDERFLOW");
    cob_exception.put("1007", "EC-SIZE-ZERO-DIVIDE");
    cob_exception.put("1100", "EC-SORT-MERGE");
    cob_exception.put("1101", "EC-SORT-MERGE-ACTIVE");
    cob_exception.put("1102", "EC-SORT-MERGE-FILE-OPEN");
    cob_exception.put("1103", "EC-SORT-MERGE-IMP");
    cob_exception.put("1104", "EC-SORT-MERGE-RELEASE");
    cob_exception.put("1105", "EC-SORT-MERGE-RETURN");
    cob_exception.put("1106", "EC-SORT-MERGE-SEQUENCE");
    cob_exception.put("1200", "EC-STORAGE");
    cob_exception.put("1201", "EC-STORAGE-IMP");
    cob_exception.put("1202", "EC-STORAGE-NOT-ALLOC");
    cob_exception.put("1203", "EC-STORAGE-NOT-AVAIL");
    cob_exception.put("1300", "EC-USER");
    cob_exception.put("1400", "EC-VALIDATE");
    cob_exception.put("1401", "EC-VALIDATE-CONTENT");
    cob_exception.put("1402", "EC-VALIDATE-FORMAT");
    cob_exception.put("1403", "EC-VALIDATE-IMP");
    cob_exception.put("1404", "EC-VALIDATE-RELATION");
    cob_exception.put("1405", "EC-VALIDATE-VARYING");
    cob_exception.put("1500", "EC-FUNCTION");
    cob_exception.put("1501", "EC-FUNCTION-NOT-FOUND");
    cob_exception.put("1502", "EC-FUNCTION-PTR-INVALID");
    cob_exception.put("1503", "EC-FUNCTION-PTR-NULL");
    cob_exception.put("1600", "EC-XML");
    cob_exception.put("1601", "EC-XML-CODESET");
    cob_exception.put("1602", "EC-XML-CODESET-CONVERSION");
    cob_exception.put("1603", "EC-XML-COUNT");
    cob_exception.put("1604", "EC-XML-DOCUMENT-TYPE");
    cob_exception.put("1605", "EC-XML-IMPLICIT-CLOSE");
    cob_exception.put("1606", "EC-XML-INVALID");
    cob_exception.put("1607", "EC-XML-NAMESPACE");
    cob_exception.put("1608", "EC-XML-STACKED-OPEN");
    cob_exception.put("1609", "EC-XML-RANGE");
  }

  /** 初期化を行う */
  public static void CobolInitCall() {

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

  public static CobolRunnable resolve(
      String packageName, AbstractCobolField cobolField, CobolRunnable runner)
      throws CobolRuntimeException {
    if (runner == null) {
      return resolve(packageName, cobolField.fieldToString());
    } else {
      return runner;
    }
  }

  public static CobolRunnable resolve(String packageName, String name, CobolRunnable runner)
      throws CobolRuntimeException {
    if (runner == null) {
      return resolve(packageName, name);
    } else {
      return runner;
    }
  }

  /**
   * プログラム名に対応するCobolRunnableインスタンスを返す
   *
   * @param name プログラム名の格納されたCOBOLの変数
   * @return nameに対応するCobolRunnableインスタンス
   * @throws CobolRuntimeException
   */
  public static CobolRunnable resolve(String packageName, AbstractCobolField cobolField)
      throws CobolRuntimeException {
    return resolve(packageName, cobolField.fieldToString());
  }

  /**
   * プログラム名に対応するCobolRunnableインスタンスを返す
   *
   * @param name プログラム名
   * @return nameに対応するCobolRunnableインスタンス
   * @throws CobolRuntimeException
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
   * @param name プログラム名を保持するCOBOLの変数
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
          CobolRuntimeException.COBOL_FITAL_ERROR, "NULL name parameter passed to 'cobcancel'");
    }

    CobolRunnable runnable = callTable.get(name);
    if (runnable.isActive() == false) {
      runnable.cancel();
    }
  }

  /** callTableに保存されているすべてのCallRunnableのインスタンスのcancelメソッドを呼び出す */
  public static void cancelAll() {
    for (CobolRunnable runnable : callTable.values()) {
      if (runnable.isActive() == false) {
        runnable.cancel();
      }
    }
  }

  /**
   * プログラム名に対応するCobolRunnableのインスタンスに対応するポインタ(UUID) をバイト配列として返す
   *
   * @param name プログラム名
   * @return プログラム名に対応するCobolRunnableのインスタンスに対応するポインタ(UUID)
   * @throws CobolRuntimeException
   */
  public static byte[] resolveToPointer(AbstractCobolField field) throws CobolRuntimeException {
    return resolveToPointer(field.getString());
  }

  /**
   * プログラム名に対応するCobolRunnableのインスタンスに対応するポインタ(UUID) をバイト配列として返す
   *
   * @param name プログラム名
   * @return プログラム名に対応するCobolRunnableのインスタンスに対応するポインタ(UUID)
   * @throws CobolRuntimeException
   */
  public static byte[] resolveToPointer(String name) throws CobolRuntimeException {
    Iterator<Entry<UUID, String>> i = pointerTable.entrySet().iterator();
    while (i.hasNext()) {
      Entry<UUID, String> e = i.next();
      if (e.getValue().equals(name)) {
        return uuidToByteBuffer(e.getKey());
      }
    }
    resolve(null, name); // TODO
    UUID uuid = UUID.randomUUID();
    pointerTable.put(uuid, name);
    return uuidToByteBuffer(uuid);
  }

  /**
   * ポインタ(UUID)に対応するCobolRunnableのインスタンスを返す
   *
   * @param b_10 ポインタ(UUID)が格納されたCobolDataStorageのインスタンス
   * @return ポインタ(UUID)に対応するCobolRunnableのインスタンス
   */
  public static CobolRunnable resolveFromPointer(CobolDataStorage d) {
    byte[] uuidBytes = new byte[Long.BYTES * 2];
    System.arraycopy(d.getData(), 0, uuidBytes, 0, uuidBytes.length);
    UUID uuid = uuidFromByteBuffer(uuidBytes);
    String name = pointerTable.get(uuid);
    try {
      return resolve(null, name); // TODO
    } catch (CobolRuntimeException e) {
      return null;
    }
  }

  // TODO 設置場所
  /**
   * UUIDをバイト配列に変換する
   *
   * @param uuid 変前のUUID
   * @return uuidから変換したバイト配列
   */
  public static byte[] uuidToByteBuffer(UUID uuid) {
    byte[] uuidBytes = new byte[16];

    byte[] hi = ByteBuffer.allocate(Long.BYTES).putLong(uuid.getMostSignificantBits()).array();
    byte[] low = ByteBuffer.allocate(Long.BYTES).putLong(uuid.getLeastSignificantBits()).array();

    System.arraycopy(hi, 0, uuidBytes, 0, hi.length);
    System.arraycopy(low, 0, uuidBytes, hi.length, low.length);

    return uuidBytes;
  }

  // TODO 設置場所
  /**
   * バイト配列に保存されたポインタ(UUID)を取り出す
   *
   * @param bytes ポインタ(UUID)が保存されているバイト配列
   * @return bytesからと取り出したポインタ(UUID)
   */
  public static UUID uuidFromByteBuffer(byte[] bytes) {
    long l1 = ByteBuffer.wrap(bytes, 0, Long.BYTES).getLong();
    long l2 = ByteBuffer.wrap(bytes, Long.BYTES, Long.BYTES).getLong();

    return new UUID(l1, l2);
  }

  public static void fieldCancel(AbstractCobolField f) throws CobolStopRunException {
    CobolResolve.cobCancel(f.fieldToString());
  }

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
