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
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolCallException;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolRuntimeException;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolStopRunException;

/** ポインタ(UUID)やプログラム名とプログラムの実行を担うCobolRunnableのインスタンスを紐づけるクラス */
public class CobolResolve {

  /** プログラム名とCobolRunnableインスタンスの対応表 */
  private static Map<String, CobolRunnable> callTable;
  /** ポインタ(UUID)のCobolRunnableインスタンスの対応表 */
  private static Map<UUID, String> pointerTable;
  /** 名前の変換方法(小文字か大文字)を示す変数 */
  private static int name_convert;
  /** デフォルトのパッケージ名 */
  private static String defaultPackageName;
  // TODO resolve_pathsの利用方法
  /** システムで設定された区切り文字で区切られた0個以上のパス 動的に読み込むクラスファイルを検索する場所を示す. */
  private static List<String> resolve_paths;
  /** システムで設定された区切り文字で区切られた0個以上のパス 動的に読み込むクラスファイルを検索するパッケージ名を示す. */
  private static List<String> package_paths;

  static {
    callTable = new HashMap<>();
    pointerTable = new HashMap<>();
    name_convert = 0;
    defaultPackageName = null;
    resolve_paths = new ArrayList<String>();
    package_paths = new ArrayList<String>();
  }

  /** 初期化を行う */
  public static void CobolInitCall() {

    String s;
    String buf;

    //		//用途不明
    //		call_filename_buff = cob_malloc (CALL_FILEBUFF_SIZE);
    //		call_entry_buff = cob_malloc (COB_SMALL_BUFF);
    //		call_entry2_buff = cob_malloc (COB_SMALL_BUFF);

    s = CobolUtil.getEnv("COB_LOAD_CASE");
    if (s != null) {
      String sU = s.toUpperCase();
      if (sU.equals("LOWER")) {
        name_convert = 1;
      } else if (sU.equals("UPPER")) {
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
    //		call_buffer = cob_malloc (CALL_BUFF_SIZE);
    //		call_lastsize = CALL_BUFF_SIZE;
    //		for (psyst = (struct system_table *)&system_tab[0]; psyst->syst_name; ++psyst) {
    //			insert (psyst->syst_name, psyst->syst_call, NULL);
    //		}
  }

  /**
   * ライブラリパスを更新する
   *
   * @param path 区切り文字で区切られた0個以上のパスを示す文字列
   */
  private static void setLibraryPath(String path) {
    if (resolve_paths.size() > 0) {
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
    if (package_paths.size() > 0) {
      package_paths.clear();
    }

    String[] paths = path.split(System.getProperty("path.separator"));

    for (String path1 : paths) {
      if (path1.length() > 0) {
        package_paths.add(path1);
      }
    }
  }

  public static void setCancel(String name, CobolRunnable runnable) {
    if (callTable.containsKey(name)) {
      // TODO 重複時の動作
    }
    callTable.put(name, runnable);
  }

  /**
   * プログラム名に対応するCobolRunnableインスタンスを返す
   *
   * @param name プログラム名の格納されたCOBOLの変数
   * @return nameに対応するCobolRunnableインスタンス
   * @throws CobolCallException
   */
  public static CobolRunnable resolve(AbstractCobolField cobolField) throws CobolCallException {
    return resolve(cobolField.fieldToString());
  }

  /**
   * プログラム名に対応するCobolRunnableインスタンスを返す
   *
   * @param name プログラム名
   * @return nameに対応するCobolRunnableインスタンス
   * @throws CobolCallException
   */
  public static CobolRunnable resolve(String name) throws CobolCallException {
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

    if (defaultPackageName != null) {
      fullName = defaultPackageName + "." + name;
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
    for (String package_path : package_paths) {
      fullName = package_path + "." + name;
      runnable = getInstance(fullName);
      if (runnable != null) {
        callTable.put(name, runnable);
        return runnable;
      }
    }

    // Not found
    throw new CobolCallException();
  }

  /**
   * libcob/call.cのcob_resolve_1の実装
   *
   * @param name
   * @return
   */
  public static CobolRunnable resolve1(String name) {
    CobolRunnable p = null;
    try {
      p = resolve(name);
    } catch (CobolCallException e) {
      // TODO cob_call_error()を書く
    }
    return p;
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
      try {
        Constructor<?> cons = c.getConstructor();
        runnable = (CobolRunnable) cons.newInstance();
        runnable = (CobolRunnable) c.newInstance();
        callTable.put(name, runnable);
        return runnable;
      } catch (NoSuchMethodException
          | SecurityException
          | InstantiationException
          | IllegalAccessException
          | IllegalArgumentException
          | InvocationTargetException e) {
        // TODO 自動生成された catch ブロック
        e.printStackTrace();
        throw new CobolRuntimeException(CobolRuntimeException.COBOL_FITAL_ERROR, "型エラー");
      }
    } catch (ClassNotFoundException e) {
      // TODO
      // e.printStackTrace();
      // Continue
    }

    return null;
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
   * @throws CobolCallException
   */
  public static byte[] resolveToPointer(AbstractCobolField field) throws CobolCallException {
    return resolveToPointer(field.getString());
  }

  /**
   * プログラム名に対応するCobolRunnableのインスタンスに対応するポインタ(UUID) をバイト配列として返す
   *
   * @param name プログラム名
   * @return プログラム名に対応するCobolRunnableのインスタンスに対応するポインタ(UUID)
   * @throws CobolCallException
   */
  public static byte[] resolveToPointer(String name) throws CobolCallException {
    Iterator<Entry<UUID, String>> i = pointerTable.entrySet().iterator();
    while (i.hasNext()) {
      Entry<UUID, String> e = i.next();
      if (e.getValue().equals(name)) {
        return uuidToByteBuffer(e.getKey());
      }
    }
    CobolRunnable runnable = resolve(name);
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
  public static CobolRunnable resolveFromPointer(CobolDataStorage b_10) {
    byte[] uuidBytes = new byte[Long.BYTES * 2];
    System.arraycopy(b_10.getData(), 0, uuidBytes, 0, uuidBytes.length);
    UUID uuid = uuidFromByteBuffer(uuidBytes);
    String name = pointerTable.get(uuid);
    try {
      return resolve(name);
    } catch (CobolCallException e) {
      // TODO 自動生成された catch ブロック
      e.printStackTrace();
      throw new CobolRuntimeException(
          CobolRuntimeException.COBOL_FITAL_ERROR, "Program Pointer adress Error'");
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

    CobolRunnable runner = callTable.get(name);
    if (runner != null) {
      runner.cancel();
    }
  }
}
