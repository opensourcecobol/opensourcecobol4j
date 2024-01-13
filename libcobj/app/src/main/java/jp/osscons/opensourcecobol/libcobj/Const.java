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

package jp.osscons.opensourcecobol.libcobj;

public class Const {

  public static final int COB_MINI_BUFF = 2056;
  public static final int COB_SMALL_BUFF = 1024;
  public static final int COB_NORMAL_BUFF = 2048;
  public static final int COB_MEDIUM_BUFF = 8192;
  public static final int COB_LARGE_BUFF = 16384;

  public static final int COB_MINI_MAX = COB_MINI_BUFF - 1;
  public static final int COB_SMALL_MAX = COB_SMALL_BUFF - 1;
  public static final int COB_NORMAL_MAX = COB_NORMAL_BUFF - 1;
  public static final int COB_MEDIUM_MAX = COB_MEDIUM_BUFF - 1;
  public static final int COB_LARGE_MAX = COB_LARGE_BUFF - 1;

  public static final int COB_MAX_FIELD_PARAMS = 64;
  public static final int COB_FERROR_INITIALIZED = 0;
  public static final String COB_SOURCE_FILE = null;
  public static final int COB_PACKAGE_VERSION = 0;
  public static final int COB_PATCH_LEVEL = 0;
  // TODO 標準パスの設定
  public static final String COB_LIBRARY_PATH = "";

  public static final String version = "1.0.18";
}
