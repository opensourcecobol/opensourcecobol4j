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

/**
 * opensource COBOL 4Jのランタイムライブラリ(libcobj)全体で共有する定数を保持するクラス
 */
public class Const {

    /**
     * opensource COBOL 4Jのバージョン番号<br>
     * {@code cobj-idx --version}などのバージョン表示に用いられる。リリース時には{@code update-version.sh}によって
     * {@code configure.ac}や{@code build.gradle.kts}などのバージョン番号とまとめて更新されるため、この値を単独で書き換えてはならない。
     */
    public static final String version = "2.1.0";
}
