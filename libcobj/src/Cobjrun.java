/*
 * Copyright (C) 2020 TOKYO SYSTEM HOUSE Co., Ltd.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, 51 Franklin Street, Fifth Floor
 * Boston, MA 02110-1301 USA
 */

import jp.osscons.opensourcecobol.libcobj.call.CobolResolve;
import jp.osscons.opensourcecobol.libcobj.call.CobolRunnable;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolCallException;

/**
 * コマンドライン引数で与えられた名前に対応するプログラムを動的に読み込んで実行する
 */
public class Cobjrun {

	public static void main(String[] args) {
		if(args.length <= 0) {
			return;
		}
		CobolRunnable runnable;
		try {
			runnable = CobolResolve.resolve(args[0]);
			runnable.run();
		} catch (CobolCallException e) {
			e.printStackTrace();
		}
	}
}
