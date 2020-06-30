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

package jp.osscons.opensourcecobol.libcobj.common;

import jp.osscons.opensourcecobol.libcobj.file.CobolFile;

public class CobolUtil {
	private static int cob_io_assume_rewrite = 0;
	private static boolean cob_verbose = false;
	private static handlerlist hdlrs = null;
	private static String cob_source_file = null;
	private static int cob_source_line = 0;
	private static String runtime_err_str = null;

	abstract class handlerlist {
		public handlerlist next = null;
		abstract public int proc(String s);
	}

	/**
	 * libcob/common.cのcob_check_envの実装
	 * @param name
	 * @param value
	 * @return
	 */
	public static int checkEnv(String name, String value) {
		if(name == null || value == null) {
			return 0;
		}

		String s = System.getenv(name);
		if(s != null) {
			if(s.contentEquals(value)) {
				return 1;
			}
		}
		return 0;
	}

	public static int cob_io_rewwrite_assumed() {
		return cob_io_assume_rewrite;
	}

	/**
	 * libcob/common.cのcob_initの実装
	 * TODO 未完成
	 */
	public static void cob_init(String[] argv, boolean cob_initialized) {
		if(!cob_initialized) {
			CobolFile.cob_init_fileio();
		}
	}

	/**
	 * libcob/cob_verbose_outputの実装
	 * opensourceCOBOLではprintfのように可変長引数を取るが,
	 * こちらは呼び出し側で事前にString.format等を使用することを期待している.
	 * @param s
	 * TODO cob_verboseの初期化
	 */
	public static void verboseOutput(String s) {
		if(cob_verbose) {
			System.out.println("libcob: " + s);
		}
	}

	/**
	 * libcob/fileio.cのcob_rintime_errorの実装
	 * opensourceCOBOLではprintfのように可変長引数を取るが,
	 * こちらは呼び出し側で事前にString.format等を使用することを期待している.
	 * @param s
	 */
	public static void runtimeError(String s) {
		if(hdlrs != null) {
			handlerlist h = hdlrs;
			if(runtime_err_str != null) {
				String p = runtime_err_str;
				if(cob_source_file != null) {
					runtime_err_str = String.format("%s:%d: ", cob_source_file, cob_source_line);
				}
				p += s;
			}
			while(h != null) {
				if(runtime_err_str != null) {
					h.proc(runtime_err_str);
				} else {
					h.proc("Malloc error");
				}
				h = h.next;
			}
			hdlrs = null;
		}

		if(cob_source_file != null) {
			System.err.print(String.format("%s:%d: ", cob_source_file, cob_source_line));
		}
		System.err.println("libcob: " + s);
		System.err.flush();
	}
}
