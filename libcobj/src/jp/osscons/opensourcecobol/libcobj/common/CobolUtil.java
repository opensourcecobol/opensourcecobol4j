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

import java.time.DateTimeException;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.Scanner;
import java.util.regex.MatchResult;

import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolException;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolExceptionId;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolRuntimeException;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolStopRunException;
import jp.osscons.opensourcecobol.libcobj.file.CobolFile;

public class CobolUtil {
	private static int cob_io_assume_rewrite = 0;
	private static boolean cob_verbose = false;
	private static handlerlist hdlrs = null;
	private static String cob_source_file = null;
	private static int cob_source_line = 0;
	private static String runtime_err_str = null;	
	
	public static LocalDateTime cobLocalTm = null;
	public static String cobLocalEnv = null;
	
	public static String[] commandLineArgs = null;
	public static int currentArgIndex = 1;
	
	public static boolean nibbleCForUnsigned = false;
	
	public static int commlncnt = 0;
	public static byte[] commlnptr = null;
	
	public static boolean[] cobSwitch = new boolean[8];
	public static int cobSaveCallParams = 0;
	
	public static boolean verbose = false;

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
			CobolUtil.commandLineArgs = argv;
			CobolFile.cob_init_fileio();
			
			for(int i=0; i<8; ++i) {
				String envVariableName = String.format("COB_SWITCH_%d", i + 1);
				String envValue = System.getenv(envVariableName);
				if(envValue == null) {
					CobolUtil.cobSwitch[i] = false;
				} else  {
					CobolUtil.cobSwitch[i] = envValue.equals("ON");
				}
			}
		}

		String s;
		
		s = System.getenv("COB_DATE");
		if(s != null) {
			Scanner scan = new Scanner(s);
			scan.findInLine("(\\d+)/(\\d+)/(\\d+)");
			MatchResult result = scan.match();
			date_time_block: if(result.groupCount() == 3) {
				System.err.println("Warning: COB_DATE format invalid, ignored.");
			} else  {
				int year = Integer.parseInt(result.group(1));
				int month = Integer.parseInt(result.group(2));
				int dayOfMonth = Integer.parseInt(result.group(3));
				LocalDateTime tm;
				try {
					tm = LocalDateTime.of(year, month, dayOfMonth, 0, 0);
				} catch (DateTimeException e) {
					break date_time_block;
				}
				cobLocalTm = tm;
			}
		}
	}

	/**
	 * libcob/common.cとcob_localtime
	 * @return
	 */
	public static LocalDateTime localtime() {
		LocalDateTime rt = LocalDateTime.now();
		if(CobolUtil.cobLocalTm != null) {
			CobolUtil.cobLocalTm = CobolUtil.cobLocalTm
					.withHour(rt.getHour())
					.withMinute(rt.getMinute())
					.withSecond(rt.getSecond());
			rt = CobolUtil.cobLocalTm;
		}
		return rt;
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

	/**
	 * libcob/common.c cob_get_environment
	 * @param envname
	 * @param envval
	 */
	public static void getEnvironment(AbstractCobolField envname, AbstractCobolField envval) {
		String p = System.getenv(envname.fieldToString());
		if(p == null) {
			CobolException.setException(CobolExceptionId.COB_EC_IMP_ACCEPT);
			p = " ";
		}
		envval.memcpy(p);
	}

	/**
	 * libcob/common.cのCOB_CHK_PARMSの実装
	 * @param funcName
	 * @param numParams
	 * @throws CobolStopRunException
	 */
	public static void COB_CHK_PARMS(String funcName, int numParams) throws CobolStopRunException {
		//TODO ifの条件式の改修
		if(false) {
			String message = String.format("CALL to %s requires %d parameters", funcName, numParams);
			CobolRuntimeException.displayRuntimeError(message);
			CobolStopRunException.stopRunAndThrow(1);
		}
	}

	/**
	 * libcob/common.cのcob_get_switchの実装
	 * @param n
	 * @return
	 */
	public static boolean getSwitch(int n) {
		return CobolUtil.cobSwitch[n];
	}

	/**
	 * libcob/common.cのcob_set_switchの実装
	 * @param n
	 * @param flag
	 */
	public static void setSwitch(int n, int flag) {
		if(flag == 0) {
			CobolUtil.cobSwitch[n] = false;
		} else if(flag == 1) {
			CobolUtil.cobSwitch[n] = true;
		}
	}
}
