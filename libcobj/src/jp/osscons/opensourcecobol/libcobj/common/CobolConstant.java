/*
 * Copyright (C) 2022-2022 TOKYO SYSTEM HOUSE Co., Ltd.
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
package jp.osscons.opensourcecobol.libcobj.common;

import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldAttribute;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldFactory;

public class CobolConstant {		
	public static final CobolFieldAttribute allAttr = new CobolFieldAttribute(
			CobolFieldAttribute.COB_TYPE_ALPHANUMERIC_ALL, 0, 0, 0, null);
	public static final CobolFieldAttribute oneAttr = new CobolFieldAttribute(
			CobolFieldAttribute.COB_TYPE_NUMERIC, 1, 0, 0, null);
		
	public static final byte[] SJZERO = {(byte)0x82, (byte)0x4f};
	public static final byte[] SJSPC  = {(byte)0x81, (byte)0x40};
	public static final byte[] SJBLK  = {(byte)0x81, (byte)0x40};
	public static final byte[] SJQUOT = {(byte)0x81, (byte)0x68};
	public static final byte[] SJSLAS = {(byte)0x81, (byte)0x5e};
	public static final int    SJCSIZ = 2;
	
	public static final byte[] ZENZERO = SJZERO;
	public static final byte[] ZENSPC  = SJSPC;
	public static final byte[] ZENBLK  = SJBLK;
	public static final byte[] ZENQUOT = SJQUOT;
	public static final byte[] ZENSLAS = SJSLAS;
	public static final int    ZENCSIZ = SJCSIZ;
	
	public static final AbstractCobolField zero  = CobolFieldFactory.makeCobolField(1, "0", allAttr);
	public static final AbstractCobolField space = CobolFieldFactory.makeCobolField(1, " ", allAttr);
	public static final AbstractCobolField blank = CobolFieldFactory.makeCobolField(1, " ", allAttr);
	
	public static final AbstractCobolField high  = CobolFieldFactory.makeCobolField(1, CobolConstant.get0xFFStorage(), allAttr);
	
	public static final AbstractCobolField low   = CobolFieldFactory.makeCobolField(1, "\0", allAttr);
	public static final AbstractCobolField quote = CobolFieldFactory.makeCobolField(1, "\"", allAttr);
	public static final AbstractCobolField one   = CobolFieldFactory.makeCobolField(1, "1", oneAttr);
	
	public static final AbstractCobolField zenZero  = CobolFieldFactory.makeCobolField(ZENCSIZ, new CobolDataStorage(ZENZERO), allAttr);
	public static final AbstractCobolField zenSpace = CobolFieldFactory.makeCobolField(ZENCSIZ, new CobolDataStorage(ZENSPC), allAttr);
	public static final AbstractCobolField zenBlank = CobolFieldFactory.makeCobolField(ZENCSIZ, new CobolDataStorage(ZENBLK), allAttr);
	public static final AbstractCobolField zenQuote = CobolFieldFactory.makeCobolField(ZENCSIZ, new CobolDataStorage(ZENQUOT), allAttr);
	
	private static CobolDataStorage get0xFFStorage() {
		byte[] bytes = new byte[1];
		bytes[0] = (byte)0xff;
		return new CobolDataStorage(bytes);
	}
	
	public static final long[] exp10LL = {
		1L,
		10L,
		100L,
		1000L,
		10000L,
		100000L,
		1000000L,
		10000000L,
		100000000L,
		1000000000L,
		10000000000L,
		100000000000L,
		1000000000000L,
		10000000000000L,
		100000000000000L,
		1000000000000000L,
		10000000000000000L,
		100000000000000000L,
		1000000000000000000L
	};
	
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
	//TODO 標準パスの設定
	public static final String COB_LIBRARY_PATH = "";
}
