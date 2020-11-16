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
		
	public static final byte[] SJZERO = {(byte)0x81, (byte)0x4f};
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
	public static final AbstractCobolField high  = CobolFieldFactory.makeCobolField(1, "¥xff", allAttr);
	public static final AbstractCobolField low   = CobolFieldFactory.makeCobolField(1, "¥0", allAttr);
	public static final AbstractCobolField quote = CobolFieldFactory.makeCobolField(1, "\"", allAttr);
	public static final AbstractCobolField one   = CobolFieldFactory.makeCobolField(1, "1", oneAttr);
	
	public static final AbstractCobolField zenZero  = CobolFieldFactory.makeCobolField(ZENCSIZ, new CobolDataStorage(ZENZERO), allAttr);
	public static final AbstractCobolField zenSpace = CobolFieldFactory.makeCobolField(ZENCSIZ, new CobolDataStorage(ZENSPC), allAttr);
	public static final AbstractCobolField zenBlank = CobolFieldFactory.makeCobolField(ZENCSIZ, new CobolDataStorage(ZENBLK), allAttr);
	public static final AbstractCobolField zenQuote = CobolFieldFactory.makeCobolField(ZENCSIZ, new CobolDataStorage(ZENQUOT), allAttr);
}
