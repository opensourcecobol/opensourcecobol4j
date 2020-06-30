package test;

import java.io.UnsupportedEncodingException;

import jp.osscons.opensourcecobol.libcobj.data.CobolAlphanumericField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldAttribute;
import jp.osscons.opensourcecobol.libcobj.data.CobolNationalField;
import jp.osscons.opensourcecobol.libcobj.data.CobolNumericField;
import jp.osscons.opensourcecobol.libcobj.data.CobolNumericPackedField;
import jp.osscons.opensourcecobol.libcobj.termio.CobolTerminal;

public class MoveAlphanumericToNational {

	public static void testN2N(String str, int srcSize, int dstSize) {
		CobolDataStorage b_src = new CobolDataStorage(new byte[srcSize]);
		CobolDataStorage b_dst = new CobolDataStorage(new byte[dstSize]);

		CobolFieldAttribute a = new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_NATIONAL, 0, 0, 0, null);

		CobolNationalField f_src = new CobolNationalField(srcSize, b_src, a);
		CobolNationalField f_dst = new CobolNationalField(dstSize, b_dst, a);

		f_src.moveFrom(str);

		f_dst.moveFrom(f_src);
		CobolTerminal.display (true, true, f_dst);
	}

	public static void testA2N(String str, int srcSize, int dstSize) {
		CobolDataStorage b_src = new CobolDataStorage(new byte[srcSize]);
		CobolDataStorage b_dst = new CobolDataStorage(new byte[dstSize]);

		CobolFieldAttribute a_a = new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null);
		CobolFieldAttribute a_n = new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_NATIONAL, 0, 0, 0, null);

		CobolAlphanumericField f_src = new CobolAlphanumericField(srcSize, b_src, a_a);
		CobolNationalField f_dst = new CobolNationalField(dstSize, b_dst, a_n);

		f_src.moveFrom(str);

		f_dst.moveFrom(f_src);
		CobolTerminal.display (true, true, f_dst);
	}

	public static void testD2N(int num, int srcSize, int dstSize) {
		CobolDataStorage b_src = new CobolDataStorage(new byte[srcSize]);
		CobolDataStorage b_dst = new CobolDataStorage(new byte[dstSize]);

		CobolFieldAttribute a_nd = new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY, srcSize, 0, 0, null);
		CobolFieldAttribute a_n = new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_NATIONAL, 0, 0, 0, null);

		CobolNumericField f_src = new CobolNumericField(srcSize, b_src, a_nd);
		CobolNationalField f_dst = new CobolNationalField(dstSize, b_dst, a_n);

		f_src.moveFrom(num);

		f_dst.moveFrom(f_src);
		CobolTerminal.display (true, true, f_dst);
	}

	public static void testPD2N(int num, int srcSize, int srcDigits, int dstSize) {
		CobolDataStorage b_src = new CobolDataStorage(new byte[srcSize]);
		CobolDataStorage b_dst = new CobolDataStorage(new byte[dstSize]);

		CobolFieldAttribute a_nd = new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_NUMERIC_PACKED, srcDigits, 0, 0, null);
		CobolFieldAttribute a_n = new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_NATIONAL, 0, 0, 0, null);

		CobolNumericPackedField f_src = new CobolNumericPackedField(srcSize, b_src, a_nd);
		CobolNationalField f_dst = new CobolNationalField(dstSize, b_dst, a_n);

		f_src.moveFrom(num);

		f_dst.moveFrom(f_src);
		CobolTerminal.display (true, true, f_dst);
	}

	public static void main(String[] args) {
		try {
			CobolTerminal.openWriter();
		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
		}
		testN2N("あいうえお", 10, 10);
		testN2N("あいう", 10, 10);
		testN2N("あいうえおかき", 10, 10);
		testN2N("あいうえお", 10, 6);
		testN2N("あいう", 10, 6);
		testN2N("あいうえおかき", 10, 6);
		testN2N("あいうえお", 10, 20);
		testN2N("あいう", 10, 20);
		testN2N("あいうえおかき", 10, 20);


		testA2N("abcde", 5, 10);
		testA2N("abcdefg", 5, 10);
		testA2N("abc", 5, 10);
		testA2N("abcde", 10, 10);
		testA2N("abcdefg", 10, 10);
		testA2N("abc", 10, 10);
		testA2N("abcde", 10, 4);
		testA2N("abcdefg", 10, 4);
		testA2N("abc", 10, 4);


		testD2N(12345, 5, 10);
		testD2N(1234567, 5, 10);
		testD2N(123, 5, 10);
		testD2N(12345, 10, 10);
		testD2N(1234567, 10, 10);
		testD2N(123, 10, 10);
		testD2N(12345, 10, 4);
		testD2N(1234567, 10, 4);
		testD2N(123, 10, 4);

		testPD2N(12345, 3, 5, 10);
		testPD2N(1234567, 3, 5, 10);
		testPD2N(123, 3, 5, 10);
		testPD2N(12345, 6, 10, 10);
		testPD2N(1234567, 6, 10, 10);
		testPD2N(123, 6, 10, 10);
		testPD2N(12345, 6, 10, 4);
		testPD2N(1234567, 6, 10, 4);
		testPD2N(123, 6, 10, 4);

		CobolTerminal.closeWriter();
	}
}
