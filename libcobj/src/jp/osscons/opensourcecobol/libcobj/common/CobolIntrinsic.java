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
package jp.osscons.opensourcecobol.libcobj.common;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Random;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Map;
import java.text.ParseException;

import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.data.CobolDecimal;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldAttribute;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldFactory;
import jp.osscons.opensourcecobol.libcobj.data.CobolNationalField;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolExceptionId;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolRuntimeException;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolStopRunException;
import jp.osscons.opensourcecobol.libcobj.common.CobolUtil;

public class CobolIntrinsic {

	private static int[] normalDays = {0,31,59,90,120,151,181,212,243,273,304,334,365};
	private static int[] leapDays = {0,31,60,91,121,152,182,213,244,274,305,335,366};
	private static int[] normalMonthDays = {0,31,28,31,30,31,30,31,31,30,31,30,31};
	private static int[] leapMonthDays = {0,31,29,31,30,31,30,31,31,30,31,30,31};
	private final static int DEPTH_LEVEL = 8;
	private final static int sizeOfDouble = 8;
	private static int currEntry = 0;
	private static AbstractCobolField currField = null;
	private static CobolFieldAttribute currAttr = null;
	private static AbstractCobolField[] calcField = new AbstractCobolField[DEPTH_LEVEL];
	private static CobolFieldAttribute[] calcAttr = new CobolFieldAttribute[DEPTH_LEVEL];
	private static Random random = new Random();

	/**
	 * libcob/intrinsicのmake_double_entryの実装
	 */
	private static void makeDoubleEntry() {
		CobolDataStorage s = new CobolDataStorage(sizeOfDouble + 1);

		CobolFieldAttribute newAttr = new CobolFieldAttribute(
			CobolFieldAttribute.COB_TYPE_NUMERIC_DOUBLE,
			18,
			9,
			CobolFieldAttribute.COB_FLAG_HAVE_SIGN,
			null);
		AbstractCobolField newField = CobolFieldFactory.makeCobolField(sizeOfDouble, s, newAttr);

		calcAttr[currEntry] = newAttr;
		calcField[currEntry] = newField;
		currAttr = newAttr;
		currField = newField;
		if(++currEntry >= DEPTH_LEVEL) {
			currEntry = 0;
		}
	}

	/**
	 * libcob/intrinsicのmake_field_entryの実装
	 */
	private static void makeFieldEntry(AbstractCobolField f) {
		AbstractCobolField newField = CobolFieldFactory.makeCobolField(
			f.getSize(), new CobolDataStorage(f.getSize() + 1), f.getAttribute());
		calcField[currEntry] = newField;
		calcAttr[currEntry] = f.getAttribute();
		currField = calcField[currEntry];
		currAttr = calcAttr[currEntry];

		if(++currEntry >= DEPTH_LEVEL) {
			currEntry = 0;
		}
	}

	/**
	 * libcob/intrinsicのcob_intr_ordの実装
	 * @param year
	 * @return
	 */
	private static boolean isLeapYear(int year) {
		return ((year % 4 == 0 && year % 100 != 0) || (year % 400 == 0));
	}

	/**
	 * libcob/intrinsicのcob_init_intrinsicの実装
	 */
	public static void init() {
		CobolFieldAttribute attr = new CobolFieldAttribute(
			CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null);
		for(int i=0; i<DEPTH_LEVEL; ++i) {
			calcField[i] = CobolFieldFactory.makeCobolField(256, new CobolDataStorage(256), attr);
		}
	}

	/**
	 * libcob/intrinsicのcob_intr_get_doubleの実装
	 */
	private static double intrGetDouble(CobolDecimal d) {
		double v = d.getValue().doubleValue();
		int n = d.getScale();
		for(int i=0; i<Math.abs(n); ++i) {
			if(n > 0) {
				v /= 10;
			} else {
				v *= 10;
			}
		}
		return v;
	}

	/**
	 * libcob/intrinsicのcalc_ref_modの実装
	 * @param f
	 * @param offset
	 * @param length
	 */
	private static void calcRefMod(AbstractCobolField f, int offset, int length) {
		if(offset <= f.getSize()) {
			int calcoff = offset - 1;
			int size = f.getSize() - calcoff;
			if(length > 0 && length < size) {
				size = length;
			}
			f.setSize(size);
			if(calcoff > 0) {
				CobolDataStorage tmp = new CobolDataStorage(size);
				tmp.memcpy(f.getDataStorage().getSubDataStorage(calcoff), size);
				f.getDataStorage().memcpy(tmp, size);
			}
		}
	}

	/**
	 * libcob/intrinsicのcob_intr_get_binopの実装
	 * @param f1
	 * @param op
	 * @param f2
	 * @return
	 * @throws CobolStopRunException
	 */
	public static AbstractCobolField intrBinop(AbstractCobolField f1, int op, AbstractCobolField f2) throws CobolStopRunException {
		CobolDecimal d1 = new CobolDecimal();
		CobolDecimal d2 = new CobolDecimal();
		d1.setField(f1);
		d2.setField(f2);

		switch((char)op) {
		case '+': d1.add(d2); break;
		case '-': d1.sub(d2); break;
		case '*': d1.mul(d2); break;
		case '/': d1.div(d2); break;
		case '^': d1.pow(d2); break;
		default: break;
		}

		int attrsign = 0;
		int sign = 0;
		if(d1.getValue().signum() < 0) {
			attrsign = CobolFieldAttribute.COB_FLAG_HAVE_SIGN;
			sign = 1;
		} else {
			attrsign = 0;
			sign = 0;
		}

		int size = sizeInBase10(d1.getValue());
		if(d1.getScale() > size) {
			size = d1.getScale();
		}
		CobolFieldAttribute attr = new CobolFieldAttribute(
			CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY, size, d1.getScale(), attrsign, null);
		AbstractCobolField field = CobolFieldFactory.makeCobolField(size, (CobolDataStorage)null, attr);
		makeFieldEntry(field);
		d1.getDisplayField(currField, 0);
		return currField;
	}

	/**
	 * libcob/intrinsicのcob_intr_lengthの実装
	 * @param srcfield
	 * @return
	 */
	public static AbstractCobolField funcLength(AbstractCobolField srcfield) {
		CobolFieldAttribute attr = new CobolFieldAttribute(
				CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY, 8, 0, 0, null);
		AbstractCobolField field = CobolFieldFactory.makeCobolField(4, (CobolDataStorage)null, attr);
		makeFieldEntry(field);
		currField.setInt(srcfield.getSize());
		return currField;
	}

	/**
	 * libcob/intrinsicのcob_intr_integerの実装
	 * @param srcfield
	 * @return
	 */
	public static AbstractCobolField funcInteger(AbstractCobolField srcfield) {
		CobolFieldAttribute attr = new CobolFieldAttribute(
				CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY, 18, 0, CobolFieldAttribute.COB_FLAG_HAVE_SIGN, null);
		AbstractCobolField field = CobolFieldFactory.makeCobolField(8, (CobolDataStorage)null, attr);
		makeFieldEntry(field);

		CobolDecimal d1 = new CobolDecimal();
		d1.setField(srcfield);
		if(d1.getValue().signum() >= 0) {
			try {d1.getField(currField, 0);} catch (CobolStopRunException e) {}
			return currField;
		}

		boolean isScalePositive = d1.getScale() > 0;
		BigDecimal val = d1.getValue();
		for(int i=0; i<Math.abs(d1.getScale()); ++i) {
			if(isScalePositive) {
				val = val.divide(BigDecimal.TEN);
			} else {
				val = val.multiply(BigDecimal.TEN);
			}
		}

		//Rouding to negative infinity
		BigDecimal[] vals = val.divideAndRemainder(BigDecimal.ONE);
		if(vals[1].signum() != 0) {
			vals[0] = vals[0].subtract(BigDecimal.ONE);
		}

		try { new CobolDecimal(vals[0], 0).getField(currField, 0); } catch (CobolStopRunException e) {}
		return currField;
	}

	/**
	 * libcob/intrinsicのcob_intr_integer_partの実装
	 * @param srcfield
	 * @return
	 */
	public static AbstractCobolField funcIntegerPart(AbstractCobolField srcfield) {
		CobolFieldAttribute attr = new CobolFieldAttribute(
			CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY, 18, 0, CobolFieldAttribute.COB_FLAG_HAVE_SIGN, null);
		AbstractCobolField field = CobolFieldFactory.makeCobolField(8, (CobolDataStorage)null, attr);

		makeFieldEntry(field);
		currField.moveFrom(srcfield);
		return currField;
	}

	/**
	 * libcob/intrinsicのcob_intr_upper_caseの実装
	 * @param srcfield
	 * @return
	 */
	public static AbstractCobolField funcUpperCase(int offset, int length, AbstractCobolField srcfield) {
		makeFieldEntry(srcfield);
		int size = srcfield.getSize();
		CobolDataStorage currStorage = currField.getDataStorage();
		CobolDataStorage srcStorage = srcfield.getDataStorage();
		for(int i=0; i<size; ++i) {
			currStorage.setByte(i, (byte)Character.toUpperCase(srcStorage.getByte(i)));
		}
		if(offset > 0) {
			calcRefMod(currField, offset, length);
		}
		return currField;
	}

	/**
	 * libcob/intrinsicのcob_intr_lower_caseの実装
	 * @param srcfield
	 * @return
	 */
	public static AbstractCobolField funcLowerCase(int offset, int length, AbstractCobolField srcfield) {
		makeFieldEntry(srcfield);
		int size = srcfield.getSize();
		CobolDataStorage currStorage = currField.getDataStorage();
		CobolDataStorage srcStorage = srcfield.getDataStorage();
		for(int i=0; i<size; ++i) {
			currStorage.setByte(i, (byte)Character.toLowerCase(srcStorage.getByte(i)));
		}
		if(offset > 0) {
			calcRefMod(currField, offset, length);
		}
		return currField;
	}

	/**
	 * libcob/intrinsicのcob_intr_reverseの実装
	 * @param srcfield
	 * @return
	 */
	public static AbstractCobolField funcReverse(int offset, int length, AbstractCobolField srcfield) {
		makeFieldEntry(srcfield);
		int size = srcfield.getSize();
		CobolDataStorage currStorage = currField.getDataStorage();
		CobolDataStorage srcStorage = srcfield.getDataStorage();
		for(int i=0; i<size; ++i) {
			currStorage.setByte(i, srcStorage.getByte(srcfield.getSize() - i - 1));
		}
		if(offset > 0) {
			calcRefMod(currField, offset, length);
		}
		return currField;
	}

	/**
	 * libcob/intrinsicのcob_intr_when_compiledの実装
	 * @param offset
	 * @param length
	 * @param f
	 * @return
	 */
	public static AbstractCobolField funcWhenCompiled(int offset, int length, AbstractCobolField f) {
		makeFieldEntry(f);
		currField.getDataStorage().memcpy(f.getDataStorage(), f.getSize());
		if(offset > 0) {
			calcRefMod(currField, offset, length);
		}
		return currField;
	}

	/**
	 * libcob/intrinsicのcob_intr_current_dateの実装
	 * @param offset
	 * @param length
	 * @return
	 */


	public static AbstractCobolField funcCurrentDate(int offset, int length) {
		CobolFieldAttribute attr = new CobolFieldAttribute(
			CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null);
		AbstractCobolField field = CobolFieldFactory.makeCobolField(21, (CobolDataStorage)null, attr);
		makeFieldEntry(field);
		Calendar cal = Calendar.getInstance();
		//TODO Time Zoneを表示する機能を取り入れる

		try{
			if(CobolUtil.cobdate.matches("[0-9]{4}/[0-9]{2}/[0-9]{2}")){
				int year = Integer.parseInt(CobolUtil.cobdate.substring(0,4));
				int month = Integer.parseInt(CobolUtil.cobdate.substring(5,7));
				int day = Integer.parseInt(CobolUtil.cobdate.substring(8,10));
				cal.set(Calendar.YEAR, year);
				cal.set(Calendar.MONTH, month -1);
				cal.set(Calendar.DAY_OF_MONTH, day);
			}
		}catch(NullPointerException ignored){
		}

		String dateString = String.format("%4d%02d%02d%02d%02d%02d%02d00000",
			cal.get(Calendar.YEAR),
			cal.get(Calendar.MONTH) + 1,
			cal.get(Calendar.DAY_OF_MONTH),
			cal.get(Calendar.HOUR),
			cal.get(Calendar.MINUTE),
			cal.get(Calendar.SECOND),
			cal.get(Calendar.MILLISECOND) / 10);
		currField.getDataStorage().memcpy(dateString.getBytes());

		if(offset > 0) {
			calcRefMod(currField, offset, length);
		}
		return currField;
	}

	/**
	 * libcob/intrinsicのcob_intr_charの実装
	 * @param srcfield
	 * @return
	 */
	public static AbstractCobolField funcChar(AbstractCobolField srcfield) {
		CobolFieldAttribute attr = new CobolFieldAttribute(
			CobolFieldAttribute.COB_TYPE_ALPHANUMERIC, 0, 0, 0, null);
		AbstractCobolField field = CobolFieldFactory.makeCobolField(1, (CobolDataStorage)null, attr);
		makeFieldEntry(field);

		int i = srcfield.getInt();
		if(i < 1 || i > 256) {
			currField.getDataStorage().setByte(0, (byte)0);
		} else {
			currField.getDataStorage().setByte(0, (byte)(i - 1));
		}
		return currField;
	}

	/**
	 * libcob/intrinsicのcob_intr_ordの実装
	 * @param srcfield
	 * @return
	 */
	public static AbstractCobolField funcOrd(AbstractCobolField srcfield) {
		CobolFieldAttribute attr = new CobolFieldAttribute(
			CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY, 8, 0, 0, null);
		AbstractCobolField field = CobolFieldFactory.makeCobolField(4, (CobolDataStorage)null, attr);
		makeFieldEntry(field);

		currField.setInt(srcfield.getDataStorage().getByte(0) + 1);
		return currField;
	}


	/**
	 * libcob/intrinsicのcob_intr_date_of_integerの実装
	 * @param srcdays
	 * @return
	 */
	public static AbstractCobolField funcDateOfInteger(AbstractCobolField srcdays) {
		CobolFieldAttribute attr = new CobolFieldAttribute(
			CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY, 8, 0, 0, null);
		AbstractCobolField field = CobolFieldFactory.makeCobolField(8, (CobolDataStorage)null, attr);
		makeFieldEntry(field);

		CobolRuntimeException.setException(0);
		int days = srcdays.getInt();

		if(days < 1 || days > 3067671) {
			CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
			currField.getDataStorage().memset((byte)'0', 8);
			return currField;
		}

		int leapyear = 365;
		int baseyear = 1601;
		while(days > leapyear) {
			days -= leapyear;
			++baseyear;
			if(isLeapYear(baseyear)) {
				leapyear = 366;
			} else {
				leapyear = 365;
			}
		}
		int i;
		for(i=0; i<13; ++i) {
			if(isLeapYear(baseyear)) {
				if(days <= leapDays[i]) {
					days -= leapDays[i-1];
					break;
				}
			} else {
				if(days <= normalDays[i]) {
					days -= normalDays[i-1];
					break;
				}
			}
		}
		String dateString = String.format("%04d%02d%02d", baseyear, i , days);
		currField.getDataStorage().memcpy(dateString.getBytes());
		return currField;
	}

	/**
	 * libcob/intrinsicのcob_intr_day_of_integerの実装
	 * @param srcdays
	 * @return
	 */
	public static AbstractCobolField funcDayOfInteger(AbstractCobolField srcdays) {
		CobolFieldAttribute attr = new CobolFieldAttribute(
			CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY, 7, 0, 0, null);
		AbstractCobolField field = CobolFieldFactory.makeCobolField(7, (CobolDataStorage)null, attr);
		makeFieldEntry(field);

		CobolRuntimeException.setException(0);
		int days = srcdays.getInt();

		if(days < 1 || days > 3067671) {
			CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
			currField.getDataStorage().memset((byte)'0', 8);
			return currField;
		}

		int leapyear = 365;
		int baseyear = 1601;
		while(days > leapyear) {
			days -= leapyear;
			++baseyear;
			if(isLeapYear((baseyear))) {
				leapyear = 366;
			} else {
				leapyear = 365;
			}
		}
		String dateString = String.format("%04d%03d", baseyear, days);
		currField.getDataStorage().memcpy(dateString.getBytes());
		return currField;
	}

	/**
	 * libcob/intrinsicのcob_intr_integer_of_dateの実装
	 * @param srcfield
	 * @return
	 */
	public static AbstractCobolField funcIntegerOfDate(AbstractCobolField srcfield) {
		CobolFieldAttribute attr = new CobolFieldAttribute(
			CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY, 8, 0, 0, null);
		AbstractCobolField field = CobolFieldFactory.makeCobolField(4, (CobolDataStorage)null, attr);
		makeFieldEntry(field);

		CobolRuntimeException.setException(0);
		int indate = srcfield.getInt();
		int year = indate / 10000;
		if(year < 1601 || year > 9999) {
			CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
			currField.setInt(0);
			return currField;
		}
		indate %= 10000;
		int month = indate / 100;
		if(month < 1 || month > 12) {
			CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
			currField.setInt(0);
			return currField;
		}
		int days = indate % 100;
		if(days < 1 || days > 31) {
			CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
			currField.setInt(0);
			return currField;
		}
		if(isLeapYear(year)) {
			if(days > leapMonthDays[month]) {
				CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
				currField.setInt(0);
				return currField;
			}
		} else {
			if(days > normalMonthDays[month]) {
				CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
				currField.setInt(0);
				return currField;
			}
		}

		int totaldays = 0;
		int baseyear = 1601;
		while(baseyear != year) {
			if(isLeapYear(baseyear)) {
				totaldays += 366;
			} else {
				totaldays += 365;
			}
			++baseyear;
		}

		if(isLeapYear(baseyear)) {
			totaldays += leapDays[month - 1];
		} else {
			totaldays += normalDays[month - 1];
		}

		totaldays += days;
		currField.setInt(totaldays);
		return currField;
	}

	/**
	 * libcob/intrinsicのcob_intr_integer_of_dayの実装
	 * @param srcfield
	 * @return
	 */
	public static AbstractCobolField funcIntegerOfDay(AbstractCobolField srcfield) {
		CobolFieldAttribute attr = new CobolFieldAttribute(
			CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY, 8, 0, 0, null);
		AbstractCobolField field = CobolFieldFactory.makeCobolField(4, (CobolDataStorage)null, attr);
		makeFieldEntry(field);

		CobolRuntimeException.setException(0);
		int indate = srcfield.getInt();
		int year = indate / 1000;
		if(year < 1601 || year > 9999) {
			CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
			currField.setInt(0);
			return currField;
		}
		int days = indate % 1000;
		if(days < 1 || days > 365 + (isLeapYear(year) ? 1 : 0)) {
			CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
			currField.setInt(0);
			return currField;
		}
		int totaldays = 0;
		int baseyear = 1601;
		while(baseyear != year) {
			if(isLeapYear(baseyear)) {
				totaldays += 366;
			} else {
				totaldays += 365;
			}
			++baseyear;
		}
		totaldays += days;
		currField.setInt(totaldays);
		return currField;
	}

	/**
	 * libcob/intrinsicのcob_intr_factorialの実装
	 * @param srcfield
	 * @return
	 */
	public static AbstractCobolField funcFactorial(AbstractCobolField srcfield) {
		CobolFieldAttribute attr = new CobolFieldAttribute(
			CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY, 18, 0, 0, null);
		AbstractCobolField field = CobolFieldFactory.makeCobolField(8, (CobolDataStorage)null, attr);
		makeFieldEntry(field);

		CobolRuntimeException.setException(0);
		int srcval = srcfield.getInt();
		if(srcval < 0) {
			CobolRuntimeException.setException(CobolExceptionId.COB_EC_ARGUMENT_FUNCTION);
			currField.setInt(0);
			return currField;
		}
		BigDecimal d = new BigDecimal(1);
		for(int i=2; i<=srcval; ++i){
			d = d.multiply(new BigDecimal(i));
		}
		try { new CobolDecimal(d, 0).getField(currField, 0); } catch (CobolStopRunException e) {}
		return currField;
	}

	private static CobolDecimal mathFunctionBefore1(AbstractCobolField srcfield) {
		CobolDecimal d1 = new CobolDecimal();
		CobolFieldAttribute attr = new CobolFieldAttribute(
			CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY, 18, 17, CobolFieldAttribute.COB_FLAG_HAVE_SIGN, null);
		AbstractCobolField field = CobolFieldFactory.makeCobolField(8, (CobolDataStorage)null, attr);
		d1.setField(srcfield);
		makeFieldEntry(field);
		return d1;
	}

	private static CobolDecimal mathFunctionBefore2(AbstractCobolField srcfield) {
		CobolDecimal d1 = new CobolDecimal();
		d1.setField(srcfield);
		makeDoubleEntry();
		return d1;
	}

	private static AbstractCobolField mathFunctionAfter1(double mathd2) {
		if(Double.isNaN(mathd2) || mathd2 == Double.POSITIVE_INFINITY || mathd2 == Double.NEGATIVE_INFINITY) {
			currField.setInt(0);
			return currField;
		}
		long result = (long)mathd2;
		mathd2 -= result;
		for(int i=0; i<17; ++i) {
			mathd2 *= 10;
			int tempres = (int)mathd2;
			result *= 10;
			result += tempres;
			mathd2 -= tempres;
		}
		currField.getDataStorage().set(result);
		return currField;
	}

	private static AbstractCobolField mathFunctionAfter2(double mathd2) {
		if(Double.isNaN(mathd2) || mathd2 == Double.POSITIVE_INFINITY || mathd2 == Double.NEGATIVE_INFINITY) {
			currField.setInt(0);
			return currField;
		}
		currField.getDataStorage().set(mathd2);
		return currField;
	}

	/**
	 * libcob/intrinsicのcob_intr_expの実装
	 */
	public static AbstractCobolField funcExp(AbstractCobolField srcfield) {
		CobolDecimal d1 = mathFunctionBefore2(srcfield);
		double mathd2 = Math.pow(2.7182818284590452354, intrGetDouble(d1));
		return mathFunctionAfter2(mathd2);
	}

	/**
	 * libcob/intrinsicのcob_intr_exp10の実装
	 */
	public static AbstractCobolField funcExp10(AbstractCobolField srcfield) {
		CobolDecimal d1 = mathFunctionBefore2(srcfield);
		double mathd2 = Math.pow(10, intrGetDouble(d1));
		return mathFunctionAfter2(mathd2);
	}

	/**
	 * libcob/intrinsicのcob_intr_absの実装
	 */
	public static AbstractCobolField funcAbs(AbstractCobolField srcfield) {
		makeFieldEntry(srcfield);
		CobolDecimal d1 = new CobolDecimal();
		d1.setValue(d1.getValue().abs());
		try {
			d1.getField(currField, 0);
		} catch (CobolStopRunException e) {
		}
		return currField;
	}

	/**
	 * libcob/intrinsicのcob_intr_acosの実装
	 */
	public static AbstractCobolField funcAcos(AbstractCobolField srcfield) {
		CobolDecimal d1 = mathFunctionBefore1(srcfield);
		double mathd2 = Math.acos(intrGetDouble(d1));
		return mathFunctionAfter1(mathd2);
	}

	/**
	 * libcob/intrinsicのcob_intr_asinの実装
	 */
	public static AbstractCobolField funcAsin(AbstractCobolField srcfield) {
		CobolDecimal d1 = mathFunctionBefore1(srcfield);
		double mathd2 = Math.asin(intrGetDouble(d1));
		return mathFunctionAfter1(mathd2);
	}

	/**
	 * libcob/intrinsicのcob_intr_atanの実装
	 */
	public static AbstractCobolField funcAtan(AbstractCobolField srcfield) {
		CobolDecimal d1 = mathFunctionBefore1(srcfield);
		double mathd2 = Math.atan(intrGetDouble(d1));
		return mathFunctionAfter1(mathd2);
	}

	/**
	 * libcob/intrinsicのcob_intr_cosの実装
	 */
	public static AbstractCobolField funcCos(AbstractCobolField srcfield) {
		CobolDecimal d1 = mathFunctionBefore1(srcfield);
		double mathd2 = Math.cos(intrGetDouble(d1));
		return mathFunctionAfter1(mathd2);
	}

	/**
	 * libcob/intrinsicのcob_intr_logの実装
	 */
	public static AbstractCobolField funcLog(AbstractCobolField srcfield) {
		CobolDecimal d1 = mathFunctionBefore2(srcfield);
		double mathd2 = Math.log(intrGetDouble(d1));
		return mathFunctionAfter2(mathd2);
	}

	/**
	 * libcob/intrinsicのcob_intr_log10の実装
	 */
	public static AbstractCobolField funcLog10(AbstractCobolField srcfield) {
		CobolDecimal d1 = mathFunctionBefore2(srcfield);
		double mathd2 = Math.log10(intrGetDouble(d1));
		return mathFunctionAfter2(mathd2);
	}

	/**
	 * libcob/intrinsicのcob_intr_sinの実装
	 */
	public static AbstractCobolField funcSin(AbstractCobolField srcfield) {
		CobolDecimal d1 = mathFunctionBefore1(srcfield);
		double mathd2 = Math.sin(intrGetDouble(d1));
		return mathFunctionAfter1(mathd2);
	}

	/**
	 * libcob/intrinsicのcob_intr_sqrtの実装
	 */
	public static AbstractCobolField funcSqrt(AbstractCobolField srcfield) {
		CobolDecimal d1 = mathFunctionBefore2(srcfield);
		double mathd2 = Math.sqrt(intrGetDouble(d1));
		return mathFunctionAfter2(mathd2);
	}

	/**
	 * libcob/intrinsicのcob_intr_tanの実装
	 */
	public static AbstractCobolField funcTan(AbstractCobolField srcfield) {
		CobolDecimal d1 = mathFunctionBefore2(srcfield);
		double mathd2 = Math.tan(intrGetDouble(d1));
		return mathFunctionAfter2(mathd2);
	}

	/**
	 * libcob/intrinsicのcob_intr_numvalの実装
	 * @param srcfield
	 * @return
	 */
	public static AbstractCobolField funcNumval(AbstractCobolField srcfield) {
		CobolFieldAttribute attr = new CobolFieldAttribute(
			CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY, 18, 0, CobolFieldAttribute.COB_FLAG_HAVE_SIGN, null);
		AbstractCobolField field = CobolFieldFactory.makeCobolField(8, (CobolDataStorage)null, attr);

		CobolDataStorage s = srcfield.getDataStorage();
		boolean sign = false;
		boolean decimalSeen = false;
		long llval = 0;
		int integerDigits = 0;
		int decimalDigits = 0;
		StringBuilder integerBuff = new StringBuilder();
		StringBuilder decimalBuff = new StringBuilder();
		for(int i=0; i<srcfield.getSize(); ++i) {
			if(i < srcfield.getSize() - 1) {
				if((Character.toUpperCase(s.getByte(i)) == 'C' && Character.toUpperCase(s.getByte(i+1)) == 'R') ||
					(Character.toUpperCase(s.getByte(i)) == 'D' && Character.toUpperCase(s.getByte(i+1)) == 'B')) {
					sign = true;
					break;
				}
			}
			char c = (char) s.getByte(i);
			if(c == ' ' || c == '+') {
				continue;
			}
			if(c == '-') {
				sign = true;
				continue;
			}
			if(c == CobolModule.getCurrentModule().decimal_point) {
				decimalSeen = true;
				continue;
			}
			if(c >= '0' && c <= '9') {
				llval *= 10;
				llval += c - '0';
				if(decimalSeen) {
					decimalBuff.append(c);
					decimalDigits++;
				} else {
					integerBuff.append(c);
					integerDigits++;
				}
			}
			if(integerDigits + decimalDigits > 30) {
				break;
			}
		}
		if(integerDigits > 0) {
			integerBuff.setCharAt(0, '0');
		}
		if(decimalDigits > 0) {
			decimalBuff.setCharAt(0, '0');
		}
		if(sign) {
			llval = -llval;
		}
		if(integerDigits + decimalDigits <= 18) {
			attr.setScale(decimalDigits);
			makeFieldEntry(field);
			currField.getDataStorage().set(llval);
		} else {
			String dataString = String.format("%s%s.%s", sign ? "-" : "", integerBuff.toString(), decimalBuff.toString());
			double val = Double.parseDouble(dataString);
			makeDoubleEntry();
			currField.getDataStorage().set(val);
		}
		return currField;
	}
	/**
	 * libcob/intrinsicのcob_intr_numval_cの実装
	 * @param srcfield
	 * @return
	 */
	public static AbstractCobolField funcNumvalC(AbstractCobolField srcfield, AbstractCobolField currency) {
		CobolFieldAttribute attr = new CobolFieldAttribute(
			CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY, 18, 0, CobolFieldAttribute.COB_FLAG_HAVE_SIGN, null);
		AbstractCobolField field = CobolFieldFactory.makeCobolField(8, (CobolDataStorage)null, attr);

		CobolDataStorage s = srcfield.getDataStorage();
		boolean sign = false;
		boolean decimalSeen = false;
		long llval = 0;
		int integerDigits = 0;
		int decimalDigits = 0;
		StringBuilder integerBuff = new StringBuilder();
		StringBuilder decimalBuff = new StringBuilder();

		CobolDataStorage currencyData = null;
		if(currency != null) {
			if(currency.getSize() < srcfield.getSize()) {
				currencyData = currency.getDataStorage();
			}
		}
		for(int i=0; i<srcfield.getSize(); ++i) {
			char c = (char) srcfield.getDataStorage().getByte(i);
			if(i < srcfield.getSize() - 1) {
				char cc = (char) srcfield.getDataStorage().getByte(i+1);
				if((Character.toUpperCase(c) == 'C' && Character.toUpperCase(cc) == 'R') ||
					(Character.toUpperCase(c) == 'D' && Character.toUpperCase(cc) == 'B')) {
					sign = true;
					break;
				}
			}
			if(currencyData != null) {
				if(i < srcfield.getSize() - currency.getSize()) {
					if(currencyData.memcmp(srcfield.getDataStorage().getSubDataStorage(i), currency.getSize()) == 0) {
						i += (currency.getSize() - 1);
						continue;
					}
				}
			}
			if(c == ' ' || c == '+') {
				continue;
			}
			if(c == '-') {
				sign = true;
				continue;
			}
			if(c == CobolModule.getCurrentModule().decimal_point) {
				decimalSeen = true;
				continue;
			}
			if(c == CobolModule.getCurrentModule().currency_symbol) {
				continue;
			}
			if(c >= '0' && c <= '9') {
				llval *= 10;
				llval += c - '0';
				if(decimalSeen) {
					decimalBuff.append(c);
					decimalDigits++;
				} else {
					integerBuff.append(c);
					integerDigits++;
				}
			}
			if(integerDigits + decimalDigits > 30) {
				break;
			}
		}
		if(integerDigits > 0) {
			integerBuff.setCharAt(0, '0');
		}
		if(decimalDigits > 0) {
			decimalBuff.setCharAt(0, '0');
		}
		if(sign) {
			llval = -llval;
		}
		if(integerDigits + decimalDigits <= 18) {
			attr.setScale(decimalDigits);
			makeFieldEntry(field);
			currField.getDataStorage().set(llval);
		} else {
			String dataString = String.format("%s%s.%s", sign ? "-" : "", integerBuff.toString(), decimalBuff.toString());
			double val = Double.parseDouble(dataString);
			makeDoubleEntry();
			currField.getDataStorage().set(val);
		}
		return currField;
	}

	public static AbstractCobolField funcNumvalC(int n, AbstractCobolField currency) {
		return funcNumvalC(null, currency);
	}
	public static AbstractCobolField funcNumvalC(AbstractCobolField srcfield, int n) {
		return funcNumvalC(srcfield, null);
	}
	public static AbstractCobolField funcNumvalC(int n, int m) {
		return funcNumvalC(null, null);
	}

	/**
	 * libcob/intrinsicのcob_intr_annuityの実装
	 * @param srcfield1
	 * @param srcfield2
	 * @return
	 */
	public static AbstractCobolField funcAnnuity(AbstractCobolField srcfield1, AbstractCobolField srcfield2) {
		makeDoubleEntry();
		CobolDecimal d1 = new CobolDecimal();
		CobolDecimal d2 = new CobolDecimal();
		d1.setField(srcfield1);
		d2.setField(srcfield2);

		double mathd1 = intrGetDouble(d1);
		double mathd2 = intrGetDouble(d2);
		if(mathd1 == 0) {
			mathd1 = 1.0 / mathd2;
			currField.getDataStorage().set(mathd1);
			return currField;
		}

		mathd1 /= (1.0 - Math.pow(mathd1 + 1.0, 0.0 - mathd2));
		currField.getDataStorage().set(mathd1);
		return currField;
	}

	private static int sizeInBase10(BigDecimal d1) {
		String s = d1.toPlainString();
		int begin = s.charAt(0) == '-' ? 0 : -1;
		int pointIndex = s.indexOf('.');
		int end = pointIndex < 0 ? s.length() : pointIndex;
		return end - begin -1;
	}

	/**
	 * libcob/intrinsicのcob_intr_sumの実装
	 * @param params
	 * @param fields
	 * @return
	 */
	public static AbstractCobolField funcSum(int params, AbstractCobolField... fields) {
		CobolDecimal d1 = new CobolDecimal();
		CobolDecimal d2 = new CobolDecimal();
		d1.setValue(BigDecimal.ZERO);

		int scale = 0;
		for(AbstractCobolField f: fields) {
			if(f.getAttribute().getScale() > scale) {
				scale = f.getAttribute().getScale();
			}
			d2.setField(f);
			d1.add(d2);
		}

		int size = sizeInBase10(d1.getValue());
		AbstractCobolField field;
		if(size < 19) {
			CobolFieldAttribute attr = new CobolFieldAttribute(
				CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY, 18, scale, CobolFieldAttribute.COB_FLAG_HAVE_SIGN, null);
			field = CobolFieldFactory.makeCobolField(8, (CobolDataStorage)null, attr);
		} else {
			if(d1.getScale() > size) {
				size = d1.getScale();
			}
			if(scale > size) {
				size = scale;
			}
			CobolFieldAttribute attr = new CobolFieldAttribute(
				CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY, size, scale, CobolFieldAttribute.COB_FLAG_HAVE_SIGN, null);
			field = CobolFieldFactory.makeCobolField(size, (CobolDataStorage)null, attr);
		}
		makeFieldEntry(field);
		try {
			d1.getField(currField, 0);
		} catch(CobolStopRunException e) {
		}
		return currField;
	}

	/**
	 * libcob/intrinsicのcob_intr_ord_minの実装
	 * @param params
	 * @param fields
	 * @return
	 */
	public static AbstractCobolField funcOrdMin(int params, AbstractCobolField... fields) {
		CobolFieldAttribute attr = new CobolFieldAttribute(
			CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY, 8, 0, 0, null);
		AbstractCobolField field = CobolFieldFactory.makeCobolField(4, (CobolDataStorage)null, attr);
		makeFieldEntry(field);

		if(fields.length <= 1) {
			currField.setInt(0);
			return currField;
		}

		AbstractCobolField basef = fields[0];
		int ordmin = 0;
		for(int i=1; i<fields.length; ++i) {
			AbstractCobolField f = fields[i];
			if(f.compareTo(basef) < 0) {
				basef = f;
				ordmin = i;
			}
		}

		currField.setInt(ordmin + 1);
		return currField;
	}

	/**
	 * libcob/intrinsicのcob_intr_ord_maxの実装
	 * @param params
	 * @param fields
	 * @return
	 */
	public static AbstractCobolField funcOrdMax(int params, AbstractCobolField... fields) {
		CobolFieldAttribute attr = new CobolFieldAttribute(
			CobolFieldAttribute.COB_TYPE_NUMERIC_DISPLAY, 8, 0, 0, null);
		AbstractCobolField field = CobolFieldFactory.makeCobolField(4, (CobolDataStorage)null, attr);
		makeFieldEntry(field);

		if(fields.length <= 1) {
			currField.setInt(0);
			return currField;
		}

		AbstractCobolField basef = fields[0];
		int ordmax = 0;
		for(int i=1; i<fields.length; ++i) {
			AbstractCobolField f = fields[i];
			if(f.compareTo(basef) > 0) {
				basef = f;
				ordmax = i;
			}
		}

		currField.setInt(ordmax + 1);
		return currField;
	}

	/**
	 * libcob/intrinsicのcob_intr_minの実装
	 * @param params
	 * @param fields
	 * @return
	 */
	public static AbstractCobolField funcMin(int params, AbstractCobolField... fields) {
		AbstractCobolField beasef = fields[0];
		for(int i=1; i<fields.length; ++i) {
			AbstractCobolField f = fields[i];
			if(f.compareTo(beasef) < 0) {
				beasef = f;
			}
		}

		return beasef;
	}

	/**
	 * libcob/intrinsicのcob_intr_maxの実装
	 * @param params
	 * @param fields
	 * @return
	 */
	public static AbstractCobolField funcMax(int params, AbstractCobolField... fields) {
		AbstractCobolField beasef = fields[0];
		for(int i=1; i<fields.length; ++i) {
			AbstractCobolField f = fields[i];
			if(f.compareTo(beasef) > 0) {
				beasef = f;
			}
		}

		return beasef;
	}

	/**
	 * libcob/intrinsicのcob_intr_midrangeの実装
	 * @param params
	 * @param fields
	 * @return
	 */
	public static AbstractCobolField funcMidrange(int params, AbstractCobolField... fields) {
		makeDoubleEntry();
		AbstractCobolField basemin = fields[0];
		AbstractCobolField basemax = fields[0];
		for(int i=1; i<params; ++i) {
			AbstractCobolField f = fields[i];
			if(f.compareTo(basemin) < 0) {
				basemin = f;
			}
			if(f.compareTo(basemax) > 0) {
				basemax = f;
			}
		}

		CobolDecimal d1 = new CobolDecimal();
		CobolDecimal d2 = new CobolDecimal();
		d1.setField(basemin);
		d2.setField(basemax);
		d1.add(d2);
		d2 = new CobolDecimal(new BigDecimal(2), 0);
		try {
			d1.div(d2);
			d1.getField(currField, 0);
		} catch (CobolStopRunException e) {
		}
		return currField;
	}

	/**
	 * libcob/intrinsicのcob_intr_medianの実装
	 * @param params
	 * @param fields
	 * @return
	 */
	public static AbstractCobolField funcMedian(int params, AbstractCobolField... fields) {
		if(fields.length == 1) {
			return fields[0];
		}

		AbstractCobolField[] fieldAlloc = new AbstractCobolField[fields.length];

		for(int i=0; i<params; ++i) {
			fieldAlloc[i] = fields[i];
		}

		Arrays.sort(fieldAlloc, (a, b) -> a.compareTo(b));
		int i = params / 2;
		if(params % 2 != 0) {
			return fieldAlloc[i];
		} else {
			makeDoubleEntry();
			CobolDecimal d1 = new CobolDecimal();
			CobolDecimal d2 = new CobolDecimal();
			d1.setField(fieldAlloc[i]);
			d2.setField(fieldAlloc[i-1]);
			d1.add(d2);
			d2 = new CobolDecimal(new BigDecimal(2), 0);
			try {
				d1.div(d2);
				d1.getField(currField, 0);
			} catch (CobolStopRunException e) {
			}
			return currField;
		}
	}

	/**
	 * libcob/intrinsicのcob_intr_medianの実装
	 * @param pramas
	 * @param fields
	 * @return
	 */
	public static AbstractCobolField funcMean(int pramas, AbstractCobolField... fields) {
		CobolFieldAttribute attr = new CobolFieldAttribute(
			CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY, 18, 0, CobolFieldAttribute.COB_FLAG_HAVE_SIGN, null);
		AbstractCobolField field = CobolFieldFactory.makeCobolField(8, (CobolDataStorage)null, attr);
		CobolDecimal d1 = new CobolDecimal(BigDecimal.ZERO);
		CobolDecimal d2 = new CobolDecimal();

		for(AbstractCobolField f: fields) {
			d2.setField(f);
			d1.add(d2);
		}

		d2 = new CobolDecimal(new BigDecimal(fields.length), 0);
		try { d1.div(d2); } catch (CobolStopRunException e) {}

		CobolDataStorage storage = new CobolDataStorage(8);
		field.setDataStorage(storage);
		try { d1.getField(field, 0); } catch (CobolStopRunException e) {}
		long n = storage.longValue();
		int i;
		for(i=0; n!=0; n /= 10, ++i);
		field.setDataStorage(null);
		if(i <= 18) {
			attr.setScale(18 - i);
		}
		makeFieldEntry(field);
		try{ d1.getField(currField, 0); } catch (CobolStopRunException e) {}
		return currField;
	}

	/**
	 * libcob/intrinsicのcob_intr_modの実装
	 * @param srcfield1
	 * @param srcfield2
	 * @return
	 * @throws CobolStopRunException
	 */
	public static AbstractCobolField funcMod(AbstractCobolField srcfield1, AbstractCobolField srcfield2) throws CobolStopRunException {
		CobolFieldAttribute attr = new CobolFieldAttribute(
			CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY, 18, 0, CobolFieldAttribute.COB_FLAG_HAVE_SIGN, null);
		AbstractCobolField field = CobolFieldFactory.makeCobolField(8, (CobolDataStorage)null, attr);
		CobolDecimal d1 = new CobolDecimal();
		CobolDecimal d2 = new CobolDecimal();
		makeFieldEntry(field);

		AbstractCobolField f1 = funcInteger(intrBinop(srcfield1, '/', srcfield2));
		d1.setField(srcfield2);
		d2.setField(f1);
		d2.mul(d1);
		d1.setField(srcfield1);
		d1.sub(d2);
		try { d1.getField(currField, 0); } catch (CobolStopRunException e) {}
		return currField;
	}

	/**
	 * libcob/intrinsicのcob_intr_rangeの実装
	 * @param params
	 * @param fields
	 * @return
	 * @throws CobolStopRunException
	 */
	public static AbstractCobolField funcRange(int params, AbstractCobolField... fields) throws CobolStopRunException {
		CobolFieldAttribute attr = new CobolFieldAttribute(
			CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY, 18, 0, CobolFieldAttribute.COB_FLAG_HAVE_SIGN, null);
		AbstractCobolField field = CobolFieldFactory.makeCobolField(8, (CobolDataStorage)null, attr);
		CobolDecimal d1 = new CobolDecimal();
		CobolDecimal d2 = new CobolDecimal();

		AbstractCobolField basemin = fields[0];
		AbstractCobolField basemax = fields[0];
		for(int i=1; i<fields.length; ++i) {
			AbstractCobolField f = fields[i];
			if(f.compareTo(basemin) < 0) {
				basemin = f;
			}
			if(f.compareTo(basemax) > 0) {
				basemax = f;
			}
		}

		attr.setScale(basemin.getAttribute().getScale());
		if(basemax.getAttribute().getScale() > attr.getScale()) {
			attr.setScale(basemax.getAttribute().getScale());
		}
		makeFieldEntry(field);
		d1.setField(basemax);
		d2.setField(basemin);
		d1.sub(d2);
		d1.getField(currField, 0);
		return currField;
	}

	/**
	 * libcob/intrinsicのcob_intr_remの実装
	 * @param srcfield1
	 * @param srcfield2
	 * @return
	 * @throws CobolStopRunException
	 */
	public static AbstractCobolField funcRem(AbstractCobolField srcfield1, AbstractCobolField srcfield2) throws CobolStopRunException {
		CobolFieldAttribute attr = new CobolFieldAttribute(
			CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY, 18, 0, CobolFieldAttribute.COB_FLAG_HAVE_SIGN, null);
		AbstractCobolField field = CobolFieldFactory.makeCobolField(8, (CobolDataStorage)null, attr);
		AbstractCobolField f1 = funcIntegerPart(intrBinop(srcfield1, '/', srcfield2));
		CobolDecimal d1 = new CobolDecimal();
		CobolDecimal d2 = new CobolDecimal();

		d1.setField(srcfield2);
		d2.setField(f1);
		d2.mul(d1);
		d1.setField(srcfield1);
		d1.sub(d2);

		attr.setScale(srcfield1.getAttribute().getScale());
		if(srcfield2.getAttribute().getScale() > attr.getScale()) {
			attr.setScale(srcfield2.getAttribute().getScale());
		}
		makeFieldEntry(field);
		d1.getField(currField, 0);
		return currField;
	}

	/**
	 * libcob/intrinsicのcob_intr_randomの実装
	 * @param prams
	 * @param fields
	 * @return
	 */
	public static AbstractCobolField funcRandom(int prams, AbstractCobolField... fields) {
		CobolFieldAttribute attr = new CobolFieldAttribute(
			CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY, 18, 0, CobolFieldAttribute.COB_FLAG_HAVE_SIGN, null);
		AbstractCobolField field = CobolFieldFactory.makeCobolField(8, (CobolDataStorage)null, attr);

		if(fields.length > 0) {
			AbstractCobolField f = fields[0];
			int seed = f.getInt();
			if(seed < 0) {
				seed = 0;
			}
			random.setSeed(seed);
		}

		int r = random.nextInt(1000000001);

		int exp10 = 1;
		int i=0;
		for(i=0; i<10; ++i) {
			if(r / exp10 == 0) {
				break;
			}
			exp10 *= 10;
		}
		if(i == 0) {
			i = 1;
		}
		attr.setScale(i);
		makeFieldEntry(field);
		currField.getDataStorage().set((long)r);
		return currField;
	}

	/**
	 * libcob/intrinsicのcob_intr_varianceの実装
	 * @param prams
	 * @param fields
	 * @return
	 * @throws CobolStopRunException
	 */
	public static AbstractCobolField funcVariance(int prams, AbstractCobolField... fields) throws CobolStopRunException {
		CobolFieldAttribute attr = new CobolFieldAttribute(
			CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY, 18, 0, CobolFieldAttribute.COB_FLAG_HAVE_SIGN, null);
		AbstractCobolField field = CobolFieldFactory.makeCobolField(8, (CobolDataStorage)null, attr);

		if(fields.length == 1) {
			makeFieldEntry(field);
			currField.setInt(0);
			return currField;
		}

		CobolDecimal d1 = new CobolDecimal(new BigDecimal(0), 0);
		CobolDecimal d2 = new CobolDecimal();

		for(AbstractCobolField f: fields) {
			d2.setField(f);
			d1.add(d2);
		}

		d2.setValue(new BigDecimal(fields.length));
		d2.setScale(0);
		try { d1.div(d2); } catch (CobolStopRunException e) {}

		CobolDecimal d4 = new CobolDecimal(new BigDecimal(0), 0);

		for(AbstractCobolField f: fields) {
			d2.setField(f);
			d2.sub(d1);
			d2.mul(d2);
			d4.add(d2);
		}

		CobolDecimal d3 = new CobolDecimal(new BigDecimal(fields.length), 0);
		try { d4.div(d3); } catch (CobolStopRunException e) {}
		CobolDataStorage data = new CobolDataStorage(8);
		field.setDataStorage(data);
		d4.getDisplayField(field, 0);
		long n = data.longValue();
		int i=0;
		for(i=0; n!=0; n /=10, ++i);
		field.setDataStorage(null);
		if(i <= 18) {
			attr.setScale(18 - i);
		}
		makeDoubleEntry();
		d4.getField(currField, 0);
		return currField;
	}

	/**
	 * libcob/intrinsicのcob_intr_standard_deviationの実装
	 * @param prams
	 * @param fields
	 * @return
	 * @throws CobolStopRunException
	 */
	public static AbstractCobolField funcStandardDeviation(int prams, AbstractCobolField... fields) throws CobolStopRunException {
		CobolFieldAttribute attr = new CobolFieldAttribute(
			CobolFieldAttribute.COB_TYPE_NUMERIC_BINARY, 18, 0, CobolFieldAttribute.COB_FLAG_HAVE_SIGN, null);
		AbstractCobolField field = CobolFieldFactory.makeCobolField(8, (CobolDataStorage)null, attr);

		makeDoubleEntry();

		if(fields.length == 1) {
			makeFieldEntry(field);
			currField.setInt(0);
			return currField;
		}

		CobolDecimal d1 = new CobolDecimal(new BigDecimal(0), 0);
		CobolDecimal d2 = new CobolDecimal();

		for(AbstractCobolField f: fields) {
			d2.setField(f);
			d1.add(d2);
		}

		d2.setValue(new BigDecimal(fields.length));
		d2.setScale(0);
		try { d1.div(d2); } catch (CobolStopRunException e) {}

		CobolDecimal d4 = new CobolDecimal(new BigDecimal(0), 0);

		for(AbstractCobolField f: fields) {
			d2.setField(f);
			d2.sub(d1);
			d2.mul(d2);
			d4.add(d2);
		}

		CobolDecimal d3 = new CobolDecimal(new BigDecimal(fields.length), 0);
		try { d4.div(d3); } catch (CobolStopRunException e) {}
		d4.getField(currField, 0);
		return funcSqrt(currField);
	}

	/**
	 * libcob/intrinsicのcob_intr_present_valueの実装
	 * @param prams
	 * @param fields
	 * @return
	 * @throws CobolStopRunException
	 */
	public static AbstractCobolField funcPresentValue(int prams, AbstractCobolField... fields) throws CobolStopRunException {
		makeDoubleEntry();
		if(fields.length < 2) {
			System.err.println("Wrong number of parameters for FUNCTION PRESENT-VALUE");
			System.err.flush();
			currField.setInt(0);
			return currField;
		}
		AbstractCobolField f = fields[0];
		CobolDecimal d1 = new CobolDecimal();
		d1.setField(f);
		CobolDecimal d2 = new CobolDecimal(new BigDecimal(1), 0);
		d1.add(d2);
		CobolDecimal d4 = new CobolDecimal(new BigDecimal(0), 0);

		for(int i=1; i<fields.length; ++i) {
			f = fields[i];
			d2.setField(f);
			CobolDecimal d3 = new CobolDecimal(d1.getValue().add(BigDecimal.ZERO), d1.getScale());
			if(i > 1) {
				CobolDecimal d5 = new CobolDecimal(new BigDecimal(i), 0);
				d3.pow(d5);
			}
			d2.div(d3);
			d4.add(d2);
		}

		d4.getField(currField, 0);
		return currField;
	}

	/**
	 * libcob/intrinsicのcob_intr_present_valueの実装
	 * @param prams
	 * @param fields
	 * @return
	 * @throws CobolStopRunException
	 */
	public static AbstractCobolField funcNational(AbstractCobolField srcfield) {
		int size = srcfield.getSize();
		byte[] pdata = CobolNationalField.han2zen(srcfield.getDataStorage().getByteBuffer(size).array(), size);
		int ndata = CobolNationalField.workReturnSize;
		CobolFieldAttribute attr = new CobolFieldAttribute(CobolFieldAttribute.COB_TYPE_NATIONAL, 0, 0, 0, null);
		AbstractCobolField field = CobolFieldFactory.makeCobolField(ndata, (CobolDataStorage)null, attr);
		makeFieldEntry(field);
		currField.getDataStorage().memcpy(pdata, ndata);
		return currField;
	}
}
