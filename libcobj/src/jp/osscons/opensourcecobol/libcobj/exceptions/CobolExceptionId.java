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
package jp.osscons.opensourcecobol.libcobj.exceptions;

public class CobolExceptionId {
	public static final int COB_EC_ZERO = 0;
	public static final int COB_EC_ALL = 1;
	public static final int COB_EC_ARGUMENT = 2;
	public static final int COB_EC_ARGUMENT_FUNCTION = 3;
	public static final int COB_EC_ARGUMENT_IMP = 4;
	public static final int COB_EC_BOUND = 5;
	public static final int COB_EC_BOUND_IMP = 6;
	public static final int COB_EC_BOUND_ODO = 7;
	public static final int COB_EC_BOUND_OVERFLOW = 8;
	public static final int COB_EC_BOUND_PTR = 9;
	public static final int COB_EC_BOUND_REF_MOD = 10;
	public static final int COB_EC_BOUND_SET = 11;
	public static final int COB_EC_BOUND_SUBSCRIPT = 12;
	public static final int COB_EC_BOUND_TABLE_LIMIT = 13;
	public static final int COB_EC_DATA = 14;
	public static final int COB_EC_DATA_CONVERSION = 15;
	public static final int COB_EC_DATA_IMP = 16;
	public static final int COB_EC_DATA_INCOMPATIBLE = 17;
	public static final int COB_EC_DATA_INTEGRITY = 18;
	public static final int COB_EC_DATA_PTR_NULL = 19;
	public static final int COB_EC_DATA_INFINITY = 20;
	public static final int COB_EC_DATA_NEGATIVE_INFINITY = 21;
	public static final int COB_EC_DATA_NOT_A_NUMBER = 22;
	public static final int COB_EC_FLOW = 23;
	public static final int COB_EC_FLOW_GLOBAL_EXIT = 24;
	public static final int COB_EC_FLOW_GLOBAL_GOBACK = 25;
	public static final int COB_EC_FLOW_IMP = 26;
	public static final int COB_EC_FLOW_RELEASE = 27;
	public static final int COB_EC_FLOW_REPORT = 28;
	public static final int COB_EC_FLOW_RETURN = 29;
	public static final int COB_EC_FLOW_SEARCH = 30;
	public static final int COB_EC_FLOW_USE = 31;
	public static final int COB_EC_I_O = 32;
	public static final int COB_EC_I_O_AT_END = 33;
	public static final int COB_EC_I_O_EOP = 34;
	public static final int COB_EC_I_O_EOP_OVERFLOW = 35;
	public static final int COB_EC_I_O_FILE_SHARING = 36;
	public static final int COB_EC_I_O_IMP = 37;
	public static final int COB_EC_I_O_INVALID_KEY = 38;
	public static final int COB_EC_I_O_LINAGE = 39;
	public static final int COB_EC_I_O_LOGIC_ERROR = 40;
	public static final int COB_EC_I_O_PERMANENT_ERROR = 41;
	public static final int COB_EC_I_O_RECORD_OPERATION = 42;
	public static final int COB_EC_IMP = 43;
	public static final int COB_EC_IMP_ACCEPT = 44;
	public static final int COB_EC_IMP_DISPLAY = 45;
	public static final int COB_EC_LOCALE = 46;
	public static final int COB_EC_LOCALE_IMP = 47;
	public static final int COB_EC_LOCALE_INCOMPATIBLE = 48;
	public static final int COB_EC_LOCALE_INVALID = 49;
	public static final int COB_EC_LOCALE_INVALID_PTR = 50;
	public static final int COB_EC_LOCALE_MISSING = 51;
	public static final int COB_EC_LOCALE_SIZE = 52;
	public static final int COB_EC_OO = 53;
	public static final int COB_EC_OO_CONFORMANCE = 54;
	public static final int COB_EC_OO_EXCEPTION = 55;
	public static final int COB_EC_OO_IMP = 56;
	public static final int COB_EC_OO_METHOD = 57;
	public static final int COB_EC_OO_NULL = 58;
	public static final int COB_EC_OO_RESOURCE = 59;
	public static final int COB_EC_OO_UNIVERSAL = 60;
	public static final int COB_EC_ORDER = 61;
	public static final int COB_EC_ORDER_IMP = 62;
	public static final int COB_EC_ORDER_NOT_SUPPORTED = 63;
	public static final int COB_EC_OVERFLOW = 64;
	public static final int COB_EC_OVERFLOW_IMP = 65;
	public static final int COB_EC_OVERFLOW_STRING = 66;
	public static final int COB_EC_OVERFLOW_UNSTRING = 67;
	public static final int COB_EC_PROGRAM = 68;
	public static final int COB_EC_PROGRAM_ARG_MISMATCH = 69;
	public static final int COB_EC_PROGRAM_ARG_OMITTED = 70;
	public static final int COB_EC_PROGRAM_CANCEL_ACTIVE = 71;
	public static final int COB_EC_PROGRAM_IMP = 72;
	public static final int COB_EC_PROGRAM_NOT_FOUND = 73;
	public static final int COB_EC_PROGRAM_PTR_NULL = 74;
	public static final int COB_EC_PROGRAM_RECURSIVE_CALL = 75;
	public static final int COB_EC_PROGRAM_RESOURCES = 76;
	public static final int COB_EC_RAISING = 77;
	public static final int COB_EC_RAISING_IMP = 78;
	public static final int COB_EC_RAISING_NOT_SPECIFIED = 79;
	public static final int COB_EC_RANGE = 80;
	public static final int COB_EC_RANGE_IMP = 81;
	public static final int COB_EC_RANGE_INDEX = 82;
	public static final int COB_EC_RANGE_INSPECT_SIZE = 83;
	public static final int COB_EC_RANGE_INVALID = 84;
	public static final int COB_EC_RANGE_PERFORM_VARYING = 85;
	public static final int COB_EC_RANGE_PTR = 86;
	public static final int COB_EC_RANGE_SEARCH_INDEX = 87;
	public static final int COB_EC_RANGE_SEARCH_NO_MATCH = 88;
	public static final int COB_EC_REPORT = 89;
	public static final int COB_EC_REPORT_ACTIVE = 90;
	public static final int COB_EC_REPORT_COLUMN_OVERLAP = 91;
	public static final int COB_EC_REPORT_FILE_MODE = 92;
	public static final int COB_EC_REPORT_IMP = 93;
	public static final int COB_EC_REPORT_INACTIVE = 94;
	public static final int COB_EC_REPORT_LINE_OVERLAP = 95;
	public static final int COB_EC_REPORT_NOT_TERMINATED = 96;
	public static final int COB_EC_REPORT_PAGE_LIMIT = 97;
	public static final int COB_EC_REPORT_PAGE_WIDTH = 98;
	public static final int COB_EC_REPORT_SUM_SIZE = 99;
	public static final int COB_EC_REPORT_VARYING = 100;
	public static final int COB_EC_SCREEN = 101;
	public static final int COB_EC_SCREEN_FIELD_OVERLAP = 102;
	public static final int COB_EC_SCREEN_IMP = 103;
	public static final int COB_EC_SCREEN_ITEM_TRUNCATED = 104;
	public static final int COB_EC_SCREEN_LINE_NUMBER = 105;
	public static final int COB_EC_SCREEN_STARTING_COLUMN = 106;
	public static final int COB_EC_SIZE = 107;
	public static final int COB_EC_SIZE_ADDRESS = 108;
	public static final int COB_EC_SIZE_EXPONENTIATION = 109;
	public static final int COB_EC_SIZE_IMP = 110;
	public static final int COB_EC_SIZE_OVERFLOW = 111;
	public static final int COB_EC_SIZE_TRUNCATION = 112;
	public static final int COB_EC_SIZE_UNDERFLOW = 113;
	public static final int COB_EC_SIZE_ZERO_DIVIDE = 114;
	public static final int COB_EC_SORT_MERGE = 115;
	public static final int COB_EC_SORT_MERGE_ACTIVE = 116;
	public static final int COB_EC_SORT_MERGE_FILE_OPEN = 117;
	public static final int COB_EC_SORT_MERGE_IMP = 118;
	public static final int COB_EC_SORT_MERGE_RELEASE = 119;
	public static final int COB_EC_SORT_MERGE_RETURN = 120;
	public static final int COB_EC_SORT_MERGE_SEQUENCE = 121;
	public static final int COB_EC_STORAGE = 122;
	public static final int COB_EC_STORAGE_IMP = 123;
	public static final int COB_EC_STORAGE_NOT_ALLOC = 124;
	public static final int COB_EC_STORAGE_NOT_AVAIL = 125;
	public static final int COB_EC_USER = 126;
	public static final int COB_EC_VALIDATE = 127;
	public static final int COB_EC_VALIDATE_CONTENT = 128;
	public static final int COB_EC_VALIDATE_FORMAT = 129;
	public static final int COB_EC_VALIDATE_IMP = 130;
	public static final int COB_EC_VALIDATE_RELATION = 131;
	public static final int COB_EC_VALIDATE_VARYING = 132;
	public static final int COB_EC_FUNCTION = 133;
	public static final int COB_EC_FUNCTION_NOT_FOUND = 134;
	public static final int COB_EC_FUNCTION_PTR_INVALID = 135;
	public static final int COB_EC_FUNCTION_PTR_NULL = 136;
	public static final int COB_EC_XML = 137;
	public static final int COB_EC_XML_CODESET = 138;
	public static final int COB_EC_XML_CODESET_CONVERSION = 139;
	public static final int COB_EC_XML_COUNT = 140;
	public static final int COB_EC_XML_DOCUMENT_TYPE = 141;
	public static final int COB_EC_XML_IMPLICIT_CLOSE = 142;
	public static final int COB_EC_XML_INVALID = 143;
	public static final int COB_EC_XML_NAMESPACE = 144;
	public static final int COB_EC_XML_STACKED_OPEN = 145;
	public static final int COB_EC_XML_RANGE = 146;
	public static final int COB_EC_MAX = 147;
}
