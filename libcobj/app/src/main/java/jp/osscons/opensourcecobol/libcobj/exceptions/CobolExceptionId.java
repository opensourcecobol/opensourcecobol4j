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

/** 例外コードを定義するクラス */
public class CobolExceptionId {
    /** この例外コードは使用されない */
    public static final int COB_EC_ZERO = 0;

    /** この例外コードは使用されない */
    public static final int COB_EC_ALL = 1;

    /** TODO: 準備中 */
    public static final int COB_EC_ARGUMENT = 2;

    /** TODO: 準備中 */
    public static final int COB_EC_ARGUMENT_FUNCTION = 3;

    /** この例外コードは使用されない */
    public static final int COB_EC_ARGUMENT_IMP = 4;

    /** この例外コードは使用されない */
    public static final int COB_EC_BOUND = 5;

    /** この例外コードは使用されない */
    public static final int COB_EC_BOUND_IMP = 6;

    /** この例外コードは使用されない */
    public static final int COB_EC_BOUND_ODO = 7;

    /** この例外コードは使用されない */
    public static final int COB_EC_BOUND_OVERFLOW = 8;

    /** TODO: 準備中 */
    public static final int COB_EC_BOUND_PTR = 9;

    /** TODO: 準備中 */
    public static final int COB_EC_BOUND_REF_MOD = 10;

    /** この例外コードは使用されない */
    public static final int COB_EC_BOUND_SET = 11;

    /** TODO: 準備中 */
    public static final int COB_EC_BOUND_SUBSCRIPT = 12;

    /** この例外コードは使用されない */
    public static final int COB_EC_BOUND_TABLE_LIMIT = 13;

    /** この例外コードは使用されない */
    public static final int COB_EC_DATA = 14;

    /** この例外コードは使用されない */
    public static final int COB_EC_DATA_CONVERSION = 15;

    /** この例外コードは使用されない */
    public static final int COB_EC_DATA_IMP = 16;

    /** この例外コードは使用されない */
    public static final int COB_EC_DATA_INCOMPATIBLE = 17;

    /** この例外コードは使用されない */
    public static final int COB_EC_DATA_INTEGRITY = 18;

    /** この例外コードは使用されない */
    public static final int COB_EC_DATA_PTR_NULL = 19;

    /** この例外コードは使用されない */
    public static final int COB_EC_DATA_INFINITY = 20;

    /** この例外コードは使用されない */
    public static final int COB_EC_DATA_NEGATIVE_INFINITY = 21;

    /** この例外コードは使用されない */
    public static final int COB_EC_DATA_NOT_A_NUMBER = 22;

    /** この例外コードは使用されない */
    public static final int COB_EC_FLOW = 23;

    /** この例外コードは使用されない */
    public static final int COB_EC_FLOW_GLOBAL_EXIT = 24;

    /** この例外コードは使用されない */
    public static final int COB_EC_FLOW_GLOBAL_GOBACK = 25;

    /** この例外コードは使用されない */
    public static final int COB_EC_FLOW_IMP = 26;

    /** この例外コードは使用されない */
    public static final int COB_EC_FLOW_RELEASE = 27;

    /** この例外コードは使用されない */
    public static final int COB_EC_FLOW_REPORT = 28;

    /** この例外コードは使用されない */
    public static final int COB_EC_FLOW_RETURN = 29;

    /** この例外コードは使用されない */
    public static final int COB_EC_FLOW_SEARCH = 30;

    /** この例外コードは使用されない */
    public static final int COB_EC_FLOW_USE = 31;

    /** TODO: 準備中 */
    public static final int COB_EC_I_O = 32;

    /** TODO: 準備中 */
    public static final int COB_EC_I_O_AT_END = 33;

    /** TODO: 準備中 */
    public static final int COB_EC_I_O_EOP = 34;

    /** TODO: 準備中 */
    public static final int COB_EC_I_O_EOP_OVERFLOW = 35;

    /** TODO: 準備中 */
    public static final int COB_EC_I_O_FILE_SHARING = 36;

    /** TODO: 準備中 */
    public static final int COB_EC_I_O_IMP = 37;

    /** TODO: 準備中 */
    public static final int COB_EC_I_O_INVALID_KEY = 38;

    /** TODO: 準備中 */
    public static final int COB_EC_I_O_LINAGE = 39;

    /** TODO: 準備中 */
    public static final int COB_EC_I_O_LOGIC_ERROR = 40;

    /** TODO: 準備中 */
    public static final int COB_EC_I_O_PERMANENT_ERROR = 41;

    /** TODO: 準備中 */
    public static final int COB_EC_I_O_RECORD_OPERATION = 42;

    /** TODO: 準備中 */
    public static final int COB_EC_IMP = 43;

    /** TODO: 準備中 */
    public static final int COB_EC_IMP_ACCEPT = 44;

    /** TODO: 準備中 */
    public static final int COB_EC_IMP_DISPLAY = 45;

    /** この例外コードは使用されない */
    public static final int COB_EC_LOCALE = 46;

    /** この例外コードは使用されない */
    public static final int COB_EC_LOCALE_IMP = 47;

    /** この例外コードは使用されない */
    public static final int COB_EC_LOCALE_INCOMPATIBLE = 48;

    /** この例外コードは使用されない */
    public static final int COB_EC_LOCALE_INVALID = 49;

    /** この例外コードは使用されない */
    public static final int COB_EC_LOCALE_INVALID_PTR = 50;

    /** この例外コードは使用されない */
    public static final int COB_EC_LOCALE_MISSING = 51;

    /** この例外コードは使用されない */
    public static final int COB_EC_LOCALE_SIZE = 52;

    /** この例外コードは使用されない */
    public static final int COB_EC_OO = 53;

    /** この例外コードは使用されない */
    public static final int COB_EC_OO_CONFORMANCE = 54;

    /** この例外コードは使用されない */
    public static final int COB_EC_OO_EXCEPTION = 55;

    /** この例外コードは使用されない */
    public static final int COB_EC_OO_IMP = 56;

    /** この例外コードは使用されない */
    public static final int COB_EC_OO_METHOD = 57;

    /** この例外コードは使用されない */
    public static final int COB_EC_OO_NULL = 58;

    /** この例外コードは使用されない */
    public static final int COB_EC_OO_RESOURCE = 59;

    /** この例外コードは使用されない */
    public static final int COB_EC_OO_UNIVERSAL = 60;

    /** この例外コードは使用されない */
    public static final int COB_EC_ORDER = 61;

    /** この例外コードは使用されない */
    public static final int COB_EC_ORDER_IMP = 62;

    /** この例外コードは使用されない */
    public static final int COB_EC_ORDER_NOT_SUPPORTED = 63;

    /** TODO: 準備中 */
    public static final int COB_EC_OVERFLOW = 64;

    /** TODO: 準備中 */
    public static final int COB_EC_OVERFLOW_IMP = 65;

    /** TODO: 準備中 */
    public static final int COB_EC_OVERFLOW_STRING = 66;

    /** TODO: 準備中 */
    public static final int COB_EC_OVERFLOW_UNSTRING = 67;

    /** この例外コードは使用されない */
    public static final int COB_EC_PROGRAM = 68;

    /** この例外コードは使用されない */
    public static final int COB_EC_PROGRAM_ARG_MISMATCH = 69;

    /** この例外コードは使用されない */
    public static final int COB_EC_PROGRAM_ARG_OMITTED = 70;

    /** この例外コードは使用されない */
    public static final int COB_EC_PROGRAM_CANCEL_ACTIVE = 71;

    /** この例外コードは使用されない */
    public static final int COB_EC_PROGRAM_IMP = 72;

    /** TODO: 準備中 */
    public static final int COB_EC_PROGRAM_NOT_FOUND = 73;

    /** この例外コードは使用されない */
    public static final int COB_EC_PROGRAM_PTR_NULL = 74;

    /** この例外コードは使用されない */
    public static final int COB_EC_PROGRAM_RECURSIVE_CALL = 75;

    /** この例外コードは使用されない */
    public static final int COB_EC_PROGRAM_RESOURCES = 76;

    /** この例外コードは使用されない */
    public static final int COB_EC_RAISING = 77;

    /** この例外コードは使用されない */
    public static final int COB_EC_RAISING_IMP = 78;

    /** この例外コードは使用されない */
    public static final int COB_EC_RAISING_NOT_SPECIFIED = 79;

    /** この例外コードは使用されない */
    public static final int COB_EC_RANGE = 80;

    /** この例外コードは使用されない */
    public static final int COB_EC_RANGE_IMP = 81;

    /** この例外コードは使用されない */
    public static final int COB_EC_RANGE_INDEX = 82;

    /** TODO: 準備中 */
    public static final int COB_EC_RANGE_INSPECT_SIZE = 83;

    /** この例外コードは使用されない */
    public static final int COB_EC_RANGE_INVALID = 84;

    /** この例外コードは使用されない */
    public static final int COB_EC_RANGE_PERFORM_VARYING = 85;

    /** この例外コードは使用されない */
    public static final int COB_EC_RANGE_PTR = 86;

    /** この例外コードは使用されない */
    public static final int COB_EC_RANGE_SEARCH_INDEX = 87;

    /** この例外コードは使用されない */
    public static final int COB_EC_RANGE_SEARCH_NO_MATCH = 88;

    /** この例外コードは使用されない */
    public static final int COB_EC_REPORT = 89;

    /** この例外コードは使用されない */
    public static final int COB_EC_REPORT_ACTIVE = 90;

    /** この例外コードは使用されない */
    public static final int COB_EC_REPORT_COLUMN_OVERLAP = 91;

    /** この例外コードは使用されない */
    public static final int COB_EC_REPORT_FILE_MODE = 92;

    /** この例外コードは使用されない */
    public static final int COB_EC_REPORT_IMP = 93;

    /** この例外コードは使用されない */
    public static final int COB_EC_REPORT_INACTIVE = 94;

    /** この例外コードは使用されない */
    public static final int COB_EC_REPORT_LINE_OVERLAP = 95;

    /** この例外コードは使用されない */
    public static final int COB_EC_REPORT_NOT_TERMINATED = 96;

    /** この例外コードは使用されない */
    public static final int COB_EC_REPORT_PAGE_LIMIT = 97;

    /** この例外コードは使用されない */
    public static final int COB_EC_REPORT_PAGE_WIDTH = 98;

    /** この例外コードは使用されない */
    public static final int COB_EC_REPORT_SUM_SIZE = 99;

    /** この例外コードは使用されない */
    public static final int COB_EC_REPORT_VARYING = 100;

    /** この例外コードは使用されない */
    public static final int COB_EC_SCREEN = 101;

    /** この例外コードは使用されない */
    public static final int COB_EC_SCREEN_FIELD_OVERLAP = 102;

    /** この例外コードは使用されない */
    public static final int COB_EC_SCREEN_IMP = 103;

    /** この例外コードは使用されない */
    public static final int COB_EC_SCREEN_ITEM_TRUNCATED = 104;

    /** この例外コードは使用されない */
    public static final int COB_EC_SCREEN_LINE_NUMBER = 105;

    /** この例外コードは使用されない */
    public static final int COB_EC_SCREEN_STARTING_COLUMN = 106;

    /** この例外コードは使用されない */
    public static final int COB_EC_SIZE = 107;

    /** この例外コードは使用されない */
    public static final int COB_EC_SIZE_ADDRESS = 108;

    /** この例外コードは使用されない */
    public static final int COB_EC_SIZE_EXPONENTIATION = 109;

    /** この例外コードは使用されない */
    public static final int COB_EC_SIZE_IMP = 110;

    /** TODO: 準備中 */
    public static final int COB_EC_SIZE_OVERFLOW = 111;

    /** この例外コードは使用されない */
    public static final int COB_EC_SIZE_TRUNCATION = 112;

    /** この例外コードは使用されない */
    public static final int COB_EC_SIZE_UNDERFLOW = 113;

    /** この例外コードは使用されない */
    public static final int COB_EC_SIZE_ZERO_DIVIDE = 114;

    /** この例外コードは使用されない */
    public static final int COB_EC_SORT_MERGE = 115;

    /** この例外コードは使用されない */
    public static final int COB_EC_SORT_MERGE_ACTIVE = 116;

    /** この例外コードは使用されない */
    public static final int COB_EC_SORT_MERGE_FILE_OPEN = 117;

    /** この例外コードは使用されない */
    public static final int COB_EC_SORT_MERGE_IMP = 118;

    /** この例外コードは使用されない */
    public static final int COB_EC_SORT_MERGE_RELEASE = 119;

    /** この例外コードは使用されない */
    public static final int COB_EC_SORT_MERGE_RETURN = 120;

    /** この例外コードは使用されない */
    public static final int COB_EC_SORT_MERGE_SEQUENCE = 121;

    /** この例外コードは使用されない */
    public static final int COB_EC_STORAGE = 122;

    /** この例外コードは使用されない */
    public static final int COB_EC_STORAGE_IMP = 123;

    /** この例外コードは使用されない */
    public static final int COB_EC_STORAGE_NOT_ALLOC = 124;

    /** この例外コードは使用されない */
    public static final int COB_EC_STORAGE_NOT_AVAIL = 125;

    /** この例外コードは使用されない */
    public static final int COB_EC_USER = 126;

    /** この例外コードは使用されない */
    public static final int COB_EC_VALIDATE = 127;

    /** この例外コードは使用されない */
    public static final int COB_EC_VALIDATE_CONTENT = 128;

    /** この例外コードは使用されない */
    public static final int COB_EC_VALIDATE_FORMAT = 129;

    /** この例外コードは使用されない */
    public static final int COB_EC_VALIDATE_IMP = 130;

    /** この例外コードは使用されない */
    public static final int COB_EC_VALIDATE_RELATION = 131;

    /** この例外コードは使用されない */
    public static final int COB_EC_VALIDATE_VARYING = 132;

    /** この例外コードは使用されない */
    public static final int COB_EC_FUNCTION = 133;

    /** この例外コードは使用されない */
    public static final int COB_EC_FUNCTION_NOT_FOUND = 134;

    /** この例外コードは使用されない */
    public static final int COB_EC_FUNCTION_PTR_INVALID = 135;

    /** この例外コードは使用されない */
    public static final int COB_EC_FUNCTION_PTR_NULL = 136;

    /** この例外コードは使用されない */
    public static final int COB_EC_XML = 137;

    /** この例外コードは使用されない */
    public static final int COB_EC_XML_CODESET = 138;

    /** この例外コードは使用されない */
    public static final int COB_EC_XML_CODESET_CONVERSION = 139;

    /** この例外コードは使用されない */
    public static final int COB_EC_XML_COUNT = 140;

    /** この例外コードは使用されない */
    public static final int COB_EC_XML_DOCUMENT_TYPE = 141;

    /** この例外コードは使用されない */
    public static final int COB_EC_XML_IMPLICIT_CLOSE = 142;

    /** この例外コードは使用されない */
    public static final int COB_EC_XML_INVALID = 143;

    /** この例外コードは使用されない */
    public static final int COB_EC_XML_NAMESPACE = 144;

    /** この例外コードは使用されない */
    public static final int COB_EC_XML_STACKED_OPEN = 145;

    /** この例外コードは使用されない */
    public static final int COB_EC_XML_RANGE = 146;

    /** この例外コードは使用されない */
    public static final int COB_EC_MAX = 147;
}
