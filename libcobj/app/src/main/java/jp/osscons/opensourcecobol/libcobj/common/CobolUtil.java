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

import java.time.DateTimeException;
import java.time.LocalDateTime;
import java.util.Calendar;
import java.util.Properties;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolExceptionId;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolExceptionInfo;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolRuntimeException;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolStopRunException;
import jp.osscons.opensourcecobol.libcobj.file.CobolFile;

/** TDOD: 準備中 */
public class CobolUtil {
    private static boolean cob_io_assume_rewrite = false;
    private static boolean cob_verbose = false;
    private static HandlerList hdlrs = null;
    private static String runtime_err_str = null;

    /** TDOD: 準備中 */
    public static LocalDateTime cobLocalTm = null;

    /** TDOD: 準備中 */
    public static String cobLocalEnv = null;

    /** TDOD: 準備中 */
    public static String[] commandLineArgs = null;

    /** TDOD: 準備中 */
    public static int currentArgIndex = 1;

    /** TDOD: 準備中 */
    public static boolean nibbleCForUnsigned = false;

    /** TDOD: 準備中 */
    public static boolean[] cobSwitch = new boolean[8];

    /** TDOD: 準備中 */
    public static int cobSaveCallParams = 0;

    /** TDOD: 準備中 */
    public static boolean verbose = false;

    /** TDOD: 準備中 */
    public static boolean cobErrorOnExitFlag = false;

    /** TDOD: 準備中 */
    public static Calendar cal;

    /** TDOD: 準備中 */
    public static int fileSeqWriteBufferSize = 10;

    /** DISPLAY/ACCEPT文によるデータ出力時のエンコーディング */
    public static CobolEncoding terminalEncoding = CobolEncoding.SHIFT_JIS;

    private static boolean lineTrace = false;

    /** TDOD: 準備中 */
    private static String sourceFile;

    /** TDOD: 準備中 */
    private static int sourceLine;

    /** TDOD: 準備中 */
    private static String currProgramId;

    /** TDOD: 準備中 */
    private static String currSection;

    /** TDOD: 準備中 */
    private static String currParagraph;

    /** TDOD: 準備中 */
    private static String sourceStatement;

    abstract static class HandlerList {
        public HandlerList next = null;

        public abstract int proc(String s);
    }

    /** TDOD: 準備中 */
    public static final int FERROR_INITIALIZED = 0;

    /** TDOD: 準備中 */
    public static final int FERROR_CODEGEN = 1;

    /** TDOD: 準備中 */
    public static final int FERROR_CHAINING = 2;

    /** TDOD: 準備中 */
    public static final int FERROR_STACK = 3;

    private static Properties envVarTable = new Properties();

    // libcob/common.cのcob_check_envの実装
    /**
     * TODO: 準備中
     *
     * @param name TODO: 準備中
     * @param value TODO: 準備中
     * @return TODO: 準備中
     */
    public static int checkEnv(String name, String value) {
        if (name == null || value == null) {
            return 0;
        }

        String s = CobolUtil.getEnv(name);
        if (s != null) {
            if (s.contentEquals(value)) {
                return 1;
            }
        }
        return 0;
    }

    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     */
    public static boolean cob_io_rewwrite_assumed() {
        return cob_io_assume_rewrite;
    }

    /**
     * TODO: 準備中
     *
     * @param offset TODO: 準備中
     * @param length TODO: 準備中
     * @param size TODO: 準備中
     * @param name TODO: 準備中
     * @throws CobolStopRunException TODO: 準備中
     */
    public static void cobCheckRefModNational(int offset, long length, int size, byte[] name)
            throws CobolStopRunException {
        CobolUtil.cobCheckRefMod((offset + 1) / 2, length / 2, size / 2, name);
    }

    /**
     * TODO: 準備中
     *
     * @param offset TODO: 準備中
     * @param length TODO: 準備中
     * @param size TODO: 準備中
     * @param name TODO: 準備中
     * @throws CobolStopRunException TODO: 準備中
     */
    public static void cobCheckRefModNational(int offset, long length, int size, String name)
            throws CobolStopRunException {
        CobolUtil.cobCheckRefMod((offset + 1) / 2, length / 2, size / 2, name);
    }

    /**
     * TODO: 準備中
     *
     * @param offset TODO: 準備中
     * @param length TODO: 準備中
     * @param size TODO: 準備中
     * @param name TODO: 準備中
     * @param nameLen TODO: 準備中
     * @throws CobolStopRunException TODO: 準備中
     */
    public static void cobCheckRefMod(
            int offset, long length, int size, CobolDataStorage name, int nameLen)
            throws CobolStopRunException {
        cobCheckRefMod(offset, length, size, name.getByteArrayRef(0, nameLen));
    }

    /**
     * TODO: 準備中
     *
     * @param offset TODO: 準備中
     * @param length TODO: 準備中
     * @param size TODO: 準備中
     * @param name TODO: 準備中
     * @param nameLen TODO: 準備中
     * @throws CobolStopRunException TODO: 準備中
     */
    public static void cobCheckRefMod(int offset, long length, int size, byte[] name, int nameLen)
            throws CobolStopRunException {
        cobCheckRefMod(offset, length, size, name);
    }

    /**
     * TODO: 準備中
     *
     * @param offset TODO: 準備中
     * @param length TODO: 準備中
     * @param size TODO: 準備中
     * @param name TODO: 準備中
     * @throws CobolStopRunException TODO: 準備中
     */
    public static void cobCheckRefMod(int offset, long length, int size, byte[] name)
            throws CobolStopRunException {
        CobolUtil.cobCheckRefMod(
                offset, length, size, new String(name, AbstractCobolField.charSetSJIS));
    }

    /**
     * TODO: 準備中
     *
     * @param offset TODO: 準備中
     * @param length TODO: 準備中
     * @param size TODO: 準備中
     * @param name TODO: 準備中
     * @param nameLen TODO: 準備中
     * @throws CobolStopRunException TODO: 準備中
     */
    public static void cobCheckRefMod(int offset, long length, int size, String name, int nameLen)
            throws CobolStopRunException {
        cobCheckRefMod(offset, length, size, name);
    }

    /**
     * TODO: 準備中
     *
     * @param offset TODO: 準備中
     * @param length TODO: 準備中
     * @param size TODO: 準備中
     * @param name TODO: 準備中
     * @throws CobolStopRunException TODO: 準備中
     */
    public static void cobCheckRefMod(int offset, long length, int size, String name)
            throws CobolStopRunException {
        /* check the offset */
        if (offset < 1 || offset > size) {
            CobolRuntimeException.setException(CobolExceptionId.COB_EC_BOUND_REF_MOD);
            CobolUtil.runtimeError(String.format("Offset of '%s' out of bounds: %d", name, offset));
            CobolStopRunException.stopRunAndThrow(1);
        }

        /* check the length */
        if (length < 1 || offset + length - 1 > size) {
            CobolRuntimeException.setException(CobolExceptionId.COB_EC_BOUND_REF_MOD);
            CobolUtil.runtimeError(String.format("Length of '%s' out of bounds: %d", name, length));
            CobolStopRunException.stopRunAndThrow(1);
        }
    }

    /**
     * TODO: 準備中
     *
     * @param x TODO: 準備中
     * @param name TODO: 準備中
     * @throws CobolStopRunException TODO: 準備中
     */
    public static void cobCheckBased(CobolDataStorage x, byte[] name) throws CobolStopRunException {
        if (x == null) {
            CobolUtil.runtimeError(String.format("BASED/LINKAGE item '%s' has NULL address", name));
            CobolStopRunException.stopRunAndThrow(1);
        }
    }

    /**
     * TODO: 準備中
     *
     * @param argv TODO: 準備中
     * @param cobInitialized TODO: 準備中
     */
    public static void cob_init(String[] argv, boolean cobInitialized) {
        // TODO 未完成
        if (!cobInitialized) {
            CobolUtil.commandLineArgs = argv;
            CobolInspect.initString();
            CobolFile.cob_init_fileio();
            CobolIntrinsic.init();
            CobolUtil.envVarTable = new Properties();

            for (int i = 0; i < 8; ++i) {
                String envVariableName = String.format("COB_SWITCH_%d", i + 1);
                String envValue = CobolUtil.getEnv(envVariableName);
                if (envValue == null) {
                    CobolUtil.cobSwitch[i] = false;
                } else {
                    CobolUtil.cobSwitch[i] = "ON".equals(envValue);
                }
            }
        }

        cal = Calendar.getInstance();
        String s = CobolUtil.getEnv("COB_DATE");
        if (s != null) {
            Pattern p = Pattern.compile("([0-9]{4})/([0-9]{2})/([0-9]{2})");
            Matcher m = p.matcher(s);
            if (m.matches()) {
                date_time_block:
                if (m.groupCount() != 3) {
                    System.err.println("Warning: COB_DATE format invalid, ignored.");
                } else {
                    int year = Integer.parseInt(m.group(1));
                    int month = Integer.parseInt(m.group(2));
                    int dayOfMonth = Integer.parseInt(m.group(3));
                    cal.set(Calendar.YEAR, year);
                    cal.set(Calendar.MONTH, month - 1);
                    cal.set(Calendar.DAY_OF_MONTH, dayOfMonth);
                    LocalDateTime tm;
                    try {
                        tm = LocalDateTime.of(year, month, dayOfMonth, 0, 0);
                    } catch (DateTimeException e) {
                        break date_time_block;
                    }
                    cobLocalTm = tm;
                }
            } else {
                System.err.println("Warning: COB_DATE format invalid, ignored.");
            }
        }

        s = CobolUtil.getEnv("COB_VERBOSE");
        if (s != null && s.length() > 0 && (s.charAt(0) == 'y' || s.charAt(0) == 'Y')) {
            CobolUtil.cob_verbose = true;
        }

        s = CobolUtil.getEnv("COB_IO_ASSUME_REWRITE");
        if (s != null && s.length() > 0 && (s.charAt(0) == 'y' || s.charAt(0) == 'Y')) {
            CobolUtil.cob_io_assume_rewrite = true;
        }

        s = CobolUtil.getEnv("COB_NIBBLE_C_UNSIGNED");
        if (s != null && s.length() > 0 && (s.charAt(0) == 'y' || s.charAt(0) == 'Y')) {
            CobolUtil.nibbleCForUnsigned = true;
        }

        s = System.getenv("COB_FILE_SEQ_WRITE_BUFFER_SIZE");
        if (s != null) {
            int size = Integer.parseInt(s);
            if (size >= 0) {
                CobolUtil.fileSeqWriteBufferSize = size;
            }
        }

        s = System.getenv("COB_TERMINAL_ENCODING");
        CobolUtil.terminalEncoding = CobolEncoding.SHIFT_JIS;
        if (s != null) {
            Pattern p = Pattern.compile("[uU][tT][fF][_-]?8");
            Matcher m = p.matcher(s);
            if (m.matches()) {
                CobolUtil.terminalEncoding = CobolEncoding.UTF8;
            }
        }
    }

    // libcob/common.cとcob_localtime
    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     */
    public static LocalDateTime localtime() {
        LocalDateTime rt = LocalDateTime.now();
        if (CobolUtil.cobLocalTm != null) {
            CobolUtil.cobLocalTm =
                    CobolUtil.cobLocalTm
                            .withHour(rt.getHour())
                            .withMinute(rt.getMinute())
                            .withSecond(rt.getSecond());
            rt = CobolUtil.cobLocalTm;
        }
        return rt;
    }

    // libcob/cob_verbose_outputの実装
    /**
     * TODO: 準備中
     *
     * @param s TODO cob_verboseの初期化
     */
    public static void verboseOutput(String s) {
        if (cob_verbose) {
            System.out.println("libcobj: " + s);
        }
    }

    // libcob/fileio.cのcob_rintime_errorの実装
    /**
     * TODO: 準備中
     *
     * @param s TODO: 準備中
     */
    public static void runtimeError(String s) {
        if (hdlrs != null) {
            HandlerList h = hdlrs;
            if (runtime_err_str != null) {
                if (sourceFile != null) {
                    runtime_err_str = String.format("%s:%d: ", sourceFile, sourceLine);
                }
            }
            while (h != null) {
                if (runtime_err_str != null) {
                    h.proc(runtime_err_str);
                } else {
                    h.proc("Malloc error");
                }
                h = h.next;
            }
            hdlrs = null;
        }

        if (sourceFile != null) {
            System.err.print(String.format("%s:%d: ", sourceFile, sourceLine));
        }
        byte[] messageBytes = ("libcobj: " + s).getBytes(AbstractCobolField.charSetSJIS);
        System.err.write(messageBytes, 0, messageBytes.length);
        System.err.println();
        System.err.flush();
    }

    // libcob/common.c cob_get_environment
    /**
     * TODO: 準備中
     *
     * @param envname TODO: 準備中
     * @param envval TODO: 準備中
     */
    public static void getEnvironment(AbstractCobolField envname, AbstractCobolField envval) {
        String p = CobolUtil.getEnv(envname.fieldToString());
        if (p == null) {
            CobolExceptionInfo.setException(CobolExceptionId.COB_EC_IMP_ACCEPT);
            p = " ";
        }
        envval.memcpy(p);
    }

    // libcob/common.cのCOB_CHK_PARMSの実装
    /**
     * TODO: 準備中
     *
     * @param funcName TODO: 準備中
     * @param numParams TODO: 準備中
     */
    public static void COB_CHK_PARMS(String funcName, int numParams) {}

    // libcob/common.cのcob_get_switchの実装
    /**
     * TODO: 準備中
     *
     * @param n TODO: 準備中
     * @return TODO: 準備中
     */
    public static boolean getSwitch(int n) {
        return CobolUtil.cobSwitch[n];
    }

    // libcob/common.cのcob_set_switchの実装
    /**
     * TODO: 準備中
     *
     * @param n TODO: 準備中
     * @param flag TODO: 準備中
     */
    public static void setSwitch(int n, int flag) {
        if (flag == 0) {
            CobolUtil.cobSwitch[n] = false;
        } else if (flag == 1) {
            CobolUtil.cobSwitch[n] = true;
        }
    }

    // libcob/common.cのalnum_cmpsの実装
    /**
     * TODO: 準備中
     *
     * @param s1 TODO: 準備中
     * @param s2 TODO: 準備中
     * @param size TODO: 準備中
     * @param col TODO: 準備中
     * @return TODO: 準備中
     */
    public static int alnumCmps(
            CobolDataStorage s1, CobolDataStorage s2, int size, CobolDataStorage col) {
        if (col != null) {
            for (int i = 0; i < size; ++i) {
                int ret =
                        col.getByte(Byte.toUnsignedInt(s1.getByte(i)))
                                - col.getByte(Byte.toUnsignedInt(s2.getByte(i)));
                if (ret != 0) {
                    return ret;
                }
            }
        } else {
            for (int i = 0; i < size; ++i) {
                int ret = Byte.toUnsignedInt(s1.getByte(i)) - Byte.toUnsignedInt(s2.getByte(i));
                if (ret != 0) {
                    return ret;
                }
            }
        }
        return 0;
    }

    // libcob/common.cのnational_cmpsの実装
    /**
     * TODO: 準備中
     *
     * @param s1 TODO: 準備中
     * @param s2 TODO: 準備中
     * @param size TODO: 準備中
     * @param col TODO: 準備中
     * @return TODO: 準備中
     */
    public static int nationalCmps(
            CobolDataStorage s1, CobolDataStorage s2, int size, CobolDataStorage col) {
        int ret = 0;
        for (int i = 0; i < size && ret == 0; i += 2) {
            int b11 = Byte.toUnsignedInt(s1.getByte(i));
            int b12 = Byte.toUnsignedInt(s1.getByte(i + 1));
            int b21 = Byte.toUnsignedInt(s2.getByte(i));
            int b22 = Byte.toUnsignedInt(s2.getByte(i + 1));
            ret = ((b11 << 8) | b12) - ((b21 << 8) | b22);
        }
        return ret;
    }

    /** TODO: 準備中 */
    public static void readyTrace() {
        CobolUtil.lineTrace = true;
    }

    /** TODO: 準備中 */
    public static void resetTrace() {
        CobolUtil.lineTrace = false;
    }

    /**
     * TODO: 準備中
     *
     * @param fatalError TODO: 準備中
     * @throws CobolStopRunException TODO: 準備中
     */
    public static void fatalError(int fatalError) throws CobolStopRunException {
        switch (fatalError) {
            case CobolUtil.FERROR_INITIALIZED:
                CobolUtil.runtimeError("cob_init() has not been called");
                break;
            case CobolUtil.FERROR_CODEGEN:
                CobolUtil.runtimeError("Codegen error - Please report this");
                break;
            case CobolUtil.FERROR_CHAINING:
                CobolUtil.runtimeError("ERROR - Recursive call of chained program");
                break;
            case CobolUtil.FERROR_STACK:
                CobolUtil.runtimeError("Stack overflow, possible PERFORM depth exceeded");
                break;
            default:
                CobolUtil.runtimeError(String.format("Unknown failure : %d", fatalError));
                break;
        }
        CobolStopRunException.stopRunAndThrow(1);
    }

    /**
     * TODO: 準備中
     *
     * @param progId TODO: 準備中
     * @param sfile TODO: 準備中
     * @param sline TODO: 準備中
     * @param csect TODO: 準備中
     * @param cpara TODO: 準備中
     * @param cstatement TODO: 準備中
     */
    public static void setLocation(
            String progId, String sfile, int sline, String csect, String cpara, String cstatement) {
        CobolUtil.sourceFile = sfile;
        CobolUtil.sourceLine = sline;
        currProgramId = progId;
        currSection = csect;
        currParagraph = cpara;
        sourceLine = sline;
        if (cstatement != null) {
            sourceStatement = cstatement;
        }
        if (CobolUtil.lineTrace) {
            System.err.println(
                    String.format(
                            "PROGRAM-ID: %s \tLine: %d \tStatement: %s",
                            progId, sline, cstatement == null ? "Unknown" : cstatement));
            System.err.flush();
        }
    }

    /**
     * TODO: 準備中
     *
     * @param envVarName TODO: 準備中
     * @return TODO: 準備中
     */
    public static String getEnv(String envVarName) {
        String envVarInTable = CobolUtil.envVarTable.getProperty(envVarName);
        if (envVarInTable != null) {
            return envVarInTable;
        } else {
            return System.getenv(envVarName);
        }
    }

    /**
     * get environemnt variable
     *
     * @param envVarName the name of an environment variable.
     * @param envVarValue the value to be set to the environment variable.
     */
    public static void setEnv(String envVarName, String envVarValue) {
        CobolUtil.envVarTable.setProperty(envVarName, envVarValue);
    }

    /**
     * Set environemnt variable
     *
     * @param envVarName the name of an environment variable. The leading and trailing spaces are
     *     ignored.
     * @param envVarValue the value of an environment variable to be set.
     */
    public static void setEnv(AbstractCobolField envVarName, AbstractCobolField envVarValue) {
        CobolUtil.envVarTable.setProperty(envVarName.getString().trim(), envVarValue.getString());
    }

    /**
     * TODO: 準備中
     *
     * @param s TODO: 準備中
     * @return TODO: 準備中
     */
    public static byte[] stringToBytes(String s) {
        return s.getBytes(AbstractCobolField.charSetSJIS);
    }

    /**
     * TODO: 準備中
     *
     * @param bytes TODO: 準備中
     * @return TODO: 準備中
     */
    public static byte[] toBytes(byte... bytes) {
        return bytes;
    }

    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     */
    public static String getCurrProgramId() {
        return currProgramId;
    }

    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     */
    public static String getCurrSection() {
        return currSection;
    }

    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     */
    public static String getCurrParagraph() {
        return currParagraph;
    }

    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     */
    public static int getSourceLine() {
        return sourceLine;
    }

    /**
     * TODO: 準備中
     *
     * @return TODO: 準備中
     */
    public static String getSourceStatement() {
        return sourceStatement;
    }
}
