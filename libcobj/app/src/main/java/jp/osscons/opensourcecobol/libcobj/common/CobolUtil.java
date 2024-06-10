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

import java.io.UnsupportedEncodingException;
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

public class CobolUtil {
  private static boolean cob_io_assume_rewrite = false;
  private static boolean cob_verbose = false;
  private static HandlerList hdlrs = null;
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
  public static boolean cobErrorOnExitFlag = false;
  public static Calendar cal;

  public static int fileSeqWriteBufferSize = 10;

  private static boolean lineTrace = false;

  public static String sourceFile;
  public static int sourceLine;
  public static String currProgramId;
  public static String currSection;
  public static String currParagraph;
  public static String sourceStatement;

  abstract static class HandlerList {
    public HandlerList next = null;

    public abstract int proc(String s);
  }

  public static final int FERROR_INITIALIZED = 0;
  public static final int FERROR_CODEGEN = 1;
  public static final int FERROR_CHAINING = 2;
  public static final int FERROR_STACK = 3;

  private static Properties envVarTable = new Properties();

  /**
   * libcob/common.cのcob_check_envの実装
   *
   * @param name TODO: 調査中
   * @param value TODO: 調査中
   * @return TODO: 調査中
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

  public static boolean cob_io_rewwrite_assumed() {
    return cob_io_assume_rewrite;
  }

  public static void cobCheckRefModNational(int offset, long length, int size, byte[] name)
      throws CobolStopRunException {
    CobolUtil.cobCheckRefMod((offset + 1) / 2, length / 2, size / 2, name);
  }

  public static void cobCheckRefModNational(int offset, long length, int size, String name)
      throws CobolStopRunException {
    CobolUtil.cobCheckRefMod((offset + 1) / 2, length / 2, size / 2, name);
  }

  public static void cobCheckRefMod(
      int offset, long length, int size, CobolDataStorage name, int nameLen)
      throws CobolStopRunException {
    cobCheckRefMod(offset, length, size, name.getByteArrayRef(0, nameLen));
  }

  public static void cobCheckRefMod(int offset, long length, int size, byte[] name, int nameLen)
      throws CobolStopRunException {
    cobCheckRefMod(offset, length, size, name);
  }

  public static void cobCheckRefMod(int offset, long length, int size, byte[] name)
      throws CobolStopRunException {
    try {
      CobolUtil.cobCheckRefMod(offset, length, size, new String(name, "Shift_JIS"));
    } catch (UnsupportedEncodingException e) {
      CobolUtil.cobCheckRefMod(offset, length, size, "");
    }
  }

  public static void cobCheckRefMod(int offset, long length, int size, String name, int nameLen)
      throws CobolStopRunException {
    cobCheckRefMod(offset, length, size, name);
  }

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

  public static void cobCheckBased(CobolDataStorage x, byte[] name) throws CobolStopRunException {
    if (x == null) {
      CobolUtil.runtimeError(String.format("BASED/LINKAGE item '%s' has NULL address", name));
      CobolStopRunException.stopRunAndThrow(1);
    }
  }

  /**
   * libcob/common.cのcob_initの実装
   *
   * @param argv TODO: 調査中
   * @param cobInitialized TODO: 調査中
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
  }

  /**
   * libcob/common.cとcob_localtime
   *
   * @return TODO: 調査中
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

  /**
   * libcob/cob_verbose_outputの実装 opensourceCOBOLではprintfのように可変長引数を取るが,
   * こちらは呼び出し側で事前にString.format等を使用することを期待している.
   *
   * @param s TODO cob_verboseの初期化
   */
  public static void verboseOutput(String s) {
    if (cob_verbose) {
      System.out.println("libcobj: " + s);
    }
  }

  /**
   * libcob/fileio.cのcob_rintime_errorの実装 opensourceCOBOLではprintfのように可変長引数を取るが,
   * こちらは呼び出し側で事前にString.format等を使用することを期待している.
   *
   * @param s TODO: 調査中
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

  /**
   * libcob/common.c cob_get_environment
   *
   * @param envname TODO: 調査中
   * @param envval TODO: 調査中
   */
  public static void getEnvironment(AbstractCobolField envname, AbstractCobolField envval) {
    String p = CobolUtil.getEnv(envname.fieldToString());
    if (p == null) {
      CobolExceptionInfo.setException(CobolExceptionId.COB_EC_IMP_ACCEPT);
      p = " ";
    }
    envval.memcpy(p);
  }

  /**
   * libcob/common.cのCOB_CHK_PARMSの実装
   *
   * @param funcName TODO: 調査中
   * @param numParams TODO: 調査中
   */
  public static void COB_CHK_PARMS(String funcName, int numParams) {}

  /**
   * libcob/common.cのcob_get_switchの実装
   *
   * @param n TODO: 調査中
   * @return TODO: 調査中
   */
  public static boolean getSwitch(int n) {
    return CobolUtil.cobSwitch[n];
  }

  /**
   * libcob/common.cのcob_set_switchの実装
   *
   * @param n TODO: 調査中
   * @param flag TODO: 調査中
   */
  public static void setSwitch(int n, int flag) {
    if (flag == 0) {
      CobolUtil.cobSwitch[n] = false;
    } else if (flag == 1) {
      CobolUtil.cobSwitch[n] = true;
    }
  }

  /**
   * libcob/common.cのcob_get_sign_asciiの実装
   *
   * @param p TODO: 調査中
   */
  public static void getSignAscii(CobolDataStorage p) {
    switch (p.getByte(0)) {
      case 'p':
        p.setByte(0, (byte) '0');
        return;
      case 'q':
        p.setByte(0, (byte) '1');
        return;
      case 'r':
        p.setByte(0, (byte) '2');
        return;
      case 's':
        p.setByte(0, (byte) '3');
        return;
      case 't':
        p.setByte(0, (byte) '4');
        return;
      case 'u':
        p.setByte(0, (byte) '5');
        return;
      case 'v':
        p.setByte(0, (byte) '6');
        return;
      case 'w':
        p.setByte(0, (byte) '7');
        return;
      case 'x':
        p.setByte(0, (byte) '8');
        return;
      case 'y':
        p.setByte(0, (byte) '9');
        return;
      default:
        return;
    }
  }

  /**
   * libcob/common.cのcob_get_sign_ebcdicの実装
   *
   * @param p TODO: 調査中
   * @return TODO: 調査中
   */
  public static int getSignEbcdic(CobolDataStorage p) {
    switch (p.getByte(0)) {
      case '{':
        p.setByte(0, (byte) '0');
        return 1;
      case 'A':
        p.setByte(0, (byte) '1');
        return 1;
      case 'B':
        p.setByte(0, (byte) '2');
        return 1;
      case 'C':
        p.setByte(0, (byte) '3');
        return 1;
      case 'D':
        p.setByte(0, (byte) '4');
        return 1;
      case 'E':
        p.setByte(0, (byte) '5');
        return 1;
      case 'F':
        p.setByte(0, (byte) '6');
        return 1;
      case 'G':
        p.setByte(0, (byte) '7');
        return 1;
      case 'H':
        p.setByte(0, (byte) '8');
        return 1;
      case 'I':
        p.setByte(0, (byte) '9');
        return 1;
      case '}':
        p.setByte(0, (byte) '0');
        return -1;
      case 'J':
        p.setByte(0, (byte) '1');
        return -1;
      case 'K':
        p.setByte(0, (byte) '2');
        return -1;
      case 'L':
        p.setByte(0, (byte) '3');
        return -1;
      case 'M':
        p.setByte(0, (byte) '4');
        return -1;
      case 'N':
        p.setByte(0, (byte) '5');
        return -1;
      case 'O':
        p.setByte(0, (byte) '6');
        return -1;
      case 'P':
        p.setByte(0, (byte) '7');
        return -1;
      case 'Q':
        p.setByte(0, (byte) '8');
        return -1;
      case 'R':
        p.setByte(0, (byte) '9');
        return -1;
      default:
        /* What to do here */
        p.setByte(0, (byte) '0');
        return 1;
    }
  }

  /**
   * libcob/common.cのcob_put_sign_asciiの実装
   *
   * @param p TODO: 調査中
   */
  public static void putSignAscii(CobolDataStorage p) {
    switch (p.getByte(0)) {
      case '0':
        p.setByte(0, (byte) 'p');
        return;
      case '1':
        p.setByte(0, (byte) 'q');
        return;
      case '2':
        p.setByte(0, (byte) 'r');
        return;
      case '3':
        p.setByte(0, (byte) 's');
        return;
      case '4':
        p.setByte(0, (byte) 't');
        return;
      case '5':
        p.setByte(0, (byte) 'u');
        return;
      case '6':
        p.setByte(0, (byte) 'v');
        return;
      case '7':
        p.setByte(0, (byte) 'w');
        return;
      case '8':
        p.setByte(0, (byte) 'x');
        return;
      case '9':
        p.setByte(0, (byte) 'v');
        return;
      default:
        return;
    }
  }

  /**
   * libcob/common.cのcob_put_sign_ebcdicの実装
   *
   * @param p TODO: 調査中
   * @param sign TODO: 調査中
   */
  public static void putSignEbcdic(CobolDataStorage p, int sign) {
    if (sign < 0) {
      switch (p.getByte(0)) {
        case '0':
          p.setByte(0, (byte) '}');
          return;
        case '1':
          p.setByte(0, (byte) 'J');
          return;
        case '2':
          p.setByte(0, (byte) 'K');
          return;
        case '3':
          p.setByte(0, (byte) 'L');
          return;
        case '4':
          p.setByte(0, (byte) 'M');
          return;
        case '5':
          p.setByte(0, (byte) 'N');
          return;
        case '6':
          p.setByte(0, (byte) 'O');
          return;
        case '7':
          p.setByte(0, (byte) 'P');
          return;
        case '8':
          p.setByte(0, (byte) 'Q');
          return;
        case '9':
          p.setByte(0, (byte) 'R');
          return;
        default:
          /* What to do here */
          p.setByte(0, (byte) '}');
          return;
      }
    }
    switch (p.getByte(0)) {
      case '0':
        p.setByte(0, (byte) '{');
        return;
      case '1':
        p.setByte(0, (byte) 'A');
        return;
      case '2':
        p.setByte(0, (byte) 'B');
        return;
      case '3':
        p.setByte(0, (byte) 'C');
        return;
      case '4':
        p.setByte(0, (byte) 'D');
        return;
      case '5':
        p.setByte(0, (byte) 'E');
        return;
      case '6':
        p.setByte(0, (byte) 'F');
        return;
      case '7':
        p.setByte(0, (byte) 'G');
        return;
      case '8':
        p.setByte(0, (byte) 'H');
        return;
      case '9':
        p.setByte(0, (byte) 'I');
        return;
      default:
        /* What to do here */
        p.setByte(0, (byte) '{');
        return;
    }
  }

  /**
   * libcob/common.cのcommon_compcの実装
   *
   * @param s1 TODO: 調査中
   * @param c TODO: 調査中
   * @param size TODO: 調査中
   * @return TODO: 調査中
   */
  public static int commonCmpc(CobolDataStorage s1, byte c, int size) {
    CobolDataStorage s = CobolModule.getCurrentModule().collating_sequence;
    int uc = c & 0xFF;
    if (s != null) {
      for (int i = 0; i < size; ++i) {
        // int ret = s.getByte((s1.getByte(i) & 0xFF) - (s.getByte(uc) & 0xFF));
        int ret = (s.getByte(s1.getByte(i) & 0xFF) & 0xFF) - (s.getByte(uc) & 0xFF);
        if (ret != 0) {
          return ret;
        }
      }
    } else {
      for (int i = 0; i < size; ++i) {
        int ret = (s1.getByte(i) & 0xFF) - uc;
        if (ret != 0) {
          return ret;
        }
      }
    }
    return 0;
  }

  /**
   * libcob/common.cのis_national_paddingの実装
   *
   * @param offset TODO: 調査中
   * @param s TODO: 調査中
   * @param size TODO: 調査中
   * @return TODO: 調査中
   */
  public static int isNationalPadding(int offset, CobolDataStorage s, int size) {
    int ret = 1;
    int i = 0;
    while (i < size && ret != 0) {
      if (s.getByte(offset + i) == ' ') {
        i++;
      } else if (size - i >= CobolConstant.ZENCSIZ) {
        for (int j = 0; j < CobolConstant.ZENCSIZ; ++j) {
          if (s.getByte(offset + i + j) != CobolConstant.ZENSPC[j]) {
            return 0;
          }
        }
        i += CobolConstant.ZENCSIZ;
      } else {
        ret = 0;
      }
    }
    return ret;
  }

  /**
   * libcob/common.cのalnum_cmpsの実装
   *
   * @param s1 TODO: 調査中
   * @param s2 TODO: 調査中
   * @param size TODO: 調査中
   * @param col TODO: 調査中
   * @return TODO: 調査中
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

  /**
   * libcob/common.cのnational_cmpsの実装
   *
   * @param s1 TODO: 調査中
   * @param s2 TODO: 調査中
   * @param size TODO: 調査中
   * @param col TODO: 調査中
   * @return TODO: 調査中
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

  public static void readyTrace() {
    CobolUtil.lineTrace = true;
  }

  public static void resetTrace() {
    CobolUtil.lineTrace = false;
  }

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

  public static byte[] stringToBytes(String s) {
    try {
      return s.getBytes("Shift_JIS");
    } catch (UnsupportedEncodingException e) {
      return null;
    }
  }

  public static byte[] toBytes(byte... bytes) {
    return bytes;
  }

  public static String getCurrProgramId() {
    return currProgramId;
  }

  public static String getCurrSection() {
    return currSection;
  }

  public static String getCurrParagraph() {
    return currParagraph;
  }

  public static int getSourceLine() {
    return sourceLine;
  }

  public static String getSourceStatement() {
    return sourceStatement;
  }
}
