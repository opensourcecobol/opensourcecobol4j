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
package jp.osscons.opensourcecobol.libcobj.file;

import java.io.IOException;
import java.nio.channels.ClosedChannelException;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.channels.NonWritableChannelException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import jp.osscons.opensourcecobol.libcobj.common.CobolModule;
import jp.osscons.opensourcecobol.libcobj.common.CobolUtil;
import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolExceptionId;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolRuntimeException;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolStopRunException;

public class CobolFile {
  protected static final int COB_ORG_SEQUENTIAL = 0;
  protected static final int COB_ORG_LINE_SEQUENTIAL = 1;
  protected static final int COB_ORG_RELATIVE = 2;
  protected static final int COB_ORG_INDEXED = 3;
  protected static final int COB_ORG_SORT = 4;
  protected static final int COB_ORG_MAX = 5;

  protected static final int COB_ACCESS_SEQUENTIAL = 1;
  protected static final int COB_ACCESS_DYNAMIC = 2;
  protected static final int COB_ACCESS_RANDOM = 3;

  protected static final int COB_IO_OPEN = 0;
  protected static final int COB_IO_READ = 1;
  protected static final int COB_IO_WRITE = 2;
  protected static final int COB_IO_CLOSE = 3;
  protected static final int COB_IO_DELETE = 4;
  protected static final int COB_IO_REWRITE = 5;
  protected static final int COB_IO_START = 6;
  protected static final int COB_IO_COMMIT = 7;
  protected static final int COB_IO_ROLLBACK = 8;
  protected static final int COB_IO_UNLOCK = 9;
  protected static final int COB_IO_DELETE_FILE = 10;

  protected static final int COB_OPEN_CLOSED = 0;
  protected static final int COB_OPEN_INPUT = 1;
  protected static final int COB_OPEN_OUTPUT = 2;
  protected static final int COB_OPEN_I_O = 3;
  protected static final int COB_OPEN_EXTEND = 4;
  protected static final int COB_OPEN_LOCKED = 5;

  protected static final int COB_CLOSE_NORMAL = 0;
  protected static final int COB_CLOSE_LOCK = 1;
  protected static final int COB_CLOSE_NO_REWIND = 2;
  protected static final int COB_CLOSE_UNIT = 3;
  protected static final int COB_CLOSE_UNIT_REMOVAL = 4;

  protected static final int COB_WRITE_MASK = 0x0000ffff;
  protected static final int COB_WRITE_LINES = 0x00010000;
  protected static final int COB_WRITE_PAGE = 0x00020000;
  protected static final int COB_WRITE_CHANNEL = 0x00040000;
  protected static final int COB_WRITE_AFTER = 0x00100000;
  protected static final int COB_WRITE_BEFORE = 0x00200000;
  protected static final int COB_WRITE_EOP = 0x00400000;
  protected static final int COB_WRITE_LOCK = 0x00800000;

  protected static final int COB_READ_NEXT = 0x01;
  protected static final int COB_READ_PREVIOUS = 0x2;
  protected static final int COB_READ_FIRST = 0x04;
  protected static final int COB_READ_LAST = 0x08;
  protected static final int COB_READ_LOCK = 0x10;
  protected static final int COB_READ_NO_LOCK = 0x20;
  protected static final int COB_READ_KEPT_LOCK = 0x40;
  protected static final int COB_READ_WAIT_LOCK = 0x80;
  protected static final int COB_READ_IGNORE_LOCK = 0x100;

  protected static final String TIS_DEFINE_USERFH = "OC_USERFH";
  protected static final String COB_IO_CREATES = "OC_IO_CREATES";
  protected static final String COB_EXTEND_CREATES = "OC_EXTEND_CREATES";

  protected static final int COB_STATUS_00_SUCCESS = 0;
  protected static final int COB_STATUS_02_SUCCESS_DUPLICATE = 2;
  protected static final int COB_STATUS_04_SUCCESS_INCOMPLETE = 4;
  protected static final int COB_STATUS_05_SUCCESS_OPTIONAL = 5;
  protected static final int COB_STATUS_07_SUCCESS_NO_UNIT = 7;
  protected static final int COB_STATUS_10_END_OF_FILE = 10;
  protected static final int COB_STATUS_14_OUT_OF_KEY_RANGE = 14;
  protected static final int COB_STATUS_21_KEY_INVALID = 21;
  protected static final int COB_STATUS_22_KEY_EXISTS = 22;
  protected static final int COB_STATUS_23_KEY_NOT_EXISTS = 23;
  protected static final int COB_STATUS_30_PERMANENT_ERROR = 30;
  protected static final int COB_STATUS_31_INCONSISTENT_FILENAME = 31;
  protected static final int COB_STATUS_34_BOUNDARY_VIOLATION = 34;
  protected static final int COB_STATUS_35_NOT_EXISTS = 35;
  protected static final int COB_STATUS_37_PERMISSION_DENIED = 37;
  protected static final int COB_STATUS_38_CLOSED_WITH_LOCK = 38;
  protected static final int COB_STATUS_39_CONFLICT_ATTRIBUTE = 39;
  protected static final int COB_STATUS_41_ALREADY_OPEN = 41;
  protected static final int COB_STATUS_42_NOT_OPEN = 42;
  protected static final int COB_STATUS_43_READ_NOT_DONE = 43;
  protected static final int COB_STATUS_44_RECORD_OVERFLOW = 44;
  protected static final int COB_STATUS_46_READ_ERROR = 46;
  protected static final int COB_STATUS_47_INPUT_DENIED = 47;
  protected static final int COB_STATUS_48_OUTPUT_DENIED = 48;
  protected static final int COB_STATUS_49_I_O_DENIED = 49;
  protected static final int COB_STATUS_51_RECORD_LOCKED = 51;
  protected static final int COB_STATUS_52_EOP = 52;
  protected static final int COB_STATUS_57_I_O_LINAGE = 57;
  protected static final int COB_STATUS_61_FILE_SHARING = 61;
  protected static final int COB_STATUS_91_NOT_AVAILABLE = 91;

  protected static final int COB_LINAGE_INVALID = 16384;
  protected static final int COB_NOT_CONFIGURED = 32768;

  public static final int COB_SELECT_FILE_STATUS = 0x01;
  public static final int COB_SELECT_EXTERNAL = 0x02;
  public static final int COB_SELECT_LINAGE = 0x04;
  public static final int COB_SELECT_SPLITKEY = 0x08;

  protected static final int FNSTATUSSIZE = 3;

  protected static final int ENOENT = 2;
  protected static final int EBADF = 9;
  protected static final int EACCESS = 13;
  protected static final int EISDIR = 21;
  protected static final int EROFS = 30;
  protected static final int EAGAIN = 11;

  public static CobolFile errorFile;

  protected static int COB_SMALL_BUFF = 1024;
  protected static int COB_SMALL_MAX = COB_SMALL_BUFF - 1;

  protected static final int COB_LOCK_EXCLUSIVE = 1;
  protected static final int COB_LOCK_MANUAL = 2;
  protected static final int COB_LOCK_AUTOMATIC = 3;
  protected static final int COB_LOCK_MULTIPLE = 1;
  protected static final int COB_LOCK_MASK = 0x7;

  protected static String cob_file_path = null;
  protected static String cob_ls_nulls = null;
  protected static String cob_ls_fixed = null;
  protected static byte[] file_open_env = new byte[1024];
  protected static String file_open_name;
  protected static byte[] file_open_buff = new byte[1024];

  protected static final String[] prefix = {"DD_", "dd_", ""};
  protected static final int NUM_PREFIX = prefix.length;

  protected static int eop_status = 0;
  protected static int cob_do_sync = 0;

  private static List<CobolFile> file_cache = new ArrayList<CobolFile>();

  protected static int[] status_exception = {
    0,
    CobolExceptionId.COB_EC_I_O_AT_END,
    CobolExceptionId.COB_EC_I_O_INVALID_KEY,
    CobolExceptionId.COB_EC_I_O_PERMANENT_ERROR,
    CobolExceptionId.COB_EC_I_O_LOGIC_ERROR,
    CobolExceptionId.COB_EC_I_O_RECORD_OPERATION,
    CobolExceptionId.COB_EC_I_O_FILE_SHARING,
    CobolExceptionId.COB_EC_I_O,
    CobolExceptionId.COB_EC_I_O,
    CobolExceptionId.COB_EC_I_O_IMP
  };
  public static String select_name;
  public static byte[] file_status;
  protected AbstractCobolField assign;
  protected AbstractCobolField record;
  protected AbstractCobolField record_size;
  protected CobolFileKey[] keys;
  public FileIO file;
  protected CobolSort filex;
  protected IndexedFile filei;
  protected Linage linorkeyptr;
  protected CobolDataStorage sort_collating;
  protected Object extfh_ptr;
  protected int record_min;
  protected int record_max;
  protected int nkeys;

  protected char organization;
  protected char access_mode;
  protected char lock_mode;
  protected char open_mode;
  protected boolean flag_optional;
  public char last_open_mode;
  protected char special;
  protected boolean flag_nonexistent;

  protected boolean flag_end_of_file;
  protected boolean flag_begin_of_file;
  protected char flag_first_read;
  protected boolean flag_read_done;
  public char flag_select_features;
  protected boolean flag_needs_nl;
  protected boolean flag_needs_top;
  protected char file_version;

  protected static String runtime_buffer;

  public Linage getLinorkeyptr() {
    return this.linorkeyptr;
  }

  public void setLinorkeyptr(Linage ptr) {
    this.linorkeyptr = ptr;
  }

  public CobolFile() {}

  public CobolFile(
      String selectName,
      byte[] fileStatus,
      AbstractCobolField assign,
      AbstractCobolField record,
      AbstractCobolField recordSize,
      int recordMin,
      int recordMax,
      int nkeys,
      CobolFileKey[] keys,
      char organization,
      char accessMode,
      char lockMode,
      char openMode,
      boolean flagOptional,
      char lastOpenMode,
      char special,
      boolean flagNonexistent,
      boolean flagEndOfFile,
      boolean flagBeginOfFile,
      char flagFirstRead,
      boolean flagReadDone,
      char flagSelectFeatures,
      boolean flagNeedsNl,
      boolean flagNeedsTop,
      char fileVersion) {

    select_name = selectName;
    file_status = fileStatus;
    this.assign = assign;
    this.record = record;
    this.record_size = recordSize;
    this.record_min = recordMin;
    this.record_max = recordMax;
    this.nkeys = nkeys;
    this.keys = keys;
    this.file = new FileIO();
    this.organization = organization;
    this.access_mode = accessMode;
    this.lock_mode = lockMode;
    this.open_mode = openMode;
    this.flag_optional = flagOptional;
    this.special = special;
    this.flag_nonexistent = flagNonexistent;
    this.flag_end_of_file = flagEndOfFile;
    this.flag_begin_of_file = flagBeginOfFile;
    this.flag_first_read = flagFirstRead;
    this.flag_read_done = flagReadDone;
    this.flag_select_features = flagSelectFeatures;
    this.flag_needs_nl = flagNeedsNl;
    this.flag_needs_top = flagNeedsTop;
    this.file_version = fileVersion;
  }

  /**
   * libcob/fileio.cのsave_statusの実装 RETURN_STATUSマクロは実装できないため,本メソッドの呼び出し後の次の文はreturn;を書くこと.
   *
   * @param status
   * @param fnstatus
   * @return
   */
  protected void saveStatus(int status, AbstractCobolField fnstatus) {
    CobolFile.errorFile = this;
    if (status == 0) {
      file_status[0] = '0';
      file_status[1] = '0';
      if (fnstatus != null) {
        fnstatus.getDataStorage().setByte(0, (byte) '0');
        fnstatus.getDataStorage().setByte(1, (byte) '0');
      }
      CobolRuntimeException.code = 0;
      return;
    }

    if (status != COB_STATUS_52_EOP) {
      CobolRuntimeException.setException(status_exception[status / 10]);
    }
    file_status[0] = (byte) (status / 10 + '0');
    file_status[1] = (byte) (status % 10 + '0');
    if (fnstatus != null) {
      fnstatus.getDataStorage().setByte(0, file_status[0]);
      fnstatus.getDataStorage().setByte(1, file_status[1]);
    }
  }

  /** libcob/fileio.のcob_invoke_funの実装 */
  public static int invokeFun(
      int operate,
      Object f,
      AbstractCobolField key,
      CobolDataStorage rec,
      AbstractCobolField fnstatus,
      String openMode,
      String startCond,
      String readOpts) {
    return 0;
  }

  /**
   * libcob/cob_cache_fileのj実装
   *
   * @param f
   */
  protected static void cacheFile(CobolFile f) {
    if (file_cache.contains(f)) {
      return;
    }
    file_cache.add(f);
  }

  /**
   * libcob/fileio.cのcob_file_linage_checkの実装 TODO 実装
   *
   * @return
   */
  protected boolean file_linage_check() {
    Linage lingptr = getLinorkeyptr();
    lingptr.setLinLines(lingptr.getLinage().getInt());

    outer:
    {
      if (lingptr.getLinLines() < 1) {
        break outer;
      }
      if (lingptr.getLatfoot() != null) {
        lingptr.setLinFoot(lingptr.getLatfoot().getInt());
        if (lingptr.getLinFoot() < 1 || lingptr.getLinFoot() > lingptr.getLinLines()) {
          break outer;
        }
      } else {
        lingptr.setLinFoot(0);
      }
      if (lingptr.getLattop() != null) {
        lingptr.setLinTop(lingptr.getLattop().getInt());
        if (lingptr.getLinTop() < 0) {
          break outer;
        }
      } else {
        lingptr.setLinTop(0);
      }
      if (lingptr.getLatbot() != null) {
        lingptr.setLinBot(lingptr.getLatbot().getInt());
        if (lingptr.getLinBot() < 0) {
          break outer;
        }
      } else {
        lingptr.setLinBot(0);
      }
      return false;
    }

    lingptr.getLinageCtr().setInt(0);
    return true;
  }

  /**
   * libcob/fileio.cのcob_linage_write_optの実装 TODO 実装
   *
   * @param opt
   * @return
   * @throws CobolStopRunException
   */
  protected int linage_write_opt(int opt) throws CobolStopRunException {
    int i, n;
    Linage lingptr = this.getLinorkeyptr();

    if ((opt & COB_WRITE_PAGE) != 0) {
      i = lingptr.getLinageCtr().getInt();
      if (i == 0) {
        return COB_STATUS_57_I_O_LINAGE;
      }
      n = lingptr.getLinLines();
      for (; i < n; ++i) {
        this.file.putc((byte) '\n');
      }
      for (i = 0; i < lingptr.getLinBot(); i++) {
        this.file.putc((byte) '\n');
      }
      if (this.file_linage_check()) {
        return COB_STATUS_57_I_O_LINAGE;
      }
      for (i = 0; i < lingptr.getLinTop(); i++) {
        this.file.putc((byte) '\n');
      }
      lingptr.getLinageCtr().setInt(1);
    } else if ((opt & COB_WRITE_LINES) != 0) {
      n = lingptr.getLinageCtr().getInt();
      if (n == 0) {
        return COB_STATUS_57_I_O_LINAGE;
      }
      lingptr.getLinageCtr().addInt(opt & COB_WRITE_MASK);
      i = lingptr.getLinageCtr().getInt();
      if ((opt & COB_WRITE_EOP) != 0 && lingptr.getLinFoot() != 0) {
        if (i >= lingptr.getLinFoot()) {
          eop_status = 1;
        }
      }
      if (i > lingptr.getLinLines()) {
        if ((opt & COB_WRITE_EOP) != 0) {
          eop_status = 1;
        }
        for (; n < lingptr.getLinLines(); n++) {
          this.file.putc((byte) '\n');
        }
        for (i = 0; i < lingptr.getLinBot(); i++) {
          this.file.putc((byte) '\n');
        }
        if (this.file_linage_check()) {
          return COB_STATUS_57_I_O_LINAGE;
        }
        lingptr.getLinageCtr().setInt(1);
        for (i = 0; i < lingptr.getLinTop(); i++) {
          this.file.putc((byte) '\n');
        }
      } else {
        for (i = (opt & COB_WRITE_MASK) - 1; i > 0; i--) {
          this.file.putc((byte) '\n');
        }
      }
    }
    return 0;
  }

  protected byte[] cb_get_jisword_buff(byte[] name, byte[] jbuf, int n) {
    int cs = 0;
    int ce = name.length - 1;
    int cp;
    boolean flagQuoted = false;
    int siz = 0;
    byte[] rt = null;
    int c;
    int p = 0;

    if (name[cs] == '\'' && name[ce] == '\'') {
      cs++;
      --ce;
      flagQuoted = true;
    }

    if (ce - cs >= 5
        && !(name[cs] == '_' && name[cs + 1] == '_' && name[cs + 2] == '_')
        && !(name[ce - 2] == '_' && name[ce - 1] == '_' && name[ce] == '_')) {
      cs += 3;
      ce -= 3;
      if (!flagQuoted) {
        siz = (ce - cs + 1) / 2 + 1;
      } else {
        siz = (ce - cs + 1) / 2 + 3;
      }
      if (jbuf != null) {
        rt = new byte[siz];
      } else {
        if (siz > n) {
          c = siz - n;
          siz -= c;
          ce -= c * 2;
        }
        /*
         * TODO fix
         * for (int i = 0; i < n; ++i) {
         * jbuf[i] = 0;
         * }
         */
        rt = jbuf;
      }
      if (flagQuoted && siz > 2) {
        rt[0] = rt[siz - 2] = (byte) '\'';
        p = 1;
      } else {
        p = 0;
      }
      for (c = 0, cp = cs; cp <= ce; cp++, p += (c == 0 ? 1 : 0)) {
        if (name[cp] >= '0' && name[cp] <= '9') {
          rt[p] |= (name[cp] - '0') << (c << 2);
        } else if (name[cp] >= 'A' && name[cp] <= 'F') {
          rt[p] |= (name[cp] - 'A' + 10) << (c << 2);
        } else {
          name[p] = '?';
          cp += c;
          c = 0;
        }
      }
    } else {
      if (jbuf != null) {
        rt = name.clone();
      } else {
        /*
         * TODO fix
         * for (int i = 0; i < n; ++i) {
         * jbuf[i] = 0;
         * }
         * for (int i = 0; i < n - 1; ++i) {
         * jbuf[i] = name[i];
         * }
         */
        rt = jbuf;
      }
    }
    return rt;
  }

  // assert 0 <= openMode < 10
  private static String openModeToString(int openMode) {
    StringBuilder sb = new StringBuilder();
    return sb.append("0").append(openMode).toString();
  }

  private static String readOptsToString(int readOpts) {
    StringBuilder sb = new StringBuilder();
    if (readOpts < 10) {
      sb.append("0");
    }
    return sb.append(readOpts).toString();
  }

  private static String concatString(String... strs) {
    StringBuilder sb = new StringBuilder();
    for (String s : strs) {
      sb.append(s);
    }
    return sb.toString();
  }

  public void open(int mode, int sharing, AbstractCobolField fnstatus) {
    String openMode = openModeToString(mode);
    if (invokeFun(COB_IO_OPEN, this, null, null, fnstatus, openMode, null, null) != 0) {
      this.last_open_mode = (char) Integer.parseInt(openMode);
      return;
    }
    this.last_open_mode = (char) Integer.parseInt(openMode);

    /* file was previously closed with lock */
    if (this.open_mode == COB_OPEN_LOCKED) {
      saveStatus(COB_STATUS_38_CLOSED_WITH_LOCK, fnstatus);
      return;
    }

    /* file is already open */
    if (((int) this.open_mode) != COB_OPEN_CLOSED) {
      saveStatus(COB_STATUS_41_ALREADY_OPEN, fnstatus);
      return;
    }

    this.last_open_mode = (char) mode;
    this.flag_nonexistent = false;
    this.flag_end_of_file = false;
    this.flag_begin_of_file = false;
    this.flag_first_read = 2;

    if (this.special != 0) {
      if (this.special == 1) {
        if (mode != COB_OPEN_INPUT) {
          saveStatus(COB_STATUS_30_PERMANENT_ERROR, fnstatus);
          return;
        }
        this.file.setIn(System.in);
        this.open_mode = (char) mode;
        this.saveStatus(COB_STATUS_00_SUCCESS, fnstatus);
        return;
      } else {
        if (mode != COB_OPEN_OUTPUT) {
          this.saveStatus(COB_STATUS_30_PERMANENT_ERROR, fnstatus);
          return;
        }
        this.file.setOut(System.out);
        this.open_mode = (char) mode;
        saveStatus(COB_STATUS_00_SUCCESS, fnstatus);
        return;
      }
    }

    /* obtain the file name */
    if (this.assign == null) {
      file_open_name = this.select_name;
    } else {
      file_open_name = this.assign.fieldToString();
    }

    byte[] src;
    byte[] dst;
    boolean simple;
    if (CobolModule.getCurrentModule().flag_filename_mapping != 0) {
      src = file_open_name.getBytes();
      dst = file_open_buff;
      simple = true;
      int srcI = 0;
      int dstI = 0;
      while (srcI < src.length) {
        char c = (char) src[srcI];
        if (!Character.isLetterOrDigit(c) && c != '_' && c != '-') {
          simple = false;
        }
        if (c == '$') {
          int i;
          for (i = 1; srcI + i < src.length; i++) {
            char d = (char) src[srcI + i];
            if (!Character.isLetterOrDigit(d) && d != '_' && c != '-') {
              break;
            }
          }
          for (int j = 0; j < i - 1; ++j) {
            file_open_env[j] = src[srcI + 1 + j];
          }
          file_open_env[i - 1] = 0;
          String p = CobolUtil.getEnv(new String(Arrays.copyOfRange(file_open_env, 0, i - 1)));
          if (p != null) {
            byte[] pbytes = p.getBytes();
            for (int j = 0; j < pbytes.length; ++j) {
              dst[dstI + j] = pbytes[j];
            }
            dstI += pbytes.length;
          }
          srcI += i;
        } else {
          dst[dstI++] = src[srcI++];
        }
      }

      file_open_name = new String(Arrays.copyOfRange(dst, 0, dstI));

      byte[] fileOpenNameBytes = file_open_name.getBytes();
      cb_get_jisword_buff(file_open_buff, fileOpenNameBytes, COB_SMALL_BUFF);

      if (simple) {
        int i;
        for (i = 0; i < NUM_PREFIX; i++) {
          byte[] fileOpenBuff = concatString(prefix[i], file_open_name).getBytes();
          String p = CobolUtil.getEnv(new String(fileOpenBuff));
          if (p != null) {
            fileOpenNameBytes = p.getBytes();
            break;
          }
        }

        if (i == NUM_PREFIX && cob_file_path != null) {
          byte[] fileOpenBuff = concatString(cob_file_path, "/", file_open_name).getBytes();
          fileOpenNameBytes = fileOpenBuff;
        }
      }

      file_open_name = new String(fileOpenNameBytes);
    }

    boolean wasNotExist = false;
    if (this.organization == COB_ORG_INDEXED) {
      if (!Files.exists(Paths.get(file_open_name))) {
        wasNotExist = true;
        if (mode != COB_OPEN_OUTPUT
            && !this.flag_optional
            && (mode != COB_OPEN_I_O || !"yes".equals(CobolUtil.getEnv(COB_IO_CREATES)))
            && (mode != COB_OPEN_EXTEND || !"yes".equals(CobolUtil.getEnv(COB_EXTEND_CREATES)))) {
          saveStatus(COB_STATUS_35_NOT_EXISTS, fnstatus);
          return;
        }
      }
    } else if (Files.notExists(Paths.get(file_open_name))) {
      wasNotExist = true;
      if (mode != COB_OPEN_OUTPUT
          && !this.flag_optional
          && (mode != COB_OPEN_I_O || CobolUtil.checkEnv(COB_IO_CREATES, "yes") == 0)
          && (mode != COB_OPEN_EXTEND || CobolUtil.checkEnv(COB_EXTEND_CREATES, "yes") == 0)) {
        saveStatus(COB_STATUS_35_NOT_EXISTS, fnstatus);
        return;
      }
    }
    cacheFile(this);

    try {
      switch (this.open_(file_open_name, mode, sharing)) {
        case 0:
          this.open_mode = (char) mode;
          if (this.flag_optional && wasNotExist) {
            saveStatus(COB_STATUS_05_SUCCESS_OPTIONAL, fnstatus);
            return;
          } else {
            saveStatus(COB_STATUS_00_SUCCESS, fnstatus);
            return;
          }
        case ENOENT:
          if (mode == COB_OPEN_EXTEND || mode == COB_OPEN_OUTPUT) {
            saveStatus(COB_STATUS_30_PERMANENT_ERROR, fnstatus);
            return;
          }
          if (this.flag_optional) {
            this.open_mode = (char) mode;
            this.flag_nonexistent = true;
            this.flag_end_of_file = true;
            this.flag_begin_of_file = true;
            saveStatus(COB_STATUS_05_SUCCESS_OPTIONAL, fnstatus);
            return;
          } else {
            saveStatus(COB_STATUS_35_NOT_EXISTS, fnstatus);
            return;
          }
        case EACCESS:
        case EISDIR:
        case EROFS:
          saveStatus(COB_STATUS_37_PERMISSION_DENIED, fnstatus);
          return;
        case COB_STATUS_61_FILE_SHARING:
          saveStatus(COB_STATUS_61_FILE_SHARING, fnstatus);
          return;
        case COB_STATUS_91_NOT_AVAILABLE:
          saveStatus(COB_STATUS_91_NOT_AVAILABLE, fnstatus);
          return;
        case COB_LINAGE_INVALID:
          saveStatus(COB_STATUS_57_I_O_LINAGE, fnstatus);
          return;
        default:
          saveStatus(COB_STATUS_30_PERMANENT_ERROR, fnstatus);
          return;
      }
    } catch (IOException e) {
      saveStatus(COB_STATUS_30_PERMANENT_ERROR, fnstatus);
      return;
    }
  }

  // protected long start;
  // protected long end;

  public void openEx(int mode, int sharing, AbstractCobolField fnstatus) {
    // this.open_("", mode, sharing);
  }

  public int open_(String filename, int mode, int sharing) throws IOException {
    FileChannel fp = null;
    try {
      switch (mode) {
        case COB_OPEN_INPUT:
          fp = FileChannel.open(Paths.get(filename), StandardOpenOption.READ);
          break;
        case COB_OPEN_OUTPUT:
          fp =
              FileChannel.open(
                  Paths.get(filename),
                  StandardOpenOption.WRITE,
                  StandardOpenOption.CREATE,
                  StandardOpenOption.TRUNCATE_EXISTING);
          break;
        case COB_OPEN_I_O:
          fp =
              FileChannel.open(
                  Paths.get(filename),
                  StandardOpenOption.READ,
                  StandardOpenOption.WRITE,
                  StandardOpenOption.CREATE);
          break;
        case COB_OPEN_EXTEND:
          fp =
              FileChannel.open(
                  Paths.get(filename), StandardOpenOption.APPEND, StandardOpenOption.CREATE);
          break;
        default:
          break;
      }
    } catch (IOException e) {
      if (Files.notExists(Paths.get(filename))) {
        return ENOENT;
      } else {
        return EACCESS;
      }
    }

    FileLock fl = null;
    if (!filename.startsWith("/dev/")) {
      try {
        boolean lockFlag;
        if (sharing != 0 || mode == COB_OPEN_OUTPUT) {
          lockFlag = false;
        } else {
          lockFlag = true;
        }
        fl = fp.tryLock(0L, Long.MAX_VALUE, lockFlag);
      } catch (NonWritableChannelException e) {
        fp.close();
        return EBADF;
      } catch (ClosedChannelException e) {
        fp.close();
        return COB_STATUS_61_FILE_SHARING;
      }

      this.file.setChannel(fp, fl);
      if (fl == null || !fl.isValid()) {
        fp.close();
        return COB_STATUS_61_FILE_SHARING;
      }
    }

    this.file.setChannel(fp, fl);
    if ((this.flag_select_features & COB_SELECT_LINAGE) != 0) {
      if (this.file_linage_check()) {
        return COB_LINAGE_INVALID;
      }
      this.flag_needs_top = true;
      Linage lingptr = this.getLinorkeyptr();
      lingptr.getLinageCtr().setInt(1);
    }
    return 0;
  }

  public void close(int opt, AbstractCobolField fnstatus) {
    String openMode = openModeToString(this.last_open_mode);
    if (invokeFun(COB_IO_CLOSE, this, null, null, fnstatus, openMode, null, null) != 0) {
      this.last_open_mode = (char) Integer.parseInt(openMode);
      return;
    }

    this.flag_read_done = false;
    if (this.special != 0) {
      this.open_mode = COB_OPEN_CLOSED;
      saveStatus(COB_STATUS_00_SUCCESS, fnstatus);
      return;
    }
    if (this.open_mode == COB_OPEN_CLOSED) {
      saveStatus(COB_STATUS_42_NOT_OPEN, fnstatus);
      return;
    }

    int ret;
    if (this.flag_nonexistent) {
      ret = COB_STATUS_00_SUCCESS;
    } else {
      ret = this.close_(opt);
    }

    if (ret == COB_STATUS_00_SUCCESS) {
      switch (opt) {
        case COB_CLOSE_LOCK:
          this.open_mode = COB_OPEN_LOCKED;
          break;
        default:
          this.open_mode = COB_OPEN_CLOSED;
          break;
      }
    }
    saveStatus(ret, fnstatus);
  }

  // public void closeEx(int opt, AbstractCobolField fnstatus) {
  // this.close_(opt);
  // }

  public int close_(int opt) {
    switch (opt) {
      case COB_CLOSE_NORMAL:
      case COB_CLOSE_LOCK:
      case COB_CLOSE_NO_REWIND:
        if (this.organization == COB_ORG_LINE_SEQUENTIAL) {
          if (this.flag_needs_nl && ((this.flag_select_features & COB_SELECT_LINAGE) == 0)) {
            this.flag_needs_nl = false;
            this.file.putc((byte) '\n');
          }
        }

        this.file.releaseLock();
        this.file.close();

        if (opt == COB_CLOSE_NO_REWIND) {
          this.open_mode = COB_OPEN_CLOSED;
          return COB_STATUS_07_SUCCESS_NO_UNIT;
        }
        return COB_STATUS_00_SUCCESS;
      default:
        this.file.flush();
        return COB_STATUS_07_SUCCESS_NO_UNIT;
    }
  }

  public void start(int cond, AbstractCobolField key, AbstractCobolField fnstatus) {
    String openMode = openModeToString(this.last_open_mode);
    String startCond = String.format("%01d", cond);
    if (invokeFun(COB_IO_START, this, null, null, fnstatus, openMode, startCond, null) != 0) {
      return;
    }

    this.flag_read_done = false;
    this.flag_first_read = 0;

    if (this.flag_nonexistent) {
      saveStatus(COB_STATUS_23_KEY_NOT_EXISTS, fnstatus);
      return;
    }

    if (this.open_mode == COB_OPEN_CLOSED
        || this.open_mode == COB_OPEN_OUTPUT
        || this.open_mode == COB_OPEN_EXTEND
        || this.access_mode == COB_ACCESS_RANDOM) {
      saveStatus(COB_STATUS_47_INPUT_DENIED, fnstatus);
      return;
    }

    int ret = this.start_(cond, key);
    if (ret == COB_STATUS_00_SUCCESS) {
      this.flag_end_of_file = false;
      this.flag_begin_of_file = false;
      this.flag_first_read = 1;
    }

    saveStatus(ret, fnstatus);
  }

  public void startEx(int cond, AbstractCobolField key, AbstractCobolField fnstatus) {
    this.start_(cond, key);
  }

  public int start_(int cond, AbstractCobolField key) {
    System.out.println("super.start");
    return 0;
  }

  public void read(AbstractCobolField key, AbstractCobolField fnstatus, int readOpts) {
    byte[] sbuff = new byte[3];
    String openMode = openModeToString(this.last_open_mode);
    String readOptStr = readOptsToString(readOpts);
    if (invokeFun(COB_IO_READ, this, key, null, fnstatus, openMode, null, readOptStr) != 0) {
      for (int i = 0; i < 3; ++i) {
        sbuff[i] = 0;
      }

      if (fnstatus == null) {
        return;
      }

      for (int i = 0; i < 2; ++i) {
        sbuff[i] = fnstatus.getDataStorage().getByte(i);
      }
      int status = Integer.parseInt(new String(sbuff));
      saveStatus(status, fnstatus);
      return;
    }

    this.flag_read_done = false;
    if (this.flag_nonexistent) {
      if (this.flag_first_read == 0) {
        saveStatus(COB_STATUS_23_KEY_NOT_EXISTS, fnstatus);
        return;
      }
      this.flag_first_read = 0;
      saveStatus(COB_STATUS_10_END_OF_FILE, fnstatus);
      return;
    }

    /* sequential read at the end of file is an error */
    if (key == null) {
      if (this.flag_end_of_file && (readOpts & COB_READ_PREVIOUS) == 0) {
        saveStatus(COB_STATUS_46_READ_ERROR, fnstatus);
        return;
      }
      if (this.flag_begin_of_file && (readOpts & COB_READ_PREVIOUS) != 0) {
        saveStatus(COB_STATUS_46_READ_ERROR, fnstatus);
        return;
      }
    }

    if (this.open_mode == COB_OPEN_CLOSED
        || this.open_mode == COB_OPEN_OUTPUT
        || this.open_mode == COB_OPEN_EXTEND) {
      saveStatus(COB_STATUS_47_INPUT_DENIED, fnstatus);
      return;
    }

    if (this.organization == COB_ORG_INDEXED) {
      if (this.open_mode != COB_OPEN_I_O || (this.lock_mode & COB_LOCK_EXCLUSIVE) != 0) {
        readOpts &= ~COB_READ_LOCK;
      } else if ((this.lock_mode & COB_LOCK_AUTOMATIC) != 0 && (readOpts & COB_READ_NO_LOCK) == 0) {
        readOpts |= COB_READ_LOCK;
      }
    } else {
      readOpts &= ~COB_READ_LOCK;
    }

    if (this.organization == COB_ORG_INDEXED /* && bdb_env != null */) {
      if (this.open_mode != COB_OPEN_I_O || (this.lock_mode & COB_LOCK_EXCLUSIVE) != 0) {
        readOpts &= ~COB_READ_LOCK;
      } else if ((this.lock_mode & COB_LOCK_AUTOMATIC) != 0 && (readOpts & COB_READ_NO_LOCK) == 0) {
        readOpts |= COB_READ_LOCK;
      }
    } else {
      readOpts &= ~COB_READ_LOCK;
    }

    int ret;
    if (key != null) {
      ret = this.read_(key, readOpts);
    } else {
      ret = this.readNext(readOpts);
    }

    switch (ret) {
      case COB_STATUS_00_SUCCESS:
        this.flag_first_read = 0;
        this.flag_read_done = true;
        this.flag_end_of_file = false;
        this.flag_begin_of_file = false;
        if (this.record_size != null && this.organization != COB_ORG_LINE_SEQUENTIAL) {
          this.record_size.setInt(record.getSize());
        }
        break;
      case COB_STATUS_10_END_OF_FILE:
        if ((readOpts & COB_READ_PREVIOUS) != 0) {
          this.flag_begin_of_file = true;
        } else {
          this.flag_end_of_file = true;
        }
        break;
      default:
        break;
    }

    saveStatus(ret, fnstatus);
  }

  public void read(int key, AbstractCobolField fnstatus, int readOpts) {
    this.read(null, fnstatus, readOpts);
  }

  public void readEx(AbstractCobolField key, AbstractCobolField fnstatus, int readOpts) {
    this.read_(key, readOpts);
  }

  public int read_(AbstractCobolField key, int readOpts) {
    System.out.println("super.read");
    return 0;
  }

  public int readNext(int readOpts) {
    System.out.println("super.readNext");
    return 0;
  }

  public void write(AbstractCobolField rec, int opt, AbstractCobolField fnstatus)
      throws CobolStopRunException {
    if (this.access_mode == COB_ACCESS_SEQUENTIAL
        && this.last_open_mode == COB_OPEN_I_O
        && CobolUtil.cob_io_rewwrite_assumed()) {
      this.rewrite(rec, opt, fnstatus);
      return;
    }

    String openMode = openModeToString(this.last_open_mode);
    if (invokeFun(COB_IO_WRITE, this, null, rec.getDataStorage(), fnstatus, openMode, null, null)
        != 0) {
      return;
    }

    this.flag_read_done = false;
    if (this.access_mode == COB_ACCESS_SEQUENTIAL) {
      if (this.open_mode == COB_OPEN_CLOSED
          || this.open_mode == COB_OPEN_INPUT
          || this.open_mode == COB_OPEN_I_O) {
        saveStatus(COB_STATUS_48_OUTPUT_DENIED, fnstatus);
        return;
      }
    } else {
      if (this.open_mode == COB_OPEN_CLOSED
          || this.open_mode == COB_OPEN_INPUT
          || this.open_mode == COB_OPEN_EXTEND) {
        saveStatus(COB_STATUS_48_OUTPUT_DENIED, fnstatus);
        return;
      }
    }

    int tmpsize = this.record.getSize();
    if (this.record_size != null) {
      this.record.setSize(this.record_size.getInt());
    } else {
      this.record.setSize(rec.getSize());
    }

    if (this.record.getSize() < this.record_min || this.record_max < this.record.getSize()) {
      saveStatus(COB_STATUS_44_RECORD_OVERFLOW, fnstatus);
      return;
    }

    int ret = this.write_(opt);

    if (cob_do_sync != 0 && ret == 0) {
      cob_sync(this, cob_do_sync);
    }

    this.record.setSize(tmpsize);
    saveStatus(ret, fnstatus);
  }

  public void writeEx(AbstractCobolField rec, int opt, AbstractCobolField fnstatus)
      throws CobolStopRunException {
    this.write_(opt);
  }

  public int write_(int opt) throws CobolStopRunException {
    System.out.println("super.write");
    return 0;
  }

  /**
   * libcob/fileio.cのcob_file_write_optの実装
   *
   * @param opt
   * @return
   * @throws CobolStopRunException
   */
  protected int file_write_opt(int opt) throws CobolStopRunException {
    if ((this.flag_select_features & COB_SELECT_LINAGE) != 0) {
      return linage_write_opt(opt);
    }
    if ((opt & COB_WRITE_LINES) != 0) {
      for (int i = opt & COB_WRITE_MASK; i > 0; i--) {
        this.file.putc((byte) '\n');
      }
    } else if ((opt & COB_WRITE_PAGE) != 0) {
      this.file.putc((byte) '\f');
    }
    return 0;
  }

  public void rewrite(AbstractCobolField rec, int opt, AbstractCobolField fnstatus) {
    String openMode = openModeToString(this.last_open_mode);
    if (invokeFun(COB_IO_REWRITE, this, null, null, fnstatus, openMode, null, null) != 0) {
      this.last_open_mode = (char) Integer.parseInt(openMode);
      return;
    }

    boolean readDone = this.flag_read_done;
    this.flag_read_done = false;

    if (this.open_mode == COB_OPEN_CLOSED || this.open_mode != COB_OPEN_I_O) {
      saveStatus(COB_STATUS_49_I_O_DENIED, fnstatus);
      return;
    }
    if (this.access_mode == COB_ACCESS_SEQUENTIAL && !readDone) {
      saveStatus(COB_STATUS_43_READ_NOT_DONE, fnstatus);
      return;
    }
    if (this.organization == COB_ORG_SEQUENTIAL) {
      if (this.record.getSize() != rec.getSize()) {
        saveStatus(COB_STATUS_44_RECORD_OVERFLOW, fnstatus);
        return;
      }
      if (this.record_size != null) {
        if (this.record.getSize() != this.record_size.getInt()) {
          saveStatus(COB_STATUS_44_RECORD_OVERFLOW, fnstatus);
          return;
        }
      }
    }
    int ret = this.rewrite_(opt);
    if (cob_do_sync != 0 && ret == 0) {
      cob_sync(this, cob_do_sync);
    }

    saveStatus(ret, fnstatus);
  }

  public void rewriteEx(AbstractCobolField rec, int opt, AbstractCobolField fnstatus) {
    this.rewrite_(opt);
  }

  public int rewrite_(int opt) {
    System.out.println("super.rewrite");
    return 0;
  }

  public void delete(AbstractCobolField fnstatus) {
    String openMode = openModeToString(this.last_open_mode);
    if (invokeFun(COB_IO_DELETE, this, null, null, fnstatus, openMode, null, null) != 0) {
      return;
    }

    boolean readDone = this.flag_read_done;
    this.flag_read_done = false;

    if (this.open_mode == COB_OPEN_CLOSED || this.open_mode != COB_OPEN_I_O) {
      saveStatus(COB_STATUS_49_I_O_DENIED, fnstatus);
      return;
    }

    if (this.access_mode == COB_ACCESS_SEQUENTIAL && !readDone) {
      saveStatus(COB_STATUS_43_READ_NOT_DONE, fnstatus);
      return;
    }

    int ret = this.delete_();

    if (cob_do_sync != 0 && ret == 0) {
      cob_sync(this, cob_do_sync);
    }
    saveStatus(ret, fnstatus);
  }

  public void deleteEx(AbstractCobolField fnstatus) {
    this.delete_();
  }

  public int delete_() {
    System.out.println("super.delete");
    return 0;
  }

  public void unlock(AbstractCobolField fnstatus) {
    String openMode = openModeToString(this.last_open_mode);
    if (invokeFun(COB_IO_UNLOCK, this, null, null, fnstatus, openMode, null, null) != 0) {
      return;
    }
    this.unlock_();
    saveStatus(COB_STATUS_00_SUCCESS, fnstatus);
  }

  public void unlock_() {
    if (this.open_mode != COB_OPEN_CLOSED && this.open_mode != COB_OPEN_LOCKED) {
      this.file.flush();
    }
  }

  public static void commit() {
    if (invokeFun(COB_IO_COMMIT, null, null, null, null, null, null, null) != 0) {
      return;
    }
    for (CobolFile l : file_cache) {
      l.unlock_();
    }
  }

  public static void rollback() {
    if (invokeFun(COB_IO_ROLLBACK, null, null, null, null, null, null, null) != 0) {
      return;
    }
    for (CobolFile l : file_cache) {
      l.unlock_();
    }
  }

  /** libcob/fileio.cのcob_exit_fileioの実装 TODO 一部だけ実装したため残りを実装する */
  public static void exitFileIO() {
    for (CobolFile f : file_cache) {
      if (f.open_mode != COB_OPEN_CLOSED && f.open_mode != COB_OPEN_LOCKED) {
        String filename = f.assign.fieldToString();
        System.err.print(
            String.format("WARNING - Implicit CLOSE of %s (\"%s\") \n", f.select_name, filename));
      }
    }
  }

  /** libcob/fileio.cのcob_syncの実装 */
  protected void cob_sync(CobolFile f, int mode) {
    // TODO
    // INDEXEDファイル実装時にやる
    // if (f.organization == COB_ORG_INDEXED) {
    // }
    if (f.organization != COB_ORG_SORT) {
      this.file.flush();
      if (mode == 2) {
        this.file.flush();
      }
    }
  }

  /** libcob/fileio.cのcob_init_fileioの実装 */
  public static void cob_init_fileio() {
    String s = CobolUtil.getEnv("COB_SYNC");
    if (s != null) {
      if (s.charAt(0) == 'Y' || s.charAt(0) == 'y') {
        cob_do_sync = 1;
      }
      if (s.charAt(0) == 'P' || s.charAt(0) == 'p') {
        cob_do_sync = 2;
      }
    }

    cob_file_path = CobolUtil.getEnv("COB_FILE_PATH");
    if (cob_file_path != null) {
      if (cob_file_path.charAt(0) == '\0' || cob_file_path.charAt(0) == ' ') {
        cob_file_path = null;
      }
    }

    cob_ls_nulls = CobolUtil.getEnv("COB_LS_NULLS");
    cob_ls_fixed = CobolUtil.getEnv("COB_LS_FIXED");

    file_open_env = new byte[COB_SMALL_BUFF];
    // file_open_name = new byte[COB_SMALL_BUFF];
    file_open_buff = new byte[COB_SMALL_BUFF];
  }

  public static void defaultErrorHandle() {
    byte[] fileStatus = CobolFile.errorFile.file_status;
    int status = (fileStatus[0] - '0') * 10 + (fileStatus[1] - '0');
    String msg;
    switch (status) {
      case COB_STATUS_10_END_OF_FILE:
        msg = "End of file";
        break;
      case COB_STATUS_14_OUT_OF_KEY_RANGE:
        msg = "Key out of range";
        break;
      case COB_STATUS_21_KEY_INVALID:
        msg = "Key order not ascending";
        break;
      case COB_STATUS_22_KEY_EXISTS:
        msg = "Record key already exists";
        break;
      case COB_STATUS_23_KEY_NOT_EXISTS:
        msg = "Record key does not exist";
        break;
      case COB_STATUS_30_PERMANENT_ERROR:
        msg = "Permanent file error";
        break;
      case COB_STATUS_35_NOT_EXISTS:
        msg = "File does not exist";
        break;
      case COB_STATUS_37_PERMISSION_DENIED:
        msg = "Permission denied";
        break;
      case COB_STATUS_41_ALREADY_OPEN:
        msg = "File already open";
        break;
      case COB_STATUS_42_NOT_OPEN:
        msg = "File not open";
        break;
      case COB_STATUS_43_READ_NOT_DONE:
        msg = "READ must be executed first";
        break;
      case COB_STATUS_44_RECORD_OVERFLOW:
        msg = "Record overflow";
        break;
      case COB_STATUS_46_READ_ERROR:
        msg = "Failed to read";
        break;
      case COB_STATUS_47_INPUT_DENIED:
        msg = "READ/START not allowed";
        break;
      case COB_STATUS_48_OUTPUT_DENIED:
        msg = "WRITE not allowed";
        break;
      case COB_STATUS_49_I_O_DENIED:
        msg = "DELETE/REWRITE not allowed";
        break;
      case COB_STATUS_51_RECORD_LOCKED:
        msg = "Record locked by another file connector";
        break;
      case COB_STATUS_52_EOP:
        msg = "A page overflow condition occurred";
        break;
      case COB_STATUS_57_I_O_LINAGE:
        msg = "LINAGE values invalid";
        break;
      case COB_STATUS_61_FILE_SHARING:
        msg = "File sharing conflict";
        break;
      case COB_STATUS_91_NOT_AVAILABLE:
        msg = "Runtime library is not configured for this operation";
        break;
      default:
        msg = "Unknown file error";
        break;
    }
    String filename = CobolFile.errorFile.assign.fieldToString();
    CobolUtil.runtimeError(String.format("%s (STATUS = %02d) File : '%s'", msg, status, filename));
  }

  public void cob_delete_file(AbstractCobolField fnstatus) {
    String openMode = openModeToString(this.last_open_mode);
    if (invokeFun(COB_IO_DELETE_FILE, this, null, null, fnstatus, openMode, null, null) != 0) {
      return;
    }

    if (this.open_mode == COB_OPEN_LOCKED) {
      saveStatus(COB_STATUS_38_CLOSED_WITH_LOCK, fnstatus);
      return;
    }

    /* file is already open */
    if (this.open_mode != COB_OPEN_CLOSED) {
      saveStatus(COB_STATUS_41_ALREADY_OPEN, fnstatus);
      return;
    }

    if (this.special != 0) {
      saveStatus(COB_STATUS_30_PERMANENT_ERROR, fnstatus);
      return;
    }

    if (this.assign == null) {
      file_open_name = this.select_name;
    } else {
      file_open_name = this.assign.fieldToString();
    }

    byte[] src;
    byte[] dst;
    boolean simple;
    if (CobolModule.getCurrentModule().flag_filename_mapping != 0) {
      src = file_open_name.getBytes();
      dst = file_open_buff;
      simple = true;
      int srcI = 0;
      int dstI = 0;
      while (srcI < src.length) {
        char c = (char) src[srcI];
        if (!Character.isLetterOrDigit(c) && c != '_' && c != '-') {
          simple = false;
        }
        if (c == '$') {
          int i;
          for (i = 1; srcI + i < src.length; i++) {
            char d = (char) src[srcI + i];
            if (!Character.isLetterOrDigit(d) && d != '_' && c != '-') {
              break;
            }
          }
          for (int j = 0; j < i - 1; ++j) {
            file_open_env[j] = src[srcI + 1 + j];
          }
          file_open_env[i - 1] = 0;
          String p = CobolUtil.getEnv(new String(Arrays.copyOfRange(file_open_env, 0, i - 1)));
          if (p != null) {
            byte[] pbytes = p.getBytes();
            for (int j = 0; j < pbytes.length; ++j) {
              dst[dstI + j] = pbytes[j];
            }
            dstI += pbytes.length;
          }
          srcI += i;
        } else {
          dst[dstI++] = src[srcI++];
        }
      }

      file_open_name = new String(Arrays.copyOfRange(dst, 0, dstI));

      byte[] fileOpenNameBytes = file_open_name.getBytes();
      cb_get_jisword_buff(file_open_buff, fileOpenNameBytes, COB_SMALL_BUFF);

      if (simple) {
        int i;
        for (i = 0; i < NUM_PREFIX; i++) {
          byte[] fileOpenBuff = concatString(prefix[i], file_open_name).getBytes();
          String p = CobolUtil.getEnv(new String(fileOpenBuff));
          if (p != null) {
            fileOpenNameBytes = p.getBytes();
            break;
          }
        }

        if (i == NUM_PREFIX && cob_file_path != null) {
          byte[] fileOpenBuff = concatString(cob_file_path, "/", file_open_name).getBytes();
          fileOpenNameBytes = fileOpenBuff;
        }
      }

      file_open_name = new String(fileOpenNameBytes);
    }

    Path filePath = Paths.get(this.assign.fieldToString());
    try {
      saveStatus(COB_STATUS_00_SUCCESS, fnstatus);
      Files.delete(filePath);
      return;
    } catch (IOException e) {
      int mode = (int) this.last_open_mode;
      try {
        switch (this.open_(file_open_name, mode, 0)) {
          case ENOENT:
            saveStatus(COB_STATUS_35_NOT_EXISTS, fnstatus);
            return;
          case EACCESS:
          case EISDIR:
          case EROFS:
            saveStatus(COB_STATUS_37_PERMISSION_DENIED, fnstatus);
            return;
          case EAGAIN:
          case COB_STATUS_61_FILE_SHARING:
            saveStatus(COB_STATUS_61_FILE_SHARING, fnstatus);
            return;
          case COB_STATUS_91_NOT_AVAILABLE:
            saveStatus(COB_STATUS_91_NOT_AVAILABLE, fnstatus);
            return;
          case COB_LINAGE_INVALID:
            saveStatus(COB_STATUS_57_I_O_LINAGE, fnstatus);
            return;
          default:
            saveStatus(COB_STATUS_30_PERMANENT_ERROR, fnstatus);
            return;
        }
      } catch (IOException e1) {
        saveStatus(COB_STATUS_30_PERMANENT_ERROR, fnstatus);
        return;
      }
    }
  }

  public static String getSelectName() {
    // CobolFile cobolFile = new CobolFile();
    return select_name;
  }

  public static byte[] getFileStatus() {
    // CobolFile cobolFile = new CobolFile();
    return file_status;
  }
}
