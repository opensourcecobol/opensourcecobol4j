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

import java.io.*;
import java.nio.channels.*;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolStopRunException;

public class CobolRelativeFile extends CobolFile {

  protected static final int COB_EQ = 1;
  protected static final int COB_LT = 2;
  protected static final int COB_LE = 3;
  protected static final int COB_GT = 4;
  protected static final int COB_GE = 5;

  public CobolRelativeFile(
      String select_name,
      byte[] file_status,
      AbstractCobolField assign,
      AbstractCobolField record,
      AbstractCobolField record_size,
      int record_min,
      int record_max,
      int nkeys,
      CobolFileKey[] keys,
      char organization,
      char access_mode,
      char lock_mode,
      char open_mode,
      boolean flag_optional,
      char last_open_mode,
      char special,
      boolean flag_nonexistent,
      boolean flag_end_of_file,
      boolean flag_begin_of_file,
      char flag_first_read,
      boolean flag_read_done,
      char flag_select_features,
      boolean flag_needs_nl,
      boolean flag_needs_top,
      char file_version) {
    super(
        select_name,
        file_status,
        assign,
        record,
        record_size,
        record_min,
        record_max,
        nkeys,
        keys,
        organization,
        access_mode,
        lock_mode,
        open_mode,
        flag_optional,
        last_open_mode,
        special,
        flag_nonexistent,
        flag_end_of_file,
        flag_begin_of_file,
        flag_first_read,
        flag_read_done,
        flag_select_features,
        flag_needs_nl,
        flag_needs_top,
        file_version);
  }

  @Override
  public int open_(String filename, int mode, int sharing) throws IOException {
    RandomAccessFile fp;
    FileChannel ch;
    try {
      switch (mode) {
        case COB_OPEN_INPUT:
          fp = new RandomAccessFile(this.assign.fieldToString(), "r"); // fileが存在しない場合エラー処理
          break;
        case COB_OPEN_OUTPUT:
          FileChannel.open(Paths.get(filename), StandardOpenOption.TRUNCATE_EXISTING);
          fp = new RandomAccessFile(this.assign.fieldToString(), "rw");
          break;
        case COB_OPEN_I_O:
          fp = new RandomAccessFile(this.assign.fieldToString(), "rw");
          break;
        case COB_OPEN_EXTEND:
          fp = new RandomAccessFile(this.assign.fieldToString(), "rw");
          fp.seek(fp.getFilePointer());
          break;
        default:
          fp = null;
          break;
      }
    } catch (IOException e) {
      // if (fp != null) {
      // this.file.setChannel(fp, null);
      // }
      if (Files.notExists(Paths.get(filename))) {
        return ENOENT;
      } else {
        return EACCESS;
      }
    }

    if (mode == COB_OPEN_EXTEND) {
      // ここは何もしない
    }

    ch = fp.getChannel();

    FileLock fl = null;
    if (!filename.startsWith("/dev/")) {
      try {
        boolean lock_flag;
        if (sharing != 0 || mode == COB_OPEN_OUTPUT) {
          lock_flag = false;
        } else {
          lock_flag = true;
        }
        fl = ch.tryLock(0L, Long.MAX_VALUE, lock_flag);
      } catch (NonWritableChannelException e) {
        fp.close();
        return EBADF;
      } catch (ClosedChannelException e) {
        fp.close();
        return COB_STATUS_61_FILE_SHARING;
      } catch (OverlappingFileLockException e) {
        fp.close();
        return COB_STATUS_61_FILE_SHARING;
      } catch (IOException e) {
        fp.close();
        return COB_STATUS_61_FILE_SHARING;
      }

      this.file.setChannel(ch, fl);
      if (fl == null || !fl.isValid()) {
        fp.close();
        return COB_STATUS_61_FILE_SHARING;
      }
    }

    this.file.setChannel(ch, fl);
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

  @Override
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

  @Override
  public int start_(int cond, AbstractCobolField key) {
    int kindex;
    int relsize;
    int off;

    /* get the index */
    kindex = key.getInt() - 1;
    relsize = this.record_max + this.record_size.getInt();
    if (cond == COB_LT) {
      kindex--;
    } else if (cond == COB_GT) {
      kindex++;
    }

    /* seek the index */
    int offset = 0;
    RandomAccessFile file;
    try {
      file = new RandomAccessFile(this.assign.fieldToString(), "r");
      for (;;) {
        off = kindex * relsize;
        try {
          file.seek((long) off);

          if (file.getFilePointer() - file.length() != 0
              || file.read(this.record_size.getBytes(), offset, this.record_size.getInt()) != 1) {
            return COB_STATUS_23_KEY_NOT_EXISTS;
          }

          /* check if a valid record */
          if (this.record_size.getInt() > 0) {
            key.setInt(kindex + 1);
            file.seek(file.getFilePointer() - this.record_size.getLong());
            return COB_STATUS_00_SUCCESS;
          }
        } catch (IOException e) {
          return COB_STATUS_30_PERMANENT_ERROR;
        }
        switch (cond) {
          case COB_EQ:
            return COB_STATUS_23_KEY_NOT_EXISTS;
          case COB_LT:
          case COB_LE:
            kindex--;
            break;
          case COB_GT:
          case COB_GE:
            kindex++;
            break;
        }
      }

    } catch (FileNotFoundException e) {
      return COB_STATUS_30_PERMANENT_ERROR;
    }
  }

  @Override
  public int read_(AbstractCobolField key, int readOpts) {
    int relnum;
    int relsize;
    int off;
    boolean isSeek = true;
    try {
      RandomAccessFile file = new RandomAccessFile(this.assign.fieldToString(), "r");
      relnum = key.getInt() - 1;
      relsize = this.record_max + this.record_size.getInt();
      off = relnum * relsize;
      try {
        file.seek(off);
      } catch (IOException e) {
        isSeek = false;
      }
      int offset = 0;
      if (!isSeek
          || file.read(this.record_size.getBytes(), offset, this.record_size.getInt()) != 1) {
        return COB_STATUS_23_KEY_NOT_EXISTS;
      }

      if (this.record_size.getInt() == 0) {
        file.seek(file.getFilePointer() - this.record_size.getInt());
        return COB_STATUS_23_KEY_NOT_EXISTS;
      }

      if (file.read(this.record.getBytes(), offset, this.record_max) != 1) {
        return COB_STATUS_30_PERMANENT_ERROR;
      }

      return COB_STATUS_00_SUCCESS;
    } catch (FileNotFoundException e) {
      return COB_STATUS_30_PERMANENT_ERROR;
    } catch (IOException e) {
      return COB_STATUS_30_PERMANENT_ERROR;
    }
  }

  @Override
  public int readNext(int readOpts) {
    int off;
    int relsize;
    int relnum;
    try {
      RandomAccessFile file = new RandomAccessFile(this.assign.fieldToString(), "r");
      relsize = this.record_max + this.record_size.getInt();
      int offset = 0;
      for (;;) {
        if (file.read(this.record_size.getBytes(), offset, this.record_size.getInt()) != 1) {
          return COB_STATUS_10_END_OF_FILE;
        }

        if (this.keys[0].getField().getInt() == 0) {
          if (this.flag_first_read == 0) {
            this.keys[0].getField().setInt(1);
            this.flag_first_read = 0;
          } else {
            off = (int) file.getFilePointer();
            relnum = (int) ((off / relsize) + 1);
            this.keys[0].getField().setInt(0);
            try {
              if (this.keys[0].getField().addInt(relnum) != 0) {
                file.seek(off - this.record_size.getInt());
                return COB_STATUS_14_OUT_OF_KEY_RANGE;
              }
            } catch (CobolStopRunException e) {
              return COB_STATUS_30_PERMANENT_ERROR;
            }
          }
        }

        if (this.record_size.getInt() > 0) {
          if (file.read(this.record.getBytes(), offset, this.record_max) != 1) {
            return COB_STATUS_30_PERMANENT_ERROR;
          }
          return COB_STATUS_00_SUCCESS;
        }

        file.seek(file.getFilePointer() + this.record_max);
      }
    } catch (FileNotFoundException e) {
      return COB_STATUS_30_PERMANENT_ERROR;
    } catch (IOException e) {
      return COB_STATUS_30_PERMANENT_ERROR;
    }
  }

  @Override
  public int write_(int opt) {
    int relsize;
    int i;
    int kindex;
    int off;
    RandomAccessFile file;

    try {
      file = new RandomAccessFile(this.assign.fieldToString(), "rw");
      relsize = this.record_max + this.record_size.getInt();
      if (this.access_mode != COB_ACCESS_SEQUENTIAL) {
        kindex = this.keys[0].getField().getInt() - 1;
        if (kindex < 0) {
          return COB_STATUS_21_KEY_INVALID;
        }
        off = relsize * kindex;

        try {
          file.seek((long) off);
        } catch (IOException e) {
          return COB_STATUS_21_KEY_INVALID;
        }
      } else {
        off = (int) file.getFilePointer();
      }
      int offset = 0;

      if (file.read(this.record_size.getBytes(), offset, this.record_size.getInt()) > 0) {

        file.seek(file.getFilePointer() - this.record_size.getInt());
        if (this.record_size.getInt() > 0) {
          return COB_STATUS_22_KEY_EXISTS;
        }
      } else {
        file.seek(off);
      }
      try {
        file.write(this.record_size.getBytes(), offset, this.record_size.getInt());
        file.write(this.record.getBytes(), offset, this.record_max);
      } catch (IOException e) {
        return COB_STATUS_30_PERMANENT_ERROR;
      }

      /* update RELATIVE KEY */
      if (this.access_mode == COB_ACCESS_SEQUENTIAL) {
        if (this.keys[0].getField().getInt() == 0) {
          off += relsize;
          i = (int) (off / relsize);
          this.keys[0].getField().setInt(i);
        }
      }

      return COB_STATUS_00_SUCCESS;
    } catch (FileNotFoundException e) {
      return COB_STATUS_30_PERMANENT_ERROR;
    } catch (IOException e) {
      return COB_STATUS_30_PERMANENT_ERROR;
    }
  }

  @Override
  public int rewrite_(int opt) {
    System.out.println("Relative.rewrite");
    return 0;
  }

  @Override
  public int delete_() {
    System.out.println("Relative.delete");
    return 0;
  }
}
