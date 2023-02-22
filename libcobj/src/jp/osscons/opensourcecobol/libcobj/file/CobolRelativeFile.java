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
import java.nio.file.Path;
import java.nio.file.Paths;
import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;

public class CobolRelativeFile extends CobolFile {

  protected static final int COB_EQ = 1;
  protected static final int COB_LT = 2;
  protected static final int COB_LE = 3;
  protected static final int COB_GT = 4;
  protected static final int COB_GE = 5;

  RandomAccessFile fp;
  byte[] sizeof_size = new byte[8];
  int seek;

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
    FileChannel ch;
    try {
      switch (mode) {
        case COB_OPEN_INPUT:
          this.fp = new RandomAccessFile(this.assign.fieldToString(), "r");
          this.fp.seek(0);
          System.out.print("(input)");
          break;
        case COB_OPEN_OUTPUT:
          Path path = Paths.get(filename);
          if (Files.exists(path)) {
            Files.delete(path);
          }
          this.fp = new RandomAccessFile(this.assign.fieldToString(), "rw");
          this.fp.seek(0);
          System.out.print("(output)");
          break;
        case COB_OPEN_I_O:
          this.fp = new RandomAccessFile(this.assign.fieldToString(), "rw");
          this.fp.seek(0);
          break;
        case COB_OPEN_EXTEND:
          this.fp = new RandomAccessFile(this.assign.fieldToString(), "rw");
          this.fp.seek(0);
          this.fp.seek(this.fp.length());
          break;
        default:
          this.fp = null;
          break;
      }
    } catch (IOException e) {
      if (this.fp != null) {
        this.file.setRandomAccessFile(this.fp, null);
      }
      if (Files.notExists(Paths.get(filename))) {
        return ENOENT;
      } else {
        return EACCESS;
      }
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
        this.fp.close();
        return EBADF;
      } catch (ClosedChannelException e) {
        this.fp.close();
        return COB_STATUS_61_FILE_SHARING;
      } catch (OverlappingFileLockException e) {
        this.fp.close();
        return COB_STATUS_61_FILE_SHARING;
      } catch (IOException e) {
        this.fp.close();
        return COB_STATUS_61_FILE_SHARING;
      }

      this.file.setRandomAccessFile(this.fp, fl);

      if (fl == null || !fl.isValid()) {
        this.fp.close();
        return COB_STATUS_61_FILE_SHARING;
      }
    }

    this.file.setRandomAccessFile(this.fp, fl);
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
    try {
      switch (opt) {
        case COB_CLOSE_NORMAL:
        case COB_CLOSE_LOCK:
        case COB_CLOSE_NO_REWIND:
          if (this.organization == COB_ORG_LINE_SEQUENTIAL) {
            if (this.flag_needs_nl && ((this.flag_select_features & COB_SELECT_LINAGE) == 0)) {
              this.flag_needs_nl = false;

              this.fp.writeChars("\n");
            }
          }

          this.file.releaseLock();
          this.fp.close();
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
    } catch (IOException e) {
      return COB_STATUS_30_PERMANENT_ERROR;
    }
  }

  @Override
  public int start_(int cond, AbstractCobolField key) {
    int kindex;
    int relsize;
    int off;
    final int offset = 0;
    boolean isSeek = true;

    /* get the index */
    kindex = key.getInt() - 1;
    relsize = this.record_max + this.sizeof_size.length;
    if (cond == COB_LT) {
      kindex--;
    } else if (cond == COB_GT) {
      kindex++;
    }

    /* seek the index */
    try {
      this.fp = new RandomAccessFile(this.assign.fieldToString(), "r");
      for (; ; ) {
        off = kindex * relsize;
        try {
          this.fp.seek((long) off);
          try {
            this.fp.seek(off);
          } catch (IOException e) {
            isSeek = false;
          }

          if (!isSeek
              || this.fp.read(this.sizeof_size, offset, this.sizeof_size.length)
                  != this.sizeof_size.length) {
            return COB_STATUS_23_KEY_NOT_EXISTS;
          }

          /* check if a valid record */
          if (this.record.getSize() > 0) {
            key.setInt(kindex + 1);
            this.fp.seek(this.fp.getFilePointer() - this.sizeof_size.length);
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
    final int offset = 0;
    boolean isSeek = true;
    try {
      relnum = key.getInt() - 1;
      relsize = this.record_max + this.sizeof_size.length;
      off = relnum * relsize;
      try {
        this.fp.seek(off);
      } catch (IOException e) {
        isSeek = false;
      }
      if (!isSeek
          || this.fp.read(this.sizeof_size, offset, this.sizeof_size.length)
              != this.sizeof_size.length) {
        return COB_STATUS_23_KEY_NOT_EXISTS;
      }

      if (this.record.getSize() == 0) {
        this.fp.seek(this.fp.getFilePointer() - this.sizeof_size.length);
        return COB_STATUS_23_KEY_NOT_EXISTS;
      }
      byte[] bytes = new byte[this.record_max];
      if (this.fp.read(bytes, offset, this.record_max) != this.record_max) {
        return COB_STATUS_30_PERMANENT_ERROR;
      }
      this.record.getDataStorage().memcpy(bytes);
      return COB_STATUS_00_SUCCESS;
    } catch (FileNotFoundException e) {
      return COB_STATUS_30_PERMANENT_ERROR;
    } catch (IOException e) {
      return COB_STATUS_30_PERMANENT_ERROR;
    }
  }

  @Override
  public int readNext(int readOpts) {
    long off;
    int relsize;
    int relnum;
    final int offset = 0;
    try {

      relsize = this.record_max + this.sizeof_size.length;
      for (; ; ) {
        if (this.fp.read(this.sizeof_size, offset, this.sizeof_size.length)
            != this.sizeof_size.length) {
          return COB_STATUS_10_END_OF_FILE;
        }
        this.fp.seek(this.fp.getFilePointer() - 4);
        this.record.setSize(this.fp.readInt());

        if (this.keys[0].getField() != null) {
          if (this.flag_first_read != 0) {
            this.keys[0].getField().setInt(1);
            this.flag_first_read = 0;
          } else {
            off = this.fp.getFilePointer();
            relnum = (int) ((off / relsize) + 1);
            this.keys[0].getField().setInt(relnum);
            if (String.valueOf(relnum).length()
                > this.keys[0].getField().getAttribute().getDigits()) {
              return COB_STATUS_14_OUT_OF_KEY_RANGE;
            }
          }
        }

        byte[] bytes = new byte[this.record_max];
        this.fp.seek(this.fp.getFilePointer() - 4);
        int record_size = this.fp.readInt();
        if (record_size > 0) {
          if (this.fp.read(bytes, offset, this.record_max) != this.record_max) {
            return COB_STATUS_30_PERMANENT_ERROR;
          }
          this.record.getDataStorage().memcpy(bytes);
          return COB_STATUS_00_SUCCESS;
        }

        this.fp.seek(this.fp.getFilePointer() + this.record_max);
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
    final int offset = 0;

    try {
      relsize = this.record_max + this.sizeof_size.length;
      if (this.access_mode != COB_ACCESS_SEQUENTIAL) {
        kindex = this.keys[0].getField().getInt() - 1;
        if (kindex < 0) {
          return COB_STATUS_21_KEY_INVALID;
        }
        off = relsize * kindex;

        try {
          this.fp.seek((long) off);
        } catch (IOException e) {
          return COB_STATUS_21_KEY_INVALID;
        }
      } else {
        off = (int) this.fp.getFilePointer();
      }
      if (this.fp.read(this.sizeof_size, offset, this.sizeof_size.length) > 0) {
        this.fp.seek(this.fp.getFilePointer() - 4);
        int size = this.fp.readInt();
        this.fp.seek(this.fp.getFilePointer() - this.sizeof_size.length);
        if (size > 0) {
          return COB_STATUS_22_KEY_EXISTS;
        }
      } else {
        this.fp.seek(off);
      }
      try {
        this.fp.writeLong(this.record.getSize());
        this.fp.write(
            this.record.getDataStorage().getByteArray(0, this.record_max), offset, this.record_max);
      } catch (IOException e) {
        return COB_STATUS_30_PERMANENT_ERROR;
      }

      /* update RELATIVE KEY */
      if (this.access_mode == COB_ACCESS_SEQUENTIAL) {
        if (this.keys[0].getField() != null) {
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
    int relsize;
    int relnum;
    int off;
    final int offset = 0;

    try {
      if (this.access_mode == COB_ACCESS_SEQUENTIAL) {
        this.fp.seek(this.fp.getFilePointer() - this.record_max);
      } else {
        relsize = this.record_max + this.sizeof_size.length;
        relnum = this.keys[0].getField().getInt() - 1;
        off = relnum * relsize;

        boolean isSeek = true;
        try {
          this.fp.seek(off);
        } catch (IOException e) {
          isSeek = false;
        }
        if (!isSeek
            || this.fp.read(this.sizeof_size, offset, this.sizeof_size.length)
                != this.sizeof_size.length) {
          return COB_STATUS_23_KEY_NOT_EXISTS;
        }
      }

      this.fp.write(
          this.record.getDataStorage().getByteArray(0, this.record_max), offset, this.record_max);
    } catch (IOException e) {
      return COB_STATUS_30_PERMANENT_ERROR;
    }
    return COB_STATUS_00_SUCCESS;
  }

  @Override
  public int delete_() {
    int relsize;
    int relnum;
    int off;
    relnum = this.keys[0].getField().getInt() - 1;
    relsize = this.record_max + this.sizeof_size.length;
    off = relnum * relsize;
    final int offset = 0;

    try {
      boolean isSeek = true;
      try {
        this.fp.seek(off);
      } catch (IOException e) {
        isSeek = false;
      }
      if (!isSeek
          || this.fp.read(this.sizeof_size, offset, this.sizeof_size.length)
              != this.sizeof_size.length) {
        return COB_STATUS_23_KEY_NOT_EXISTS;
      }

      this.fp.seek(this.fp.getFilePointer() - this.sizeof_size.length);

      this.fp.writeLong(0);

      this.fp.seek(this.fp.getFilePointer() + this.record_max);
      return COB_STATUS_00_SUCCESS;

    } catch (FileNotFoundException e) {
      return COB_STATUS_30_PERMANENT_ERROR;
    } catch (IOException e) {
      return COB_STATUS_30_PERMANENT_ERROR;
    }
  }
}
