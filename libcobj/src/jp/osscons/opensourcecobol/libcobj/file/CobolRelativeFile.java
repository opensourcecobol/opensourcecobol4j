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
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolStopRunException;

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
    // RandomAccessFile fp = null;
    FileChannel ch;
    try {
      switch (mode) {
        case COB_OPEN_INPUT:
          this.fp = new RandomAccessFile(this.assign.fieldToString(), "r"); // fileが存在しない場合エラー処理
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
      // if (fp != null) {
      // this.file.setChannel(fp, null);
      // }
      if (this.fp != null) {
        this.file.setRandomAccessFile(this.fp, null);
      }
      // e.printStackTrace();
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

      // this.file.setChannel(ch, fl);
      this.file.setRandomAccessFile(this.fp, fl);

      if (fl == null || !fl.isValid()) {
        this.fp.close();
        return COB_STATUS_61_FILE_SHARING;
      }
    }

    // this.file.setChannel(ch, fl);
    this.file.setRandomAccessFile(this.fp, fl);
    if ((this.flag_select_features & COB_SELECT_LINAGE) != 0) {
      if (this.file_linage_check()) {
        return COB_LINAGE_INVALID;
      }
      this.flag_needs_top = true;
      Linage lingptr = this.getLinorkeyptr();
      lingptr.getLinageCtr().setInt(1);
    }
    System.out.println("COB_STATUS_00_SUCCESS(open)");
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
          System.out.println("COB_STATUS_00_SUCCESS(close)");
          return COB_STATUS_00_SUCCESS;
        default:
          this.file.flush();
          System.out.println("COB_STATUS_07_SUCCESS_NO_UNIT(close)");
          return COB_STATUS_07_SUCCESS_NO_UNIT;
      }
    } catch (IOException e) {
      System.out.println("COB_STATUS_30_PERMANENT_ERROR(close)");
      return COB_STATUS_30_PERMANENT_ERROR;
    }
  }

  @Override
  public int start_(int cond, AbstractCobolField key) {
    int kindex;
    int relsize;
    int off;

    /* get the index */
    kindex = key.getInt() - 1;
    relsize = this.record_max + this.sizeof_size.length;
    if (cond == COB_LT) {
      kindex--;
    } else if (cond == COB_GT) {
      kindex++;
    }

    /* seek the index */
    int offset = 0;
    boolean isSeek = true;
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
            System.out.println("COB_STATUS_00_SUCCESS(start)");
            return COB_STATUS_00_SUCCESS;
          }
        } catch (IOException e) {
          System.err.println("COB_STATUS_30_PERMANENT_ERROR(start1)");
          return COB_STATUS_30_PERMANENT_ERROR;
        }
        switch (cond) {
          case COB_EQ:
            System.err.println("COB_STATUS_23_KEY_NOT_EXISTS(start)");
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

      System.err.println("COB_STATUS_30_PERMANENT_ERROR(start2)");
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
      // RandomAccessFile file = this.fp;
      relnum = key.getInt() - 1;
      relsize = this.record_max + this.sizeof_size.length; // this.record.getSize();
      off = relnum * relsize;
      try {
        this.fp.seek(off);
      } catch (IOException e) {
        isSeek = false;
      }
      int offset = 0;
      if (!isSeek
          || this.fp.read(this.sizeof_size, offset, this.sizeof_size.length)
              != this.sizeof_size.length) {
        System.out.println("relnum=" + relnum);
        System.out.println("off=" + off);
        System.out.println("COB_STATUS_23_KEY_NOT_EXISTS(read1)");
        return COB_STATUS_23_KEY_NOT_EXISTS;
      }

      if (this.record.getSize() == 0) {
        this.fp.seek(this.fp.getFilePointer() - this.sizeof_size.length);
        System.out.println("COB_STATUS_23_KEY_NOT_EXISTS(read2)");
        return COB_STATUS_23_KEY_NOT_EXISTS;
      }
      byte[] bytes = new byte[this.record_max];
      if (this.fp.read(bytes, offset, this.record_max) != this.record_max) {
        System.err.println("COB_STATUS_30_PERMANENT_ERROR(read1)");
        return COB_STATUS_30_PERMANENT_ERROR;
      }
      this.record.getDataStorage().memcpy(bytes);
      System.out.println("COB_STATUS_00_SUCCESS(read)");
      return COB_STATUS_00_SUCCESS;
    } catch (FileNotFoundException e) {
      System.err.println("COB_STATUS_30_PERMANENT_ERROR(read2)");
      return COB_STATUS_30_PERMANENT_ERROR;
    } catch (IOException e) {
      System.err.println("COB_STATUS_30_PERMANENT_ERROR(read3)");
      return COB_STATUS_30_PERMANENT_ERROR;
    }
  }

  @Override
  public int readNext(int readOpts) {
    long off;
    int relsize;
    int relnum;
    int offset = 0;
    try {

      relsize = this.record_max + this.sizeof_size.length;
      for (; ; ) {
        int a = this.fp.read(this.sizeof_size, offset, this.sizeof_size.length);

        if (a != this.sizeof_size.length) {
          System.out.println("COB_STATUS_10_END_OF_FILE(readnext)");
          return COB_STATUS_10_END_OF_FILE;
        }

        if (this.keys[0].getField() != null) {
          if (this.flag_first_read != 0) {
            this.keys[0].getField().setInt(1);
            this.flag_first_read = 0;
          } else {
            off = this.fp.getFilePointer();
            System.out.println("off=" + off);

            relnum = (int) ((off / relsize) + 1);
            this.keys[0].getField().setInt(0);
            try {
              // System.out.println("addint=" + c);
              if (this.record.addInt(this.keys[0].getField(), relnum) != 0) {
                this.fp.seek(off - this.sizeof_size.length);
                System.out.println("COB_STATUS_14_OUT_OF_KEY_RANGE(readnext)");
                return COB_STATUS_14_OUT_OF_KEY_RANGE;
              }
            } catch (CobolStopRunException e) {
              System.out.println("COB_STATUS_30_PERMANENT_ERROR(readnext1)");
              return COB_STATUS_30_PERMANENT_ERROR;
            }
          }
        }
        byte[] bytes = new byte[this.record_max];
        if (this.record.getSize() > 0) {
          if (this.fp.read(bytes, offset, this.record_max) != this.record_max) {
            System.out.println("COB_STATUS_30_PERMANENT_ERROR(readnext2)");
            return COB_STATUS_30_PERMANENT_ERROR;
          }
          this.record.getDataStorage().memcpy(bytes);
          System.out.println("COB_STATUS_00_SUCCESS(read_next)");
          return COB_STATUS_00_SUCCESS;
        }

        this.fp.seek(this.fp.getFilePointer() + this.record_max);
      }
    } catch (FileNotFoundException e) {
      System.out.println("COB_STATUS_30_PERMANENT_ERROR(readnext3)");
      return COB_STATUS_30_PERMANENT_ERROR;
    } catch (IOException e) {
      System.out.println("COB_STATUS_30_PERMANENT_ERROR(readnext4)");
      return COB_STATUS_30_PERMANENT_ERROR;
    }
  }

  @Override
  public int write_(int opt) {
    // byte[] size = new byte[1000];
    int relsize;
    int i;
    int kindex;
    int off;

    try {
      relsize = this.record_max + this.sizeof_size.length; // this.record.getSize();
      if (this.access_mode != COB_ACCESS_SEQUENTIAL) {
        kindex = this.keys[0].getField().getInt() - 1;
        if (kindex < 0) {
          System.out.println("kindex=" + kindex);
          System.out.println("COB_STATUS_21_KEY_INVALID(write)");
          return COB_STATUS_21_KEY_INVALID;
        }
        off = relsize * kindex;

        try {
          this.fp.seek((long) off);
        } catch (IOException e) {
          System.out.println("COB_STATUS_21_KEY_INVALID(write)");
          return COB_STATUS_21_KEY_INVALID;
        }
      } else {
        off = (int) this.fp.getFilePointer();
      }
      int offset = 0;

      // ByteBuffer bf = ByteBuffer.allocate(10000);
      if (this.fp.read(this.sizeof_size, offset, this.sizeof_size.length) > 0) {
        // bf.get(size);
        this.fp.seek(this.fp.getFilePointer() - this.sizeof_size.length);
        if (this.sizeof_size.length > 0) {
          System.out.println("COB_STATUS_22_KEY_EXISTS(write)");
          return COB_STATUS_22_KEY_EXISTS;
        }
      } else {
        this.fp.seek(off);
      }
      try {
        this.fp.writeLong(this.record.getSize());
        // this.record_size.setInt((int) this.fp.getFilePointer());
        this.fp.write(
            this.record.getDataStorage().getByteArray(0, this.record_max), offset, this.record_max);
      } catch (IOException e) {
        System.out.println("COB_STATUS_30_PERMANENT_ERROR(write1)");
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
      // System.err.println("COB_STATUS_00_SUCCESS(write)");
      return COB_STATUS_00_SUCCESS;
    } catch (FileNotFoundException e) {
      System.err.println("COB_STATUS_30_PERMANENT_ERROR(write2)");
      return COB_STATUS_30_PERMANENT_ERROR;
    } catch (IOException e) {
      System.err.println("COB_STATUS_30_PERMANENT_ERROR(write3)");
      return COB_STATUS_30_PERMANENT_ERROR;
    }
  }

  @Override
  public int rewrite_(int opt) {
    int relsize;
    int relnum;
    int off;

    try {
      RandomAccessFile file = this.fp;
      int offset = 0;
      if (this.access_mode == COB_ACCESS_SEQUENTIAL) {
        file.seek(file.getFilePointer() - this.record_max);
      } else {
        relsize = this.record_max + 8; // this.record.getSize();
        relnum = this.keys[0].getField().getInt() - 1;
        off = relnum * relsize;

        boolean isSeek = true;
        try {
          file.seek(off);
        } catch (IOException e) {
          isSeek = false;
        }
        if (!isSeek
            || file.read(this.sizeof_size, offset, this.sizeof_size.length)
                != this.sizeof_size.length) {
          return COB_STATUS_23_KEY_NOT_EXISTS;
        }
      }

      file.write(
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
    System.out.println("delete_");
    relnum = this.keys[0].getField().getInt() - 1;
    relsize = this.record_max + this.sizeof_size.length;
    off = relnum * relsize;

    try {
      boolean isSeek = true;
      try {
        this.fp.seek(off);
      } catch (IOException e) {
        isSeek = false;
      }
      int offset = 0;
      if (!isSeek
          || this.fp.read(this.sizeof_size, offset, this.sizeof_size.length)
              != this.sizeof_size.length) {
        System.out.println("COB_STATUS_23_KEY_NOT_EXISTS(delete)");
        return COB_STATUS_23_KEY_NOT_EXISTS;
      }

      this.fp.seek(this.fp.getFilePointer() - 8);

      this.record.setSize(0);
      this.fp.writeLong(this.record.getSize());

      this.fp.seek(this.fp.getFilePointer() + this.record_max);
      System.out.println("COB_STATUS_00_SUCCESS(delete)");
      return COB_STATUS_00_SUCCESS;

    } catch (FileNotFoundException e) {
      System.out.println("COB_STATUS_30_PERMANENT_ERROR(delete1)");
      return COB_STATUS_30_PERMANENT_ERROR;
    } catch (IOException e) {
      System.out.println("COB_STATUS_30_PERMANENT_ERROR(delete2)");
      return COB_STATUS_30_PERMANENT_ERROR;
    }
  }
}
