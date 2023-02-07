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
import java.nio.ByteBuffer;
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
          System.out.print("(input)");
          break;
        case COB_OPEN_OUTPUT:
          // FileChannel.open(Paths.get(filename), StandardOpenOption.TRUNCATE_EXISTING);
          Path path = Paths.get(filename);
          if (Files.exists(path)) {
            Files.delete(path);
          }
          this.fp = new RandomAccessFile(this.assign.fieldToString(), "rw");
          System.out.print("(output)");
          break;
        case COB_OPEN_I_O:
          this.fp = new RandomAccessFile(this.assign.fieldToString(), "rw");
          break;
        case COB_OPEN_EXTEND:
          this.fp = new RandomAccessFile(this.assign.fieldToString(), "rw");
          this.fp.seek(fp.length());
          break;
        default:
          this.fp = null;
          break;
      }
    } catch (IOException e) {
      // if (fp != null) {
      // this.file.setChannel(fp, null);
      // }
      if (fp != null) {
        this.file.setRandomAccessFile(fp, null);
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

      // this.file.setChannel(ch, fl);
      this.file.setRandomAccessFile(fp, fl);

      if (fl == null || !fl.isValid()) {
        fp.close();
        return COB_STATUS_61_FILE_SHARING;
      }
    }

    // this.file.setChannel(ch, fl);
    this.file.setRandomAccessFile(fp, fl);
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
        System.out.println("COB_STATUS_00_SUCCESS(close)");
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
    relsize = this.record_max + this.record.getSize();
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
      for (; ; ) {
        off = kindex * relsize;
        try {
          file.seek((long) off);

          if (file.getFilePointer() - file.length() != 0
              || file.read(this.record.getBytes(), offset, this.record.getSize()) != 1) {
            return COB_STATUS_23_KEY_NOT_EXISTS;
          }

          /* check if a valid record */
          if (this.record.getSize() > 0) {
            key.setInt(kindex + 1);
            file.seek(file.getFilePointer() - this.record.getSize());
            return COB_STATUS_00_SUCCESS;
          }
        } catch (IOException e) {
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
      RandomAccessFile file = this.fp;
      relnum = key.getInt() - 1;
      relsize = this.record_max + this.record.getSize();
      off = relnum * relsize;
      try {
        file.seek(off);
      } catch (IOException e) {
        isSeek = false;
      }
      int offset = 0;
      if (!isSeek || file.read(this.record.getBytes(), offset, this.record.getSize()) != 1) {
        System.out.println("COB_STATUS_23_KEY_NOT_EXISTS(read1)");
        return COB_STATUS_23_KEY_NOT_EXISTS;
      }

      if (this.record.getSize() == 0) {
        file.seek(file.getFilePointer() - this.record.getSize());
        System.out.println("COB_STATUS_23_KEY_NOT_EXISTS(read2)");
        return COB_STATUS_23_KEY_NOT_EXISTS;
      }

      if (file.read(this.record.getDataStorage().getData(), offset, this.record_max) != 1) {
        System.out.println("COB_STATUS_30_PERMANENT_ERROR(read1)");
        return COB_STATUS_30_PERMANENT_ERROR;
      }
      System.out.println("COB_STATUS_00_SUCCESS(read)");
      return COB_STATUS_00_SUCCESS;
    } catch (FileNotFoundException e) {
      System.out.println("COB_STATUS_30_PERMANENT_ERROR(read2)");
      return COB_STATUS_30_PERMANENT_ERROR;
    } catch (IOException e) {
      System.out.println("COB_STATUS_30_PERMANENT_ERROR(read3)");
      return COB_STATUS_30_PERMANENT_ERROR;
    }
  }

  @Override
  public int readNext(int readOpts) {
    int off;
    int relsize;
    int relnum;
    try {
      RandomAccessFile file = this.fp;
      relsize = this.record_max + this.record.getSize();
      int offset = 0;
      // byte[] size_bytes =
      // ByteBuffer.allocate(64).putInt(this.record.getSize()).array();
      // ByteBuffer bf = ByteBuffer.allocate(64);
      // int c = this.record.getInt(this.record_size.getInt());
      file.seek(0);
      for (; ; ) {
        int a = file.read(this.record.getBytes(), offset, this.record.getSize());
        if (a != this.record.getSize()) {
          System.out.printf("a=%d\n", a);
          // System.out.println(c);
          // System.out.println(bf.getLong());
          System.out.println("COB_STATUS_10_END_OF_FILE(readnext)");
          return COB_STATUS_10_END_OF_FILE;
        }

        if (this.keys[0].getField() != null) {
          if (this.flag_first_read != 0) {
            this.keys[0].getField().setInt(1);
            this.flag_first_read = 0;
          } else {
            off = (int) file.getFilePointer();
            relnum = (int) ((off / relsize) + 1);
            this.keys[0].getField().setInt(0);
            try {
              if (this.keys[0].getField().addInt(relnum) != 0) {
                file.seek(off - this.record.getSize());
                System.out.println("COB_STATUS_14_OUT_OF_KEY_RANGE(readnext)");
                return COB_STATUS_14_OUT_OF_KEY_RANGE;
              }
            } catch (CobolStopRunException e) {
              System.out.println("COB_STATUS_30_PERMANENT_ERROR(readnext1)");
              return COB_STATUS_30_PERMANENT_ERROR;
            }
          }
        }

        if (this.record.getSize() > 0) {
          if (file.read(this.record.getBytes(), offset, this.record_max) != this.record_max) {
            System.out.println("COB_STATUS_30_PERMANENT_ERROR(readnext2)");
            return COB_STATUS_30_PERMANENT_ERROR;
          }
          System.out.println("COB_STATUS_00_SUCCESS(read_next)");
          return COB_STATUS_00_SUCCESS;
        }

        file.seek(file.getFilePointer() + this.record_max);
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
    byte[] size = new byte[1000];
    int relsize;
    int i;
    int kindex;
    int off;

    try {
      this.fp.seek(0);
      relsize = this.record_max + this.record.getSize();

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
      int offset = 0;

      ByteBuffer bf = ByteBuffer.allocate(10000);
      int c = this.fp.read(size, offset, this.record.getSize());
      System.out.println("c=" + c);
      if (c > 0) {
        bf.get(size);
        this.fp.seek(this.fp.getFilePointer() - this.record.getSize());
        if (bf.getInt() > 0) {
          return COB_STATUS_22_KEY_EXISTS;
        }
      } else {
        this.fp.seek(off);
      }
      try {
        this.fp.write(this.record.getBytes(), offset, this.record.getSize());
        this.fp.write(this.record.getDataStorage().getData(), offset, this.record_max);
      } catch (IOException e) {
        return COB_STATUS_30_PERMANENT_ERROR;
      }

      /* update RELATIVE KEY */
      // if (this.access_mode == COB_ACCESS_SEQUENTIAL) {
      // if (this.keys[0].getField() != null) {
      // off += relsize;
      // i = (int) (off / relsize);
      // this.keys[0].getField().setInt(i);
      // }
      // }
      System.out.println("COB_STATUS_00_SUCCESS(write)");
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

    try {
      RandomAccessFile file = this.fp;
      int offset = 0;
      if (this.access_mode == COB_ACCESS_SEQUENTIAL) {
        file.seek(file.getFilePointer() - this.record_max);
      } else {
        relsize = this.record_max + this.record.getSize();
        relnum = this.keys[0].getField().getInt() - 1;
        off = relnum * relsize;

        boolean isSeek = true;
        try {
          file.seek(off);
        } catch (IOException e) {
          isSeek = false;
        }
        if (!isSeek || file.read(this.record.getBytes(), offset, this.record.getSize()) != 1) {
          return COB_STATUS_23_KEY_NOT_EXISTS;
        }
        file.seek(0);
      }

      file.write(this.record.getBytes(), offset, this.record_max);
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
    relsize = this.record_max + this.record.getSize();
    off = relnum * relsize;

    try {
      RandomAccessFile file = this.fp;
      boolean isSeek = true;
      try {
        file.seek(off);
      } catch (IOException e) {
        isSeek = false;
      }
      int offset = 0;
      if (!isSeek
          || file.read(this.record.getBytes(), offset, this.record.getSize())
              != this.record.getSize()) {
        // System.err.println("COB_STATUS_23_KEY_NOT_EXISTS(delete)");
        return COB_STATUS_23_KEY_NOT_EXISTS;
      }

      file.seek(file.getFilePointer() - this.record.getSize());

      this.record.setSize(0);
      file.write(this.record.getBytes(), offset, this.record.getSize());

      file.seek(file.getFilePointer() + this.record_max);

      return COB_STATUS_00_SUCCESS;

    } catch (FileNotFoundException e) {
      return COB_STATUS_30_PERMANENT_ERROR;
    } catch (IOException e) {
      return COB_STATUS_30_PERMANENT_ERROR;
    }
  }
}
