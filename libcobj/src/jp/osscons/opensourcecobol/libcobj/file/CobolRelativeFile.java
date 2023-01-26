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
import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;

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
      for (; ; ) {
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
    }
    return 0;
  }

  @Override
  public int read_(AbstractCobolField key, int readOpts) {
    System.out.println("Relative.read");
    return 0;
  }

  @Override
  public int readNext(int readOpts) {
    System.out.println("Relative.readNext");
    return 0;
  }

  @Override
  public int write_(int opt) {
    System.out.println("Relative.write");
    return 0;
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
