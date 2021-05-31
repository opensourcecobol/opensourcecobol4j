/*
 * Copyright (C) 2020 TOKYO SYSTEM HOUSE Co., Ltd.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, 51 Franklin Street, Fifth Floor
 * Boston, MA 02110-1301 USA
 */

package jp.osscons.opensourcecobol.libcobj.file;

import java.io.IOException;
import java.nio.ByteBuffer;

import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolStopRunException;

public class CobolSequentialFile extends CobolFile {

	public CobolSequentialFile(String select_name, byte[] file_status, AbstractCobolField assign,
			AbstractCobolField record, AbstractCobolField record_size, int record_min, int record_max, int nkeys,
			CobolFileKey[] keys, char organization, char access_mode, char lock_mode, char open_mode,
			boolean flag_optional, char last_open_mode, char special, boolean flag_nonexistent,
			boolean flag_end_of_file, boolean flag_begin_of_file, char flag_first_read, boolean flag_read_done,
			char flag_select_features, boolean flag_needs_nl, boolean flag_needs_top, char file_version) {
		super(select_name, file_status, assign, record, record_size, record_min, record_max, nkeys, keys, organization,
				access_mode, lock_mode, open_mode, flag_optional, last_open_mode, special, flag_nonexistent, flag_end_of_file,
				flag_begin_of_file, flag_first_read, flag_read_done, flag_select_features, flag_needs_nl, flag_needs_top,
				file_version);
	}

	@Override
	public int start_(int cond, AbstractCobolField key) {
		System.out.println("Seq.start");
		return 0;
	}

	@Override
	public int read_(AbstractCobolField key, int readOpts) {
		System.out.println("Seq.read");
		return 0;
	}

	/**
	 * libcob/fileio.cのsequential_readの実装
	 */
	@Override
	public int readNext(int readOpts) {
		byte[] sbuff = new byte[4];
		this.file.seekInit();

		if(this.record_min != this.record_max) {
			if(this.file.read(sbuff, 4) != 1) {
				if(this.file.isAtEnd()) {
					return COB_STATUS_10_END_OF_FILE;
				} else {
					return COB_STATUS_30_PERMANENT_ERROR;
				}
			}
			int size = ByteBuffer.wrap(sbuff).getInt();
			this.record.setSize(size);
		}

		try {
			int bytesread = this.file.read(this.record.getDataStorage(), this.record.getSize());
			if(bytesread != this.record.getSize()) {
				if(bytesread == 0) {
					return COB_STATUS_10_END_OF_FILE;
				} else {
					return COB_STATUS_04_SUCCESS_INCOMPLETE;
				}
			}
		} catch(IOException e) {
			return COB_STATUS_30_PERMANENT_ERROR;
		}
		return COB_STATUS_00_SUCCESS;
	}

	@Override
	public int write_(int opt) throws CobolStopRunException {
		int ret;
		byte[] sbuff = new byte[4];

		this.file.seekInit();

		/* WRITE AFTER */
		if((opt & COB_WRITE_AFTER) != 0) {
			ret = file_write_opt(opt);
			if(ret != 0) {
				return ret;
			}
		}

		if(this.record_min != this.record_max) {
			ByteBuffer.wrap(sbuff).putInt(this.record.getSize());
			if(this.file.write(sbuff, 4, 1) != 1) {
				return COB_STATUS_30_PERMANENT_ERROR;
			}
		}

		/* write the record */
		if(this.file.write(this.record.getDataStorage(), this.record.getSize(), 1) != 1) {
			return COB_STATUS_30_PERMANENT_ERROR;
		}

		/* WRITE BEFORE */
		if((opt & COB_WRITE_BEFORE) != 0) {
			ret = this.file_write_opt(opt);
			if(ret != 0) {
				return ret;
			}
			this.flag_needs_nl = false;
		}
		return 0;
	}

	@Override
	public int rewrite_(int opt) {
		if(!this.file.seek(-this.record.getSize(), FileIO.SEEK_CUR)) {
			return COB_STATUS_30_PERMANENT_ERROR;
		}
		if(this.file.write(this.record.getDataStorage(), this.record.getSize(), 1) != 1) {
			return COB_STATUS_30_PERMANENT_ERROR;
		}
		return COB_STATUS_00_SUCCESS;
	}

	@Override
	public int delete_() {
		System.out.println("Seq.delete");
		return 0;
	}
}