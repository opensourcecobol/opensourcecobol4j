package jp.osscons.opensourcecobol.libcobj.file;

import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolRuntimeException;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolStopRunException;

public class CobolLineSequentialFile extends CobolFile {

	public CobolLineSequentialFile(String select_name, byte[] file_status, AbstractCobolField assign,
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
		System.out.println("LineSeq.start");
		return 0;
	}

	@Override
	public int read_(AbstractCobolField key, int readOpts) {
		System.out.println("LineSeq.read");
		return 0;
	}

	@Override
	public int readNext(int readOpts) {
		int dataptr = 0;
		int n;
		int i = 0;
		CobolDataStorage data = this.record.getDataStorage();

		for(;;) {
			n = this.file.getc();
			if(n == -1) {
				if(i == 0) {
					return COB_STATUS_10_END_OF_FILE;
				} else {
					break;
				}
			}
			if(n == 0 && cob_ls_nulls != null) {
				n = this.file.getc();
				if(n == -1) {
					return COB_STATUS_30_PERMANENT_ERROR;
				}
			} else {
				if(n == '\r') {
					continue;
				}
				if(n == '\n') {
					break;
				}
			}
			if(i < this.record.getSize()) {
				data.setByte(dataptr++, (byte)n);
				i++;
			}
		}
		if(i < this.record.getSize()) {
			for(int j=0; i + j <this.record.getSize(); ++j) {
				data.setByte(i + j, (byte)' ');
			}
		}
		if(this.record_size != null) {
			this.record_size.setInt(i);
		}
		return COB_STATUS_00_SUCCESS;
	}

	@Override
	public int write_(int opt) throws CobolStopRunException {
		int size;
		int i;

		/* determine the size to be written */
		if(cob_ls_fixed != null) {
			size = this.record.getSize();
		} else {
			CobolDataStorage data = this.record.getDataStorage();
			for(i= this.record.getSize() - 1; i >= 0; i--) {
				if(data.getByte(i) != ' ') {
					break;
				}
			}
			size = i + 1;
		}

		if((this.flag_select_features & COB_SELECT_LINAGE) != 0) {
			if(this.flag_needs_top) {
				this.flag_needs_top = false;
				Linage lingptr = this.linorkeyptr;
				for(i=0; i<lingptr.getLinTop(); i++) {
					this.file.putc((byte)'\n');
				}
			}
		}

		if((opt & COB_WRITE_AFTER) != 0) {
			int ret = file_write_opt(opt);
			if(ret != 0) {
				return ret;
			}
			this.flag_needs_nl = true;
		}

		if(size != 0) {
			if(cob_ls_nulls != null) {
				int p=0;
				CobolDataStorage data = this.record.getDataStorage();
				for(i=0; i<size; ++i, ++p) {
					if(data.getByte(p) < ' ') {
						this.file.putc((byte)0);
					}
					this.file.putc(data.getByte(p));
				}
			} else {
				if(this.file.write(this.record.getDataStorage(), size, 1) != 1) {
					return COB_STATUS_30_PERMANENT_ERROR;
				}
			}
		}

		if((this.flag_select_features & COB_SELECT_LINAGE) != 0) {
			this.file.putc((byte)'\n');
		}

		if((opt & COB_WRITE_BEFORE) != 0) {
			int ret = file_write_opt(opt);
			if(ret != 0) {
				return ret;
			}
			this.flag_needs_nl = false;
		}

		if(this.flag_needs_nl && (this.flag_select_features & COB_SELECT_LINAGE) == 0) {
			this.file.putc((byte)'\n');
			this.flag_needs_nl = false;
		}

		if(eop_status != 0) {
			eop_status = 0;
			CobolRuntimeException.code = 0x0502;
			return COB_STATUS_52_EOP;
		}
		return COB_STATUS_00_SUCCESS;
	}

	@Override
	public int rewrite_(int opt) {
		System.out.println("LineSeq.rewrite");
		return 0;
	}

	@Override
	public int delete_() {
		System.out.println("LineSeq.delete");
		return 0;
	}
}
