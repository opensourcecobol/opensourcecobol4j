package jp.osscons.opensourcecobol.libcobj.file;

import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;

public class CobolRelativeFile extends CobolFile {

	public CobolRelativeFile(String select_name, byte[] file_status, AbstractCobolField assign,
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
	public int open_(String filename, int mode, int sharing) {
		System.out.println("Relative.open");
		return 0;
	}

	@Override
	public int close_(int opt) {
		System.out.println("Relative.close");
		return 0;
	}

	@Override
	public int start_(int cond, AbstractCobolField key) {
		System.out.println("Relative.start");
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
