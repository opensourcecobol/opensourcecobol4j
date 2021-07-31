package jp.osscons.opensourcecobol.libcobj.file;

import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;

public class CobolFileFactory {

	public static CobolFile makeCobolFileInstance(
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
				switch(organization) {
				case CobolFile.COB_ORG_SEQUENTIAL:
					return new CobolSequentialFile(
							select_name, file_status, assign, record, record_size,
							record_min, record_max, nkeys, keys,
							organization, access_mode, lock_mode, open_mode, flag_optional,
							last_open_mode, special, flag_nonexistent, flag_end_of_file, flag_begin_of_file,
							flag_first_read, flag_read_done, flag_select_features, flag_needs_nl, flag_needs_top,file_version);
				case CobolFile.COB_ORG_LINE_SEQUENTIAL:
					return new CobolLineSequentialFile(
							select_name, file_status, assign, record, record_size,
							record_min, record_max, nkeys, keys,
							organization, access_mode, lock_mode, open_mode, flag_optional,
							last_open_mode, special, flag_nonexistent, flag_end_of_file, flag_begin_of_file,
							flag_first_read, flag_read_done, flag_select_features, flag_needs_nl, flag_needs_top,file_version);
				case CobolFile.COB_ORG_RELATIVE:
					return new CobolRelativeFile(
							select_name, file_status, assign, record, record_size,
							record_min, record_max, nkeys, keys,
							organization, access_mode, lock_mode, open_mode, flag_optional,
							last_open_mode, special, flag_nonexistent, flag_end_of_file, flag_begin_of_file,
							flag_first_read, flag_read_done, flag_select_features, flag_needs_nl, flag_needs_top,file_version);
				case CobolFile.COB_ORG_INDEXED:
					return new CobolIndexedFile(
							select_name, file_status, assign, record, record_size,
							record_min, record_max, nkeys, keys,
							organization, access_mode, lock_mode, open_mode, flag_optional,
							last_open_mode, special, flag_nonexistent, flag_end_of_file, flag_begin_of_file,
							flag_first_read, flag_read_done, flag_select_features, flag_needs_nl, flag_needs_top,file_version);
				default:
					return new CobolFile(
							select_name, file_status, assign, record, record_size,
							record_min, record_max, nkeys, keys,
							organization, access_mode, lock_mode, open_mode, flag_optional,
							last_open_mode, special, flag_nonexistent, flag_end_of_file, flag_begin_of_file,
							flag_first_read, flag_read_done, flag_select_features, flag_needs_nl, flag_needs_top,file_version);
				}
			}
}
