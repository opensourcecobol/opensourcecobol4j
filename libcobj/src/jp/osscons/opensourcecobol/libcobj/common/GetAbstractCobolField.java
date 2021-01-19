package jp.osscons.opensourcecobol.libcobj.common;

import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolStopRunException;

public interface GetAbstractCobolField {
	public AbstractCobolField run() throws CobolStopRunException;
}
