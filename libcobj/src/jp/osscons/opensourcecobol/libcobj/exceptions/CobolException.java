package jp.osscons.opensourcecobol.libcobj.exceptions;

public class CobolException {
	public static int code = 0;

	public static void setException(int id) {
		CobolException.code = CobolExceptionTabCode.code[id];
	}
}
