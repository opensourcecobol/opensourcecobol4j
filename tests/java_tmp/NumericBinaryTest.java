//未完成,実行不可
package test;

import java.io.UnsupportedEncodingException;

import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldAttribute;
import jp.osscons.opensourcecobol.libcobj.data.CobolNumericBinaryField;
import jp.osscons.opensourcecobol.libcobj.termio.CobolTerminal;


public class NumericBinaryTest {

	private CobolDataStorage b_5;

	private CobolFieldAttribute a_1;

	private CobolNumericBinaryField f_5;

	public NumericBinaryTest() {
		this.init();
	}

	private void init() {
		this.b_5 = new CobolDataStorage(4);

		this.a_1 = new CobolFieldAttribute(17, 5, 0, 32, null);

		this.f_5 = new CobolNumericBinaryField(4, b_5, a_1);
	}

	/* Functions */
	public int NumericBinaryTest_ (int entry) {
		try {
			CobolTerminal.openWriter();
		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
		}
		this.f_5.moveFrom(6789);

		CobolTerminal.display (true, true, f_5);


		CobolTerminal.closeWriter();
		return 0;
	}

	public static void main(String[] args) {
		new NumericBinaryTest().NumericBinaryTest_(0);
	}

}
