package test;

import java.io.UnsupportedEncodingException;

import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldAttribute;
import jp.osscons.opensourcecobol.libcobj.data.CobolNumericBinaryField;
import jp.osscons.opensourcecobol.libcobj.data.CobolNumericPackedField;
import jp.osscons.opensourcecobol.libcobj.termio.CobolTerminal;


public class MoveNumericBinaryToNumericPacked {
	private CobolDataStorage b_5;
	private CobolDataStorage b_6;

	private CobolFieldAttribute a_1;
	private CobolFieldAttribute a_2;

	private CobolNumericBinaryField f_5;
	private CobolNumericPackedField f_6;

	public MoveNumericBinaryToNumericPacked() {
		this.init();
	}

	private void init() {
		this.b_5 = new CobolDataStorage(4);
		this.b_6 = new CobolDataStorage(3);

		this.a_1 = new CobolFieldAttribute(18, 5, 0, 0, null);
		this.a_2 = new CobolFieldAttribute(17, 5, 0, 32, null);

		this.f_5 = new CobolNumericBinaryField(5, b_5, a_2);
		this.f_6 = new CobolNumericPackedField(4, b_6, a_1);
	}

	/* Functions */
	public int MoveNumericToNumericBinary_ (int entry) {
		try {
			CobolTerminal.openWriter();
		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
		}

		this.f_6.moveFrom(12);
		CobolTerminal.display (true, true, f_6);

		this.f_5.moveFrom(345);
		CobolTerminal.display (true, true, f_5);

		f_6.moveFrom(f_5);
		CobolTerminal.display (true, true, f_6);

		CobolTerminal.closeWriter();
		return 0;
	}

	public static void main(String[] args) {
		new MoveNumericToNumericBinary().MoveNumericToNumericBinary_(0);
	}
}

