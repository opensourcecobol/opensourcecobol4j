package test;

import java.io.UnsupportedEncodingException;

import jp.osscons.opensourcecobol.libcobj.data.CobolAlphanumericField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldAttribute;
import jp.osscons.opensourcecobol.libcobj.termio.CobolTerminal;

public class MoveAlphanumericToAlphanumeric {
	private CobolDataStorage b_5;
	private CobolDataStorage b_6;

	private CobolFieldAttribute a_1;
	private CobolFieldAttribute a_2;

	private CobolAlphanumericField f_5;
	private CobolAlphanumericField f_6;

	public MoveAlphanumericToAlphanumeric() {
		this.init();
	}

	private void init() {
		this.b_5 = new CobolDataStorage(5);
		this.b_6 = new CobolDataStorage(5);

		this.a_1 = new CobolFieldAttribute(33, 0, 0, 0, null);

		this.f_5 = new CobolAlphanumericField(5, b_5, a_1);
		this.f_6 = new CobolAlphanumericField(5, b_6, a_1);
	}

	/* Functions */
	public int MoveAlphanumericToAlphanumeric_ (int entry) {
		try {
			CobolTerminal.openWriter();
		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
		}

		this.f_5.moveFrom("12");
		this.f_6.moveFrom("345");

		this.f_5.moveFrom(f_6);

		System.out.println("想定される出力");
		System.out.println("345");
		System.out.println("実際の出力");
		CobolTerminal.display (true, true, f_5);

		CobolTerminal.closeWriter();
		return 0;
	}

	public static void main(String[] args) {
		new MoveAlphanumericToAlphanumeric().MoveAlphanumericToAlphanumeric_(0);
	}
}
