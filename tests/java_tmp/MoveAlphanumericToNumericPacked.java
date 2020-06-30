//このプログラムは動作するものの,正しいかどうかは不明
package test;

import java.io.UnsupportedEncodingException;

import jp.osscons.opensourcecobol.libcobj.data.CobolAlphanumericField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldAttribute;
import jp.osscons.opensourcecobol.libcobj.data.CobolNumericPackedField;
import jp.osscons.opensourcecobol.libcobj.termio.CobolTerminal;

public class MoveAlphanumericToNumericPacked {
	private CobolDataStorage b_5;
	private CobolDataStorage b_6;

	private CobolFieldAttribute a_1;
	private CobolFieldAttribute a_2;

	private CobolNumericPackedField f_5;
	private CobolAlphanumericField f_6;

	public MoveAlphanumericToNumericPacked() {
		this.init();
	}

	private void init() {
		this.b_5 = new CobolDataStorage(3);
		this.b_6 = new CobolDataStorage(5);

		this.a_1 = new CobolFieldAttribute(18, 5, 0, 0, null);
		this.a_2 = new CobolFieldAttribute(33, 0, 0, 0, null);

		this.f_5 = new CobolNumericPackedField(3, b_5, a_1);
		this.f_6 = new CobolAlphanumericField(5, b_6, a_2);
	}

	/* Functions */
	public int MoveAlphanumericToNumericPacked_ (int entry) {
		try {
			CobolTerminal.openWriter();
		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
		}
		this.f_5.moveFrom(12);
		this.f_6.moveFrom("00345");

		System.out.println("想定される出力");
		System.out.println("00345");
		System.out.println("実際の出力");

		this.f_5.moveFrom(f_6);

		CobolTerminal.display (true, true, f_5);
		CobolTerminal.closeWriter();
		return 0;
	}

	public static void main(String[] args) {
		new MoveAlphanumericToNumericPacked().MoveAlphanumericToNumericPacked_(0);
	}
}
