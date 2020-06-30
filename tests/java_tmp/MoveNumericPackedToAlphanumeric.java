package test;

import java.io.UnsupportedEncodingException;

import jp.osscons.opensourcecobol.libcobj.Const;
import jp.osscons.opensourcecobol.libcobj.common.CobolCallParams;
import jp.osscons.opensourcecobol.libcobj.common.CobolFrame;
import jp.osscons.opensourcecobol.libcobj.common.CobolModule;
import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolAlphanumericField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldAttribute;
import jp.osscons.opensourcecobol.libcobj.data.CobolNumericPackedField;
import jp.osscons.opensourcecobol.libcobj.termio.CobolTerminal;

public class MoveNumericPackedToAlphanumeric {
	private boolean initialized;
	private CobolModule cobolCurrentModule;
	private CobolFrame frame;
	private CobolFrame[] frameStack;
	private boolean cobolInitialized;
	private CobolCallParams cobolSaveCallParams = null;
	private CobolCallParams cobolCallParams = null;
	private boolean cobolErrorOnExitFlag;

	private CobolDataStorage b_1;
	private CobolDataStorage b_5;
	private CobolDataStorage b_6;

	private CobolFieldAttribute a_1;
	private CobolFieldAttribute a_2;

	private CobolAlphanumericField f_5;
	private CobolNumericPackedField f_6;

	public MoveNumericPackedToAlphanumeric() {
		this.init();
	}

	private void init() {
		this.b_1 = new CobolDataStorage(4);
		this.b_5 = new CobolDataStorage(5);
		this.b_6 = new CobolDataStorage(3);

		this.a_1 = new CobolFieldAttribute(18, 5, 0, 0, null);
		this.a_2 = new CobolFieldAttribute(33, 0, 0, 0, null);

		this.f_5 = new CobolAlphanumericField(5, b_5, a_2);
		this.f_6 = new CobolNumericPackedField(3, b_6, a_1);
	}

	/* Functions */
	public int MoveNumericPackedToAlphanumeric_ (int entry) {
		try {
			CobolTerminal.openWriter();
		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
		}

		this.initialized = false;
		//TODO 以下要修正
		AbstractCobolField[] cobolUserParameters = new AbstractCobolField[Const.COB_MAX_FIELD_PARAMS];
		CobolModule module = new CobolModule(null, null, null, null, cobolUserParameters, 0, '.', '\\', ',', 1, 1, 1, 0, null);

		/* Start of function code */

		/* CANCEL callback handling */
		if (entry < 0) {
			if (!this.initialized) {
				return 0;
			}
			this.initialized = false;
			return 0;
		}

		/* Initialize frame stack */
		this.frameStack = new CobolFrame[255];
		for(int i=0; i<frameStack.length; ++i) {
			frameStack[i] = new CobolFrame();
		}
		this.frame = frameStack[0];
		this.frameStack[0].setPerformThrough(0);

		/* Push module stack */
		module.next = cobolCurrentModule;
		cobolCurrentModule = module;
		this.cobolPushCallStackList("MoveNumericPackedToNumeric");

		/* Initialize program */
		if (!this.initialized) {
			if (!this.cobolInitialized) {
				this.cobolFatalError (Const.COB_FERROR_INITIALIZED);
			}
			this.cobolCheckVersion (Const.COB_SOURCE_FILE, Const.COB_PACKAGE_VERSION, Const.COB_PATCH_LEVEL);
			if (module.next == null) {
				this.cobolSetCancel ("MoveNumericPackedToAlphanumeric", null, null);
			}
			module.setProgramId("MoveNumericPackedToAlphanumeric");
			this.b_1.set(0);
			this.f_5.moveFrom("12");
			this.f_6.moveFrom(345);
			this.initialized = true;
		}

		cobolSaveCallParams = cobolCallParams;

		/* PROCEDURE DIVISION */

		/* MAIN SECTION */
		this.f_5.moveFrom(f_6);

		System.out.println("想定される出力");
		System.out.println("00345");
		System.out.println("実際の出力");
		/* MAIN PARAGRAPH */
		{
			CobolTerminal.display (true, true, f_5);
		}

		/* Program exit */

		/* Pop module stack */
		cobolPopCallStackList();
		cobolCurrentModule = cobolCurrentModule.getNext();

		/* Program return */
		//return (*(int *) (b_1));

		CobolTerminal.closeWriter();
		return b_1.intValue();
	}
	/* End functions */



	private void cobolPushCallStackList(String programId) {
		//TODO 実装
	}

	private void cobolFatalError (int errorCode) {
		//TODO 実装
	}

	private void cobolCheckVersion(String sourceFile, int packageVersion, int patchVersion) {
		//TODO 実装
	}

	private void cobolSetCancel(String programId, Object a, Object b) {
		//TODO 実装
	}

	private void cobolPopCallStackList() {
		//TODO 実装
	}

	public static void main(String[] args) {
		new MoveNumericPackedToAlphanumeric().MoveNumericPackedToAlphanumeric_(0);
	}
}
