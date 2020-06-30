package test;


import java.io.UnsupportedEncodingException;

import jp.osscons.opensourcecobol.libcobj.Const;

/**
以下のCOBOLソースコードに対応するjavaコード

        IDENTIFICATION DIVISION.
        PROGRAM-ID. a.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 A PIC X(20) VALUE "Hello World".
        PROCEDURE DIVISION.
        DISPLAY A.
 */

import jp.osscons.opensourcecobol.libcobj.common.CobolCallParams;
import jp.osscons.opensourcecobol.libcobj.common.CobolFrame;
import jp.osscons.opensourcecobol.libcobj.common.CobolModule;
import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolAlphanumericField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldAttribute;
import jp.osscons.opensourcecobol.libcobj.termio.CobolTerminal;

class a {
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

	private CobolFieldAttribute a_1;

	private CobolAlphanumericField f_5;

	public a() {
		this.init();
	}

	private void init() {
		this.b_1 = new CobolDataStorage(4);
		this.b_5 = new CobolDataStorage(20);

		this.a_1 = new CobolFieldAttribute(33, 0, 0, 0, null);

		this.f_5 = new CobolAlphanumericField(20, b_5, a_1);
	}

	/* Functions */
	public int a_ (int entry) {
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
		this.cobolPushCallStackList("a");

		/* Initialize program */
		if (!this.initialized) {
			if (!this.cobolInitialized) {
				this.cobolFatalError (Const.COB_FERROR_INITIALIZED);
			}
			this.cobolCheckVersion (Const.COB_SOURCE_FILE, Const.COB_PACKAGE_VERSION, Const.COB_PATCH_LEVEL);
			if (module.next == null) {
				this.cobolSetCancel ("a", null, null);
			}
			module.setProgramId("a");
			//(*(int *) (b_1)) = 0;
			this.b_1.set(0);
			//memcpy (b_5, "Hello World", 11);
			this.b_5.setString("Hello World");
			//memset (b_5 + 11, 32, 9);
			this.initialized = true;
		}

		cobolSaveCallParams = cobolCallParams;

		/* PROCEDURE DIVISION */

		/* MAIN SECTION */

		/* MAIN PARAGRAPH */

		/* a.cob:7: DISPLAY */
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
		new a().a_(0);
	}
}
