package test;

import jp.osscons.opensourcecobol.libcobj.Const;
import jp.osscons.opensourcecobol.libcobj.call.CobolRunnable;
import jp.osscons.opensourcecobol.libcobj.common.CobolCallParams;
import jp.osscons.opensourcecobol.libcobj.common.CobolCheck;
import jp.osscons.opensourcecobol.libcobj.common.CobolFrame;
import jp.osscons.opensourcecobol.libcobj.common.CobolModule;
import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldAttribute;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldFactory;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolGoBackException;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolStopRunException;
import jp.osscons.opensourcecobol.libcobj.termio.CobolTerminal;

public class srch implements CobolRunnable {

  private boolean initialized;
  private CobolModule cobolCurrentModule;
  private CobolFrame frame;
  private CobolFrame[] frameStack;
  private boolean cobolInitialized = false;
  private CobolCallParams cobolSaveCallParams = null;
  private CobolCallParams cobolCallParams = null;
  private boolean cobolErrorOnExitFlag;

  private CobolRunnable cob_unifunc;


  @Override
  public int run(AbstractCobolField... fields) {
    return srch_(0);
  }

  @Override
  public void cancel() {
    srch_(1);
  }

  @Override
  public boolean isActive() {
    return false;
  }

  int srch_ (int entry)
  {
    this.initialized = false;

    AbstractCobolField[] cobolUserParameters = new AbstractCobolField[Const.COB_MAX_FIELD_PARAMS];
    CobolModule module = new CobolModule(null, null, null, null, cobolUserParameters, 0, '.', '\\', ',', 1, 1, 1, 0, null );

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
    for(int i=0; i<frameStack.length; ++i){
      frameStack[i] = new CobolFrame();
    }
    this.frame = frameStack[0];
    frameStack[0].setPerformThrough(0);

    /* Push module stack */
    module.next = cobolCurrentModule;
    cobolCurrentModule = module;
    this.cobolPushCallStackList("srch");

    b_1.set(0);
    b_7.set(1);
    for (int i1 = 1; i1 <= 40; i1++)
      {
        b_5.getSubDataStorage(26 * (i1 - 1)).fillBytes((byte)(32), 20);
        b_5.getSubDataStorage(20).getSubDataStorage(26 * (i1 - 1)).fillBytes((byte)(48), 6);
      }
    b_11.fillBytes (48, 2);
    /* PROCEDURE DIVISION */
    try{
      CobolStopRunException.dummy();
      CobolGoBackException.dummy();

      /* Entry srch */

      /* MAIN SECTION */

      /* MAIN PARAGRAPH */

      /* srch.cob:12: SET */
      {
        b_7.set(20);
      }
      /* srch.cob:13: MOVE */
      {
                CobolCheck.checkSubscript (b_7, 1, 40, "MATH");
CobolFieldFactory.makeCobolField(3, b_5.getSubDataStorage(20).getSubDataStorage(26 * (b_7.intValue() - 1)), a_1).moveFrom (c_1);
      }
      /* srch.cob:20: DISPLAY */
      {
        CobolTerminal.display (0, 1, 1, f_11);
      }
      /* srch.cob:21: STOP */
      {
        CobolStopRunException.throwException (b_1);
      }
    } catch(CobolGoBackException e) {
      return e.getReturnCode();
    } catch(CobolStopRunException e) {
      System.exit(e.getReturnCode());
    }
    return 0;
  }

  public static void main(String[] args)
  {
    new srch().srch_(0);
  }

  public srch()
  {
    init();
  }

  public void init()
  {
    try {
      /* Data storage */

      RETURN_CODE = 0;
      cob_unifunc = null;

      /* PROGRAM-ID : srch */
      b_1 = new CobolDataStorage(4);	/* RETURN-CODE */
      b_5 = new CobolDataStorage(1040);	/* CCLASS */
      b_7 = new CobolDataStorage(4);	/* K */
      b_11 = new CobolDataStorage(2);	/* NUM */

      /* End of data storage */


      initAttr();

      /* Fields */

      /* PROGRAM-ID : srch */
      f_11	= CobolFieldFactory.makeCobolField(2, b_11, a_2);	/* NUM */

      /* End of fields */


      /* Constants */

      c_1	= CobolFieldFactory.makeCobolField(3, "100", a_1);

    } catch(Exception e) {
      e.printStackTrace();
    }
  }

  private void initAttr() {
    /* Attributes */

    a_1 = new CobolFieldAttribute (16, 3, 0, 0, null);
    a_2 = new CobolFieldAttribute (16, 2, 0, 0, null);

  }

  /* Data storage */

  private int RETURN_CODE;

  /* PROGRAM-ID : srch */
  private CobolDataStorage b_1;	/* RETURN-CODE */
  private CobolDataStorage b_5;	/* CCLASS */
  private CobolDataStorage b_7;	/* K */
  private CobolDataStorage b_11;	/* NUM */

  /* End of data storage */


  /* Fields */

  /* PROGRAM-ID : srch */
  private AbstractCobolField f_11;	/* NUM */

  /* End of fields */


  /* Constants */

  private AbstractCobolField c_1;

  /* Attributes */

  private CobolFieldAttribute a_2;
  private CobolFieldAttribute a_1;


  private void cobolPushCallStackList(String programId)
  {
  }

  private void cobolFatalError(int errorCode)
  {
  }

  private void cobolCheckVersion(String sourceFile, int packageVersion, int patchVersion)
  {
  }

  private void cobolSetCancel(String programId, Object a, Object b)
  {
  }

  private void cobolPopCallStackList()
  {
  }

  private static byte[] makeByteArray(byte ...bytes) {
    return bytes;
  }
}
