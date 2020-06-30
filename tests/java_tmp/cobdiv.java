package test;

import jp.osscons.opensourcecobol.libcobj.call.CobolRunnable;
import jp.osscons.opensourcecobol.libcobj.common.CobolCallParams;
import jp.osscons.opensourcecobol.libcobj.common.CobolFrame;
import jp.osscons.opensourcecobol.libcobj.common.CobolModule;
import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldAttribute;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldFactory;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolGoBackException;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolStopRunException;
import jp.osscons.opensourcecobol.libcobj.termio.CobolTerminal;

public class cobdiv implements CobolRunnable {
  interface __B {
    public AbstractCobolField run();
  }

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
    return cobdiv_(0, fields);
  }

  @Override
  public void cancel() {
    cobdiv_(-1);
  }

  @Override
  public boolean isActive() {
    return false;
  }

  int cobdiv_ (int entry, AbstractCobolField... fields )
  {
    this.initialized = false;


    CobolModule module = new CobolModule(null, null, null, null, 0, '.', '\\', ',', 1, 1, 1, 0, null );

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
    CobolModule.push (module);

    b_1.set(0);
    b_5.setBytes ("00003", 5);
    b_6.setBytes ("00013", 5);
    b_7.fillBytes((byte)(48), 5);
    b_8.fillBytes((byte)(48), 5);
    /* PROCEDURE DIVISION */
    try{
      CobolStopRunException.dummy();
      CobolGoBackException.dummy();

      /* Entry cobdiv */

      /* MAIN SECTION */

      /* MAIN PARAGRAPH */

      /* cobdiv.cob:10: DIVIDE */
      cobolErrorOnExitFlag = true;
      {
        f_6.divQuotient (f_5, f_7, 4);
        f_8.divRemainder (4);
      }
      /* cobdiv.cob:11: DISPLAY */
      {
        CobolTerminal.display (0, 1, 1, f_7);
      }
      /* cobdiv.cob:12: DISPLAY */
      {
        CobolTerminal.display (0, 1, 1, f_8);
      }
    } catch(CobolGoBackException e) {
      return e.getReturnCode();
    } catch(CobolStopRunException e) {
      System.exit(e.getReturnCode());
    }
    /* Program return */
    return b_1.intValue ();
  }

  public static void main(String[] args)
  {
    new cobdiv().cobdiv_(0);
  }

  public cobdiv()
  {
    init();
  }

  public void init()
  {
    try {
      /* Data storage */

      cob_unifunc = null;

      /* PROGRAM-ID : cobdiv */
      b_1 = new CobolDataStorage(4);	/* RETURN-CODE */
      b_5 = new CobolDataStorage(5);	/* C */
      b_6 = new CobolDataStorage(5);	/* D */
      b_7 = new CobolDataStorage(5);	/* X */
      b_8 = new CobolDataStorage(5);	/* Y */

      /* End of data storage */


      initAttr();

      /* Fields */

      /* PROGRAM-ID : cobdiv */
      f_5	= CobolFieldFactory.makeCobolField(5, b_5, a_1);	/* C */
      f_6	= CobolFieldFactory.makeCobolField(5, b_6, a_1);	/* D */
      f_7	= CobolFieldFactory.makeCobolField(5, b_7, a_1);	/* X */
      f_8	= CobolFieldFactory.makeCobolField(5, b_8, a_1);	/* Y */

      /* End of fields */


    } catch(Exception e) {
      e.printStackTrace();
    }
  }

  private void initAttr() {
    /* Attributes */

    a_1 = new CobolFieldAttribute (16, 5, 0, 0, null);

  }

  /* Data storage */

  /* PROGRAM-ID : cobdiv */
  private CobolDataStorage b_1;	/* RETURN-CODE */
  private CobolDataStorage b_5;	/* C */
  private CobolDataStorage b_6;	/* D */
  private CobolDataStorage b_7;	/* X */
  private CobolDataStorage b_8;	/* Y */

  /* End of data storage */


  /* Fields */

  /* PROGRAM-ID : cobdiv */
  private AbstractCobolField f_5;	/* C */
  private AbstractCobolField f_6;	/* D */
  private AbstractCobolField f_7;	/* X */
  private AbstractCobolField f_8;	/* Y */

  /* End of fields */


  /* Attributes */

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
