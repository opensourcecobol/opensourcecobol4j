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

public class cmpuint implements CobolRunnable {
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
    return cmpuint_(0, fields);
  }

  @Override
  public void cancel() {
    cmpuint_(-1);
  }

  @Override
  public boolean isActive() {
    return false;
  }

  int cmpuint_ (int entry, AbstractCobolField... fields )
  {
    this.initialized = false;


    CobolModule module = new CobolModule(null, null, null, null, 0, '.', '$', ',', 1, 0, 0, 0, null );

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
    b_5.setSwpU24Binary (10);
    b_6.setSwpU24Binary (3);
    /* PROCEDURE DIVISION */
    try{
      CobolStopRunException.dummy();
      CobolGoBackException.dummy();

      /* Entry cmpuint */

      /* MAIN SECTION */

      /* MAIN PARAGRAPH */

      /* cmpuint.cob:8: IF */
      {
        if (((int)f_5.cmpUint (f_6.getInt()) >  0))
          {
            /* cmpuint.cob:10: DISPLAY */
            {
              CobolTerminal.display (0, 1, 1, c_1);
            }
          }
        else
          {
            /* cmpuint.cob:12: DISPLAY */
            {
              CobolTerminal.display (0, 1, 1, c_2);
            }
          }
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
    new cmpuint().cmpuint_(0);
  }

  public cmpuint()
  {
    init();
  }

  public void init()
  {
    try {
      /* Data storage */

      cob_unifunc = null;

      /* PROGRAM-ID : cmpuint */
      b_1 = new CobolDataStorage(4);	/* RETURN-CODE */
      b_5 = new CobolDataStorage(3);	/* E */
      b_6 = new CobolDataStorage(3);	/* F */

      /* End of data storage */


      initAttr();

      /* Fields */

      /* PROGRAM-ID : cmpuint */
      f_5	= CobolFieldFactory.makeCobolField(3, b_5, a_1);	/* E */
      f_6	= CobolFieldFactory.makeCobolField(3, b_6, a_2);	/* F */

      /* End of fields */


      /* Constants */

      c_1	= CobolFieldFactory.makeCobolField(2, "OK", a_3);
      c_2	= CobolFieldFactory.makeCobolField(2, "NG", a_3);

    } catch(Exception e) {
      e.printStackTrace();
    }
  }

  private void initAttr() {
    /* Attributes */

    a_1 = new CobolFieldAttribute (17, 5, 0, 32, null);
    a_2 = new CobolFieldAttribute (17, 7, 0, 32, null);
    a_3 = new CobolFieldAttribute (33, 0, 0, 0, null);

  }

  /* Data storage */

  /* PROGRAM-ID : cmpuint */
  private CobolDataStorage b_1;	/* RETURN-CODE */
  private CobolDataStorage b_5;	/* E */
  private CobolDataStorage b_6;	/* F */

  /* End of data storage */


  /* Fields */

  /* PROGRAM-ID : cmpuint */
  private AbstractCobolField f_5;	/* E */
  private AbstractCobolField f_6;	/* F */

  /* End of fields */


  /* Constants */

  private AbstractCobolField c_2;
  private AbstractCobolField c_1;

  /* Attributes */

  private CobolFieldAttribute a_3;
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
