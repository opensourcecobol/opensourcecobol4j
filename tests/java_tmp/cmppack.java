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

public class cmppack implements CobolRunnable {
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
    return cmppack_(0, fields);
  }

  @Override
  public void cancel() {
    cmppack_(-1);
  }

  @Override
  public boolean isActive() {
    return false;
  }

  int cmppack_ (int entry, AbstractCobolField... fields )
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
    f_5.setZero ();
    /* PROCEDURE DIVISION */
    try{
      CobolStopRunException.dummy();
      CobolGoBackException.dummy();

      /* Entry cmppack */

      /* MAIN SECTION */

      /* MAIN PARAGRAPH */

      /* cmppack.cob:7: PERFORM */
      {
        f_5.moveFrom (1);
        for (;;)
          {
            if (((int)f_5.cmpInt (6) >= 0))
              break;
            {
              /* cmppack.cob:8: DISPLAY */
              {
                CobolTerminal.display (0, 1, 1, f_5);
              }
            }
            f_5.addInt (2);
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
    new cmppack().cmppack_(0);
  }

  public cmppack()
  {
    init();
  }

  public void init()
  {
    try {
      /* Data storage */

      cob_unifunc = null;

      /* PROGRAM-ID : cmppack */
      b_1 = new CobolDataStorage(4);	/* RETURN-CODE */
      b_5 = new CobolDataStorage(6);	/* C */

      /* End of data storage */


      initAttr();

      /* Fields */

      /* PROGRAM-ID : cmppack */
      f_5	= CobolFieldFactory.makeCobolField(6, b_5, a_1);	/* C */

      /* End of fields */


    } catch(Exception e) {
      e.printStackTrace();
    }
  }

  private void initAttr() {
    /* Attributes */

    a_1 = new CobolFieldAttribute (18, 11, 0, 0, null);

  }

  /* Data storage */

  /* PROGRAM-ID : cmppack */
  private CobolDataStorage b_1;	/* RETURN-CODE */
  private CobolDataStorage b_5;	/* C */

  /* End of data storage */


  /* Fields */

  /* PROGRAM-ID : cmppack */
  private AbstractCobolField f_5;	/* C */

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
