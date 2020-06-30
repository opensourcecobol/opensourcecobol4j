package test;

import jp.osscons.opensourcecobol.libcobj.Const;
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

public class pv implements CobolRunnable {
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
    return pv_(0, fields);
  }

  @Override
  public void cancel() {
    pv_(-1);
  }

  @Override
  public boolean isActive() {
    return false;
  }

  int pv_ (int entry, AbstractCobolField... fields )
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
    this.cobolPushCallStackList("pv");

    b_1.set(0);
    f_5.setZero ();
    /* PROCEDURE DIVISION */
    try{
      CobolStopRunException.dummy();
      CobolGoBackException.dummy();

      /* Entry pv */

      /* MAIN SECTION */

      /* MAIN PARAGRAPH */

      /* pv.cob:8: PERFORM */
      {
        f_5.moveFrom (1);
        for (;;)
          {
            if (((int)f_5.compareTo (c_1) >  0))
              break;
            {
              /* pv.cob:9: DISPLAY */
              {
                CobolTerminal.display (0, 1, 1, f_5);
              }
            }
            //System.out.print("__ f_5 = "); CobolTerminal.display(true, true, f_5);
            f_5.addInt (1);
            //System.out.print("** f_5 = "); CobolTerminal.display(true, true, f_5);
          }
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
    new pv().pv_(0);
  }

  public pv()
  {
    init();
  }

  public void init()
  {
    try {
      /* Data storage */

      cob_unifunc = null;

      /* PROGRAM-ID : pv */
      b_1 = new CobolDataStorage(4);	/* RETURN-CODE */
      b_5 = new CobolDataStorage(3);	/* C */

      /* End of data storage */


      initAttr();

      /* Fields */

      /* PROGRAM-ID : pv */
      f_5	= CobolFieldFactory.makeCobolField(3, b_5, a_1);	/* C */

      /* End of fields */


      /* Constants */

      c_1	= CobolFieldFactory.makeCobolField(2, "10", a_2);

    } catch(Exception e) {
      e.printStackTrace();
    }
  }

  private void initAttr() {
    /* Attributes */

    a_1 = new CobolFieldAttribute (18, 5, 0, 0, null);
    a_2 = new CobolFieldAttribute (16, 2, 0, 0, null);

  }

  /* Data storage */

  /* PROGRAM-ID : pv */
  private CobolDataStorage b_1;	/* RETURN-CODE */
  private CobolDataStorage b_5;	/* C */

  /* End of data storage */


  /* Fields */

  /* PROGRAM-ID : pv */
  private AbstractCobolField f_5;	/* C */

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
