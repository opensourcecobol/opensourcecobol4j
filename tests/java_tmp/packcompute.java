package test;
import jp.osscons.opensourcecobol.libcobj.call.CobolRunnable;
import jp.osscons.opensourcecobol.libcobj.common.CobolCallParams;
import jp.osscons.opensourcecobol.libcobj.common.CobolFrame;
import jp.osscons.opensourcecobol.libcobj.common.CobolModule;
import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.data.CobolDecimal;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldAttribute;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldFactory;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolGoBackException;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolStopRunException;
import jp.osscons.opensourcecobol.libcobj.termio.CobolTerminal;

public class packcompute implements CobolRunnable {
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
    return packcompute_(0, fields);
  }

  @Override
  public void cancel() {
    packcompute_(-1);
  }

  @Override
  public boolean isActive() {
    return false;
  }

  int packcompute_ (int entry, AbstractCobolField... fields )
  {
    this.initialized = false;


    CobolModule module = new CobolModule(null, null, null, null, 0, '.', '\\', ',', 1, 1, 1, 0, null );

    /* Start of function code */

    /* CANCEL callback handling */
    if (entry < 0) {
    	if (!this.initialized) {
    		return 0;
    	}
    	d0.clear();
    	d0.setScale(0);
    	d1.clear();
    	d1.setScale(0);
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
    f_5.moveFrom (100);
    f_6.moveFrom (100);
    /* PROCEDURE DIVISION */
    try{
      CobolStopRunException.dummy();
      CobolGoBackException.dummy();

      /* Entry packcompute */

      /* MAIN SECTION */

      /* MAIN PARAGRAPH */

      /* packcompute.cob:8: COMPUTE */
      cobolErrorOnExitFlag = true;
      {
        {
          {
            f_5.checkNumeric ("C");
            d0.set (f_5.getInt());
            d1.set (23);
            d0.add (d1);
            d0.getField (f_5, 4);
          }
        }
      }
      /* packcompute.cob:9: DISPLAY */
      {
        CobolTerminal.display (0, 1, 1, f_5);
      }
      /* packcompute.cob:11: COMPUTE */
      cobolErrorOnExitFlag = true;
      {
        {
          {
            f_6.checkNumeric ("D");
            d0.setField (f_6);
            d1.set (23);
            d0.add (d1);
            d0.getField (f_6, 4);
          }
        }
      }
      /* packcompute.cob:12: DISPLAY */
      {
        CobolTerminal.display (0, 1, 1, f_6);
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
    new packcompute().packcompute_(0);
  }

  public packcompute()
  {
    init();
  }

  public void init()
  {
    try {
      /* Decimal structures */

      d0 = new CobolDecimal();
      d1 = new CobolDecimal();

      /* Data storage */

      cob_unifunc = null;

      /* PROGRAM-ID : packcompute */
      b_1 = new CobolDataStorage(4);	/* RETURN-CODE */
      b_5 = new CobolDataStorage(3);	/* C */
      b_6 = new CobolDataStorage(6);	/* D */

      /* End of data storage */


      initAttr();

      /* Fields */

      /* PROGRAM-ID : packcompute */
      f_5	= CobolFieldFactory.makeCobolField(3, b_5, a_1);	/* C */
      f_6	= CobolFieldFactory.makeCobolField(6, b_6, a_2);	/* D */

      /* End of fields */


    } catch(Exception e) {
      e.printStackTrace();
    }
  }

  private void initAttr() {
    /* Attributes */

    a_1 = new CobolFieldAttribute (18, 5, 0, 0, null);
    a_2 = new CobolFieldAttribute (18, 11, 0, 0, null);

  }

  /* Decimal structures */

  private CobolDecimal d0;
  private CobolDecimal d1;

  /* Data storage */

  /* PROGRAM-ID : packcompute */
  private CobolDataStorage b_1;	/* RETURN-CODE */
  private CobolDataStorage b_5;	/* C */
  private CobolDataStorage b_6;	/* D */

  /* End of data storage */


  /* Fields */

  /* PROGRAM-ID : packcompute */
  private AbstractCobolField f_5;	/* C */
  private AbstractCobolField f_6;	/* D */

  /* End of fields */


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
