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

public class bin implements CobolRunnable {
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
    return bin_(0, fields);
  }

  @Override
  public void cancel() {
    bin_(-1);
  }

  @Override
  public boolean isActive() {
    return false;
  }

  int bin_ (int entry, AbstractCobolField... fields )
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
    b_5.setSwpU64Binary (100);
    /* PROCEDURE DIVISION */
    try{
      CobolStopRunException.dummy();
      CobolGoBackException.dummy();

      /* Entry bin */

      /* MAIN SECTION */

      /* MAIN PARAGRAPH */

      /* bin.cob:7: COMPUTE */
      cobolErrorOnExitFlag = true;
      {
        {
          {
            d0.setField (f_5);
        	//System.out.println("d0 = " + d0.getValue());
            d1.set (23);
        	//System.out.println("d1 = " + d1.getValue());
            d0.add (d1);
        	//System.out.println("d0 + d1 = " + d0.getValue());
            d0.getField (f_5, 4);
          }
        }
      }
      /* bin.cob:8: DISPLAY */
      {
        CobolTerminal.display (0, 1, 1, f_5);
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
    new bin().bin_(0);
  }

  public bin()
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

      /* PROGRAM-ID : bin */
      b_1 = new CobolDataStorage(4);	/* RETURN-CODE */
      b_5 = new CobolDataStorage(8);	/* C */

      /* End of data storage */


      initAttr();

      /* Fields */

      /* PROGRAM-ID : bin */
      f_5	= CobolFieldFactory.makeCobolField(8, b_5, a_1);	/* C */

      /* End of fields */


    } catch(Exception e) {
      e.printStackTrace();
    }
  }

  private void initAttr() {
    /* Attributes */

    a_1 = new CobolFieldAttribute (17, 11, 0, 32, null);

  }

  /* Decimal structures */

  private CobolDecimal d0;
  private CobolDecimal d1;

  /* Data storage */

  /* PROGRAM-ID : bin */
  private CobolDataStorage b_1;	/* RETURN-CODE */
  private CobolDataStorage b_5;	/* C */

  /* End of data storage */


  /* Fields */

  /* PROGRAM-ID : bin */
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
