package test;

import java.io.UnsupportedEncodingException;

import jp.osscons.opensourcecobol.libcobj.Const;
import jp.osscons.opensourcecobol.libcobj.call.CobolRunnable;
import jp.osscons.opensourcecobol.libcobj.common.CobolCallParams;
import jp.osscons.opensourcecobol.libcobj.common.CobolFrame;
import jp.osscons.opensourcecobol.libcobj.common.CobolModule;
import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.data.CobolDecimal;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldAttribute;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldFactory;
import jp.osscons.opensourcecobol.libcobj.termio.CobolTerminal;

public class puntil implements CobolRunnable {

  private boolean initialized;
  private CobolModule cobolCurrentModule;
  private CobolFrame frame;
  private CobolFrame[] frameStack;
  private boolean cobolInitialized = false;
  private CobolCallParams cobolSaveCallParams = null;
  private CobolCallParams cobolCallParams = null;
  private boolean cobolErrorOnExitFlag;

  @Override
  public int run(AbstractCobolField... fields) {
    return puntil_(0);
  }

  @Override
  public void cancel() {
    puntil_(1);
  }

  @Override
  public boolean isActive() {
    return false;
  }

  int puntil_ (int entry)
  {
    try {
      CobolTerminal.openWriter();
    } catch(UnsupportedEncodingException e) {
      e.printStackTrace();
    }

    this.initialized = false;

    AbstractCobolField[] cobolUserParameters = new AbstractCobolField[Const.COB_MAX_FIELD_PARAMS];
    CobolModule module = new CobolModule(null, null, null, null, cobolUserParameters, 0, '.', '\\', ',', 1, 1, 1, 0, null );

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
    module.next = cobolCurrentModule;
    cobolCurrentModule = module;
    this.cobolPushCallStackList("puntil");

    b_1.set(0);
    b_5.fillBytes (48, 5);
    /* PROCEDURE DIVISION */

    /* Entry puntil */

    /* MAIN SECTION */

    /* MAIN PARAGRAPH */

    /* puntil.cob:7: PERFORM */
    {
      for (;;)
        {
          if (((int)f_5.compareTo (c_1) >= 0))
            break;
          {
            /* puntil.cob:8: DISPLAY */
            {
              CobolTerminal.display (0, 1, 1, f_5);
            }
            /* puntil.cob:9: COMPUTE */
            cobolErrorOnExitFlag = true;
            {
              {
                {
                  f_5.checkNumeric ("C");
                  d0.set (f_5.getInt());
                  d1.set (1);
                  d0.add (d1);
                  d0.getField (f_5, 4);
                }
              }
            }
          }
        }
    }
    CobolTerminal.closeWriter();
    return 0;
  }

  public static void main(String[] args)
  {
    new puntil().puntil_(0);
  }

  /* Decimal structures */

  private CobolDecimal d0;
  private CobolDecimal d1;

  /* Data storage */

  /* PROGRAM-ID : puntil */
  private CobolDataStorage b_1;	/* RETURN-CODE */
  private CobolDataStorage b_5;	/* C */

  /* End of data storage */


  /* Attributes */

  private CobolFieldAttribute a_1;
  private CobolFieldAttribute a_2;

  /* Fields */

  /* PROGRAM-ID : puntil */
  private AbstractCobolField f_5;	/* C */

  /* End of fields */


  /* Constants */

  private AbstractCobolField c_1;


  public puntil()
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

      /* PROGRAM-ID : puntil */
      b_1 = new CobolDataStorage(4);	/* RETURN-CODE */
      b_5 = new CobolDataStorage(5);	/* C */

      /* End of data storage */


      /* Attributes */

      a_2 = new CobolFieldAttribute (16, 2, 0, 0, null);
      a_1 = new CobolFieldAttribute (16, 5, 0, 1, null);

      /* Fields */

      /* PROGRAM-ID : puntil */
      f_5	= CobolFieldFactory.makeCobolField(5, b_5, a_1);	/* C */

      /* End of fields */


      /* Constants */

      c_1	= CobolFieldFactory.makeCobolField(2, "10", a_2);

    } catch(Exception e) {
      e.printStackTrace();
    }
  }

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

