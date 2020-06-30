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

public class ptimes implements CobolRunnable {

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
    return ptimes_(0);
  }

  @Override
  public void cancel() {
    ptimes_(1);
  }

  @Override
  public boolean isActive() {
    return false;
  }

  int ptimes_ (int entry)
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
    this.cobolPushCallStackList("ptimes");

    b_1.set(0);
    f_5.moveFrom (3);
    f_6.moveFrom (1);
    /* PROCEDURE DIVISION */

    /* Entry ptimes */

    /* MAIN SECTION */

    /* MAIN PARAGRAPH */

    /* ptimes.cob:8: PERFORM */
    {
      for (int n0 = 3; n0 > 0; n0--)
        {
          {
            /* ptimes.cob:9: DISPLAY */
            {
              CobolTerminal.display (0, 1, 1, c_1);
            }
          }
        }
    }
    /* ptimes.cob:11: PERFORM */
    {
      for (int n1 = f_5.getInt(); n1 > 0; n1--)
        {
          {
            /* ptimes.cob:12: DISPLAY */
            {
              CobolTerminal.display (0, 1, 2, c_2, f_6);
            }
            /* ptimes.cob:13: COMPUTE */
            cobolErrorOnExitFlag = true;
            {
              {
                {
                  f_6.checkNumeric ("D");
                  d0.set (f_6.getInt());
                  d1.set (2);
                  d0.mul (d1);
                  d0.getField (f_6, 4);
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
    new ptimes().ptimes_(0);
  }

  public ptimes()
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

      /* PROGRAM-ID : ptimes */
      b_1 = new CobolDataStorage(4);	/* RETURN-CODE */
      b_5 = new CobolDataStorage(3);	/* C */
      b_6 = new CobolDataStorage(3);	/* D */

      /* End of data storage */


      initAttr();

      /* Fields */

      /* PROGRAM-ID : ptimes */
      f_5	= CobolFieldFactory.makeCobolField(3, b_5, a_1);	/* C */
      f_6	= CobolFieldFactory.makeCobolField(3, b_6, a_1);	/* D */

      /* End of fields */


      /* Constants */

      c_1	= CobolFieldFactory.makeCobolField(5, "Hello", a_2);
      c_2	= CobolFieldFactory.makeCobolField(5, "World", a_2);

    } catch(Exception e) {
      e.printStackTrace();
    }
  }

  private void initAttr() {
    /* Attributes */

    a_1 = new CobolFieldAttribute (18, 5, 0, 1, null);
    a_2 = new CobolFieldAttribute (33, 0, 0, 0, null);

  }

  /* Decimal structures */

  private CobolDecimal d0;
  private CobolDecimal d1;

  /* Data storage */

  /* PROGRAM-ID : ptimes */
  private CobolDataStorage b_1;	/* RETURN-CODE */
  private CobolDataStorage b_5;	/* C */
  private CobolDataStorage b_6;	/* D */

  /* End of data storage */


  /* Fields */

  /* PROGRAM-ID : ptimes */
  private AbstractCobolField f_5;	/* C */
  private AbstractCobolField f_6;	/* D */

  /* End of fields */


  /* Constants */

  private AbstractCobolField c_2;
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
