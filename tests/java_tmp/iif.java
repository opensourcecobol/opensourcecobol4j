package test;

import java.io.UnsupportedEncodingException;

import jp.osscons.opensourcecobol.libcobj.Const;
import jp.osscons.opensourcecobol.libcobj.call.CobolRunnable;
import jp.osscons.opensourcecobol.libcobj.common.CobolCallParams;
import jp.osscons.opensourcecobol.libcobj.common.CobolFrame;
import jp.osscons.opensourcecobol.libcobj.common.CobolModule;
import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldAttribute;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldFactory;
import jp.osscons.opensourcecobol.libcobj.termio.CobolTerminal;

public class iif implements CobolRunnable {

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
    return iif_(0);
  }

  @Override
  public void cancel() {
    iif_(1);
  }

  @Override
  public boolean isActive() {
    return false;
  }

  int iif_ (int entry)
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
    this.cobolPushCallStackList("iif");

    b_1.set(0);
    f_5.moveFrom (2);
    /* PROCEDURE DIVISION */

    /* Entry iif */

    /* MAIN SECTION */

    /* MAIN PARAGRAPH */

    /* iif.cob:7: IF */
    {
      if (((int)f_5.compareTo (c_1) >  0))
        {
          /* iif.cob:9: DISPLAY */
          {
            CobolTerminal.display (0, 1, 1, c_2);
          }
        }
      else
        {
          /* iif.cob:11: DISPLAY */
          {
            CobolTerminal.display (0, 1, 1, c_3);
          }
        }
    }
    CobolTerminal.closeWriter();
    return 0;
  }

  public static void main(String[] args)
  {
    new iif().iif_(0);
  }

  public iif()
  {
    init();
  }

  public void init()
  {
    try {
      /* Data storage */

      /* PROGRAM-ID : iif */
      b_1 = new CobolDataStorage(4);	/* RETURN-CODE */
      b_5 = new CobolDataStorage(3);	/* A */

      /* End of data storage */


      initAttr();

      /* Fields */

      /* PROGRAM-ID : iif */
      f_5	= CobolFieldFactory.makeCobolField(3, b_5, a_1);	/* A */

      /* End of fields */


      /* Constants */

      c_1	= CobolFieldFactory.makeCobolField(2, "1-", a_2);
      c_2	= CobolFieldFactory.makeCobolField(2, "OK", a_3);
      c_3	= CobolFieldFactory.makeCobolField(2, "NG", a_3);

    } catch(Exception e) {
      e.printStackTrace();
    }
  }

  private void initAttr() {
    /* Attributes */

    a_1 = new CobolFieldAttribute (18, 5, 0, 0, null);
    a_2 = new CobolFieldAttribute (16, 1, 0, 3, null);
    a_3 = new CobolFieldAttribute (33, 0, 0, 0, null);

  }

  /* Data storage */

  /* PROGRAM-ID : iif */
  private CobolDataStorage b_1;	/* RETURN-CODE */
  private CobolDataStorage b_5;	/* A */

  /* End of data storage */


  /* Fields */

  /* PROGRAM-ID : iif */
  private AbstractCobolField f_5;	/* A */

  /* End of fields */


  /* Constants */

  private AbstractCobolField c_3;
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
