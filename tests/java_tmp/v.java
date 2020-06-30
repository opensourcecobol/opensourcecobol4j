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

public class v implements CobolRunnable {

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
    return v_(0);
  }

  @Override
  public void cancel() {
    v_(1);
  }

  @Override
  public boolean isActive() {
    return false;
  }

  int v_ (int entry)
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
    this.cobolPushCallStackList("v");

    b_1.set(0);
    b_5.setBytes ("123", 3);
    b_6.setBytes ("45v", 3);
    f_7.moveFrom (c_1);
    f_8.moveFrom (c_2);
    f_9.moveFrom (c_1);
    f_10.moveFrom (c_2);
    /* PROCEDURE DIVISION */

    /* Entry v */

    /* MAIN SECTION */

    /* MAIN PARAGRAPH */

    /* v.cob:14: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_5);
    }
    /* v.cob:15: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_6);
    }
    /* v.cob:16: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_7);
    }
    /* v.cob:17: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_8);
    }
    /* v.cob:18: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_9);
    }
    /* v.cob:19: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_10);
    }
    CobolTerminal.closeWriter();
    return 0;
  }

  public static void main(String[] args)
  {
    new v().v_(0);
  }

  /* Data storage */

  /* PROGRAM-ID : v */
  private CobolDataStorage b_1;	/* RETURN-CODE */
  private CobolDataStorage b_5;	/* A91 */
  private CobolDataStorage b_6;	/* A92 */
  private CobolDataStorage b_7;	/* AC1 */
  private CobolDataStorage b_8;	/* AC2 */
  private CobolDataStorage b_9;	/* AB1 */
  private CobolDataStorage b_10;	/* AB2 */

  /* End of data storage */


  /* Attributes */

  private CobolFieldAttribute a_1;
  private CobolFieldAttribute a_2;
  private CobolFieldAttribute a_3;
  private CobolFieldAttribute a_4;
  private CobolFieldAttribute a_5;
  private CobolFieldAttribute a_6;
  private CobolFieldAttribute a_7;

  /* Fields */

  /* PROGRAM-ID : v */
  private AbstractCobolField f_5;	/* A91 */
  private AbstractCobolField f_6;	/* A92 */
  private AbstractCobolField f_7;	/* AC1 */
  private AbstractCobolField f_8;	/* AC2 */
  private AbstractCobolField f_9;	/* AB1 */
  private AbstractCobolField f_10;	/* AB2 */

  /* End of fields */


  /* Constants */

  private AbstractCobolField c_1;
  private AbstractCobolField c_2;


  public v()
  {
    init();
  }

  public void init()
  {
    try {
      /* Data storage */

      /* PROGRAM-ID : v */
      b_1 = new CobolDataStorage(4);	/* RETURN-CODE */
      b_5 = new CobolDataStorage(3);	/* A91 */
      b_6 = new CobolDataStorage(3);	/* A92 */
      b_7 = new CobolDataStorage(2);	/* AC1 */
      b_8 = new CobolDataStorage(2);	/* AC2 */
      b_9 = new CobolDataStorage(2);	/* AB1 */
      b_10 = new CobolDataStorage(2);	/* AB2 */

      /* End of data storage */


      /* Attributes */

      a_7 = new CobolFieldAttribute (16, 3, 1, 1, null);
      a_6 = new CobolFieldAttribute (17, 3, 1, 33, null);
      a_5 = new CobolFieldAttribute (17, 3, 1, 32, null);
      a_4 = new CobolFieldAttribute (16, 3, 1, 3, null);
      a_3 = new CobolFieldAttribute (18, 3, 1, 1, null);
      a_2 = new CobolFieldAttribute (16, 3, 1, 0, null);
      a_1 = new CobolFieldAttribute (18, 3, 1, 0, null);

      /* Fields */

      /* PROGRAM-ID : v */
      f_5	= CobolFieldFactory.makeCobolField(3, b_5, a_2);	/* A91 */
      f_6	= CobolFieldFactory.makeCobolField(3, b_6, a_7);	/* A92 */
      f_7	= CobolFieldFactory.makeCobolField(2, b_7, a_1);	/* AC1 */
      f_8	= CobolFieldFactory.makeCobolField(2, b_8, a_3);	/* AC2 */
      f_9	= CobolFieldFactory.makeCobolField(2, b_9, a_5);	/* AB1 */
      f_10	= CobolFieldFactory.makeCobolField(2, b_10, a_6);	/* AB2 */

      /* End of fields */


      /* Constants */

      c_2	= CobolFieldFactory.makeCobolField(4, "456-", a_4);
      c_1	= CobolFieldFactory.makeCobolField(3, "123", a_2);

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
