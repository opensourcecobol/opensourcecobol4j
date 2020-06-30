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

public class tvmove implements CobolRunnable {

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
    return tvmove_(0);
  }

  @Override
  public void cancel() {
    tvmove_(1);
  }

  @Override
  public boolean isActive() {
    return false;
  }

  int tvmove_ (int entry)
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
    this.cobolPushCallStackList("tvmove");

    b_1.set(0);
    b_5.setBytes ("123", 3);
    b_6.setBytes ("123", 3);
    b_7.setBytes ("45v", 3);
    b_8.setBytes ("45v", 3);
    f_9.moveFrom (123);
    CobolFieldFactory.makeCobolField(2, b_10, a_2).moveFrom (c_1);
    f_11.moveFrom (-456);
    f_12.moveFrom (c_2);
    b_13.setSwpU16Binary (123);
    CobolFieldFactory.makeCobolField(2, b_14, a_7).moveFrom (c_1);
    b_15.setSwpS16Binary (-456);
    CobolFieldFactory.makeCobolField(2, b_16, a_8).moveFrom (c_2);
    b_17.setBytes ("04567", 5);
    b_18.setBytes ("04567", 5);
    /* PROCEDURE DIVISION */

    /* Entry tvmove */

    /* MAIN SECTION */

    /* MAIN PARAGRAPH */

    /* tvmove.cob:24: MOVE */
    {
      f_5.checkMoveStrNum (f_12);
      f_12.moveFrom (f_5);
    }
    /* tvmove.cob:25: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_12);
    }
    /* tvmove.cob:27: MOVE */
    {
      f_7.checkMoveStrNum (f_12);
      f_12.moveFrom (f_7);
    }
    /* tvmove.cob:28: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_12);
    }
    /* tvmove.cob:30: MOVE */
    {
      f_7.checkMoveStrNum (f_12);
      f_12.moveFrom (f_7);
    }
    /* tvmove.cob:31: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_12);
    }
    /* tvmove.cob:33: MOVE */
    {
      f_9.checkMoveStrNum (f_12);
      f_12.moveFrom (f_9);
    }
    /* tvmove.cob:34: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_12);
    }
    /* tvmove.cob:36: MOVE */
    {
      f_11.checkMoveStrNum (f_12);
      f_12.moveFrom (f_11);
    }
    /* tvmove.cob:37: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_12);
    }
    /* tvmove.cob:39: MOVE */
    {
      f_13.checkMoveStrNum (f_12);
      f_12.moveFrom (f_13);
    }
    /* tvmove.cob:40: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_12);
    }
    /* tvmove.cob:42: MOVE */
    {
      f_15.checkMoveStrNum (f_12);
      f_12.moveFrom (f_15);
    }
    /* tvmove.cob:43: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_12);
    }
    CobolTerminal.closeWriter();
    return 0;
  }

  public static void main(String[] args)
  {
    new tvmove().tvmove_(0);
  }

  /* Data storage */

  /* PROGRAM-ID : tvmove */
  private CobolDataStorage b_1;	/* RETURN-CODE */
  private CobolDataStorage b_5;	/* A91 */
  private CobolDataStorage b_6;	/* A92 */
  private CobolDataStorage b_7;	/* A93 */
  private CobolDataStorage b_8;	/* A94 */
  private CobolDataStorage b_9;	/* AC1 */
  private CobolDataStorage b_10;	/* AC2 */
  private CobolDataStorage b_11;	/* AC3 */
  private CobolDataStorage b_12;	/* AC4 */
  private CobolDataStorage b_13;	/* AB1 */
  private CobolDataStorage b_14;	/* AB2 */
  private CobolDataStorage b_15;	/* AB3 */
  private CobolDataStorage b_16;	/* AB4 */
  private CobolDataStorage b_17;	/* AX */
  private CobolDataStorage b_18;	/* AX2 */

  /* End of data storage */


  /* Attributes */

  private CobolFieldAttribute a_1;
  private CobolFieldAttribute a_2;
  private CobolFieldAttribute a_3;
  private CobolFieldAttribute a_4;
  private CobolFieldAttribute a_5;
  private CobolFieldAttribute a_6;
  private CobolFieldAttribute a_7;
  private CobolFieldAttribute a_8;
  private CobolFieldAttribute a_9;
  private CobolFieldAttribute a_10;
  private CobolFieldAttribute a_11;
  private CobolFieldAttribute a_12;

  /* Fields */

  /* PROGRAM-ID : tvmove */
  private AbstractCobolField f_5;	/* A91 */
  private AbstractCobolField f_7;	/* A93 */
  private AbstractCobolField f_9;	/* AC1 */
  private AbstractCobolField f_11;	/* AC3 */
  private AbstractCobolField f_12;	/* AC4 */
  private AbstractCobolField f_13;	/* AB1 */
  private AbstractCobolField f_15;	/* AB3 */

  /* End of fields */


  /* Constants */

  private AbstractCobolField c_1;
  private AbstractCobolField c_2;


  public tvmove()
  {
    init();
  }

  public void init()
  {
    try {
      /* Data storage */

      /* PROGRAM-ID : tvmove */
      b_1 = new CobolDataStorage(4);	/* RETURN-CODE */
      b_5 = new CobolDataStorage(3);	/* A91 */
      b_6 = new CobolDataStorage(3);	/* A92 */
      b_7 = new CobolDataStorage(3);	/* A93 */
      b_8 = new CobolDataStorage(3);	/* A94 */
      b_9 = new CobolDataStorage(2);	/* AC1 */
      b_10 = new CobolDataStorage(2);	/* AC2 */
      b_11 = new CobolDataStorage(2);	/* AC3 */
      b_12 = new CobolDataStorage(2);	/* AC4 */
      b_13 = new CobolDataStorage(2);	/* AB1 */
      b_14 = new CobolDataStorage(2);	/* AB2 */
      b_15 = new CobolDataStorage(2);	/* AB3 */
      b_16 = new CobolDataStorage(2);	/* AB4 */
      b_17 = new CobolDataStorage(5);	/* AX */
      b_18 = new CobolDataStorage(5);	/* AX2 */

      /* End of data storage */


      /* Attributes */

      a_12 = new CobolFieldAttribute (17, 3, 0, 33, null);
      a_11 = new CobolFieldAttribute (17, 3, 0, 32, null);
      a_10 = new CobolFieldAttribute (16, 3, 0, 1, null);
      a_9 = new CobolFieldAttribute (16, 3, 0, 0, null);
      a_8 = new CobolFieldAttribute (17, 3, 1, 33, null);
      a_7 = new CobolFieldAttribute (17, 3, 1, 32, null);
      a_6 = new CobolFieldAttribute (16, 3, 1, 3, null);
      a_5 = new CobolFieldAttribute (18, 3, 1, 1, null);
      a_4 = new CobolFieldAttribute (18, 3, 0, 1, null);
      a_3 = new CobolFieldAttribute (16, 3, 1, 0, null);
      a_2 = new CobolFieldAttribute (18, 3, 1, 0, null);
      a_1 = new CobolFieldAttribute (18, 3, 0, 0, null);

      /* Fields */

      /* PROGRAM-ID : tvmove */
      f_5	= CobolFieldFactory.makeCobolField(3, b_5, a_9);	/* A91 */
      f_7	= CobolFieldFactory.makeCobolField(3, b_7, a_10);	/* A93 */
      f_9	= CobolFieldFactory.makeCobolField(2, b_9, a_1);	/* AC1 */
      f_11	= CobolFieldFactory.makeCobolField(2, b_11, a_4);	/* AC3 */
      f_12	= CobolFieldFactory.makeCobolField(2, b_12, a_5);	/* AC4 */
      f_13	= CobolFieldFactory.makeCobolField(2, b_13, a_11);	/* AB1 */
      f_15	= CobolFieldFactory.makeCobolField(2, b_15, a_12);	/* AB3 */

      /* End of fields */


      /* Constants */

      c_2	= CobolFieldFactory.makeCobolField(4, "456-", a_6);
      c_1	= CobolFieldFactory.makeCobolField(3, "123", a_3);

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
