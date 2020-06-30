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

public class vmove implements CobolRunnable {

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
    return vmove_(0);
  }

  @Override
  public void cancel() {
    vmove_(1);
  }

  @Override
  public boolean isActive() {
    return false;
  }

  int vmove_ (int entry)
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
    this.cobolPushCallStackList("vmove");

    b_1.set(0);
    b_5.setBytes ("123", 3);
    b_6.setBytes ("123", 3);
    b_7.setBytes ("45v", 3);
    b_8.setBytes ("45v", 3);
    f_9.moveFrom (123);
    f_10.moveFrom (c_1);
    f_11.moveFrom (-456);
    f_12.moveFrom (c_2);
    b_13.setSwpU16Binary (123);
    f_14.moveFrom (c_1);
    b_15.setSwpS16Binary (-456);
    f_16.moveFrom (c_2);
    b_17.setBytes ("04567", 5);
    b_18.setBytes ("04567", 5);
    /* PROCEDURE DIVISION */

    /* Entry vmove */

    /* MAIN SECTION */

    /* MAIN PARAGRAPH */

    /* vmove.cob:23: MOVE */
    {
      b_8.setBytes ("45v", 3);
    }
    /* vmove.cob:24: MOVE */
    {
      f_16.moveFrom (c_2);
    }
    /* vmove.cob:25: MOVE */
    {
      f_12.moveFrom (c_2);
    }
    /* vmove.cob:27: MOVE */
    {
      f_5.checkMoveStrNum (f_8);
      f_8.moveFrom (f_5);
    }
    /* vmove.cob:28: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_8);
    }
    /* vmove.cob:30: MOVE */
    {
      f_6.checkMoveStrNum (f_8);
      f_8.moveFrom (f_6);
    }
    /* vmove.cob:31: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_8);
    }
    /* vmove.cob:33: MOVE */
    {
      f_7.checkMoveStrNum (f_8);
      f_8.moveFrom (f_7);
    }
    /* vmove.cob:34: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_8);
    }
    /* vmove.cob:36: MOVE */
    {
      f_9.checkMoveStrNum (f_8);
      f_8.moveFrom (f_9);
    }
    /* vmove.cob:37: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_8);
    }
    /* vmove.cob:39: MOVE */
    {
      f_10.checkMoveStrNum (f_8);
      f_8.moveFrom (f_10);
    }
    /* vmove.cob:40: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_8);
    }
    /* vmove.cob:42: MOVE */
    {
      f_11.checkMoveStrNum (f_8);
      f_8.moveFrom (f_11);
    }
    /* vmove.cob:43: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_8);
    }
    /* vmove.cob:45: MOVE */
    {
      f_12.checkMoveStrNum (f_8);
      f_8.moveFrom (f_12);
    }
    /* vmove.cob:46: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_8);
    }
    /* vmove.cob:48: MOVE */
    {
      f_13.checkMoveStrNum (f_8);
      f_8.moveFrom (f_13);
    }
    /* vmove.cob:49: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_8);
    }
    /* vmove.cob:51: MOVE */
    {
      f_14.checkMoveStrNum (f_8);
      f_8.moveFrom (f_14);
    }
    /* vmove.cob:52: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_8);
    }
    /* vmove.cob:54: MOVE */
    {
      f_15.checkMoveStrNum (f_8);
      f_8.moveFrom (f_15);
    }
    /* vmove.cob:55: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_8);
    }
    /* vmove.cob:57: MOVE */
    {
      f_16.checkMoveStrNum (f_8);
      f_8.moveFrom (f_16);
    }
    /* vmove.cob:58: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_8);
    }
    /* vmove.cob:60: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, c_3);
    }
    /* vmove.cob:61: MOVE */
    {
      b_8.setBytes ("45v", 3);
    }
    /* vmove.cob:62: MOVE */
    {
      f_16.moveFrom (c_2);
    }
    /* vmove.cob:63: MOVE */
    {
      f_12.moveFrom (c_2);
    }
    /* vmove.cob:65: MOVE */
    {
      f_5.checkMoveStrNum (f_12);
      f_12.moveFrom (f_5);
    }
    /* vmove.cob:66: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_12);
    }
    /* vmove.cob:68: MOVE */
    {
      f_6.checkMoveStrNum (f_12);
      f_12.moveFrom (f_6);
    }
    /* vmove.cob:69: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_12);
    }
    /* vmove.cob:71: MOVE */
    {
      f_7.checkMoveStrNum (f_12);
      f_12.moveFrom (f_7);
    }
    /* vmove.cob:72: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_12);
    }
    /* vmove.cob:74: MOVE */
    {
      f_7.checkMoveStrNum (f_12);
      f_12.moveFrom (f_7);
    }
    /* vmove.cob:75: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_12);
    }
    /* vmove.cob:77: MOVE */
    {
      f_9.checkMoveStrNum (f_12);
      f_12.moveFrom (f_9);
    }
    /* vmove.cob:78: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_12);
    }
    /* vmove.cob:80: MOVE */
    {
      f_10.checkMoveStrNum (f_12);
      f_12.moveFrom (f_10);
    }
    /* vmove.cob:81: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_12);
    }
    /* vmove.cob:83: MOVE */
    {
      f_11.checkMoveStrNum (f_12);
      f_12.moveFrom (f_11);
    }
    /* vmove.cob:84: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_12);
    }
    /* vmove.cob:86: MOVE */
    {
      f_13.checkMoveStrNum (f_12);
      f_12.moveFrom (f_13);
    }
    /* vmove.cob:87: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_12);
    }
    /* vmove.cob:89: MOVE */
    {
      f_14.checkMoveStrNum (f_12);
      f_12.moveFrom (f_14);
    }
    /* vmove.cob:90: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_12);
    }
    /* vmove.cob:92: MOVE */
    {
      f_15.checkMoveStrNum (f_12);
      f_12.moveFrom (f_15);
    }
    /* vmove.cob:93: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_12);
    }
    /* vmove.cob:95: MOVE */
    {
      f_16.checkMoveStrNum (f_12);
      f_12.moveFrom (f_16);
    }
    /* vmove.cob:96: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_12);
    }
    /* vmove.cob:98: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, c_3);
    }
    /* vmove.cob:99: MOVE */
    {
      b_8.setBytes ("45v", 3);
    }
    /* vmove.cob:100: MOVE */
    {
      f_16.moveFrom (c_2);
    }
    /* vmove.cob:101: MOVE */
    {
      f_12.moveFrom (c_2);
    }
    /* vmove.cob:103: MOVE */
    {
      f_5.checkMoveStrNum (f_16);
      f_16.moveFrom (f_5);
    }
    /* vmove.cob:104: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_16);
    }
    /* vmove.cob:106: MOVE */
    {
      f_6.checkMoveStrNum (f_16);
      f_16.moveFrom (f_6);
    }
    /* vmove.cob:107: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_16);
    }
    /* vmove.cob:109: MOVE */
    {
      f_7.checkMoveStrNum (f_16);
      f_16.moveFrom (f_7);
    }
    /* vmove.cob:110: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_16);
    }
    /* vmove.cob:112: MOVE */
    {
      f_8.checkMoveStrNum (f_16);
      f_16.moveFrom (f_8);
    }
    /* vmove.cob:113: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_16);
    }
    /* vmove.cob:115: MOVE */
    {
      f_9.checkMoveStrNum (f_16);
      f_16.moveFrom (f_9);
    }
    /* vmove.cob:116: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_16);
    }
    /* vmove.cob:118: MOVE */
    {
      f_10.checkMoveStrNum (f_16);
      f_16.moveFrom (f_10);
    }
    /* vmove.cob:119: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_16);
    }
    /* vmove.cob:121: MOVE */
    {
      f_11.checkMoveStrNum (f_16);
      f_16.moveFrom (f_11);
    }
    /* vmove.cob:122: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_16);
    }
    /* vmove.cob:124: MOVE */
    {
      f_12.checkMoveStrNum (f_16);
      f_16.moveFrom (f_12);
    }
    /* vmove.cob:125: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_16);
    }
    /* vmove.cob:127: MOVE */
    {
      f_13.checkMoveStrNum (f_16);
      f_16.moveFrom (f_13);
    }
    /* vmove.cob:128: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_16);
    }
    /* vmove.cob:130: MOVE */
    {
      f_14.checkMoveStrNum (f_16);
      f_16.moveFrom (f_14);
    }
    /* vmove.cob:131: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_16);
    }
    /* vmove.cob:133: MOVE */
    {
      f_15.checkMoveStrNum (f_16);
      f_16.moveFrom (f_15);
    }
    /* vmove.cob:134: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_16);
    }
    /* vmove.cob:136: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, c_3);
    }
    CobolTerminal.closeWriter();
    return 0;
  }

  public static void main(String[] args)
  {
    new vmove().vmove_(0);
  }

  /* Data storage */

  /* PROGRAM-ID : vmove */
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
  private CobolFieldAttribute a_13;
  private CobolFieldAttribute a_14;

  /* Fields */

  /* PROGRAM-ID : vmove */
  private AbstractCobolField f_5;	/* A91 */
  private AbstractCobolField f_6;	/* A92 */
  private AbstractCobolField f_7;	/* A93 */
  private AbstractCobolField f_8;	/* A94 */
  private AbstractCobolField f_9;	/* AC1 */
  private AbstractCobolField f_10;	/* AC2 */
  private AbstractCobolField f_11;	/* AC3 */
  private AbstractCobolField f_12;	/* AC4 */
  private AbstractCobolField f_13;	/* AB1 */
  private AbstractCobolField f_14;	/* AB2 */
  private AbstractCobolField f_15;	/* AB3 */
  private AbstractCobolField f_16;	/* AB4 */

  /* End of fields */


  /* Constants */

  private AbstractCobolField c_1;
  private AbstractCobolField c_2;
  private AbstractCobolField c_3;


  public vmove()
  {
    init();
  }

  public void init()
  {
    try {
      /* Data storage */

      /* PROGRAM-ID : vmove */
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

      a_14 = new CobolFieldAttribute (33, 0, 0, 0, null);
      a_13 = new CobolFieldAttribute (17, 3, 0, 33, null);
      a_12 = new CobolFieldAttribute (17, 3, 0, 32, null);
      a_11 = new CobolFieldAttribute (16, 3, 0, 1, null);
      a_10 = new CobolFieldAttribute (16, 3, 1, 1, null);
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

      /* PROGRAM-ID : vmove */
      f_5	= CobolFieldFactory.makeCobolField(3, b_5, a_9);	/* A91 */
      f_6	= CobolFieldFactory.makeCobolField(3, b_6, a_3);	/* A92 */
      f_7	= CobolFieldFactory.makeCobolField(3, b_7, a_11);	/* A93 */
      f_8	= CobolFieldFactory.makeCobolField(3, b_8, a_10);	/* A94 */
      f_9	= CobolFieldFactory.makeCobolField(2, b_9, a_1);	/* AC1 */
      f_10	= CobolFieldFactory.makeCobolField(2, b_10, a_2);	/* AC2 */
      f_11	= CobolFieldFactory.makeCobolField(2, b_11, a_4);	/* AC3 */
      f_12	= CobolFieldFactory.makeCobolField(2, b_12, a_5);	/* AC4 */
      f_13	= CobolFieldFactory.makeCobolField(2, b_13, a_12);	/* AB1 */
      f_14	= CobolFieldFactory.makeCobolField(2, b_14, a_7);	/* AB2 */
      f_15	= CobolFieldFactory.makeCobolField(2, b_15, a_13);	/* AB3 */
      f_16	= CobolFieldFactory.makeCobolField(2, b_16, a_8);	/* AB4 */

      /* End of fields */


      /* Constants */

      c_3	= CobolFieldFactory.makeCobolField(21, "*********************", a_14);
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
