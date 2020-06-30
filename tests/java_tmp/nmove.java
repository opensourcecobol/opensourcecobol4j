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

public class nmove implements CobolRunnable {

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
    return nmove_(0);
  }

  @Override
  public void cancel() {
    nmove_(1);
  }

  @Override
  public boolean isActive() {
    return false;
  }

  int nmove_ (int entry)
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
    this.cobolPushCallStackList("nmove");

    b_1.set(0);
    b_5.setBytes ("0012s", 5);
    b_6.setBytes ("04567", 5);
    f_7.moveFrom (-123);
    f_8.moveFrom (4567);
    b_9.setSwpS32Binary (-123);
    b_10.setSwpS32Binary (4567);
    b_11.setBytes ("04567", 5);
    b_12.setBytes ("04567", 5);
    f_13.moveFrom(c_1);
    f_14.moveFrom(c_2);
    /* PROCEDURE DIVISION */

    /* Entry nmove */

    /* MAIN SECTION */

    /* MAIN PARAGRAPH */

    /* nmove.cob:20: DISPLAY */
    {
      CobolTerminal.display (0, 1, 2, c_3, f_6);
    }
    /* nmove.cob:21: DISPLAY */
    {
      CobolTerminal.display (0, 1, 2, c_4, f_8);
    }
    /* nmove.cob:22: DISPLAY */
    {
      CobolTerminal.display (0, 1, 2, c_5, f_10);
    }
    /* nmove.cob:23: DISPLAY */
    {
      CobolTerminal.display (0, 1, 2, c_6, f_12);
    }
    /* nmove.cob:25: MOVE */
    {
      f_6.checkMoveStrNum (f_13);
      f_13.moveFrom (f_6);
    }
    /* nmove.cob:26: DISPLAY */
    {
      CobolTerminal.display (0, 1, 2, c_7, f_13);
    }
    /* nmove.cob:28: MOVE */
    {
      f_8.checkMoveStrNum (f_13);
      f_13.moveFrom (f_8);
    }
    /* nmove.cob:29: DISPLAY */
    {
      CobolTerminal.display (0, 1, 2, c_8, f_13);
    }
    /* nmove.cob:31: MOVE */
    {
      f_10.checkMoveStrNum (f_13);
      f_13.moveFrom (f_10);
    }
    /* nmove.cob:32: DISPLAY */
    {
      CobolTerminal.display (0, 1, 2, c_9, f_13);
    }
    /* nmove.cob:34: MOVE */
    {
      f_12.checkMoveStrNum (f_13);
      f_13.moveFrom (f_12);
    }
    /* nmove.cob:35: DISPLAY */
    {
      CobolTerminal.display (0, 1, 2, c_10, f_13);
    }
    /* nmove.cob:37: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, c_11);
    }
    /* nmove.cob:39: MOVE */
    {
      f_14.checkMoveStrNum (f_5);
      f_5.moveFrom (f_14);
    }
    /* nmove.cob:40: DISPLAY */
    {
      CobolTerminal.display (0, 1, 2, c_12, f_5);
    }
    /* nmove.cob:42: MOVE */
    {
      f_14.checkMoveStrNum (f_7);
      f_7.moveFrom (f_14);
    }
    /* nmove.cob:43: DISPLAY */
    {
      CobolTerminal.display (0, 1, 2, c_13, f_7);
    }
    /* nmove.cob:45: MOVE */
    {
      f_14.checkMoveStrNum (f_9);
      f_9.moveFrom (f_14);
    }
    /* nmove.cob:46: DISPLAY */
    {
      CobolTerminal.display (0, 1, 2, c_14, f_9);
    }
    /* nmove.cob:48: MOVE */
    {
      f_14.checkMoveStrNum (f_11);
      f_11.moveFrom (f_14);
    }
    /* nmove.cob:49: DISPLAY */
    {
      CobolTerminal.display (0, 1, 2, c_15, f_11);
    }
    CobolTerminal.closeWriter();
    return 0;
  }

  public static void main(String[] args)
  {
    new nmove().nmove_(0);
  }

  /* Data storage */

  /* PROGRAM-ID : nmove */
  private CobolDataStorage b_1;	/* RETURN-CODE */
  private CobolDataStorage b_5;	/* A9 */
  private CobolDataStorage b_6;	/* A92 */
  private CobolDataStorage b_7;	/* AC */
  private CobolDataStorage b_8;	/* AC2 */
  private CobolDataStorage b_9;	/* AB */
  private CobolDataStorage b_10;	/* AB2 */
  private CobolDataStorage b_11;	/* AX */
  private CobolDataStorage b_12;	/* AX2 */
  private CobolDataStorage b_13;	/* AN */
  private CobolDataStorage b_14;	/* AN2 */

  /* End of data storage */


  /* Attributes */

  private CobolFieldAttribute a_1;
  private CobolFieldAttribute a_2;
  private CobolFieldAttribute a_3;
  private CobolFieldAttribute a_4;
  private CobolFieldAttribute a_5;

  /* Fields */

  /* PROGRAM-ID : nmove */
  private AbstractCobolField f_5;	/* A9 */
  private AbstractCobolField f_6;	/* A92 */
  private AbstractCobolField f_7;	/* AC */
  private AbstractCobolField f_8;	/* AC2 */
  private AbstractCobolField f_9;	/* AB */
  private AbstractCobolField f_10;	/* AB2 */
  private AbstractCobolField f_11;	/* AX */
  private AbstractCobolField f_12;	/* AX2 */
  private AbstractCobolField f_13;	/* AN */
  private AbstractCobolField f_14;	/* AN2 */

  /* End of fields */


  /* Constants */

  private AbstractCobolField c_1;
  private AbstractCobolField c_2;
  private AbstractCobolField c_3;
  private AbstractCobolField c_4;
  private AbstractCobolField c_5;
  private AbstractCobolField c_6;
  private AbstractCobolField c_7;
  private AbstractCobolField c_8;
  private AbstractCobolField c_9;
  private AbstractCobolField c_10;
  private AbstractCobolField c_11;
  private AbstractCobolField c_12;
  private AbstractCobolField c_13;
  private AbstractCobolField c_14;
  private AbstractCobolField c_15;


  public nmove()
  {
    init();
  }

  public void init()
  {
    try {
      /* Data storage */

      /* PROGRAM-ID : nmove */
      b_1 = new CobolDataStorage(4);	/* RETURN-CODE */
      b_5 = new CobolDataStorage(5);	/* A9 */
      b_6 = new CobolDataStorage(5);	/* A92 */
      b_7 = new CobolDataStorage(3);	/* AC */
      b_8 = new CobolDataStorage(3);	/* AC2 */
      b_9 = new CobolDataStorage(4);	/* AB */
      b_10 = new CobolDataStorage(4);	/* AB2 */
      b_11 = new CobolDataStorage(5);	/* AX */
      b_12 = new CobolDataStorage(5);	/* AX2 */
      b_13 = new CobolDataStorage(10);	/* AN */
      b_14 = new CobolDataStorage(10);	/* AN2 */

      /* End of data storage */


      /* Attributes */

      a_5 = new CobolFieldAttribute (17, 5, 0, 33, null);
      a_4 = new CobolFieldAttribute (16, 5, 0, 1, null);
      a_3 = new CobolFieldAttribute (33, 0, 0, 0, null);
      a_2 = new CobolFieldAttribute (64, 0, 0, 0, null);
      a_1 = new CobolFieldAttribute (18, 5, 0, 1, null);

      /* Fields */

      /* PROGRAM-ID : nmove */
      f_5	= CobolFieldFactory.makeCobolField(5, b_5, a_4);	/* A9 */
      f_6	= CobolFieldFactory.makeCobolField(5, b_6, a_4);	/* A92 */
      f_7	= CobolFieldFactory.makeCobolField(3, b_7, a_1);	/* AC */
      f_8	= CobolFieldFactory.makeCobolField(3, b_8, a_1);	/* AC2 */
      f_9	= CobolFieldFactory.makeCobolField(4, b_9, a_5);	/* AB */
      f_10	= CobolFieldFactory.makeCobolField(4, b_10, a_5);	/* AB2 */
      f_11	= CobolFieldFactory.makeCobolField(5, b_11, a_3);	/* AX */
      f_12	= CobolFieldFactory.makeCobolField(5, b_12, a_3);	/* AX2 */
      f_13	= CobolFieldFactory.makeCobolField(10, b_13, a_2);	/* AN */
      f_14	= CobolFieldFactory.makeCobolField(10, b_14, a_2);	/* AN2 */

      /* End of fields */


      /* Constants */

      c_15	= CobolFieldFactory.makeCobolField(16, "AN2 -> AX; AX = ", a_3);
      c_14	= CobolFieldFactory.makeCobolField(16, "AN2 -> AB; AB = ", a_3);
      c_13	= CobolFieldFactory.makeCobolField(16, "AN2 -> AC; AC = ", a_3);
      c_12	= CobolFieldFactory.makeCobolField(16, "AN2 -> A9; A9 = ", a_3);
      c_11	= CobolFieldFactory.makeCobolField(20, "********************", a_3);
      c_10	= CobolFieldFactory.makeCobolField(16, "AX2 -> AN; AN = ", a_3);
      c_9	= CobolFieldFactory.makeCobolField(16, "AB2 -> AN; AN = ", a_3);
      c_8	= CobolFieldFactory.makeCobolField(16, "AC2 -> AN; AN = ", a_3);
      c_7	= CobolFieldFactory.makeCobolField(16, "A92 -> AN; AN = ", a_3);
      c_6	= CobolFieldFactory.makeCobolField(6, "AX2 = ", a_3);
      c_5	= CobolFieldFactory.makeCobolField(6, "AB2 = ", a_3);
      c_4	= CobolFieldFactory.makeCobolField(6, "AC2 = ", a_3);
      c_3	= CobolFieldFactory.makeCobolField(6, "A92 = ", a_3);
      c_2	= CobolFieldFactory.makeCobolField(10, new String(makeByteArray((byte)0202, (byte)0117, (byte)0202, (byte)0123, (byte)0202, (byte)0124, (byte)0202, (byte)0125, (byte)0202, (byte)0126), "SHIFT-JIS"), a_2);
      c_1	= CobolFieldFactory.makeCobolField(10, new String(makeByteArray((byte)0202, (byte)0240, (byte)0202, (byte)0242, (byte)0202, (byte)0244, (byte)0202, (byte)0246, (byte)0202, (byte)0250), "SHIFT-JIS"), a_2);

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
