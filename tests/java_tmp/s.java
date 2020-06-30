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

public class s implements CobolRunnable {

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
    return s_(0);
  }

  @Override
  public void cancel() {
    s_(1);
  }

  @Override
  public boolean isActive() {
    return false;
  }

  int s_ (int entry)
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
    this.cobolPushCallStackList("s");

    b_1.set(0);
    b_5.setBytes ("0012s", 5);
    b_6.setBytes ("04567", 5);
    b_7.setBytes ("0456w", 5);
    f_8.moveFrom (-123);
    f_9.moveFrom (4567);
    f_10.moveFrom (-4567);
    b_11.setSwpS32Binary (-123);
    b_12.setSwpS32Binary (4567);
    b_13.setSwpS32Binary (-4567);
    b_14.setBytes ("04567", 5);
    /* PROCEDURE DIVISION */

    /* Entry s */

    /* MAIN SECTION */

    /* MAIN PARAGRAPH */

    /* s.cob:19: DISPLAY */
    {
      CobolTerminal.display (0, 1, 2, c_1, f_5);
    }
    /* s.cob:20: DISPLAY */
    {
      CobolTerminal.display (0, 1, 2, c_2, f_8);
    }
    /* s.cob:21: DISPLAY */
    {
      CobolTerminal.display (0, 1, 2, c_3, f_11);
    }
    /* s.cob:23: MOVE */
    {
      f_6.checkMoveStrNum (f_5);
      b_5.setBytes (b_6, 5);
    }
    /* s.cob:24: DISPLAY */
    {
      CobolTerminal.display (0, 1, 2, c_4, f_5);
    }
    /* s.cob:26: MOVE */
    {
      f_9.checkMoveStrNum (f_5);
      f_5.moveFrom (f_9);
    }
    /* s.cob:27: DISPLAY */
    {
      CobolTerminal.display (0, 1, 2, c_5, f_5);
    }
    /* s.cob:29: MOVE */
    {
      f_12.checkMoveStrNum (f_5);
      f_5.moveFrom (f_12);
    }
    /* s.cob:30: DISPLAY */
    {
      CobolTerminal.display (0, 1, 2, c_6, f_5);
    }
    /* s.cob:32: MOVE */
    {
      f_7.checkMoveStrNum (f_5);
      b_5.setBytes (b_7, 5);
    }
    /* s.cob:33: DISPLAY */
    {
      CobolTerminal.display (0, 1, 2, c_7, f_5);
    }
    /* s.cob:35: MOVE */
    {
      f_10.checkMoveStrNum (f_5);
      f_5.moveFrom (f_10);
    }
    /* s.cob:36: DISPLAY */
    {
      CobolTerminal.display (0, 1, 2, c_8, f_5);
    }
    /* s.cob:38: MOVE */
    {
      f_13.checkMoveStrNum (f_5);
      f_5.moveFrom (f_13);
    }
    /* s.cob:39: DISPLAY */
    {
      CobolTerminal.display (0, 1, 2, c_9, f_5);
    }
    /* s.cob:42: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, c_10);
    }
    /* s.cob:44: MOVE */
    {
      f_6.checkMoveStrNum (f_8);
      f_8.moveFrom (f_6);
    }
    /* s.cob:45: DISPLAY */
    {
      CobolTerminal.display (0, 1, 2, c_11, f_8);
    }
    /* s.cob:47: MOVE */
    {
      f_9.checkMoveStrNum (f_8);
      b_8.setBytes (b_9, 3);
    }
    /* s.cob:48: DISPLAY */
    {
      CobolTerminal.display (0, 1, 2, c_12, f_8);
    }
    /* s.cob:50: MOVE */
    {
      f_12.checkMoveStrNum (f_8);
      f_8.moveFrom (f_12);
    }
    /* s.cob:51: DISPLAY */
    {
      CobolTerminal.display (0, 1, 2, c_13, f_8);
    }
    /* s.cob:53: MOVE */
    {
      f_7.checkMoveStrNum (f_8);
      f_8.moveFrom (f_7);
    }
    /* s.cob:54: DISPLAY */
    {
      CobolTerminal.display (0, 1, 2, c_14, f_8);
    }
    /* s.cob:56: MOVE */
    {
      f_10.checkMoveStrNum (f_8);
      b_8.setBytes (b_10, 3);
    }
    /* s.cob:57: DISPLAY */
    {
      CobolTerminal.display (0, 1, 2, c_15, f_8);
    }
    /* s.cob:59: MOVE */
    {
      f_13.checkMoveStrNum (f_8);
      f_8.moveFrom (f_13);
    }
    /* s.cob:60: DISPLAY */
    {
      CobolTerminal.display (0, 1, 2, c_16, f_8);
    }
    /* s.cob:63: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, c_10);
    }
    /* s.cob:65: MOVE */
    {
      f_6.checkMoveStrNum (f_11);
      f_11.moveFrom (f_6);
    }
    /* s.cob:66: DISPLAY */
    {
      CobolTerminal.display (0, 1, 2, c_17, f_11);
    }
    /* s.cob:68: MOVE */
    {
      f_9.checkMoveStrNum (f_11);
      f_11.moveFrom (f_9);
    }
    /* s.cob:69: DISPLAY */
    {
      CobolTerminal.display (0, 1, 2, c_18, f_11);
    }
    /* s.cob:71: MOVE */
    {
      f_12.checkMoveStrNum (f_11);
      b_11.setBytes (b_12, 4);
    }
    /* s.cob:72: DISPLAY */
    {
      CobolTerminal.display (0, 1, 2, c_19, f_11);
    }
    /* s.cob:74: MOVE */
    {
      f_7.checkMoveStrNum (f_11);
      f_11.moveFrom (f_7);
    }
    /* s.cob:75: DISPLAY */
    {
      CobolTerminal.display (0, 1, 2, c_20, f_11);
    }
    /* s.cob:77: MOVE */
    {
      f_10.checkMoveStrNum (f_11);
      f_11.moveFrom (f_10);
    }
    /* s.cob:78: DISPLAY */
    {
      CobolTerminal.display (0, 1, 2, c_21, f_11);
    }
    /* s.cob:80: MOVE */
    {
      f_13.checkMoveStrNum (f_11);
      b_11.setBytes (b_13, 4);
    }
    /* s.cob:81: DISPLAY */
    {
      CobolTerminal.display (0, 1, 2, c_22, f_11);
    }
    CobolTerminal.closeWriter();
    return 0;
  }

  public static void main(String[] args)
  {
    new s().s_(0);
  }

  /* Data storage */

  /* PROGRAM-ID : s */
  private CobolDataStorage b_1;	/* RETURN-CODE */
  private CobolDataStorage b_5;	/* A9 */
  private CobolDataStorage b_6;	/* A92 */
  private CobolDataStorage b_7;	/* A93 */
  private CobolDataStorage b_8;	/* AC */
  private CobolDataStorage b_9;	/* AC2 */
  private CobolDataStorage b_10;	/* AC3 */
  private CobolDataStorage b_11;	/* AB */
  private CobolDataStorage b_12;	/* AB2 */
  private CobolDataStorage b_13;	/* AB3 */
  private CobolDataStorage b_14;	/* AX2 */

  /* End of data storage */


  /* Attributes */

  private CobolFieldAttribute a_1;
  private CobolFieldAttribute a_2;
  private CobolFieldAttribute a_3;
  private CobolFieldAttribute a_4;

  /* Fields */

  /* PROGRAM-ID : s */
  private AbstractCobolField f_5;	/* A9 */
  private AbstractCobolField f_6;	/* A92 */
  private AbstractCobolField f_7;	/* A93 */
  private AbstractCobolField f_8;	/* AC */
  private AbstractCobolField f_9;	/* AC2 */
  private AbstractCobolField f_10;	/* AC3 */
  private AbstractCobolField f_11;	/* AB */
  private AbstractCobolField f_12;	/* AB2 */
  private AbstractCobolField f_13;	/* AB3 */

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
  private AbstractCobolField c_16;
  private AbstractCobolField c_17;
  private AbstractCobolField c_18;
  private AbstractCobolField c_19;
  private AbstractCobolField c_20;
  private AbstractCobolField c_21;
  private AbstractCobolField c_22;


  public s()
  {
    init();
  }

  public void init()
  {
    /* Data storage */

    /* PROGRAM-ID : s */
    b_1 = new CobolDataStorage(4);	/* RETURN-CODE */
    b_5 = new CobolDataStorage(5);	/* A9 */
    b_6 = new CobolDataStorage(5);	/* A92 */
    b_7 = new CobolDataStorage(5);	/* A93 */
    b_8 = new CobolDataStorage(3);	/* AC */
    b_9 = new CobolDataStorage(3);	/* AC2 */
    b_10 = new CobolDataStorage(3);	/* AC3 */
    b_11 = new CobolDataStorage(4);	/* AB */
    b_12 = new CobolDataStorage(4);	/* AB2 */
    b_13 = new CobolDataStorage(4);	/* AB3 */
    b_14 = new CobolDataStorage(5);	/* AX2 */

    /* End of data storage */


    /* Attributes */

    a_4 = new CobolFieldAttribute (17, 5, 0, 33, null);
    a_3 = new CobolFieldAttribute (16, 5, 0, 1, null);
    a_2 = new CobolFieldAttribute (33, 0, 0, 0, null);
    a_1 = new CobolFieldAttribute (18, 5, 0, 1, null);

    /* Fields */

    /* PROGRAM-ID : s */
    f_5	= CobolFieldFactory.makeCobolField(5, b_5, a_3);	/* A9 */
    f_6	= CobolFieldFactory.makeCobolField(5, b_6, a_3);	/* A92 */
    f_7	= CobolFieldFactory.makeCobolField(5, b_7, a_3);	/* A93 */
    f_8	= CobolFieldFactory.makeCobolField(3, b_8, a_1);	/* AC */
    f_9	= CobolFieldFactory.makeCobolField(3, b_9, a_1);	/* AC2 */
    f_10	= CobolFieldFactory.makeCobolField(3, b_10, a_1);	/* AC3 */
    f_11	= CobolFieldFactory.makeCobolField(4, b_11, a_4);	/* AB */
    f_12	= CobolFieldFactory.makeCobolField(4, b_12, a_4);	/* AB2 */
    f_13	= CobolFieldFactory.makeCobolField(4, b_13, a_4);	/* AB3 */

    /* End of fields */


    /* Constants */

    c_22	= CobolFieldFactory.makeCobolField(16, "AB3 -> AB; AB = ", a_2);
    c_21	= CobolFieldFactory.makeCobolField(16, "AC3 -> AB; AB = ", a_2);
    c_20	= CobolFieldFactory.makeCobolField(16, "A93 -> AB; AB = ", a_2);
    c_19	= CobolFieldFactory.makeCobolField(16, "AB2 -> AB; AB = ", a_2);
    c_18	= CobolFieldFactory.makeCobolField(16, "AC2 -> AB; AB = ", a_2);
    c_17	= CobolFieldFactory.makeCobolField(16, "A92 -> AB; AB = ", a_2);
    c_16	= CobolFieldFactory.makeCobolField(16, "AB3 -> AC; AC = ", a_2);
    c_15	= CobolFieldFactory.makeCobolField(16, "AC3 -> AC; AC = ", a_2);
    c_14	= CobolFieldFactory.makeCobolField(16, "A93 -> AC; AC = ", a_2);
    c_13	= CobolFieldFactory.makeCobolField(16, "AB2 -> AC; AC = ", a_2);
    c_12	= CobolFieldFactory.makeCobolField(16, "AC2 -> AC; AC = ", a_2);
    c_11	= CobolFieldFactory.makeCobolField(16, "A92 -> AC; AC = ", a_2);
    c_10	= CobolFieldFactory.makeCobolField(16, "****************", a_2);
    c_9	= CobolFieldFactory.makeCobolField(16, "AB3 -> A9; A9 = ", a_2);
    c_8	= CobolFieldFactory.makeCobolField(16, "AC3 -> A9; A9 = ", a_2);
    c_7	= CobolFieldFactory.makeCobolField(16, "A93 -> A9; A9 = ", a_2);
    c_6	= CobolFieldFactory.makeCobolField(16, "AB2 -> A9; A9 = ", a_2);
    c_5	= CobolFieldFactory.makeCobolField(16, "AC2 -> A9; A9 = ", a_2);
    c_4	= CobolFieldFactory.makeCobolField(16, "A92 -> A9; A9 = ", a_2);
    c_3	= CobolFieldFactory.makeCobolField(5, "AB = ", a_2);
    c_2	= CobolFieldFactory.makeCobolField(5, "AC = ", a_2);
    c_1	= CobolFieldFactory.makeCobolField(5, "A9 = ", a_2);

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

}
