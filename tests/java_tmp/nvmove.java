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

public class nvmove implements CobolRunnable {

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
    return nvmove_(0);
  }

  @Override
  public void cancel() {
    nvmove_(1);
  }

  @Override
  public boolean isActive() {
    return false;
  }

  int nvmove_ (int entry)
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
    this.cobolPushCallStackList("nvmove");

    b_1.set(0);
    b_5.setBytes ("123", 3);
    b_6.setBytes ("45v", 3);
    b_7.setBytes ("123", 3);
    b_8.setBytes ("45v", 3);
    CobolFieldFactory.makeCobolField(2, b_9, a_1).moveFrom (c_1);
    CobolFieldFactory.makeCobolField(2, b_10, a_3).moveFrom (c_2);
    f_11.moveFrom (c_1);
    f_12.moveFrom (c_2);
    CobolFieldFactory.makeCobolField(2, b_13, a_5).moveFrom (c_1);
    CobolFieldFactory.makeCobolField(2, b_14, a_6).moveFrom (c_2);
    f_15.moveFrom (c_1);
    f_16.moveFrom (c_2);
    b_17.setBytes ("04567", 5);
    b_18.setBytes ("123  ", 5);
    b_19.setBytes ("12.3 ", 5);
    b_20.setBytes ("00123", 5);
    CobolFieldFactory.makeCobolField(10, b_21, a_7).moveFrom ("");
    CobolFieldFactory.makeCobolField(10, b_22, a_7).moveFrom(c_3);
    CobolFieldFactory.makeCobolField(10, b_23, a_7).moveFrom(c_4);
    CobolFieldFactory.makeCobolField(10, b_24, a_7).moveFrom(c_5);
    /* PROCEDURE DIVISION */

    /* Entry nvmove */

    /* MAIN SECTION */

    /* MAIN PARAGRAPH */

    /* nvmove.cob:89: MOVE */
    {
      f_19.checkMoveStrNum (f_8);
      f_8.moveFrom (f_19);
    }
    /* nvmove.cob:90: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_8);
    }
    /* nvmove.cob:92: MOVE */
    {
      f_19.checkMoveStrNum (f_7);
      f_7.moveFrom (f_19);
    }
    /* nvmove.cob:93: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_7);
    }
    /* nvmove.cob:95: MOVE */
    {
      f_19.checkMoveStrNum (f_12);
      f_12.moveFrom (f_19);
    }
    /* nvmove.cob:96: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_12);
    }
    /* nvmove.cob:98: MOVE */
    {
      f_19.checkMoveStrNum (f_11);
      f_11.moveFrom (f_19);
    }
    /* nvmove.cob:99: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_11);
    }
    /* nvmove.cob:101: MOVE */
    {
      f_19.checkMoveStrNum (f_16);
      f_16.moveFrom (f_19);
    }
    /* nvmove.cob:102: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_16);
    }
    /* nvmove.cob:104: MOVE */
    {
      f_19.checkMoveStrNum (f_15);
      f_15.moveFrom (f_19);
    }
    /* nvmove.cob:105: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, f_15);
    }
    /* nvmove.cob:107: DISPLAY */
    {
      CobolTerminal.display (0, 1, 1, c_6);
    }
    CobolTerminal.closeWriter();
    return 0;
  }

  public static void main(String[] args)
  {
    new nvmove().nvmove_(0);
  }

  /* Data storage */

  /* PROGRAM-ID : nvmove */
  private CobolDataStorage b_1;	/* RETURN-CODE */
  private CobolDataStorage b_5;	/* A91 */
  private CobolDataStorage b_6;	/* A92 */
  private CobolDataStorage b_7;	/* A9U */
  private CobolDataStorage b_8;	/* A9S */
  private CobolDataStorage b_9;	/* AC1 */
  private CobolDataStorage b_10;	/* AC2 */
  private CobolDataStorage b_11;	/* ACU */
  private CobolDataStorage b_12;	/* ACS */
  private CobolDataStorage b_13;	/* AB1 */
  private CobolDataStorage b_14;	/* AB2 */
  private CobolDataStorage b_15;	/* ABU */
  private CobolDataStorage b_16;	/* ABS */
  private CobolDataStorage b_17;	/* AX1 */
  private CobolDataStorage b_18;	/* AX2 */
  private CobolDataStorage b_19;	/* AX3 */
  private CobolDataStorage b_20;	/* AX4 */
  private CobolDataStorage b_21;	/* AN1 */
  private CobolDataStorage b_22;	/* AN2 */
  private CobolDataStorage b_23;	/* AN3 */
  private CobolDataStorage b_24;	/* AN4 */

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

  /* Fields */

  /* PROGRAM-ID : nvmove */
  private AbstractCobolField f_7;	/* A9U */
  private AbstractCobolField f_8;	/* A9S */
  private AbstractCobolField f_11;	/* ACU */
  private AbstractCobolField f_12;	/* ACS */
  private AbstractCobolField f_15;	/* ABU */
  private AbstractCobolField f_16;	/* ABS */
  private AbstractCobolField f_19;	/* AX3 */

  /* End of fields */


  /* Constants */

  private AbstractCobolField c_1;
  private AbstractCobolField c_2;
  private AbstractCobolField c_3;
  private AbstractCobolField c_4;
  private AbstractCobolField c_5;
  private AbstractCobolField c_6;


  public nvmove()
  {
    init();
  }

  public void init()
  {
    try {
      /* Data storage */

      /* PROGRAM-ID : nvmove */
      b_1 = new CobolDataStorage(4);	/* RETURN-CODE */
      b_5 = new CobolDataStorage(3);	/* A91 */
      b_6 = new CobolDataStorage(3);	/* A92 */
      b_7 = new CobolDataStorage(3);	/* A9U */
      b_8 = new CobolDataStorage(3);	/* A9S */
      b_9 = new CobolDataStorage(2);	/* AC1 */
      b_10 = new CobolDataStorage(2);	/* AC2 */
      b_11 = new CobolDataStorage(2);	/* ACU */
      b_12 = new CobolDataStorage(2);	/* ACS */
      b_13 = new CobolDataStorage(2);	/* AB1 */
      b_14 = new CobolDataStorage(2);	/* AB2 */
      b_15 = new CobolDataStorage(2);	/* ABU */
      b_16 = new CobolDataStorage(2);	/* ABS */
      b_17 = new CobolDataStorage(5);	/* AX1 */
      b_18 = new CobolDataStorage(5);	/* AX2 */
      b_19 = new CobolDataStorage(5);	/* AX3 */
      b_20 = new CobolDataStorage(5);	/* AX4 */
      b_21 = new CobolDataStorage(10);	/* AN1 */
      b_22 = new CobolDataStorage(10);	/* AN2 */
      b_23 = new CobolDataStorage(10);	/* AN3 */
      b_24 = new CobolDataStorage(10);	/* AN4 */

      /* End of data storage */


      /* Attributes */

      a_9 = new CobolFieldAttribute (16, 3, 1, 1, null);
      a_8 = new CobolFieldAttribute (33, 0, 0, 0, null);
      a_7 = new CobolFieldAttribute (64, 0, 0, 0, null);
      a_6 = new CobolFieldAttribute (17, 3, 1, 33, null);
      a_5 = new CobolFieldAttribute (17, 3, 1, 32, null);
      a_4 = new CobolFieldAttribute (16, 3, 1, 3, null);
      a_3 = new CobolFieldAttribute (18, 3, 1, 1, null);
      a_2 = new CobolFieldAttribute (16, 3, 1, 0, null);
      a_1 = new CobolFieldAttribute (18, 3, 1, 0, null);

      /* Fields */

      /* PROGRAM-ID : nvmove */
      f_7	= CobolFieldFactory.makeCobolField(3, b_7, a_2);	/* A9U */
      f_8	= CobolFieldFactory.makeCobolField(3, b_8, a_9);	/* A9S */
      f_11	= CobolFieldFactory.makeCobolField(2, b_11, a_1);	/* ACU */
      f_12	= CobolFieldFactory.makeCobolField(2, b_12, a_3);	/* ACS */
      f_15	= CobolFieldFactory.makeCobolField(2, b_15, a_5);	/* ABU */
      f_16	= CobolFieldFactory.makeCobolField(2, b_16, a_6);	/* ABS */
      f_19	= CobolFieldFactory.makeCobolField(5, b_19, a_8);	/* AX3 */

      /* End of fields */


      /* Constants */

      c_6	= CobolFieldFactory.makeCobolField(14, "**************", a_8);
      c_5	= CobolFieldFactory.makeCobolField(10, new String(makeByteArray((byte)0202, (byte)0117, (byte)0202, (byte)0117, (byte)0202, (byte)0120, (byte)0202, (byte)0121, (byte)0202, (byte)0122), "SHIFT-JIS"), a_7);
      c_4	= CobolFieldFactory.makeCobolField(6, new String(makeByteArray((byte)0202, (byte)0120, (byte)0202, (byte)0121, (byte)0202, (byte)0122), "SHIFT-JIS"), a_7);
      c_3	= CobolFieldFactory.makeCobolField(8, new String(makeByteArray((byte)0202, (byte)0120, (byte)0202, (byte)0121, (byte)0201, (byte)0104, (byte)0202, (byte)0122), "SHIFT-JIS"), a_7);
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