package test;

import jp.osscons.opensourcecobol.libcobj.Const;
import jp.osscons.opensourcecobol.libcobj.call.CobolRunnable;
import jp.osscons.opensourcecobol.libcobj.common.CobolCallParams;
import jp.osscons.opensourcecobol.libcobj.common.CobolFrame;
import jp.osscons.opensourcecobol.libcobj.common.CobolModule;
import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldAttribute;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldFactory;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolGoBackException;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolStopRunException;
import jp.osscons.opensourcecobol.libcobj.termio.CobolTerminal;

public class iifa implements CobolRunnable {

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
    return iifa_(0);
  }

  @Override
  public void cancel() {
    iifa_(1);
  }

  @Override
  public boolean isActive() {
    return false;
  }

  int iifa_ (int entry)
  {
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
    this.cobolPushCallStackList("iifa");

    b_1.set(0);
    b_5.setString("Hello");
    b_6.setString("World");
    b_7.setString("Hello!");
    b_8.setString("Hello");
    b_9.setBytes ("12345", 5);
    b_10.setBytes ("123  ", 5);
    b_11.setBytes ("12345", 5);
    b_12.setBytes ("00123", 5);
    f_13.moveFrom (12345);
    CobolFieldFactory.makeCobolField(3, b_14, a_1).moveFrom (123);
    /* PROCEDURE DIVISION */
    try{
    CobolStopRunException.dummy();
    CobolGoBackException.dummy();

      /* Entry iifa */

      /* MAIN SECTION */

      /* MAIN PARAGRAPH */

      /* iifa.cob:68: IF */
      {
        if (((int)f_9.compareTo (f_13) == 0))
          {
            /* iifa.cob:70: DISPLAY */
            {
              CobolTerminal.display (0, 1, 1, c_1);
            }
          }
        else
          {
            /* iifa.cob:72: DISPLAY */
            {
              CobolTerminal.display (0, 1, 1, c_2);
            }
          }
      }
    } catch(CobolGoBackException e) {
      return e.getReturnCode();
    } catch(CobolStopRunException e) {
      System.exit(e.getReturnCode());
    }
    return 0;
  }

  public static void main(String[] args)
  {
    new iifa().iifa_(0);
  }

  public iifa()
  {
    init();
  }

  public void init()
  {
    try {
      /* Data storage */

      /* PROGRAM-ID : iifa */
      b_1 = new CobolDataStorage(4);	/* RETURN-CODE */
      b_5 = new CobolDataStorage(10);	/* A1 */
      b_6 = new CobolDataStorage(10);	/* A2 */
      b_7 = new CobolDataStorage(10);	/* A3 */
      b_8 = new CobolDataStorage(10);	/* A4 */
      b_9 = new CobolDataStorage(5);	/* AA1 */
      b_10 = new CobolDataStorage(5);	/* AA2 */
      b_11 = new CobolDataStorage(5);	/* A91 */
      b_12 = new CobolDataStorage(5);	/* A92 */
      b_13 = new CobolDataStorage(3);	/* AC1 */
      b_14 = new CobolDataStorage(3);	/* AC2 */

      /* End of data storage */


      initAttr();

      /* Fields */

      /* PROGRAM-ID : iifa */
      f_9	= CobolFieldFactory.makeCobolField(5, b_9, a_2);	/* AA1 */
      f_13	= CobolFieldFactory.makeCobolField(3, b_13, a_1);	/* AC1 */

      /* End of fields */


      /* Constants */

      c_1	= CobolFieldFactory.makeCobolField(2, "OK", a_2);
      c_2	= CobolFieldFactory.makeCobolField(2, "NG", a_2);

    } catch(Exception e) {
      e.printStackTrace();
    }
  }

  private void initAttr() {
    /* Attributes */

    a_1 = new CobolFieldAttribute (18, 5, 0, 0, null);
    a_2 = new CobolFieldAttribute (33, 0, 0, 0, null);

  }

  /* Data storage */

  /* PROGRAM-ID : iifa */
  private CobolDataStorage b_1;	/* RETURN-CODE */
  private CobolDataStorage b_5;	/* A1 */
  private CobolDataStorage b_6;	/* A2 */
  private CobolDataStorage b_7;	/* A3 */
  private CobolDataStorage b_8;	/* A4 */
  private CobolDataStorage b_9;	/* AA1 */
  private CobolDataStorage b_10;	/* AA2 */
  private CobolDataStorage b_11;	/* A91 */
  private CobolDataStorage b_12;	/* A92 */
  private CobolDataStorage b_13;	/* AC1 */
  private CobolDataStorage b_14;	/* AC2 */

  /* End of data storage */


  /* Fields */

  /* PROGRAM-ID : iifa */
  private AbstractCobolField f_9;	/* AA1 */
  private AbstractCobolField f_13;	/* AC1 */

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