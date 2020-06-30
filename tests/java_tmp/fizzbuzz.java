package test;
import java.io.UnsupportedEncodingException;
import jp.osscons.opensourcecobol.libcobj.*;
import jp.osscons.opensourcecobol.libcobj.common.*;
import jp.osscons.opensourcecobol.libcobj.data.*;
import jp.osscons.opensourcecobol.libcobj.exceptions.*;
import jp.osscons.opensourcecobol.libcobj.termio.*;
import jp.osscons.opensourcecobol.libcobj.call.*;

public class fizzbuzz implements CobolRunnable {
  interface __B {
    public AbstractCobolField run();
  }

  private boolean initialized;
  private CobolModule cobolCurrentModule;
  private CobolFrame frame;
  private CobolFrame[] frameStack;
  private boolean cobolInitialized = false;
  private CobolCallParams cobolSaveCallParams = null;
  private CobolCallParams cobolCallParams = null;
  private boolean cobolErrorOnExitFlag;

  private CobolRunnable cob_unifunc;


  @Override
  public int run(AbstractCobolField... fields) {
    return fizzbuzz_(0, fields);
  }

  @Override
  public void cancel() {
    fizzbuzz_(-1);
  }

  @Override
  public boolean isActive() {
    return false;
  }

  int fizzbuzz_ (int entry, AbstractCobolField... fields )
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
    	d0.clear();
    	d0.setScale(0);
    	d1.clear();
    	d1.setScale(0);
    	d2.clear();
    	d2.setScale(0);
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
    this.cobolPushCallStackList("fizzbuzz");

    b_1.set(0);
    b_5.fillBytes((byte)(48), 5);
    b_6.fillBytes((byte)(48), 5);
    /* PROCEDURE DIVISION */
    try{
      CobolStopRunException.dummy();
      CobolGoBackException.dummy();

      /* Entry fizzbuzz */

      /* MAIN SECTION */

      /* MAIN PARAGRAPH */

      /* fizzbuzz.cob:8: PERFORM */
      {
        b_5.setBytes ("00001", 5);
        for (;;)
          {
            if (((int)f_5.compareTo (c_1) >  0))
              break;
            {
              /* fizzbuzz.cob:9: COMPUTE */
              cobolErrorOnExitFlag = true;
              {
                {
                  {
                    f_5.checkNumeric ("C");
                    d0.set (f_5.getInt(5));
                    d1.set (15);
                    d0.div (d1);
                    d0.getField (f_6, 4);
                  }
                }
              }
              /* fizzbuzz.cob:10: COMPUTE */
              cobolErrorOnExitFlag = true;
              {
                {
                  {
                    f_5.checkNumeric ("C");
                    d0.set (f_5.getInt(5));
                    f_6.checkNumeric ("X");
                    d1.set (f_6.getInt());
                    d2.set (15);
                    d1.mul (d2);
                    d0.sub (d1);
                    d0.getField (f_6, 4);
                  }
                }
              }
              /* fizzbuzz.cob:11: EVALUATE */
              {
                if (((int)f_6.compareTo (c_2) == 0))
                  {
                    /* fizzbuzz.cob:13: DISPLAY */
                    {
                      CobolTerminal.display (0, 1, 1, c_3);
                    }
                  }
                else
                  if ((((((int)f_6.compareTo (c_4) == 0) || ((int)f_6.compareTo (c_5) == 0)) || ((int)f_6.compareTo (c_6) == 0)) || ((int)f_6.compareTo (c_7) == 0)))
                    {
                      /* fizzbuzz.cob:18: DISPLAY */
                      {
                        CobolTerminal.display (0, 1, 1, c_8);
                      }
                    }
                  else
                    if ((((int)f_6.compareTo (c_9) == 0) || ((int)f_6.compareTo (c_10) == 0)))
                      {
                        /* fizzbuzz.cob:21: DISPLAY */
                        {
                          CobolTerminal.display (0, 1, 1, c_11);
                        }
                      }
                    else
                      {
                        /* fizzbuzz.cob:23: DISPLAY */
                        {
                          CobolTerminal.display (0, 1, 1, f_5);
                        }
                      }
              }
            }
            f_5.addInt (1);
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
    new fizzbuzz().fizzbuzz_(0);
  }

  public fizzbuzz()
  {
    init();
  }

  public void init()
  {
    try {
      /* Decimal structures */

      d0 = new CobolDecimal();
      d1 = new CobolDecimal();
      d2 = new CobolDecimal();

      /* Data storage */

      cob_unifunc = null;

      /* PROGRAM-ID : fizzbuzz */
      b_1 = new CobolDataStorage(4);	/* RETURN-CODE */
      b_5 = new CobolDataStorage(5);	/* C */
      b_6 = new CobolDataStorage(5);	/* X */

      /* End of data storage */


      initAttr();

      /* Fields */

      /* PROGRAM-ID : fizzbuzz */
      f_5	= CobolFieldFactory.makeCobolField(5, b_5, a_1);	/* C */
      f_6	= CobolFieldFactory.makeCobolField(5, b_6, a_2);	/* X */

      /* End of fields */


      /* Constants */

      c_1	= CobolFieldFactory.makeCobolField(3, "100", a_3);
      c_2	= CobolFieldFactory.makeCobolField(1, "0", a_4);
      c_3	= CobolFieldFactory.makeCobolField(8, "FizzBuzz", a_5);
      c_4	= CobolFieldFactory.makeCobolField(1, "3", a_4);
      c_5	= CobolFieldFactory.makeCobolField(1, "6", a_4);
      c_6	= CobolFieldFactory.makeCobolField(1, "9", a_4);
      c_7	= CobolFieldFactory.makeCobolField(2, "12", a_6);
      c_8	= CobolFieldFactory.makeCobolField(4, "Fizz", a_5);
      c_9	= CobolFieldFactory.makeCobolField(1, "5", a_4);
      c_10	= CobolFieldFactory.makeCobolField(2, "10", a_6);
      c_11	= CobolFieldFactory.makeCobolField(4, "Buzz", a_5);

    } catch(Exception e) {
      e.printStackTrace();
    }
  }

  private void initAttr() {
    /* Attributes */

    a_1 = new CobolFieldAttribute (16, 5, 0, 0, null);
    a_2 = new CobolFieldAttribute (16, 5, 0, 1, null);
    a_3 = new CobolFieldAttribute (16, 3, 0, 0, null);
    a_4 = new CobolFieldAttribute (16, 1, 0, 0, null);
    a_5 = new CobolFieldAttribute (33, 0, 0, 0, null);
    a_6 = new CobolFieldAttribute (16, 2, 0, 0, null);

  }

  /* Decimal structures */

  private CobolDecimal d0;
  private CobolDecimal d1;
  private CobolDecimal d2;

  /* Data storage */

  /* PROGRAM-ID : fizzbuzz */
  private CobolDataStorage b_1;	/* RETURN-CODE */
  private CobolDataStorage b_5;	/* C */
  private CobolDataStorage b_6;	/* X */

  /* End of data storage */


  /* Fields */

  /* PROGRAM-ID : fizzbuzz */
  private AbstractCobolField f_5;	/* C */
  private AbstractCobolField f_6;	/* X */

  /* End of fields */


  /* Constants */

  private AbstractCobolField c_11;
  private AbstractCobolField c_10;
  private AbstractCobolField c_9;
  private AbstractCobolField c_8;
  private AbstractCobolField c_7;
  private AbstractCobolField c_6;
  private AbstractCobolField c_5;
  private AbstractCobolField c_4;
  private AbstractCobolField c_3;
  private AbstractCobolField c_2;
  private AbstractCobolField c_1;

  /* Attributes */

  private CobolFieldAttribute a_6;
  private CobolFieldAttribute a_5;
  private CobolFieldAttribute a_4;
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
