package test;
import jp.osscons.opensourcecobol.libcobj.call.CobolRunnable;
import jp.osscons.opensourcecobol.libcobj.common.CobolCallParams;
import jp.osscons.opensourcecobol.libcobj.common.CobolFrame;
import jp.osscons.opensourcecobol.libcobj.common.CobolModule;
import jp.osscons.opensourcecobol.libcobj.data.AbstractCobolField;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;
import jp.osscons.opensourcecobol.libcobj.data.CobolDecimal;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldAttribute;
import jp.osscons.opensourcecobol.libcobj.data.CobolFieldFactory;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolGoBackException;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolRuntimeException;
import jp.osscons.opensourcecobol.libcobj.exceptions.CobolStopRunException;
import jp.osscons.opensourcecobol.libcobj.file.CobolFile;
import jp.osscons.opensourcecobol.libcobj.file.CobolFileFactory;
import jp.osscons.opensourcecobol.libcobj.termio.CobolTerminal;

public class reads implements CobolRunnable {
  interface __B {
    public AbstractCobolField run();
  }
  interface __C {
    public int run();
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
    return reads_(0, fields);
  }

  @Override
  public void cancel() {
    reads_(-1);
  }

  @Override
  public boolean isActive() {
    return false;
  }

  int reads_ (int entry, AbstractCobolField... fields )
  {
    this.initialized = false;


    CobolModule module = new CobolModule(null, null, null, null, 0, '.', '\\', ',', 1, 1, 1, 0, null );

    /* Start of function code */

    /* CANCEL callback handling */
    if (entry < 0) {
    	if (!this.initialized) {
    		/* TODO cob_init()に置き換える */
    		CobolDecimal.cobInitNumeric();
    		return 0;
    	}
    	h_DATA_FILE.close (0, null);
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
    CobolModule.push (module);

    b_1.set(0);
    b_10.fillBytes((byte)(32), 2);
    b_11.setByte((byte)32);

    h_DATA_FILE = CobolFileFactory.makeCobolFileInstance(
    /* select_name = */ "DATA-FILE",
    /* file_status = */ h_DATA_FILE_status,
    /* assign = */ c_1,
    /* record = */ f_9,
    /* record_size = */ null,
    /* record_min = */ 23,
    /* record_max = */ 23,
    /* nkeys = */ 0,
    /* keys = */ null,
    /* organization = */ (char)0,
    /* access_mode = */ (char)1,
    /* lock_mode = */ (char)0,
    /* open_mode = */ (char)0,
    /* flag_optional = */ false,
    /* last_open_mode = */ (char)0,
    /* special = */ (char)0,
    /* flag_nonexistent = */ false,
    /* flag_end_of_file = */ false,
    /* flag_begin_of_file = */ false,
    /* flag_first_read = */ (char)0,
    /* flag_read_done = */ false,
    /* flag_select_features = */ (char)1,
    /* flag_needs_nl = */ false,
    /* flag_needs_top = */ false,
    /* file_version = */ (char)0
    );

    /* PROCEDURE DIVISION */
    try{
      CobolStopRunException.dummy();
      CobolGoBackException.dummy();

      /* Entry reads */

      /* MAIN SECTION */

      /* MAIN PARAGRAPH */

      /* reads.cob:26: OPEN */
      {
        CobolRuntimeException.code = 0;
        {
          h_DATA_FILE.open (1, 0, f_10);
        }
        if (CobolRuntimeException.code != 0)
          {
            /* perform call */
            throw new CobolRuntimeException(0, "");
          }
      }
      /* reads.cob:33: SET */
      {
        b_11.setByte((byte)49);
      }
      /* reads.cob:34: PERFORM */
      {
        for (;;)
          {
            if (((int)f_11.compareTo (c_2) == 0))
              break;
            {
              /* reads.cob:35: READ */
              CobolRuntimeException.code = 0;
              {
                h_DATA_FILE.read (0, f_10, 1);
              }
              if (CobolRuntimeException.code != 0)
                {
                  if (CobolRuntimeException.code == 0x0501)
                    {
                      {
                        /* reads.cob:37: DISPLAY */
                        {
                          CobolTerminal.display (0, 1, 1, c_3);
                        }
                        /* reads.cob:38: SET */
                        {
                          b_11.setByte((byte)50);
                        }
                      }
                    }
                  else
                    {
                      /* perform call */
                      throw new CobolRuntimeException(0, "");
                    }
                }
              else
                {
                  {
                    /* reads.cob:40: DISPLAY */
                    {
                      CobolTerminal.display (0, 1, 2, c_4, f_6);
                    }
                    /* reads.cob:41: DISPLAY */
                    {
                      CobolTerminal.display (0, 1, 2, c_5, f_7);
                    }
                    /* reads.cob:42: DISPLAY */
                    {
                      CobolTerminal.display (0, 1, 2, c_6, f_8);
                    }
                  }
                }
            }
          }
      }
      /* reads.cob:46: CLOSE */
      {
        CobolRuntimeException.code = 0;
        {
          h_DATA_FILE.close (0, f_10);
        }
        if (CobolRuntimeException.code != 0)
          {
            /* perform call */
            throw new CobolRuntimeException(0, "");
          }
      }
    } catch(CobolRuntimeException e) {
      System.out.println("Runtime Error");
      System.out.println("code = " + CobolRuntimeException.code);
      System.out.println("message = " + e.getMessage());
    } catch(CobolGoBackException e) {
      return e.getReturnCode();
    } catch(CobolStopRunException e) {
      System.exit(e.getReturnCode());
    }
    /* Program return */
    return b_1.intValue ();
  }

  public static void main(String[] args)
  {
    /* TODO cob_init()に変更する */
    CobolDecimal.cobInitNumeric();
    new reads().reads_(0);
  }

  public reads()
  {
    init();
  }

  public void init()
  {
    try {
      /* Data storage */

      cob_unifunc = null;

      /* PROGRAM-ID : reads */
      b_1 = new CobolDataStorage(4);	/* RETURN-CODE */
      b_9 = new CobolDataStorage(23);	/* DATA-FILE_record */
      b_10 = new CobolDataStorage(2);	/* FILE-STATUS */
      b_11 = new CobolDataStorage(1);	/* STATE */

      /* End of data storage */


      initAttr();

      /* Fields */

      /* PROGRAM-ID : reads */
      f_6	= CobolFieldFactory.makeCobolField(5, b_9, a_1);	/* CODE1 */
      f_7	= CobolFieldFactory.makeCobolField(3, b_9.getSubDataStorage(5), a_1);	/* CODE2 */
      f_8	= CobolFieldFactory.makeCobolField(15, b_9.getSubDataStorage(8), a_1);	/* NAME */
      f_9	= CobolFieldFactory.makeCobolField(23, b_9, a_1);	/* DATA-FILE_record */
      f_10	= CobolFieldFactory.makeCobolField(2, b_10, a_1);	/* FILE-STATUS */
      f_11	= CobolFieldFactory.makeCobolField(1, b_11, a_1);	/* STATE */

      /* End of fields */


      /* Constants */

      c_1	= CobolFieldFactory.makeCobolField(14, "./data_sam.txt", a_1);
      c_2	= CobolFieldFactory.makeCobolField(1, "2", a_1);
      c_3	= CobolFieldFactory.makeCobolField(8, "END-READ", a_1);
      c_4	= CobolFieldFactory.makeCobolField(7, "CODE1: ", a_1);
      c_5	= CobolFieldFactory.makeCobolField(7, "CODE2: ", a_1);
      c_6	= CobolFieldFactory.makeCobolField(6, "NAME: ", a_1);

    } catch(Exception e) {
      e.printStackTrace();
    }
  }

  private void initAttr() {
    /* Attributes */

    a_1 = new CobolFieldAttribute (33, 0, 0, 0, null);

  }

  /* Data storage */

  /* PROGRAM-ID : reads */
  private CobolDataStorage b_1;	/* RETURN-CODE */
  private CobolDataStorage b_9;	/* DATA-FILE_record */
  private CobolDataStorage b_10;	/* FILE-STATUS */
  private CobolDataStorage b_11;	/* STATE */

  /* End of data storage */


  /* Fields */

  /* PROGRAM-ID : reads */
  private AbstractCobolField f_6;	/* CODE1 */
  private AbstractCobolField f_7;	/* CODE2 */
  private AbstractCobolField f_8;	/* NAME */
  private AbstractCobolField f_9;	/* DATA-FILE_record */
  private AbstractCobolField f_10;	/* FILE-STATUS */
  private AbstractCobolField f_11;	/* STATE */

  /* End of fields */


  /* Constants */

  private AbstractCobolField c_6;
  private AbstractCobolField c_5;
  private AbstractCobolField c_4;
  private AbstractCobolField c_3;
  private AbstractCobolField c_2;
  private AbstractCobolField c_1;

  /* Attributes */

  private CobolFieldAttribute a_1;


  /* File DATA-FILE */
  CobolFile		h_DATA_FILE = null;
  byte[]	h_DATA_FILE_status = new byte[4];

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
