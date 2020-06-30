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

public class samwrite implements CobolRunnable {
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
    return samwrite_(0, fields);
  }

  @Override
  public void cancel() {
    samwrite_(-1);
  }

  @Override
  public boolean isActive() {
    return false;
  }

  int samwrite_ (int entry, AbstractCobolField... fields )
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

    h_DATA_FILE = CobolFileFactory.makeCobolFileInstance(
    /* select_name = */ "DATA-FILE",
    /* file_status = */ h_DATA_FILE_status,
    /* assign = */ c_1,
    /* record = */ f_9,
    /* record_size = */ null,
    /* record_min = */ 18,
    /* record_max = */ 18,
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

      /* Entry samwrite */

      /* MAIN SECTION */

      /* MAIN PARAGRAPH */

      /* samwrite.cob:23: OPEN */
      {
        CobolRuntimeException.code = 0;
        {
          h_DATA_FILE.open (2, 1, f_10);
        }
        if (CobolRuntimeException.code != 0)
          {
            /* perform call */
            throw new CobolRuntimeException(0, "");
          }
      }
      /* samwrite.cob:31: CLOSE */
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
    new samwrite().samwrite_(0);
  }

  public samwrite()
  {
    init();
  }

  public void init()
  {
    try {
      /* Data storage */

      cob_unifunc = null;

      /* PROGRAM-ID : samwrite */
      b_1 = new CobolDataStorage(4);	/* RETURN-CODE */
      b_9 = new CobolDataStorage(18);	/* DATA-FILE_record */
      b_10 = new CobolDataStorage(2);	/* FILE-STATUS */

      /* End of data storage */


      initAttr();

      /* Fields */

      /* PROGRAM-ID : samwrite */
      f_9	= CobolFieldFactory.makeCobolField(18, b_9, a_1);	/* DATA-FILE_record */
      f_10	= CobolFieldFactory.makeCobolField(2, b_10, a_1);	/* FILE-STATUS */

      /* End of fields */


      /* Constants */

      c_1	= CobolFieldFactory.makeCobolField(14, "./data_sam.txt", a_1);

    } catch(Exception e) {
      e.printStackTrace();
    }
  }

  private void initAttr() {
    /* Attributes */

    a_1 = new CobolFieldAttribute (33, 0, 0, 0, null);

  }

  /* Data storage */

  /* PROGRAM-ID : samwrite */
  private CobolDataStorage b_1;	/* RETURN-CODE */
  private CobolDataStorage b_9;	/* DATA-FILE_record */
  private CobolDataStorage b_10;	/* FILE-STATUS */

  /* End of data storage */


  /* Fields */

  /* PROGRAM-ID : samwrite */
  private AbstractCobolField f_9;	/* DATA-FILE_record */
  private AbstractCobolField f_10;	/* FILE-STATUS */

  /* End of fields */


  /* Constants */

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
