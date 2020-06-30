package test;
import java.io.UnsupportedEncodingException;
import jp.osscons.opensourcecobol.libcobj.*;
import jp.osscons.opensourcecobol.libcobj.common.*;
import jp.osscons.opensourcecobol.libcobj.data.*;
import jp.osscons.opensourcecobol.libcobj.exceptions.*;
import jp.osscons.opensourcecobol.libcobj.termio.*;
import jp.osscons.opensourcecobol.libcobj.call.*;
import jp.osscons.opensourcecobol.libcobj.file.*;

public class cob implements CobolRunnable {
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
    return cob_(0, fields);
  }

  @Override
  public void cancel() {
    cob_(-1);
  }

  @Override
  public boolean isActive() {
    return false;
  }

  int cob_ (int entry, AbstractCobolField... fields )
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

      /* Entry cob */

      /* MAIN SECTION */

      /* MAIN PARAGRAPH */

      /* cob.cob:23: OPEN */
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
      /* cob.cob:25: MOVE */
      {
        b_9.setBytes ("00011", 5);
      }
      /* cob.cob:26: MOVE */
      {
        b_9.getSubDataStorage(5).setBytes ("789", 3);
      }
      /* cob.cob:27: MOVE */
      {
        b_9.getSubDataStorage(8).setBytes ("Suzuki Taro    ", 15);
      }
      /* cob.cob:28: WRITE */
      CobolRuntimeException.code = 0;
      {
        h_DATA_FILE.write (f_5, 0, f_10);
      }
      if (CobolRuntimeException.code != 0)
        {
          /* perform call */
          throw new CobolRuntimeException(0, "");
        }
      /* cob.cob:31: MOVE */
      {
        b_9.setBytes ("00022", 5);
      }
      /* cob.cob:32: MOVE */
      {
        b_9.getSubDataStorage(5).setBytes ("123", 3);
      }
      /* cob.cob:33: MOVE */
      {
        b_9.getSubDataStorage(8).setBytes ("Sato Jiro      ", 15);
      }
      /* cob.cob:34: WRITE */
      CobolRuntimeException.code = 0;
      {
        h_DATA_FILE.write (f_5, 0, f_10);
      }
      if (CobolRuntimeException.code != 0)
        {
          /* perform call */
          throw new CobolRuntimeException(0, "");
        }
      /* cob.cob:37: CLOSE */
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
    new cob().cob_(0);
  }

  public cob()
  {
    init();
  }

  public void init()
  {
    try {
      /* Data storage */

      cob_unifunc = null;

      /* PROGRAM-ID : cob */
      b_1 = new CobolDataStorage(4);	/* RETURN-CODE */
      b_9 = new CobolDataStorage(23);	/* DATA-FILE_record */
      b_10 = new CobolDataStorage(2);	/* FILE-STATUS */

      /* End of data storage */


      initAttr();

      /* Fields */

      /* PROGRAM-ID : cob */
      f_5	= CobolFieldFactory.makeCobolField(23, b_9, a_2);	/* FILE-RECORD */
      f_9	= CobolFieldFactory.makeCobolField(23, b_9, a_1);	/* DATA-FILE_record */
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
    a_2 = new CobolFieldAttribute (1, 0, 0, 0, null);

  }

  /* Data storage */

  /* PROGRAM-ID : cob */
  private CobolDataStorage b_1;	/* RETURN-CODE */
  private CobolDataStorage b_9;	/* DATA-FILE_record */
  private CobolDataStorage b_10;	/* FILE-STATUS */

  /* End of data storage */


  /* Fields */

  /* PROGRAM-ID : cob */
  private AbstractCobolField f_5;	/* FILE-RECORD */
  private AbstractCobolField f_9;	/* DATA-FILE_record */
  private AbstractCobolField f_10;	/* FILE-STATUS */

  /* End of fields */


  /* Constants */

  private AbstractCobolField c_1;

  /* Attributes */

  private CobolFieldAttribute a_2;
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

