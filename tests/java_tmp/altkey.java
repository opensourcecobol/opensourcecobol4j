package test;
import jp.osscons.opensourcecobol.libcobj.call.CobolRunnable;
import jp.osscons.opensourcecobol.libcobj.common.CobolCallParams;
import jp.osscons.opensourcecobol.libcobj.common.CobolFrame;
import jp.osscons.opensourcecobol.libcobj.common.CobolModule;
import jp.osscons.opensourcecobol.libcobj.common.CobolUtil;
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
import jp.osscons.opensourcecobol.libcobj.file.CobolFileKey;
import jp.osscons.opensourcecobol.libcobj.termio.CobolTerminal;

public class altkey implements CobolRunnable {
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
  private static boolean cobolInitialized = false;
  private CobolCallParams cobolSaveCallParams = null;
  private CobolCallParams cobolCallParams = null;
  private boolean cobolErrorOnExitFlag;

  private CobolRunnable cob_unifunc;


  @Override
  public int run(AbstractCobolField... fields) {
    return altkey_(0, fields);
  }

  @Override
  public void cancel() {
    altkey_(-1);
  }

  @Override
  public boolean isActive() {
    return false;
  }

  int altkey_ (int entry, AbstractCobolField... fields )
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

    if (k_DATA_FILE == null)
    {
      k_DATA_FILE = new CobolFileKey[2];
      for (int i=0; i<2; ++i)
        k_DATA_FILE[i] = new CobolFileKey();
    }
    k_DATA_FILE[0].setField(f_6);
    k_DATA_FILE[0].setFlag(0);
    k_DATA_FILE[0].setOffset(0);
    k_DATA_FILE[1].setField(f_7);
    k_DATA_FILE[1].setFlag(0);
    k_DATA_FILE[1].setOffset(9);
    h_DATA_FILE = CobolFileFactory.makeCobolFileInstance(
    /* select_name = */ "DATA-FILE",
    /* file_status = */ h_DATA_FILE_status,
    /* assign = */ c_1,
    /* record = */ f_9,
    /* record_size = */ null,
    /* record_min = */ 17,
    /* record_max = */ 17,
    /* nkeys = */ 2,
    /* keys = */ k_DATA_FILE,
    /* organization = */ (char)3,
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

      /* Entry altkey */

      /* MAIN SECTION */

      /* MAIN PARAGRAPH */

      /* altkey.cob:27: OPEN */
      {
        CobolRuntimeException.code = 0;
        {
          h_DATA_FILE.open (2, 1, f_10);
        }
        if (CobolRuntimeException.code != 0)
          {
            /* perform call */
          }
      }
      /* altkey.cob:28: MOVE */
      {
        b_9.setBytes ("bbb      ", 9);
      }
      /* altkey.cob:29: MOVE */
      {
        b_9.getSubDataStorage(9).setBytes ("aaa  ", 5);
      }
      /* altkey.cob:30: MOVE */
      {
        b_9.getSubDataStorage(14).setBytes ("123", 3);
      }
      /* altkey.cob:31: WRITE */
      CobolRuntimeException.code = 0;
      {
        h_DATA_FILE.write (f_5, 0, f_10);
      }
      if (CobolRuntimeException.code != 0)
        {
          /* perform call */
        }
      /* altkey.cob:32: DISPLAY */
      {
        CobolTerminal.display (0, 1, 2, c_2, f_10);
      }
      /* altkey.cob:33: MOVE */
      {
        b_9.setBytes ("bbb      ", 9);
      }
      /* altkey.cob:34: MOVE */
      {
        b_9.getSubDataStorage(9).setBytes ("xxx  ", 5);
      }
      /* altkey.cob:35: MOVE */
      {
        b_9.getSubDataStorage(14).setBytes ("789", 3);
      }
      /* altkey.cob:36: WRITE */
      CobolRuntimeException.code = 0;
      {
        h_DATA_FILE.write (f_5, 0, f_10);
      }
      if (CobolRuntimeException.code != 0)
        {
          /* perform call */
        }
      /* altkey.cob:37: DISPLAY */
      {
        CobolTerminal.display (0, 1, 2, c_2, f_10);
      }
      /* altkey.cob:38: CLOSE */
      {
        CobolRuntimeException.code = 0;
        {
          h_DATA_FILE.close (0, f_10);
        }
        if (CobolRuntimeException.code != 0)
          {
            /* perform call */
          }
      }
      /* altkey.cob:40: MOVE */
      {
        b_9.setBytes ("bbb      ", 9);
      }
      /* altkey.cob:41: MOVE */
      {
        b_9.getSubDataStorage(9).setBytes ("aaa  ", 5);
      }
      /* altkey.cob:42: MOVE */
      {
        b_9.getSubDataStorage(9).setBytes ("___  ", 5);
      }
      /* altkey.cob:43: OPEN */
      {
        CobolRuntimeException.code = 0;
        {
          h_DATA_FILE.open (1, 0, f_10);
        }
        if (CobolRuntimeException.code != 0)
          {
            /* perform call */
          }
      }
      /* altkey.cob:44: READ */
      CobolRuntimeException.code = 0;
      {
        h_DATA_FILE.read (0, f_10, 1);
      }
      if (CobolRuntimeException.code != 0)
        {
          /* perform call */
        }
      /* altkey.cob:45: DISPLAY */
      {
        CobolTerminal.display (0, 1, 2, c_3, f_6);
      }
      /* altkey.cob:46: DISPLAY */
      {
        CobolTerminal.display (0, 1, 2, c_4, f_7);
      }
      /* altkey.cob:47: DISPLAY */
      {
        CobolTerminal.display (0, 1, 2, c_5, f_8);
      }
      /* altkey.cob:48: DISPLAY */
      {
        CobolTerminal.display (0, 1, 2, c_2, f_10);
      }
      /* altkey.cob:49: CLOSE */
      {
        CobolRuntimeException.code = 0;
        {
          h_DATA_FILE.close (0, f_10);
        }
        if (CobolRuntimeException.code != 0)
          {
            /* perform call */
          }
      }
    } catch(CobolRuntimeException e) {
      CobolStopRunException.stopRun();
    } catch(CobolGoBackException e) {
      return e.getReturnCode();
    } catch(CobolStopRunException e) {
      CobolStopRunException.stopRun();
      System.exit(e.getReturnCode());
    }
    //cobcrunで呼ばれるcob_stop_run
    //TODO 適切な場所に移す
    CobolStopRunException.stopRun();
    /* Program return */
    return b_1.intValue ();
  }

  public static void main(String[] args)
  {
    /* TODO cob_init()に変更する */
    /* TODO 暫定的にcob_initを読んでいるだけ */
    CobolUtil.cob_init(args, cobolInitialized);
    CobolDecimal.cobInitNumeric();
    new altkey().altkey_(0);
  }

  public altkey()
  {
    init();
  }

  public void init()
  {
    try {
      /* Data storage */

      cob_unifunc = null;

      /* PROGRAM-ID : altkey */
      b_1 = new CobolDataStorage(4);	/* RETURN-CODE */
      b_9 = new CobolDataStorage(17);	/* DATA-FILE_record */
      b_10 = new CobolDataStorage(2);	/* FILE-STATUS */

      /* End of data storage */


      initAttr();

      /* Fields */

      /* PROGRAM-ID : altkey */
      f_5	= CobolFieldFactory.makeCobolField(17, b_9, a_2);	/* FILE-RECORD */
      f_6	= CobolFieldFactory.makeCobolField(9, b_9, a_1);	/* NAME */
      f_7	= CobolFieldFactory.makeCobolField(5, b_9.getSubDataStorage(9), a_1);	/* CODE1 */
      f_8	= CobolFieldFactory.makeCobolField(3, b_9.getSubDataStorage(14), a_1);	/* CODE2 */
      f_9	= CobolFieldFactory.makeCobolField(17, b_9, a_1);	/* DATA-FILE_record */
      f_10	= CobolFieldFactory.makeCobolField(2, b_10, a_1);	/* FILE-STATUS */

      /* End of fields */


      /* Constants */

      c_1	= CobolFieldFactory.makeCobolField(11, "altkey_data", a_1);
      c_2	= CobolFieldFactory.makeCobolField(13, "FILE-STAUTS: ", a_1);
      c_3	= CobolFieldFactory.makeCobolField(6, "NAME: ", a_1);
      c_4	= CobolFieldFactory.makeCobolField(7, "CODE1: ", a_1);
      c_5	= CobolFieldFactory.makeCobolField(7, "CODE2: ", a_1);

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

  /* PROGRAM-ID : altkey */
  private CobolDataStorage b_1;	/* RETURN-CODE */
  private CobolDataStorage b_9;	/* DATA-FILE_record */
  private CobolDataStorage b_10;	/* FILE-STATUS */

  /* End of data storage */


  /* Fields */

  /* PROGRAM-ID : altkey */
  private AbstractCobolField f_5;	/* FILE-RECORD */
  private AbstractCobolField f_6;	/* NAME */
  private AbstractCobolField f_7;	/* CODE1 */
  private AbstractCobolField f_8;	/* CODE2 */
  private AbstractCobolField f_9;	/* DATA-FILE_record */
  private AbstractCobolField f_10;	/* FILE-STATUS */

  /* End of fields */


  /* Constants */

  private AbstractCobolField c_5;
  private AbstractCobolField c_4;
  private AbstractCobolField c_3;
  private AbstractCobolField c_2;
  private AbstractCobolField c_1;

  /* Attributes */

  private CobolFieldAttribute a_2;
  private CobolFieldAttribute a_1;


  /* File DATA-FILE */
  CobolFileKey[]	k_DATA_FILE = null;
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

  private static CobolDataStorage makeCobolDataStorage(byte ...bytes) {
    return new CobolDataStorage(bytes);
  }
}
