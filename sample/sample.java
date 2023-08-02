import java.io.UnsupportedEncodingException;
import jp.osscons.opensourcecobol.libcobj.*;
import jp.osscons.opensourcecobol.libcobj.common.*;
import jp.osscons.opensourcecobol.libcobj.data.*;
import jp.osscons.opensourcecobol.libcobj.exceptions.*;
import jp.osscons.opensourcecobol.libcobj.termio.*;
import jp.osscons.opensourcecobol.libcobj.call.*;
import jp.osscons.opensourcecobol.libcobj.file.*;
import jp.osscons.opensourcecobol.libcobj.ui.*;
import java.util.Optional;

public class sample implements CobolRunnable {

  private boolean initialized = false;
  private CobolModule cobolCurrentModule;
  private CobolModule module;
  private CobolFrame frame;
  private static boolean cobolInitialized = false;
  private CobolCallParams cobolSaveCallParams = null;
  private CobolCallParams cobolCallParams = null;
  private boolean cobolErrorOnExitFlag;
  private int entry;

  private CobolRunnable cob_unifunc;


  @Override
  public int run(CobolDataStorage... argStorages) {
    return sample_(0, argStorages);
  }

  @Override
  public void cancel() {
    sample_(-1);
  }

  @Override
  public boolean isActive() {
    return false;
  }

  public CobolResultSet execute () {
    int returnCode = run_module(0);
    return new CobolResultSet(returnCode);
  }

  public int sample_ (int entry, CobolDataStorage ...argStorages) {
    this.entry = entry;
    return this.run_module(entry);
  }

  int run_module (int entry) {
    this.module = new CobolModule(null, null, null, null, 0, '.', '$', ',', 1, 1, 1, 0, null );

    /* Start of function code */

    /* CANCEL callback handling */
    if (entry < 0) {
    	if (!this.initialized) {
    		CobolDecimal.cobInitNumeric();
    		return 0;
    	}
    	this.initialized = false;
    	return 0;
    }

    /* Push module stack */
    CobolModule.push (module);

    /* Initialize program */
    if (!this.initialized) {
      b_RETURN_CODE.set((int)0);
      b_COUNT_OF_RECORDS.fillBytes(0, 4);
      this.initialized = true;
    }
    /* PROCEDURE DIVISION */
    try{
      CobolStopRunException.dummy();
      CobolGoBackException.dummy();
      /* Entry dispatch */
      execEntry(1);

    } catch(CobolGoBackException e) {
      return e.getReturnCode();
    } catch(CobolStopRunException e) {
      CobolStopRunException.stopRun();
      System.exit(e.getReturnCode());
    }
    /* Pop module stack */
    CobolModule.pop();

    /* Program return */
    return b_RETURN_CODE.intValue();
  }
  public CobolControl[] contList = {
    new CobolControl(0, CobolControl.LabelType.label) {
      public Optional<CobolControl> run() throws CobolRuntimeException, CobolGoBackException, CobolStopRunException {

        return Optional.of(contList[1]);
      }
    },
    /* Entry sample */
    new CobolControl(1, CobolControl.LabelType.label) {
      public Optional<CobolControl> run() throws CobolRuntimeException, CobolGoBackException, CobolStopRunException {

        return Optional.of(contList[2]);
      }
    },
    /* MAIN SECTION */
    new CobolControl(2, CobolControl.LabelType.section) {
      public Optional<CobolControl> run() throws CobolRuntimeException, CobolGoBackException, CobolStopRunException {

        return Optional.of(contList[3]);
      }
    },
    /* main */
    new CobolControl(3, CobolControl.LabelType.label) {
      public Optional<CobolControl> run() throws CobolRuntimeException, CobolGoBackException, CobolStopRunException {
        /* sample.cbl:16: MOVE */
        {
          b_COUNT_OF_RECORDS.setNative((int)0,4);
        }
        /* sample.cbl:17: PERFORM */
        for (int n0 = 10; n0 > 0; n0--)
          {
            /* PERFORM test-run */
            CobolControl.perform(contList, 4).run();
          }
        /* sample.cbl:18: STOP */
        {
          CobolStopRunException.throwException (b_RETURN_CODE.intValue());
        }

        return Optional.of(contList[4]);
      }
    },
    /* test-run */
    new CobolControl(4, CobolControl.LabelType.label) {
      public Optional<CobolControl> run() throws CobolRuntimeException, CobolGoBackException, CobolStopRunException {
        /* sample.cbl:20: ADD */
        {
          f_COUNT_OF_RECORDS.addNative (c_1, 4);
        }
        /* sample.cbl:21: DISPLAY */
        {
          CobolTerminal.display (0, 1, 2, c_2, f_COUNT_OF_RECORDS);
        }
        return Optional.of(CobolControl.pure());
      }
    },
    CobolControl.pure()
  };
  public void execEntry(int start) throws CobolRuntimeException, CobolGoBackException, CobolStopRunException {
    Optional<CobolControl> nextLabel = Optional.of(contList[start]);
    while(nextLabel.isPresent()) {
      CobolControl section = nextLabel.get();
      nextLabel = section.run();
    }
  }

  public static void main(String[] args)
  {
    CobolUtil.cob_init(args, cobolInitialized);
    CobolDecimal.cobInitNumeric();
    new sample().sample_(0);
    CobolStopRunException.stopRun();
  }

  public sample()
  {
    init();
  }

  public void init()
  {
    try {
      /* Data storage */

      cob_unifunc = null;

      /* PROGRAM-ID : sample */
      b_RETURN_CODE = new CobolDataStorage(4);	/* RETURN-CODE */
      b_COUNT_OF_RECORDS = new CobolDataStorage(4);	/* COUNT-OF-RECORDS */

      /* End of data storage */


      initAttr();

      /* Fields */

      /* PROGRAM-ID : sample */
      f_COUNT_OF_RECORDS	= CobolFieldFactory.makeCobolField(4, b_COUNT_OF_RECORDS, a_1);	/* COUNT-OF-RECORDS */

      /* End of fields */


      /* Constants */

      c_1	= CobolFieldFactory.makeCobolField(1, "1", a_2);
      c_2	= CobolFieldFactory.makeCobolField(8, str_literal_0_counter, a_3);

    } catch(Exception e) {
      e.printStackTrace();
    }
  }

  private void initAttr() {
    /* Attributes */

    a_1 = new CobolFieldAttribute (17, 5, 0, 1, null);
    a_2 = new CobolFieldAttribute (16, 1, 0, 0, null);
    a_3 = new CobolFieldAttribute (33, 0, 0, 0, null);

  }

  /* Data storage */

  /* PROGRAM-ID : sample */
  private CobolDataStorage b_RETURN_CODE;	/* RETURN-CODE */
  private CobolDataStorage b_COUNT_OF_RECORDS;	/* COUNT-OF-RECORDS */

  /* End of data storage */


  /* Fields */

  /* PROGRAM-ID : sample */
  private AbstractCobolField f_COUNT_OF_RECORDS;	/* COUNT-OF-RECORDS */

  /* End of fields */


  private static AbstractCobolField f_native;

  /* Constants */

  private AbstractCobolField c_2;
  private AbstractCobolField c_1;

  /* Attributes */

  private CobolFieldAttribute a_3;
  private CobolFieldAttribute a_2;
  private CobolFieldAttribute a_1;



  /* String literals */
  public static final byte[] str_literal_0_counter = CobolUtil.stringToBytes("counter=");

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
