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

public class sub implements CobolRunnable {

  private boolean initialized = false;
  private CobolModule cobolCurrentModule;
  private int frameIndex;
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
    return sub_(0, argStorages);
  }

  @Override
  public void cancel() {
    sub_(-1);
  }

  @Override
  public boolean isActive() {
    return false;
  }

  CobolResultSet execute (String arg_string, int arg_num) throws CobolResultSetException {
    this.f_arg_string.setDataStorage(new CobolDataStorage(5));
    this.f_arg_string.moveFrom(arg_string);
    this.b_arg_string = this.f_arg_string.getDataStorage();
    this.f_arg_num.setDataStorage(new CobolDataStorage(3));
    this.f_arg_num.moveFrom(arg_num);
    this.b_arg_num = this.f_arg_num.getDataStorage();
    int returnCode = run_module(0);
    return new CobolResultSet(returnCode,
      new CobolResultString(f_arg_string.getString()),
      new CobolResultInt(f_arg_num.getInt())
    );
  }

  int sub_ (int entry, CobolDataStorage ...argStorages) {
    this.entry = entry;
    this.b_arg_string = 0 < argStorages.length ? argStorages[0] : null;
    this.b_arg_num = 1 < argStorages.length ? argStorages[1] : null;
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
    /* Entry sub */
    new CobolControl(1, CobolControl.LabelType.label) {
      public Optional<CobolControl> run() throws CobolRuntimeException, CobolGoBackException, CobolStopRunException {

        return Optional.of(contList[2]);
      }
    },
    /* main SECTION */
    new CobolControl(2, CobolControl.LabelType.section) {
      public Optional<CobolControl> run() throws CobolRuntimeException, CobolGoBackException, CobolStopRunException {

        return Optional.of(contList[3]);
      }
    },
    /* MAIN PARAGRAPH */
    new CobolControl(3, CobolControl.LabelType.label) {
      public Optional<CobolControl> run() throws CobolRuntimeException, CobolGoBackException, CobolStopRunException {
        /* sub.cbl:11: DISPLAY */
        {
          CobolTerminal.display (0, 1, 1, new GetAbstractCobolField() { public AbstractCobolField run() { f_arg_string.setDataStorage(b_arg_string); return f_arg_string; }}.run());
        }
        /* sub.cbl:12: DISPLAY */
        {
          CobolTerminal.display (0, 1, 1, new GetAbstractCobolField() { public AbstractCobolField run() { f_arg_num.setDataStorage(b_arg_num); return f_arg_num; }}.run());
        }
        /* sub.cbl:13: ADD */
        {
          new GetAbstractCobolField() { public AbstractCobolField run() { f_arg_num.setDataStorage(b_arg_num); return f_arg_num; }}.run().add (c_1, 4);
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
    new sub().sub_(0);
    CobolStopRunException.stopRun();
  }

  public sub()
  {
    init();
  }

  public void init()
  {
    try {
      /* Data storage */

      cob_unifunc = null;

      /* PROGRAM-ID : sub */
      b_RETURN_CODE = new CobolDataStorage(4);	/* RETURN-CODE */

      /* End of data storage */


      initAttr();

      /* Fields */

      /* PROGRAM-ID : sub */
      f_arg_string	= CobolFieldFactory.makeCobolField(5, (CobolDataStorage)null, a_1);	/* arg-string */
      f_arg_num	= CobolFieldFactory.makeCobolField(3, (CobolDataStorage)null, a_2);	/* arg-num */

      /* End of fields */


      /* Constants */

      c_1	= CobolFieldFactory.makeCobolField(1, "1", a_3);

    } catch(Exception e) {
      e.printStackTrace();
    }
  }

  private void initAttr() {
    /* Attributes */

    a_1 = new CobolFieldAttribute (33, 0, 0, 0, null);
    a_2 = new CobolFieldAttribute (16, 3, 0, 0, null);
    a_3 = new CobolFieldAttribute (16, 1, 0, 0, null);

  }

  /* Data storage */

  /* PROGRAM-ID : sub */
  private CobolDataStorage b_RETURN_CODE;	/* RETURN-CODE */

  /* End of data storage */


  /* Fields */

  /* PROGRAM-ID : sub */
  private AbstractCobolField f_arg_string;	/* arg-string */
  private AbstractCobolField f_arg_num;	/* arg-num */

  /* End of fields */


  private static AbstractCobolField f_native;

  /* Constants */

  private AbstractCobolField c_1;

  /* Attributes */

  private CobolFieldAttribute a_3;
  private CobolFieldAttribute a_2;
  private CobolFieldAttribute a_1;

  /* Call parameters */
  private CobolDataStorage b_arg_num;
  private CobolDataStorage b_arg_string;



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
