# How to run the NIST CCVS85 (aka. ANSI85) Test Suite

**NOTE** It is expected that WARNING messages appear when running the test.

**NOTE** The test driver is written in POSIX shell, so a POSIX shell (`sh`) and `awk` are required to run these tests.
On Windows, run the driver from a POSIX shell environment such as Git Bash or MSYS2.

**NOTE** This test can take a long time depending on your hardware.

The final command of the the test is a diff between expected results and actual results.
ie. diff summary.txt summary.log.


1. Run the test suite:

```shell
	cd tests/cobol85
	make test
```

2. Test report summary will be put in summary.log.

3. The default configuration tests are NC SM IC SQ RL IX ST SG OB IF

# Make Options

- `make test`: run the test suite
- `make test-extra`: run the extra test suite
- `make save`: save test reports in *.txt
- `make diff`: diff from *.txt to the last reports
- `make clean`: remove built files

# Test Driver Scripts

The test suite is driven by the following POSIX shell scripts.

- `report.sh`: run the test programs of one module directory and write the result table to `report.txt`.
  It is invoked by `make test` in each module directory (`sh ../report.sh`).
  All COBOL sources of the module are compiled with a single `cobj` invocation so that `javac` runs only once.
  If the batch compilation fails, the sources are compiled one by one so that a compile error is still reported per program.
  Extra options given to the script (`sh ../report.sh [extra-cobj-options]`) are passed through to `cobj`.
- `summary.sh`: print the summary table of the given modules. Used as `sh summary.sh MODULE... > summary.log`.
- `expand.sh`: expand the NIST test suite archive into the per-module directories. Used as `sh expand.sh newcob.val`.

# Test Modules

Core tests:

* `NC`: COBOL nucleus tests
* `SM`: COPY sentence tests
* `IC`: CALL sentence tests

File I-O tests:

* `SQ`: Sequential file I-O tests
* `RL`: Relative file I-O tests
* `IX`: Indexed file I-O tests
* `ST`: SORT sentence tests

Advanced facilities:

* `IF`: Intrinsic Function tests
* `SG`: Segment tests
* `OB`: Obsolete facilities tests

Extra tests:

* `CM`: COMMUNICATION SECTION tests
* `DB`: Debugging facilities tests
* `RW`: REPORT SECTION tests
