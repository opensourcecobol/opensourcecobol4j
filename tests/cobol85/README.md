# How to run the NIST CCVS85 (aka. ANSI85) Test Suite

**NOTE** It is expected that WARNING messages appear when running the test.

**NOTE** The language interpreter "perl" is required to run these tests.

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
