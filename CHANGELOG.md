# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).
## [1.0.15] - 2023-08-31
### Added
- Add new intrinsic functions
  - EXCEPTION-FILE (#218)
  - EXCEPTION-LOCATION (218)
  - EXCEPTION-STATEMENT (#221)
  - EXCEPTION-STATUS (#221)
  - FRACTION-PART (#224)
- Add `-Wredefinition` option (#223)
- Implement `FD` clause with `EXTERNAL` specified (#222)
### Fixed
- Fix a build error on some platforms (#225)
### Changed
- Improve the readability of CALL statements (#202)

## [1.0.14] - 2023-08-31
### Added
- Add `-jar` and `-single-jar` option (#171)
- Add FUNCTION COMBINED-DATETIM (#207)
- Add FUNCTION CONCATENATE (#212)
### Fixed
- Fix INDEXED files (#203)
- Fix comparison of PIC N (#208)
### Changed
- Improve the readability of CALL statements (#202)
## [1.0.13] - 2023-07-31
### Added
- Add new options
  - Add `-debug` option (#146)
  - Add `-Wparentheses` (#182)
  - Add `-Wcolumn-overflow` (#184)
  - Add `-fmfcomment` (#185)
  - Add `-ffunctions-all` (#188)
- Implement `COB_IO_ASSUME_REWRITE` (#166)
- Output version strings in generated Java files (#153)
### Fixed
- Allow string literals to be used as a call parameter (#127)
- Fix displaying numbers (#138)
- Fix the bug of COMPUTE statement (#143)
- Fix `EXIT PERFORM CYCLE` (#150)
- Fix SORT statement (#151)
- Fix functions `CURRENT-DATE` and `SYSTEM` (#162)
- Fix the bug of COMPUTE statement for `COMP-3` (#186)
- Fix non-NIST tests
  - Fix `i18n_sjis`and `data-rep` (#141)
  - Fix `syntax` (#142, #172)
  - Fix `jp-compat` (#141, #149, #161)
  - Fix `run` (#169, #172, #180)
### Optimized
- Optimize ADD statement and SUBTRACT statement (#133)
- Optimize comparison of `PIC S9(n)V(m)` (#143)
- Implement the precomputation for `CobolDataStorage` (#163)
### Changed
- Improve the variable names in generated Java files
- Update tests for NIST COBOL85 tests (#135)
- Refactor generated Java files (#125)
## [1.0.12] - 2023-06-30
### Added
- Run NIST85 tests for Relative files and add the test status to README.md
- Add CHANGELOG.md (#119)
### Fixed
- Fix overflows of USAGE COMP data (#105)
- Fix the bug related to overflows of USAGE COMP (Issue #105) #107
- Fix the bug of comparison process for national strings of different lengths (#113)
- Fix overflows of USAGE COMP-3 data (#118)
- Fix CANCEL statement (#121)
### Changed
- Refactor Java files in libcobj/ (#111)
- Improve the performance of File processing and comparision of 9(m)V9(n) (#114)
- Update README.md (#116)
- Improve the performance of converting Strings to byte arrays (#117)

## [1.0.11] - 2023-05-31
### Added
- Implement new options `-o`, `-j`, `-class-file-path`, `-java-file-path` (#99)
### Fixed
- Fix the command checking the results of NIST test and the read functoin of RELATIVE files (#98)
- Fix the method `realPutSign` of `AbstractCobolField` (#100)

## [1.0.10] - 2023-04-28
### Fixed
- Fix string literals containing mutiple bytes
- Fix some broken tests for special-names

## [1.0.9] - 2023-03-31
### Added
- Add `-constant` option and fix tests for dollarif
### Fixed
- Fix some build warnings
- Fix `Source file not found` test
- Fix `FUNCTION ABS`
- Fix warning messages involved with MOVE statements
- Fix the test for `EXIT PERFORM CYCLE`
- Fix warning messages involved with MOVE statements
### Changed
- Improve the representations of string literals
### Removed
- Remove `frameIndex`, frame` and comments in generated Java source code

## [1.0.8] - 2023-02-28
### Added
- Implement relative files (#97)
- Add `DESTDIR` support in libcobj/Makefile (#90)
- Add help messages to cobj and add comments to spring_batch_tasklet.sh
- Add static analysis for C source code
### Fixed
- Fix moveFrom of CobolNumericPackedField (#79)
- Fix some warning messages
- Use equals to compare Java Strings
- Fix CI and remove unnecessary files
### Changed
- Update .gitignore and tests/cobol85/Makefile.am (#79)
- Use BigDecimal.valueOf to create a new instance with double value (#85)
### Removed

## [1.0.7] - 2023-01-31
### Added
- Create cob_delete_file method (#68)
- Implement -java-package option (#70)
- Implement "SET ENVIRONMENT" statement (#72)
- Add a test for exchanging Japanese data between COBOL and Java (#73)
### Fixed
- Fix file-control.at
- Fix cmpInt of CobolNumericPackedField and add tests (#71)
- Fix EXIT PERFORM and EXIT PERFORM CYCLE (#75)
### Changed
- Improve Java interface (#69)
- Improve tests and CI (#67)

## [1.0.6] - 2022-12-29
### Added
-  Add code-format scripts and Update CI (#58)
- Implement the matrix build (#59)
- Implement -fshort-variable option (#61)
- Add PMD analysis (#63)
### Fixed
- Fix FUNCTION CURRENT-DATE (#56)
### Changed
- Update libcobj/Makefile to install libcobj.jar into /usr/lib/opensourcecobol4j (#57)

## [1.0.5] - 2022-11-22
### Added
- Add  options
- Add -B, -E, -g, -t, -C, -m options
- Add --listreserved, -fsyntax-only, -Wunreachable, -free and -free_1col_aster options
- Add the test case for NATIONAL items
### Fixed
- Fix warnings in cobj/*.c
- Fix codegen.c
- Fix segmentation faults when parsing exit-perform
- Fix exit perform and perform cycle
- Fix the help message of -m option
- Fix the bug of NATIONAL literals (#47)
- Fix the bug of NATIONAL items (#49)
- Fix moving sign-leading-separate to COMP3
### Changed
- Update README_JP.md
- Update CI
- Update default compile level
- Output raw multi-byte characters in generated Java source code
- Improve test cases for binary data
- Update NIST85 tests
### Removed
- remove libcob/ and po/ directory
- Remove options -c,-S, -O,-O2,-Os,-Q,-D

## [1.0.4] - 2022-07-20
### Added
- Embedding COBOL variable names into Java variable names (#39)
### Changed
- Add libcobj/compile.sh that builds java source code

## [1.0.3] - 2022-01-31
### Added
- implement INDEXED files with SQLite 

## [1.0.2] - 2021-11-10
### Fixed
- Fix compilation errors related to EXIT PERFORM statements
### Changed
- The license upgrades to GPL3.(#22)

## [1.0.1] - 2021-08-26
### Added
- Add the conversoin process for CALL statement

## [1.0.0] - 2021-08-02
- The first release
