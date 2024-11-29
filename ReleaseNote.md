### Fixed
* Process Japanese identifiers in COBOL correctly.
  * The older versions convert some Japanese characters in COBOL identifiers to another characters in Java.
* Fix the conditions `PERFORM UNTIL`.
  * If COBOL source code contains divisions in conditions of `PERFORM UNTIL`, the older versions emit compile errors.
* Fix `SEARCH` statements with data specified `OCCURS` and `DEPENDING`.
  * The older versions emit compile errors when `SEARCH` statements with data specified `OCCURS` and `DEPENDING`
* Fix `ADD` statements and `SUBTRACT` statements.
  * In some case, the result of `SUBTRACT` was previously incorrect when the operands contais PIC S9(n) negative values.
  * In some case, the result of `ADD` was -0 instead of +0 previously.
* Fix the process of checking signs of PIC 9(n) containing spaces.
* Fix `INSPECT` statements.
  * The older versions change signs of some values accidentally because of the bug of `INSPECT` statements.