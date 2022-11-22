# opensource COBOL 4J

[日本語版README](https://github.com/opensourcecobol/opensourcecobol4j/blob/main/README_JP.md)

"opensource COBOL 4J" is a COBOL compiler that translates COBOL parograms to Java programs.
This compiler is deeply inspired by ["opensource COBOL"](https://github.com/opensourcecobol/opensource-cobol) which translates COBOL programs to C programs.
(In fact, this repository contains a lot of source files of ["opensource COBOL"](https://github.com/opensourcecobol/opensource-cobol))

## LICENSE

libcobj, the runtime libraries, are distributed under the the GNU Lesser General Public License Version 3.
Other software and libraries are distributed under the GNU GENERAL PUBLIC LICENSE Version 3.

## Requirements and Installation

See [Installation page](https://github.com/opensourcecobol/opensourcecobol4j/wiki/Installation).

## Usage

Compile.
```bash
cobj [COBOL source file]
```
("cobj" command produces [PROGRAM-ID].java and [PROGRAM-ID].class in the current directory.)

Run.
```bash
java [PROGRAM-ID]
```

## The progress of the development

The functions in the following "implemented" list are tested using [NIST COBOL85 test suite](https://www.itl.nist.gov/div897/ctg/cobol_form.htm)
and **99%** of the test cases are passed.

Implemented.

* Data calculation (MOVE, COMPUTE, ... )
* Control statements (IF, PERFORM, GO TO, ...)
* Some I/O functions (DISPLAY, ACCEPT, ...)
* Call statements
* Sequential files
* Indexed files
* Sort statements
* Embedded functions (ACOS, LENGTH, MAX, ...)

Not Implemented.

* Relative files

Known bugs

* Cannot call inline programs.
