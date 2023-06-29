# opensource COBOL 4J

[日本語版README](./README_JP.md)

"opensource COBOL 4J" is a COBOL compiler that translates COBOL programs to Java programs.
This compiler is deeply inspired by ["opensource COBOL"](https://github.com/opensourcecobol/opensource-cobol) which translates COBOL programs to C programs.
(In fact, this repository contains a lot of source files of ["opensource COBOL"](https://github.com/opensourcecobol/opensource-cobol))
[CHANGELOG.md](./CHANGELOG.md) contains all notable changes.

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


Implemented.

* Data calculation (MOVE, COMPUTE, ... )
* Control statements (IF, PERFORM, GO TO, ...)
* Some I/O functions (DISPLAY, ACCEPT, ...)
* Call statements
* Sequential files
* Indexed files
* Relative files
* Sort statements
* Embedded functions (ACOS, LENGTH, MAX, ...)

## The status of NIST85 test

opensource COBOL 4J is tested using [NIST COBOL85 test suite](https://www.itl.nist.gov/div897/ctg/cobol_form.htm)

The result of NIST COBOL85 test suite

```
------ Directory Information -------   --- Total Tests Information ---
Module Programs Executed Error Crash   Pass Fail Deleted Inspect Total
------ -------- -------- ----- -----  ----- ---- ------- ------- -----
NC           90       90     0     0   4352    0       6      11  4369
SM           15       15     0     0    290    0       3       1   294
IC           13       13     0     0     97    0       0       0    97
SQ           81       81     0     0    512    0       6      81   599
IX           39       39     0     0    507    0       1       0   508
ST           39       39     0     0    278    0       0       0   278
SG            5        5     0     0    193    0       0       0   193
OB            5        5     0     0     16    0       0       0    16
IF           42       42     0     0    732    0       0       0   732
RL           32       32     0     0   1827    0       5       0  1832
------ -------- -------- ----- -----  ----- ---- ------- ------- -----
Total       361      361     0     0   8804    0      21      93  8918
```

# Contributors

See https://github.com/opensourcecobol/opensourcecobol4j/graphs/contributors
