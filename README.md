# opensource COBOL 4J

[日本語版README](./README_JP.md)

"opensource COBOL 4J" is a COBOL compiler that translates COBOL programs to Java programs.
This compiler is deeply inspired by ["opensource COBOL"](https://github.com/opensourcecobol/opensource-cobol) which translates COBOL programs to C programs.
(In fact, this repository contains a lot of source files of ["opensource COBOL"](https://github.com/opensourcecobol/opensource-cobol))

[CHANGELOG.md](./CHANGELOG.md) contains all notable changes.

## LICENSE

libcobj, the runtime libraries, are distributed under the the GNU Lesser General Public License Version 3.
Other software and libraries are distributed under the GNU GENERAL PUBLIC LICENSE Version 3.

## Requirements

opensource COBOL 4J is tested with the following platforms and dependencies

* Ubuntu 22.04 and AlmaLinux 9
* OpenJDK 11

In order to check requirements of older versions,
see [doc/requirements-all.md](./requirements-all.md).

## Installation

### Install dependencies

```
sudo apt-get update
sudo apt-get install -y default-jdk build-essential bison flex gettext texinfo libgmp-dev autoconf
```

### Install opensource COBOL 4J
```
curl -L -o opensourcecobol4j-v1.0.17.tar.gz https://github.com/opensourcecobol/opensourcecobol4j/archive/refs/tags/v1.0.17.tar.gz
tar zxvf opensourcecobol4j-v1.0.17.tar.gz
cd opensourcecobol4j-1.0.17
./configure --prefix=/usr/
make
sudo make install
```

### Set $CLASSPATH
Add /usr/lib/opensourcecobol4j/libcobj.jar to the environment variable $CLASSPATH.

In order to check installations of older versions,
[doc/installation/](./installation) directory respectively

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

The result of NIST COBOL85 main test suites

```
------ Directory Information -------   --- Total Tests Information ---
Module Programs Executed Error Crash   Pass Fail Deleted Inspect Total
------ -------- -------- ----- -----  ----- ---- ------- ------- -----
NC           90       90     0     0   4352    0       6      11  4369   COBOL nucleus tests
SM           15       15     0     0    290    0       3       1   294   COPY sentence tests
IC           13       13     0     0     97    0       0       0    97   CALL sentence tests
SQ           81       81     0     0    512    0       6      81   599   Sequential file I-O tests
IX           39       39     0     0    507    0       1       0   508   Indexed file I-O tests
ST           39       39     0     0    278    0       0       0   278   SORT sentence tests
SG            5        5     0     0    193    0       0       0   193   Segment tests
OB            5        5     0     0     16    0       0       0    16   Obsolete facilities tests
IF           42       42     0     0    732    0       0       0   732   Intrinsic Function tests
RL           32       32     0     0   1827    0       5       0  1832   Relative file I-O tests
------ -------- -------- ----- -----  ----- ---- ------- ------- -----
Total       361      361     0     0   8804    0      21      93  8918
```

The result of nist cobol85 extra test suites

```
------ directory information -------   --- total tests information ---
module programs executed error crash   pass fail deleted inspect total
------ -------- -------- ----- -----  ----- ---- ------- ------- -----
cm            7        0     7     0      0    0       0       0     0   COMMUNICATION SECTION tests
db           10        0    10     0      0    0       0       0     0   Debugging facilities tests
rw            4        0     4     0      0    0       0       0     0   REPORT SECTION tests
------ -------- -------- ----- -----  ----- ---- ------- ------- -----
total        21        0    21     0      0    0       0       0     0
```

# Contributing

Guidelines for contributing to opensource COBOL 4J can be found in [CONTRIBUTING.md](./CONTRIBUTING.md).
Contributors are listed in https://github.com/opensourcecobol/opensourcecobol4j/graphs/contributors
