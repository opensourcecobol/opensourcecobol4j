Thank you for your interest in contributing to opensource COBOL 4J.
A summary of how to contribute is below.

# Issues

Although any topics related to opensource COBOL 4J can be posted in [Issues](https://github.com/opensourcecobol/opensourcecobol4j/issues), please submit ones written in English or Japanese.

# Pull Requests

We will check pull requests that passed all CI checks running both tests and static code analysis.
The static analysis checks whether C and Java source files are formatted using [clang-format](https://clang.llvm.org/docs/ClangFormat.html) and [google-java-format](https://github.com/google/google-java-format) respectively, and whether [PMD](https://pmd.github.io/) finds no error and warning in Java source files.

The below sections describe how to setup and run static code analysis.

## Setup static analysis tools

Run `sudo apt install clang-format` in Ubuntu to install `clang-format`.

## Run static analysis

### check with clang-format and google-java-format

Run `./format` in the top directory of opensource COBOL 4J.
If you want to make sure all files are formatted, run `./check-format` in the top directory of opensource COBOL 4J.

### PMD

Run the following command in libcobj/ directory:
```
./gradlew pmdMain
```
