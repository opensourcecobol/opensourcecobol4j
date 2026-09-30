Execute make command to generate the following test scripts.

* cobj-idx
* cobol\_utf8
* command-line-options
* data-rep
* file-lock
* file-lock2
* i18n\_sjis
* i18n\_utf8
* jp-compat
* misc
* multithread
* run
* syntax

For example, execute ./command-line-options in order to run command-line-options test.

`multithread` runs generated programs from several threads of one JVM at the
same time. Its test programs are driven by the small Java harnesses in
`multithread.src/harness/`, which start the threads and synchronise them.
`esql-multithread` does the same for embedded SQL and needs a PostgreSQL server
like the other ESQL suites.
See [the multi-threading guide](../doc/multithreading.md) for the run-unit model
the suites check.
