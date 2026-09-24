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
* run
* syntax

For example, execute ./command-line-options in order to run command-line-options test.

## ESQL tests

The ESQL suites -- `esql-basic`, `esql-cobol-data`, `esql-misc`, `esql-sql-data`,
`esql-sqlca` and `esql-utf8` -- additionally need a PostgreSQL server.
`embed_db_info.sh` in this directory is what tells the test programs where to
find it. The copy checked in here points at `localhost:5432`, which is right for
a PostgreSQL running on the same machine; edit it if yours is somewhere else.

`.github/workflows/db-settings/` holds the copies CI uses instead.
`embed_db_info_postgresql_15.sh` and `embed_db_info_postgresql_9.6.sh` name
GitHub Actions service containers, so they resolve nowhere but CI and are not
worth copying locally. `embed_db_info_windows.sh` is the one for the cluster
`win/start-test-postgresql.ps1` starts, and that script also reads its own
defaults out of it, so the two cannot disagree about the port.

On Windows, `win/start-test-postgresql.ps1` brings up a cluster to test against.
It initialises one on first use and reuses it afterwards, and `-Stop` shuts it
down. From this directory, in Git Bash:

```
powershell -File ../win/start-test-postgresql.ps1
cp ../.github/workflows/db-settings/embed_db_info_windows.sh embed_db_info.sh
./esql-basic
```

That `cp` overwrites a tracked file; `git checkout embed_db_info.sh` puts the
default back.

`esql-utf8` needs a compiler configured with `--enable-utf8`, so it does not run
against the Shift_JIS Windows build.
