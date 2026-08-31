## New Features

- **Abstracted classes to support multiple databases in embedded SQL** (#857)
  - Consolidated shared logic and abstracted DB-specific parts so that the embedded SQL functionality can support databases other than PostgreSQL.
  - opensource COBOL 4J provides an implementation for PostgreSQL. Support for other databases can now be added easily by creating a class that extends `AbstractCobolEsqlBackend`. The database to use is specified via the `OCDB_DB_TYPE` environment variable (PostgreSQL is used if unspecified).
