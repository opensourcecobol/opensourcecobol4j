---
paths:
  - "tests/esql-*"
  - "tests/esql-*.src/**"
  - "tests/embed_db_info.sh"
  - "cobj/esql*"
---

# ESQL テストをローカルで実行する手順

ESQLのautotestスイート（esql-basic等）はPostgreSQLが要るため通常はCI任せだが、PostgreSQLが入っている環境なら
sudoなしで私設クラスタを起動してローカル実行できる。バージョンやバイナリのパス、DBユーザ名は環境に合わせて読み替えること。

```bash
PGBIN=$(dirname "$(command -v initdb || echo /usr/lib/postgresql/16/bin/initdb)")
PGDIR=<作業用ディレクトリ>/pgdata
PGSOCK=/tmp/pgsock        # Unix socketのパスには107バイト制限がある。PGDIR配下に置くと超えることがある
PGPORT=55432              # 既定の5432が塞がっていることがあるので空きポートを使う

"$PGBIN/initdb" -D "$PGDIR" -U "$USER" --auth=trust -E UTF8
mkdir -p "$PGSOCK"
"$PGBIN/pg_ctl" -D "$PGDIR" -o "-p $PGPORT -k $PGSOCK" -l "$PGDIR/log" start
"$PGBIN/createdb" -h 127.0.0.1 -p "$PGPORT" -U "$USER" testdb
```

- 既定と違うポートを使う場合は`tests/embed_db_info.sh`の`DB_PORT`を一時的に書き換える（**コミット前に必ず戻すこと**）。
- `esql-utf8`だけは`./configure --enable-utf8`でビルドし直す必要がある。
- **環境によっては必ず落ちるテストがある**: `esql-sqlca.src/connect-disconnect.at`は存在しないホスト`invalid`への接続でSQLSTATE 08001を期待しているが、
  名前解決が常に応答する環境（DNSのワイルドカード応答、一部のWSL環境など）では28P01が返って失敗する。環境差でありコードの問題ではない。
