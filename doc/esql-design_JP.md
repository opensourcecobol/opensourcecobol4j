# Embedded SQL (ESQL) 設計

## 概要

このドキュメントは Embedded SQL (EXEC SQL ... END-EXEC) のサポートが opensource COBOL 4J の内部でどのように構成されているかを示します。ユーザ向けの使い方は [esql-guide_JP.md](./esql-guide_JP.md) を参照してください。

ESQL サポートは大きく次の 2 層に分かれます。

1. **コンパイラ層 (`cobj/` 配下)**: EXEC SQL ブロックを字句・構文解析し、COBOL 通常の AST と同じ木構造に乗せたうえで、ホスト変数を `AbstractCobolField` 引数として渡す Java コードを生成する。
2. **ランタイム層 (`libcobj/.../sql/` 配下)**: 生成 Java から呼ばれて JDBC (PostgreSQL) を駆動し、結果を COBOL 領域に書き戻す。

## コンパイラ層

### 主要ファイル

| ファイル | 役割 |
|---|---|
| `cobj/pplex.l.m4` (前処理、`pplex.l` を生成) | `EXEC SQL INCLUDE <name>` を `COPY` に書き換え、`BEGIN/END DECLARE SECTION` を処理する。`INCLUDE SQLCA` の有無を `cb_sqlca_include_seen` に記録。その他の `EXEC SQL` テキストは本体スキャナへそのまま素通しする。 |
| `cobj/scanner.l.m4` (本体レキサ, `ESQL_STATE`) | `EXEC SQL` 〜 `END-EXEC` の本体を 1 つの文字列にまとめ、`EXEC_SQL_STATEMENT` トークン (英数字リテラル) として emit する。SQL テキストを単一の文字列として保存するのはここ。 |
| `cobj/parser.y` (`exec_sql_statement`) | `EXEC_SQL_STATEMENT` を受け取り `cb_parse_exec_sql()` で ESQL 専用スキャナ/パーサを起動する。最初の埋め込み SQL 検出時に `esql_inject_sqlca()` を呼ぶ。 |
| `cobj/esql-scanner.l` | flex レキサ。EXEC SQL ... END-EXEC で囲まれた SQL テキストをトークン化する。 |
| `cobj/esql-parser.y` | bison パーサ。トークン列からホスト変数情報を取り出して `cb_exec_sql` ノードを構築する。 |
| `cobj/esql-common.h` | `esql-parser.y` / `esql-scanner.l` と `esql.c` が共有する型と関数のヘッダ。tree.h を含めない (`YYSTYPE` の衝突を避けるため)。 |
| `cobj/esql.c` | `esql_build_and_resolve()`（ホスト変数の型解決、GROUP の子展開、SELECT INTO/FETCH OCCURS への昇格、`cb_tree` 構築のラッパ関数群）と `esql_inject_sqlca()`（最初の埋め込み SQL 文を検出した時点で `01 SQLCA GLOBAL.` を暗黙挿入）を提供する。 |
| `cobj/codegen.c` (`joutput_exec_sql` 周辺) | `cb_exec_sql` ノードを Java の `CobolEsql.exec(...)` 等の呼び出しに展開する。 |

### 解析パイプライン

```
EXEC SQL ... END-EXEC (COBOL ソース)
        │
        │  preproc (cobj/pplex.l.m4): EXEC SQL INCLUDE <name> を COPY に書き換え、
        │  BEGIN/END DECLARE SECTION を処理し、INCLUDE SQLCA の有無を
        │  cb_sqlca_include_seen に記録する。それ以外の EXEC SQL テキストは
        │  そのまま本体スキャナへ素通しする。
        ▼
本体スキャナ (cobj/scanner.l.m4 の ESQL_STATE)
        │
        │  EXEC SQL〜END-EXEC の本体を 1 つの文字列に蓄積し、END-EXEC で
        │  EXEC_SQL_STATEMENT トークン (英数字リテラル) として emit する。
        ▼
本体パーサ (cobj/parser.y の exec_sql_statement)
        │
        │  cb_parse_exec_sql(リテラル文字列) を呼んで以下の ESQL 専用
        │  スキャナ/パーサを起動する。最初の EXEC SQL 検出時に
        │  esql_inject_sqlca() を一度だけ実行する。
        ▼
esql-scanner.l (flex)
        │
        │  ホスト変数を '?' に置換しつつ SQL 本体を構築。
        │  ホスト変数や添字、修飾名は ESQL_HOSTTOKEN / HOSTSUB_* で
        │  パーサに渡す。
        ▼
esql-parser.y (bison)
        │
        │  cb_sql_host_var のリストとカーソル名、SQL 本体を組み立て、
        │  esql.c の esql_build_and_resolve() を呼ぶ。
        ▼
esql.c
        │
        │  - resolve_host_var_type(): COBOL の PIC / USAGE を見て
        │    HVARTYPE_* を決める。
        │  - expand_group_host_vars(): SELECT INTO 等で GROUP が
        │    渡されたら子フィールドの個別バインドに展開する。
        │  - OCCURS 検出: leaf に flag_occurs があれば
        │    SELECT_INTO_OCCURS / FETCH_OCCURS に昇格させる。
        │  cb_build_exec_sql() で cb_exec_sql ノードを返す。
        ▼
typeck.c / codegen.c
        │
        │  cb_exec_sql ノードを通常の COBOL 文と同じ経路で型検査・
        │  コード生成する。host_list / res_host_list の各 cb_reference
        │  は joutput_param 経由で b_X.getSubDataStorage(...) を含む
        │  AbstractCobolField 引数に変換される。
        ▼
Java ソース (CobolEsql.exec / .selectInto / .fetchCursor ...)
```

### ホスト変数の AST 表現

ESQL のホスト変数は専用のノード型を増やさず、COBOL 通常の参照 (`cb_reference`) にそのまま乗せます。

| 入力表記 | 内部表現 |
|---|---|
| `:VAR` | `cb_reference{ word: VAR }` |
| `:VAR(IDX)` | `cb_reference{ word: VAR, subs: [IDX] }` |
| `:VAR(I, J)` | `cb_reference{ word: VAR, subs: [J, I] }` (leaf→root の順) |
| `:GRP.SUB` | `cb_reference{ word: SUB, chain: cb_reference{ word: GRP } }` |
| `:GRP.SUB(IDX)` | 上記 + `subs: [IDX]` |
| `:GRP.SUB(GRP2.IDX)` | 上記 + `subs: [cb_reference{ word: IDX, chain: GRP2 }]` |

これにより `codegen.c` の `joutput_param` / `joutput_data` (`joutput_data` 内の OCCURS 解決ループ) が **ESQL 専用パスを増やさずに**正しい `b_X.getSubDataStorage(...)` を含む `AbstractCobolField` を生成します。

### スキャナの状態遷移

`esql-scanner.l` は次の排他状態を持ちます。

- `INITIAL`: EXEC SQL の最初のキーワード (`SELECT` / `INSERT` / `CONNECT` / ...) を待つ。
- `ESQL_STATE`: SQL 本体を読む。空白・コメント・トークンを `esql_sqlbody` に追記しつつ、ホスト変数だけ `?` に差し替える。
- `ESQL_HOSTSUB_STATE`: ホスト変数の直後に `(` が来たときに push される。`(`, `)`, `,`, 整数、修飾 IDENT を **SQL 本体に書き込まずに** トークンとして返す。`)` で pop する。
- `ESQL_DBNAME_STATE`: `AT :NAME` の `:NAME` を取りに行く専用状態。
- `WHERE_CURRENT_OF`: `WHERE CURRENT OF cursor` のカーソル名を捕まえる専用状態。

`HOSTWORD` の正規表現は dotted 修飾 (`("\."IDENT)*`) を含み、`HOSTWORD/"("` の trailing context で「`(` が直後に来るか」を見て `HOSTSUB_STATE` への分岐を決めています。

### サブチャネル `esql_pending_subs`

添字情報は文法アクション間で静的変数 `esql_pending_subs` (`cb_tree` 型) を経由して渡されます。`host_reference` を還元した直後に親規則の `esql_add_host_var()` が必ず消費・クリアするので、複数ホスト変数が交錯することはありません。union を拡張せず最小限の変更で済ませるための妥協です。

### 集団項目修飾の構築

`esql_build_qualified_ref()` (`cobj/esql.c`) は `"GRP.SUB.X"` のような dotted 名を `strtok` で分割し、右端を leaf として `cb_reference` を作り、`chain` を leaf → 親 → 祖父 と連結します。COBOL 通常の `X OF SUB OF GRP` と同じ AST 形になり、`cb_ref` の resolver も同じパスを通ります。

### OCCURS の扱い

SELECT INTO / FETCH では、`esql_build_and_resolve()` が leaf に `flag_occurs` を持つホスト変数を検出すると、コマンドを `CB_SQL_SELECT_INTO_OCCURS` / `CB_SQL_FETCH_OCCURS` に昇格させ、`occurs_size` (1 要素のバイト数) と `occurs_max` (要素数) を `cb_exec_sql` ノードに記録します。ランタイム側はこの 2 値を使って結果セットを配列の連続領域に書き込みます。

`expand_group_host_vars()` は SELECT INTO に GROUP がそのまま渡されたとき、子フィールドを個別のホスト変数に展開して並べる責任を持ちます (PostgreSQL 側からは N 列の SELECT として扱える)。

## ランタイム層

### パッケージ構成 (`jp.osscons.opensourcecobol.libcobj.sql`)

| クラス | 可視性 | 役割 |
|---|---|---|
| `CobolEsql` | `public` | 生成 Java から呼ばれる唯一の公開 API。`connect`, `disconnect`, `exec`, `execWithParams`, `execWhereCurrentOf`, `execWithParamsWhereCurrentOf`, `selectInto`, `selectIntoOccurs`, `declareCursor`, `declareCursorWithParams`, `openCursor`, `openCursorWithParams`, `fetchCursor`, `fetchCursorOccurs`, `closeCursor`, `prepare`, `executePrepared`, `commit`, `rollback` を提供。`execWhereCurrentOf` / `execWithParamsWhereCurrentOf` は `WHERE CURRENT OF` を含む文専用で、先読みで進んだカーソル位置を巻き戻してから実行する。 |
| `SqlState` | package-private | 接続テーブル (`addConnection`/`getConnection`)、PREPARE テーブル、カーソルテーブルを保持する内部状態管理。`clearCursors()` は全カーソルをクローズ扱いにし先読みバッファを破棄する（COMMIT / ROLLBACK 時に呼ばれる）。 |
| `SqlConnection` | package-private | JDBC `Connection` のラッパ。接続文字列 `dbname@host:port` のパース、値が空のときの環境変数 `OCDB_DB_NAME` / `OCDB_DB_USER` / `OCDB_DB_PASS` へのフォールバック、`OCDB_DB_CHAR`（既定 `UTF-8`）による接続エンコーディング設定、各値の末尾空白（COBOL の固定長フィールド由来のパディング）を除去する処理 (`stripTrailingSpaces`、値の途中の空白は保持する)、autocommit + トランザクションごとの明示 `BEGIN` (`beginTransaction`) を担う。 |
| `SqlCursor` | package-private | カーソルの状態 (open/closed)、`ResultSet`、`PreparedStatement` の組を保持する。先読み（バルクフェッチ）バッファと `overFetch` フラグも保持する。カーソルはインライン `SELECT` からでも、PREPARE 済みステートメント名からでも DECLARE できる。 |
| `BulkFetchConfig` | package-private | 環境変数 `OCESQL4J_FETCH_RECORDS` から先読み件数を読み取り、プロセス内でキャッシュする。 |
| `SqlCA` | package-private | SQLCA フィールド (`SQLCODE`, `SQLSTATE`, `SQLERRMC`, `SQLERRD`, ...) を `CobolDataStorage` に書き戻す。JDBC の `SQLState` を `sqlStateToCode` で ECPG コードにマッピングする。 |
| `CobolDataConverter` | package-private | `AbstractCobolField` ⇔ JDBC `PreparedStatement.setXxx` / `ResultSet.getXxx` の変換を担当。COBOL フィールド型で分岐する: 数値 (display)、パック 10 進 (COMP-3、符号付き/なし)、ネイティブバイナリ (COMP-5)、float/double、英数字 / group、national (`PIC N`)、英数字 / 日本語の `VARYING`（先頭 4 バイトのビッグエンディアン長ヘッダ + データ）。national と日本語の値は SHIFT-JIS で変換する。なお、ネイティブバイナリ (COMP-5) と float/double は送信方向 (COBOL→SQL、`cobolToString`) のみ実装されており、取得方向 (SQL→COBOL、`stringToCobol`) では専用の書き戻しを持たず英数字としてバイト列をコピーするフォールバックになる（正しいバイナリ値の書き戻しは未対応）。 |

`SqlConnection`, `SqlCursor`, `SqlState`, `SqlCA`, `CobolDataConverter` はすべて package-private なため、SLF4J のロガー名としては利用できますが、外部から直接 import することは想定していません。

### CobolEsql のシグネチャ

生成 Java から渡されるホスト変数は `AbstractCobolField[]` です。コンパイラ側で添字や修飾は解決済みのため、ランタイムは **添字や階層構造を意識せず**、配列の各要素を `CobolDataConverter` 経由で JDBC のパラメータ / 結果カラムに対応付けるだけです。

### SQLCA の自動定義

`parser.y` は、プログラム内で最初の実際の埋め込み SQL 文を検出した時点で `esql_inject_sqlca()`（`cobj/esql.c`）を呼び出し、明示的な `EXEC SQL INCLUDE SQLCA` がなくても `01 SQLCA GLOBAL.` を WORKING-STORAGE に自動挿入します。挿入は実際の `EXEC SQL` 文を含むプログラムに対してのみ行われ、`EXEC SQL INCLUDE SQLCA` や `BEGIN/END DECLARE SECTION` だけで実行可能な SQL を持たないプログラムには SQLCA を挿入しません。明示的な `EXEC SQL INCLUDE SQLCA END-EXEC` が記述されていない場合（前処理段（`pplex.l.m4`）で `cb_sqlca_include_seen` に記録される）、`cobj` はコンパイル時に警告を出力します。`SQLERRD` などは `OCCURS 6` を含む構造なので、`b_SQLERRD__SQLCA.getSubDataStorage(...)` を生成時に組み立てる対象になります。

### NULL 列の通知 (ECPG_MISSING_INDICATOR)

ECPG 互換として、ホスト変数側に指標変数が用意されていない状況で NULL 列を読み込んだ場合、ランタイムは `SQLCODE = -213` / `SQLSTATE = 22002`（`ECPG_MISSING_INDICATOR`）を返します。`SqlCA.java` では `ECPG_MISSING_INDICATOR = -213` で、`setMissingIndicator()` が状態を `22002` に設定します。COBOL フィールド自体には（ゼロ埋めで）値が書き込まれるため、行は処理済みとして扱われます。JUnit テスト `CobolEsqlTest`, `SqlCATest` に該当ケースが含まれます。

### バルクフェッチ（先読み）と WHERE CURRENT OF の位置補正

`SqlCursor.fetch` は、環境変数 `OCESQL4J_FETCH_RECORDS`（`BulkFetchConfig` がキャッシュ。既定 1）で指定した件数を 1 回の `FETCH FORWARD N FROM <cursor>` でまとめて取得し、`fetchBuffer` に保持します。以降の `fetchCursor` 呼び出しは、バッファを使い切るまで DB に問い合わせず 1 行ずつ供給し、使い切った時点で次の N 件を先読みします。これにより COBOL の N 回 FETCH に対する DB 往復を 1 回に集約します。既定値 1 のときは従来どおり 1 行ずつ取得します。COMMIT / ROLLBACK / CLOSE 時にはバッファをクリアします（`clearBuffer`）。

先読みはサーバカーソルを実際の現在行より先へ進めるため、`WHERE CURRENT OF` を使う位置付き UPDATE/DELETE では論理的な現在行とずれます。これを補正するため `SqlCursor` は先読みで進めすぎた状態を `overFetch` フラグで記録し、`CobolEsql.execWhereCurrentOf` / `execWithParamsWhereCurrentOf` は実行直前に `FETCH BACKWARD` でカーソル位置を巻き戻してから SQL を発行し、補正後は先読みバッファを無効化します（Open COBOL ESQL 4J の overFetch 補正と同じ挙動）。`codegen.c` の `joutput_exec_sql` は `WHERE CURRENT OF` を含む文をこれら専用 API へ振り分けます。

### トランザクションモデル

`SqlConnection.connect` は JDBC 接続を `setAutoCommit(true)` にしたうえで明示的に `BEGIN` を発行します。`COMMIT` / `ROLLBACK` のたび、および `DISCONNECT` 時には、`SqlState.clearCursors()`（サーバ側ポータルが消えるため全カーソルをクローズし先読みバッファを破棄する）と `SqlConnection.beginTransaction()` を呼び、次のトランザクションを開始します。これにより commit 間は常にトランザクションが有効な状態が保たれ、埋め込み文がトランザクションブロック内で実行される ECPG のセマンティクスに一致します。

### 文の失敗時のエラー処理

ランタイムはトランザクションを開いたまま（前述の「トランザクションモデル」参照）各埋め込み文を実行します。文が失敗すると、エラーを SQLCA（`SQLCODE` / `SQLSTATE` / `SQLERRMC`）に記録し、トランザクションは PostgreSQL の aborted 状態のままにします。ECPG / PostgreSQL と同様に、トランザクションが aborted になると、プログラムが `ROLLBACK`（または `COMMIT`）を発行するまで以降のすべての文が SQLSTATE `25P02`（`in_failed_sql_transaction`）で拒否されます。したがってエラーからの回復は COBOL プログラムの責任です。

パラメータ付き文では、JDBC の Describe（`getParameterMetaData`）を `execute()` と同じ `try` ブロック内で実行します。そのため Describe の失敗（例: テーブル不在）はそのままエラーハンドラへ伝播し、SQLCA には真のエラー（例: `42P01`）が記録されます。

### prepared statement のキャッシュ (`stmtCache`)

`CobolEsql` は `PreparedStatement` を、接続 (`Connection`) をキー、その配下に SQL 文字列をキーとする入れ子の `ConcurrentHashMap` (`stmtCache`) に保持します。接続はオブジェクト同一性、クエリは文字列の完全一致で照合するため、ハッシュ衝突で別クエリの文を取り違える心配はありません。`getOrCreatePreparedStatement` は、同一の `(接続, クエリ)` の組に対しては毎回 prepare し直さず、キャッシュ済みの `PreparedStatement` を再利用します。

### カーソル名の修飾

カーソル名は `<program-id>_<cursor>` の形でプログラム修飾されます。`codegen.c` はすべてのカーソル API 呼び出しを `"%s_%s"`（`excp_current_program_id`, `cursor_name`）の書式で出力するため、同名のカーソルを宣言する 2 つのプログラムが共有サーバ接続上で衝突しません。ランタイムはこの修飾名で `SqlState` にカーソルを登録・検索します。

`WHERE CURRENT OF` の場合、codegen はカーソル名を SQL 本体に**含めません**。`repositionForCurrentOf` がカーソル位置を巻き戻したあと実行時に補います（`query + " " + cursorName`）。これにより最終的な文が正しい修飾ポータルを対象とします。

### FETCH ... INTO OCCURS は先読みバッファを経由しない

`fetchCursorOccurs` は単一行用の先読みバッファを使いません。まず `clearBuffer()` を呼び（直前の単一行先読みが残したサーバカーソル位置とのずれを避ける）、`FETCH FORWARD <occursMax>` を直接発行して、行を OCCURS の連続領域に書き込み、取得件数を `SQLERRD(3)` に報告します。

### エラーマッピング

JDBC の `SQLException` 発生時、`SqlCA.setResultFromException` は例外の `SQLState` を `SqlCA.sqlStateToCode` で ECPG の `SQLCODE` にマッピングし、`e.getMessage()` を `SQLERRMC`（70 バイトに切り詰め）に格納します。主なマッピング: `02000` → `+100`（`ECPG_NOT_FOUND`）、`08001`/`08003` → `-402`（`ECPG_CONNECT`）、`34000` → `-602`、`YE002` → `-212`（`ECPG_EMPTY`）。認識できない状態はすべて `-9999`（`ECPG_UNKNOWN_ERROR`）になります。

カーソルの異常系は一部ランタイム側で判定されます: 未登録カーソルへの OPEN / FETCH / CLOSE は `-602` / `34000` を返し、登録済みだが未 OPEN のカーソルの CLOSE は成功を返し、未登録カーソルへの `WHERE CURRENT OF` は `ECPG_EMPTY` / `YE002` を返します。登録済みだが未 OPEN のカーソル（OPEN に失敗したものを含む）への FETCH はそのまま PostgreSQL へ送られ、トランザクションが正常なら `cursor "..." does not exist`、OPEN 失敗でトランザクションが abort のままなら `25P02` が返ります。回復する（`ROLLBACK` を発行する）かどうかは COBOL プログラムの責任です。

## テスト

### コンパイラ・統合テスト (`tests/`)

PostgreSQL コンテナを使う autotest スイートを以下のディレクトリで定義しています。

| ディレクトリ | 内容 |
|---|---|
| `tests/esql-basic.src/` | CONNECT / DISCONNECT / 基本 INSERT / SELECT |
| `tests/esql-cobol-data.src/` | COBOL データ型 × SQL 型のラウンドトリップ (数値・パック 10 進・英数字・日本語・VARYING・添字付き) |
| `tests/esql-sql-data.src/` | SQL 側の型バリエーション |
| `tests/esql-sqlca.src/` | SQLCA フィールドのアサーション |
| `tests/esql-misc.src/` | カーソル / PREPARE / EXECUTE 等 |
| `tests/esql-utf8.src/` | UTF-8 ビルド時の対応 |

各スイートは `make <name>` で生成され、`./<name>` でローカル実行できます (PostgreSQL コンテナが必要)。CI からは `.github/workflows/test-esql.yml` 経由で実行されます。

### libcobj 単体テスト (`libcobj/app/src/test/.../sql/`)

`CobolEsqlTest`, `SqlStateTest`, `SqlCursorTest`, `SqlConnectionTest`, `CobolDataConverterTest`, `CobolDataConverterPureTest`, `CobolEsqlLoggingTest`, `SqlCATest` が単体検証を行います。DB を要するものは testcontainers の PostgreSQL イメージを使い、`CobolDataConverterPureTest` のように DB を介さずデータ変換のみを検証するものもあります。

## 設計上の意思決定

- **ESQL 専用 AST を増やさない**: ホスト変数は `cb_reference` の `subs`/`chain` にそのまま乗せ、`codegen.c` の既存パスで処理する。これにより多次元添字・集団項目修飾・修飾名添字が `joutput_param` / `joutput_data` の OCCURS 解決ループに自然に乗る。
- **scanner 状態を分離する**: 添字や修飾名の解析を `ESQL_HOSTSUB_STATE` に閉じ込めることで、SQL 本体への文字列追記 (`esql_sqlbody_append`) を **その状態では絶対に行わない** という構造的保証を作っている。`?` 以外がプレースホルダ位置に混入する事故が起きない。
- **dblibj を捨てる**: Open-COBOL-ESQL の Scala 実装 (dblibj) への依存を撤廃し、純粋な Java で `libcobj.jar` 内に閉じる。配布物が `libcobj.jar` 1 本になり、PostgreSQL JDBC ドライバもバンドルする。
- **`OF` 修飾を採用しない**: COBOL 本体は `X OF Y` も受け付けるが、ESQL の修飾は dotted 形式 (`:Y.X`) のみに統一する。スキャナの状態数を抑えるためと、Embedded SQL の文脈で `OF` が予約語衝突を招きにくくするため。
- **生成 Java のホスト変数リストを折り返す**: `CobolEsql.*` 呼び出しに渡すホスト変数は、`codegen.c` の `joutput_sql_host_list_newline` (引数リスト) と `joutput_sql_field_array` (`new AbstractCobolField[]{...}` リテラル) が出力する。どちらも全要素を 1 行に並べるのではなく `SQL_HOST_VAR_WRAP` (= 5) 個ごとに改行を入れるため、ホスト変数が多い文でも生成ソースが読みやすく保たれる。これは見た目だけの調整で、渡す引数自体は変わらない。

## 関連ドキュメント

- [esql-guide_JP.md](./esql-guide_JP.md) -- ユーザ向け使い方（SLF4J ランタイムログの設定方法を含む）
- [esql-design.md](./esql-design.md) -- 本ドキュメントの英語版
