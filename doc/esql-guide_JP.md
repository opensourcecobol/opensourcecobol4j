# Embedded SQL (ESQL) ガイド -- opensource COBOL 4J

## 概要

opensource COBOL 4Jは、COBOLプログラムからPostgreSQLデータベースに直接アクセスするためのEmbedded SQL (EXEC SQL) をサポートしています。COBOL のホスト変数をSQLステートメントのバインドパラメータとして使用することで、COBOLとSQLの間でシームレスにデータをやり取りできます。

`EXEC SQL ... END-EXEC` ステートメントを含むCOBOLソースファイルを `cobj` でコンパイルすると、コンパイラはEmbedded SQLをJDBCを介したJavaデータベース呼び出しに変換します。

## 前提条件

- **opensource COBOL 4J** がインストール済みであること（[README_JP.md](../README_JP.md) を参照）
- **PostgreSQL** サーバー（バージョン9.6以降）

## クイックスタート

以下は、PostgreSQLに接続し、行を挿入して読み戻し、切断する最小限のCOBOLプログラムです。

```cobol
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 QUICK-START.
       DATA                        DIVISION.
       WORKING-STORAGE             SECTION.

       EXEC SQL INCLUDE SQLCA END-EXEC.

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  DBNAME    PIC X(30) VALUE "testdb@localhost:5432".
       01  USERNAME  PIC X(30) VALUE "main_user".
       01  PASSWD    PIC X(10) VALUE "password".
       01  EMP-NAME  PIC X(20).
       EXEC SQL END DECLARE SECTION END-EXEC.

       PROCEDURE                   DIVISION.
           EXEC SQL
               CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME
           END-EXEC.

           MOVE "Alice" TO EMP-NAME.
           EXEC SQL
               INSERT INTO employees (name) VALUES (:EMP-NAME)
           END-EXEC.

           MOVE SPACES TO EMP-NAME.
           EXEC SQL
               SELECT name INTO :EMP-NAME FROM employees LIMIT 1
           END-EXEC.
           DISPLAY "Employee: " EMP-NAME.

           EXEC SQL COMMIT END-EXEC.
           EXEC SQL DISCONNECT ALL END-EXEC.
           STOP RUN.
```

コンパイルと実行:

```bash
cobj quick-start.cbl
java QUICK-START
```

> [!NOTE]
> `EXEC SQL INCLUDE SQLCA END-EXEC` はコンパイラが内部的に処理するため、SQLCA 用に
> COPY ファイルを用意したり `-I` でコピーディレクトリを指定したりする必要はありません。
> （独自の COPY ブックを併用する場合のみ、通常どおり `-I` でそのディレクトリを指定してください。）

## サポートされるSQL文

### CONNECT / DISCONNECT

```cobol
       EXEC SQL
           CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME
       END-EXEC.

       EXEC SQL
           DISCONNECT ALL
       END-EXEC.
```

`:DBNAME` の接続文字列形式:

```
dbname@host:port
```

例: `"testdb@localhost:5432"`

ユーザ・パスワード・データベース名のホスト変数が空の場合、対応する値はそれぞれ環境変数
`OCDB_DB_USER`、`OCDB_DB_PASS`、`OCDB_DB_NAME` にフォールバックします（[環境変数](#環境変数)
を参照）。特に短縮形 `EXEC SQL CONNECT END-EXEC` はこれらの環境変数のみに依存します。

### BEGIN / END DECLARE SECTION

`EXEC SQL BEGIN DECLARE SECTION END-EXEC` と `EXEC SQL END DECLARE SECTION END-EXEC` は後方互換のために受理されますが、**無視されます**。WORKING-STORAGE SECTIONおよびLINKAGE SECTIONの全変数は、DECLARE SECTIONの有無にかかわらず、常にSQL文のホスト変数として使用できます。

```cobol
       WORKING-STORAGE SECTION.
       01  MY-VAR  PIC X(20).
       01  MY-NUM  PIC 9(5).
       PROCEDURE DIVISION.
           EXEC SQL
               INSERT INTO TBL VALUES (:MY-VAR, :MY-NUM)
           END-EXEC.
```

### ホスト変数の表記

ホスト変数として以下の形式を受け付けます。

| 表記 | 意味 |
|---|---|
| `:VAR` | 単純なホスト変数 |
| `:VAR(IDX)` | OCCURS 要素。`IDX` は整数リテラルまたは COBOL 変数。 |
| `:VAR(I, J)` | 多次元 OCCURS 要素 |
| `:GRP.SUB` | 集団項目で修飾されたホスト変数 (`GRP` 配下の `SUB`) |
| `:GRP.SUB(IDX)` | 集団修飾 + 添字 |
| `:GRP.SUB(GRP2.IDX)` | 添字値も集団修飾された COBOL 変数 |

添字に使う変数自身は添字を持てない (`:VAR(IDX(1))` のような入れ子添字は不可) という制約があります。間接的な添字が必要なら、COBOL 側でいったん作業変数に MOVE してから渡してください。

```cobol
       01  GRP.
         03 ROW OCCURS 5.
           05 VAL PIC 9(4).
       01  GRP2.
         03 TMP-IDX PIC S9(2).
       01  IDX PIC S9(2).
       ...
           MOVE 3 TO TMP-IDX OF GRP2.
           EXEC SQL
               SELECT FIELD INTO :GRP.VAL(GRP2.TMP-IDX)
                 FROM TESTTABLE WHERE N = :IDX
           END-EXEC.
```

### SQLCA（SQL通信領域）

SQLCAは、`EXEC SQL` 文を実行するプログラムで自動的に定義されます。各SQL文の実行後に `SQLCODE`、`SQLSTATE`、`SQLERRMC` などの診断フィールドが利用可能になります。

WORKING-STORAGE SECTIONに `EXEC SQL INCLUDE SQLCA END-EXEC.` を明示的に記述することを推奨します。埋め込みSQLを含むプログラムでこの宣言を省略した場合、`cobj` はSQLCAを暗黙に定義しますが、コンパイル時に以下の警告を出力します。

```
embedded SQL is used without 'EXEC SQL INCLUDE SQLCA END-EXEC'; SQLCA is declared implicitly
```

暗黙に定義されるSQLCAの構造は以下の通りです：

```cobol
       01  SQLCA GLOBAL.
           05  SQLCAID               PIC X(8).
           05  SQLCABC               PIC S9(9) COMP-5.
           05  SQLCODE               PIC S9(9) COMP-5.
           05  SQLERRM.
           49  SQLERRML              PIC S9(4) COMP-5.
           49  SQLERRMC              PIC X(70).
           05  SQLERRP               PIC X(8).
           05  SQLERRD OCCURS 6 TIMES
                                     PIC S9(9) COMP-5.
           05  SQLWARN.
               10 SQLWARN0           PIC X(1).
               10 SQLWARN1           PIC X(1).
               10 SQLWARN2           PIC X(1).
               10 SQLWARN3           PIC X(1).
               10 SQLWARN4           PIC X(1).
               10 SQLWARN5           PIC X(1).
               10 SQLWARN6           PIC X(1).
               10 SQLWARN7           PIC X(1).
           05  SQLSTATE              PIC X(5).
```

| フィールド | 説明 |
|-----------|------|
| `SQLCODE` | 戻りコード：0 = 成功、100 = データなし、負の値 = エラー |
| `SQLSTATE` | 5文字のSQL状態コード（例：`"00000"`, `"02000"`, `"08001"`） |
| `SQLERRMC` | エラーメッセージテキスト（最大70文字） |
| `SQLERRML` | `SQLERRMC` 内のエラーメッセージの長さ |
| `SQLERRD(3)` | 直前の文で影響を受けた行数 |

> [!NOTE]
> `CONNECT` 成功時、`SQLERRMC` は**上書きされません**。COBOL の初期値（70 桁の空白）が
> そのまま残り、`SQLERRML` は 0 に設定されます。これは Open COBOL ESQL 4J の挙動に合わせた
> ものです。`SQLERRMC` にメッセージが書き込まれるのはエラー時のみです。

### SELECT INTO

単一行の取得:

```cobol
       EXEC SQL
           SELECT emp_name, emp_salary
               INTO :EMP-NAME, :EMP-SALARY
               FROM employees
               WHERE emp_no = :EMP-NO
       END-EXEC.
```

OCCURS を使った配列取得（OCCURS ホスト変数への `SELECT ... INTO` は最大 `OCCURS` 件の
行を取得し、実際に格納された行数は `SQLERRD(3)` に報告されます）:

```cobol
       01  EMP-NAMES.
           05 EMP-NAME PIC X(20) OCCURS 10 TIMES.

       EXEC SQL
           SELECT emp_name INTO :EMP-NAME FROM employees
       END-EXEC.
```

### INSERT, UPDATE, DELETE

ホスト変数の前に `:` を付けてバインドパラメータとして使用します:

```cobol
       EXEC SQL
           INSERT INTO employees (emp_no, emp_name, emp_salary)
               VALUES (:EMP-NO, :EMP-NAME, :EMP-SALARY)
       END-EXEC.

       EXEC SQL
           UPDATE employees SET emp_salary = :NEW-SALARY
               WHERE emp_no = :EMP-NO
       END-EXEC.

       EXEC SQL
           DELETE FROM employees WHERE emp_no = :EMP-NO
       END-EXEC.
```

### DECLARE CURSOR / OPEN / FETCH / CLOSE

```cobol
       EXEC SQL
           DECLARE emp_cursor CURSOR FOR
               SELECT emp_no, emp_name FROM employees
               ORDER BY emp_no
       END-EXEC.

       EXEC SQL OPEN emp_cursor END-EXEC.

       PERFORM UNTIL SQLCODE NOT = ZERO
           EXEC SQL
               FETCH emp_cursor INTO :EMP-NO, :EMP-NAME
           END-EXEC
           IF SQLCODE = ZERO
               DISPLAY "No: " EMP-NO " Name: " EMP-NAME
           END-IF
       END-PERFORM.

       EXEC SQL CLOSE emp_cursor END-EXEC.
```

> [!NOTE]
> 単一行 FETCH は、環境変数 `OCESQL4J_FETCH_RECORDS`（後述）で指定した件数を 1 回の
> `FETCH FORWARD` でまとめて先読みし、以降の FETCH はバッファから 1 行ずつ供給します。
> これにより DB との往復回数を削減できます。既定値は 1 で、その場合は従来どおり
> 1 行ずつ取得します。`WHERE CURRENT OF` を使う位置付き UPDATE/DELETE では、先読みで
> 進んだカーソル位置を自動的に巻き戻してから実行するため、論理的な「現在行」が更新/削除されます。

#### OCCURS 配列への複数行 FETCH

`FETCH ... INTO` の対象が OCCURS ホスト変数の場合、複数行を一括取得します。ランタイムは
（単一行用の先読みバッファを経由せず）`FETCH FORWARD <occurs-max>` を直接発行し、最大
`OCCURS` 件の行を格納します。実際に取得した行数は `SQLERRD(3)` に報告されます。

```cobol
       01  EMP-NAMES.
           05 EMP-NAME PIC X(20) OCCURS 10 TIMES.

       EXEC SQL OPEN emp_cursor END-EXEC.
       EXEC SQL
           FETCH emp_cursor INTO :EMP-NAME
       END-EXEC.
       DISPLAY "取得行数: " SQLERRD(3).
```

#### カーソルのエラー挙動

| 状況 | 結果 |
|---|---|
| DECLARE していないカーソルへの OPEN / FETCH / CLOSE | `SQLCODE = -602`、`SQLSTATE = 34000` |
| DECLARE 済みだが未 OPEN のカーソルの CLOSE | 成功 (`SQLCODE = 0`) |
| DECLARE 済みだが未 OPEN のカーソルの FETCH | 文はそのまま PostgreSQL に送られ、`"cursor does not exist"` 由来の `SQLSTATE` とメッセージが SQLCA に格納される（独自の固定メッセージではない） |

### PREPARE / EXECUTE

```cobol
       01  SQL-STMT  PIC X(200).

       MOVE "INSERT INTO employees (emp_no, emp_name) VALUES (?, ?)"
           TO SQL-STMT.
       EXEC SQL
           PREPARE stmt1 FROM :SQL-STMT
       END-EXEC.

       EXEC SQL
           EXECUTE stmt1 USING :EMP-NO, :EMP-NAME
       END-EXEC.
```

### COMMIT / ROLLBACK

```cobol
       EXEC SQL COMMIT END-EXEC.
       EXEC SQL ROLLBACK END-EXEC.
```

## ホスト変数として使用できるCOBOL項目

以下のCOBOL項目をホスト変数として使用できます。ランタイムは項目の PIC 句・USAGE に従ってバイト列を解釈し、SQL とやり取りします。

| COBOL 項目 | 説明 |
|---|---|
| `PIC X(n)` | 英数字文字列 |
| `PIC A(n)` | 英字 |
| `PIC 9(n)` / `PIC S9(n)` | DISPLAY（ゾーン10進）数値。符号の有無・位置（LEADING/TRAILING、SEPARATE/結合）に対応 |
| `PIC 9(n)V9(m)` | 固定小数点数 |
| `USAGE COMP-3`（パック10進） | パック10進数 |
| `PIC N(n)` | 日本語、Shift-JIS |
| 集団項目 | 英数字として扱われる |
| VARYING 項目 | 可変長の英数字／日本語文字列 |

## SQLCAによるエラーハンドリング

各 `EXEC SQL` 文の実行後、SQLCAフィールドが更新されます:

| フィールド | 説明 |
|---|---|
| `SQLCODE` | 0 = 成功、+100 = レコードなし、負の値 = エラー |
| `SQLSTATE` | 5文字のSQL状態コード |
| `SQLERRMC` | エラーメッセージテキスト |
| `SQLERRD(3)` | 影響を受けた行数 |

主なSQLCODEの値（`SqlCA.java` で定義）:

| SQLCODE | SQLSTATE | 意味 |
|---|---|---|
| `+0` | `00000` | 成功 |
| `+100` | `02000` | 行なし / カーソルの終端 (`ECPG_NOT_FOUND`) |
| `-213` | `22002` | 指標変数なしでホスト変数に NULL を読み込んだ (`ECPG_MISSING_INDICATOR`) |
| `-220` | `08003` | 有効な接続がない (`ECPG_NO_CONN`) |
| `-402` | `08001` 等 | CONNECT 失敗 (`ECPG_CONNECT`) |
| `-602` | `34000` | カーソル（ポータル）が存在しない (`ECPG_WARNING_UNKNOWN_PORTAL`) |
| `-9999` | (サーバ依存) | 特定の ECPG コードに対応しない PostgreSQL エラー (`ECPG_UNKNOWN_ERROR`) |

エラーハンドリングの例:

```cobol
       IF SQLCODE NOT = ZERO
           DISPLAY "SQL Error: " SQLCODE
           DISPLAY "State: " SQLSTATE
           DISPLAY "Message: " SQLERRMC
           EXEC SQL ROLLBACK END-EXEC
           STOP RUN
       END-IF.
```

## 接続文字列の形式

CONNECTに渡す接続文字列は以下の形式です:

```
dbname@host:port
```

例:

```cobol
       MOVE "testdb@localhost:5432" TO DBNAME.
```

## 環境変数

| 変数名 | 説明 |
|---|---|
| `OCDB_DB_NAME` | デフォルトのデータベース名 |
| `OCDB_DB_USER` | デフォルトのデータベースユーザー |
| `OCDB_DB_PASS` | デフォルトのデータベースパスワード |
| `OCDB_DB_CHAR` | データベース接続の文字エンコーディング。未設定の場合は `UTF-8` が既定値として使用される。 |
| `OCESQL4J_FETCH_RECORDS` | カーソルの先読み（バルクフェッチ）件数。1 回の `FETCH FORWARD` でまとめて取得する行数を指定する。既定値は 1（1 行ずつ取得）。0 以下や数値でない値は 1 として扱う。プロセス起動時に一度だけ読み取られる。 |

`OCDB_DB_NAME`、`OCDB_DB_USER`、`OCDB_DB_PASS` は、対応する `CONNECT` のホスト変数が空のときの
フォールバックとして使用されます。短縮形 `EXEC SQL CONNECT END-EXEC` はこの仕組みで接続パラメータを取得します。

## コンパイル方法

```bash
# EXEC SQLを含むCOBOLプログラムをコンパイル
cobj program.cbl

# コンパイルしたプログラムを実行
java program
```

## ランタイムログ

ESQL ランタイムは [SLF4J](https://www.slf4j.org/) を使ってログを出力します。`EXEC SQL` 文を実行すると、
その種類に応じて以下のログが出力されます。実行中の SQL や接続状況の確認、トラブルシュートに利用できます。

### 構文ごとのログ出力

| ESQL 構文 | ERROR（失敗時） | DEBUG | TRACE |
|---|---|---|---|
| `CONNECT` | 接続失敗 | 接続成功（接続ID）、JDBC URL とユーザー名 | 接続パラメータのホスト変数値（ユーザー名・DB名） |
| `DISCONNECT` | 切断失敗 | 切断開始（接続ID） | - |
| `INSERT` / `UPDATE` / `DELETE` / DDL 等の `EXEC SQL` | SQL 実行失敗 | 実行する SQL 文（トリム済み） | - |
| パラメータ付きの `EXEC SQL`（`EXECUTE ... USING` など） | SQL 実行失敗 | SQL 文とパラメータ数 | - |
| `SELECT ... INTO` | SELECT INTO 実行失敗 | 実行する SQL 文 | - |
| `DECLARE ... CURSOR` | - | カーソル名と SQL 文 | - |
| `OPEN` カーソル | カーソルオープン失敗 | カーソル名 | - |
| `FETCH` | - | - | カーソル名 |

ポイント:

- **ERROR** … SQL 実行に失敗したとき、対象の SQL 文とエラーメッセージを記録します。
- **DEBUG** … 実行した SQL 文、接続の成立・切断、カーソル操作など、通常の動作経過を記録します。
- **TRACE** … `CONNECT` のホスト変数値や `FETCH` のように高頻度・詳細な情報を記録します（DEBUG とは分離）。
- `WHERE CURRENT OF` を伴う位置付き `UPDATE` / `DELETE` は専用ログを持ちませんが、内部でカーソル
  位置を補正したのち通常の `EXEC SQL` として実行されるため、上表の `EXEC SQL` のログが出力されます。

> [!NOTE]
> ログにはホスト変数の値（`CONNECT` のユーザー名・DB名など）やパラメータを含む SQL 文が
> 出力される場合があります。本番環境で TRACE/DEBUG を有効にする際は、機微な情報がログに
> 残る可能性に注意してください。

### ログ出力の設定

`libcobj.jar` には slf4j-simple が同梱されており、ログは標準エラー出力に出力されます。既定では
ESQL のロガーは **off** です。`libcobj.jar` には
`org.slf4j.simpleLogger.log.jp.osscons.opensourcecobol.libcobj.sql=off` を設定した
`simplelogger.properties` が同梱されているため、オプションを何も付けずに `java` コマンドで実行した
場合は ESQL 関連のログは一切出力されません。この設定はグローバルな `defaultLogLevel` ではなく ESQL の
ロガー名前空間に限定しているため、同梱ファイルが libcobj.jar を利用するアプリケーション側の他の
slf4j ロガーを抑制してしまうことはありません。ログを出力したい場合は、システムプロパティでこの
名前空間のレベルを上げます（`-D` で指定したプロパティは同梱の既定値より常に優先されます）。ESQL
ランタイムが出力するログは ERROR / DEBUG / TRACE の 3 種類で、設定値に応じて次の 3 パターンを
使い分けます。

ERROR のみ（失敗時のエラーだけを出力）:

```bash
java -Dorg.slf4j.simpleLogger.log.jp.osscons.opensourcecobol.libcobj.sql=error YourProgram
```

ERROR と DEBUG（実行する SQL 文・接続/切断・カーソル操作まで出力）:

```bash
java -Dorg.slf4j.simpleLogger.log.jp.osscons.opensourcecobol.libcobj.sql=debug YourProgram
```

ERROR と DEBUG と TRACE（`CONNECT` のホスト変数値や `FETCH` など、すべてを出力）:

```bash
java -Dorg.slf4j.simpleLogger.log.jp.osscons.opensourcecobol.libcobj.sql=trace YourProgram
```

> [!NOTE]
> `...libcobj.sql` 名前空間は `CobolEsql` ロガー（EXEC SQL の実行）と `SqlConnection` ロガー
> （接続/切断）の両方を含むため、上記の設定で ESQL のログがすべて有効になります。単一のロガーだけを
> 対象にしたい場合は、クラス名まで付与します
> （例: `-Dorg.slf4j.simpleLogger.log.jp.osscons.opensourcecobol.libcobj.sql.CobolEsql=debug`）。

## 制限事項

以下に挙げる制限事項は、いずれも移植元である Open COBOL ESQL 4J でも同様に制限されている事項です。本機能は Open COBOL ESQL 4J の挙動に合わせて移植しているため、これらの点についても元の実装と同じ制限を引き継いでいます。

- COBOL 標準の `OF` 修飾 (`:VAR OF GRP`) はサポートされません。dotted 修飾 (`:GRP.VAR`) を使用してください。
- 添字の値に算術式 (`:VAR(I+1)`) を書いたり、添字値そのものが添字を持つホスト変数 (`:VAR(IDX(1))`) を書いたりすることはできません。間接的な添字が必要な場合は、COBOL 側でいったん作業変数に MOVE してから渡してください。
- 対象データベースはPostgreSQLのみサポートされています。
- 以下の ECPG / 埋め込み SQL 機能は**サポートされていません**:
  - `EXECUTE IMMEDIATE`。
  - `WHENEVER`（宣言的な条件ハンドリング）。代わりに `SQLCODE` / `SQLSTATE` を明示的に確認してください。
  - 逆方向・スクロール FETCH（`FETCH PRIOR`、`FETCH BACKWARD`、スクロールカーソル等）。プログラムから使えるのは順方向 FETCH のみです。（`FETCH BACKWARD` は `WHERE CURRENT OF` のカーソル位置補正のため内部的にのみ使用されます。）
  - 複数接続。`AT db` 句は構文上受理されますが**無視**され、すべての文は単一のデフォルト接続に対して実行されます。`DISCONNECT ALL` もデフォルト接続のみに作用します。
  - 指標変数（`:VAR:IND`）。指標変数なしでホスト変数に NULL がフェッチされた場合は、代わりに `SQLCODE = -213`（`SQLSTATE 22002`）で通知されます。
- ユーザ名・パスワード・接続文字列の値は、末尾の空白（COBOL の固定長フィールド由来のパディング）が実行時に除去されます。値の途中の空白は保持されますが、末尾に意味のある空白を含む値は扱えません。

内部アーキテクチャ (どのように解析・コード生成されているか) については [esql-design_JP.md](./esql-design_JP.md) を参照してください。

## サンプル

[example/esql/](../example/esql/) ディレクトリに実行可能なサンプルがあります:

- **sample.cbl** -- 基本的なCONNECT、INSERT、SELECT、DISCONNECT
- **cursor.cbl** -- カーソルを使った行の反復処理
- **prepare.cbl** -- EXECUTE USINGによるプリペアドステートメント
- **INSERTTBL.cbl** -- EMP テーブルを作成し、リテラル値とホスト変数（OCCURS テーブル）で行を INSERT
- **FETCHTBL.cbl** -- SELECT COUNT(*) で件数を取得後、カーソルで EMP テーブルを FETCH して整形表示（INSERTTBL.cbl の後に実行）
