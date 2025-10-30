# configuration-params-JP
## 概要
opensource COBOL 4J のコンフィグファイルに記載されているパラメータについて説明する。

opensource COBOL 4J をインストールすると、デフォルトの設定ファイルが `/usr/local/share/opensource-cobol-4j-1.0.7/config/default.conf` に配置され、コンパイル時に参照されるようになっている。また、コンパイル時に `-conf` を使用することで参照するコンフィグファイルを変更することもできる。設定ファイルはリポジトリの `config` ディレクトリに格納されていて、デフォルトで参照される `default.conf` のほかに以下のものがある。これらは、それぞれファイル名に対応する方言を許容する設定ファイルである。
* `boundary-limit.conf`
* `bs2000.conf`
* `cobol85.conf`
* `cobol2002.conf`
* `default-en.conf`
* `default-jp.conf`
* `ibm.conf`
* `jp-compat.conf`
* `mf.conf`
* `mvs.conf`

これらの設定ファイルにあるほとんどのパラメータは `default.conf` のものと同じで値が異なる（`mvs.conf` に `not-reserved` というパラメータがあり、それは `default.conf` にはない）。パラメータ一覧の表にあるデフォルト値は `default.conf` のものである。

## 構文のサポートレベルについて
いくつかのパラメータは、値として `ok`, `archaic`, `obsolete`, `skip`, `ignore`, `unconformable`, `error` が選択できる。これにより、いくつかの構文をコンパイラがサポートするか、警告するか、拒否するかを選択することができる。

| レベル | 期待される表示（xxxは該当構文、〇〇〇はパラメータ一覧No.1のnameで設定した値が入る） |
| --- | --- |
| ok | 何も表示されない |
| archaic | 警告として xxx is archaic in 〇〇〇 が表示される|
| obsolete | 警告として xxx is obsolete in 〇〇〇 が表示される |
| skip | 何も表示されない |
| ignore | 警告として xxx ignored が表示される |
| unconformtable | エラーとして xxx does not conform to 〇〇〇 が表示される |
| error | 構文エラーが表示される |

## パラメータ一覧

| No | パラメータ名 | 設定できる値 | デフォルト値 | パラメータの説明 | 備考 |
|----|--------------|--------------|--------------|------------------|------|
| 1  | name | 任意の文字列 | OpenCOBOL | 設定ファイル名 | |
| 2  | tab-width | 整数 | 8 | タブ文字を何文字分のスペースとして扱うかを決める | 動作が安定しない |
| 3  | text-column | 整数 | 72 | コードを何桁目まで記述できるかを決める | |
| 4  | default-organization | `record-sequential`, `line-sequential` | `record-sequential` | `SELECT`で`ORGANIZATION`が定義されていない場合に、ここにセットされている値を`ORGANIZATION`として扱う | |
| 5  | assign-clause | `cobol2002`, `mf`, `ibm`, `jph1` | `mf` | `SELECT`文の`ASSIGN`の書き方で、IBMやMFの書き方が使えるようになる | 動作が安定しない |
| 6  | filename-mapping | `yes`, `no` | `yes` | ファイル名を環境変数から取得することができる | |
| 7  | pretty-display | `yes`, `no` | `yes` | `USAGE BINARY`で定義したデータ項目の表示を設定する | 未実装 |
| 8  | auto-initialize | `yes`, `no` | `yes` | `WORKING-STORAGE SECTION`で`VALUE`を使って初期化していなくても自動で初期値を付与する | |
| 9  | complex-odo | `yes`, `no` | `no` | yesの場合は、集団項目の中（先頭や末尾ではなく）に`OCCURS ~ DEPENDING ON`があってもコンパイルが通る | |
| 10 | indirect-redefines | `yes`, `no` | `no` | `REDEFINES`を多段階で定義できるようにする | |
| 11 | binary-size | `2-4-8`, `1-2-4-8`, `1--8` | `1-2-4-8` | `PIC 9(n) BINARY`の変数が確保するバイト数を設定する | |
| 12 | binary-truncate | `yes`, `no` | `yes` | オーバーフロー時の切り捨てが発生した場合に例外を発生させる | 動作が安定しない |
| 13 | binary-byteorder | `native`, `big-endian` | `big-endian` | バイトの並びを設定する | Javaではバイトの並びはデフォルトでビッグエンディアンとなっている |
| 14 | abort-on-io-exception | `any`, `fatal`, `never` | `any` | ファイル操作のエラーハンドリングのレベルを設定する。ハンドリングのレベルによって、出力されるJavaのコードが違う。 | |
| 15 | larger-redefines-ok | `yes`, `no` | `no` | `REDEFINES`で定義した集団項目のサイズが元の集団項目のサイズを上回っている場合でもコンパイルできるようにする | |
| 16 | relaxed-syntax-check | `yes`, `no` | `no` | 以下のMFの書き方を許容できるようにする<br>・`INPUT-OUTPUT SECTION`という記述がなくてもよい<br>・`FILE SECTION`という記述がなくてもよい<br>・`REDEFINES`を書くときに、`PIC X(n) REDEFINES`のような書き方を許容する | |
| 17 | perform-osvs | `yes`, `no` | `no` | IBM OS/VS COBOLの方言を使えるようにする（PERFORMの扱い方が違う） | 未実装 |
| 18 | sticky-linkage | `yes`, `no` | `no` | `LINKAGE SECTION`にあるデータが`CALL`の呼び出しごとに初期化されずに保持されるようになる | 未実装 |
| 19 | assign_external | `yes`, `no` | `no` | 対象のCOBOLプログラムの`SELECT`文のすべての`ASSIGN`句に`EXTERNAL`が指定されているものとして扱うことができる | コンパイルオプションの`-assign_external`と同じ |
| 20 | relax-level-hierarchy | `yes`, `no` | `no` | データ項目の階層の数字の大小が不揃いでもコンパイルが通るようにする | |
| 21 | author-paragraph | `ok`, `archaic`, `obsolete`, `skip`, `ignore`, `unconformable`, `error` | `obsolete` | `AUTHOR`パラグラフを有効にするか決める |  |
| 22 | memory-size-clause | `ok`, `archaic`, `obsolete`, `skip`, `ignore`, `unconformable`, `error` | `obsolete` | `OBJECT-COMPUTER`節の`MEMORY SIZE`指定を有効にするか決める |  |
| 23 | multiple-file-tape-clause | `ok`, `archaic`, `obsolete`, `skip`, `ignore`, `unconformable`, `error` | `obsolete` | `I-O CONTROL`の`MULTIPLE FILE TAPE`指定を有効にするか決める |  |
| 24 | label-records-clause | `ok`, `archaic`, `obsolete`, `skip`, `ignore`, `unconformable`, `error` | `obsolete` | `FILE CONTROL`の`FD`句で`LABEL RECORDS`の指定を有効にするか決める |  |
| 25 | value-of-clause | `ok`, `archaic`, `obsolete`, `skip`, `ignore`, `unconformable`, `error` | `obsolete` | `FILE CONTROL`の`FD`句で`VALUE OF {WORD}`を有効にするか決める |  |
| 26 | data-records-clause | `ok`, `archaic`, `obsolete`, `skip`, `ignore`, `unconformable`, `error` | `obsolete` | `FILE CONTROL`の`FD`句で`DATA RECORD`を有効にするか決める |  |
| 27 | top-level-occurs-clause | `ok`, `archaic`, `obsolete`, `skip`, `ignore`, `unconformable`, `error` | `skip` | 01および77のデータ項目に`OCCURS`を使えるか決める |  |
| 28 | synchronized-clause | `ok`, `archaic`, `obsolete`, `skip`, `ignore`, `unconformable`, `error` | `ok` | データ定義において`SYNCHRONIZED`を有効にするか決める |  |
| 29 | goto-statement-without-name | `ok`, `archaic`, `obsolete`, `skip`, `ignore`, `unconformable`, `error` | `obsolete` | `GO TO`のあとに何も指定しない書き方を有効にするか決める |  |
| 30 | stop-literal-statement | `ok`, `archaic`, `obsolete`, `skip`, `ignore`, `unconformable`, `error` | `obsolete` | `STOP LITERAL`を有効にするかを設定する | 未実装 |
| 31 | debugging-line | `ok`, `archaic`, `obsolete`, `skip`, `ignore`, `unconformable`, `error` | `obsolete` | `CONFIGURATION SECTION`の`SOURCE-COMPUTER`の指定で、`WITH DEBUGGING MODE`の記述を有効にするか決める |  |
| 32 | padding-character-clause | `ok`, `archaic`, `obsolete`, `skip`, `ignore`, `unconformable`, `error` | `obsolete` | `SELECT`文で`PADDING CHARACTER`の記述を有効にするか決める |  |
| 33 | next-sentence-phrase | `ok`, `archaic`, `obsolete`, `skip`, `ignore`, `unconformable`, `error` | `archaic` | `NEXT SENTENCE`を使えるかを決める |  |
| 34 | eject-statement | `ok`, `archaic`, `obsolete`, `skip`, `ignore`, `unconformable`, `error` | `skip` | `EJECT`を使えるか決める | 未実装 |
| 35 | entry-statement | `ok`, `archaic`, `obsolete`, `skip`, `ignore`, `unconformable`, `error` | `obsolete` | `ENTRY`を使えるか決める | 未実装 |
| 36 | move-noninteger-to-alphanumeric | 同上 | `error` | 小数などの非整数を文字列型に`MOVE`できるようにする | 値がokの場合は警告が出て、errorの場合は出なかった |
| 37 | odo-without-to | `ok`, `archaic`, `obsolete`, `skip`, `ignore`, `unconformable`, `error` | `ok` | `OCCURS ~ DEPENDING ON`で`TO`を使って配列のサイズの最大値を定義していないときにエラーを出すようにする |  |
| 38 | default-currency-symbol | 任意の1文字 | `$` | デフォルトの通貨記号（`$`、`\`など）を設定できる | |
| 39 | max-alpha-character-data-size | 整数 | 2147483647 | `PIC X`の最大サイズを設定する | |
| 40 | max-sjis-character-data-size | 整数 | 1073741823 | `PIC N`の最大サイズを設定する | |
| 41 | max-utf8-character-data-size | 整数 | 715827882 | `PIC N`の最大サイズを設定する |  |
| 42 | c89-identifier-length-check | `yes`, `no` | `no` | `PROGRAM-ID`が31文字より多い場合に警告を出す | 動作が安定しない |
| 43 | allow-end-program-with-wrong-name | `yes`, `no` | `no` | `PROGRAM-ID`に書いてある単語と`END PROGRAM`に書いてある単語が異なるような記述を許容する | |
| 44 | allow-missing-also-clause-in-evaluate | `yes`, `no` | `no` | `EVALUATE`の条件式で`ALSO`が省略する記述を許容する | |
| 45 | allow-empty-imperative-statement | `yes`, `no` | `no` | `IF`や`EVALUATE`で空の無条件文を許容する | |
| 46 | enable-program-status-register | `yes`, `no` | `no` | `PROGRAM-STATUS`という特殊レジスタを使用できるようにする | |
| 47 | enable-sort-status-register | `yes`, `no` | `no` | `SORT-STATUS`という特殊レジスタを使用できるようにする | |
| 48 | enable-special-names-argument-clause | `yes`, `no` | `no` | `SPECIAL-NAMES`で`ARGUMENT-NUMBER`と`ARGUMENT-VALUE`を使えるようにする | |
| 49 | enable-special-names-environment-clause | `yes`, `no` | `no` | `SPECIAL-NAMES`で`ENVIRONMENT-NAME`と`ENVIRONMENT-VALUE`を使えるようにする | |
| 50 | enable-leng-intrinsic-function | `yes`, `no` | `no` | 組み込み関数`FUNCTION LENG`を使えるようにする | |
| 51 | enable-length-an-intrinsic-function | `yes`, `no` | `no` | 組み込み関数`FUNCTION LENGTH-AN`を使えるようにする | |
| 52 | enable-national-intrinsic-function | `yes`, `no` | `no` | 組み込み関数`FUNCTION NATIONAL`を使えるようにする | |
| 53 | use-invalidkey-handler-on-status34 | `yes`, `no` | `no` | SAMファイルの`READ/WRITE`で`INVALID KEY`の指定を可能にする | |
| 54 | cobol68-copy-in-data-description | `yes`, `no` | `no` | `DATA DIVISION`において、COBOL68スタイルの`COPY`を使えるか決める | [参考ページ](https://www.microfocus.com/documentation/visual-cobol/vc50all/DevHub/HRLHLHEXAM06.html) |
| 55 | switch-no-mnemonic | `yes`, `no` | `no` | `SPECIAL-NAMES`で設定するスイッチ名を別名がなくても参照可能にする | |
| 56 | allow-is-in-sort-key-spec | `yes`, `no` | `no` |`SORT`の`KEY`指定で`KEY IS`と書けるようにする | |
| 57 | allow-search-key-in-rhs | `yes`, `no` | `no` | `SEARCH ALL`文の検索条件の右辺にキー項目を記述できるようにする | |
| 58 | ignore-invalid-record-contains | `yes`, `no` | `no` | `FD`句の`RECORD CONTAINS`によるレコードのバイト数の制限を無視することができる | |
| 59 | enable-zero-division-error | `yes`, `no` | `no` | プログラムにゼロ除算があったときにコンパイルの段階でエラーを出す | |
| 60 | enable-check-subscript-out-of-bounds | `yes`, `no` | `no` | `OCCURS ~ DEPENDING ON`で定義した要素数以上の添え字にアクセスするコードがあった場合にコンパイルエラーを出す | |
| 61 | enable-expect-numeric-error | `yes`, `no` | `no` | 文字列型から数値型へのMOVEがあった場合、コンパイルエラーを出す | |
| 62 | enable-expect-compute-string-error | `yes`, `no` | `no` | 文字列と数値を計算しようとしたときにエラーを出す | 動作が安定しない |
| 63 | not-reserved | 任意の文字列 | `CYCLE`, `NORMAL` | 指定した文字列を予約語から一時的に除外する | mvs.confにあるパラメータでdefault.confにはない |