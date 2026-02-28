# 環境変数リファレンス

opensource COBOL 4J で使用される環境変数について説明します。

## コンパイラ設定

準備中

## ランタイム設定

### 一般設定

#### COB_DATE

実行時の日付を固定するための環境変数です。

- **形式**: `YYYY/MM/DD`
- **例**: `COB_DATE=2024/01/15`
- **用途**: `CURRENT-DATE` などの組み込み関数が返す日付を固定できます。テストや再現性のある実行に便利です。

**サンプルプログラム**

`ACCEPT FROM DATE YYYYMMDD` と `FUNCTION CURRENT-DATE` の両方で取得した日付を表示するプログラムです。`COB_DATE` を設定すると、どちらの方法で取得した日付も固定値になります。

```cobol
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      cobdate.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  DATE-YMD.
         03  DATE-YYYY PIC X(4).
         03  DATE-MM   PIC X(2).
         03  DATE-DD   PIC X(2).
       01  CURR-DATE.
         03  CURR-YYYY PIC X(4).
         03  CURR-MM   PIC X(2).
         03  CURR-DD   PIC X(2).
         03  FILLER    PIC X(13).
       PROCEDURE        DIVISION.
           ACCEPT DATE-YMD FROM DATE YYYYMMDD.
           DISPLAY "ACCEPT FROM DATE: " DATE-YMD.
           MOVE FUNCTION CURRENT-DATE TO CURR-DATE.
           DISPLAY "CURRENT-DATE:     "
               CURR-YYYY CURR-MM CURR-DD.
           STOP RUN.
```

実行例:

```bash
$ cobj cobdate.cbl

# 環境変数なし（今日の日付が表示される）
$ java cobdate
ACCEPT FROM DATE: 20260227
CURRENT-DATE:     20260227

# COB_DATE を設定（日付が固定される）
$ COB_DATE=1970/01/02 java cobdate
ACCEPT FROM DATE: 19700102
CURRENT-DATE:     19700102
```

#### COB_VERBOSE

詳細なデバッグ情報を出力するかどうかを制御します。

- **値**: `Y` または `y` で有効化
- **例**: `COB_VERBOSE=Y`
- **用途**: ランタイムの動作を詳細に確認したい場合に使用します。

**サンプルプログラム**

SORT処理を行うプログラムです。`COB_VERBOSE=Y` を設定すると、SORT完了時にレコード数などの情報が標準出力に表示されます。

```cobol
       IDENTIFICATION       DIVISION.
       PROGRAM-ID.          cobverbose.
       ENVIRONMENT          DIVISION.
       INPUT-OUTPUT         SECTION.
       FILE-CONTROL.
           SELECT SORT-FILE  ASSIGN TO "S01"
               ORGANIZATION SEQUENTIAL.
           SELECT INPUT-FILE  ASSIGN TO "./sort-input.txt".
           SELECT OUTPUT-FILE ASSIGN TO "./sort-output.txt".
       DATA                 DIVISION.
       FILE                 SECTION.
       SD SORT-FILE.
       01 SORT-REC.
          02 SORT-KEY  PIC XX.
          02 SORT-DATA PIC XX.
       FD INPUT-FILE.
       01 INPUT-REC.
          02 XFLD1   PIC X(2).
          02 XFLD2   PIC X(2).
       FD OUTPUT-FILE.
       01 OUTPUT-REC.
          02 XFLD1   PIC X(2).
          02 XFLD2   PIC X(2).
       PROCEDURE            DIVISION.
          SORT SORT-FILE ON ASCENDING KEY SORT-KEY
             USING INPUT-FILE
             GIVING OUTPUT-FILE.
          STOP RUN.
```

実行例:

```bash
$ cobj cobverbose.cbl

# 入力ファイルを作成（3レコード x 4バイト: キー2バイト + データ2バイト）
$ printf "11CC33AA22BB" > sort-input.txt

# 環境変数なし（メッセージなし）
$ java cobverbose

# COB_VERBOSE=Y を設定（SORT完了メッセージが表示される）
$ COB_VERBOSE=Y java cobverbose
libcobj: END OF SORT/MERGE, RECORD=3.
```

#### COB_TERMINAL_ENCODING

DISPLAY文およびACCEPT文で使用するターミナルのエンコーディングを指定します。

- **値**: `UTF-8` または `UTF8`（デフォルトは Shift_JIS）
- **例**: `COB_TERMINAL_ENCODING=UTF-8`
- **用途**: UTF-8環境でのターミナル入出力に使用します。

#### COB_SWITCH_1 〜 COB_SWITCH_8

COBOLプログラム内で参照できるスイッチの初期値を設定します。

- **値**: `ON` で有効化（それ以外は無効）
- **例**: `COB_SWITCH_1=ON`
- **用途**: プログラムの動作を切り替えるためのフラグとして使用します。

**サンプルプログラム**

SPECIAL-NAMESで定義した SWITCH-1 と SWITCH-2 の状態を IF 文で判定して表示するプログラムです。環境変数 `COB_SWITCH_1`, `COB_SWITCH_2` で各スイッチの初期値を ON/OFF に設定できます。

```cobol
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      cobswitch.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           SWITCH-1 IS SW1
             ON STATUS IS SWIT1-ON
             OFF STATUS IS SWIT1-OFF
           SWITCH-2 IS SW2
             ON STATUS IS SWIT2-ON
             OFF STATUS IS SWIT2-OFF.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       PROCEDURE        DIVISION.
           DISPLAY "SWITCH-1: " NO ADVANCING.
           IF SWIT1-ON
              DISPLAY "ON"
           ELSE
              DISPLAY "OFF"
           END-IF.
           DISPLAY "SWITCH-2: " NO ADVANCING.
           IF SWIT2-ON
              DISPLAY "ON"
           ELSE
              DISPLAY "OFF"
           END-IF.
           STOP RUN.
```

実行例:

```bash
$ cobj cobswitch.cbl

$ COB_SWITCH_1=ON COB_SWITCH_2=OFF java cobswitch
SWITCH-1: ON
SWITCH-2: OFF

$ COB_SWITCH_1=OFF COB_SWITCH_2=ON java cobswitch
SWITCH-1: OFF
SWITCH-2: ON
```

### ファイル入出力

#### COB_FILE_PATH

ファイルを検索するデフォルトのディレクトリパスを指定します。

- **例**: `COB_FILE_PATH=/data/cobol/files`
- **用途**: SELECT句で指定されたファイル名に対して、このパスを基準に検索します。

**サンプルプログラム**

`SELECT TEST-FILE ASSIGN "PATHTEST"` で定義されたファイルに書き込むプログラムです。`COB_FILE_PATH` を設定すると、ファイルの作成先ディレクトリが変わります。

```cobol
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      cobfilepath.
       ENVIRONMENT      DIVISION.
       INPUT-OUTPUT     SECTION.
       FILE-CONTROL.
       SELECT TEST-FILE ASSIGN "PATHTEST".
       DATA             DIVISION.
       FILE             SECTION.
       FD TEST-FILE.
       01 TEST-REC      PIC X(4).
       PROCEDURE        DIVISION.
           OPEN OUTPUT TEST-FILE.
           MOVE "DATA" TO TEST-REC.
           WRITE TEST-REC.
           CLOSE TEST-FILE.
           DISPLAY "File created successfully.".
           STOP RUN.
```

実行例:

```bash
$ cobj cobfilepath.cbl

# 環境変数なし（カレントディレクトリに PATHTEST が作成される）
$ java cobfilepath
File created successfully.
$ ls PATHTEST
PATHTEST

# COB_FILE_PATH を設定（subdir/PATHTEST が作成される）
$ mkdir -p subdir
$ COB_FILE_PATH=subdir java cobfilepath
File created successfully.
$ ls subdir/PATHTEST
subdir/PATHTEST
```

#### DD_ファイル名 / dd_ファイル名

特定のファイルに対して実際のファイルパスを指定します。

- **形式**: `DD_<ファイル名>=<実際のパス>` または `dd_<ファイル名>=<実際のパス>`
- **例**: `DD_MASTER=/data/master.dat`
- **用途**: COBOLプログラム内のファイル名を実行時に別のパスにマッピングできます。

**サンプルプログラム**

`SELECT TEST-FILE ASSIGN "MYDATA"` でファイルを定義しています。環境変数 `DD_MYDATA` を設定すると、論理名 `MYDATA` が指定したパスにマッピングされます。

```cobol
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      ddfilename.
       ENVIRONMENT      DIVISION.
       INPUT-OUTPUT     SECTION.
       FILE-CONTROL.
       SELECT TEST-FILE ASSIGN "MYDATA".
       DATA             DIVISION.
       FILE             SECTION.
       FD TEST-FILE.
       01 TEST-REC      PIC X(4).
       PROCEDURE        DIVISION.
           OPEN OUTPUT TEST-FILE.
           MOVE "DATA" TO TEST-REC.
           WRITE TEST-REC.
           CLOSE TEST-FILE.
           DISPLAY "File created successfully.".
           STOP RUN.
```

実行例:

```bash
$ cobj ddfilename.cbl

# 環境変数なし（MYDATA というファイルが作成される）
$ java ddfilename
File created successfully.
$ ls MYDATA
MYDATA

# DD_MYDATA を設定（actual-output.dat にマッピングされる）
$ DD_MYDATA=actual-output.dat java ddfilename
File created successfully.
$ ls actual-output.dat
actual-output.dat
```

#### COB_SYNC

WRITE文、REWRITE文、DELETE文が成功した直後にファイルのflush（バッファの書き出し）を実行するかどうかを制御します。

- **値**: `Y` または `y` で有効化（flush を実行）、`P` または `p` でも有効化
- **例**: `COB_SYNC=Y`
- **用途**: 各書き込み操作の直後にバッファをディスクに書き出すことで、異常終了時のデータ損失を軽減できます。ただし、操作ごとにflushが発生するためパフォーマンスは低下します。

#### COB_LS_NULLS

準備中

#### COB_LS_FIXED

行順編成ファイルを固定長として扱うかどうかを制御します。

- **値**: `Y` または `y` で有効化
- **用途**: 行順編成ファイルのレコード長の扱いを指定します。

**サンプルプログラム**

`ORGANIZATION LINE SEQUENTIAL` のファイルに `PIC X(20)` のレコードへ `"AB"` を書き込むプログラムです。通常は末尾の空白がトリムされますが、`COB_LS_FIXED=Y` を設定すると固定長（20バイト + 改行）で書き込まれます。

```cobol
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      coblsfixed.
       ENVIRONMENT      DIVISION.
       INPUT-OUTPUT     SECTION.
       FILE-CONTROL.
       SELECT TEST-FILE ASSIGN       "./ls-test-output.txt"
                        ORGANIZATION LINE SEQUENTIAL.
       DATA             DIVISION.
       FILE             SECTION.
       FD TEST-FILE.
       01 TEST-REC      PIC X(20).
       PROCEDURE        DIVISION.
           OPEN OUTPUT TEST-FILE.
           MOVE "AB" TO TEST-REC.
           WRITE TEST-REC.
           CLOSE TEST-FILE.
           DISPLAY "File written.".
           STOP RUN.
```

実行例:

```bash
$ cobj coblsfixed.cbl

# 環境変数なし（末尾空白がトリムされ、"AB" + 改行 = 3バイト）
$ java coblsfixed
File written.
$ wc -c < ls-test-output.txt
3

# COB_LS_FIXED=Y を設定（固定長20バイト + 改行 = 21バイト）
$ COB_LS_FIXED=Y java coblsfixed
File written.
$ wc -c < ls-test-output.txt
21
```

#### OC_IO_CREATES

I-O モードでファイルを開く際に、ファイルが存在しない場合に自動作成するかどうかを制御します。

- **値**: `yes` で有効化
- **例**: `OC_IO_CREATES=yes`
- **用途**: OPEN I-O時にファイルが存在しない場合の挙動を制御します。

**サンプルプログラム**

存在しないファイルに対して `OPEN I-O` を実行し、FILE STATUS を確認するプログラムです。通常はステータス `35`（ファイルが存在しない）になりますが、`OC_IO_CREATES=yes` を設定するとファイルが自動作成されステータス `00` になります。

```cobol
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      ociocreates.
       ENVIRONMENT      DIVISION.
       INPUT-OUTPUT     SECTION.
       FILE-CONTROL.
       SELECT TEST-FILE ASSIGN       "./TEST-IO-FILE"
                        ORGANIZATION SEQUENTIAL
                        FILE STATUS  F-STATUS.
       DATA             DIVISION.
       FILE             SECTION.
       FD TEST-FILE.
       01 TEST-RECORD.
         03 TEST-KEY    PIC 9(10).
       WORKING-STORAGE  SECTION.
       77 F-STATUS      PIC X(02).
       PROCEDURE DIVISION.
           OPEN I-O TEST-FILE.
           DISPLAY "FILE STATUS: " F-STATUS.
           CLOSE TEST-FILE.
           STOP RUN.
```

実行例:

```bash
$ cobj ociocreates.cbl

# 環境変数なし（ファイルが存在しないためステータス 35）
$ java ociocreates
FILE STATUS: 35

# OC_IO_CREATES=yes を設定（ファイルが自動作成されステータス 00）
$ OC_IO_CREATES=yes java ociocreates
FILE STATUS: 00
```

#### OC_EXTEND_CREATES

EXTEND モードでファイルを開く際に、ファイルが存在しない場合に自動作成するかどうかを制御します。

- **値**: `yes` で有効化
- **例**: `OC_EXTEND_CREATES=yes`
- **用途**: OPEN EXTEND時にファイルが存在しない場合の挙動を制御します。

**サンプルプログラム**

存在しないファイルに対して `OPEN EXTEND` を実行し、FILE STATUS を確認するプログラムです。OC_IO_CREATES と同様に、`OC_EXTEND_CREATES=yes` を設定するとファイルが自動作成されます。

```cobol
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      ocextcreates.
       ENVIRONMENT      DIVISION.
       INPUT-OUTPUT     SECTION.
       FILE-CONTROL.
       SELECT TEST-FILE ASSIGN       "./TEST-EXT-FILE"
                        ORGANIZATION SEQUENTIAL
                        FILE STATUS  F-STATUS.
       DATA             DIVISION.
       FILE             SECTION.
       FD TEST-FILE.
       01 TEST-RECORD.
         03 TEST-KEY    PIC 9(10).
       WORKING-STORAGE  SECTION.
       77 F-STATUS      PIC X(02).
       PROCEDURE DIVISION.
           OPEN EXTEND TEST-FILE.
           DISPLAY "FILE STATUS: " F-STATUS.
           CLOSE TEST-FILE.
           STOP RUN.
```

実行例:

```bash
$ cobj ocextcreates.cbl

# 環境変数なし（ファイルが存在しないためステータス 35）
$ java ocextcreates
FILE STATUS: 35

# OC_EXTEND_CREATES=yes を設定（ファイルが自動作成されステータス 00）
$ OC_EXTEND_CREATES=yes java ocextcreates
FILE STATUS: 00
```

#### COB_FILE_SEQ_WRITE_BUFFER_SIZE

順編成ファイルの書き込みバッファサイズを指定します。

- **値**: 0以上の整数（デフォルト: 10）
- **例**: `COB_FILE_SEQ_WRITE_BUFFER_SIZE=100`
- **用途**: 書き込みパフォーマンスを調整できます。

#### COB_IO_ASSUME_REWRITE

REWRITEの前にREADが必要かどうかの動作を制御します。

- **値**: `Y` または `y` で有効化
- **例**: `COB_IO_ASSUME_REWRITE=Y`
- **用途**: READなしでREWRITEを許可する場合に使用します。

**サンプルプログラム**

3レコード（各4バイト）のファイルを `OPEN I-O` で開き、2レコード目まで READ した後に `WRITE` を実行するプログラムです。`COB_IO_ASSUME_REWRITE=Y` を設定すると、`WRITE` が `REWRITE` として動作し、2レコード目が上書きされます。

```cobol
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      cobrewrite.
       ENVIRONMENT      DIVISION.
       INPUT-OUTPUT     SECTION.
       FILE-CONTROL.
       SELECT TEST-FILE ASSIGN "TEST-REWRITE".
       DATA             DIVISION.
       FILE             SECTION.
       FD TEST-FILE.
       01 TEST-REC      PIC X(4).
       PROCEDURE        DIVISION.
           OPEN I-O TEST-FILE.
           READ  TEST-FILE.
           READ  TEST-FILE.
           MOVE  "AAAA" TO TEST-REC.
           WRITE TEST-REC.
           CLOSE TEST-FILE.
           STOP RUN.
```

実行例:

```bash
$ cobj cobrewrite.cbl

# 入力ファイルを作成（3レコード x 4バイト: "0000", "1111", "2222"）
$ printf "000011112222" > TEST-REWRITE

# COB_IO_ASSUME_REWRITE=Y を設定して実行
$ COB_IO_ASSUME_REWRITE=Y java cobrewrite

# 2レコード目が "AAAA" に上書きされている
$ cat TEST-REWRITE
0000AAAA2222
```

### 一時ファイル

#### TMPDIR / TMP / TEMP

ソート処理などで使用する一時ファイルのディレクトリを指定します。

- **優先順位**: `TMPDIR` > `TMP` > `TEMP`
- **例**: `TMPDIR=/tmp/cobol`
- **用途**: ソートや一時ファイルの作成場所を指定します。

### プログラム呼び出し

#### COB_LOAD_CASE

準備中

#### COB_LIBRARY_PATH

CALL文で呼び出されるプログラムを検索するパスを指定するための環境変数です。

- **注意**: 現在の実装では、設定された値は内部変数に格納されますが、プログラム検索時に参照されないため、動作に影響を与えません。

#### COB_PACKAGE_PATH

CALL文で呼び出されるJavaクラスのパッケージパスを指定します。

- **例**: `COB_PACKAGE_PATH=com.example.cobol`
- **用途**: Javaパッケージ構造でのプログラム検索に使用します。

#### COB_PRE_LOAD

プログラム開始時に事前ロードするモジュールを指定します。

- **注意**: 現在未実装

### 数値処理

#### COB_NIBBLE_C_UNSIGNED

符号なしパック10進数のニブル値に関する処理を制御します。

- **値**: `Y` または `y` で有効化
- **例**: `COB_NIBBLE_C_UNSIGNED=Y`
- **用途**: 特定のCOBOL方言との互換性のために使用します。

**サンプルプログラム**

`S9(2) COMP-3`（符号付き）と `9(2) COMP-3`（符号なし）の REDEFINES を使い、`IS NUMERIC` テストの結果を表示するプログラムです。通常は符号付きフィールドのみが NUMERIC と判定されますが、`COB_NIBBLE_C_UNSIGNED=Y` を設定するとニブルCが符号なしとして扱われ、符号なしフィールドも NUMERIC と判定されます。

```cobol
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      cobnibble.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  UNI01.
         03 SIGNED01.
           05 SDEC01 PIC S9(2) COMP-3.
         03 UNSIGNED01 REDEFINES SIGNED01.
           05 UDEC01 PIC  9(2) COMP-3.
       PROCEDURE        DIVISION.
           MOVE 1 TO SDEC01.
           IF SDEC01 IS NUMERIC THEN
               DISPLAY "Signed:   NUMERIC"
           ELSE
               DISPLAY "Signed:   NOT NUMERIC"
           END-IF.
           IF UDEC01 IS NUMERIC THEN
               DISPLAY "Unsigned: NUMERIC"
           ELSE
               DISPLAY "Unsigned: NOT NUMERIC"
           END-IF.
           STOP RUN.
```

実行例:

```bash
$ cobj cobnibble.cbl

# 環境変数なし（符号付きのみ NUMERIC と判定される）
$ java cobnibble
Signed:   NUMERIC
Unsigned: NOT NUMERIC

# COB_NIBBLE_C_UNSIGNED=Y を設定（両方 NUMERIC と判定される）
$ COB_NIBBLE_C_UNSIGNED=Y java cobnibble
Signed:   NUMERIC
Unsigned: NUMERIC
```
