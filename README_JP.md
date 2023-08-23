# opensource COBOL 4J

[English version README](./README.md)

opensource COBOL 4JはCOBOLソースコードをJavaソースコードに変換するCOBOLコンパイラです.
opensource COBOL 4JはCOBOLからCに変換するCOBOLコンパイラ["opensource COBOL"](https://github.com/opensourcecobol/opensource-cobol)をもとに開発されています.

変更履歴は[CHANGELOG.md](./CHANGELOG.md)を参照してください.

## ライセンス
ランタイムライブラリであるlibcobjはLGPL 3,それ以外のソフトウェアやライブラリはGPL 3の下で配布されています.

## インストール

[インストールページ](https://github.com/opensourcecobol/opensourcecobol4j/wiki/Installation_JP)をご覧ください.

## 使い方

コンパイル.
```bash
cobj [COBOL source file]
```
(cobjコマンドにより,カレントディレクトリに[PROGRAM-ID].javaと[PROGRAM-ID].classが生成されます.)

実行.
```bash
java [PROGRAM-ID]
```

## 実装状況

実装済み

* 基本的なデータ操作 (MOVE, COMPUTE, ... )
* 制御文 (IF, PERFORM, GO TO, ...)
* DISPLAY文, ACCEPT文
* CALLによる呼び出し
* SEQUENTIALファイルの入出力機能
* RELATIVEファイルの入出力機能
* INDEXEDファイルの入出力機能
* SORT文
* 組み込み関数 (ACOS, LENGTH, MAX, ...)

# テストのステータス

## NIST COBOL85 test suite

opensource COBOL 4Jは[NIST COBOL85 test suite](https://www.itl.nist.gov/div897/ctg/cobol_fo
rm.htm)によりテストされています。

主要テスト結果

```
------ Directory Information -------   --- Total Tests Information ---
Module Programs Executed Error Crash   Pass Fail Deleted Inspect Total
------ -------- -------- ----- -----  ----- ---- ------- ------- -----
NC           90       90     0     0   4352    0       6      11  4369   COBOLの中核機能のテスト
SM           15       15     0     0    290    0       3       1   294   COPY句のテスト
IC           13       13     0     0     97    0       0       0    97   CALL文のテスト
SQ           81       81     0     0    512    0       6      81   599   Sequentialファイルの入出力テスト
IX           39       39     0     0    507    0       1       0   508   Indexedファイルの入出力テスト
ST           39       39     0     0    278    0       0       0   278   SORT文のテスト
SG            5        5     0     0    193    0       0       0   193   セグメントのテスト
OB            5        5     0     0     16    0       0       0    16   廃止された機能のテスト
IF           42       42     0     0    732    0       0       0   732   組み込み関数のテスト
RL           32       32     0     0   1827    0       5       0  1832   Relativeファイルの入出力テスト
------ -------- -------- ----- -----  ----- ---- ------- ------- -----
Total       361      361     0     0   8804    0      21      93  8918
```

その他のテストの結果

```
------ directory information -------   --- total tests information ---
module programs executed error crash   pass fail deleted inspect total
------ -------- -------- ----- -----  ----- ---- ------- ------- -----
cm            7        0     7     0      0    0       0       0     0   COMMUNICATION SECTIONのテスト
db           10        0    10     0      0    0       0       0     0   Debugging機能のテスト
rw            4        0     4     0      0    0       0       0     0   REPORT SECTIONのテスト
------ -------- -------- ----- -----  ----- ---- ------- ------- -----
total        21        0    21     0      0    0       0       0     0
```

# コントリビュータ

https://github.com/opensourcecobol/opensourcecobol4j/graphs/contributors を参照してください.
