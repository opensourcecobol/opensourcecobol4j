# opensource COBOL 4j

gEnglish version README](https://github.com/opensourcecobol/opensourcecobol4j/blob/develop/README.md)

opensource COBOL 4jはCOBOLソースコードをJavaソースコードに変換するCOBOLコンパイラです.
opensource COBOL 4jはCOBOLからCに変換するCOBOLコンパイラ["opensource COBOL"](https://github.com/opensourcecobol/opensource-cobol)をもとに開発されています.

**注意**: 現在は開発者バージョンです

## インストール

[インストールページ](https://github.com/opensourcecobol/opensourcecobol4j/wiki/Installation_JP)をご覧ください.

## 使い方

コンパイル.
```bash
cobc -m [COBOL source file]
```
(cobcコマンドにより,カレントディレクトリに[PROGRAM-ID].javaと[PROGRAM-ID].classが生成されます.)

実行.
```bash
java [PROGRAM-ID]
```

## The progress of the development

下記の実装済みリストにある機能は[NIST COBOL85 test suite](https://www.itl.nist.gov/div897/ctg/cobol_form.htm)でテストされており,**95%** のテストケースをパスしています.

実装済み

* 基本的なデータ操作 (MOVE, COMPUTE, ... )
* 制御文 (IF, PERFORM, GO TO, ...)
* DISPLAY文, ACCEPT文
* CALLによる呼び出し
* SEQUENTIALファイルの入出力機能
* INDEXEDファイルの入出力機能
* SORT文

実装予定

* RELATIVEファイルの入出力機能
* 組み込み関数 (ACOS, LENGTH, MAX, ...)

既知の不具合

* 同一ソースコード内の別プログラムのCALLができない.
* ALTERNATE KEYが指定されたINDEXEDファイルに対するREWRITE文の不具合.
* SORT文の結果が少数のケースにおいて不正になること.
