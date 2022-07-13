g opensource COBOL 4j

[English version README](https://github.com/opensourcecobol/opensourcecobol4j/blob/develop/README.md)

opensource COBOL 4jはCOBOLソースコードをJavaソースコードに変換するCOBOLコンパイラです.
opensource COBOL 4jはCOBOLからCに変換するCOBOLコンパイラ["opensource COBOL"](https://github.com/opensourcecobol/opensource-cobol)をもとに開発されています.

## ライセンス
ランタイムライブラリであるlibcobjはLGPL 3,それ以外のソフトウェアやライブラリはGPL 3の下で配布されています.

## インストール

[インストールページ](https://github.com/opensourcecobol/opensourcecobol4j/wiki/Installation_JP)をご覧ください.

## 使い方

コンパイル.
```bash
cobj [COBOL source file]
```
(cobjコマンドにより,./buildに[PROGRAM-ID].javaと[PROGRAM-ID].classが生成されます.)

実行.
```bash
java [PROGRAM-ID]
```

## The progress of the development

下記の実装済みリストにある機能は[NIST COBOL85 test suite](https://www.itl.nist.gov/div897/ctg/cobol_form.htm)でテストされており,**999%** のテストケースをパスしています.

実装済み

* 基本的なデータ操作 (MOVE, COMPUTE, ... )
* 制御文 (IF, PERFORM, GO TO, ...)
* DISPLAY文, ACCEPT文
* CALLによる呼び出し
* SEQUENTIALファイルの入出力機能
* INDEXEDファイルの入出力機能
* SORT文
* 組み込み関数 (ACOS, LENGTH, MAX, ...)

実装予定

* RELATIVEファイルの入出力機能

既知の不具合

* 同一ソースコード内の別プログラムのCALLができない.
