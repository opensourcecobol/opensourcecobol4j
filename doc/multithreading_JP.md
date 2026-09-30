# マルチスレッドのJavaアプリケーションから変換後のプログラムを呼び出す

`cobj`で変換したプログラムは、TomcatやSpring Bootのようなマルチスレッドの
Javaアプリケーションから呼び出すことができる。
1つのJVM内の複数のスレッドが、同時にCOBOLプログラムを実行してもよい。

本文書では、それを可能にしている実行モデルと、JavaとCOBOLの境界でアプリケーションが
行うべき処理について説明する。

以前のバージョンの`cobj`で変換したプログラムは、変換し直す必要がある。
変換後のコードは、以前は直接参照していた`libcobj`の公開staticフィールドの代わりに、
スレッドごとの状態にアクセスするメソッドを呼び出すようになったため、
古いクラスをこの`libcobj.jar`で実行すると`NoSuchFieldError`で失敗する。

## 1スレッド = 1実行単位

`libcobj`はCOBOLの**実行単位(run unit)**の状態をスレッドごとに保持しており、
**各スレッドはそれぞれ独立した実行単位**として扱われる。
以下の状態は呼び出し元のスレッドに紐づいており、他のスレッドと共有されることはない。

* モジュールスタックと現在のモジュール(`CobolModule`)
* 直前の例外コードと、直前にエラーが発生したファイル
* `CALL`文で解決したプログラムのキャッシュとコールスタック
* 実行単位が開いたファイル、その位置、および保持しているロック
* `EXTERNAL`のデータ項目と`EXTERNAL`ファイル
* `SWITCH-1`〜`SWITCH-8`、`CALL`の引数の個数、`DISPLAY UPON COMMAND-LINE`のデータ
* ESQLの接続、カーソル、プリペアドステートメント、`SQLCA`

一方、以下はJVM全体で共有される。

* `System.out`、`System.err`、`System.in`
* `COB_*`環境変数から読み込まれるランタイム設定
  ([環境変数リファレンス](./environment_variables_JP.md)を参照)。
  これらはスレッドごとではなく、JVMごとに1回だけ読み込まれる。
  ただし`COB_SWITCH_1`〜`COB_SWITCH_8`は例外で、実行単位ごとに読み込まれる
* ファイルシステム。したがって、JVM内の複数の実行単位が同じファイルに対して
  取得するファイルロック・レコードロック

`WORKING-STORAGE`は変換後のJavaコードでstaticな変数にはならず、
変換後のクラスのインスタンスが保持する。
そのため、**変換後のプログラムのインスタンスはスレッドごとに用意すること**。
1つのインスタンスを複数のスレッドから同時に使用してはならない。

```java
import jp.osscons.opensourcecobol.libcobj.common.CobolRunUnit;
import jp.osscons.opensourcecobol.libcobj.data.CobolDataStorage;

// COBOLプログラム"sample"のインスタンスをスレッドごとに保持する
private static final ThreadLocal<sample> program = ThreadLocal.withInitial(sample::new);

CobolDataStorage arg1 = new CobolDataStorage(9);
CobolDataStorage arg2 = new CobolDataStorage(9);
int returnCode = program.get().run(arg1, arg2);
```

変換後のプログラムは`jp.osscons.opensourcecobol.libcobj.call.CobolRunnable`を実装しており、
その`run`メソッドは`PROCEDURE DIVISION USING`の引数を`CobolDataStorage`として受け取り、
`RETURN-CODE`を返す。
`CALL`文で呼び出されるプログラムも、呼び出し元スレッドの実行単位が解決・生成するため、
その`WORKING-STORAGE`も同様にスレッドごとに独立している。

## ランタイムの初期化

`CobolUtil.ensureInitialized()`はランタイム設定(`COB_*`環境変数、プログラムスイッチ、
ファイルI/Oの初期化)を行う。
初期化はJVM内で高々1回だけ実行され、複数のスレッドから同時に呼び出しても安全である。

変換後のプログラムは実行開始時に自身でこのメソッドを呼び出すため、
COBOLプログラムを呼び出すだけのアプリケーションでは明示的に呼び出す必要はない。
最初のCOBOL呼び出しより前にランタイム設定を反映させたい場合(アプリケーションの起動時など)に、
明示的に呼び出す。

```java
import jp.osscons.opensourcecobol.libcobj.common.CobolUtil;

CobolUtil.ensureInitialized();
```

初期化済みかどうかは`CobolUtil.isInitialized()`で確認できる。

## 実行単位を終了する

```java
jp.osscons.opensourcecobol.libcobj.common.CobolRunUnit.end();
```

`CobolRunUnit.end()`は、呼び出し元スレッドの実行単位を終了する。
開いたままのファイルは暗黙的にクローズされ(レコードがコミットされ、ロックが解放される)、
上に挙げた実行単位の状態はすべて破棄される。

スレッドでのCOBOLの処理が終わった時点、たとえばスレッドを再利用するサーバでは
リクエストの処理の最後に呼び出すこと。
呼び出さない場合、前の実行単位の状態(開いたままのファイル、解決済みのプログラム、
`EXTERNAL`項目、スイッチ)がプールされたスレッドに残り、
次にそのスレッドで処理されるリクエストから見えてしまう。

プログラムが`DISCONNECT`せずに残したESQLの接続は、未確定の更新をロールバックしてクローズされる
(プロセスの終了で接続が切れた場合と同じ結果になる。実行時エラーによる異常終了でも途中までの更新が確定しないようにするため)。

`CobolRunUnit.resetThreadState()`は、ファイルのクローズを行わずに同じ状態を破棄する。
ファイルを既にクローズしている場合にのみ使用すること。

実行単位を終了しても、プログラム自体のJavaオブジェクトは初期化されない。
生成されたプログラムの同じインスタンスをその後もう一度実行すると、
`WORKING-STORAGE`には前回の実行の値が残っている。`VALUE`句の値から開始したい場合は、
新しい実行単位では新しいインスタンスを作成すること。`CALL`で到達するプログラムは
`CALL`のキャッシュが実行単位に属するため、新たにインスタンス化される。

`SET ENVIRONMENT`や`DISPLAY ... UPON ENVIRONMENT-VALUE`で設定した値も実行単位に属する。
同じスレッドのプログラムからのみ見え、実行単位とともに破棄される。

`STOP RUN`でプログラムが終了した場合、実行単位は既に終了しているため(後述)、
その後に`CobolRunUnit.end()`を呼び出しても問題はない。

## `STOP RUN`

Javaから呼び出されたプログラムの`STOP RUN`は、JVMを終了させ**ない**。
呼び出し元スレッドの実行単位を終了し(開いたままのファイルは暗黙的にクローズされる)、
その実行単位のルートのプログラムを呼び出したJavaのコードへ復帰する。
`run`は`STOP RUN`の返り値を返す。他のスレッドの実行には影響しない。

`CALL`文で呼び出されたプログラムで`STOP RUN`が実行された場合、
同じスレッドの呼び出し元のCOBOLプログラムを実行単位のルートまで巻き戻し、
そのルートのプログラムだけがJavaへ復帰する。

`System.exit`を呼び出すのは、変換後のプログラムの`main`メソッド、
すなわち`java [PROGRAM-ID]`のようにコマンドラインから起動した場合だけである。
このときプロセスは`STOP RUN`の返り値で終了する。

## `DISPLAY`文と`ACCEPT`文

`System.out`と`System.err`はJVM全体で共有される。
1つの`DISPLAY`文の出力はまとめて書き出されるため、
1つの文の出力の途中に他のスレッドの出力が混ざることはない。
ただし、複数のスレッドの出力の順序は規定されない。

`ACCEPT`文は共有された標準入力から読み込む。
サーバ上では意味を持たないことが多く、`ACCEPT`を実行したスレッドは
同じストリームを他のすべてのスレッドと奪い合うことになる。

## スレッド間でのファイルの共有

同一JVM内の複数の実行単位は、別々のプロセスと同じ規則でファイルを共有する。

* **順編成・行順編成・相対編成ファイル**: `OPEN`時に取得するロックは、
  OSのロックに加えてJVM内でも記録される。
  そのため、同一JVM内の別スレッドが排他的に開いているファイルを開こうとすると、
  別のプロセスがそのファイルを保持している場合と同様に`FILE STATUS 61`となる。
  すべてのスレッドが入力用に開いている場合は共有できる。
* **INDEXEDファイル**: ファイルロックとレコードロックの所有者はプロセスではなく実行単位である。
  そのため、あるスレッドがロックしたレコードを同一JVM内の別スレッドがロック付きで
  読むことはできず(`FILE STATUS 51`)、`OPEN OUTPUT`でロックされたファイルを
  別スレッドが開くこともできない(`FILE STATUS 61`)。
  [INDEXEDファイルのロックの仕様](./specification-locking-indexed-file_JP.md)を参照。

ファイルのクローズ、`STOP RUN`、`CobolRunUnit.end()`のいずれによっても、
その実行単位が保持しているロックは解放される。

## ログ出力(SLF4J)

`libcobj.jar`はshadow jar(fat jar)であり、SLF4Jのプロバイダとして`slf4j-simple`を同梱している。
Spring Bootのアプリケーションは`spring-boot-starter-logging`によって独自のプロバイダ(Logback)を
持ち込むため、クラスパス上でプロバイダが衝突する。どちらか一方を除外する必要がある。
`libcobj.jar`が同梱するプロバイダを使う場合は、以下のように設定する。

```kotlin
// build.gradle.kts
configurations.all {
    exclude(group = "org.springframework.boot", module = "spring-boot-starter-logging")
    exclude(group = "ch.qos.logback")
}
```

[Visual Studio Codeを利用したSpring Bootプロジェクトの作成](./cobj-api_SpringBoot_JP.md)も参照。

## サンプルとテスト

* `samples/spring-boot-smoke/`は、HTTPリクエストのスレッド上でCOBOLプログラムを実行する
  小さなSpring Bootアプリケーションである。
  プログラムのインスタンスと実行単位をスレッドごとに持ち、
  `CobolRunUnit.end()`を呼び出すエンドポイントを備えている。
* `tests/multithread`は、変換後のプログラムを並行に実行するテストスイートである。
  算術演算と組み込み関数、`CALL`されたプログラムの`WORKING-STORAGE`、`STOP RUN`、
  順編成ファイル、ファイルの共有、INDEXEDファイルのレコードロック、`SORT`を検証する。
