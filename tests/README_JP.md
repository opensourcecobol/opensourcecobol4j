makeコマンドを実行すると,下記のテストスクリプトが生成される

* cobj-idx
* cobol\_utf8
* command-line-options
* data-rep
* file-lock
* file-lock2
* i18n\_sjis
* i18n\_utf8
* jp-compat
* misc
* multithread
* run
* syntax

例えば./command-line-options を実行すればコマンドラインオプションに関するテストが実行される.

multithreadは,1つのJVM内の複数のスレッドから変換後のプログラムを同時に実行するテストである.
テストプログラムは`multithread.src/harness/`にある小さなJavaのハーネスから起動され,
スレッドの生成と同期はこのハーネスが行う.
このテストが検証している実行単位のモデルについては,
[マルチスレッドのJavaアプリケーションから変換後のプログラムを呼び出す](../doc/multithreading_JP.md)を参照.
