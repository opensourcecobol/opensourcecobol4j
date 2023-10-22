opensource COBOL 4Jへのコントリビュートを検討頂きありがとうございます。
下記にコントリビュートの手順を示します。

# Issues

opensource COBOL 4Jに関するトピックを投稿してください。ただし、英語か日本語での記載をお願いします。

# Pull Requests
CIはテストとコードの静的解析を実行します。
CIの静的解析はCとJavaのソースコードがそれぞれ[clang-format](https://clang.llvm.org/docs/ClangFormat.html) and [google-java-format](https://github.com/google/google-java-format)で整形されているか、
[PMD](https://pmd.github.io/)によるJavaソースコードの静的解析でエラーや警告が表示されないかをチェックします。

下記にそれぞれのツールのセットアップと使用方法を説明します。

## セットアップ

Ubuntuでは`sudo apt install clang-format`コマンドを実行すれば`clang-format`をインストールできます。

## 静的解析の実行

### clang-formatとgoogle-java-format

opensource COBOL 4Jのトップディレクトリで`./format`を実行してください。
`./check-format`を実行することで、フォーマットが完了したかを確認できます。

### PMD

opensource COBOL 4Jのlibcobj/ディレクトリで下記のコマンドを実行してください。
```
./gradlew pmdMain
```
