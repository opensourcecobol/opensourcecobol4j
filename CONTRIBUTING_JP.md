opensource COBOL 4Jへのコントリビュートを検討頂きありがとうございます。
下記にコントリビュートの手順を示します。

# Issues

opensource COBOL 4Jに関するトピックを投稿してください。ただし、英語か日本語での記載をお願いします。

# Pull Requests

We will check pull requests which pass all CI checks which run tests and static code analysis.
The static analysis checks whether C and Java source files are respectively formated with [clang-format](https://clang.llvm.org/docs/ClangFormat.html) and [google-java-format](https://github.com/google/google-java-format) and [PMD](https://pmd.github.io/) finds no error and warning in Java source files.

The below sections describe how to setup and run static code analysis.

CIはテストとコードの静的解析を実行します。
CIの静的解析はCとJavaのソースコードがそれぞれ[clang-format](https://clang.llvm.org/docs/ClangFormat.html) and [google-java-format](https://github.com/google/google-java-format)で整形されているか、
[PMD](https://pmd.github.io/)によるJavaソースコードの静的解析でエラーや警告が表示されないかをチェックします。

下記にそれぞれのツールのセットアップと使用方法を説明します。

## セットアップ

### clang-format

Ubuntuでは`sudo apt install clang-format`コマンドを実行すれば`clang-format`をインストールできます。

### google-java-format

下記のコマンドを実行して`google-java-format`をダウンロードしてください。
```sh
curl -L -o google-java-format.jar https://github.com/google/google-java-format/releases/download/v1.15.0/google-java-format-1.15.0-all-deps.jar
```
さらに環境変数`PATH_GOOGLE_JAVA_FORMAT`に上記のjarファイルのファイルパスを設定してください。

### PMD

下記のコマンドを実行して`PMD`をインストールしてください。
```sh
curl -L -o pmd-bin-6.52.0.zip https://github.com/pmd/pmd/releases/download/pmd_releases%2F6.52.0/pmd-bin-6.52.0.zip
unzip pmd-bin-6.52.0.zip
mv pmd-bin-6.52 /usr/local/bin/
rm pmd-bin-6.52.0.zip
```

## 静的解析の実行

### clang-formatとgoogle-java-format

opensource COBOL 4Jのトップディレクトリで`./format`を実行してください。
`./check-format`を実行することで、フォーマットが完了したかを確認できます。

### PMD

opensource COBOL 4Jのトップディレクトリで下記のコマンドを実行してください。
```
/usr/local/bin/pmd-bin-6.52.0/bin/run.sh -d libcobj/src -R .github/ruleset.xml -f text
```
