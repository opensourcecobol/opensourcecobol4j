opensource COBOL 4Jへのコントリビュートを検討頂きありがとうございます。
下記にコントリビュートの手順を示します。

# Issues

opensource COBOL 4Jに関するトピックを投稿してください。ただし、英語か日本語での記載をお願いします。

# Pull Requests
CIはテストとコードの静的解析を実行します。
CIの静的解析はCとJavaのソースコードがそれぞれ[clang-format](https://clang.llvm.org/docs/ClangFormat.html) and [google-java-format](https://github.com/google/google-java-format)で整形されているか、
[PMD](https://pmd.github.io/)によるJavaソースコードの静的解析でエラーや警告が表示されないかをチェックします。

下記にそれぞれのツールのセットアップと使用方法を説明します。

## 開発環境のセットアップ

一貫した開発環境を確保するために、[Visual Studio Code with Dev Containers](https://code.visualstudio.com/docs/devcontainers/containers)の使用を強く推奨します。以下の手順に従って開発環境をセットアップしてください。

1. [Docker](https://www.docker.com/get-started)をインストールします。
1. [Visual Studio Code](https://code.visualstudio.com/)をインストールします。
1. Visual Studio Codeに[Remote - Containers](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers)拡張機能をインストールします。
1. リポジトリをクローンします。
1. Visual Studio Codeでリポジトリを開きます。
1. `Ctrl+Shift+P`を押して、`Dev Containers: Reopen in Container`を選択します。
1. DevContainerの起動とビルドが完了するまで待ちます。このプロセスは数分かかることがあります。
1. （オプション）`Ctrl+Shift+@`を押して、Visual Studio Codeの新しいターミナルを開きます。
1. （オプション）[gitの認証情報を設定](https://code.visualstudio.com/remote/advancedcontainers/sharing-git-credentials)します。

> [!CAUTION]
> Dev container内では、[Git Hooks](https://git-scm.com/book/ms/v2/Customizing-Git-Git-Hooks)が`git commit`コマンドを開始する際にコードフォーマッタを実行します。初めて`git commit`を実行する際には数分かかることがあります。


## 静的解析の実行

> [!CAUTION]
> CIはAlmalinux 9でフォーマッタと静的解析ツールを実行します。これらのツールの動作は他のオペレーティングシステムとは異なる場合があります。

### clang-formatとgoogle-java-format

opensource COBOL 4Jのトップディレクトリで`./format`を実行してください。
`./check-format`を実行することで、フォーマットが完了したかを確認できます。

### PMD

opensource COBOL 4Jのlibcobj/ディレクトリで下記のコマンドを実行してください。
```
./gradlew pmdMain
```
