Thank you for your interest in contributing to opensource COBOL 4J.
A summary of how to contribute is below.

# Issues

Although any topics related to opensource COBOL 4J can be posted in [Issues](https://github.com/opensourcecobol/opensourcecobol4j/issues), please submit ones written in English or Japanese.

# Pull Requests

We will check pull requests that passed all CI checks running both tests and static code analysis.
The static analysis checks whether C and Java source files are formatted using [clang-format](https://clang.llvm.org/docs/ClangFormat.html) and [google-java-format](https://github.com/google/google-java-format) respectively, and whether [PMD](https://pmd.github.io/) finds no error and warning in Java source files.

Before you submit pull requests, you should run `./format` in order to format files in this repository.

The below sections describe how to setup and run static code analysis.

## Setup Development Environment

We strongly recommend using [Visual Studio Code with Dev Containers](https://code.visualstudio.com/docs/devcontainers/containers) for a consistent development environment. Follow the steps below to set up your development environment.

1. Install [Docker](https://www.docker.com/get-started) on your machine.
1. Install [Visual Studio Code](https://code.visualstudio.com/).
1. Install the [Remote - Containers](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers) extension in Visual Studio Code.
1. Clone the repository.
1. Open the repository in Visual Studio Code.
1. Press `Ctrl+Shift+P` and Select `Dev Containers: Reopen in Container`.
1. Wait for the DevContainer to start up and the build to complete. It may take several minutes to complete this process.
1. (Optional) Press `Ctrl+Shift+@` to open a new terminal of Visual Studio code.
1. (Optional) [Setup credentials for git](https://code.visualstudio.com/remote/advancedcontainers/sharing-git-credentials).

## Run static analysis

> [!CAUTION]
> Since the behavior of these tools may differ from the one in other operatins systems, we recommend that you run `./format` in Visual Studio Code with Dev Containers described in the previous section.

### check with clang-format and google-java-format

Run `./format` in the top directory of opensource COBOL 4J.
If you want to make sure all files are formatted, run `./check-format` in the top directory of opensource COBOL 4J.

### PMD

Run the following command in libcobj/ directory:
```
./gradlew pmdMain
```
