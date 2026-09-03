これはopensource COBOL 4Jと呼ばれるCOBOLからJavaへのトランスパイラのリポジトリである。

# 開発方法

## ビルド方法

トップディレクトリのconfigure.acや各フォルダのMakefile.amを修正したときは、トップディレクトリで以下のコマンドを実行する

```bash
autoreconf && ./configure --prefix=local/ && make && make install
```

それ以外のコードを修正したときは、以下のコマンドを実行する

```bash
./configure --prefix=/local/ && make && make install
```

インストール先はlocal/であることに注意。

### ビルド時の落とし穴

- **sandboxを無効にする**: `make` / `make install` / `./format` / `tests/`配下のテスト実行は、libcobjのビルドでGradle wrapperが`~/.gradle/`に書き込むため、デフォルトのsandboxでは`Read-only file system`で失敗する。これらは最初から`dangerouslyDisableSandbox: true`で実行する（`configure`と`autom4te`はsandbox内で動く）。
- **UTF-8ビルドは`touch cobj/*.m4`が必須**: `./configure --enable-utf8`の後は必ず`touch cobj/*.m4 && make`する。`cobj/scanner.l`と`cobj/pplex.l`はm4生成物だがgitにコミットされており、checkout直後は`.l`と`.l.m4`のmtimeが同じでmakeが再生成しない。結果、Cコードは`I18N_UTF8`定義済みなのにレキサーはShift_JIS版という混成ビルドができ、日本語の利用者定義語が全滅する。`tests/cobol_utf8`が大量に「syntax error」で落ちたらまずこれを疑う（コードの不具合ではない）。SJIS版に戻すときも同様。
- **`Makefile.in`と`configure`は手で編集しない**: これらは`Makefile.am`と`configure.ac`からautoreconfが自動生成するファイル。編集するのは常に`.am`/`.ac`のほうで、生成物はautoreconfで作り直す。
- **autoreconfの出力は確認してからコミットする**: この環境で実行するとaclocal.m4からgettext/libtoolのm4マクロが脱落するなど、意図しない環境差分が混ざることがある(2026-08-05に15000行超の差分を確認)。CIはチェックイン済みのconfigure/Makefile.inをそのまま使うため、壊れた再生成ファイルをコミットするとビルドが壊れる。コミット前に`git diff --stat`で変更が意図した範囲に収まっているか確認すること。

## テスト方法

**tests/にテストコードがあるが、原則としてCIでのみ実行する。**

念のためtests/以下の構成を説明する。

- cobol85/ - COBOL 85の文法に基づくテストコード
- \*.at - たとえばmisc.atであれば、misc.src/以下のどのテストスクリプトを参照するかを定義する。command-line-options.at等も同じ規則。
  - makeを実行すると、テストスクリプトが生成される。make miscとすればmisc.atに基づいてテストスクリプトが生成される。misc.atのテストを実行するときは、tests/で./miscを実行すればよい

もしCOBOL85以外のテストを実行したい場合は、場合によってテストが長くなることがあるので、関係ない部分を`#`でコメントアウトしてからmakeでスクリプトを再生成してから実行すること。
misc.atのテストなら、テストの実行はmisc.dir/にて実施され、失敗したテストの記録はテスト終了後もそのフォルダに残る。

繰り返すが原則として、GitHubにpushしてCIでテストを実行すること。
(結果を適宜ghコマンドで確認するとよい)

### CIの既知のフレーク

`run-test-other (file-lock*, almalinux:9)`が`COB4J_REAL_JAVA does not point at a real java`を大量に出して落ちることがある。コード変更なしの再実行で成功するリポジトリ全体の既存問題。自分の変更を疑う前に失敗ログを`gh run download`で確認し、該当すれば`gh run rerun <run-id> --failed`する。なお`run-test-other`のmatrixはfail-fastが既定なので、1ジョブの失敗で10件前後がcancelされる。cancelは失敗ではない。

## コード変更終了後

一通りコードの修正が完了したら、トップディレクトリで`./format`を実行してコードを整形すること。
(Stopフックの`.claude/hooks/format-on-stop.sh`が変更のあったC/Javaソースを検出して自動実行するが、手動で実行しても構わない)
また、`code-reviewer`と`docs-sync-inspector`のサブエージェントを**同一メッセージ内で同時に**呼び出して、コードレビューとドキュメントの更新を並行して実施すること。
コードレビューに組み込みの`/code-review`スキルを使ってはいけない(トークン消費が大きいため`.claude/settings.json`の`skillOverrides`で非表示化済み。ユーザが明示的に`/code-review`と打った場合のみ動く)。レビューは常に`code-reviewer`サブエージェントで行う。

## サブエージェントの使い分け

以下は`.claude/agents/`で定義している。`docs-sync-inspector`以外は**読み取り専用**で、報告するだけでコードは変更しない。調査やログ確認を自分の会話に抱え込まずに委譲することで、メインの文脈を実装そのものに使えるようにする。

| エージェント | 使う場面 |
|---|---|
| `cobj-explorer` | 「この構文/機能はどこで処理されている?」を調べるとき。cobj/の1万行級のCソースやlibcobjの104ファイルを横断する調査は、**自分でgrepせず必ずこれに投げる** |
| `ci-triage` | CIが失敗したとき。`gh`でログを取り、既知のフレークか本当の回帰かを判定する。**巨大なCIログを自分で読まない** |
| `commit-preflight` | コミット/PRの直前。生成物の混入、CRLF・Shift_JISの事故、フォーマット漏れ、PMD違反を検査する |
| `code-reviewer` | 実装が一段落したとき、コミット前 |
| `docs-sync-inspector` | コード変更後にドキュメントの整合を取るとき。**これだけはドキュメントを実際に編集する**(コードは変更しない) |

複数を同時に走らせられる場面(レビューとドキュメント更新など)では、1つのメッセージ内で複数のエージェントを呼び出して並行させること。

## git

- **リモート名は環境によって異なる**。`origin`が存在しない構成もあるので、pushや`gh`の操作の前に必ず`git remote -v`で宛先を確認すること。
  - 上流は opensourcecobol/opensourcecobol4j。PRは上流の`develop`ブランチに出す(`/pr`)。
  - 自分のフォークは上流へ出す前の確認用に使う。PRのソースブランチはそちらにpushし、必要ならフォーク内でDraft PRを出してCIを回す(`/local-pr`)。
- **`git commit`は変更済みの追跡ファイルを全て巻き込む**: `.git/hooks/pre-commit`が`./format`を実行したあと`git add -u`をするため、明示的に`git add`したファイルだけをコミットすることはできない。生成物などを意図的に除外したい場合は`git commit --no-verify`を使う(この場合`./format`も走らないので必要なら手動で実行する)。またフックが`./format`経由でgradleを呼ぶため、**コミット自体もsandbox無効化が必要**。
- **`CLAUDE.md`と`.claude/`はgit管理下**。`git worktree add`で自動的にチェックアウトされるので、worktreeにリンクを張る必要はない。ただし`.claude/settings.local.json`（通知フックなどマシン固有の個人設定）は`.gitignore`で除外しているので、worktreeで使いたい場合は各自でコピーする。

# フォルダ構成

以下に主要なフォルダとファイルの説明を示す。

- cobj/ - COBOLからJavaへの変換のためのコードを格納
  - cobj.c - cobjコマンド(opensource COBOL 4Jのメインコマンド)のコード
  - codegen.c - COBOLからJavaへのコード生成を行うコード
  - esql.c - EXEC SQL文の解析と処理を行うコード
  - esql-scanner.l - flex向けのEmbedded SQLレキサー定義
  - esql-parser.y - bison向けのEmbedded SQLパーサー定義
  - typeck.c - codegen.c実行前に型チェックなどの多様な処理を行うコード
  - parser.y - bison向けのCOBOLパーサー定義
  - scanner.l - flex向けのCOBOLレキサー定義
  - scanner.l.m4 - m4マクロで、scanner.lを生成するためのファイル
- libcobj/ - libcobj.jarのためのコードを格納する。libocobj.jarは、COBOLからJavaへの変換で生成されたJavaコードが依存するライブラリ

# ファイル種別ごとの注意事項

対象ファイルを読んだときだけ読み込まれるルールを`.claude/rules/`に置いている（CRLF改行、Shift_JISの.atファイル、PMDの制約など）。全部を常時読む必要はない。
