# setup-worktree

SKIP_TESTを修正するための新しいworktree作業環境を作成する。

## 引数

`$ARGUMENTS` に `--file` と `--test-title` を指定する。省略された場合はユーザーに確認する。

- `--file`: テストファイルのパス（例: `cobol_utf8.src/pic-x.at`、`data-rep.src/pointer.at`）
- `--test-title`: テスト名（例: `POINTER: display`）

ブランチ名は `--test-title` から自動生成する（スペースをハイフンに変換し、記号を除去して `fix/` プレフィックスをつける）。

## 実行手順

1. 引数から `--file` と `--test-title` を取得する。不足している場合はユーザーに確認する
2. テスト名からブランチ名を自動生成する
3. `wt/` ディレクトリ配下に `wt/{ブランチ名}` としてworktreeを作成する
4. worktreeのディレクトリに `task.md` を作成し、修正すべきテストの情報を記載する
5. worktreeディレクトリで `./configure --prefix=$(pwd)/local && make && make install` を実行してビルドする
6. 最後に、ユーザーにworktreeのパスを伝え、そのディレクトリで `claude` コマンドを実行して `/fix-skipped-test` コマンドを使うよう案内する

## 実行するコマンドの例

```bash
# 開発ブランチからworktreeを作成（ベースブランチはリポジトリの構成に合わせる）
git worktree add -b {ブランチ名} wt/{ブランチ名} {ベースブランチ}

# task.mdを作成（fix-skipped-testが自動で読み込む）
cat > wt/{ブランチ名}/task.md << 'EOF'
# 修正対象テスト

- file: {--fileの値}
- test-title: {--test-titleの値}
EOF

# worktreeディレクトリに移動してビルド
cd wt/{ブランチ名}
./configure --prefix=$(pwd)/local && make && make install
```

## 注意事項

- worktreeは開発ブランチ（`develop`。無ければ`main`）から作成する
- ブランチ名は `fix/` プレフィックスをつけることを推奨（例: `fix/pointer-display`）
- ビルドエラーが発生した場合はユーザーに報告して確認する
- `CLAUDE.md` と `.claude/` はgit管理下なので、`git worktree add` で自動的にチェックアウトされる。リンクを張る必要はない
  （個人設定の `.claude/settings.local.json` だけはgit管理外なので、必要なら各自でコピーする）
- `task.md` を作成することで、worktreeで `/fix-skipped-test` を引数なしで実行できる
