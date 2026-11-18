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
4. worktreeの `.claude-work/task.md` を作成し、修正すべきテストの情報を記載する
5. worktreeディレクトリで `./configure --prefix=$(pwd)/local && make && make install` を実行してビルドする
6. 最後に、ユーザーにworktreeのパスを伝え、そのディレクトリで `claude` コマンドを実行して `/fix-skipped-test` コマンドを使うよう案内する

## 実行するコマンドの例

```bash
# デフォルトブランチからworktreeを作成する（ブランチ名は決め打ちにしない）
BASE=$(git remote show {リモート名} | sed -n '/HEAD branch/s/.*: //p')
# git remote show はリモートに問い合わせる。ローカルだけで済ませたい場合は
#   BASE=$(git symbolic-ref --short refs/remotes/{リモート名}/HEAD | sed 's|^{リモート名}/||')
# だが refs/remotes/<リモート名>/HEAD が未設定のcloneでは空になる（先に git remote set-head <リモート名> -a が要る）
git fetch {リモート名} "$BASE"
git worktree add -b {ブランチ名} wt/{ブランチ名} {リモート名}/"$BASE"

# .claude-work/task.mdを作成（fix-skipped-testが自動で読み込む）
mkdir -p wt/{ブランチ名}/.claude-work
cat > wt/{ブランチ名}/.claude-work/task.md << 'EOF'
# 修正対象テスト

- file: {--fileの値}
- test-title: {--test-titleの値}
EOF

# worktreeディレクトリに移動してビルド
cd wt/{ブランチ名}
./configure --prefix=$(pwd)/local && make && make install
```

## 注意事項

- worktreeは**デフォルトブランチ**から作成する。ブランチ名を決め打ちにせず、`git remote show <リモート名>` の `HEAD branch` から取得する
  （`gh` が使えるなら `gh repo view <owner>/<repo> --json defaultBranchRef -q .defaultBranchRef.name` でもよい）
- ブランチ名は `fix/` プレフィックスをつけることを推奨（例: `fix/pointer-display`）
- ビルドエラーが発生した場合はユーザーに報告して確認する
- `CLAUDE.md` と `.claude/` はgit管理下なので、`git worktree add` で自動的にチェックアウトされる。リンクを張る必要はない
  （個人設定の `.claude/settings.local.json` だけはgit管理外なので、必要なら各自でコピーする）
- `.claude-work/task.md` を作成することで、worktreeで `/fix-skipped-test` を引数なしで実行できる
- `.claude-work/` はClaudeの作業用ディレクトリ（`.gitignore`で除外済み）で、worktreeごとに独立する
