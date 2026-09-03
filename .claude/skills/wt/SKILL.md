---
name: wt
description: git worktreeを使って新しい作業用ブランチとworktreeを作成する
---

# wt

`wt/{ブランチ名}` に新しい作業用ブランチとworktreeを作成する。

## 実行手順

1. ブランチ名を決める（`$ARGUMENTS` があればそれを使い、無ければユーザーに確認する）
2. `wt/{ブランチ名}` にworktreeを作成する
3. 作成したworktreeのパスをユーザーに伝える

```bash
BRANCH={ブランチ名}
ROOT=$(git rev-parse --show-toplevel)

git -C "$ROOT" worktree add -b "$BRANCH" "$ROOT/wt/$BRANCH"
```

## 注意事項

- `CLAUDE.md` と `.claude/`（`settings.local.json`を除く）はgit管理下にあるため、`git worktree add`で自動的にチェックアウトされる。
  リンクやコピーの作業は不要。
- `.claude/settings.local.json` は個人設定でgit管理外なので、worktreeには入らない。worktreeでも同じ個人設定を使いたい場合だけ、
  各worktreeにコピーするかシンボリックリンクを張る。
- worktreeでビルドする場合は `./configure --prefix=$(pwd)/local && make && make install` を使う（インストール先は各worktreeの`local/`）。
- `bin/cobjrun` は環境変数`CLASSPATH`を先頭に展開するため、worktree間でビルド結果を比較するときは
  `java -cp "<明示したlibcobj.jar>:." PROG` を直接使う（`.claude/rules/cobj-c-sources.md`参照）。
