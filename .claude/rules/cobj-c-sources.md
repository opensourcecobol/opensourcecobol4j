---
paths:
  - "cobj/**"
  - "bin/**"
  - "libcob/**"
  - "lib/**"
---

# cobj のCソース・生成物を扱うときの注意

- **`cobj/codegen.c`はCRLF改行**: テキストモードでの一括置換（Pythonの`open(p).read()`/`write()`等）を行うとLFに正規化され、7000行超のファイル全体が差分になる。Edit/Writeツールを使うか、スクリプトならバイナリモードで読み書きすること。編集後は`git diff --stat`で行数を確認する。
- **ビルドで再生成される生成物をコミットしない**: コミット前に`cobj/esql-scanner.c`が差分に出ていないか確認し、出ていれば`git checkout --`で戻す。UTF-8ビルドをした場合は`cobj/pplex.c` `cobj/scanner.c` `cobj/pplex.l` `cobj/scanner.l`も同様。
- **`cobj/scanner.l`と`cobj/pplex.l`はm4生成物**: 実体の編集対象は`.l.m4`のほう。`.l`を直接編集しても`touch cobj/*.m4 && make`で上書きされる。
- **`bin/cobjrun`は環境変数CLASSPATHを先頭に展開する**: `bin/cobjrun.c`は`java -cp "$CLASSPATH:<インストール先のjar>"`という形でコマンドを組むため、CLASSPATHが設定されているとそちらのlibcobj.jarが優先される。worktree間のビルド比較やベンチでcobjrunを使うと全worktreeで同じ実装が動いてしまう。比較検証では`java -cp "<明示したlibcobj.jar>:." PROG`を直接使うこと。
