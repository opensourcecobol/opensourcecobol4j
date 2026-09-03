#!/bin/bash
# Stop hook: 未コミットのC/Javaソースに前回のフォーマット以降の変更があれば ./format を実行する。
#
# CLAUDE.md の「コード変更終了後に ./format を実行する」を確実にするためのフック。
# worktree から起動された場合も git rev-parse でそのworktreeのルートを解決するので、
# 各worktreeが自分自身をフォーマットする。
set -u

ROOT=$(git rev-parse --show-toplevel 2>/dev/null) || exit 0
cd "$ROOT" || exit 0
[ -x ./format ] || exit 0

STAMP="$(git rev-parse --git-path claude-format-stamp)"

# 未コミットの変更があるC/Javaソース（削除されたものは除く）
# --untracked-files=no: cobj/parser.tab.c や root の test__*.java など未追跡の生成物を対象外にする
mapfile -t files < <(git status --porcelain --untracked-files=no -- '*.c' '*.h' '*.java' \
  | sed 's/^...//' | sed 's/.* -> //' | while read -r f; do [ -f "$f" ] && printf '%s\n' "$f"; done)
[ "${#files[@]}" -eq 0 ] && exit 0

# 前回 ./format を走らせた時点より新しい変更が無ければ何もしない
if [ -f "$STAMP" ]; then
  newer=0
  for f in "${files[@]}"; do
    [ "$f" -nt "$STAMP" ] && { newer=1; break; }
  done
  [ "$newer" -eq 0 ] && exit 0
fi

echo "[format-on-stop] ${#files[@]} 個の変更ソースを検出、./format を実行します"
if ./format >/dev/null 2>&1; then
  touch "$STAMP"
  echo "[format-on-stop] 完了"
else
  echo "[format-on-stop] ./format が失敗しました。手動で実行して確認してください" >&2
fi
exit 0
