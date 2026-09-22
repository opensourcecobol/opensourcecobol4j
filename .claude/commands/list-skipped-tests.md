# list-skipped-tests

SKIP_TESTでスキップされているテストの一覧を表示する。

## 実行手順

1. `tests/**/*.at` ファイルから `SKIP_TEST` を含むテストを検索する
2. 各テストについて以下を抽出する：
   - テストファイルパス
   - テスト名（`AT_SETUP([...])` から取得）
3. 一覧を整形して表示する

## 表示形式

以下の形式で一覧を表示する：

```
# SKIP_TESTでスキップされているテスト一覧

## data-rep.src
1. pointer.at - POINTER: display
2. binary.at - BINARY: 2-4-8 little-endian
3. binary.at - BINARY: 1-2-4-8 little-endian
...

## run.src
1. miscellaneous.at - LOCAL-STORAGE
2. miscellaneous.at - CALL binary literal parameter/LENGTH OF - so
...
```

## 使用例

この一覧を使って、`/setup-worktree` でworktreeを作成し、`/fix-skipped-test` で修正を開始できる。
