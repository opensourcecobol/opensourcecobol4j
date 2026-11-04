---
paths:
  - "libcobj/**"
---

# libcobj (Java) を編集するときの注意

- **CIのPMDはコメントのみのcatchブロックを弾く**: static-analysisジョブのPMD `EmptyCatchBlock`は`allowCommentedBlocks`が無効なため、`catch (E e) { /* コメント */ }`はビルド失敗になる。意図的に例外を握りつぶすcatchには既存スタイルに合わせて`System.err.println("Failed to ...")`を入れる（例: `CobolIndexedFile.commitJdbcTransaction`）。
- **ローカルでの静的解析**: `cd libcobj && ./gradlew pmdMain spotbugsMain spotlessCheck`（sandbox無効化が必要）。
- **フォーマット**: トップディレクトリの`./format`が`./gradlew spotlessApply`を呼ぶ。Stopフックで自動実行される。
