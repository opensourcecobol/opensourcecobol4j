---
name: pr
description: 上流リポジトリ(opensourcecobol/opensourcecobol4j)のdevelopブランチにDraft Pull Requestを出す
---

# pr

上流リポジトリ https://github.com/opensourcecobol/opensourcecobol4j の `develop` ブランチに
Draft Pull Request を出す。

- PRのタイトルは英語で、本文は前半を日本語・後半を英語で書く。
- タイトルにも本文にも、「Claudeが書いた」といった記述は一切入れない。
- `$ARGUMENTS` に `not-draft` が指定されていたら、Draftではなく通常のPull Requestを作成する。

## 注意

- 自分のブランチが `develop` から離れているとCIが通らないことがある。PRを出す前に上流の `develop` を取り込んでおく。
- それでもCIが落ちる場合は、上流の `develop` に入っている修正がまだ手元のブランチに無い可能性がある。
  `ci-triage` エージェントで失敗の原因を切り分け、必要な変更を取り込んでから出し直す。
