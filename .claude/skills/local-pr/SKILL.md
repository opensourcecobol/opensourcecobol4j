---
name: local-pr
description: 自分のフォークのmainブランチに対してDraft Pull Requestを出す（上流に出す前の自己確認用）
---

# local-pr

自分のフォーク（`git remote -v` で確認する。上流の opensourcecobol/opensourcecobol4j ではない方）の
`main` ブランチに対して Draft Pull Request を出す。上流へ出す前に、自分のリポジトリのCIで確認するための手順。

- PRのタイトルは英語で、本文は前半を日本語・後半を英語で書く。
- タイトルにも本文にも、「Claudeが書いた」といった記述は一切入れない。
- `$ARGUMENTS` に `not-draft` が指定されていたら、Draftではなく通常のPull Requestを作成する。
