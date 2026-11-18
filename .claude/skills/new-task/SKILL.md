---
name: new-task
description: 新しい作業をworktreeで開始し、CIループとレビューを回してPRまで持っていく一連の流れ
---

# new-task

1. `/wt` で作業用ブランチとworktreeを作る。
2. `$ARGUMENTS` で指定された作業をする。
3. 作業は `/ci-loop` に従って進める。
4. 一通り作業を終えたら `code-reviewer` エージェントでレビューし、問題点があれば再度 `/ci-loop` に従って作業する。
5. その後 `/local-pr`（自分のフォークで確認）または `/pr`（上流へ提出）を実施する。
