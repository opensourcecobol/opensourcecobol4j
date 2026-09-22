---
paths:
  - "tests/cobol85/**"
---

# COBOL85 テストを扱うときの注意

- **`tests/cobol85/summary.txt`は陳腐化している**: 期待値（IC 24プログラム、NC 92等）は`report.pl`にIC222A〜IC237AやNC127A/NC219A等のskipが追加される前のもので、現在の実行結果（IC 13、NC 90）と一致しない。`make diff`は事実上使えない。
- **結果の検証方法**: summary.txtとの比較ではなく、report.txt同士の直接diffか`Successfully executed: (100.00%)`で判断する。CIも`Successfully executed: (100.00%)`のチェックとsummary.logの最終行のawk検査だけを使っている。
