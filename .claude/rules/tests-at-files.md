---
paths:
  - "tests/**/*.at"
  - "tests/*.src/**"
---

# autotest の .at ファイルを扱うときの注意

- **一部の`.at`はShift_JISでエンコードされている**: `esql-basic.src/`、`esql-cobol-data.src/`、`esql-misc.src/`、`esql-sqlca.src/`の日本語データを含むもの、および`i18n_sjis.src/`など。`esql-utf8.src/`はUTF-8。
  - Edit/Writeツールはファイルを UTF-8 として読み書きするため、SJISのバイト列がU+FFFDに置換されて全行が壊れる。「1行だけ直したはず」なのに数十行の差分になる。
  - 編集はPython等で `open(f,'rb').read().decode('latin-1')` → 置換 → `.encode('latin-1')` で書き戻す（latin-1はバイト列を無損失で往復できる）。編集後は`git diff --numstat`で差分行数が想定どおりか必ず確認する。
- **grepには必ず`-a`を付ける**: SJISのバイト列があるとgrepはこれらをバイナリと判定し、マッチしても何も出力せずエラーも出さずに終了する。「該当なし」と誤認する（2026-08-08に`esql-cobol-data.src/varying.at`の`ORDER BY EMP_NAME`を見落として誤った結論を出した実績がある）。
- **テストの実行**: `.at`を修正したら`make {テスト名}`でスクリプトを再生成してから`tests/`で`./{テスト名}`を実行する。長くなる場合は関係ない部分を`#`でコメントアウトしてから再生成する。失敗の記録は`{テスト名}.dir/`にテスト終了後も残る。
