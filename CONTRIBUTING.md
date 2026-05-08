# Contributing to hanalyze

開発・貢献の手順とコーディング規約をまとめています。Issue / PR は
[github.com/frenzieddoll/hanalyze](https://github.com/frenzieddoll/hanalyze)
からどうぞ。

## ビルドとテスト

```bash
cabal build all      # ライブラリ + 全実行ファイル
cabal test           # テストスイート (hspec)
```

## ブランチ運用

```
master ← develop ← feature/<name>
```

- 新機能は `git switch develop && git switch -c <name>` で派生
- `feature/*` 上で論理単位ごとに commit
- 完了後: `git switch develop && git merge --ff-only <name>` → ローカルブランチ削除
- `develop → master` の merge は明示許可後に fast-forward + version tag (例: `v0.1.0`) を打つ
- feature 中に develop が進んだ場合は `git merge develop` で取り込む (rebase は使わない)
- 緊急 hotfix は `master` から `hotfix/*` を切り、修正後 master と develop の両方に merge

## コーディング規約

### 性能要件

1. **計算 hot path での Haskell リスト経由は禁止**。`[Double]` を per-iteration の演算で使ってはならない。
   `LA.Vector Double` / `LA.Matrix Double` を使う。コスト無視できる boundary
   (`fromList` / `toList` を 1 回) のみ許可。
2. **Mutable Vector の使用は限定的に**。`Data.Vector.Storable.Mutable`
   は以下の **両方** を満たす場合のみ使う:
   - immutable 表現がプロファイルでボトルネックになっている (allocation 量 / GC 時間が支配的)
   - アルゴリズム的に in-place 更新で本質的な改善 (O(n) → O(1) 等) が出る

   単に「速そう」程度の動機での使用は禁止 (Haskell の意義が薄れる)。
3. **最適化改善のフロー**: 計測 → 原因特定 (source / 文献を引用) → 案提示
   (案 A/B/C のプロコン表) → 実装。

### ドキュメント

- 機能追加 / 更新は **README/docs 更新とセット** で commit する。
- `README.ja.md` (ja が正) → `README.md` 同期 → `docs/01-quickstart.{ja,}.md`
  の早見表 → `docs/<genre>/` の詳細を更新する。

## ベンチマーク

- 場所: `bench/haskell/` (Haskell) と `bench/python/` (Python 比較)
- 計測時は **必ず** `OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1` を付ける
  (single-thread fairness)
- 多目的最適化 (MO) は `bench/python/bench_mo.py` が pymoo で hanalyze 側
  Pareto も評価する
- 結果サマリは [`bench/results/SUMMARY.md`](bench/results/SUMMARY.md)

新規ベンチを追加する場合:

- Haskell 側: `bench/haskell/Bench<Name>.hs` を作り `hanalyze.cabal` に
  `executable bench-<name>` の stanza を追加
- Python 側: `bench/python/bench_<name>.py` で同条件を実装

## テスト追加

- `test/Spec.hs` に hspec 形式で追加
- 新しい数値アルゴリズムは reference 値 (Python / R / 文献) との一致を
  確認する例を含める

## License

BSD-3-Clause License — 詳細は [LICENSE](LICENSE) を参照。
