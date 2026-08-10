# D-optimal による実験の追加 (Augment Design)

対象 module: `Hanalyze.Design.Optimal` (`hanalyze-design`)

「すでに走らせた実験は無駄にせず、 あと N run だけ足して精度を上げたい」
ときに使う。 既存行を**固定したまま**候補集合から N 点を選び、
完成 design (既存 + 追加) の最適性規準を最大化する Fedorov 交換法。

計画を新規に建てる `dOptimal` / `aOptimal` との違いは、 **既存行が swap の
対象にならない**点。 途中で仕様が変わった / 想定外の交互作用が見えた、
といった現場の追加実験に対応する。

Custom Design のメニュー経由 (`AddRuns` 等) で使う場合は
[usage-augment-splitplot.ja.md](usage-augment-splitplot.ja.md) を参照。
本 doc はその下地になる低レベル API を扱う。

## 0. API

```haskell
data AugmentResult = AugmentResult
  { arNewIndices  :: ![Int]         -- 選ばれた候補の index
  , arNewRows     :: ![[Double]]    -- 追加点の実値
  , arFullDesign  :: ![[Double]]    -- 既存 ++ 追加 (既存の順序は保持)
  , arInitialCrit :: !Double        -- 既存単独の criterion 値
  , arFinalCrit   :: !Double        -- 完成 design の criterion 値
  }

augmentDesign
  :: OptCriterion    -- DOpt / AOpt / IOpt / EOpt / GOpt / …
  -> [[Double]]      -- 既存行 (固定)
  -> Int             -- N (追加する行数)
  -> [[Double]]      -- 候補集合
  -> Int             -- seed
  -> AugmentResult

candidateGrid :: Int -> Int -> [[Double]]           -- k 因子 × numLevels 水準
quadraticCandidates :: Int -> Int -> [[Double]]     -- 2 次モデル行に展開済
```

行はすべて **model 行 (model matrix の 1 行)** として渡す。 1 次モデルなら
`[1, x1, x2]`、 2 次モデルなら `[1, x1, x2, x1², x2², x1·x2]`。 切片列を
自分で入れる点に注意 (`quadraticCandidates` は展開済の行を返す)。

乱数は **`Int` seed のみ** (内部の LCG `pseudoShuffle`)。 `MWC.GenIO` は
取らず純粋関数なので、 同じ seed なら必ず同じ結果になる。

## 1. 使い方

2 因子 1 次モデルの 4 run (2² 要因計画) を既に走らせた状態から、 3 run 追加する。

```haskell
import Hanalyze.Design.Optimal

main :: IO ()
main = do
  let existing = [ [1, -1, -1], [1, 1, -1], [1, -1, 1], [1, 1, 1] ]
      cands = [ [1, x1, x2] | x1 <- [-1, 0, 1], x2 <- [-1, 0, 1] ]
      r = augmentDesign DOpt existing 3 cands 42
  print (arNewIndices r)
  mapM_ print (arNewRows r)
  print (arInitialCrit r, arFinalCrit r)
  print (length (arFullDesign r))
```

実行結果:

```
[0,6,2]
[1.0,-1.0,-1.0]
[1.0,1.0,-1.0]
[1.0,-1.0,1.0]
(64.0,320.0)
7
```

読み方:

- **`arInitialCrit` = 64 → `arFinalCrit` = 320** — `DOpt` の criterion は
  `det(XᵀX)`。 既存 4 run で `det = 4³ = 64`、 追加後は 320 に上がった
- 選ばれたのは **既存の隅点の反復**。 モデルが飽和 (3 パラメータ / 4 run) して
  いるので、 新しい水準を増やすより端点を反復した方が `det` が伸びる。
  D-optimal は「モデルの係数を精度良く推定する」 規準であり、
  **モデルの妥当性検証 (lack-of-fit) を見たいなら中心点を自分で足す**
- `arFullDesign` は既存 4 + 追加 3 = 7 行。 既存の順序は保たれるので、
  実験順や block 情報を別に持っていても対応が崩れない

## 2. 退避条件

`N ≤ 0` または候補数が N より少ない場合、 **追加なし** (空の結果) が返る。
例外は投げない。

```haskell
  let r0 = augmentDesign DOpt existing 0 cands 42
  print (arNewIndices r0, arFullDesign r0 == existing)
```

```
([],True)
```

`arFinalCrit >= arInitialCrit` は Fedorov 交換の性質から常に成り立つ
(改善する交換しか採用しない)。

## 3. 候補集合を作る

```haskell
  print (candidateGrid 2 3)
```

```
[[-1.0,-1.0],[-1.0,0.0],[-1.0,1.0],[0.0,-1.0],[0.0,0.0],[0.0,1.0],
 [1.0,-1.0],[1.0,0.0],[1.0,1.0]]
```

`candidateGrid k numLevels` は `[-1, 1]` を等間隔に割った格子を返す
(**切片列は含まない**ので、 1 次モデルなら自分で `1 :` する)。
2 次モデルの行まで展開したものが要るなら `quadraticCandidates k numLevels`。

## 4. 規準の選択

| criterion | 意味 | 使う場面 |
|---|---|---|
| `DOpt` | `max det(XᵀX)` | 係数を精度良く推定したい (既定) |
| `AOpt` | `min trace((XᵀX)⁻¹)` | 平均推定分散を下げたい |
| `IOptRegion m` | 領域上の平均予測分散を最小化 | **予測**が目的 (RSM 等) |
| `EOpt` | 最小固有値を最大化 | 最悪方向の精度を保証したい |
| `GOpt` | 最大 leverage を最小化 | 影響の大きい点を作りたくない |
| `BayesianD k` | `max det(XᵀX + K)` | 事前情報あり (DuMouchel-Jones) |
| `Compound ws` | 上記の重み付き和 | 複数目的 (スケールは自分で揃える) |

> 旧 `IOpt` は self-moment 近似のため `p/n` に縮退し設計に依らない。
> I-optimal が要るときは **`IOptRegion`** (領域積分版) を使う
> (`Optimal.hs` の haddock に明記)。

## 5. 使い分け

| やりたいこと | 使うもの |
|---|---|
| 既存実験を固定して N 点足す | `augmentDesign` (本 doc) |
| 候補集合から一から N 点選ぶ | `dOptimal` / `aOptimal` / `optimalDesign` |
| Custom Design のメニューから追加 | [usage-augment-splitplot.ja.md](usage-augment-splitplot.ja.md) |
| 事前分布つき D 最適 | [usage-bayesian-d.ja.md](usage-bayesian-d.ja.md) |
| 規則的な要因計画で足りる | [01-doe.ja.md](01-doe.ja.md) |
| 逐次的に最適点へ寄せる | [usage-sequential-rsm.ja.md](usage-sequential-rsm.ja.md) |

## 関連

- package: [hanalyze-design](../../hanalyze-design/README.ja.md)
- Custom Design の augment メニュー: [usage-augment-splitplot.ja.md](usage-augment-splitplot.ja.md)
- Bayesian D-optimal: [usage-bayesian-d.ja.md](usage-bayesian-d.ja.md)
- 低レベル API 一覧: [internal/09-doe-lowlevel.md](../internal/09-doe-lowlevel.md)
