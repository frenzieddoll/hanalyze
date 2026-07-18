# Plate 記法 (Hanalyze.Model.HBM)

> Phase 40 (2026-05-30) で導入。 Pyro / NumPyro 流の plate-block 糖衣で
> 既存 do-block の中に `plate "name" n $ ...` を入れるだけで、
> `buildModelGraph` が出す DAG に PyMC `model_to_graphviz` 相当の
> plate (角丸長方形 + サイズ数字) が反映される。

## なぜ plate か

階層モデル (8-schools / random intercept GLMM / 多レベル混合効果) は
indexed RV (`eta_0, eta_1, …, eta_{n-1}`) の **繰り返し** で構成される。
これら全部を個別ノードで描くと DAG が爆発する (n=1000 で 1000 ノード)。
**plate 記法** は「同じ分布 + 同じ親集合の繰り返しを 1 つの抽象ノードに集約」
する慣習で、 PyMC / Pyro / Stan の文献図と同じ抽象度になる。

## API (3 つ)

```haskell
-- bracket: do-block の任意領域を plate に
plate :: Text -> Int -> Model a r -> Model a r

-- 利便糖衣: plate name n (forM [0..n-1] f) と同等
plateI :: Text -> Int -> (Int -> Model a r) -> Model a [r]

-- alias (= plate、 低レベル primitive)
withPlate :: Text -> Int -> Model a r -> Model a r
```

すべて `Hanalyze.Model.HBM` から export。

## 基本例: 8-schools

```haskell
import qualified Hanalyze.Model.HBM as HBM
import qualified Data.Text as T
import Control.Monad (forM)

eightSchools :: HBM.ModelP ()
eightSchools = do
  mu  <- HBM.sample "mu"  (HBM.Normal 0 5)
  tau <- HBM.sample "tau" (HBM.HalfCauchy 5)
  etas <- HBM.plate "school" 8 $ forM [0..7 :: Int] $ \j ->
            HBM.sample ("eta_" <> T.pack (show j)) (HBM.Normal 0 1)
  _ <- HBM.plate "school" 8 $ forM_ [0..7 :: Int] $ \j ->
         HBM.observe ("y_" <> T.pack (show j))
                     (HBM.Normal (mu + tau * (etas !! j)) 1)
                     [ys !! j]
  return ()
```

DAG 抽出後の `mgPlates` には `Map.fromList [("school", 8)]` が入り、
`eta_*` / `y_*` ノードの `nodePlates` は `["school"]`、 `mu` / `tau` は `[]`。

### 描画 (2 モード)

hanalyze は 2 つの描画モードを提供:

- **expanded** (`buildModelGraph` の結果をそのまま渡す): plate 内に
  全 N 個ノード列挙 (eta_0..eta_7、 デバッグ向け)
- **collapsed** (`collapseIndexedPlateNodes` を 1 段適用): plate 内の
  `<prefix>_<digit>` パターンを **代表 1 ノードに集約** → **PyMC
  `pm.model_to_graphviz` と同等**

```haskell
import qualified Hanalyze.Model.HBM       as HBM
import qualified Hanalyze.Viz.ModelGraph  as VMG
import qualified Hanalyze.Viz.ModelGraphDot as VMGD

main = do
  let g  = HBM.buildModelGraph eightSchools          -- expanded
      gc = HBM.collapseIndexedPlateNodes g           -- collapsed (PyMC 同等)
  VMG.renderModelGraph "8schools.html"      "8 schools (expanded)"  g
  VMG.renderModelGraph "8schools-pymc.html" "8 schools (collapsed)" gc
  VMGD.writeModelGraphDot "8schools.dot"      g
  VMGD.writeModelGraphDot "8schools-pymc.dot" gc
  -- $ dot -Tpng 8schools-pymc.dot -o 8schools-pymc.png
```

`collapseIndexedPlateNodes` の集約条件 (heuristic):

- 同じ `nodePlates` (plate スタック) に属する
- 名前が `<prefix>_<digit+>$` パターン
- 同じ `prefix` を持つノード群が 2 個以上
- 同じ `nodeDist` (= 分布名一致)

集約結果:

- 代表ノード名は `prefix` (例: `eta_0..eta_7` → `eta`)
- 観測ノードは観測数を全集約 (`y_0..y_7` (各 n=1) → `y (n=8)`)
- edges は集約後の名前で dedupe + 自己ループ除去
- nested plate (school × student) は **不動点で 2 段集約** (内側 →
  外側 の順で代表 1 ノードに収束)

plate 文脈外での「同じ命名規則の名前衝突」 (e.g. `beta_0` 固定効果 vs
`u_0` 群効果) はこの heuristic で **誤って集約されない** (plate 制約)。
非集約 (1 個のみ) / 異なる分布 / 命名規則違いはそのまま残る。

## nested plate (multi-level)

`plate` を入れ子にすれば `nodePlates` に外→内の順でスタックされる:

```haskell
m = do
  _ <- HBM.plate "school" 3 $ forM_ [0..2 :: Int] $ \j ->
         HBM.plate "student" 2 $ forM_ [0..1 :: Int] $ \i ->
           HBM.sample ("y_" <> T.pack (show j) <> "_" <> T.pack (show i))
                      (HBM.Normal 0 1)
  return ()
```

`y_1_0` の `nodePlates = ["school", "student"]`、 mermaid / dot は
nested subgraph (school の中に student) で出力。

## crossed plate

「subject × time」 のような完全交差は **PyMC でも 2 plate 並列描画が標準**。
hanalyze も同じ慣習に従う:

```haskell
m = do
  _ <- HBM.plate "subject" 3 $ forM_ [0..2 :: Int] $ \s ->
         HBM.sample ("u_" <> T.pack (show s)) (HBM.Normal 0 1)
  _ <- HBM.plate "time" 2 $ forM_ [0..1 :: Int] $ \t ->
         HBM.sample ("v_" <> T.pack (show t)) (HBM.Normal 0 1)
  return ()
```

`mgPlates` に "subject" と "time" の 2 件、 ノードはそれぞれの 1 plate のみ
属する。

## 既存 helper との合成

`dirichlet` / `nonCenteredNormal` / `ar1Latent` / `glmmRandomIntercept` は
すべて `plate` で **そのまま** 包めば自動的に plate-aware 化する
(B2 設計の利点):

```haskell
-- dirichlet を plate で包む
_ <- HBM.plate "K" 3 $ HBM.dirichlet "pi" [1, 1, 1]

-- ar1Latent を plate で包む
_ <- HBM.plate "T" 100 $ HBM.ar1Latent "x" 100 0.5 1

-- glmmRandomIntercept を plate で包む
_ <- HBM.plate "subject" nGroups $
       HBM.glmmRandomIntercept HBM.GlmmGaussian xs gids ys
```

helper 内部の latent / det はすべて plate メンバとして登録される。

## PyMC との対応表

| PyMC v5 | hanalyze (Phase 40) |
|---|---|
| `pm.Model(coords={"school": 8})` | (不要、 plate 名 + サイズを直接渡す) |
| `eta = pm.Normal("eta", 0, 1, dims="school")` | `etas <- plateI "school" 8 (\j -> sample ("eta_" <> show j) (Normal 0 1))` |
| `pm.model_to_graphviz(model)` | `VMGD.renderModelGraphDot (buildModelGraph m)` |
| 角丸長方形 + 右下サイズ数字 | `subgraph cluster_X { labelloc="b"; label="X × N"; ... }` |
| nested plate (rectangles) | nested `subgraph cluster_*` |
| crossed plate (overlapping) | 2 別 plate (PyMC でも完全交差描画は非対応) |

## サンプラへの影響

**ない**。 plate は **描画レイヤーのみの抽象** で、 `logJoint` / `logPrior`
/ `logLikelihood` / NUTS / Gibbs / VI などは全て `PlateBegin` / `PlateEnd`
を **透過的に pass-through** する。 NUTS の連続変数 latent も plate の
有無で振る舞いは不変。

## 罠 (Phase 38 + 40 で確立)

1. **`plate` の name と内側 RV の name は別物**: plate 名は `"school"`、
   個別 RV 名は `"eta_0", "eta_1", …`。 mermaid `subgraph plate_<name>`
   の `<name>` は plate 名側
2. **nested plate の `nodePlates` 順は外→内**: `["school", "student"]` で
   入れ子 cluster の正しい入れ子順を保証
3. **同名 observe の自動 merge と plate の整合**: `mergeByName` は
   最初の出現の plate を維持。 plate 内で同名 observe を複数回呼ぶと
   1 ノードに統合、 plate は最初のものになる (通常は別名 `y_0, y_1, ...`
   推奨)
4. **`PlateBegin` 抜きの `PlateEnd` は無視**: スタックが空のときは
   pop しない (defensive)
5. **既存 `forM` パターンとの違いは plate name の有無のみ**: コードを
   plate に書き換えるコストはゼロ (`forM` を `plate "name" n $ forM` に
   置き換えるだけ)
6. **大規模 plate (N=1000) でも 1 plate ノード**: mermaid / dot 出力は
   plate メンバ全てを subgraph に列挙するので O(N) lines。 集約描画を
   true plate (1 アイコン) で出すには将来的に renderer の拡張が必要

## ガラリ (gallery)

主要モデルの mermaid + dot 例は [`dag-gallery.ja.md`](dag-gallery.ja.md)
の Phase 40 セクション (将来追記) を参照。
