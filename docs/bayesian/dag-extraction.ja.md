# DAG 抽出 — 制約と回避策 (Phase 38)

`Hanalyze.Model.HBM.buildModelGraph` / `extractDeps` の挙動と、 ユーザ
モデル / helper を書く際に踏みやすい罠を、 Phase 38 検証で見つかった
事例とともにまとめる。

## 概要: DAG 抽出の仕組み

- `extractDeps` は `Model Track` 型でモデルを走査する。
- `Track` は `(値, 触れた latent 名集合)` のペア (HBM.hs:2167)。
- 各 `Sample n d` ノードの親は `distDepsT d` で `d` の引数 Track の deps
  和。 そして継続には `trackVar n 1.0` (deps = {n}) を渡す。 → 下流の
  Track 操作は **遠い親 (mu, tau) ではなく n** を deps として保持する。
- 各 `Observe n d ys` ノードの親も `distDepsT d`、 観測数は `length ys`。
- 各 `Deterministic n v` ノードの親は `trackDeps v`。 **Phase 38 修正後**:
  継続には `Track (trackVal v) {n}` を渡し、 下流はやはり n を親に取る。
- 各 `Potential n v` ノードの親は `trackDeps v` (LatentN として記録、
  `nodeDist = "Potential"`)。
- 同名 `Observe n` の重複は `mergeByName` で 1 ノードに統合される
  (観測数合算、 親集合は和)。

## 罠リスト

### 罠 1: `distDepsT` が新分布で非網羅 (Phase 37 → 38 で実害)

**症状**: `buildModelGraph` がモデル内で `SkewNormal` / `OrderedLogistic`
/ `MvStudentT` 等を見た瞬間に `Non-exhaustive patterns in function
distDepsT` で runtime crash。

**原因**: `distDepsT` (HBM.hs:2257-) に Phase 37 で追加した 11 分布の
case が無かった。

**Phase 38 で修正**: 11 case 追加済。 今後新分布を `Distribution` に
追加する際は **`distDepsT` への追加を忘れない**:

```haskell
distDepsT (NewDist a b c) = trackDeps a <> trackDeps b <> trackDeps c
-- 引数が Int 等の non-Track のみなら mempty で OK
distDepsT NewDist{} = mempty   -- 例: DiscreteUniform / HyperGeometric
```

### 罠 2: Deterministic 透過 (Phase 38 で修正済)

**症状**: `nonCenteredNormal "theta" mu tau` の `theta` (det 値) を
`observe "y" (Normal theta 1)` に渡しても、 DAG 上で `y` の親が
`{theta}` ではなく `{mu, tau, theta_raw}` (theta の遠い親) になる。

**原因 (修正前)**: `extractDeps` の `Deterministic` handler が継続に
元の Track `v` をそのまま渡しており、 deps が relabel されない。

**Phase 38 修正**: 継続に `Track (trackVal v) (Set.singleton nm)` を
渡すように変更。 値は保持しつつ deps を det 名で再ラベル。

```haskell
go (Free (Deterministic nm v k)) acc =
  let parentDeps = trackDeps v
      node = Node nm LatentN "Deterministic" parentDeps
      v'   = Track (trackVal v) (Set.singleton nm)
  in go (k v') (node : acc)
```

### 罠 3: Helper が deterministic を「副作用として捨てる」 と plate にならない

**症状**: `ar1Latent "x" 3 0.8 0.3` で返した `xs` を使うと、 各 `x_t`
の親が `{x_raw0, …, x_raw_t}` という遠い親集合になり、 plate-style の
chain `x_{t-1} → x_t` が出ない。

**原因 (修正前)**: ar1Latent が以下の構造だった:

```haskell
let xs = scanl (\xPrev (rt, _) -> phi*xPrev + sigma*rt) x0 (zip ...)
_ <- mapM (\(t, x) -> deterministic (...) x) (zip [0..] xs)
return xs   -- xs は pure な Haskell scanl 結果。 各要素は full chain deps
```

`scanl` が **pure 計算**で `xs` を構築するため、 後で `deterministic`
登録しても deps の relabel は手遅れ。 各 `x_t` の Track は構築時の
deps をそのまま保持。

**Phase 38 修正**: monadic recursion に書き直し、 各 step で
deterministic の戻り値 (det 名で relabel された Track) を次 step の
`xPrev` に渡す:

```haskell
x0 <- deterministic (name <> "_0") (stat * head raws)
let chain _ [] = return []
    chain xPrev ((t, rt):rest) = do
      xt <- deterministic (name <> "_" <> ...) (phi * xPrev + sigma * rt)
      xs' <- chain xt rest
      return (xt : xs')
xs' <- chain x0 (zip [1..] (tail raws))
return (x0 : xs')
```

**一般原則** (今後 helper を書く際):

- helper の最終ステップが `deterministic` の戻り値ならば OK
  (`nonCenteredNormal` は最後の `deterministic name (loc + scale * raw)`
   を return) 。
- helper が `mapM (\(i, p) -> deterministic ...) xs` で締める場合、
  **mapM の戻り値** を return すれば OK (`dirichlet` がこの形)。
- helper 内で deterministic 結果を `_ <-` で捨てて元の純計算リストを
  返すパターンは **DAG 抽出を壊す**。

### 罠 4: `ModelP r` は rank-2 → パターン束縛できない

**症状**:

```haskell
let m :: HBM.ModelP () = do { … }
```

がコンパイルエラー: `Couldn't match expected type 'forall a. ...' with
'Free (ModelF Double) ()'`。

**原因**: `type ModelP r = forall a. (Floating a, Ord a) => Model a r`
は rank-2 型。 pattern binding の中で polymorphic 化されないため、 単型
として推論される。

**回避**: 型シグネチャと束縛を分ける (関数式束縛):

```haskell
let m :: HBM.ModelP ()
    m = do { … }
```

test/Spec.hs / demo はすべてこの形で書くこと。

### 罠 5: 同名 observe の合算 (`mergeByName`)

**仕様**: `forM_ [0..n-1] $ \i -> observe "y" (Normal mu 1) [ys !! i]` の
ように同名 observe を繰り返すと、 `buildModelGraph` が 1 ノードに統合
する:

- `nodeKind = ObservedN (n_1 + n_2 + …)` (観測数合算)
- `nodeDeps = ∪ (それぞれの parent 集合)`

これは N=1000 でも観測ノードが爆発しないための設計だが、 **異なる y_i に
個別の親が居る場合は名前を分けないと親が混ざる**。 例えば GLMM helper
の glmmRandomIntercept は `y_0, y_1, …` のように 1 観測 1 ノードで分けて
いるため、 各 y_i の親 `u_g(i)` (群効果) が正しく分離される。

### 罠 6: `Categorical` / `Mixture` 等の引数リストは全要素が親

`distDepsT (Categorical ps) = mconcat (map trackDeps ps)`、
`distDepsT (Mixture ws ds) = mconcat (map trackDeps ws) <> mconcat (map distDepsT ds)`
であることに注意。 `dirichlet "p" α` で返された `pis = [p_0, p_1, p_2]`
を `observe "y" (Categorical pis) ys` に渡すと、 `y` の親は
`{p_0, p_1, p_2}` (det 全部) になる。 これは plate-style として正しい
挙動。

### 罠 7: ローカル変数を観測の `eta` に組み立てる場合の deps 伝播

GLMM 系で `eta = beta_0 * x + u_g + …` のように parents を線形結合する
場合、 `eta` は Haskell の値 (Track) で、 Track の `+`/`*` が deps の
和集合を保持する (HBM.hs:2185-2196)。 したがって `observe "y_i"
(Normal eta sigma) [y]` の親は `{beta_0, …, u_g, sigma}` 等、 eta を
構成した全 latent になる (これは期待通り)。

ただし **`realToFrac`** を介すと Track の deps が失われるとは限らず、
HBM.hs:2218 の `Real Track` インスタンスが `toRational . trackVal` で
deps を捨てる。 `realToFrac (xs !! i) :: Double` のような変換を間に挟む
と DAG が切れる。 helper 内で `map realToFrac xRow :: [a]` のように
Track が走るパス (a = Track のとき) では `realToFrac` 経由でも
fromInteger / fromRational 経路で Track が再構築されるので問題ない。
**境界**: Track → Double の明示変換は **deps を消す**、 Double → Track
は `trackConst` で **deps なしの定数**として扱われる。

## helper を書くチェックリスト (DAG-safe)

新しい階層 helper を書くときの確認項目:

1. helper の戻り値は **deterministic / sample の戻り値** か?
   pure 計算した値を返していないか?
2. helper 内で間接的に `_ <- mapM (\… -> deterministic …) ` のように
   deterministic 結果を捨てて元値を return していないか?
3. 新しい `Distribution` コンストラクタを足したら `distDepsT` /
   `distName` / `logDensity` (および関連の `logDensityObs` / `obsLogSum`
   / sample) **すべて** に case を追加したか?
4. helper を使ったモデルで `buildModelGraph` の `mgEdges` を検証する
   test を 1 つ書いたか? (回帰防止)

## Phase 40 で追加された罠 (plate 関連)

### 罠 8: PlateBegin 抜きの PlateEnd は無視 (defensive)

`extractDeps` の plate 文脈 stack は LIFO。 `PlateEnd` 単独 (空 stack
で pop) は **黙って無視**。 これは plate を手動で `liftF` した
ユーザコードがバランス崩した場合の防御策。 通常 `plate name n body`
helper を使えば bracket が保証される。

### 罠 9: 同名 plate を異なるサイズで使うと最後の値で上書き

```haskell
do _ <- plate "g" 3 $ ...
   _ <- plate "g" 8 $ ...   -- mgPlates の "g" は 8 になる
```

これは `Map.insert` 上書きの帰結。 通常は同名 plate は同サイズ前提だが、
誤って異なるサイズで書くと検出されない。 将来的に warn を出すかは
ユーザ要望次第。

### 罠 10: plate 内同名 observe の merge は最初の plate を維持

`mergeByName` は最初の出現 (deps 和 + observation 数加算) を残す。
plate 内で同名 observe を複数回呼ぶと **1 ノードに統合**、 plate stack
は最初のものになる。 これは plate 外の同名 observe も同様。

→ 推奨: plate 内で個別 observe する場合は `y_0, y_1, …` のように
別名にする (Phase 38 罠 5 と整合)。

## Phase 63 で追加された罠 (データ slot 関連)

### 罠 11: dataNamedObs の obs→slot エッジは「値一致」 ヒューリスティック

`dataNamedObs` の snd view (生 `[Double]`) には `Track` タグを流せない
(observe の ys は素の Double 列) ため、 `extractDeps` は walk 終端で
**obs 名ごとの連結 ys と slot の生値の完全一致** で obs→slot エッジを張る
(PyMC `make_compute_graph` の `obs -> y` 同型 = slot は obs の**子**として
obs の下に描かれる)。 per-point loop (`observe "y" … [y]` を N 回) も
連結すれば一致する。 帰結:

- **偶然の同値で誤エッジ**: x slot の値が observe の ys とたまたま完全一致
  すると、 x slot にも obs→slot エッジが張られる (Phase 60.6 の plate
  「長さ一意 match」 と同種の、 表示専用ヒューリスティックの限界)
- 同値の slot が複数あれば**全部に**エッジが張られる
- slot の値を**変換してから** observe に渡すと一致せずエッジは出ない
  (slot はエッジゼロ → source rank に描かれる)
- `dataNamedObs "y"` + `observe "y"` の**同名慣例**では `mergeByName` の
  統合が優先 (データ容器が観測ノードに吸収される)。 値一致エッジは
  同名を対象外にする (自己ループ回避)

→ 推奨: docs 慣例どおり **slot と observe を同名**にする (容器吸収表示) か、
別名にする場合は slot の値を未変換のまま observe に渡す。

## 参照

- 実装: `src/hanalyze/Analyze/Model/HBM.hs`
  - `extractDeps` (line 2228-)、 `distDepsT` (line 2257-)、
    `Deterministic` handler (line 2247-)
  - `ar1Latent` (line 1683-)、 `nonCenteredNormal` (line 1719-)、
    `dirichlet` (line 1809-)、 `glmmRandomIntercept` (line 1750-)
- 検証 test: `test/Spec.hs` の `describe "(Phase 38: …)"` 3 ブロック
  (簡単 6 / 代表 9 / 複雑 9)
- DAG ギャラリー (mermaid): `docs/bayesian/dag-gallery.ja.md`
- Phase 38 計画: `specification/phases/phase-38-model-dag-verification.md`
- Phase 40 plate 記法ガイド: `docs/bayesian/plate-notation.ja.md`
- Phase 40 計画: `specification/phases/phase-40-plate-notation.md`
