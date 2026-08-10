# hmatrix `accum` の引数順序が doc から判別できない件

**ステータス**: 既知の使いにくさ (バグではない、 ドキュメント改善案件)。
upstream (`haskell-numerics/hmatrix`) に PR / issue を出す候補。

**確認した版**: hmatrix-0.20.2 (`src/Internal/Numeric.hs`)
**hanalyze 内での該当遭遇**: 2026-05-29、 Phase 24-3 Coordinate Exchange
実装 (`src/Hanalyze/Design/Custom/Coordinate.hs`) で 1 セル置換を
`LA.accum m (\_ new -> new) [((i, j), v)]` と書いたが、 値が置換されず
test が失敗。 debug の結果、 combining function の引数順は **`f listValue oldValue`**
で、 私の lambda は old を返していたことが判明。

## 事実 (源コード確認)

`Numeric.LinearAlgebra.Data.accum` は以下のシグネチャ:

```haskell
accum :: Container c e => c e -> (e -> e -> e) -> [(IndexOf c, e)] -> c e
```

doctest 例 (`src/Internal/Numeric.hs:530`):

```haskell
>>> accum (ident 5) (+) [((1,1),5),((0,3),3)] :: Matrix Double
```

実装本体 (`src/Internal/Numeric.hs:855`):

```haskell
accumM m0 f xs = ST.runSTMatrix $ do
        m <- ST.thawMatrix m0
        mapM_ (\((i,j),x) -> ST.modifyMatrix m i j (f x)) xs
        return m
```

`ST.modifyMatrix m i j g` は `m[i,j] <- g m[i,j]` 相当。 ここで `g = f x`
(x はリスト値) なので、 結果は **`m[i,j] <- f x m[i,j]`**。

つまり combining function の引数順序は:

- **第 1 引数 = リスト側の値 (新しい値)**
- **第 2 引数 = matrix の現行値 (古い値)**

## なぜ doc から判別できないか

doc コメント (`src/Internal/Numeric.hs:528`):

```
-- | Modify a structure using an update function
```

「update function」 としか書かれておらず、 引数の役割が無い。 doctest 例も
`(+)` (可換) と `(map (flip (,) 1) ...)` (histogram、 `(+)`) の 2 例だけで、
**両方とも可換関数のため引数順序が判別不能**。

## 「置換」 のイディオム

値を **そのまま置換** する場合は `const` を使う:

```haskell
-- セル (i, j) を v で上書き
LA.accum m const [((i, j), v)]
```

`const :: a -> b -> a` = `\x _ -> x`、 第 1 引数 (リスト値) をそのまま返す。

hanalyze 内では `src/Hanalyze/Design/Custom/Coordinate.hs::setEntry`
(Phase 24-3) でこのイディオムを使っている。

## upstream に提案したい改善

1. **docstring に引数順序を明記**: `(e -> e -> e)` を
   `(newValue -> oldValue -> resultValue)` のような疑似シグネチャに変える、
   または 1 行コメント追加。
2. **非可換な doctest 例を 1 つ追加**: 例えば
   `accum (konst 0 3) (-) [(0, 10)] :: Vector Double` の期待結果が
   `[10, 0, 0]` (= `10 - 0`) か `[-10, 0, 0]` (= `0 - 10`) かが doc から
   判別できる例。
3. **`replace` / `setAt` のような置換専用ヘルパを export**: ユーザが順序を
   気にせず使える `replace m [((i, j), v)] = accum m const xs` 相当。

## 報告先候補

- GitHub: <https://github.com/haskell-numerics/hmatrix>
- 該当ファイル: `packages/base/src/Internal/Numeric.hs` (mono-repo 構成、
  確認した version は 0.20.2 で src/Internal/Numeric.hs)

## 影響範囲 (hanalyze 内)

- `src/Hanalyze/Design/Custom/Coordinate.hs::setEntry` のみ。 修正済 (`const`)。
- 他の使用箇所 (2026-05-29 grep):
  - `src/Hanalyze/Optim/NSGA.hs:775, 879, 921` — 3 箇所、 いずれも
    `LA.accum acc (+) ...` (可換関数)。 順序不依存のため影響なし。
