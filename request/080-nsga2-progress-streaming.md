# 080: NSGA-II の進捗ストリーミング

## 背景

HPotfire 側で /api/optimize を叩くと NSGA-II が完了するまで応答が
返らない。世代数 × pop が大きいと数分待つ場合があり、UI 側は
"loading…" だけで進捗が見えない。フロント側で「進捗バー」を出したいが、
現状の hanalyze API は同期(`nsga2 :: NSGAConfig -> ... -> IO [Solution]`)
で、世代の途中状態を流す手段がない。

## 提案

`Hanalyze.Optim.NSGA` に **コールバック付きの API** を追加する。

```haskell
data NSGAProgress = NSGAProgress
  { ngpGeneration   :: !Int       -- 0-based 現世代
  , ngpTotal        :: !Int       -- 総世代数
  , ngpParetoSize   :: !Int       -- 現 rank0 サイズ
  , ngpBestObjs     :: ![Double]  -- rank0 中で各目的の最小値
  } deriving (Show)

-- | コールバックを毎世代終端で呼ぶ版。コールバックが IO () なので
-- |  Servant 側で chan に push して SSE で投げる、というのが想定の使い方。
nsga2WithProgress
  :: NSGAConfig
  -> ([Double] -> [Double])        -- objective
  -> ([Double] -> Double)           -- constraint violation(常に >= 0)
  -> [(Double, Double)]             -- bounds
  -> (NSGAProgress -> IO ())        -- 毎世代の進捗コールバック
  -> GenIO
  -> IO [Solution]
```

コールバック未指定なら `nsga2` / `nsga2WithConstraints` は今まで通り
動くこと(後者を `nsga2WithProgress _ _ _ _ (const (pure ())) _` で
実装し直して良い)。

## 利用側(HPotfire)の設計メモ

- backend 側で `/api/optimize` を SSE / chunked transfer に変える
  (もしくは `/api/optimize/stream` を分ける)
- フロントは `EventSource` で `progress` イベントを受けて
  loading 表示を「100 世代中 47 世代(rank0=23)」のような表示にする
- 完了時に最後の `done` イベントで結果 JSON を送る

## 優先度

低(機能としては今動いている)。UX 改善目的なので、上位の
モデル機能(GP/MGP/GLMM)が落ち着いたら戻ってくる。

## 関連

- hanalyze: `src/Hanalyze/Optim/NSGA.hs` (`nsga2`, `nsga2WithConstraints`)
- HPotfire backend: `backend/src/HPotfire/Optimize.hs::runOptimize`
- HPotfire frontend: `src/App/Types.purs::OptimResultState` で
  `OptimLoading` を `OptimProgress { gen, total, paretoSize }` に
  拡張する想定
