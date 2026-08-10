{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Hanalyze.Model.SVM
-- Description : SMO ソルバによる双対形カーネル SVM (C-SVC)
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- カーネル SVM (双対形・SMO ソルバ) — Phase 75.11 / 共有 Kernel 化 Phase 75.15。
--
-- 双対 C-SVC (hinge 損失) を SMO (Platt 1998) で解き、 共有カーネル語彙
-- ('Hanalyze.Model.Kernel': Linear/Poly/RBF/Matern52/Periodic) と
-- **スパースな真のサポートベクタ** (α>0 の点) を提供する。 既定カーネルは Linear で、
-- 線形 SVM が必要なら kernel=Linear・非線形は RBF/Poly を選ぶ (R `e1071::svm` の kernel= 流)。
--
-- カーネルハイパラは 'KernelParams' (ℓ/σ_f²/period) を持つ。 GP の観測ノイズ σ_n² は
-- SVM には不要なので 'GPParams' でなく 'KernelParams' のみに依存する (Phase 75.18)。
--
-- 双対問題: max_α  Σα_i − ½ ΣΣ α_i α_j y_i y_j K(x_i,x_j)
--           s.t.   0 ≤ α_i ≤ C,  Σ α_i y_i = 0
--
-- SMO は 2 変数 (α_i, α_j) ずつ解析更新する。 第 1 変数 = KKT 違反点、 第 2 変数 =
-- |E_i − E_j| 最大 (Platt の 2nd heuristic)。 **乱数不使用ゆえ純粋・決定的** (簡易 SMO の
-- ランダム j 選択は使わない)。 予測は Σ_{SV} α_i y_i K(x_i, x) + b (SV のみで決まる)。
--
-- カーネル評価は 'kEvalMV'(距離カーネルは ‖a−b‖²、 内積カーネル Linear/Poly は a·b、
-- Poly の γ は 'kpLengthScale' から γ=1/(2ℓ²)・Linear の倍率は σ_f²)で共有する。
module Hanalyze.Model.SVM
  ( SVMConfig (..)
  , defaultSVM
  , SVM (..)
  , SVMMulti (..)
  , fitSVM
  , fitSVMMulti
  , predictSVMScore
  , predictSVM
  , predictSVMMulti
  , numSupportVectors
    -- * 自動最適化 (k-fold CV グリッド探索・config に畳む)
  , SVMHyper (..)
  , SVMTuneGrid (..)
  , defaultSVMTuneGrid
  , tuneSVM
  ) where

import qualified Data.Vector           as V
import qualified Data.Vector.Unboxed   as VU
import qualified Numeric.LinearAlgebra as LA
import           Data.Text             (Text)
import           Data.List             (nub, sort, maximumBy)
import           Data.Ord              (comparing)
import           Control.Monad.ST      (runST)
import qualified System.Random.MWC     as MWC
import           Hanalyze.Stat.CV (Fold, kFold)
import           Hanalyze.Model.Kernel (Kernel (..), KernelParams (..), defaultKernelParams, kEvalMV)

-- ===========================================================================
-- カーネル (共有 'Kernel' + 'KernelParams' を使う)
-- ===========================================================================

-- | Gram 行列 K (n×n)。 K_ij = kEvalMV ker params (row i) (row j)。
kGram :: Kernel -> KernelParams -> LA.Matrix Double -> LA.Matrix Double
kGram ker p x =
  let rv = V.fromList (LA.toRows x)   -- boxed Vector of 行ベクトル (O(1) 添字)
      n  = V.length rv
  in LA.build (n, n) (\i j -> kEvalMV ker p (rv V.! round i) (rv V.! round j))
  -- NB: LA.build の i,j は Double。 round で Int 添字に戻す (整数値ゆえ安全)。

-- ===========================================================================
-- 設定 / モデル
-- ===========================================================================

data SVMConfig = SVMConfig
  { svmC         :: !Double        -- ^ 正則化 C (0 ≤ α ≤ C)。
  , svmKernel    :: !Kernel        -- ^ 共有カーネル (既定 'Linear')。
  , svmParams    :: !KernelParams  -- ^ カーネルハイパラ (ℓ→γ=1/2ℓ²、 σ_f²=Linear 倍率)。
  , svmTol       :: !Double    -- ^ KKT 許容 (E の許容)。
  , svmMaxPasses :: !Int       -- ^ 変化が無いパスの連続上限 (収束判定)。
  , svmMaxIter   :: !Int       -- ^ 総パス数の上限 (安全弁)。
  , svmHyper     :: !SVMHyper  -- ^ ハイパラの決め方 (固定 or CV グリッド探索)。 GP の
                               --   'HyperStrategy' と同型: 調整は config に畳み動詞は @svmCls@ 一本。
  } deriving (Show)

defaultSVM :: SVMConfig
defaultSVM = SVMConfig
  { svmC = 1.0, svmKernel = Linear, svmParams = defaultKernelParams
  , svmTol = 1e-3, svmMaxPasses = 5, svmMaxIter = 1000
  , svmHyper = SVMFixed }

-- | 学習済カーネル SVM。 **α>0 のサポートベクタのみ**保持 (スパース)。
data SVM = SVM
  { svmSVx    :: !(LA.Matrix Double)  -- ^ サポートベクタ (n_sv × d)。
  , svmSVy    :: !(VU.Vector Double)  -- ^ その符号ラベル ±1。
  , svmSVa    :: !(VU.Vector Double)  -- ^ 双対係数 α (>0)。
  , svmB      :: !Double              -- ^ バイアス。
  , svmKern   :: !Kernel              -- ^ 共有カーネル。
  , svmKParams :: !KernelParams       -- ^ カーネルハイパラ (予測時に再利用)。
  } deriving (Show)

-- | サポートベクタ数 (= α>0 の点数)。
numSupportVectors :: SVM -> Int
numSupportVectors = LA.rows . svmSVx

-- ===========================================================================
-- SMO (双対・2 クラス {0,1} → ±1)
-- ===========================================================================

-- | 2 クラス C-SVC を SMO で学習 (y ∈ {0,1})。 決定的 (乱数不使用)。
fitSVM :: SVMConfig -> LA.Matrix Double -> VU.Vector Int -> SVM
fitSVM cfg x yInt =
  let !n    = LA.rows x
      ys    = VU.generate n (\i -> if yInt VU.! i == 0 then -1 else 1) :: VU.Vector Double
      gram  = kGram (svmKernel cfg) (svmParams cfg) x
      cC    = svmC cfg
      tol   = svmTol cfg
      kij i j = gram `LA.atIndex` (i, j)
      -- 決定関数 f(i) = Σ_j α_j y_j K_ij + b
      decision al b i = b + sum [ al VU.! j * ys VU.! j * kij i j | j <- [0 .. n - 1] ]
      -- 1 パス: 全 i を走査し KKT 違反点を見つけ第 2 変数を選んで更新。
      onePass (!al0, !b0) =
        let step (al, b, changed) i =
              let ei = decision al b i - ys VU.! i
                  ai = al VU.! i; yi = ys VU.! i
                  viol = (yi * ei < negate tol && ai < cC) || (yi * ei > tol && ai > 0)
              in if not viol then (al, b, changed)
                 else
                   -- 第 2 変数 j = |E_i − E_j| 最大 (j /= i)。
                   let es = [ (j, decision al b j - ys VU.! j) | j <- [0 .. n - 1], j /= i ]
                       (j, ej) = maximumBy (comparing (\(_, e) -> abs (ei - e))) es
                       aj = al VU.! j; yj = ys VU.! j
                       (lo, hi) = if yi /= yj
                                    then (max 0 (aj - ai), min cC (cC + aj - ai))
                                    else (max 0 (ai + aj - cC), min cC (ai + aj))
                       eta = 2 * kij i j - kij i i - kij j j
                   in if lo >= hi || eta >= 0 then (al, b, changed)
                      else
                        let ajNew0 = aj - yj * (ei - ej) / eta
                            ajNew  = min hi (max lo ajNew0)
                        in if abs (ajNew - aj) < 1e-5 then (al, b, changed)
                           else
                             let aiNew = ai + yi * yj * (aj - ajNew)
                                 al'   = al VU.// [(i, aiNew), (j, ajNew)]
                                 b1 = b - ei - yi * (aiNew - ai) * kij i i
                                        - yj * (ajNew - aj) * kij i j
                                 b2 = b - ej - yi * (aiNew - ai) * kij i j
                                        - yj * (ajNew - aj) * kij j j
                                 bNew | aiNew > 0 && aiNew < cC = b1
                                      | ajNew > 0 && ajNew < cC = b2
                                      | otherwise               = (b1 + b2) / 2
                             in (al', bNew, changed + 1)
        in foldl step (al0, b0, 0 :: Int) [0 .. n - 1]
      -- パスを回す: 変化無しが maxPasses 連続 or maxIter 到達で停止。
      loop !al !b !passes !iter
        | passes >= svmMaxPasses cfg || iter >= svmMaxIter cfg = (al, b)
        | otherwise =
            let (al', b', changed) = onePass (al, b)
            in if changed == 0 then loop al' b' (passes + 1) (iter + 1)
                               else loop al' b' 0 (iter + 1)
      (alphaF, bF) = loop (VU.replicate n 0) 0 0 0
      -- α>0 のみ保持 (スパース SV)。
      svIdx = [ i | i <- [0 .. n - 1], alphaF VU.! i > 1e-8 ]
      svX   = LA.fromRows [ LA.toRows x !! i | i <- svIdx ]
      svY   = VU.fromList [ ys VU.! i | i <- svIdx ]
      svA   = VU.fromList [ alphaF VU.! i | i <- svIdx ]
  in SVM { svmSVx = svX, svmSVy = svY, svmSVa = svA
               , svmB = bF, svmKern = svmKernel cfg
               , svmKParams = svmParams cfg }

-- | 決定値 f(x) = Σ_{SV} α_i y_i K(x_i, x) + b (各行)。
predictSVMScore :: SVM -> LA.Matrix Double -> VU.Vector Double
predictSVMScore m x =
  let svRows = LA.toRows (svmSVx m)
      nsv    = length svRows
      ker    = svmKern m
      kp     = svmKParams m
      score xr = svmB m
        + sum [ svmSVa m VU.! s * svmSVy m VU.! s * kEvalMV ker kp (svRows !! s) xr
              | s <- [0 .. nsv - 1] ]
  in VU.fromList (map score (LA.toRows x))

-- | 予測ラベル {0,1} (score ≥ 0 → 1)。
predictSVM :: SVM -> LA.Matrix Double -> VU.Vector Int
predictSVM m x = VU.map (\s -> if s >= 0 then 1 else 0) (predictSVMScore m x)

-- ===========================================================================
-- 多クラス (one-vs-rest)
-- ===========================================================================

data SVMMulti = SVMMulti
  { svmmClasses    :: ![Int]
  , svmmBinaries   :: ![SVM]   -- ^ クラス順に 1-vs-rest。
  , svmmClassNames :: ![Text]  -- ^ クラス名 (df|-> が levels 注入・空=数値表示)。
  } deriving (Show)

-- | 多クラス C-SVC (one-vs-rest・各 binary は 'fitSVM'・決定的)。
fitSVMMulti :: SVMConfig -> LA.Matrix Double -> VU.Vector Int -> SVMMulti
fitSVMMulti cfg x y =
  let classes = sort (nub (VU.toList y))
      bins = [ fitSVM cfg x (VU.map (\yi -> if yi == c then 1 else 0) y)
             | c <- classes ]
  in SVMMulti { svmmClasses = classes, svmmBinaries = bins, svmmClassNames = [] }

-- | 各クラスの score 最大で分類。
predictSVMMulti :: SVMMulti -> LA.Matrix Double -> VU.Vector Int
predictSVMMulti m x =
  let classes = svmmClasses m
      scores  = [ VU.toList (predictSVMScore b x) | b <- svmmBinaries m ]
      n       = LA.rows x
      pick i  = let col = [ (classes !! k, scores !! k !! i) | k <- [0 .. length classes - 1] ]
                in fst (maximumBy (comparing snd) col)
  in VU.fromList [ pick i | i <- [0 .. n - 1] ]

-- ===========================================================================
-- 自動最適化 (k-fold CV グリッド探索)
--
-- SVM は確率モデルでないため GP の周辺尤度最適化は使えない。 代わりに
-- **k-fold 交差検証の accuracy を最大化**する格子探索 (sklearn `GridSearchCV` /
-- R `e1071::tune.svm` 相当)。 SMO は乱数不使用・fold 分割も固定 seed の
-- 'Hanalyze.Stat.CV.kFold' を 'runST' で回すため **完全に決定的**。
-- ===========================================================================

-- | ハイパラの決め方 (GP の 'HyperStrategy' と同型)。 固定値をそのまま使うか、
--   CV グリッドを探索して最良を選ぶか。 'SVMConfig' の @svmHyper@ に持たせ、 動詞 @svmCls@ が
--   これを見て分岐する (別動詞 @svmClsTuned@ は作らない)。
data SVMHyper
  = SVMFixed              -- ^ 'SVMConfig' の C/kernel/params をそのまま使う。
  | SVMTuneCV SVMTuneGrid -- ^ グリッドを k-fold CV で探索し最良ハイパラで再学習。
  deriving (Show)

-- | SVM ハイパラ探索グリッド。 候補は C × kernel × ℓ の直積。
-- 'Linear' カーネルは ℓ を使わないので ℓ 軸は無視する (重複評価を避ける)。
data SVMTuneGrid = SVMTuneGrid
  { svmtCs      :: ![Double]   -- ^ 正則化 C 候補 (0 < C)。
  , svmtKernels :: ![Kernel]   -- ^ カーネル候補。
  , svmtLengths :: ![Double]   -- ^ 長さスケール ℓ 候補 (距離カーネル/Poly の γ=1/2ℓ²)。
  , svmtFolds   :: !Int        -- ^ CV fold 数 k (2 以上)。
  } deriving (Show)

-- | 既定グリッド: C ∈ {0.1,1,10,100} × RBF × ℓ ∈ {0.25,0.5,1,2,4}・5-fold。
defaultSVMTuneGrid :: SVMTuneGrid
defaultSVMTuneGrid = SVMTuneGrid
  { svmtCs      = [0.1, 1, 10, 100]
  , svmtKernels = [RBF]
  , svmtLengths = [0.25, 0.5, 1, 2, 4]
  , svmtFolds   = 5
  }

-- | グリッドの 1 点に対応する 'SVMConfig' を作る (base から C/kernel/ℓ を差し替え)。
tuneCandidate :: SVMConfig -> Double -> Kernel -> Double -> SVMConfig
tuneCandidate base c ker l =
  base { svmC = c, svmKernel = ker
       , svmParams = (svmParams base) { kpLengthScale = l } }

-- | グリッドの全候補 'SVMConfig' (Linear は ℓ 軸を畳む)。
tuneCandidates :: SVMConfig -> SVMTuneGrid -> [SVMConfig]
tuneCandidates base grid =
  [ tuneCandidate base c ker l
  | c   <- svmtCs grid
  , ker <- svmtKernels grid
  , l   <- lengthsFor ker ]
  where
    lengthsFor Linear = take 1 (svmtLengths grid ++ [1.0])  -- ℓ 無関係 → 1 点
    lengthsFor _      = svmtLengths grid

-- | 行添字リストで行列の行とラベルを抜き出す。
sliceRows :: V.Vector (LA.Vector Double) -> VU.Vector Int -> [Int]
          -> (LA.Matrix Double, VU.Vector Int)
sliceRows rows y idx =
  ( LA.fromRows [ rows V.! i | i <- idx ]
  , VU.fromList [ y VU.! i | i <- idx ] )

-- | 1 候補の平均 CV accuracy。 各 fold で train に学習し test の正解率を測る。
cvAccuracy :: SVMConfig -> [Fold]
           -> V.Vector (LA.Vector Double) -> VU.Vector Int -> Double
cvAccuracy cfg folds rows y =
  let accs = [ foldAcc tr te | (tr, te) <- folds, not (null te) ]
      foldAcc trIdx teIdx =
        let (xTr, yTr) = sliceRows rows y trIdx
            (xTe, yTe) = sliceRows rows y teIdx
            model = fitSVMMulti cfg xTr yTr
            pred  = predictSVMMulti model xTe
            nTe   = VU.length yTe
            ok    = length [ () | i <- [0 .. nTe - 1], pred VU.! i == yTe VU.! i ]
        in fromIntegral ok / fromIntegral nTe
  in if null accs then 0 else sum accs / fromIntegral (length accs)

-- | k-fold CV で SVM のハイパラ (C × kernel × ℓ) を調律する。 CV accuracy を
-- 最大化する 'SVMConfig' と、 その平均 CV accuracy を返す。 **決定的** (固定 seed の
-- fold 分割・SMO は乱数不使用)。 sklearn `GridSearchCV` / R `tune.svm` 相当。
tuneSVM :: SVMConfig -> SVMTuneGrid -> LA.Matrix Double -> VU.Vector Int
        -> (SVMConfig, Double)
tuneSVM base grid x y =
  let n     = LA.rows x
      rows  = V.fromList (LA.toRows x)
      k     = max 2 (min (svmtFolds grid) n)
      -- 固定 seed の k-fold (決定的・再現可能)。
      folds = runST $ do
                gen <- MWC.initialize (V.singleton 42)
                kFold k n gen
      scored = [ (cfg, cvAccuracy cfg folds rows y)
               | cfg <- tuneCandidates base grid ]
  in maximumBy (comparing snd) scored
