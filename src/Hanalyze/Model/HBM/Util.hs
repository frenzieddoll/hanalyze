{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Hanalyze.Model.HBM.Util
-- Description : HBM の純粋な数値・線形代数 leaf ユーティリティ
-- Copyright   : (c) 2026 Aelysce Project (Toshiaki Honda)
-- License     : BSD-3-Clause
--
-- HBM の純粋な数値・線形代数 leaf ユーティリティ。
--
-- ここに集めた定義は HBM のいずれの型 (Distribution / Model / Track 等) にも
-- 依存しない葉 (leaf) であり、 Floating / Ord のみで多相に書かれている。
-- AD (Reverse.Double) でも Track でも評価できるよう型クラス制約を最小に保つ。
-- 'Hanalyze.Model.HBM' は本モジュールを import して内部利用 + 一部を re-export する
-- (公開シンボル: 'lgammaApprox' / 'digamma')。
--
-- Phase 58.2 で 'Hanalyze.Model.HBM' (5,519 行) から責務分離して抽出。
-- 数値は 1 bit も変えていない (純粋な移設)。
module Hanalyze.Model.HBM.Util
  ( -- * 線形代数 (下三角ソルバ / Cholesky / リスト整形)
    backSubLT
  , chunksOf
  , choleskyL
  , forwardSub
  , gpRBFCovList
    -- * log-sum-exp / HMM forward
  , negInf
  , logSumExpA
  , hmmForwardLogLik
    -- * 不完全ガンマ関数 P(a, x)
  , incGammaPA
  , igammSer
  , igammCF
    -- * 正則化不完全ベータ関数 I_x(a, b)
  , incBetaA
  , betaCFA
    -- * 数値ユーティリティ (Γ / digamma / 階乗 / Bessel)
  , lgammaApprox
  , digamma
  , lgammaApproxDeriv
  , logFactorial
  , logBinomCoeff
  , logBesselI0
  ) where

import Data.List (foldl')
import qualified Data.Vector as V

-- ===========================================================================
-- 線形代数 (下三角ソルバ / Cholesky / リスト整形)
-- ===========================================================================
-- Phase 95 A2 (2026-07-13): choleskyL/forwardSub/backSubLT の内部を nested-list
--   ([[a]] + !! O(n)索引 + ++ O(n)追記) から Data.Vector (O(1) 索引 + snoc) へ
--   脱リスト化。公開シグネチャ ([[a]]) は不変 = 呼び出し元は無改修。数値は回帰
--   テスト内で一致 (posterior bit 一致を実測)。★N=11 の gp-regr では効果ゼロ
--   (真因は AD tape ノード alloc・§A2 参照) だが、大 N の密行列では list !!/++ が
--   O(N⁴) 化して支配的になるため user 判断で先行 infra として採用 (2026-07-13)。
--   ※さらなる高速化には interface 自体の Vector 化 (呼出側の per-call 変換除去) が
--   要・大 N 密行列モデル出現時の TODO。

-- | Phase 95 B-dsl: RBF (exponentiated-quadratic) GP カーネルの共分散行列を
--   nested list で構築する。 @Σ_ij = α² exp(-0.5 (x_i-x_j)²/ρ²) + [i=j](1e-10 + σ)@。
--   'Hanalyze.Model.HBM.gpExpQuadCov' (jitter 1e-10 込) + 対角 σ と一致する
--   = 'MvNormalGpRBF' 密度が呼ぶ (値は既存 gp-regr モデルと bit 一致)。 下層 (Util)
--   に置くことで 'Distribution' の @obsLogSum@ から参照できる (Model 層の
--   'gpExpQuadCov' は上層ゆえ密度からは呼べない)。 ★ホット経路 (Gradient の
--   'gpRBFAnalyticVG') は本 list 版を使わず hmatrix Matrix で直接組む (脱リスト)。
{-# INLINABLE gpRBFCovList #-}
gpRBFCovList :: forall a. Floating a => [a] -> a -> a -> a -> [[a]]
gpRBFCovList xs alpha rho sigma =
  [ [ let d = xi - xj
          k = alpha * alpha * exp (negate 0.5 * d * d / (rho * rho))
      in k + (if i == j then 1e-10 + sigma else 0)
    | (j, xj) <- zip [0 :: Int ..] xs ]
  | (i, xi) <- zip [0 :: Int ..] xs ]

-- | 下三角 L から Lᵀ x = b を後退代入で解く (L は @choleskyL@ 形式)。
{-# INLINABLE backSubLT #-}
backSubLT :: forall a. Floating a => [[a]] -> [a] -> [a]
backSubLT l b =
  let n   = length b
      lV  = V.fromList [ V.fromList r | r <- l ]
      arr = V.fromListN n (b ++ repeat 0)
      go :: Int -> V.Vector a -> V.Vector a
      go i acc                       -- acc = x[i+1..n-1]
        | i < 0 = acc
        | otherwise =
            -- (acc は index i+1..n-1 の解、 i 番目を解く)
            -- Lᵀ x = b → 行 i: Σ_{j>=i} L[j][i] x_j = b_i
            -- → x_i = (b_i - Σ_{j>i} L[j][i] x_j) / L[i][i]
            let lii  = (lV V.! i) V.! i
                bi   = arr V.! i
                s    = V.sum (V.imap (\t xj -> (lV V.! (i + 1 + t)) V.! i * xj) acc)
                xi   = (bi - s) / lii
            in go (i - 1) (V.cons xi acc)
  in V.toList (go (n - 1) V.empty)

-- | リストを長さ @n@ ごとに分割。最後が短ければそのまま (本実装では使わない想定)。
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t

-- | 対称正定値行列 Σ の Cholesky 下三角分解 L (Σ = L Lᵀ)。
-- 行列は行リスト @[[a]]@ で、l[i] は長さ @i+1@ の下三角行 ([L[i][0]..L[i][i]])。
-- 対角が非正になれば @Nothing@。
{-# INLINABLE choleskyL #-}
choleskyL :: forall a. (Floating a, Ord a) => [[a]] -> Maybe [[a]]
choleskyL a0 =
  let n  = length a0
      aV = V.fromList [ V.fromList r | r <- a0 ]   -- 入力行 (各行長 >= i+1)
      step :: Int -> V.Vector (V.Vector a) -> Maybe (V.Vector (V.Vector a))
      step i prev                                   -- prev = 確定済 L[0..i-1]
        | i == n = Just prev
        | otherwise =
            let row = aV V.! i
                buildCol :: Int -> V.Vector a -> Maybe (V.Vector a)
                buildCol j cur                        -- cur = L[i][0..j-1]
                  | j > i  = Just cur
                  | j == i =
                      let s  = V.sum (V.map (\v -> v * v) cur)
                          d2 = (row V.! i) - s
                      in if d2 <= 0
                           then Nothing
                           else buildCol (j + 1) (V.snoc cur (sqrt d2))
                  | otherwise =
                      let lj  = prev V.! j           -- 長さ j+1
                          s   = V.sum (V.zipWith (*) cur lj)
                          ljj = lj V.! j
                      in if ljj == 0
                           then Nothing
                           else buildCol (j + 1) (V.snoc cur ((row V.! j - s) / ljj))
            in case buildCol 0 V.empty of
                 Nothing -> Nothing
                 Just nr -> step (i + 1) (V.snoc prev nr)
  in fmap (\v -> [ V.toList r | r <- V.toList v ]) (step 0 V.empty)

-- | 下三角系 L z = b の前進代入 (L は @choleskyL@ 形式、長さ各 i+1)。
{-# INLINABLE forwardSub #-}
forwardSub :: forall a. Floating a => [[a]] -> [a] -> [a]
forwardSub l b =
  let n   = length b
      lV  = V.fromList [ V.fromList r | r <- l ]
      bV  = V.fromList b
      go :: Int -> V.Vector a -> V.Vector a
      go i acc                          -- acc = z[0..i-1]
        | i == n = acc
        | otherwise =
            let lrow = lV V.! i           -- 長さ i+1
                lii  = lrow V.! i
                lpre = V.take i lrow      -- L[i][0..i-1]
                bi   = bV V.! i
                s    = V.sum (V.zipWith (*) lpre acc)
                zi   = (bi - s) / lii
            in go (i + 1) (V.snoc acc zi)
  in V.toList (go 0 V.empty)

-- ===========================================================================
-- log-sum-exp
-- ===========================================================================

negInf :: Floating a => a
negInf = -1/0

-- | 多相 log-sum-exp。AD でも Track でも使えるよう Floating + Ord で書く。
-- @logSumExpA xs = log (Σ exp x)@ を最大値シフトで安定化。
{-# INLINABLE logSumExpA #-}
logSumExpA :: (Floating a, Ord a) => [a] -> a
logSumExpA []  = negInf
logSumExpA [x] = x
logSumExpA xs  =
  let m = maximum xs
  -- 全要素が -∞ なら m - m = NaN になるので早期 return
  in if m == negInf
       then negInf
       else m + log (sum (map (\x -> exp (x - m)) xs))

-- ===========================================================================
-- HMM forward algorithm (状態列の周辺化)
-- ===========================================================================
-- Phase 92 A2 (2026-07-17): Model.hs:1071 から純粋移設 (数値は 1 bit も不変)。
-- 'Distribution' の 'HmmForwardNormal' 密度 ('obsLogSum') が呼ぶため、
-- Model 非依存の leaf である本モジュールへ降ろした。
-- 'Hanalyze.Model.HBM.Model' が従来どおり re-export する。

-- | 隠れマルコフモデルの周辺対数尤度 (forward algorithm)。
--
-- Recursion in log-space (underflow 防止):
-- * @α_1[k] = log π_0[k] + emit[0][k]@
-- * @α_{t+1}[k'] = logSumExp_j (α_t[j] + log T[j][k']) + emit[t+1][k']@
-- * @log P(y_{1..T}) = logSumExp_k α_T[k]@
--
-- 多相 (@Floating a, Ord a@) のため Track / AD 経由でも動く。
-- 計算量 @O(T K²)@。 大 T では list-based なので O(K²) の内部ループは
-- そのまま、 step は foldl' で過去 α を破棄しメモリは @O(K)@。
hmmForwardLogLik :: forall a. (Floating a, Ord a)
                 => [a]     -- ^ 初期分布 π_0 (length K)
                 -> [[a]]   -- ^ 遷移行列 (K×K rows of length K)
                 -> [[a]]   -- ^ log emission [T][K]
                 -> a
hmmForwardLogLik pi0 trans emit
  | null emit       = 0  -- T=0: 観測なし
  | null pi0        = negInf
  | length pi0 /= length trans = negInf
  | any ((/= k) . length) trans = negInf
  | otherwise =
      let -- α_1[s] = log π_0[s] + emit[0][s]
          alpha0 = zipWith (\p e -> log p + e) pi0 (head emit)
          -- 1 step: α_{t+1}[s'] = logSumExp_s (α_t[s] + log T[s][s']) + emit_{t+1}[s']
          step :: [a] -> [a] -> [a]
          step alphaT emT =
            [ logSumExpA
                [ (alphaT !! s) + log ((trans !! s) !! s')
                | s <- [0 .. k - 1] ]
              + (emT !! s')
            | s' <- [0 .. k - 1] ]
          alphaFinal = foldl' step alpha0 (tail emit)
      in logSumExpA alphaFinal
  where
    k = length pi0

-- ===========================================================================
-- 不完全ガンマ関数 P(a, x) = γ(a, x) / Γ(a)  (Numerical Recipes 6.2)
-- ===========================================================================

-- | 正則化された下側不完全ガンマ関数 P(a, x) = γ(a, x) / Γ(a) ∈ [0, 1]。
-- これは Gamma(shape=a, rate=1) の CDF F(x)。
{-# INLINABLE incGammaPA #-}
incGammaPA :: (Floating a, Ord a) => a -> a -> a
incGammaPA a x
  | x <= 0 || a <= 0 = 0
  | x < a + 1        = igammSer a x          -- 級数展開で P(a,x)
  | otherwise        = 1 - igammCF a x        -- 連分数で Q(a,x)、P = 1 - Q

-- 級数展開: P(a, x) = e^{-x} x^a / Γ(a) * Σ x^n / (a(a+1)...(a+n))
{-# INLINABLE igammSer #-}
igammSer :: forall a. (Floating a, Ord a) => a -> a -> a
igammSer a x = sumSer * exp (-x + a * log x - lgammaApprox a)
  where
    -- 反復: term_{n+1} = term_n * x / (a + n + 1)
    sumSer = go (0 :: Int) (1 / a) (1 / a)
    eps :: a
    eps    = 1e-13
    maxIt  = 200 :: Int
    go n term acc
      | n >= maxIt           = acc
      | abs term < abs acc * eps = acc
      | otherwise =
          let n'    = n + 1
              term' = term * x / (a + fromIntegral n')
              acc'  = acc + term'
          in go n' term' acc'

-- 連分数 (Lentz 法): Q(a, x) = e^{-x} x^a / Γ(a) * CF
-- CF = 1/(x+1-a - 1·(1-a)/(x+3-a - 2·(2-a)/(...))
{-# INLINABLE igammCF #-}
igammCF :: forall a. (Floating a, Ord a) => a -> a -> a
igammCF a x = exp (-x + a * log x - lgammaApprox a) * h
  where
    fpmin, eps :: a
    fpmin = 1e-300
    eps   = 1e-13
    maxIt = 200 :: Int
    -- modified Lentz's method
    b0    = x + 1 - a
    c0    = 1 / fpmin
    d0    = 1 / b0
    h     = goCF (1 :: Int) b0 c0 d0 d0
    goCF i b c d hh
      | i > maxIt              = hh
      | abs (del - 1) < eps    = hh'
      | otherwise              = goCF (i + 1) b' c'' d''' hh'
      where
        an   = -fromIntegral i * (fromIntegral i - a)
        b'   = b + 2
        d'   = b' + an * d
        d''  = if abs d' < fpmin then fpmin else d'
        c'   = b' + an / c
        c''  = if abs c' < fpmin then fpmin else c'
        d''' = 1 / d''
        del  = d''' * c''
        hh'  = hh * del
    _ = c0  -- 未使用ダミー (修正された Lentz 法の起動値: 別経路)

-- ===========================================================================
-- 正則化された不完全ベータ関数 I_x(a, b) = B(x; a, b) / B(a, b)
-- ===========================================================================

-- | 正則化された不完全ベータ関数 I_x(a, b) ∈ [0, 1]。
-- これは Beta(a, b) の CDF F(x)。
-- StudentT の CDF にも内部で使用。
{-# INLINABLE incBetaA #-}
incBetaA :: (Floating a, Ord a) => a -> a -> a -> a
incBetaA x a b
  | x <= 0    = 0
  | x >= 1    = 1
  | otherwise =
      -- 対数ベータ正規化定数
      let bt = exp ( lgammaApprox (a + b)
                   - lgammaApprox a
                   - lgammaApprox b
                   + a * log x
                   + b * log (1 - x))
      in if x < (a + 1) / (a + b + 2)
           then bt * betaCFA x a b / a
           else 1 - bt * betaCFA (1 - x) b a / b

-- 連分数 (modified Lentz, Numerical Recipes §6.4)
{-# INLINABLE betaCFA #-}
betaCFA :: forall a. (Floating a, Ord a) => a -> a -> a -> a
betaCFA x a b = iterate' (1 :: Int) 1 d0 h0
  where
    fpmin, eps :: a
    fpmin = 1e-300
    eps   = 1e-13
    maxIt = 200 :: Int
    qab = a + b
    qap = a + 1
    qam = a - 1
    capLent v = if abs v < fpmin then fpmin else v
    d0 = 1 / capLent (1 - qab * x / qap)
    h0 = d0

    iterate' m c d h
      | m > maxIt          = h
      | abs (del - 1) < eps = hO
      | otherwise          = iterate' (m + 1) cO dO hO
      where
        mD  = fromIntegral m :: a
        -- 偶数項: aa_2m = m(b-m)x / ((qam+2m)(a+2m))
        aaE = mD * (b - mD) * x / ((qam + 2 * mD) * (a + 2 * mD))
        dE  = 1 / capLent (1 + aaE * d)
        cE  = capLent (1 + aaE / c)
        hE  = h * dE * cE
        -- 奇数項: aa_2m+1 = -(a+m)(qab+m)x / ((a+2m)(qap+2m))
        aaO = -(a + mD) * (qab + mD) * x / ((a + 2 * mD) * (qap + 2 * mD))
        dO  = 1 / capLent (1 + aaO * dE)
        cO  = capLent (1 + aaO / cE)
        del = dO * cO
        hO  = hE * del

-- ===========================================================================
-- 数値ユーティリティ (Γ / digamma / 階乗 / Bessel)
-- ===========================================================================

-- | log Γ(z) の Stirling 近似 (z > 0)。AD でも Track でも使える多相版。
{-# INLINABLE lgammaApprox #-}
lgammaApprox :: (Floating a, Ord a) => a -> a
lgammaApprox z
  | z < 12    = lgammaApprox (z + 1) - log z
  | otherwise = (z - 0.5) * log z - z + 0.5 * log (2 * pi)
              + 1 / (12 * z) - 1 / (360 * z ^ (3::Int))

-- | ψ(z) = d/dz log Γ(z) (z > 0・Phase 56.1)。 記号微分 IR の lgamma 単項 op
-- ('SLgammaO' 予定) の導関数用。 'lgammaApprox' と同一の recurrence
-- (z < 12 を押し上げ) + 漸近級数を lgammaApprox の Stirling 微分より 1 項深く
-- (-1/(252 z⁶) まで) 打切り: 真の ψ との差は z=12 で ~1e-11、
-- lgammaApprox の数値微分との差は lgammaApprox 側の打切り由来 ~1.3e-9
-- (試験許容 1e-8 内)。 z ≤ 0 は未対応 (利用箇所は正値前提)。
digamma :: Double -> Double
digamma z
  | z < 12    = digamma (z + 1) - 1 / z
  | otherwise = log z - 1 / (2 * z) - 1 / (12 * z * z)
              + 1 / (120 * z ^ (4 :: Int)) - 1 / (252 * z ^ (6 :: Int))

-- | 'lgammaApprox' の**厳密な項別導関数** (Phase 56.4)。 'digamma' とは最終項
-- 1/(252z⁶) の有無だけ違う (digamma は真の ψ に 1 項深い分この差 ~1.3e-9 が
-- z=12 境界で出る・実測)。 記号微分 IR ('SLgammaO') の導関数は、 評価関数
-- (lgammaApprox) の AD 微分 = walk+ad fallback / 参照勾配と一致させる必要が
-- あるためこちらを使う。
lgammaApproxDeriv :: Double -> Double
lgammaApproxDeriv z
  | z < 12    = lgammaApproxDeriv (z + 1) - 1 / z
  | otherwise = log z - 1 / (2 * z) - 1 / (12 * z * z)
              + 1 / (120 * z ^ (4 :: Int))

logFactorial :: Int -> Double
logFactorial n
  | n <= 1    = 0
  | otherwise = sum (map log [2 .. fromIntegral n])

logBinomCoeff :: Int -> Int -> Double
logBinomCoeff n k = logFactorial n - logFactorial k - logFactorial (n - k)

-- | log I_0(x) — 修正 Bessel 関数 (第一種・order 0) の対数。VonMises 用。
-- 小 x: 級数 I_0(x) = Σ (x/2)^(2k) / (k!)² (k = 0..)
-- 大 x: 漸近展開 I_0(x) ≈ exp(x) / √(2πx) × [1 + 1/(8x) + 9/(128x²) + …]
-- AD/Track 互換のため (Floating a, Ord a) 多相。
{-# INLINABLE logBesselI0 #-}
logBesselI0 :: (Floating a, Ord a) => a -> a
logBesselI0 x
  | x < 0     = logBesselI0 (-x)  -- 偶関数
  | x < 3.75  =
      -- Abramowitz & Stegun 9.8.1: 多項式近似 (誤差 < 1.6e-7)
      let t = (x / 3.75) ^ (2::Int)
          i0 = 1 + t * (3.5156229 + t * (3.0899424 + t * (1.2067492
             + t * (0.2659732 + t * (0.0360768 + t * 0.0045813)))))
      in log i0
  | otherwise =
      -- Abramowitz & Stegun 9.8.2: 漸近 (誤差 < 1.9e-7)
      let t = 3.75 / x
          poly = 0.39894228 + t * (0.01328592 + t * (0.00225319
               + t * (-0.00157565 + t * (0.00916281 + t * (-0.02057706
               + t * (0.02635537 + t * (-0.01647633 + t * 0.00392377)))))))
      in x - 0.5 * log x + log poly
