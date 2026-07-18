# ベイズ推論 advanced: SMC / Bridge Sampling / Bayes Factor / BMA (Phase 29)

> 2026-05-29 Phase 29 で追加された **marginal likelihood ベースの advanced
> 機能** をまとめたマニュアル。 既存 HBM (`Hanalyze.Model.HBM`) + MCMC
> (NUTS / MH / Gibbs) で得た posterior chain を、 **モデル比較・仮説検定・
> 多モデル予測** に拡張して使うための API ガイド。

---

## 0. 概観

| 機能 | API | 用途 |
|---|---|---|
| SMC (Sequential Monte Carlo) | `Hanalyze.MCMC.SMC.smc` | 多峰 posterior の効率的なサンプリング、 副次的に log marginal の粗推定 |
| Bridge Sampling | `Hanalyze.Stat.BridgeSampling.bridgeSampling` | **log marginal likelihood の高精度推定** (Meng-Wong 1996) |
| Bayes Factor | `Hanalyze.Stat.BayesFactor.bayesFactor` | 2 モデル間の証拠強度 BF_{10} = p(y\|M_1)/p(y\|M_0) + Kass-Raftery 解釈 |
| BMA | `Hanalyze.Stat.BayesianModelAveraging.bayesianModelAveraging` | K モデルの posterior weights + 予測の重み付き平均 |

依存: BF / BMA は Bridge の log marginal を入力に使う。 SMC は独立の sampler。

---

## 1. SMC: tempered Sequential Monte Carlo

多峰 posterior や尖り posterior で NUTS が混合不良に陥るケースで、 粒子集合
+ temperature annealing で安定にサンプリングする。

```haskell
import qualified Hanalyze.MCMC.SMC as SMC
import qualified Hanalyze.Model.HBM as HBM
import qualified Data.Map.Strict as M
import qualified System.Random.MWC as MWC

model :: HBM.ModelP ()
model = do
  mu <- HBM.sample "mu" (HBM.Normal 0 10)
  HBM.observe "y" (HBM.Normal mu 1) [4.8, 5.2, 5.0, 4.9, 5.1]

main = do
  gen <- MWC.create
  let cfg = SMC.defaultSMCConfig ["mu"]
  res <- SMC.smc model cfg (M.fromList [("mu", 0)]) gen
  print (SMC.smcChain res)        -- 粒子を Chain 形に詰めた posterior
  print (SMC.smcLogMarginal res)  -- log marginal の粗推定 (bias 注意)
  print (SMC.smcESSHistory res)   -- temperature step ごとの ESS
```

**重要**: SMC の `smcLogMarginal` は **初期粒子が prior 近似である**ことを
仮定した推定値。 初期粒子を `init_` 中心の jittered Gaussian で生成する
本実装は prior が広いと bias する。 **厳密な log marginal が必要なら Bridge
Sampling を使う**。

設定 (`SMCConfig`):

- `smcNParticles`: 粒子数 (典型 500-2000)
- `smcNSteps`: temperature step 数 T (典型 10-50、 多いほど精度向上)
- `smcMHIterations`: 各 temperature 内の MH 移動回数 K (典型 5-20)
- `smcESSThreshold`: ESS < N · this で systematic resample (典型 0.5)

---

## 2. Bridge Sampling: log marginal likelihood

既存 MCMC chain + diagonal Gaussian proposal (chain から自動 fit) で
log marginal を Meng-Wong iterative formula で推定する。

```haskell
import qualified Hanalyze.MCMC.NUTS as NUTS
import qualified Hanalyze.Stat.BridgeSampling as BS

main = do
  gen <- MWC.create
  chain <- NUTS.nuts model NUTS.defaultNUTSConfig (M.fromList [("mu", 5)]) gen
  gen2 <- MWC.create
  br <- BS.bridgeSampling model BS.defaultBridgeConfig chain gen2
  print (BS.brLogMarginal br)   -- log p(y) 推定
  print (BS.brConverged br)     -- True なら tol 以内収束
  print (BS.brIterations br)    -- 収束に要した反復数
```

設定 (`BridgeConfig`):

- `bcNProposal`: proposal samples 数 (典型 chain と同等 ≈ 500-2000)
- `bcMaxIter`: 反復上限 (典型 100、 通常 < 20 で収束)
- `bcTolerance`: 反復収束判定 |Δ log r̂| < tol (典型 1e-6)

**精度実証** (Phase 29-A2 unit test):
Gaussian × Gaussian model (μ ~ N(0, 10), 10 obs y=5) で解析解
log p(y) = -12.7686 に対し **0.5 以内の絶対誤差で一致**。

---

## 3. Bayes Factor: 2 モデル間の証拠強度

```haskell
import qualified Hanalyze.Stat.BayesFactor as BF

main = do
  -- M_0、 M_1 の chain を別々に取る
  ch0 <- NUTS.nuts m0 cfg init_ gen0
  ch1 <- NUTS.nuts m1 cfg init_ gen1
  gen2 <- MWC.create
  r <- BF.bayesFactor m0 ch0 m1 ch1 BS.defaultBridgeConfig gen2
  print (BF.bfLogE r)             -- log_e BF_{10}
  print (BF.bfLog10 r)            -- log_10 BF_{10}
  print (BF.interpretBF (BF.bfLogE r))  -- Negligible / Positive / Strong / VeryStrong
```

**Kass-Raftery 解釈** (1995 Table 1):

| log_e BF | log_10 BF | 解釈 |
|---|---|---|
| 0..1 | 0..0.5 | Negligible (弱い) |
| 1..3 | 0.5..1.3 | Positive (substantial) |
| 3..5 | 1.3..2 | Strong |
| 5+ | 2+ | Very strong (decisive) |

---

## 4. BMA: 真の Bayesian Model Averaging

Bridge 推定 log marginal から K モデルの posterior weights を計算し、
予測の重み付き平均を取る。 既存 pseudo-BMA (PSIS-LOO 近似) よりも
**Bayes Factor / 仮説検定と一貫した重み**を返す。

```haskell
import qualified Hanalyze.Stat.BayesianModelAveraging as BMA

main = do
  -- K モデル分の chain + Bridge 推定 log marginal を集める
  lms <- forM [m0, m1, m2] $ \m -> do
    ch <- NUTS.nuts m cfg init_ =<< MWC.create
    BS.brLogMarginal <$> BS.bridgeSampling m BS.defaultBridgeConfig ch =<< MWC.create
  let bma = BMA.bayesianModelAveraging lms Nothing  -- Nothing = uniform model prior
  print (BMA.bmaWeights bma)   -- posterior model weights (sum to 1)

  -- 各モデルから出した予測 (= per-model posterior mean prediction at x*) を BMA
  let v0 = LA.fromList [1.0, 2.0, 3.0]
      v1 = LA.fromList [1.1, 2.1, 3.1]
      v2 = LA.fromList [1.2, 2.2, 3.2]
      avg = BMA.averagePredictions bma [v0, v1, v2]
  print avg
```

---

## 5. SMC vs Bridge Sampling cross-check

Phase 29-A1/A2 では SMC と Bridge を **独立な推定経路**として位置付け:

- SMC log marginal: temperature schedule の incremental log-mean-weight
- Bridge log marginal: posterior chain + Gaussian proposal の Meng-Wong 解

両者が同じ値に近ければ chain 収束 + schedule 適切の裏付け。 大きく乖離する
なら NUTS chain の混合不足 / SMC schedule 粗さ / proposal 不適切のサイン。

**注**: SMC の log marginal は init particle 分布に依存する bias を持つ。
精度が必要なら Bridge を primary、 SMC を sanity check に使う。

---

## 6. 既知の制限 / 注意

- Bridge Sampling の proposal は **diagonal Gaussian** (= 相関無視)。
  posterior が強い相関を持つ場合は精度低下。 将来: full covariance fit option。
- SMC は **single chain で multi-modal** に有利だが、 unimodal posterior では
  NUTS の方が ESS / 時間 で勝つ。
- BMA は log marginal の **絶対値スケール** に敏感。 model 間で異なる priors
  / parameterization を使う場合は注意 (= Lindley paradox 警戒)。

---

## 出典

- Del Moral, Doucet, Jasra (2006) "Sequential Monte Carlo samplers". JRSSB 68.
- Meng & Wong (1996) "Simulating ratios of normalising constants". Statistica Sinica 6.
- Gronau et al. (2017) "A tutorial on bridge sampling". J. Math. Psych. 81.
- Kass & Raftery (1995) "Bayes factors". JASA 90.
- Hoeting, Madigan, Raftery, Volinsky (1999) "Bayesian Model Averaging:
  A Tutorial". Statistical Science 14(4).
