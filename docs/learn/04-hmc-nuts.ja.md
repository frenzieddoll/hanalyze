# 学習資料 4 — Hamiltonian Monte Carlo と NUTS

> 高次元事後分布を効率的に sample する HMC と、その自動軌道長版
> NUTS の幾何学・実装・診断。

## 1. HMC のアイデア

### 1.1 物理アナロジー

事後 $\pi(\theta) \propto \exp(-U(\theta))$, $U(\theta) = -\log \pi(\theta)$
を **ポテンシャルエネルギー** とみなす。

「丘の上にボールを置いて、ある方向に蹴る (= 運動量 $p$ を与える)」。
保存則のもとで動かしてから、新しい位置を提案として使う。

| 物理 | 統計 |
|---|---|
| 位置 $\mathbf{q}$ | パラメタ $\theta$ |
| 運動量 $\mathbf{p}$ | 補助変数 ($\sim \text{Normal}(0, M)$) |
| ポテンシャル $U$ | $-\log \pi(\theta)$ |
| 運動エネルギー $K$ | $\frac{1}{2} \mathbf{p}^T M^{-1} \mathbf{p}$ |
| Hamiltonian $H$ | $U + K$ |

### 1.2 Hamiltonian 方程式

$$ \frac{d\theta}{dt} = \frac{\partial K}{\partial p} = M^{-1} p $$
$$ \frac{dp}{dt} = -\frac{\partial U}{\partial \theta} = \nabla \log \pi(\theta) $$

連続時間で動かすと **エネルギー保存** $H(\theta(t), p(t)) = H_0$ が成立。
連結体上の uniform 分布から sample することと、$\pi$ から sample
することが同値になる (詳細は Neal 2011)。

### 1.3 なぜ MH より速いか

各ステップで「**物理運動による有意な移動**」を行うため、Random Walk
より遥かに早く事後を探索。次元 $D$ に対して:

| 手法 | 1 サンプルに必要なステップ |
|---|---|
| Random Walk Metropolis | $O(D)$ |
| HMC | $O(D^{1/4})$ |

---

## 2. Leapfrog 積分

### 2.1 離散化

連続時間を保てないので、**leapfrog** で離散化:

```text
1. p_{1/2} ← p − (ε/2) ∇U(θ)        [半ステップ運動量]
2. θ' ← θ + ε M⁻¹ p_{1/2}            [位置更新]
3. p' ← p_{1/2} − (ε/2) ∇U(θ')       [後半運動量]
```

これを $L$ 回繰り返す = $L \epsilon$ の時刻長。

### 2.2 シンプレクティック性

leapfrog は **位相空間の体積を保つ** + **時間反転対称**。
完全なエネルギー保存ではないが、長期間でも誤差が累積しない。

### 2.3 Metropolis 補正

leapfrog は近似なので $H_0 \ne H'$ となり得る。MH 補正で修正:

$$ \alpha = \min\!\left(1, \exp(H_0 - H')\right) $$

エネルギー誤差が小さければほぼ常に受容。

### 2.4 hanalyze の実装

```haskell
-- src/MCMC/HMC.hs
leapfrogWith :: ([Text] -> Params -> [Double])  -- gradFn
             -> [Text] -> Double -> Int          -- names, ε, L
             -> Params -> [Double]               -- θ, p
             -> (Params, [Double])               -- (θ', p')
```

勾配 `gradFn` は `Numeric.AD.Mode.Forward.grad` で AD 計算。

---

## 3. 制約変換 (Constrained → Unconstrained)

HMC は実数空間 $\mathbb{R}^D$ で動くが、パラメタには制約 (e.g. $\sigma > 0$, $0 < p < 1$)
がある。これらを **unconstrained 空間に写像** してから leapfrog を行う。

### 3.1 主要な変換

| 制約 | 変換 $u = T(\theta)$ | 逆変換 | log Jacobian |
|---|---|---|---|
| $\theta \in \mathbb{R}$ | $u = \theta$ | $\theta = u$ | 0 |
| $\theta > 0$ | $u = \log \theta$ | $\theta = e^u$ | $u$ |
| $\theta \in (0, 1)$ | $u = \text{logit}(\theta)$ | $\theta = \sigma(u)$ | $\log \sigma(u) (1-\sigma(u))$ |

### 3.2 Jacobian 補正

変数変換すると確率密度に **Jacobian** が必要:

$$ p_U(u) = p_\Theta(\theta(u)) \left|\frac{d\theta}{du}\right| $$

log で:

$$ \log p_U(u) = \log p_\Theta(\theta(u)) + \log |J| $$

`logJointU` がこの補正を含めて計算。

### 3.3 hanalyze の `getTransforms`

```haskell
getTransforms :: ModelP r -> Map Text Transform
-- 各 latent の事前分布から自動判定:
-- Normal → UnconstrainedT
-- HalfNormal/Gamma/Exponential/InverseGamma/Weibull/Pareto → PositiveT
-- Beta/BetaBinomial → UnitIntervalT
-- ...
```

---

## 4. NUTS (No-U-Turn Sampler)

### 4.1 動機

HMC では **軌道長 $L$** をユーザーが指定。短いと探索不十分、長いと
無駄な往復。$L$ の自動決定が NUTS。

### 4.2 アルゴリズム (Hoffman & Gelman 2014)

1. **二分木構築**: 各深さで時間を 2 倍ずつ伸ばす (前向き / 後向き ランダム)
2. **U-turn 検出**: 軌道の両端 $\theta_-, \theta_+$ と運動量 $p_-, p_+$ で

   $$ (\theta_+ - \theta_-) \cdot p_- < 0 \quad \text{or} \quad (\theta_+ - \theta_-) \cdot p_+ < 0 $$

   が成立 → 軌道がループしているので停止
3. **木全体から uniform に提案点を選ぶ** (詳細釣り合いを保つために
   slice variable + 比率による選択)

### 4.3 Dual averaging でステップサイズ自動調整

Stan 流儀: **バーンイン中に target 受容率** (default 0.8) に向けて
ステップサイズ $\epsilon$ を Nesterov dual averaging で更新:

$$ \log \epsilon_{n+1} = \mu - \frac{\sqrt{n}}{\gamma} \bar H_n $$

$\bar H_n$: 受容率の累積偏差。バーンイン後は固定。

### 4.4 hanalyze の実装

```haskell
import MCMC.NUTS (nuts, defaultNUTSConfig, NUTSConfig (..))

ch <- nuts model
        defaultNUTSConfig
          { nutsIterations    = 2000
          , nutsBurnIn        = 1000
          , nutsStepSize      = 0.1     -- 初期値、dual averaging で調整
          , nutsTargetAccept  = 0.8     -- 目標受容率
          , nutsMaxDepth      = 10      -- 二分木の最大深さ
          , nutsAdaptStepSize = True
          }
        init0 gen
```

---

## 5. 診断: BFMI と Divergence

### 5.1 BFMI (Bayesian Fraction of Missing Information)

Betancourt 2016 が提案。エネルギー履歴 $\{H_t\}$ から計算:

$$ \text{BFMI} = \frac{E[(H_t - H_{t-1})^2]}{\text{Var}(H_t)} $$

| BFMI | 解釈 |
|---|---|
| < 0.3 | 病的 (運動量再サンプリングが事後の裾を探索しきれない) |
| 0.3〜0.5 | 注意 |
| > 0.5 | 良好 |

`Stat.MCMC.bfmi`、`Viz.MCMC.energyPlot` で可視化。

### 5.2 Divergence

leapfrog の積分誤差がしきい値 ($|\Delta H| > 1000$) を超えると
**divergent transition**:

- 局所的に curvature が大きい (= 病的事後分布)
- ほぼ常に **non-centered パラメタ化** で改善

`MCMC.NUTS` は `Chain.chainDivergences` に divergent 反復の index を
記録。`Viz.MCMC.pairScatterDiv` でパラメタ空間での発生位置を可視化。

### 5.3 典型例: Neal's funnel

```text
v ~ Normal(0, 3)
x ~ Normal(0, exp(v/2))
```

$v$ が小さいと $x$ のスケールが極小、大きいとスケール巨大 → funnel 状。
HMC が苦手な代表例。`noncentered-demo` で実演:

| | Centered | Non-centered |
|---|---|---|
| BFMI | 0.65 | **1.02** |
| ESS(v) | 102 | **781** (7.6 倍) |
| Divergences | 127 / 2000 | **0** |

---

## 6. 非中心化パラメタ化

### 6.1 Centered (病的)

```haskell
v <- sample "v" (Normal 0 3)
x <- sample "x" (Normal 0 (exp (v / 2)))
```

### 6.2 Non-centered (推奨)

```haskell
v <- sample "v" (Normal 0 3)
x <- nonCenteredNormal "x" 0 (exp (v / 2))
-- 内部:
--   x_raw <- sample "x_raw" (Normal 0 1)
--   x = 0 + exp(v/2) * x_raw         (deterministic)
```

`x_raw` は $v$ から独立になり、posterior 形状が単純化される。

---

## 7. メトリック (Mass Matrix)

leapfrog の運動量 $p \sim \text{Normal}(0, M)$ で **質量行列** $M$ を選ぶ:

| 選択 | 状況 |
|---|---|
| $M = I$ | デフォルト |
| $M = \text{diag}(1/\hat\sigma_i^2)$ | 各成分の分散で正規化 (Stan default) |
| $M = \hat\Sigma^{-1}$ | 共分散全体を使う (warmup で推定) |

hanalyze の現状は $M = I$ (改善の余地あり)。

---

## 8. NUTS 実装の主要ファイル

| ファイル | 内容 |
|---|---|
| `src/MCMC/NUTS.hs` | `nuts`, `nutsChains`, `buildTree`, `uTurn`, dual averaging |
| `src/MCMC/HMC.hs` | `hmc`, `leapfrogWith` (NUTS と共有), `gradUU` |
| `src/Stat/Distribution.hs` | `Transform` 定義, `toUnconstrained`, `logJacobianAdj` |
| `src/Model/HBM.hs` | `getTransforms`, `gradADU`, `logJointUnconstrained` |

NUTS / HMC は AD で勾配を取るため `Numeric.AD.Mode.Forward` を使用。

---

## 9. 実演

```bash
# Energy plot + BFMI 比較 (centered vs non-centered)
cabal run noncentered-demo

# Pair plot で divergence 分布を確認
# → funnel-centered-pair.html (赤 X が divergent transition)

# 統合 demo で BFMI / divergence / energy 全部出る
cabal run integrated-demo
```

---

## 次のステップ

- VI / ADVI と モデル選択 → [05-vi-modelselect.ja.md](05-vi-modelselect.ja.md) (M5)
- 既存ドキュメント: [03-mcmc-samplers.ja.md](../03-mcmc-samplers.ja.md) は API 概観
- 原論文:
  - HMC: Neal "MCMC using Hamiltonian dynamics" (2011)
  - NUTS: Hoffman & Gelman (2014)
  - BFMI: Betancourt "Diagnosing Suboptimal Cotangent Disintegrations" (2016)
