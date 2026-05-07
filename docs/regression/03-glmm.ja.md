# GLMM (混合効果モデル) — 混合分布と計算手法

> 🌐 [English](03-glmm.md) | **日本語**

> 階層構造 (グループ・繰返し測定) を扱う **混合効果モデル**。
> 本 doc では **混合分布の理論** (= 負の二項分布が Gamma-Poisson 混合から
> 出てくる導出) と **GLMM の計算手法 (EM, Laplace 近似, MCMC)** を詳しく解説。
> **多出力対応**: `fitLMEMulti` / `fitGLMMMulti` (列ごと EM/Laplace、グループ化情報共有) — [05-multivariate.ja.md](05-multivariate.ja.md) を参照。

## 目次

1. [なぜ GLMM が必要か](#1-なぜ-glmm-が必要か)
2. [LME (線形混合効果モデル)](#2-lme-線形混合効果モデル)
3. [GLMM (一般化版)](#3-glmm-一般化版)
4. [混合分布: 負の二項分布の導出](#4-混合分布-負の二項分布の導出)
5. [計算手法](#5-計算手法)
   - 5.1 [EM アルゴリズム (LME)](#51-em-アルゴリズム-lme)
   - 5.2 [Laplace 近似 (一般 GLMM)](#52-laplace-近似-一般-glmm)
   - 5.3 [MCMC (フルベイズ)](#53-mcmc-フルベイズ)
6. [hanalyze での実装](#6-hanalyze-での実装)
7. [非中心化パラメタ化](#7-非中心化パラメタ化)
8. [診断と落とし穴](#8-診断と落とし穴)
9. [参考文献](#9-参考文献)

---

## 1. なぜ GLMM が必要か

### 1.1 LM/GLM の独立性仮定の限界

LM/GLM は **観測が独立** を仮定するが、実データは:

- **病院の患者**: 同じ病院内で似ている (院内のケアレベルなど)
- **学校の生徒**: クラス・先生・地域で相関
- **時系列**: 時間的に近い観測は相関
- **繰り返し測定**: 同一被験者の複数回測定

これらを単純に LM で扱うと:
- 標準誤差を **過小評価** → 信頼区間が狭すぎる
- 第一種過誤 (false positive) が増える

### 1.2 プールの選択

3 つの極端な分析方針:

| 方針 | 説明 | 問題 |
|---|---|---|
| **完全プール** (Complete pooling) | 全データを 1 つの LM で fit、グループ無視 | グループ間差を無視 (bias) |
| **個別プール** (No pooling) | グループごとに別の LM | サンプル少ないグループで過剰フィット |
| **部分プール** (Partial pooling) | グループ効果を確率分布で結ぶ = **GLMM** | 中庸、推奨 |

部分プールは **「全体平均からあまり離れない」という事前** を入れる感じ。
データが少ないグループは全体平均に **shrink** する。

### 1.3 例: シンプソンのパラドックス

「全体では x が増えると y が増えるように見えるが、
グループ別では x が増えると y は減る」 — というように **集約と層別で逆転** する現象。

```bash
cabal run simpson-paradox  # デモ
```

GLMM を使うと層内構造を捕捉できる。

---

## 2. LME (線形混合効果モデル)

### 2.1 モデル式

応答が連続実数の場合の混合効果モデル:

$$ y_{ij} = \mathbf{x}_{ij}^T \boldsymbol\beta + \mathbf{z}_{ij}^T \mathbf{u}_j + \varepsilon_{ij} $$

- $i$: グループ内の観測インデックス
- $j$: グループインデックス (例: 学校 ID)
- $\boldsymbol\beta$: **固定効果** (全グループ共通の係数)
- $\mathbf{u}_j \sim \text{Normal}(0, G)$: **ランダム効果** (グループ $j$ 固有)
- $\varepsilon_{ij} \sim \text{Normal}(0, \sigma^2)$: 観測ノイズ
- $\mathbf{z}_{ij}$: ランダム効果用の計画行列

### 2.2 ランダム切片モデル

最も簡単な GLMM は **ランダム切片**:

$$ y_{ij} = \beta_0 + \beta_1 x_{ij} + u_j + \varepsilon_{ij}, \quad u_j \sim \text{Normal}(0, \sigma_u^2) $$

「グループごとに切片だけが違う、傾きは共通」。$\sigma_u^2$ がグループ間ばらつきの大きさ。

### 2.3 ランダム傾きモデル

切片だけでなく **傾きもグループで違う**:

$$ y_{ij} = (\beta_0 + u_{0j}) + (\beta_1 + u_{1j}) x_{ij} + \varepsilon_{ij} $$

$\mathbf{u}_j = (u_{0j}, u_{1j}) \sim \text{Normal}(0, G)$、$G$ は 2×2 共分散行列。
切片と傾きの相関が $G$ に入る。

### 2.4 ICC (Intraclass Correlation Coefficient)

ランダム切片モデルで「グループ内相関」:

$$ \text{ICC} = \frac{\sigma_u^2}{\sigma_u^2 + \sigma^2} $$

- 0: グループ無関係 (= 普通の LM で OK)
- 1: グループ内が完全に同一
- 0.05-0.30 が実データの典型

---

## 3. GLMM (一般化版)

応答が非正規分布のときの拡張:

$$ y_{ij} \mid \mathbf{u}_j \sim \text{ExpFamily}(\mu_{ij}), \quad g(\mu_{ij}) = \mathbf{x}_{ij}^T \boldsymbol\beta + \mathbf{z}_{ij}^T \mathbf{u}_j $$

例:

| ファミリ | リンク | 用途 |
|---|---|---|
| Binomial | Logit | 階層ロジスティック回帰 |
| Poisson | Log | 階層ポアソン (= 過分散カウント) |
| Gamma | Log | 階層待ち時間 |

LM の `LME` (Linear Mixed Effects) は Gaussian + Identity の特殊形。

---

## 4. 混合分布: 負の二項分布の導出

### 4.1 動機

実データのカウント (例: 商品の販売数、保険請求件数) は **Poisson 分布より分散が大きい**
(過分散) ことが多い。これはなぜか? どう対処するか?

**鍵**: rate $\lambda$ がグループ・観測ごとに **揺らいでいる**。
個々の $\lambda$ で条件付きでは Poisson だが、$\lambda$ で周辺化すると違う分布に。

### 4.2 階層モデルとして

```text
λ ~ Gamma(α, β)          ← rate がガンマ分布で揺らぐ
y | λ ~ Poisson(λ)       ← 与えられた rate で Poisson
```

これを **$y$ について** 周辺化 (= $\lambda$ で積分) すると:

$$ p(y) = \int p(y \mid \lambda) p(\lambda) d\lambda $$

### 4.3 導出 (式)

Poisson の PMF: $p(y \mid \lambda) = \frac{\lambda^y e^{-\lambda}}{y!}$

Gamma の PDF (shape α, rate β): $p(\lambda) = \frac{\beta^\alpha}{\Gamma(\alpha)} \lambda^{\alpha-1} e^{-\beta \lambda}$

積を積分:

$$ p(y) = \int_0^\infty \frac{\lambda^y e^{-\lambda}}{y!} \cdot \frac{\beta^\alpha \lambda^{\alpha-1} e^{-\beta\lambda}}{\Gamma(\alpha)} d\lambda $$

$$ = \frac{\beta^\alpha}{y! \Gamma(\alpha)} \int_0^\infty \lambda^{y+\alpha-1} e^{-(1+\beta)\lambda} d\lambda $$

積分は Gamma 関数の定義から $\Gamma(y+\alpha) / (1+\beta)^{y+\alpha}$:

$$ p(y) = \frac{\Gamma(y+\alpha)}{y! \Gamma(\alpha)} \cdot \frac{\beta^\alpha}{(1+\beta)^{y+\alpha}} $$

$p = \beta/(1+\beta)$、$1-p = 1/(1+\beta)$ とおくと:

$$ \boxed{p(y) = \binom{y+\alpha-1}{y} p^\alpha (1-p)^y} $$

これが **負の二項分布** $\text{NegBin}(\alpha, p)$。
別パラメタ化: $\mu = \alpha(1-p)/p$ (= mean)、分散 $\mu + \mu^2/\alpha$ (Poisson より分散大)。

### 4.4 直観的な意味

- **Gamma** が $\lambda$ の事前分布 (= rate が揺らぐ大きさを表現)
- **Poisson** が $\lambda$ 与えてのカウント
- **NegBin** が y の周辺分布 (= 観測者から見える分布)

「**Poisson + Gamma 階層 = NegBin 周辺**」と覚える。
hanalyze の demo:

```bash
cabal run negbinom-demo  # μ=10, α=2 のデータで Poisson と NB を比較
```

### 4.5 他の混合分布の例

同じ「**Y は Poisson、rate が別分布**」のパターン:

| Rate の分布 | 周辺の y の分布 |
|---|---|
| Gamma | NegBin (上記) |
| LogNormal | Poisson-LogNormal (閉形式なし) |
| InverseGaussian | Sichel 分布 |

「**Y は Bernoulli、p が Beta**」:

| p の分布 | 周辺の y の分布 |
|---|---|
| Beta(α, β) | **Beta-Binomial** |

これは hanalyze に `BetaBinomial` 観測として実装済。
n=1 の場合は単に Bernoulli の周辺と同じだが、複数試行で過分散二項を表現。

### 4.6 GLMM 観点での意味

「**過分散 = ランダム効果が隠れている**」ことが多い。

例: 「店舗別の売上カウント」データで Poisson regression したら過分散。
これは **店舗ごとの未観測効果** $u_j$ が隠れているから。明示的に GLMM で:

$$ y_{ij} \mid u_j \sim \text{Poisson}(\mu_{ij} e^{u_j}), \quad u_j \sim \text{Normal}(0, \sigma^2) $$

これを $u$ で積分すると **Poisson-LogNormal**。
$u$ の分布を Gamma に置けば NegBin (上の導出)。

---

## 5. 計算手法

GLMM は周辺尤度に **積分** が含まれるため、最尤法が直接適用できません。
3 つの代表的解き方:

### 5.1 EM アルゴリズム (LME)

LME (Gaussian + Identity) の場合は **閉形式の EM** が使える。

#### EM の概要

ランダム効果 $\mathbf{u}_j$ を欠損変数とみなし:

- **E-step**: 現在の $(\boldsymbol\beta, \sigma_u^2, \sigma^2)$ で $\mathbf{u}_j$ の事後 (= **BLUP**) を計算
- **M-step**: $\mathbf{u}_j$ を固定して $(\boldsymbol\beta, \sigma_u^2, \sigma^2)$ を更新

LME では BLUP は閉形式 (正規 + 正規 = 正規):

$$ \hat{\mathbf{u}}_j = (\sigma_u^2 / (\sigma_u^2 + \sigma^2/n_j)) \cdot (\bar y_j - \bar X_j^T \hat{\boldsymbol\beta}) $$

これが **shrinkage estimator**: 各グループの「個別平均」を全体に向かって縮める。

#### hanalyze 実装

```haskell
-- src/Model/GLMM.hs
fitLME :: LA.Matrix Double         -- 固定効果計画
       -> LA.Vector Double          -- 応答
       -> V.Vector Int              -- 観測ごとのグループ ID
       -> V.Vector Text              -- グループラベル
       -> V.Vector Int              -- グループサイズ
       -> GLMMResult
```

`fitLMEDataFrame` で DataFrame から直接呼べる。

### 5.2 Laplace 近似 (一般 GLMM)

非 Gaussian な GLMM では EM が閉じない (E-step の積分が解けない)。
**Laplace 近似** で対処:

#### 概要

周辺尤度

$$ p(\mathbf{y} \mid \boldsymbol\theta) = \int p(\mathbf{y} \mid \mathbf{u}, \boldsymbol\theta) p(\mathbf{u} \mid \boldsymbol\theta) d\mathbf{u} $$

の被積分関数を **モード周辺の正規近似** で:

1. $\hat{\mathbf{u}} = \arg\max_{\mathbf{u}} \log p(\mathbf{y}, \mathbf{u} \mid \boldsymbol\theta)$ を Newton 法で
2. Hessian $H$ を計算
3. 積分を $\sqrt{(2\pi)^d / |H|} p(\mathbf{y}, \hat{\mathbf{u}} \mid \boldsymbol\theta)$ で近似

これを $\boldsymbol\theta$ で最適化 (= 最尤推定)。

#### 精度

- 1 階近似: 速いが精度限定 (= **PQL**, Penalized Quasi-Likelihood)
- 2 階近似: 標準的、精度良し
- **AGHQ** (Adaptive Gauss-Hermite Quadrature): 高精度だが重い

hanalyze は **Laplace 近似 (2 階)** を実装。

#### hanalyze 実装

```haskell
fitGLMM :: Family -> LinkFn
        -> LA.Matrix Double -> LA.Vector Double
        -> V.Vector Int -> V.Vector Text -> V.Vector Int
        -> GLMMResult
```

### 5.3 MCMC (フルベイズ)

最も一般的な対処: 全パラメタ ($\boldsymbol\beta, \mathbf{u}, \sigma_u^2, \sigma^2$) を
**事後分布から MCMC で sample**。

#### モデル例

```haskell
import Model.HBM

hierarchicalNormal :: ModelP ()
hierarchicalNormal = do
  muPop  <- sample "mu_pop"  (Normal 0 10)
  sigPop <- sample "sig_pop" (HalfNormal 5)
  -- ランダム切片: グループ J 個
  thetas <- mapM (\j -> sample ("theta_" <> tShow j)
                                (Normal muPop sigPop))
                 [1 .. nGroups]
  forM_ (zip thetas dataByGroup) $ \(theta, ys) ->
    observe ("y_" <> ...) (Normal theta sigY) ys
```

### 5.4 比較

| 手法 | 速度 | 精度 | 実装難度 |
|---|---|---|---|
| EM (LME) | 高速 | 厳密 | 中 |
| Laplace | 中 | 近似 (中精度) | 中 |
| AGHQ | 遅め | 高精度 | 高 |
| MCMC (NUTS) | 遅い | 真の事後 | 低 (DSL があれば) |
| VI (ADVI) | 非常に高速 | 平均場近似 | 中 |

実用的には:
- 大規模データ → Laplace
- 推論精度が重要 → MCMC
- 探索的分析 → VI で初期値、最終分析は MCMC

---

## 6. hanalyze での実装

### 6.1 LME (Gaussian + Identity)

```haskell
import Model.GLMM (fitLMEDataFrame, GLMMResult (..),
                   glmmFixed, glmmRandVar, glmmResidVar, glmmICC)
import Model.Core (coefficientsV)

case fitLMEDataFrame [("hours", 1)] "school" "score" df of
  Nothing -> putStrLn "fit failed"
  Just gr -> do
    let beta = coefficientsV (glmmFixed gr)
        ranV = glmmRandVar gr   -- σ_u²
        resV = glmmResidVar gr  -- σ²
        icc  = glmmICC gr       -- ICC
    print beta
    putStrLn $ "ICC = " ++ show icc
```

### 6.2 GLMM (Binomial / Poisson)

```haskell
import Model.GLMM (fitGLMMDataFrame)
import Model.GLM (Family (..), LinkFn (..))

case fitGLMMDataFrame Binomial Logit [("dose", 1)] "patient" "outcome" df of
  Just gr -> ...
```

### 6.3 CLI 経由

```bash
# LME (グループあり LM)
hanalyze data.csv x y LM --group school --report

# GLMM (グループあり GLM)
hanalyze data.csv x y GLM -d binomial -l logit --group hospital --report
```

### 6.4 ベイズ階層モデル (推奨)

複雑な階層は `Model.HBM` で書くのが柔軟:

```haskell
import Model.HBM

complexModel :: ModelP ()
complexModel = do
  -- ハイパー
  muPop   <- sample "mu_pop"   (Normal 0 10)
  sigPop  <- sample "sig_pop"  (HalfNormal 5)
  sigY    <- sample "sig_y"    (HalfNormal 3)
  -- ランダム切片 (非中心化推奨)
  thetas <- mapM (\j -> nonCenteredNormal ("theta_" <> tShow j)
                                          muPop sigPop)
                 [1 .. nGroups]
  -- 観測
  forM_ (zip thetas dataByGroup) $ \(theta, ys) ->
    observe ("y_" <> ...) (Normal theta sigY) ys
```

`hbm-example`、`simpson-paradox`、`hbm-random-slope` の demo を参照。

---

## 7. 非中心化パラメタ化

### 7.1 問題

Centered パラメタ化:
```haskell
theta_j <- sample ("theta_" <> tShow j) (Normal mu sigma)
```

データが少ないと **funnel** 形の事後分布 (Neal's funnel) で HMC が苦しむ。
具体的には σ が小さいと θ_j が μ 周辺に集中し、σ が大きいと広範に散らばる。
σ の posterior と θ の posterior が強く相関し、curvature が病的に。

### 7.2 解決: 非中心化

```haskell
-- 旧 (centered)
theta_j <- sample "theta" (Normal mu sigma)

-- 新 (non-centered, 推奨)
theta_raw <- sample "theta_raw" (Normal 0 1)
let theta_j = mu + sigma * theta_raw
```

これにより `theta_raw` は μ, σ から **独立**。HMC が安定する。

hanalyze の `nonCenteredNormal` ヘルパで簡略化:

```haskell
theta_j <- nonCenteredNormal "theta" mu sigma
-- 内部で raw を sample し、theta = mu + sigma * raw を deterministic で返す
```

実証: `noncentered-demo` で BFMI 0.65 → 1.02、ESS 7.6 倍改善、divergences 127 → 0。

---

## 8. 診断と落とし穴

### 8.1 過剰収縮

ランダム効果 $\sigma_u$ が小さく推定されると **shrinkage が強すぎ**。
グループ間差が消える。
→ ハイパー事前を弱情報 (HalfNormal(2) など) に変更。

### 8.2 アンバランスデータ

各グループの観測数が大きく違うと推定が不安定。
小さなグループは大きく shrink する (理論通り)。

### 8.3 ランダム効果が説明変数と相関

例: $x_{ij}$ がグループ平均と相関する場合、固定効果と混同される。
→ **between-group**(グループ平均) と **within-group**(平均からの偏差) を分離して計画行列に。

### 8.4 R-hat / divergence

GLMM (特に Binomial / Poisson) は Bayesian で解くと NUTS が苦しみがち。

- 非中心化必須
- BFMI < 0.3 や divergences > 5% は要警戒
- target_accept を 0.95 に上げる、step size を小さく

`energy-demo` / `noncentered-demo` を参照。

---

## 9. 参考文献

- **Pinheiro, J. C., Bates, D. M.** (2000). *Mixed-Effects Models in S and S-PLUS*. Springer.
  → 古典、LME の基礎。
- **Demidenko, E.** (2013). *Mixed Models: Theory and Applications with R* (2nd ed.). Wiley.
  → 数学的厳密、計算手法を網羅。
- **Bolker, B. M.** (2015). *Linear and Generalized Linear Mixed Models*. In Fox, Negrete-Yankelevich & Sosa (eds.), *Ecological Statistics: Contemporary Theory and Application*. Oxford.
  → 実践的、診断と注意点が豊富。
- **Gelman, A., Hill, J.** (2007). *Data Analysis Using Regression and Multilevel/Hierarchical Models*. Cambridge.
  → ベイズ寄り、partial pooling の哲学。
- **Hilbe, J. M.** (2011). *Negative Binomial Regression* (2nd ed.). Cambridge.
  → NegBin の導出と応用が詳しい (本 doc §4 のさらに詳細版)。

### 関連 hanalyze ドキュメント

- [01-lm.ja.md](01-lm.ja.md) — LM の基礎
- [02-glm.ja.md](02-glm.ja.md) — GLM (NegBin の議論あり)
- [theory-regression-extensions.ja.md](theory-regression-extensions.ja.md) — 理論
- [../bayesian/02-probabilistic-model.ja.md](../bayesian/02-probabilistic-model.ja.md) — Bayesian 階層モデル
- [../bayesian/theory-hmc-nuts.ja.md](../bayesian/theory-hmc-nuts.ja.md) — 非中心化と divergence
