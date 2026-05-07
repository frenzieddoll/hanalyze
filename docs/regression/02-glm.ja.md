# GLM (一般化線形モデル) — 全ファミリ網羅

> 🌐 [English](02-glm.md) | **日本語**

> hanalyze の `Model.GLM` は LM の自然な拡張で、応答変数が
> 正規分布以外 (二項・ポアソン・負の二項…) の場合をカバーします。
> 関連: [01-lm.ja.md](01-lm.ja.md) / [03-glmm.ja.md](03-glmm.ja.md)
> **多出力対応**: `fitGLMMulti` (列ごと IRLS) — 詳細は [05-multivariate.ja.md](05-multivariate.ja.md)。

## 目次

1. [GLM の 3 要素](#1-glm-の-3-要素)
2. [指数型分布族](#2-指数型分布族)
3. [リンク関数](#3-リンク関数)
4. [IRLS による推定](#4-irls-による推定)
5. [hanalyze での実装](#5-hanalyze-での実装)
6. [ファミリ別解説](#6-ファミリ別解説)
   - 6.1 [Gaussian (= LM)](#61-gaussian--lm)
   - 6.2 [Binomial (ロジスティック / プロビット)](#62-binomial-ロジスティック--プロビット)
   - 6.3 [Poisson (カウントデータ)](#63-poisson-カウントデータ)
   - 6.4 [NegativeBinomial (過分散)](#64-negativebinomial-過分散)
   - 6.5 [Gamma (正の連続値)](#65-gamma-正の連続値)
   - 6.6 [InverseGaussian](#66-inversegaussian)
7. [偏差と疑似 R²](#7-偏差と疑似-r²)
8. [診断](#8-診断)
9. [GLMM への接続](#9-glmm-への接続)
10. [よくある落とし穴](#10-よくある落とし穴)
11. [参考文献](#11-参考文献)

---

## 1. GLM の 3 要素

GLM (Generalized Linear Model, Nelder & Wedderburn 1972) は以下の 3 要素で定義:

1. **応答分布** (random component): $y_i$ が指数型分布族の何かに従う
   $$ y_i \mid \mu_i \sim \text{ExponentialFamily}(\mu_i, \phi) $$
2. **線形予測子** (systematic component): 説明変数の線形結合
   $$ \eta_i = X_i \boldsymbol\beta $$
3. **リンク関数** (link function): 平均 $\mu_i$ と $\eta_i$ を結ぶ
   $$ g(\mu_i) = \eta_i, \quad \text{つまり} \quad \mu_i = g^{-1}(\eta_i) $$

LM は GLM の特殊形 (応答 = Normal、リンク = Identity)。

---

## 2. 指数型分布族

指数型分布族の標準形:

$$ p(y; \theta, \phi) = \exp\!\left( \frac{y\theta - b(\theta)}{\phi} + c(y, \phi) \right) $$

- $\theta$: 自然パラメタ
- $\phi$: 散布パラメタ (Gaussian の $\sigma^2$ など)
- $b(\theta)$: 累積関数 ($E[Y] = b'(\theta)$, $\text{Var}(Y) = \phi b''(\theta)$)

| 分布 | $\theta$ | $\phi$ | $b(\theta)$ | mean | variance |
|---|---|---|---|---|---|
| Normal $(\mu, \sigma^2)$ | $\mu$ | $\sigma^2$ | $\theta^2 / 2$ | $\theta$ | $\sigma^2$ |
| Bernoulli/Binomial | $\log\frac{p}{1-p}$ | 1 | $n\log(1+e^\theta)$ | $np$ | $np(1-p)$ |
| Poisson $(\lambda)$ | $\log \lambda$ | 1 | $e^\theta$ | $\lambda$ | $\lambda$ |
| Gamma $(\mu, \nu)$ | $-1/\mu$ | $1/\nu$ | $-\log(-\theta)$ | $\mu$ | $\mu^2 / \nu$ |
| Inverse Gaussian | $-1/(2\mu^2)$ | $1/\lambda$ | $-\sqrt{-2\theta}$ | $\mu$ | $\mu^3/\lambda$ |
| NegativeBinomial $(\mu, \alpha)$ | $\log\frac{\mu}{\mu+\alpha}$ | 1 (固定 α) | $-\alpha \log(1-e^\theta)$ | $\mu$ | $\mu + \mu^2/\alpha$ |

**分散関数** $V(\mu) = b''(\theta(\mu))$ が分布によって違うのが GLM の本質。

---

## 3. リンク関数

各分布には **正準リンク** (canonical link) があります — これは $\theta$ 自体を $\eta$ にマップする関数:

| 分布 | 正準リンク | $g(\mu)$ | $g^{-1}(\eta)$ |
|---|---|---|---|
| Normal | Identity | $\mu$ | $\eta$ |
| Binomial | Logit | $\log(\mu/(1-\mu))$ | $1/(1+e^{-\eta})$ |
| Poisson | Log | $\log \mu$ | $e^\eta$ |
| Gamma | Inverse | $1/\mu$ | $1/\eta$ |
| Inverse Gaussian | Inverse² | $1/\mu^2$ | $1/\sqrt\eta$ |

正準リンクには良い性質 (十分統計量、Fisher 情報 = 観測情報) がありますが、
**他のリンクも使える**:

- **Probit** (Binomial, $\Phi^{-1}(\mu)$ — Φ は標準正規 CDF)
- **Cloglog** (Binomial, $\log(-\log(1-\mu))$ — 生存解析寄り)
- **Sqrt** (Poisson, $\sqrt\mu$ — variance stabilizing)

hanalyze は `Identity / Log / Logit / Sqrt` をサポート。

---

## 4. IRLS による推定

### 4.1 最尤推定の困難

LM は閉形式 ($\hat\beta = (X^T X)^{-1} X^T y$) ですが、GLM は **非線形** (リンク経由) のため
反復的に解きます。

### 4.2 IRLS (Iteratively Reweighted Least Squares)

各反復で **重み付き OLS** を解く:

```text
1. 初期 β₀ を設定
2. 反復:
   η = X β
   μ = g⁻¹(η)
   z = η + (y - μ) / g'(μ)        # 作業応答
   W = diag(1 / (g'(μ)² V(μ)))   # 重み
   β_new = (Xᵀ W X)⁻¹ Xᵀ W z
3. ||β_new - β|| < tol で停止
```

### 4.3 直観

- $z$ は「線形予測子の足元での y のテイラー展開」
- $W$ は「分散が大きい点を信用しない」
- 結局、**重み付き平均」を反復で更新する**

### 4.4 hanalyze の実装

```haskell
-- src/Model/GLM.hs より抜粋
runIRLS :: Family -> LinkFn -> LA.Matrix Double -> LA.Vector Double
        -> (FitResult, LA.Matrix Double)
runIRLS family linkFn x y = ...
  where
    step = irlsStep link (varOf family) safeMu family x y
    converge n beta
      | norm < tol = beta
      | otherwise  = converge (n - 1) (step beta)
```

`Fisher 情報行列の逆行列` も同時に返す (信頼区間や Bayesian Laplace 近似用)。

---

## 5. hanalyze での実装

### 5.1 主要 API

```haskell
import Model.GLM
import Model.Core (FitResult)

-- ファミリ型
data Family = Gaussian | Binomial | Poisson
  deriving (Show, Eq)

-- リンク関数型
data LinkFn = Identity | Log | Logit | Sqrt
  deriving (Show, Eq)

-- 基本 fit (canonical link 使用)
fitGLM :: Family -> LA.Matrix Double -> LA.Vector Double -> FitResult

-- Fisher 情報も返す版
fitGLMFull :: Family -> LinkFn -> LA.Matrix Double -> LA.Vector Double
           -> (FitResult, LA.Matrix Double)

-- DataFrame + 信頼帯付き
fitGLMWithSmooth :: Family -> LinkFn -> [(Text, Int)] -> Band -> Int
                 -> DataFrame -> Text
                 -> Maybe (FitResult, Maybe SmoothFit)
```

### 5.2 最小例: ロジスティック回帰

```haskell
import Model.GLM
import Model.Core (coefficientsV)

let xs   = ...                                -- 計画行列 (1 列目は切片の 1)
    ys   = LA.fromList [0, 1, 1, 0, 1, ...]   -- 0/1 応答
    fit  = fitGLM Binomial xs ys             -- canonical link = Logit
    beta = coefficientsV fit                  -- ロジット空間の係数
    -- 確率: p = 1 / (1 + exp(-Xβ))
```

### 5.3 ポアソン回帰 (カウントデータ)

```haskell
let cnt  = LA.fromList [3, 5, 0, 2, 7, ...]   -- 非負整数
    fit  = fitGLM Poisson xs cnt             -- canonical = Log
    beta = coefficientsV fit
    -- 期待数: λ = exp(Xβ)
```

### 5.4 CLI 経由

```bash
# ロジスティック回帰
hanalyze data.csv x y GLM -d binomial -l logit --report

# ポアソン回帰
hanalyze data.csv x y GLM -d poisson -l log --report

# プロビット (Binomial + Probit) は CLI 未対応、Haskell から呼ぶ
```

---

## 6. ファミリ別解説

### 6.1 Gaussian (= LM)

応答が **連続実数**、誤差が正規分布のとき:

$$ y_i \sim \text{Normal}(\mu_i, \sigma^2), \quad \mu_i = X_i \boldsymbol\beta $$

LM と等価。`Model.LM.fitLM` を使うべき (IRLS 不要、閉形式)。
`fitGLM Gaussian xs ys` でも同じ結果。

### 6.2 Binomial (ロジスティック / プロビット)

応答が **0/1 の二値** または **n 試行中の成功数 k**。

#### 6.2.1 数式

$$ y_i \sim \text{Binomial}(n_i, p_i), \quad g(p_i) = X_i \boldsymbol\beta $$

リンク関数の選択:

| リンク | $g(p)$ | $g^{-1}(\eta)$ | 用途 |
|---|---|---|---|
| **Logit** (canonical) | $\log\frac{p}{1-p}$ | $\sigma(\eta) = \frac{1}{1+e^{-\eta}}$ | 標準的なロジスティック回帰 |
| **Probit** | $\Phi^{-1}(p)$ | $\Phi(\eta)$ | プロビットモデル |
| **Cloglog** | $\log(-\log(1-p))$ | $1 - \exp(-e^\eta)$ | 「最初に発生する時刻」の解析 |

実用上は **Logit と Probit でほぼ同じ結果**。Logit の係数 = "log odds ratio" として解釈しやすい。

#### 6.2.2 hanalyze

```haskell
fitGLM Binomial xs ys       -- canonical = Logit
fitGLMFull Binomial Logit xs ys  -- Logit を明示
-- Probit は現状未サポート (Logit と同等の結果が出る)
```

#### 6.2.3 解釈

ロジスティック回帰で $\beta_j = 0.5$ なら:

- $x_j$ が 1 単位増えると **log odds が +0.5**
- 等価に **odds が $e^{0.5} ≈ 1.65$ 倍** (= 65% 増)

#### 6.2.4 例: 臨床試験

```haskell
-- 治療の成否 (0/1) を年齢と治療群で予測
let xs = LA.fromColumns
           [ LA.konst 1 n             -- 切片
           , LA.fromList ages         -- 年齢
           , LA.fromList treatments   -- 0=コントロール / 1=治療群
           ]
    fit = fitGLM Binomial xs (LA.fromList outcomes)
```

### 6.3 Poisson (カウントデータ)

応答が **非負整数** (件数、頻度):

$$ y_i \sim \text{Poisson}(\lambda_i), \quad \log \lambda_i = X_i \boldsymbol\beta $$

特徴: **平均 = 分散** ($E[Y] = \text{Var}(Y) = \lambda$)。

#### 6.3.1 hanalyze

```haskell
fitGLM Poisson xs counts   -- canonical = Log
```

#### 6.3.2 解釈

$\beta_j = 0.3$ なら:

- $x_j$ が 1 単位増えると **log expected count が +0.3**
- 等価に **expected count が $e^{0.3} ≈ 1.35$ 倍** (= 35% 増)

#### 6.3.3 オフセット (Exposure)

「異なる観測時間で件数を測った」場合、log(時間) を **オフセット** として固定:

$$ \log \lambda_i = X_i \boldsymbol\beta + \log T_i $$

→ $\lambda_i = T_i \cdot \exp(X_i \boldsymbol\beta)$。
レート (件数/時間) を回帰しているのと等価。
hanalyze はオフセット引数なし、計画行列に列追加して係数を 1 に固定する手動対処。

### 6.4 NegativeBinomial (過分散)

#### 6.4.1 動機

Poisson は「**平均 = 分散**」を仮定するが、現実のカウントデータは **平均 < 分散** (過分散)
が多い。これを許容する分布が NegativeBinomial。

$$ y_i \sim \text{NegativeBinomial}(\mu_i, \alpha) $$

平均 $\mu_i$、分散 $\mu_i + \mu_i^2/\alpha$ ($\alpha \to \infty$ で Poisson に収束)。

#### 6.4.2 GLM としての扱い

- リンク: Log ($\log \mu = X\beta$)
- $\alpha$ は **ニューサンスパラメタ** (= 別途推定)

しかし NegativeBinomial は **正規の指数型分布族 ではない** (α が固定なら family、free なら違う)。
hanalyze の `fitGLM` には現状ファミリとして含まれていないが、
ベイズ枠組み (`Model.HBM` の `NegativeBinomial`) では使えます:

```haskell
-- ベイズ NB regression (Model.HBM)
nbModel :: ModelP ()
nbModel = do
  beta  <- mapM (\j -> sample ("b" <> tShow j) (Normal 0 5)) [0..p-1]
  alpha <- sample "alpha" (HalfNormal 5)
  let mus = [exp (sum (zipWith (*) (xRow i) beta)) | i <- [0..n-1]]
  observe "y" (NegativeBinomial (mus !! ...) alpha) ys
```

#### 6.4.3 NegativeBinomial の導出 (Gamma-Poisson 混合)

これは [03-glmm.ja.md](03-glmm.ja.md) で詳しく解説しますが、要点:

> $\lambda \sim \text{Gamma}(\alpha, \beta)$、$y \mid \lambda \sim \text{Poisson}(\lambda)$ を周辺化すると $y \sim \text{NegativeBinomial}$。

つまり「**Poisson の rate がガンマ分布で揺らいでいる**」モデル。
これが過分散の起源。`negbinom-demo` で実証。

### 6.5 Gamma (正の連続値)

応答が **正の連続値** (時間、費用、気象データなど):

$$ y_i \sim \text{Gamma}(\nu, \nu/\mu_i), \quad g(\mu_i) = X_i \boldsymbol\beta $$

- 形状 $\nu$ 固定、scale $= \nu/\mu$ で平均 $\mu$、分散 $\mu^2/\nu$
- リンク: Inverse (canonical) / Log / Identity

#### 用途
- 待ち時間 (insurance claim 額、修理時間)
- 等分散性違反の対処 (応答 log 変換の代わりに)

#### hanalyze
現在 `Model.GLM` には Gamma family は未実装。代替:
- 応答を log 変換して LM
- ベイズ枠組み (`Model.HBM.Gamma`) で書く

### 6.6 InverseGaussian

応答が **正の連続値** で **分散が平均の 3 乗** に比例 (Wald 分布):

$$ \text{Var}(Y) = \mu^3 / \lambda $$

- 用途: 工学的な耐久試験
- hanalyze 未実装

---

## 7. 偏差と疑似 R²

### 7.1 Deviance (偏差)

LM の RSS (残差平方和) の GLM 版:

$$ D(\mathbf{y}, \hat{\boldsymbol\mu}) = 2 \left[ \log L(\mathbf{y}; \mathbf{y}) - \log L(\hat{\boldsymbol\mu}; \mathbf{y}) \right] $$

これは「飽和モデル (saturated, 各観測に専用パラメタ) 対 fit したモデル」の対数尤度比。
データが多変数で説明されるほど D は小さい。

### 7.2 各分布の deviance 公式

- **Gaussian**: $D = \sum (y_i - \hat\mu_i)^2 / \sigma^2$ (= RSS)
- **Binomial**: $D = 2 \sum [y_i \log(y_i / \hat\mu_i) + (n_i - y_i) \log((n_i - y_i)/(n_i - \hat\mu_i))]$
- **Poisson**: $D = 2 \sum [y_i \log(y_i / \hat\mu_i) - (y_i - \hat\mu_i)]$

### 7.3 疑似 R²

LM の R² と類似の指標。複数の流派:

| 名前 | 定義 |
|---|---|
| **McFadden's** | $1 - \log L(\hat) / \log L(\text{null})$ |
| **Cox-Snell** | $1 - (L_{\text{null}}/L_{\hat})^{2/n}$ |
| **Nagelkerke** | Cox-Snell の正規化版 |
| **Deviance** | $1 - D(\hat) / D(\text{null})$ |

hanalyze の `pseudoR2` (`Model.GLM`) は **Deviance ベース** を採用。
LM の R² と直接比較しないこと (尺度が違う)。

---

## 8. 診断

### 8.1 残差の種類

GLM では複数の残差定義:

| 名前 | 定義 |
|---|---|
| **Pearson** | $r_i^P = (y_i - \hat\mu_i) / \sqrt{V(\hat\mu_i)}$ |
| **Deviance** | $r_i^D = \text{sign}(y_i - \hat\mu_i) \sqrt{d_i}$, $d_i$ は単点 deviance |
| **Working** | IRLS の作業応答 $z - \eta$ |

**Deviance 残差** が最も推奨 (近似的に N(0,1) 分布)。

### 8.2 過分散 (Overdispersion)

Poisson / Binomial で:

$$ \hat\phi = \frac{\sum (r_i^P)^2}{n - p - 1} $$

- $\hat\phi \approx 1$: OK
- $\hat\phi > 1.5$: 過分散の可能性

対処:
1. **Quasi-likelihood**: 分散を $\phi V(\mu)$ に置換 (係数推定は同じ、SE が広がる)
2. **NegativeBinomial** (Poisson の代替)
3. **Beta-Binomial** (Binomial の代替, hanalyze の `BetaBinomial` 観測)
4. **混合効果 (GLMM)** で残差相関を吸収 ([03-glmm.ja.md](03-glmm.ja.md))

### 8.3 影響点

LM と同様、leverage と Cook's distance で検出。GLM 用の調整版あり。

---

## 9. GLMM への接続

GLM の限界:
- **観測が独立** という仮定
- 実データはグループ構造 (患者 ∈ 病院、生徒 ∈ 学校) で相関しがち

→ **GLMM (Generalized Linear Mixed Model)** で対処。
**ランダム効果**を加えてグループ間ばらつきをモデル化:

$$ g(\mu_{ij}) = X_{ij} \boldsymbol\beta + Z_{ij} \mathbf{u}_j, \quad \mathbf{u}_j \sim \text{Normal}(0, G) $$

詳細は [03-glmm.ja.md](03-glmm.ja.md) を参照。

### 例: 過分散 Poisson の GLMM 解釈

「過分散 Poisson」は実は **観測ごとにランダム効果** を入れた GLMM の周辺化:

$$ y_i \mid u_i \sim \text{Poisson}(\mu_i e^{u_i}), \quad u_i \sim \text{Normal}(0, \sigma_u^2) $$

これを $u_i$ で積分すると **Poisson-LogNormal** に。
ランダム効果が **Gamma** だと **NegativeBinomial** ([03-glmm.ja.md §混合分布](03-glmm.ja.md))。

---

## 10. よくある落とし穴

### 10.1 完全分離 (Complete Separation)

ロジスティック回帰で **クラス分離が完全** (例: 男性は全員 1、女性は全員 0)
だと係数が **無限大に発散** し、IRLS が収束しません。

対処:
- データを確認 (sample 不足の指標)
- **Firth 修正** (= ペナルティ最尤、hanalyze 未実装)
- **Bayesian** で弱情報事前を入れる

### 10.2 リンク関数選択ミス

Probit / Logit はほぼ同じだが、**Cloglog vs Logit** は形が違う。
データの生成過程を考えて選ぶ。

### 10.3 Poisson の過分散

**経験則**: 観測カウント データは大抵過分散。
NegativeBinomial や混合効果を疑う。

### 10.4 オフセットを切片扱い

オフセット (= 既知の $\log T$) は係数を 1 に固定する必要あり。
通常の説明変数として入れると意味が変わる。

### 10.5 解釈の誤り

「ロジスティック回帰の係数 = 確率の差」ではない (= log odds の差)。
- $\beta_j = 0.5$ は確率変化 0.5 ではなく、odds が $e^{0.5}$ 倍。
- 確率変化を見るには予測値で計算。

---

## 11. 参考文献

- **McCullagh, P., Nelder, J. A.** (1989). *Generalized Linear Models* (2nd ed.). Chapman & Hall.
  → GLM のバイブル。理論が体系的。
- **Agresti, A.** (2015). *Foundations of Linear and Generalized Linear Models*. Wiley.
  → 教科書として読みやすい、例題豊富。
- **Dobson, A. J., Barnett, A. G.** (2018). *An Introduction to Generalized Linear Models* (4th ed.). CRC.
  → 入門向け、実践的。
- **Hilbe, J. M.** (2011). *Negative Binomial Regression* (2nd ed.). Cambridge.
  → 過分散カウントの専門書。
- **Long, J. S.** (1997). *Regression Models for Categorical and Limited Dependent Variables*. Sage.
  → 二値 / 順序 / 多項 / トランケート の包括解説。

### 関連 hanalyze ドキュメント

- [01-lm.ja.md](01-lm.ja.md) — 線形回帰の基礎 (GLM の特殊形)
- [03-glmm.ja.md](03-glmm.ja.md) — 混合効果モデル (グループ構造)
- [theory-regression-extensions.ja.md](theory-regression-extensions.ja.md) — 理論
- [../bayesian/02-probabilistic-model.ja.md](../bayesian/02-probabilistic-model.ja.md) — Bayesian GLM (HBM 経由)
