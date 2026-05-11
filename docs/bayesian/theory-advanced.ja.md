# 学習資料 — VI / モデル選択 / 高度トピック (入門者向け再整理)

> **想定読者**: ベイズ統計の基礎 (事前/尤度/事後、共役、HBM の概念) を
> 学んだ人。各トピックを **用語の定義から** 噛み砕いて説明し、
> 「次に何を勉強すればよいか」の道標を示します。
>
> 関連: [theory-bayesian-basics.ja.md](theory-bayesian-basics.ja.md) /
> [theory-mcmc.ja.md](theory-mcmc.ja.md) /
> [theory-hmc-nuts.ja.md](theory-hmc-nuts.ja.md)

## 目次

1. [Variational Inference (VI) — 事後を最適化で求める](#1-variational-inference-vi--事後を最適化で求める)
2. [モデル選択 — WAIC / PSIS-LOO](#2-モデル選択--waic--psis-loo)
3. [Mixture (混合分布)](#3-mixture-混合分布)
4. [LKJ — 相関行列の事前分布](#4-lkj--相関行列の事前分布)
5. [AR / 状態空間モデル](#5-ar--状態空間モデル)
6. [**Truncated 分布** — 切り詰め (★重点)](#6-truncated-分布--切り詰め-重点)
7. [**Censored 分布** — 打ち切り (★重点)](#7-censored-分布--打ち切り-重点)
8. [どれをいつ使うか](#8-どれをいつ使うか)
9. [次に学ぶこと](#9-次に学ぶこと)
10. [参考文献](#10-参考文献)

---

## 1. Variational Inference (VI) — 事後を最適化で求める

### 1.1 何が問題か

ベイズ推論で事後 $p(\theta \mid y)$ を求めたいが、

- **正規化定数 $p(y) = \int p(y \mid \theta) p(\theta) d\theta$ が高次元積分で計算不能**
- MCMC は正確だが **遅い** (収束まで数千〜数万反復)

→ **VI** で事後を「**簡単な分布で近似**」する。最適化問題に帰着するので速い。

### 1.2 用語の定義

- **変分分布** (variational distribution) $q(\theta; \phi)$:
  事後を近似する分布。パラメタ $\phi$ を持つ (= 通常の確率分布のパラメタ μ, σ など)。
- **平均場近似** (mean-field): $q(\theta) = \prod_i q_i(\theta_i)$
  パラメタ間を独立とみなす最も簡単な選択。
- **KL ダイバージェンス** (Kullback-Leibler):
  2 分布の "距離" (非対称、非負)。0 のとき同一。

### 1.3 ELBO の導出 (噛み砕き版)

我々が求めたいのは「$q$ が事後 $p(\theta \mid y)$ にどれだけ近いか」。
**KL divergence** で測る:

$$ \text{KL}(q \,\|\, p_{\text{post}}) = E_q\!\left[\log q(\theta) - \log p(\theta \mid y)\right] $$

ここで $p(\theta \mid y) = p(\theta, y) / p(y)$ なので:

$$ \text{KL} = E_q[\log q(\theta) - \log p(\theta, y)] + \log p(y) $$

$\log p(y)$ は $\theta$ によらない定数。第 1 項を **負の** ELBO と呼ぶ:

$$ \text{ELBO}(\phi) = E_q[\log p(\theta, y) - \log q(\theta; \phi)] $$

すると:

$$ \log p(y) = \text{ELBO}(\phi) + \text{KL}(q_\phi \| p_{\text{post}}) $$

$\log p(y)$ は定数、$\text{KL} \ge 0$ なので **ELBO は $\log p(y)$ の下限** (lower bound)。
**ELBO 最大化 = KL 最小化** = 事後への最良近似。

### 1.4 ADVI: 自動 VI

Kucukelbir et al. (2017)。任意のモデルに自動適用:

1. 全 latent を **unconstrained 空間** (`PositiveT`/`UnitIntervalT` 等) に変換
2. 各成分を **Normal**$(\mu_i, \sigma_i)$ で近似 (= 平均場)
3. ELBO 勾配を AD (自動微分) で計算
4. **Adam** などで最適化

### 1.5 hanalyze での使い方

```haskell
import Stat.VI (advi, defaultVIConfig, VIConfig (..), VIResult (..))

result <- advi model defaultVIConfig
                  { viIterations = 5000, viLearningRate = 0.05 }
                init0 gen
-- result.viParams :: Map Text (Double, Double)  -- (μ, σ) per param
-- result.viELBO   :: [Double]                    -- 反復ごとの ELBO 履歴
```

`vi-demo` で NUTS と精度比較。VI は ~700 倍速。

### 1.6 VI の弱点

- **平均場の制約**: パラメタ間の相関を捉えない (= sd を過小評価しがち)
- **多峰分布に弱い**: 1 つの峰しか捕えない
- **裾の評価が悪い**: VI は KL(q‖p) を最小化するので、p の裾を q が無視しがち
  (= "mode-seeking")

→ 探索的分析や初期値選定で VI、最終分析は MCMC、が実用的。

---

## 2. モデル選択 — WAIC / PSIS-LOO

### 2.1 何のための指標か

「複数モデルから良いものを選ぶ」基準。**観測データへの fit ではなく、
新観測への予測精度** を評価する (= out-of-sample 性能)。

### 2.2 用語の定義

- **elpd** (expected log predictive density):
  $\text{elpd} = E_{p(\tilde y)}[\log p(\tilde y \mid y)]$
  「新観測 $\tilde y$ に対する対数予測密度の期待値」。**大きい (= 0 に近い負の値) ほど良い**。
- **WAIC** (Widely Applicable Information Criterion, Watanabe 2010):
  事後サンプルから elpd を直接近似する指標。
- **PSIS-LOO** (Pareto Smoothed Importance Sampling LOO-CV, Vehtari 2017):
  Leave-One-Out 交差検証 を重要度サンプリングで近似。

### 2.3 WAIC の式

$$ \widehat{\text{elpd}}_{\text{WAIC}} = \sum_{i=1}^n \log\!\left(\frac{1}{S}\sum_{s=1}^S p(y_i \mid \theta^{(s)})\right) - \sum_i \text{Var}_s\!\left(\log p(y_i \mid \theta^{(s)})\right) $$

第 1 項: 事後予測密度の対数平均
第 2 項: **有効パラメタ数 $p_{\text{WAIC}}$** (過適合ペナルティ)

### 2.4 PSIS-LOO の概要

LOO-CV は通常 $n$ 回 fit が必要 ($O(n)$ コスト)。
**重要度サンプリング** で 1 回の事後 sample から $n$ 個の LOO 予測を近似:

$$ p(y_i \mid y_{-i}) \approx \frac{\sum_s w_i^{(s)} p(y_i \mid \theta^{(s)})}{\sum_s w_i^{(s)}} $$

重みの分布が裾重いと不安定。**Pareto** 分布で平滑化 (PSIS):

- 上位 M 個の重みから Pareto の形状 $\hat k$ を推定
- $\hat k < 0.5$: OK
- $0.5 \le \hat k < 0.7$: 注意
- $\hat k \ge 0.7$: その観測点は信頼性低い (= 該当点を除外して MCMC 再実行を推奨)

### 2.5 hanalyze での使用

```haskell
import Stat.ModelSelect (waic, loo, compareModels, ModelInfo (..))

let waicResult = waic loglikMatrix    -- loglik :: [[Double]]  (S × N)
let looResult  = loo  loglikMatrix
-- looKHat looResult :: [Double]       (各観測の k̂)

-- 複数モデル比較
let cmps = compareModels [ ModelInfo "M1" loglik1
                         , ModelInfo "M2" loglik2 ]
-- 各モデルの elpd, se, 重み (Pseudo-BMA)
```

### 2.6 Pseudo-BMA (モデル平均化)

複数モデルの予測を **重み付き平均**:

$$ w_k = \frac{\exp(\text{elpd}_k)}{\sum_l \exp(\text{elpd}_l)} $$

「モデル選択 (1 つを選ぶ)」より「モデル平均化 (複数を確率重みで使う)」が
ロバストなことが多い。

---

## 3. Mixture (混合分布)

### 3.1 用語の定義

**混合分布** (mixture distribution):

$$ p(x) = \sum_{k=1}^K w_k \, p_k(x), \quad \sum_k w_k = 1, \, w_k \ge 0 $$

各 $p_k$ が **成分** (component)、$w_k$ が **混合比** (mixing weight)。

### 3.2 用途

- **クラスタリング**: データが複数の正規分布の混合と仮定 → 各点がどの成分由来か推論
- **外れ値モデル**: 主成分 + 「広い」成分を混合 → 外れ値を許容
- **ヘテロシダスティック** な誤差: 異なる分散を持つ成分の混合
- **柔軟な分布**: GMM (Gaussian Mixture Model) で任意分布を近似

### 3.3 log-sum-exp

ログ空間で計算するための数値安定化:

$$ \log p(x) = \log \sum_k w_k p_k(x) = \text{logsumexp}(\log w_k + \log p_k(x)) $$

`logSumExpA` ヘルパで実装。

### 3.4 hanalyze

```haskell
mix <- sample "x" (Mixture [0.3, 0.7] [Normal 0 1, Normal 5 2])
-- 30% 確率で Normal(0, 1), 70% 確率で Normal(5, 2)
```

### 3.5 ラベルスイッチング問題

**MCMC で混合モデルを fit** すると、成分の番号が反復ごとに入れ替わる
(= 同じ事後分布なので尤度が変わらない)。

対処:
- 順序制約 ($\mu_1 < \mu_2 < \cdots$) を `potential` で課す
- post-hoc に成分を整列

---

## 4. LKJ — 相関行列の事前分布

### 4.1 動機

多変量正規 $\text{MvN}(\boldsymbol\mu, \Sigma)$ で **共分散行列 $\Sigma$** を推定したい。
$\Sigma$ は対称正定値で、構造が複雑。

**LKJ 分解**:

$$ \Sigma = \text{diag}(\boldsymbol\sigma) \, R \, \text{diag}(\boldsymbol\sigma) $$

- $\boldsymbol\sigma$: 各次元の sd (= **scale**)
- $R$: 相関行列 (対角 1、対称正定値)

各々に独立な事前を置く:
- $\sigma_i \sim \text{HalfNormal}$
- $R \sim \text{LKJ}(\eta)$

### 4.2 LKJ 分布 (Lewandowski-Kurowicka-Joe 2009)

K×K 相関行列上の分布。確率密度:

$$ p(R) \propto |R|^{\eta - 1} $$

| $\eta$ | 性質 |
|---|---|
| $\eta = 1$ | uniform on correlation matrices |
| $\eta > 1$ | I (単位行列) に集中 (= 弱い相関) |
| $\eta < 1$ | 相関が ±1 に集中 |

### 4.3 hanalyze の実装

```haskell
import Model.HBM (lkjCorrCholesky)

l <- lkjCorrCholesky "R" 3 1.0  -- K=3, η=1
-- l :: [[a]] は L (R = L Lᵀ の Cholesky factor)
```

内部は **CPC (Canonical Partial Correlations)** 法で K(K-1)/2 個の Beta 変数を sample。

### 4.4 用途

- 多変量階層モデルの共分散事前
- 多出力 GP の出力間相関 (`Multi-output GP`)
- ランダム傾きモデルで切片と傾きの相関

---

## 5. AR / 状態空間モデル

### 5.1 AR(1) モデル

時系列の最も基本的なモデル:

$$ x_t = \phi x_{t-1} + \varepsilon_t, \quad \varepsilon_t \sim \text{Normal}(0, \sigma) $$

- $|\phi| < 1$ で **定常**: 長期平均 = 0、分散 = $\sigma^2 / (1-\phi^2)$
- $\phi = 1$ で **ランダムウォーク**

### 5.2 状態空間モデル

潜在状態 $x_t$ + 観測 $y_t$:

```text
x_t = φ x_{t-1} + ε_t        (状態方程式)
y_t = x_t + η_t              (観測方程式)
```

ノイズ $\varepsilon_t$ と $\eta_t$ を別に扱うので **ノイズ除去** が可能。

### 5.3 hanalyze の `ar1Latent`

```haskell
xs <- ar1Latent "x" T phi sigma
-- 内部:
--   raw_t ~ Normal(0, 1)
--   x_0 = (σ / √(1-φ²)) × raw_0           (定常分布)
--   x_t = φ x_{t-1} + σ × raw_t           (t > 0)
-- 全部 raw_t は独立な Normal で、x_t は派生量として保存
```

これは **非中心化パラメタ化** ([§7 — non-centered](theory-hmc-nuts.ja.md))。
HMC が安定。

### 5.4 拡張

- **AR(p)**: 過去 p ステップに依存
- **VAR**: 多変量時系列
- **Kalman filter**: 線形ガウス状態空間の最尤推定 (hanalyze 未実装)

---

## 6. Truncated 分布 — 切り詰め (★重点)

### 6.1 状況: なぜ必要か

「**観測が特定の範囲内のみ** で、範囲外の値は **そもそも観測されない**」場合。

**例 1: 生存解析の打ち切り (truncation)**
- 病院に入院した患者の生存時間を観測
- 観測期間は最大 5 年 → 5 年超で死亡した人は **存在自体が分からない** (退院済 etc.)
- 観測対象は「5 年以内に死亡した患者だけ」

**例 2: センサーの検出範囲**
- センサーが [0.1, 100] の範囲しか読めない
- 範囲外の値は記録されない (= 信号が来ない)

**例 3: アンケートの自己選別**
- 「過去 1 年に運動した日数」を聞く
- 「全く運動しない人」がアンケート対象から外れていると、最小値 ≥ 1 のデータに

これらでは「**観測サンプル自体が偏っている**」(= selection bias) ので、
普通の Normal/Exp などで尤度を計算すると **bias** がかかる。

### 6.2 数学的定式化

元の分布 $p(x)$ を範囲 $[a, b]$ に **切り詰める**:

$$ p_T(x \mid a \le x \le b) = \begin{cases} \dfrac{p(x)}{F(b) - F(a)} & a \le x \le b \\ 0 & \text{otherwise} \end{cases} $$

ここで $F$ は元分布の CDF。**正規化定数 $F(b) - F(a)$** で確率を 1 に揃える。

**直観**: 「分布の質量を範囲内にスケールアップする」。

### 6.3 範囲が片側だけの場合

- 下限のみ ($x \ge a$): $p_T(x) = p(x) / [1 - F(a)]$
- 上限のみ ($x \le b$): $p_T(x) = p(x) / F(b)$

例: 「**指数分布で観測されるのは t > 1 の場合のみ**」なら:

$$ p_T(t \mid t > 1) = \frac{\lambda e^{-\lambda t}}{e^{-\lambda}} = \lambda e^{-\lambda(t-1)} \quad (t \ge 1) $$

メモリレス性のおかげで実は **平行移動した指数分布**。一般的にはこんな簡素にはならない。

### 6.4 hanalyze での使い方

```haskell
import Model.HBM (Distribution (..), observe)

-- 例: 生存時間が観測期間 [0, 5] 内に切り詰められた指数分布
truncatedSurvival :: ModelP ()
truncatedSurvival = do
  rate <- sample "rate" (HalfNormal 2)
  observe "y" (Truncated (Exponential rate) (Just 0) (Just 5)) survivalTimes
  -- y の値は全部 [0, 5] 内、5 年超で亡くなった人のデータは含まれない
```

**鍵**: `Truncated d (Just lo) (Just hi)` で正規化を自動化。

### 6.5 真値推定との対比

「観測された平均生存時間」と「真の rate」は別物:
- データが [0, 5] に切り詰められていると、真の rate より **小さく見える**
  (長生きの患者がデータから抜けている)
- Truncated を使うと正しく rate を推定できる

`trunc-censor-demo` で実証:
```
正解: rate = 0.5 → 平均生存 2 年
Truncated 補正あり: rate ≈ 0.5  ✓
補正なし (普通の Exponential): rate を過大推定 (生存時間を短く見積もる)
```

### 6.6 注意点

- **両側 Truncated** (区間 [a, b]) は log-density 不連続性が強く、NUTS で収束困難な場合あり。
  代替: MH で解く、または Gibbs サンプラー。
- 元分布が **CDF を持つもの** に限る (Normal/Exponential/LogNormal/Uniform/
  Beta/Gamma/Cauchy/StudentT/HalfCauchy で hanalyze は対応)。

---

## 7. Censored 分布 — 打ち切り (★重点)

### 7.1 Truncated との違い

**Censored (打ち切り)** は **「サンプルは取れるが値が一部しか分からない」**:

| | Truncated (切り詰め) | Censored (打ち切り) |
|---|---|---|
| 観測自体 | 範囲外は **存在しない** | 範囲外も **記録される (が境界値として)** |
| データ件数 | 範囲内のみ | 全件 (境界値含む) |
| 例 | 5 年以内死亡者のみ | 全患者観測、5 年生きてる人は ≥5 と記録 |

### 7.2 例

#### 例 A: Tobit モデル (経済学)
- 時計の購入金額を観測
- 「買わなかった人」は 0 と記録 (= 検出下限以下は 0 にまるめ)
- 真値は連続的だが、観測は 0 でクリッピング

#### 例 B: 検出限界
- 化学分析で物質濃度を測定、検出下限 = 0.01 ppm
- 0.005 ppm の真値は「< 0.01」と記録 (= 0.01 で打ち切り)

#### 例 C: 生存解析の右側打ち切り
- 観測終了時点でまだ生きている患者
- 真の死亡時刻は不明だが「≥ 観測終了時刻」と分かる

### 7.3 数学的定式化

観測値 $y_i$ が:

- **境界内**: 通常の密度 $p(y_i)$
- **左境界 lo に等しい** (= 真値が ≤ lo の打ち切り): $P(Y \le \text{lo}) = F(\text{lo})$
- **右境界 hi に等しい** (= 真値が ≥ hi の打ち切り): $P(Y \ge \text{hi}) = 1 - F(\text{hi})$

これらを **対数尤度に正しく組み込む** ことで bias なく推定できる。

### 7.4 hanalyze の `Censored`

```haskell
import Model.HBM (Distribution (..), observe)

-- 例: 検出下限 1.0 のセンサーで Normal 観測
censoredSensor :: ModelP ()
censoredSensor = do
  mu  <- sample "mu"    (Normal 0 5)
  sig <- sample "sigma" (HalfNormal 3)
  observe "y" (Censored (Normal mu sig) (Just 1.0) Nothing) sensorReadings
  -- sensorReadings には 1.0 (打ち切り) と通常値が混在
```

内部では:
- 観測 $y_i$ が `lo` に等しいなら **CDF 値** $F(\text{lo})$ を使う
- 普通の値なら **density** $p(y_i)$ を使う

#### Truncated/Censored の API 共通

```haskell
data Distribution a
  = ...
  | Truncated (Distribution a) (Maybe a) (Maybe a)
  --             元分布           lo        hi   (Nothing = -∞ / +∞)
  | Censored  (Distribution a) (Maybe a) (Maybe a)
```

### 7.5 真値推定との対比

`trunc-censor-demo` の Censored Normal の例:

| | μ̂ (推定) | σ̂ |
|---|---|---|
| Censored 補正あり (正しいモデル) | ≈ 真値 | ≈ 真値 |
| 補正なし (1.0 を真値として扱う) | μ を **上方バイアス** | σ を **過小推定** |

下限値を「真値」と勘違いすると、平均がそちらに引っ張られる。

### 7.6 Tobit との関係

経済学の **Tobit モデル** = Censored Normal の特殊形:

```text
y* = X β + ε (真値、観測されない)
y  = max(0, y*)  (観測値、検出下限 0 で打ち切り)
```

これは `Censored (Normal (X β) sigma) (Just 0) Nothing` と等価。

### 7.7 両者の混在

実用では **両方が同時に発生** することも:

- 生存解析: 範囲 [0, 5] で観測 (truncated) + 観測終了時に生きている人は右側打ち切り
- センサー: 検出範囲 [0.01, 100] (truncated) + 範囲内でも上限超は 100 にまるめ (censored)

→ 両方の機構を組み合わせて尤度を構築する。hanalyze では別々の `Distribution`
として扱い、`observe` を 2 回呼ぶ等で対応。

### 7.8 識別性 (Identifiability)

**Censored / Truncated は適切なデータ量がないと推定不可**:

- 全部が打ち切り境界 → μ や σ が定まらない
- 範囲が極端に狭い → 観測数が少なすぎ

→ 事後 HDI が広い時は警戒。事前情報を強めるか、データ追加。

---

## 8. どれをいつ使うか

| やりたいこと | 使うもの |
|---|---|
| 事後を素早く得る (大規模データ) | **VI (ADVI)** |
| 多峰・複雑事後 | NUTS (MCMC) — VI は単峰しか捕えない |
| クラスタ自動検出 | Mixture |
| 重い裾の観測 | StudentT, Mixture (主+広成分) |
| 多変量の共分散推定 | LKJ + scale 分解 |
| 時系列 | AR(1)、状態空間 |
| 観測が範囲内のみ存在 (selection bias) | **Truncated** |
| 観測が境界値で潰れている (= 全件あるが部分情報) | **Censored** |
| 生存時間データ | 用途で Truncated と Censored を組合せ |
| モデル比較 | WAIC + LOO + (compareModels) |

---

## 9. 次に学ぶこと

入門者として次のステップ:

1. **demo を読んで動かす** ([demo](../../demo/) の以下を順に):
   - `clinical-trial`, `simpson-paradox` (基本)
   - `mixture-demo` (Mixture)
   - `lkj-demo`, `lkj3d-demo` (LKJ)
   - `ar1-demo` (AR)
   - `trunc-censor-demo` (★ Truncated / Censored)
   - `vi-demo` (VI vs NUTS)
   - `forest-compare` (モデル比較)

2. **実データで適用**:
   - 自分のデータを `Hanalyze.Model.HBM` で書く
   - NUTS で fit → `Hanalyze.Viz.Report` で診断 HTML を見る
   - 仮定がおかしければ Truncated/Censored/Mixture を検討

3. **理論を深掘り** (本書の参考文献に進む):
   - Gelman BDA: ベイズ統計のバイブル
   - Gelman & Hill: 階層モデル実例豊富
   - Vehtari papers: WAIC/LOO の最新

4. **HMC/NUTS の動作原理** ([theory-hmc-nuts.ja.md](theory-hmc-nuts.ja.md))

5. **次のレベルへ**:
   - 因果推論 (DoWhy, IV, DID)
   - Gaussian Process (`gp-demo`)
   - 多目的最適化 ([../optim/02-multi-objective.ja.md](../optim/02-multi-objective.ja.md))

---

## 10. 参考文献

### VI / モデル選択
- **Kucukelbir, A., Tran, D., Ranganath, R., Gelman, A., Blei, D. M.** (2017). "Automatic Differentiation Variational Inference". *JMLR*. → ADVI 原論文
- **Watanabe, S.** (2010). "Asymptotic Equivalence of Bayes Cross Validation and WAIC". *JMLR*. → WAIC
- **Vehtari, A., Gelman, A., Gabry, J.** (2017). "Practical Bayesian model evaluation using leave-one-out cross-validation and WAIC". *Statistics and Computing*. → PSIS-LOO
- **Yao, Y., Vehtari, A., Simpson, D., Gelman, A.** (2018). "Using Stacking to Average Bayesian Predictive Distributions". *Bayesian Analysis*. → Pseudo-BMA / Stacking

### Mixture
- **McLachlan, G., Peel, D.** (2000). *Finite Mixture Models*. Wiley. → 古典

### LKJ
- **Lewandowski, D., Kurowicka, D., Joe, H.** (2009). "Generating random correlation matrices based on vines and extended onion method". *J. Multivariate Analysis*.

### Truncated / Censored ★
- **Klein, J. P., Moeschberger, M. L.** (2003). *Survival Analysis: Techniques for Censored and Truncated Data* (2nd ed.). Springer.
  → 生存解析の標準教科書、打ち切り/切り詰めを徹底的に。
- **Greene, W. H.** (2017). *Econometric Analysis* (8th ed.). Pearson. Chapter 19.
  → Tobit / Heckman / Censored regression の経済学的扱い。
- **Cohen, A. C.** (1991). *Truncated and Censored Samples*. Marcel Dekker.
  → 専門書、両者の数学的扱い。

### 時系列
- **Durbin, J., Koopman, S. J.** (2012). *Time Series Analysis by State Space Methods* (2nd ed.). Oxford.

### 全般
- **Gelman, A., Carlin, J. B., Stern, H. S., Dunson, D. B., Vehtari, A., Rubin, D. B.** (2013). *Bayesian Data Analysis* (3rd ed.). CRC. [Web (free)](http://www.stat.columbia.edu/~gelman/book/)
  → ベイズ統計のバイブル。
- **McElreath, R.** (2020). *Statistical Rethinking* (2nd ed.). CRC.
  → 直観重視、入門書として最良。

### 関連 hanalyze ドキュメント
- [theory-bayesian-basics.ja.md](theory-bayesian-basics.ja.md) — ベイズの基礎
- [theory-mcmc.ja.md](theory-mcmc.ja.md) — MCMC 原理
- [theory-hmc-nuts.ja.md](theory-hmc-nuts.ja.md) — HMC/NUTS
- [theory-distributions.ja.md](theory-distributions.ja.md) — 分布カタログ
- [05-vi.ja.md](05-vi.ja.md) — VI の実装と使い方
- [06-model-comparison.ja.md](06-model-comparison.ja.md) — WAIC/LOO の実装
