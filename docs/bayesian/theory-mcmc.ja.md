# 学習資料 3 — MCMC の原理

> なぜ MCMC で事後分布から sample できるのか? 数学的な根拠から
> Metropolis-Hastings と Gibbs サンプラーまで。

## 1. なぜ MCMC が必要か

ベイズ事後 $p(\theta \mid y)$ は通常、

- **正規化定数 $p(y)$ が計算できない** (高次元積分)
- **複雑な形** (多峰、非対称、偏り)

ため解析解がない。代わりに **事後からの sample** $\theta^{(1)}, \ldots, \theta^{(N)}$
を得て、期待値や分位数を経験的に近似する:

$$ E_{\theta \sim p(\theta \mid y)}[f(\theta)] \approx \frac{1}{N} \sum_{n=1}^N f(\theta^{(n)}) $$

問題: **どうやって独立サンプルを得るか?** → MCMC が答え。

---

## 2. マルコフ連鎖

### 2.1 定義

確率変数列 $\theta^{(0)}, \theta^{(1)}, \theta^{(2)}, \ldots$ が
**マルコフ性** を持つとは:

$$ p(\theta^{(t+1)} \mid \theta^{(t)}, \theta^{(t-1)}, \ldots, \theta^{(0)}) = p(\theta^{(t+1)} \mid \theta^{(t)}) $$

「次の状態は現在の状態にのみ依存し、過去には依存しない」。

遷移核 $K(\theta' \mid \theta) = p(\theta^{(t+1)} = \theta' \mid \theta^{(t)} = \theta)$
で完全に決まる。

### 2.2 定常分布

ある分布 $\pi$ が **定常 (stationary)** とは:

$$ \pi(\theta') = \int K(\theta' \mid \theta) \pi(\theta) d\theta $$

「$\pi$ から始めて 1 ステップ進めても分布が変わらない」。

### 2.3 詳細釣り合い (Detailed Balance)

十分条件として:

$$ \pi(\theta) K(\theta' \mid \theta) = \pi(\theta') K(\theta \mid \theta') $$

(可逆性)。MH や Gibbs はこれを満たすように設計される。

### 2.4 エルゴード性 (Ergodicity)

連鎖が **エルゴード的** とは:

1. **既約 (irreducible)**: 任意の 2 状態が有限ステップで往来可能
2. **非周期的 (aperiodic)**: 周期 1 の戻り
3. **正再帰的 (positive recurrent)**: 期待戻り時間が有限

エルゴード的かつ $\pi$ が定常なら:

$$ \theta^{(t)} \xrightarrow{d} \pi \quad \text{as } t \to \infty $$

(任意の初期分布から始めて $\pi$ に収束)。

### 2.5 大数の法則

エルゴード的なら、長期平均が空間平均に収束:

$$ \frac{1}{N} \sum_{n=1}^N f(\theta^{(n)}) \xrightarrow{a.s.} E_{\pi}[f(\theta)] $$

これが **MCMC の正当性の根拠**。

---

## 3. Metropolis-Hastings (MH)

### 3.1 アルゴリズム

目標分布 $\pi(\theta) \propto \tilde\pi(\theta)$ (正規化定数不要) からサンプル:

```text
1. 現在 θ から提案分布 q(·|θ) で θ' をドロー
2. 受容率 α = min(1, [π̃(θ')/π̃(θ)] × [q(θ|θ')/q(θ'|θ)]) を計算
3. u ~ Uniform(0,1) をドロー、u < α なら θ ← θ' 受容、さもなくば棄却 (留まる)
```

### 3.2 なぜ正しいか

詳細釣り合いを満たすことが示せる:

$$ \pi(\theta) K(\theta'|\theta) = \pi(\theta) q(\theta'|\theta) \alpha(\theta, \theta') $$

$\alpha = \min(1, r)$ で $r = \pi(\theta') q(\theta|\theta') / [\pi(\theta) q(\theta'|\theta)]$ なので、
両辺で $\pi(\theta) q(\theta'|\theta) \min(1, r) = \pi(\theta') q(\theta|\theta') \min(1, 1/r)$
が成立する (簡単な代数で確認できる)。

### 3.3 Random Walk Metropolis

提案を $\theta' = \theta + \epsilon$, $\epsilon \sim \text{Normal}(0, s)$ にする
(対称提案)。その場合:

$$ \alpha = \min\!\left(1, \frac{\pi(\theta')}{\pi(\theta)}\right) $$

シンプルだが **次元が増えると非効率**: 受容率を維持するには $s$ を $1/\sqrt{D}$ で
小さくする必要 → 探索が遅い。

### 3.4 ステップサイズの調整

| 受容率 | 状態 |
|---|---|
| < 20% | $s$ が大きすぎ (棄却ばかり) |
| 20%〜50% | 良好 |
| > 50% | $s$ が小さすぎ (動かない) |

特に高次元では 23.4% (Roberts 1997) を目安。

### 3.5 hanalyze での実装

```haskell
import Hanalyze.MCMC.MH (metropolis, defaultMCMCConfig)

ch <- metropolis model
        (defaultMCMCConfig ["mu", "sigma"])
          { mcmcStepSizes = Map.fromList [("mu", 0.1), ("sigma", 0.05)] }
        init0
        gen
```

`Chain.chainAccepted` / `chainTotal` から受容率が確認できる。

---

## 4. Gibbs サンプリング

### 4.1 アイデア

各パラメタの **完全条件分布** $p(\theta_i \mid \theta_{-i}, y)$
が解析的に sample できる場合、それらを順番に更新する:

```text
for t = 1, 2, ...:
  θ_1 ~ p(θ_1 | θ_2, ..., θ_K, y)
  θ_2 ~ p(θ_2 | θ_1, θ_3, ..., θ_K, y)
  ...
  θ_K ~ p(θ_K | θ_1, ..., θ_{K-1}, y)
```

これは MH の特殊形 (受容率 100%)。

### 4.2 共役モデルでの威力

ベイズ階層モデル + 共役事前なら、各 $\theta_i$ の完全条件分布が
閉形式 (Beta, Gamma, Normal, …) になる。受容率調整不要で高速。

### 4.3 Hybrid Gibbs+MH

非共役パラメタは MH ステップで更新、共役パラメタは Gibbs ステップで
更新。`Hanalyze.MCMC.Gibbs.gibbsMH` は事前/尤度の組合せから自動検出。

### 4.4 hanalyze の実装

```haskell
import Hanalyze.MCMC.Gibbs (gibbsMH, defaultGibbsConfig)

-- 事前/尤度の組み合わせから 共役構造を自動検出:
ch <- gibbsMH model defaultGibbsConfig init0 gen
```

`Hanalyze.Stat.Gibbs.detectConjugate` で各 latent の完全条件が
閉形式かを判定。

---

## 5. 収束診断

### 5.1 Burn-in

連鎖は初期分布の影響が消えるまで時間がかかる。最初の数百〜数千ステップを
**burn-in** として捨てる。hanalyze では `mcmcBurnIn` / `nutsBurnIn` 等で指定。

### 5.2 自己相関と Effective Sample Size (ESS)

連続する $\theta^{(t)}, \theta^{(t+1)}$ は強い相関を持つ。$N$ サンプル
あっても **独立な情報量はずっと少ない**:

$$ \text{ESS} = \frac{N}{1 + 2 \sum_{k=1}^\infty \rho_k} $$

$\rho_k$: ラグ $k$ の自己相関。`Hanalyze.Stat.MCMC.ess` で計算 (Geyer's
initial monotone sequence 推定量)。

| ESS | 状態 |
|---|---|
| < 100 | サンプル不足 |
| 100〜400 | 最低限 |
| > 400 | 推奨 |

### 5.3 R-hat (Gelman-Rubin)

複数チェーンを並列実行し、

$$ \hat{R} = \sqrt{\frac{\text{var}_+}{W}} $$

- $W$: チェーン内分散
- $\text{var}_+ = \frac{n-1}{n} W + \frac{B}{n}$, $B$: チェーン間分散

| R-hat | 状態 |
|---|---|
| > 1.01 | 未収束 |
| < 1.01 | 収束 |

`Hanalyze.Stat.MCMC.rhat` (Vehtari 2021 の split-R-hat) で計算。
`Hanalyze.MCMC.NUTS.nutsChains` で並列実行 → `Hanalyze.Viz.MCMC.posteriorSummary`
で表示。

### 5.4 Trace plot / Rank plot

- **Trace plot**: 反復 vs 値 (ホワイトノイズ風が理想)
- **Rank plot**: チェーンを混ぜて順位、各チェーンで一様分布が理想
   (Vehtari 2021、`Hanalyze.Viz.MCMC.rankPlot`)

---

## 6. 高次元の壁

MH は次元 $D$ が大きいと非効率:

| 手法 | スケーリング |
|---|---|
| Random Walk Metropolis | $\sim D$ ステップで 1 単位移動 |
| Gibbs (共役) | 1 ステップで全成分更新 |
| HMC | $\sim D^{1/4}$ |
| NUTS | HMC の自動軌道長版 |

→ 高次元なら **HMC / NUTS** ([theory-hmc-nuts.ja.md](theory-hmc-nuts.ja.md) M4)。

---

## 7. Slice sampler

調整不要のオルタナ:

1. $y \sim \text{Uniform}(0, p(\theta))$ (= log space で $\log y = \log p(\theta) - \text{Exp}(1)$)
2. **水平スライス** $S = \{\theta' : p(\theta') > y\}$ を求め
3. $\theta' \sim \text{Uniform}(S)$ をドロー

実用的には **stepping-out** で軸ごとに区間 $[L, R]$ を構築、**shrinkage**
で uniform draw → 受容するまで縮める。

`Hanalyze.MCMC.Slice.slice` で実装。勾配不要、ステップ幅は自動調整。

---

## 8. 各サンプラーの使い分け

```mermaid
graph TD
  Q1{パラメタ次元?}
  Q1 -->|低次元 (~10)| Q2{共役?}
  Q1 -->|高次元| Q3{勾配計算可能?}

  Q2 -->|はい| Gibbs["Gibbs / gibbsMH"]
  Q2 -->|いいえ| Q4{自動調整したい?}
  Q4 -->|はい| Slice["Slice"]
  Q4 -->|いいえ| MH["Metropolis-Hastings"]

  Q3 -->|はい (微分可能)| NUTS["NUTS (推奨)"]
  Q3 -->|いいえ (離散など)| Gibbs2["Gibbs+MH"]
```

---

## 次のステップ

- HMC/NUTS の幾何学と実装 → [theory-hmc-nuts.ja.md](theory-hmc-nuts.ja.md) (M4)
- 既存ドキュメント: [03-mcmc-samplers.ja.md](03-mcmc-samplers.ja.md) は API レベル概観
- 実演: `cabal run slice-demo` (Slice/MH/NUTS 比較)
