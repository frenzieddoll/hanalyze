# 確率分布の関係図

> hanalyze で実装している分布同士の極限・特殊化・共役関係をまとめた図。
> 学習資料の補助として、何が何の特殊ケースかを把握する用。

## 凡例

```mermaid
graph LR
  A((親分布)) -- "極限/特殊化条件" --> B((子分布))
```

- **極限**: パラメタを ∞ や 0 などに飛ばすと別分布に収束
- **特殊化**: パラメタを特定値に固定すると別分布と一致
- **混合**: 2 つの分布の階層から導かれる
- **共役**: ベイズ事後計算で閉形式が得られる対

## 1. 連続分布の家系図

```mermaid
graph TD
  N["Normal(μ, σ)"]
  HN["HalfNormal(σ)"]
  LN["LogNormal(μ, σ)"]
  T["StudentT(ν, μ, σ)"]
  C["Cauchy(loc, scale)"]
  HC["HalfCauchy(scale)"]
  E["Exponential(rate)"]
  G["Gamma(α, β)"]
  IG["InverseGamma(α, β)"]
  W["Weibull(k, λ)"]
  P["Pareto(α, x_m)"]
  B["Beta(α, β)"]
  U["Uniform(a, b)"]
  Chi["χ²(k)"]
  VM["VonMises(μ, κ)"]

  N -->|"x ≥ 0 制約 (折り返し)"| HN
  N -->|"x = log y (対数変換)"| LN
  T -->|"ν → ∞"| N
  T -->|"ν = 1"| C
  C -->|"x ≥ 0 制約"| HC
  G -->|"α = 1"| E
  G -->|"α = k/2, β = 1/2"| Chi
  G -->|"X ~ Gamma → 1/X ~ InvGamma"| IG
  E -->|"k = 1 (Weibull)"| W
  W -->|"shape k = 2"| Rayleigh["Rayleigh(σ=λ/√2)"]
  G -->|"X / (X+Y), Y ~ Gamma 同形 → Beta"| B
  B -->|"α = β = 1"| U2["Uniform(0,1)"]
  U2 -->|"線形変換"| U
  P -->|"X = log Y - log x_m, X ~ Exp(α)"| E
  VM -->|"κ → 0"| UAng["Uniform(-π, π)"]
  VM -->|"κ → ∞"| Napprox["Normal(μ, 1/√κ)"]
```

### よく使う変換

| 変換 | 関係 | 備考 |
|---|---|---|
| `LogNormal(μ, σ)` | log y ~ Normal(μ, σ) | 正の値、対数変換で正規 |
| `HalfNormal(σ)` | abs(z), z ~ Normal(0, σ) | sd 事前で頻用 |
| `Chi²(k)` | sum of k 個の Normal(0,1)² | Gamma の特殊形 |
| `Rayleigh(σ)` | √(X² + Y²), X,Y ~ N(0, σ) | Weibull(k=2, λ=σ√2) と等価 |

## 2. 離散分布の家系図

```mermaid
graph TD
  Bern["Bernoulli(p)"]
  Bin["Binomial(n, p)"]
  Multi["Multinomial(n, π)"]
  Cat["Categorical(π)"]
  Poi["Poisson(λ)"]
  NB["NegativeBinomial(μ, α)"]
  Geo["Geometric(p)"]
  ZIP["ZeroInflatedPoisson(ψ, λ)"]
  ZIB["ZeroInflatedBinomial(n, ψ, p)"]
  BB["BetaBinomial(n, α, β)"]
  N["Normal(μ, σ)"]

  Bern -->|"n=1 と等価"| Bin
  Bin -->|"K=2 (binary outcome)"| Multi2["Multinomial(n, [p, 1-p])"]
  Bern -->|"K カテゴリへ拡張"| Cat
  Cat -->|"n 回試行の集計"| Multi
  Bin -->|"n→∞, p→0, np=λ 固定"| Poi
  Bin -->|"n→∞ (中心極限)"| N
  Poi -->|"λ大 (中心極限)"| N
  NB -->|"α → ∞"| Poi
  NB -->|"r=1 とみなす"| Geo
  Poi -->|"+ 構造的ゼロ ψ"| ZIP
  Bin -->|"+ 構造的ゼロ ψ"| ZIB
  Bin -->|"p ~ Beta(α,β) 混合"| BB
  BB -->|"α, β → ∞ (集中)"| Bin
```

### 重要な極限・近似

```mermaid
graph LR
  Bin["Binomial(n, p)"]
  Poi["Poisson(λ=np)"]
  N["Normal(μ=np, σ=√(np(1-p)))"]
  Bin -->|"n→∞, p→0, np=λ 固定 = ポアソン近似"| Poi
  Bin -->|"n→∞, p 固定 = 正規近似 (中心極限)"| N
  Poi -->|"λ→∞ = 正規近似"| N
```

これは「**De Moivre-Laplace の定理** (二項 → 正規)」「**ポアソン近似**」
「**中心極限定理**」の典型例。

## 3. 共役関係 (ベイズ事後が閉形式)

```mermaid
graph TD
  subgraph "Beta-Binomial"
    Bp["Beta(α, β)"] --"事前 p"--> Bb["Binomial(n, p)"]
    Bb --"事後"--> Bp2["Beta(α+k, β+n-k)"]
  end
  subgraph "Gamma-Poisson"
    Gl["Gamma(α, β)"] --"事前 λ"--> Pp["Poisson(λ)"]
    Pp --"事後"--> Gl2["Gamma(α+Σy, β+n)"]
  end
  subgraph "Dirichlet-Multinomial"
    D["Dirichlet(α₁,…,α_K)"] --"事前 π"--> M["Multinomial(n, π)"]
    M --"事後"--> D2["Dirichlet(α_k + count_k)"]
  end
  subgraph "Normal-InverseGamma"
    IG["InverseGamma(α, β)"] --"事前 σ²"--> Nl["Normal(μ, σ)"]
    Nl --"事後"--> IG2["InverseGamma(α+n/2, β+SS/2)"]
  end
  subgraph "Normal-Normal"
    Np["Normal(μ₀, σ₀)"] --"事前 μ"--> Nl2["Normal(μ, σ)"]
    Nl2 --"事後"--> Np2["Normal(updated)"]
  end
```

これらは Gibbs サンプラーで個別パラメタを直接 sample するときに使う。
hanalyze の `MCMC.Gibbs.gibbsMH` は事前/尤度の組み合わせから自動で
共役構造を検出して使う。

## 4. 多変量と相関

```mermaid
graph TD
  N1["Normal (1D)"]
  MvN["MvNormal([μ_1,...,μ_K], Σ)"]
  LKJ["LKJ(η) on R"]
  Sigma["Σ = diag(σ) R diag(σ)"]
  IG["InverseGamma (各 σ_i² 事前)"]
  Wish["Wishart"]
  HN["HalfNormal/HalfCauchy (σ_i 事前)"]

  N1 -->|"次元 K に拡張"| MvN
  LKJ -->|"R = L Lᵀ"| Sigma
  HN -->|"各 σ_i"| Sigma
  IG -->|"各 σ_i² (代替)"| Sigma
  Sigma -->|"パラメタ"| MvN
  Wish -->|"Σ⁻¹ の事前 (古典)"| MvN
  Wish -.->|"非推奨 (LKJ + scale 推奨)"| LKJ
```

PyMC や Stan では **LKJ + scale** 分解が **Wishart** より好まれる。
hanalyze では `lkjCorrCholesky` で R の Cholesky factor を sample。

## 5. 時系列・状態空間

```mermaid
graph TD
  N["Normal (innovation)"]
  AR1["AR(1): x_t = φ x_{t-1} + ε_t"]
  RW["RandomWalk: x_t = x_{t-1} + ε_t"]
  GP["GaussianProcess (RBF/Matérn 等)"]
  SS["State-Space (= AR + 観測)"]

  N -->|"ε_t"| AR1
  AR1 -->|"φ = 1"| RW
  AR1 -->|"連続時間極限 + 共分散関数"| GP
  AR1 -->|"+ 観測ノイズ"| SS
  GP -->|"格子上 = 多変量正規"| MvN["MvNormal"]
```

`ar1Latent` (J2) と `Model.GP` (master) で実装済み。

## 6. 切り詰め・打ち切り・混合

```mermaid
graph TD
  D["Distribution(d)"]
  T["Truncated(d, lo, hi)"]
  Cs["Censored(d, lo, hi)"]
  Mix["Mixture([w_k], [d_k])"]
  ZI["ZeroInflated(d, ψ)"]
  D -->|"範囲制限 (CDF 補正)"| T
  D -->|"検出限界 (CDF/SF 尤度)"| Cs
  D -->|"K 成分の重み付き和"| Mix
  Mix -->|"成分が delta_0 と d (= 構造的ゼロ)"| ZI
```

`Truncated` / `Censored` / `Mixture` / `ZeroInflated*` は
任意の base 分布に対して定義可能 (CDF が必要なものは限定的)。

## 7. 角度データ

```mermaid
graph TD
  VM["VonMises(μ, κ)"]
  N["Normal(μ, 1/√κ)"]
  Unf["Uniform(-π, π)"]
  WN["WrappedNormal"]
  VM -->|"κ → 0"| Unf
  VM -->|"κ → ∞"| N
  WN -.->|"近似的に"| VM
```

`VonMises` は角度上の "Normal-like" 分布。

## まとめ表 — どの分布をいつ使う?

| データの性質 | 第一選択 | 過分散時 | 角度なら |
|---|---|---|---|
| 0/1 二値 | `Bernoulli` | — | — |
| n 試行のうち成功数 | `Binomial(n, p)` | `BetaBinomial(n, α, β)` | — |
| カウント | `Poisson(λ)` | `NegativeBinomial(μ, α)` | — |
| カウント (ゼロ過剰) | `ZeroInflatedPoisson` | — | — |
| 連続 (実数) | `Normal(μ, σ)` | `StudentT(ν, μ, σ)` | — |
| 連続 (正値) | `LogNormal` / `Gamma` / `Weibull` | — | — |
| 比率 (0-1) | `Beta(α, β)` | — | — |
| 多項 (K カテゴリの集計) | `Multinomial(n, π)` | — | — |
| 単位ベクトル / シンプレックス | `Dirichlet` | — | — |
| 多変量実数 | `MvNormal(μ, Σ)` | — | — |
| 角度 | — | — | `VonMises(μ, κ)` |

| 推定対象 | 共役事前 | 弱情報事前 |
|---|---|---|
| Bernoulli/Binomial の `p` | `Beta(α, β)` | `Beta(2, 2)` 等 |
| Poisson の `λ` | `Gamma(α, β)` | `HalfNormal` |
| Multinomial の `π` | `Dirichlet(α₁,…)` | `Dirichlet(1,…)` |
| Normal の `μ` | `Normal(μ₀, σ₀)` | `Normal(0, 大)` |
| Normal の `σ²` | `InverseGamma(α, β)` | `HalfNormal` / `HalfCauchy` |
| 相関行列 `R` | `LKJ(η)` | `LKJ(1)` (uniform) |

## 参考

- 各分布の数式と意味は [docs/learn/01-probability-distributions.ja.md](learn/01-probability-distributions.ja.md) を参照 (Phase M1 で追加)
- 共役関係を活用した Gibbs サンプリングは [docs/04-gibbs.ja.md](04-gibbs.ja.md) を参照
