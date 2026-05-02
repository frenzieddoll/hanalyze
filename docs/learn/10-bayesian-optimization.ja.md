# 学習資料 10 — Bayesian Optimization

## 1. 動機

評価コストが高い目的関数 (シミュレーション、実験) の最適化。
**サロゲート (代理) モデル** で次の評価点を賢く選ぶ。

## 2. ループ

```
1. 初期点を LHS / random で評価
2. 観測列 D = {(x_i, y_i)} を GP で fit
3. acquisition 関数 a(x) を最大化して x_next を選ぶ
4. y_next = f(x_next) を評価して D に追加
5. 2-4 を予算まで反復
```

## 3. Acquisition 関数

### 3.1 Expected Improvement (EI)

$$ \text{EI}(x) = E[\max(y_{\text{best}} - y(x), 0)] $$
$$ = (y_{\text{best}} - \mu - \xi) \Phi(z) + \sigma \phi(z), \quad z = (y_{\text{best}} - \mu - \xi)/\sigma $$

### 3.2 Upper / Lower Confidence Bound

$$ \text{LCB}(x) = \mu(x) - \beta \sigma(x) $$

$\beta$ で探索-活用のバランス。

### 3.3 Probability of Improvement (PI)

$$ \text{PI}(x) = \Phi\!\left(\frac{y_{\text{best}} - \mu - \xi}{\sigma}\right) $$

## 4. 多目的 BO

### 4.1 ParEGO (Knowles 2006)

各反復で random 重み $w$ を choose、Tchebycheff scalarization:

$$ y_{\text{scalar}}(x) = \max_j w_j (y_j - z_j^*) + \rho \sum_j w_j (y_j - z_j^*) $$

そして単目的 EI を計算。シンプルだが効果的。

### 4.2 EHVI (Expected Hypervolume Improvement)

$$ \text{EHVI}(x) = E[\text{HV}(P \cup \{y(x)\}) - \text{HV}(P)] $$

新点を front に加えた際の **期待 HV 増分**。情報量最大。
2D で閉形式、3D 以上は MC 近似。

### 4.3 q-EHVI (バッチ)

$q$ 点を同時提案。並列実験で有効。

## 5. NSGA-II vs Bayesian MOO

| | NSGA-II | Bayesian MOO |
|---|---|---|
| 評価コスト | 安価向け | 高価向け |
| 評価回数 | 数千 〜 数万 | 数十 〜 数百 |
| 多目的対応 | ◎ | ○ (acquisition で工夫) |
| 並列性 | 自然 (世代単位) | バッチ acquisition で対応 |

## 6. hanalyze の実装

```haskell
-- 単目的
import Optim.BayesOpt (bayesOpt, defaultBayesOptConfig)
(history, best) <- bayesOpt cfg f (lo, hi) gen

-- 多目的 (NSGA-II 内側で acquisition 最大化)
import Optim.BayesOpt (bayesOptMOWithNSGA)
hist <- bayesOptMOWithNSGA nIter nInit RBF f bounds gen
```
