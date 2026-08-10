# 加速寿命試験モデル (Arrhenius / Eyring / Inverse Power Law)

対象 module: `Hanalyze.Model.Reliability` (`hanalyze-models`)

使用条件で数年かかる故障を、 高ストレス (高温 / 高電圧) で短時間に再現し、
**ストレスと寿命の関係を回帰して使用条件へ外挿**するのが加速寿命試験。
本 module は古典的な 3 モデルを、 対数寿命の線形回帰として提供する。

| 関数 | モデル | 主なストレス |
|---|---|---|
| `fitArrhenius` | `t = A · exp(Ea / (k_B · T))` | 温度 |
| `fitEyring` | `t · T = A · exp(Ea / (k_B · T)) · exp(B · S)` | 温度 + もう 1 つ |
| `fitInversePower` | `t = A · S^(-n)` | 電圧 / 機械応力 |

寿命分布そのもの (Weibull の形状 / 尺度) の推定は
[usage-weibull.ja.md](usage-weibull.ja.md) を参照。 本 module は寿命の
**平均的な水準**をストレスで説明する層で、 分布形の指定は行わない
(残差 Gaussian 仮定の OLS)。

## 0. API

```haskell
kBoltzmann :: Double        -- Boltzmann 定数 (eV/K)

fitArrhenius    :: [(Double, [Double])]         -> Either Text ArrheniusFit
fitEyring       :: [(Double, Double, [Double])] -> Either Text EyringFit
fitInversePower :: [(Double, [Double])]         -> Either Text InversePowerFit

accelerationFactor :: ArrheniusFit -> Double -> Double -> Double
```

入力はいずれも **「ストレス水準ごとに寿命のリスト」** という形。
同じ水準で複数個を試験した実データの形に素直に対応する。

`Left` になる条件: 入力が空 / 全観測 0 個、 ストレス水準が 1 種類しかない
(傾きが決まらない)、 温度 ≤ 0 や寿命 ≤ 0 (対数が取れない)。

## 1. Arrhenius (温度加速)

3 温度水準 × 各 3 個の寿命データ。 温度は **絶対温度 K** で渡す。

```haskell
import Hanalyze.Model.Reliability

main :: IO ()
main = do
  let arr = [ (348.15, [ 1050, 980, 1120 ])     -- 75 ℃
            , (373.15, [  310, 290,  335 ])     -- 100 ℃
            , (398.15, [  105,  95,  115 ]) ]   -- 125 ℃
  case fitArrhenius arr of
    Left err -> putStrLn ("error: " ++ show err)
    Right f  -> do
      print (afA f, afEa f, afN f)
      print (accelerationFactor f 298.15 398.15)
  print kBoltzmann
```

実行結果:

```
(1.1385057209261695e-5,0.5503049757365821,9)
216.93288276839078
8.617333262145e-5
```

- **`afEa` = 0.55 eV** — 活性化エネルギー。 半導体の代表的な故障機構は
  0.3 〜 1.0 eV 程度に収まるので、 桁が外れていれば試験条件かデータを疑う
- **`afA` = 1.14e-5** — 前指数因子。 単位は寿命の単位に依存する
- **`afN` = 9** — (温度, 寿命) の観測総数。 水準数ではなく個体数
- **加速係数 216.9** — `accelerationFactor fit T_use T_test` は
  `exp(Ea/k_B · (1/T_use − 1/T_test))`。 「125 ℃ で 1 時間 = 25 ℃ で
  217 時間」 に相当する。 引数順は **(使用条件, 試験条件)** で、
  同じ温度を渡せば 1 になる

## 2. Inverse Power Law (電圧 / 応力加速)

```haskell
  let ipl = [ (10, [ 4200, 3900, 4400 ])
            , (15, [  820,  760,  880 ])
            , (20, [  260,  240,  275 ]) ]
  case fitInversePower ipl of
    Left err -> putStrLn ("error: " ++ show err)
    Right f  -> print (ipfA f, ipfN f, ipfNobs f)
```

```
(4.278453355340438e7,4.011957418619679,9)
```

`ipfN` = 4.01 が power 指数 n。 **ストレスが 2 倍になると寿命は 2^4 = 16 分の 1**
という読み方をする。 絶縁体の電圧加速では n = 3 〜 5 が経験的な範囲。
フィールド名が `ipfNobs` (観測数) と `ipfN` (指数) で紛らわしいので注意。

## 3. Eyring (温度 + もう 1 つのストレス)

温度と第 2 ストレス (湿度 / 電流密度 / 電圧など) を同時に扱う。 入力は
`(温度 K, ストレス, [寿命])`。

```haskell
  let eyr = [ (348.15, 0.0, [ 1050, 980 ])
            , (348.15, 1.0, [  520, 495 ])
            , (373.15, 0.0, [  310, 290 ])
            , (373.15, 1.0, [  155, 148 ]) ]
  case fitEyring eyr of
    Left err -> putStrLn ("error: " ++ show err)
    Right f  -> print (efA f, efEa f, efB f, efN f)
```

```
(1.3425050183030706e-2,0.5125061323103351,-0.6878818176120822,8)
```

`efB` = −0.688 が第 2 ストレスの係数。 モデルが `exp(B · S)` なので
**負の B = ストレスが上がると寿命が短くなる**。 実際 `exp(-0.688) = 0.503`
で、 S を 0 → 1 にすると寿命がほぼ半分になる入力と整合する。

内部では `y = log t + log T` を `(1/T, S)` の 2 変量 OLS で解いており、
`log T` の項がモデルに入る点が Arrhenius との違い。

## 4. 結果型

| 型 | フィールド |
|---|---|
| `ArrheniusFit` | `afA` / `afEa` (eV) / `afLogLik` / `afN` |
| `EyringFit` | `efA` / `efEa` / `efB` / `efLogLik` / `efN` |
| `InversePowerFit` | `ipfA` / `ipfN` (指数) / `ipfLogLik` / `ipfNobs` |

`*LogLik` は残差 Gaussian 仮定の対数尤度。 同じデータに対する
Arrhenius と Inverse Power Law の当てはまり比較などに使える (パラメータ数が
同じなら大きい方が良い)。

## 5. 使い分け

| やりたいこと | 使うもの |
|---|---|
| 温度加速のみ・活性化エネルギーを出す | `fitArrhenius` (本 doc) |
| 温度 + 湿度 / 電圧を同時に | `fitEyring` (本 doc) |
| 電圧 / 応力の冪則 | `fitInversePower` (本 doc) |
| 寿命の分布形 (k / λ / B_p) | [usage-weibull.ja.md](usage-weibull.ja.md) |
| 打ち切り込みのノンパラ生存解析 | [10-survival.ja.md](10-survival.ja.md) |
| 系統 (直列 / 並列 / k-of-n) の信頼度 | [`Model.ReliabilityBlockDiagram`](../api-guide/07-survival.ja.md) |

## 関連

- package: [hanalyze-models](../../hanalyze-models/README.ja.md)
- Weibull MLE / B_p 寿命: [usage-weibull.ja.md](usage-weibull.ja.md)
- 生存時間解析: [10-survival.ja.md](10-survival.ja.md)
