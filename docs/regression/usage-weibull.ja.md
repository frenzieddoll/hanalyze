# Weibull 分布の最尤推定と B_p 寿命

対象 module: `Hanalyze.Model.Weibull` (`hanalyze-models`)

寿命 / 故障時間データに Weibull 分布を当てはめ、 形状 k・尺度 λ の最尤推定と
**B_p 寿命** (母集団の割合 p が故障する時刻) を求める。 半導体・材料の
信頼性評価で最も基本になる分布。

```
f(x) = (k/λ) (x/λ)^(k-1) exp(-(x/λ)^k)      x > 0
S(x) = exp(-(x/λ)^k)
```

形状 k の読み方 (バスタブ曲線):

| k | 故障率 | 典型 |
|---|---|---|
| k < 1 | 時間とともに低下 | 初期不良 (バーンイン期) |
| k = 1 | 一定 | 指数分布 (偶発故障) |
| k > 1 | 上昇 | 摩耗故障 |

ストレス (温度 / 電圧) と寿命の関係をモデル化する加速寿命試験は
[usage-reliability.ja.md](usage-reliability.ja.md) を参照。

## 0. API

```haskell
data WeibullFit = WeibullFit
  { wfShape  :: !Double                    -- k (> 0)
  , wfScale  :: !Double                    -- λ (> 0)
  , wfLogLik :: !Double                    -- 対数尤度の MLE 値
  , wfN      :: !Int                       -- 観測総数 (打ち切り含む)
  , wfRObs   :: !Int                       -- 故障数 (打ち切りを除く)
  , wfFisher :: !(Double, Double, Double)  -- Fisher 情報行列 (I_kk, I_kλ, I_λλ)
  }

fitWeibullMLE      :: Vector Double -> Either Text WeibullFit
fitWeibullCensored :: Vector Double -> Vector Bool -> Either Text WeibullFit

bxLife   :: Double -> WeibullFit -> Double                      -- p -> B_p
bxLifeCI :: Double -> Double -> WeibullFit
         -> (Double, Double, Double)                            -- (推定, 下限, 上限)

weibullParameterSE         :: WeibullFit -> (Double, Double)
weibullParameterCovariance :: WeibullFit -> (Double, Double, Double)
```

`Vector` は **`Data.Vector`** (boxed) — `hmatrix` の `Vector` ではない。

## 1. 全数故障データ

```haskell
import qualified Data.Vector as V
import Hanalyze.Model.Weibull

main :: IO ()
main = do
  let ts = V.fromList [ 5.1, 8.3, 11.0, 12.4, 15.9, 18.2, 21.5, 25.0 ]
  case fitWeibullMLE ts of
    Left err -> putStrLn ("error: " ++ show err)
    Right f  -> do
      print (wfShape f, wfScale f)
      print (bxLife 0.10 f)
      print (weibullParameterSE f)
      print (bxLifeCI 0.10 0.05 f)
```

実行結果:

```
(2.546174113334201,16.580410383584077)
6.851029056082719
(0.7451839904102958,2.11069724624789)
(6.851029056082719,2.433033727866307,11.269024384299133)
```

k = 2.55 > 1 なので摩耗型の故障。 B_10 寿命は 6.85 (= 10% が故障する時刻)
で、 95% 信頼区間は 2.43 〜 11.27。 n = 8 と少ないので区間は広い。

> **`bxLifeCI` の第 2 引数は信頼度ではなく α**。 95% CI なら `0.05` を渡す
> (`0.95` を渡すと z = 0.063 の 5% 区間になる)。

## 2. 右打ち切りデータ

試験を打ち切った個体 (まだ故障していない) を含む場合は
`fitWeibullCensored` に **`True` = 故障 / `False` = 打ち切り**の
イベント指示ベクトルを渡す。

```haskell
  let ts2 = V.fromList [ 5.1, 8.3, 11.0, 12.4, 15.9, 18.2, 21.5, 25.0 ]
      ev  = V.fromList [ True, True, True, True, True, False, False, False ]
  case fitWeibullCensored ts2 ev of
    Left err -> putStrLn ("error: " ++ show err)
    Right f  -> print (wfShape f, wfScale f, wfN f, wfRObs f)
```

```
(1.678639197705706,20.616867764344537,8,5)
```

同じ時刻列でも、 後半 3 個を「まだ壊れていない」 と扱うと k が 2.55 → 1.68、
λ が 16.6 → 20.6 に動く。 打ち切りを故障として扱う (= §1 の呼び方) と
**寿命を短く見誤る**ので、 試験を途中で止めたデータでは必ずこちらを使う。
`wfN` は総数 8、 `wfRObs` は故障数 5 を保持するので、 打ち切り率の確認に使える。

## 3. B_p 寿命の信頼区間 (delta method)

`bxLifeCI p α fit` は delta method で B_p の分散を伝播させる:

```
Var(B_p) ≈ (∂B_p/∂k)² Var(k) + (∂B_p/∂λ)² Var(λ) + 2 (∂B_p/∂k)(∂B_p/∂λ) Cov(k,λ)
```

分散共分散は Fisher 情報行列の逆行列から得る:

```haskell
  print (weibullParameterCovariance f)      -- (Var(k), Cov(k,λ), Var(λ))
```

```
(0.5552991795638119,0.6004104002867886,4.455042865318426)
```

`weibullParameterSE` はこの対角成分の平方根 (= `(0.745, 2.111)`) を返す
簡便版。 共分散が非正定値で SE が計算できない場合、 `bxLifeCI` は
区間幅 0 の `(推定, 推定, 推定)` を返す (下限は寿命が非負なので 0 でクリップ)。

## 4. 使い分け

| やりたいこと | 使うもの |
|---|---|
| Weibull を当てはめて k / λ と B_p を出す | `fitWeibullMLE` (本 doc) |
| 打ち切りを含む試験データ | `fitWeibullCensored` (本 doc) |
| ストレスと寿命の関係 (加速試験) | [usage-reliability.ja.md](usage-reliability.ja.md) |
| ノンパラに生存曲線を見る | [10-survival.ja.md](10-survival.ja.md) (Kaplan-Meier / Cox) |
| 直列 / 並列 / k-of-n の系統信頼度 | [`Model.ReliabilityBlockDiagram`](../api-guide/07-survival.ja.md) |
| 分布そのものの pdf / cdf / 乱数 | `Hanalyze.Stat.Distribution` (core 層。 専用 doc は未整備) |

## 関連

- package: [hanalyze-models](../../hanalyze-models/README.ja.md)
- 加速寿命試験: [usage-reliability.ja.md](usage-reliability.ja.md)
- 生存時間解析: [10-survival.ja.md](10-survival.ja.md)
