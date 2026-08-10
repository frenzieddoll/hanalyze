# Space-Filling Designs (LHS / Maximin LHS / Halton)

Module: `Hanalyze.Design.SpaceFilling` (`hanalyze-design`)

A DoE style for placing training points for simulations (computer
experiments) or surrogate models. Since there's no experimental error and
the response isn't necessarily smooth, it's more advantageous to
**cover the input space evenly** than to push points toward the corners the
way a factorial design does.

| Function | Method | Determinism |
|---|---|---|
| `latinHypercube` | stratified random sampling (one cell per dimension, used once each) | random |
| `latinHypercubeMaximin` | LHS as an initial solution, then maximize minimum inter-point distance | random |
| `haltonDesign` | Halton low-discrepancy sequence | **deterministic** |

All outputs are points in `[0, 1)^d`. Scaling to actual factor ranges is up
to the caller (§3).

## 0. API

```haskell
data SpaceFillingDesign = SpaceFillingDesign
  { sfdMatrix  :: !(Matrix Double)  -- n × d, points in [0,1)^d
  , sfdNPoints :: !Int
  , sfdNDims   :: !Int
  , sfdMinDist :: !Double           -- minimum Euclidean distance between points (larger is better)
  , sfdMethod  :: !Text             -- "LHS" / "MaximinLHS" / "Halton"
  }

latinHypercube        :: Int -> Int        -> MWC.GenIO -> IO SpaceFillingDesign
latinHypercubeMaximin :: Int -> Int -> Int -> MWC.GenIO -> IO SpaceFillingDesign
                      -- n      d     nTries
haltonDesign          :: Int -> Int -> SpaceFillingDesign      -- pure function

designMinDistance :: Matrix Double -> Double
```

`latinHypercube*` are `IO` actions taking `MWC.GenIO`; `haltonDesign` is a
pure function that uses no randomness.

## 1. Latin Hypercube

8 points × 2 dimensions. Use `MWC.create` (fixed seed) for reproducibility.

```haskell
import qualified Numeric.LinearAlgebra as LA
import qualified System.Random.MWC     as MWC
import Hanalyze.Design.SpaceFilling

main :: IO ()
main = do
  gen <- MWC.create
  lhs <- latinHypercube 8 2 gen
  print (sfdNPoints lhs, sfdNDims lhs, sfdMethod lhs)
  print (sfdMinDist lhs)
  mapM_ print (LA.toLists (sfdMatrix lhs))
```

Output:

```
(8,2,"LHS")
0.15917993786380238
[3.1012953603702514e-3,0.7336283364127267]
[0.5283808232035737,0.3147626793809132]
[0.21760800849316259,0.10765495638623752]
[0.2699204433479854,0.6097739846510616]
[0.6812301915661529,0.8840376332763719]
[0.8901926889521693,0.20508481375541604]
[0.4619085958091119,0.45939910495740116]
[0.8153674317579357,0.7837126085149456]
```

Each column is split into 8 cells `[0, 1/8), [1/8, 2/8), …`, and **every
column uses each cell exactly once** — that's the defining property of LHS.
Marginal distributions come out even per column, but viewed in 2D, some
points can still land close together by chance (here, minimum distance
0.159).

## 2. Maximin LHS for more separation

A local search that keeps the stratification but swaps values within a
column to improve the minimum inter-point distance.

```haskell
  gen2 <- MWC.create
  mx <- latinHypercubeMaximin 8 2 1000 gen2
  print (sfdMethod mx, sfdMinDist mx)
```

```
("MaximinLHS",0.26779098411089647)
```

For the same 8 points in 2 dimensions, the minimum distance goes from
**0.159 → 0.268 (about 1.7×)**. `nTries` is the total number of swap
attempts; around 1000 gives a practical improvement (depends on n and d).
More points need more tries.

## 3. Halton (deterministic)

```haskell
  let hal = haltonDesign 8 2
  print (sfdMethod hal, sfdMinDist hal)
  mapM_ print (LA.toLists (sfdMatrix hal))
```

```
("Halton",0.167244369149893)
[0.5,0.3333333333333333]
[0.25,0.6666666666666666]
[0.75,0.1111111111111111]
[0.125,0.4444444444444444]
[0.625,0.7777777777777777]
[0.375,0.2222222222222222]
[0.875,0.5555555555555556]
[6.25e-2,0.8888888888888888]
```

The first column is a van der Corput sequence in base 2, the second in base
3. **The same `(n, d)` always yields the same point set**, so it's
reproducible without seed management, and adding points later doesn't move
the existing ones (well-suited to sequential addition). At higher
dimensions, though, correlation shows up between low-order bases, so use it
as a rule of thumb for d ≲ 10.

## 4. Scaling to factor ranges and checking quality

```haskell
  -- map to temperature 180–220 °C, pressure 10–30 MPa
  let lo = LA.fromList [180, 10]
      hi = LA.fromList [220, 30]
      scaled = LA.fromRows
        [ lo + (hi - lo) * row | row <- LA.toRows (sfdMatrix lhs) ]

  -- measure the minimum inter-point distance of an arbitrary matrix
  print (designMinDistance (LA.fromLists [[0, 0], [3, 4]]))
```

```
5.0
```

`designMinDistance` returns 0 when there are fewer than 2 rows. `sfdMinDist`
is the value computed right after generation, on the `[0,1)^d` scale, so
**if you want to compare distances after scaling, recompute with
`designMinDistance`**. Comparing methods (LHS vs. Maximin vs. Halton) is
easiest by lining up minimum distances for the same `(n, d)`.

## 5. Which to use

| Goal | Use |
|---|---|
| Spread points evenly, quickly | `latinHypercube` (this doc) |
| Maximize inter-point distance (e.g. GP training points) | `latinHypercubeMaximin` (this doc) |
| Reproducible without a seed, add points later | `haltonDesign` (this doc) |
| A real physical experiment with measurement error | [Factorial / RSM](01-doe.md) |
| Screen a small number of influential factors from many | [DSD / screening](01-doe.md) |
| Sequentially pick the next single point smartly | [`Optim.BayesOpt`](../optim/01-singleobj.md) |
| The raw quasi-random sequences (`haltonSamples` etc.) | `Hanalyze.Stat.QuasiRandom` (core layer; see [semiconductor-design-workflow.md](../manual/semiconductor-design-workflow.md) §5.2 for a usage example) |

## See also

- Package: [hanalyze-design](../../hanalyze-design/README.md)
- DoE entry point: [01-doe.md](01-doe.md)
- Application to a semiconductor process: [manual/semiconductor-design-workflow.md](../manual/semiconductor-design-workflow.md) §4.3
