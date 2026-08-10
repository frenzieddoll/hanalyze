# Sequential RSM

Module: `Hanalyze.Design.Sequential` (`hanalyze-design`)

Support helpers for the workflow of running RSM not as a single shot but as
a repeated loop of **"small design → fit → move along steepest ascent →
new design centered on the new point"** to home in on the optimum. This is
the standard approach when the search region is broad enough that fitting a
quadratic model from the start isn't yet warranted.

```
initial 2^k design ──fit──> linear coefficients ──steepestAscent──> candidate path of trial points
                                                          │
                          next CCD ←──sequentialCCD──── new center
```

The heavy numerics (fitting the quadratic model, solving for the extremum
analytically) live in [`Design.RSM`](01-doe.md); this module only handles
**generating the path and laying out the next design**.

## 0. API

```haskell
data SteepestAscentResult = SteepestAscentResult
  { sarDirection  :: !(Vector Double)  -- unit-normalized direction (length k)
  , sarStepPoints :: ![[Double]]       -- trial point sequence (length nSteps + 1, first = center)
  , sarMaximize   :: !Bool             -- True = ascent, False = descent
  }

steepestAscent
  :: Bool -> [Double] -> [Double] -> Double -> Int -> SteepestAscentResult
  --  maximize?  center     linear coeffs b  step size  # trial points

steepestAscentFromQuad
  :: Bool -> [Double] -> RSM.QuadFit -> Double -> Int -> SteepestAscentResult

data SequentialCCDResult = SequentialCCDResult
  { sccdCenter :: ![Double]      -- new design center (original coordinates)
  , sccdSpan   :: !Double        -- one-sided span (coded ±1 maps to center ± span in original coordinates)
  , sccdCoded  :: ![[Double]]    -- design in coded units (−α..+α)
  , sccdReal   :: ![[Double]]    -- design in original coordinates (= center + span · coded)
  }

sequentialCCD
  :: [Double] -> Double -> Int -> RSM.CCDType -> Int -> SequentialCCDResult
  --  new center   span    # factors k  CCD type      center point repeats
```

`steepestAscentFromQuad` is the variant that pulls the linear coefficients
out of an already-fit `QuadFit`, saving you the trouble of extracting them
yourself.

## 1. Drawing a path from a fit

Centered on temperature 200 °C / pressure 30, run a 2^2 + center-point
experiment, fit a linear model, and take 4 steps along steepest ascent.

```haskell
import qualified Numeric.LinearAlgebra as LA
import qualified Hanalyze.Design.RSM        as RSM
import qualified Hanalyze.Design.Sequential as Seq

main :: IO ()
main = do
  let xs = [[-1,-1], [1,-1], [-1,1], [1,1], [0,0]]           -- coded
      ys = [ 40 + 3*x1 + 4*x2 | [x1, x2] <- xs ]              -- observed response
      qf = RSM.fitQuadratic xs ys
      sa = Seq.steepestAscentFromQuad True [200, 30] qf 5.0 4
  print (LA.toList (Seq.sarDirection sa))
  mapM_ print (Seq.sarStepPoints sa)
```

Output:

```
[0.5999999999999999,0.7999999999999999]
[200.0,30.0]
[203.0,34.0]
[206.0,38.0]
[209.0,42.0]
[212.0,46.0]
```

- The direction is `b / |b|` = `(3,4)/5` = `(0.6, 0.8)`. **The factor with
  the larger coefficient moves further** — that's the nature of steepest
  ascent
- The point sequence is `center + i · step · direction` (step = 5.0). The
  first point is the center, so the length is `nSteps + 1 = 5`
- **Step size is interpreted on the scale of the original coordinates.**
  When factor units differ wildly (e.g. temperature in °C vs. pressure in
  MPa), it's safer to draw the path in coded coordinates and convert back
  afterward

In practice you'd run these points one at a time from the top of the list,
stopping once the response stops improving.

## 2. Placing the next CCD at the new center

```haskell
  let center = last (Seq.sarStepPoints sa)
      ccd    = Seq.sequentialCCD center 5.0 2 RSM.CCF 2
  print (Seq.sccdCenter ccd, Seq.sccdSpan ccd)
  print (length (Seq.sccdReal ccd))
  mapM_ print (Seq.sccdReal ccd)
```

```
([212.0,46.0],5.0)
10
[207.0,41.0]
[207.0,51.0]
[217.0,41.0]
[217.0,51.0]
[207.0,46.0]
[217.0,46.0]
[212.0,41.0]
[212.0,51.0]
[212.0,46.0]
[212.0,46.0]
```

With `RSM.CCF` (face-centered, α = 1), k = 2, and 2 center-point repeats,
that's **4 (factorial) + 4 (axial) + 2 (center) = 10 runs**. `sccdReal` has
already computed `center + span · coded`, so it can be used directly as the
experiment run sheet. `sccdCoded` gives the coded matrix for analysis.

Choosing `RSM.CCDType`:

| Type | α | When to use |
|---|---|---|
| `RSM.CCC α` | specified value (rotatable: `(2^k)^(1/4)`) | factors can range outside the box |
| `RSM.CCF` | 1 | **factors cannot go outside the range** (equipment limits, safety) |
| `RSM.CCI α` | 1 (factorial part shrunk by `1/α`) | the range is a hard upper bound |

## 3. When to stop iterating

Once the linear coefficients shrink and quadratic curvature becomes visible,
stop drawing the path, fit the CCD, and solve for the extremum with
`RSM.optimumPoint`. See the RSM section of [01-doe.md](01-doe.md) for
judging curvature and analyzing the quadratic model.

## 4. Which to use

| Goal | Use |
|---|---|
| Home in on the optimum stepwise from a wide search region | this doc (`steepestAscent` + `sequentialCCD`) |
| Fit a quadratic model from the start and solve for the extremum | [RSM (CCD / Box-Behnken)](01-doe.md) |
| Add runs to an existing experiment to improve precision | [D-optimal augment](usage-doptimal-augment.md) |
| Optimize multiple responses simultaneously | [`Design.MultiRSM`](01-doe.md) / Desirability |
| Black-box optimization without a model | [`Optim.BayesOpt`](../optim/01-singleobj.md) |

## See also

- Package: [hanalyze-design](../../hanalyze-design/README.md)
- RSM core: [01-doe.md](01-doe.md)
- Adding experiments: [usage-doptimal-augment.md](usage-doptimal-augment.md)
