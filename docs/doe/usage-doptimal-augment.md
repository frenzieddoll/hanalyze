# Adding Experiments via D-optimal Augmentation

Module: `Hanalyze.Design.Optimal` (`hanalyze-design`)

Used when you want to keep the runs you've already collected and just add
N more runs to improve precision. It selects N points from a candidate set
while **holding the existing rows fixed**, using Fedorov exchange to
maximize the optimality criterion of the completed design (existing +
added).

The difference from building a design from scratch with `dOptimal` /
`aOptimal` is that **existing rows are never swap candidates**. This fits
real-world situations where the spec changed mid-experiment or an
unexpected interaction showed up and you need to add runs.

If you're using this through the Custom Design menu (`AddRuns` etc.), see
[usage-augment-splitplot.md](usage-augment-splitplot.md). This doc covers
the lower-level API that underlies it.

## 0. API

```haskell
data AugmentResult = AugmentResult
  { arNewIndices  :: ![Int]         -- indices of the selected candidates
  , arNewRows     :: ![[Double]]    -- actual values of the added points
  , arFullDesign  :: ![[Double]]    -- existing ++ added (existing order preserved)
  , arInitialCrit :: !Double        -- criterion value of the existing design alone
  , arFinalCrit   :: !Double        -- criterion value of the completed design
  }

augmentDesign
  :: OptCriterion    -- DOpt / AOpt / IOpt / EOpt / GOpt / …
  -> [[Double]]      -- existing rows (fixed)
  -> Int             -- N (number of rows to add)
  -> [[Double]]      -- candidate set
  -> Int             -- seed
  -> AugmentResult

candidateGrid :: Int -> Int -> [[Double]]           -- k factors × numLevels levels
quadraticCandidates :: Int -> Int -> [[Double]]     -- rows already expanded to a quadratic model
```

All rows are passed as **model rows (one row of the model matrix)**. For a
linear model that's `[1, x1, x2]`; for a quadratic model,
`[1, x1, x2, x1², x2², x1·x2]`. Note that you must supply the intercept
column yourself (`quadraticCandidates` returns already-expanded rows).

Randomness uses **only an `Int` seed** (an internal LCG, `pseudoShuffle`).
It doesn't take `MWC.GenIO` — it's a pure function, so the same seed always
gives the same result.

## 1. Usage

Starting from a 4-run 2² factorial with a linear model in 2 factors already
run, add 3 more runs.

```haskell
import Hanalyze.Design.Optimal

main :: IO ()
main = do
  let existing = [ [1, -1, -1], [1, 1, -1], [1, -1, 1], [1, 1, 1] ]
      cands = [ [1, x1, x2] | x1 <- [-1, 0, 1], x2 <- [-1, 0, 1] ]
      r = augmentDesign DOpt existing 3 cands 42
  print (arNewIndices r)
  mapM_ print (arNewRows r)
  print (arInitialCrit r, arFinalCrit r)
  print (length (arFullDesign r))
```

Output:

```
[0,6,2]
[1.0,-1.0,-1.0]
[1.0,1.0,-1.0]
[1.0,-1.0,1.0]
(64.0,320.0)
7
```

Reading it:

- **`arInitialCrit` = 64 → `arFinalCrit` = 320** — `DOpt`'s criterion is
  `det(XᵀX)`. With the existing 4 runs, `det = 4³ = 64`; after adding runs
  it rises to 320
- The runs chosen were **repeats of existing corner points**. Since the
  model is already saturated (3 parameters / 4 runs), repeating a corner
  raises `det` more than adding new levels would. D-optimal is a criterion
  for "estimate the model's coefficients precisely" — **if you want to
  check model adequacy (lack-of-fit), you need to add center points
  yourself**
- `arFullDesign` is the existing 4 + added 3 = 7 rows. Order of the
  existing rows is preserved, so run order or block information tracked
  separately elsewhere won't get out of sync

## 2. Degenerate cases

If `N ≤ 0` or the candidate set has fewer than N points, **nothing is
added** (an empty result is returned). No exception is thrown.

```haskell
  let r0 = augmentDesign DOpt existing 0 cands 42
  print (arNewIndices r0, arFullDesign r0 == existing)
```

```
([],True)
```

`arFinalCrit >= arInitialCrit` always holds, by construction of Fedorov
exchange (only improving swaps are accepted).

## 3. Building a candidate set

```haskell
  print (candidateGrid 2 3)
```

```
[[-1.0,-1.0],[-1.0,0.0],[-1.0,1.0],[0.0,-1.0],[0.0,0.0],[0.0,1.0],
 [1.0,-1.0],[1.0,0.0],[1.0,1.0]]
```

`candidateGrid k numLevels` returns a grid evenly dividing `[-1, 1]`
(**it does not include the intercept column**, so prepend `1 :` yourself
for a linear model). For rows already expanded to a quadratic model, use
`quadraticCandidates k numLevels`.

## 4. Choosing a criterion

| Criterion | Meaning | When to use |
|---|---|---|
| `DOpt` | `max det(XᵀX)` | want to estimate coefficients precisely (default) |
| `AOpt` | `min trace((XᵀX)⁻¹)` | want to reduce average estimation variance |
| `IOptRegion m` | minimizes average prediction variance over a region | the goal is **prediction** (e.g. RSM) |
| `EOpt` | maximizes the smallest eigenvalue | want to guarantee precision in the worst direction |
| `GOpt` | minimizes the maximum leverage | don't want any point with outsized influence |
| `BayesianD k` | `max det(XᵀX + K)` | prior information available (DuMouchel-Jones) |
| `Compound ws` | weighted sum of the above | multiple objectives (scale them yourself) |

> The legacy `IOpt` degenerates to `p/n` (independent of the design) due to
> its self-moment approximation. If you need I-optimal, use
> **`IOptRegion`** (the region-integral version) instead — noted in the
> `Optimal.hs` haddock.

## 5. Which to use

| Goal | Use |
|---|---|
| Fix an existing experiment and add N runs | `augmentDesign` (this doc) |
| Select N points from scratch from a candidate set | `dOptimal` / `aOptimal` / `optimalDesign` |
| Add via the Custom Design menu | [usage-augment-splitplot.md](usage-augment-splitplot.md) |
| D-optimal with a prior distribution | [usage-bayesian-d.md](usage-bayesian-d.md) |
| A regular factorial design is enough | [01-doe.md](01-doe.md) |
| Move toward the optimum sequentially | [usage-sequential-rsm.md](usage-sequential-rsm.md) |

## See also

- Package: [hanalyze-design](../../hanalyze-design/README.md)
- Custom Design's augment menu: [usage-augment-splitplot.md](usage-augment-splitplot.md)
- Bayesian D-optimal: [usage-bayesian-d.md](usage-bayesian-d.md)
- Low-level API listing: [internal/09-doe-lowlevel.md](../internal/09-doe-lowlevel.md)
