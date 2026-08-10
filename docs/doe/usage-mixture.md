# Mixture Design

Module: `Hanalyze.Design.Mixture` (`hanalyze-design`)

A DoE for the constrained setting where component proportions always sum to
1. Common in materials and chemical process work, where "increasing A means
decreasing B or C." Each experimental point must satisfy `x_i ≥ 0` and
`Σ x_i = 1`, so factors can't be varied independently the way they can in an
ordinary factorial design.

| Method | Generation rule | # points |
|---|---|---|
| `SimplexLattice d` | each component takes a value from `{0, 1/d, …, 1}` summing to 1 | `C(m+d−1, d)` |
| `SimplexCentroid` | any k components set equally to `1/k`, the rest to 0 (k = 1..m) | `2^m − 1` |

## 0. API

```haskell
data MixtureDesignType
  = SimplexLattice !Int   -- degree d
  | SimplexCentroid

data MixtureResult = MixtureResult
  { mdMatrix      :: !(Matrix Double)   -- nRuns × m, each row sums to 1
  , mdNComponents :: !Int               -- m (number of components)
  , mdNRuns       :: !Int               -- number of runs
  , mdType        :: !MixtureDesignType
  }

mixtureDesign :: MixtureDesignType -> Int -> Either Text MixtureResult
```

The second argument is the number of components m. `Matrix` is from
`hmatrix`.

## 1. Simplex Lattice

A lattice with 3 components and degree 2 (each component takes 0 / 0.5 / 1).

```haskell
import qualified Numeric.LinearAlgebra as LA
import Hanalyze.Design.Mixture

main :: IO ()
main = do
  case mixtureDesign (SimplexLattice 2) 3 of
    Left err -> putStrLn ("error: " ++ show err)
    Right r  -> do
      print (mdNComponents r, mdNRuns r)
      mapM_ print (LA.toLists (mdMatrix r))
```

Output:

```
(3,6)
[0.0,0.0,1.0]
[0.0,0.5,0.5]
[0.0,1.0,0.0]
[0.5,0.0,0.5]
[0.5,0.5,0.0]
[1.0,0.0,0.0]
```

The point count is `C(3+2−1, 2) = C(4,2) = 6`. It consists of **the 3
vertices (pure components) plus the 3 edge midpoints** (two components
mixed 50/50), filling the vertices and edges of the ternary plot. Raising
the degree d refines the spacing along the edges and produces interior
points as well.

## 2. Simplex Centroid

```haskell
  case mixtureDesign SimplexCentroid 3 of
    Left err -> putStrLn ("error: " ++ show err)
    Right r  -> do
      print (mdNRuns r)
      mapM_ print (LA.toLists (mdMatrix r))
```

```
7
[1.0,0.0,0.0]
[0.0,1.0,0.0]
[0.0,0.0,1.0]
[0.5,0.5,0.0]
[0.5,0.0,0.5]
[0.0,0.5,0.5]
[0.3333333333333333,0.3333333333333333,0.3333333333333333]
```

`2^3 − 1 = 7` points = 3 vertices + 3 edge midpoints + **1 overall
centroid**. The difference from SimplexLattice 2 is that final centroid
point, and **always including a run where all 3 components are mixed** is
the advantage of the centroid method. Use it when you want to see the
three-way synergy (the `x1·x2·x3` term in a Scheffé model).

## 3. Scaling to actual quantities

The output is a ratio (summing to 1), so if a total amount is fixed, just
multiply:

```haskell
  -- convert to a formulation with total mass 500 g
  let grams = LA.scale 500 (mdMatrix r)
```

Constrained mixtures with lower/upper bounds (e.g. component A must be at
least 20%) require an **Extreme Vertices design**, which this module does
not provide (the `Mixture.hs` haddock notes it's planned for a future
phase). Until then, use [Custom Design](usage-custom-design.md)'s
constraint support as a substitute.

## 4. Which to use

| Goal | Use |
|---|---|
| Fill vertices and edges systematically | `SimplexLattice d` (this doc) |
| Also want a centroid point mixing all components | `SimplexCentroid` (this doc) |
| Components have upper/lower bound constraints | [Custom Design](usage-custom-design.md) |
| A design over independently variable factors | [Factorial / RSM](01-doe.md) |
| Adding runs to an existing mixture experiment | [D-optimal augment](usage-doptimal-augment.md) |

## See also

- Package: [hanalyze-design](../../hanalyze-design/README.md)
- DoE entry point: [01-doe.md](01-doe.md)
- Constrained design: [usage-custom-design.md](usage-custom-design.md)
