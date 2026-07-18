# Plate Notation (Hanalyze.Model.HBM)

> 🌐 **English** | [日本語](plate-notation.ja.md)

> Introduced in Phase 40 (2026-05-30). Pyro / NumPyro-style plate-block syntactic sugar:
> add `plate "name" n $ ...` anywhere in do-block, and `buildModelGraph` outputs a DAG
> with PyMC `model_to_graphviz`-equivalent plate (rounded rect + size number).

## Why Plate Notation?

Hierarchical models (8-schools / random intercept GLMM / multi-level mixed effects)
are built from repeated indexed RV (`eta_0, eta_1, …, eta_{n-1}`).
Listing all individually in DAG → explosion (n=1000 → 1000 nodes).

**Plate notation** abstracts "identical distribution + identical parent set, repeated"
into one collective node — achieving the same abstraction level as PyMC / Pyro / Stan
documentation diagrams.

## API (3 functions)

```haskell
-- Bracket: wrap any region of do-block in plate
plate :: Text -> Int -> Model a r -> Model a r

-- Convenience sugar: plate name n (forM [0..n-1] f) equivalent
plateI :: Text -> Int -> (Int -> Model a r) -> Model a [r]

-- Alias (= plate; low-level primitive)
withPlate :: Text -> Int -> Model a r -> Model a r
```

All exported from `Hanalyze.Model.HBM`.

## Basic example: Eight schools

```haskell
import qualified Hanalyze.Model.HBM as HBM
import qualified Data.Text as T
import Control.Monad (forM)

eightSchools :: HBM.ModelP ()
eightSchools = do
  mu  <- HBM.sample "mu"  (HBM.Normal 0 5)
  tau <- HBM.sample "tau" (HBM.HalfCauchy 5)
  etas <- HBM.plate "school" 8 $ forM [0..7 :: Int] $ \j ->
            HBM.sample ("eta_" <> T.pack (show j)) (HBM.Normal 0 1)
  _ <- HBM.plate "school" 8 $ forM_ [0..7 :: Int] $ \j ->
         HBM.observe ("y_" <> T.pack (show j))
                     (HBM.Normal (mu + tau * (etas !! j)) 1)
                     [ys !! j]
  return ()
```

After DAG extraction, `mgPlates` contains `Map.fromList [("school", 8)]`;
`eta_*` / `y_*` nodes' `nodePlates` is `["school"]`, `mu` / `tau` is `[]`.

### Visualization (2 modes)

hanalyze provides 2 render modes:

- **expanded** (pass `buildModelGraph` result as-is): All N nodes listed inside plate
  (eta_0..eta_7; debug-oriented).
- **collapsed** (`collapseIndexedPlateNodes` applied once): `<prefix>_<digit>` pattern
  nodes **fold to 1 representative** → **PyMC `pm.model_to_graphviz` equivalent.**

```haskell
import qualified Hanalyze.Model.HBM       as HBM
import qualified Hanalyze.Viz.ModelGraph  as VMG
import qualified Hanalyze.Viz.ModelGraphDot as VMGD

main = do
  let g  = HBM.buildModelGraph eightSchools          -- expanded
      gc = HBM.collapseIndexedPlateNodes g           -- collapsed (PyMC equivalent)
  VMG.renderModelGraph "8schools.html"      "8 schools (expanded)"  g
  VMG.renderModelGraph "8schools-pymc.html" "8 schools (collapsed)" gc
  VMGD.writeModelGraphDot "8schools.dot"      g
  VMGD.writeModelGraphDot "8schools-pymc.dot" gc
  -- $ dot -Tpng 8schools-pymc.dot -o 8schools-pymc.png
```

`collapseIndexedPlateNodes` aggregation condition (heuristic):

- Same `nodePlates` (plate stack) membership
- Name matches `<prefix>_<digit+>$` pattern
- 2+ nodes sharing same `prefix`
- Identical `nodeDist` (distribution name)

Aggregation result:

- Representative node name = `prefix` (e.g., `eta_0..eta_7` → `eta`)
- Observation nodes aggregate observation count (`y_0..y_7` (each n=1) → `y (n=8)`)
- Edges deduplicated by aggregated names, self-loops removed
- Nested plate (school × student) collapses via fixed-point: inner → outer
  converges to 1 representative node

Non-plate "same naming convention, name collision" (e.g., fixed effect `beta_0`
vs group effect `u_0`) is **not mistakenly aggregated** (plate boundary constraint).
Unaggregated (singleton) / different distribution / naming pattern divergence stays as-is.

## Nested plate (multi-level)

Nest `plate` calls: `nodePlates` stack outer→inner:

```haskell
m = do
  _ <- HBM.plate "school" 3 $ forM_ [0..2 :: Int] $ \j ->
         HBM.plate "student" 2 $ forM_ [0..1 :: Int] $ \i ->
           HBM.sample ("y_" <> T.pack (show j) <> "_" <> T.pack (show i))
                      (HBM.Normal 0 1)
  return ()
```

`y_1_0` 's `nodePlates = ["school", "student"]`; mermaid / dot output
as nested subgraph (student inside school).

## Crossed plate

"subject × time" complete crossing → **PyMC also uses 2 plate side-by-side** convention.
hanalyze follows the same:

```haskell
m = do
  _ <- HBM.plate "subject" 3 $ forM_ [0..2 :: Int] $ \s ->
         HBM.sample ("u_" <> T.pack (show s)) (HBM.Normal 0 1)
  _ <- HBM.plate "time" 2 $ forM_ [0..1 :: Int] $ \t ->
         HBM.sample ("v_" <> T.pack (show t)) (HBM.Normal 0 1)
  return ()
```

`mgPlates` has 2 entries: "subject" and "time"; nodes belong to one plate each.

## Composing with existing helpers

`dirichlet` / `nonCenteredNormal` / `ar1Latent` / `glmmRandomIntercept`
all become **plate-aware automatically** when wrapped in `plate`
(B2 design benefit):

```haskell
-- dirichlet wrapped in plate
_ <- HBM.plate "K" 3 $ HBM.dirichlet "pi" [1, 1, 1]

-- ar1Latent wrapped in plate
_ <- HBM.plate "T" 100 $ HBM.ar1Latent "x" 100 0.5 1

-- glmmRandomIntercept wrapped in plate
_ <- HBM.plate "subject" nGroups $
       HBM.glmmRandomIntercept HBM.GlmmGaussian xs gids ys
```

Latents / deterministics inside helpers register as plate members.

## PyMC Correspondence

| PyMC v5 | hanalyze (Phase 40) |
|---|---|
| `pm.Model(coords={"school": 8})` | (unnecessary; pass plate name + size directly) |
| `eta = pm.Normal("eta", 0, 1, dims="school")` | `etas <- plateI "school" 8 (\j -> sample ("eta_" <> show j) (Normal 0 1))` |
| `pm.model_to_graphviz(model)` | `VMGD.renderModelGraphDot (buildModelGraph m)` |
| Rounded rect + size number bottom-right | `subgraph cluster_X { labelloc="b"; label="X × N"; ... }` |
| Nested plate (rectangles) | Nested `subgraph cluster_*` |
| Crossed plate (overlapping) | 2 separate plates (PyMC also lacks true crossing rendering) |

## Samplers: No Impact

**None.** Plate is **visualization layer only**; `logJoint` / `logPrior`
/ `logLikelihood` / NUTS / Gibbs / VI all **transparently pass through**
`PlateBegin` / `PlateEnd`. NUTS on continuous-variable latents is unchanged
with/without plate.

## Traps (established Phase 38 + 40)

1. **Plate name ≠ RV name inside**: `"school"` is plate name,
   `"eta_0"`, `"eta_1"`, … are individual RV names. mermaid `subgraph plate_<name>`
   uses plate name side.
2. **Nested plate `nodePlates` order is outer→inner**: `["school", "student"]`
   maintains correct nesting order in cluster hierarchy.
3. **Same-name observe merge + plate alignment**: `mergeByName` keeps first
   occurrence's plate. Repeated same-name observe in plate → 1 node, first plate retained.
   (Normal: use distinct `y_0, y_1, …`.)
4. **`PlateEnd` without `PlateBegin` silently dropped**: Empty stack pop is defensive.
5. **Code change cost: only plate name addition**: Swapping `forM` to `plate "name" n $ forM`
   is zero-cost refactor.
6. **Large plate (N=1000) still renders as 1 plate node**: mermaid / dot lists all members
   in subgraph (O(N) lines). True plate icon (single symbol) requires future renderer extension.

## Gallery

Main model mermaid + dot examples: see [`dag-gallery.md`](dag-gallery.md)
Phase 40 section (future updates).
