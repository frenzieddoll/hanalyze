# DAG Extraction — Constraints and Workarounds (Phase 38)

> 🌐 **English** | [日本語](dag-extraction.ja.md)

Behavior of `Hanalyze.Model.HBM.buildModelGraph` / `extractDeps` and common
pitfalls when writing user models / helpers, documented with Phase 38 validation
examples.

## Overview: How DAG Extraction Works

- `extractDeps` walks the model in `Model Track` type.
- `Track` is a pair `(value, set of touched latent names)` (HBM.hs:2167).
- Each `Sample n d` node's parents come from `distDepsT d` on the `d`'s argument Tracks;
  continuation passes `trackVar n 1.0` (deps = {n}). → Downstream Track operations
  preserve **n as deps, not distant ancestors (mu, tau)**.
- Each `Observe n d ys` node's parents come from `distDepsT d`; observation count is `length ys`.
- Each `Deterministic n v` node's parents come from `trackDeps v`. **After Phase 38 fix**:
  continuation passes `Track (trackVal v) {n}`, so downstream takes n as parent.
- Each `Potential n v` node's parents come from `trackDeps v` (recorded as LatentN,
  `nodeDist = "Potential"`).
- Duplicate `Observe n` calls are merged by `mergeByName` into one node
  (observation counts summed, parent sets unioned).

## Trap List

### Trap 1: `distDepsT` non-exhaustive on new distributions (Phase 37 → 38, actual impact)

**Symptom**: `buildModelGraph` crashes on runtime with `Non-exhaustive patterns in function
distDepsT` when model encounters `SkewNormal` / `OrderedLogistic` / `MvStudentT`, etc.

**Root cause**: `distDepsT` (HBM.hs:2257-) lacked cases for 11 distributions added in Phase 37.

**Phase 38 fix**: Added 11 cases. **When adding a new `Distribution`, remember to add
`distDepsT` case**:

```haskell
distDepsT (NewDist a b c) = trackDeps a <> trackDeps b <> trackDeps c
-- If args are non-Track only (e.g., Int), use mempty
distDepsT NewDist{} = mempty   -- Example: DiscreteUniform / HyperGeometric
```

### Trap 2: Deterministic transparency (fixed in Phase 38)

**Symptom**: Passing `nonCenteredNormal "theta" mu tau` 's `theta` (det value) to
`observe "y" (Normal theta 1)` results in y's parents being `{theta}` only, NOT
`{mu, tau, theta_raw}` (theta's distant ancestors) — wait, that's actually correct.
But if the DAG shows distant parents instead, that's the bug.

**Root cause (pre-fix)**: `extractDeps` 's `Deterministic` handler was passing
continuation the original Track `v` unchanged, so deps weren't relabeled.

**Phase 38 fix**: Continuation now receives `Track (trackVal v) (Set.singleton nm)`,
preserving value but relabeling deps to det name.

```haskell
go (Free (Deterministic nm v k)) acc =
  let parentDeps = trackDeps v
      node = Node nm LatentN "Deterministic" parentDeps
      v'   = Track (trackVal v) (Set.singleton nm)
  in go (k v') (node : acc)
```

### Trap 3: Helper "throws away" deterministic as side-effect; doesn't form plate

**Symptom**: Using `ar1Latent "x" 3 0.8 0.3` 's return `xs` results in each `x_t`
having parents `{x_raw0, …, x_raw_t}` (distant ancestors), not plate-style chain `x_{t-1} → x_t`.

**Root cause (pre-fix)**: ar1Latent had this structure:

```haskell
let xs = scanl (\xPrev (rt, _) -> phi*xPrev + sigma*rt) x0 (zip ...)
_ <- mapM (\(t, x) -> deterministic (...) x) (zip [0..] xs)
return xs   -- xs is pure Haskell scanl result; each element is full chain deps
```

`scanl` builds `xs` in **pure computation**, so later `deterministic` registrations
can't relabel deps retroactively. Each `x_t` 's Track retains construction-time deps.

**Phase 38 fix**: Rewrite to monadic recursion; each step passes next step the
deterministic return value (relabeled by det name):

```haskell
x0 <- deterministic (name <> "_0") (stat * head raws)
let chain _ [] = return []
    chain xPrev ((t, rt):rest) = do
      xt <- deterministic (name <> "_" <> ...) (phi * xPrev + sigma * rt)
      xs' <- chain xt rest
      return (xt : xs')
xs' <- chain x0 (zip [1..] (tail raws))
return (x0 : xs')
```

**General principle** (for future helpers):

- If helper's final step returns a `deterministic` result → OK
  (`nonCenteredNormal` returns `deterministic name (loc + scale * raw)` last).
- If helper ends with `mapM (\(i, p) -> deterministic ...) xs`, return **the mapM result** → OK
  (`dirichlet` uses this form).
- Helper discarding deterministic result with `_ <-` and returning original pure list
  → **breaks DAG extraction**.

### Trap 4: `ModelP r` is rank-2 → can't pattern-bind

**Symptom**:

```haskell
let m :: HBM.ModelP () = do { … }
```

Compilation error: `Couldn't match expected type 'forall a. ...' with
'Free (ModelF Double) ()'`.

**Root cause**: `type ModelP r = forall a. (Floating a, Ord a) => Model a r`
is rank-2. Pattern binding doesn't polymorphize inside let-bindings.

**Workaround**: Separate type signature and binding (function-style):

```haskell
let m :: HBM.ModelP ()
    m = do { … }
```

All test/Spec.hs / demo examples use this form.

### Trap 5: Same-name observe merging (`mergeByName`)

**Spec**: Calling `forM_ [0..n-1] $ \i -> observe "y" (Normal mu 1) [ys !! i]`
repeatedly, `buildModelGraph` merges into one node:

- `nodeKind = ObservedN (n_1 + n_2 + …)` (observation counts summed)
- `nodeDeps = ∪ (each parent set)`

This design prevents observation node explosion (1000 obs don't spawn 1000 nodes),
but **when different y_i have different parents, split names to avoid mixing**.
E.g., GLMM helper `glmmRandomIntercept` uses `y_0, y_1, …` (one observe per observation)
so each `y_i` 's parent `u_g(i)` (group effect) stays separated correctly.

### Trap 6: `Categorical` / `Mixture` args are all parents

`distDepsT (Categorical ps) = mconcat (map trackDeps ps)`,
`distDepsT (Mixture ws ds) = mconcat (map trackDeps ws) <> mconcat (map distDepsT ds)`.
When you pass `dirichlet "p" α` 's return `pis = [p_0, p_1, p_2]` to
`observe "y" (Categorical pis) ys`, y's parents are `{p_0, p_1, p_2}` (all deterministics).
This is correct plate-style behavior.

### Trap 7: Local variables assembled into observation `eta` — deps propagation

GLMM-style `eta = beta_0 * x + u_g + …` combines parents as a Haskell value (Track),
and Track's `+`/`*` preserve union of deps (HBM.hs:2185-2196). So `observe "y_i"
(Normal eta sigma) [y]`'s parents are `{beta_0, …, u_g, sigma}`, etc. — all latents
composing eta (as expected).

But **via `realToFrac`**, Track deps may be lost: HBM.hs:2218 's `Real Track` instance
uses `toRational . trackVal`, dropping deps. `realToFrac (xs !! i) :: Double` breaks DAG.
Conversely, inside helpers where `map realToFrac xRow :: [a]` with `a = Track` runs,
Track flows through `fromInteger`/`fromRational` paths, **deps rebuild** → no issue.
**Boundary**: Track → Double explicit conversion **erases deps**; Double → Track
is `trackConst`, a **constant with no deps**.

## Helper Writing Checklist (DAG-safe)

Before writing new hierarchical helpers:

1. Does helper's return value come from **deterministic / sample**? Or pure computation?
2. Is helper ending with `_ <- mapM (\… -> deterministic …)`, discarding result and
   returning the original pure list? (Breaks DAG.)
3. Added new `Distribution` constructor? **Add cases to `distDepsT` /
   `distName` / `logDensity` (and `logDensityObs` / `obsLogSum` / sample) — all**.
4. Written a test validating `buildModelGraph` 's `mgEdges` on a model using
   this helper? (Regression prevention.)

## Traps added in Phase 40 (plate-related)

### Trap 8: PlateEnd without PlateBegin is silently ignored (defensive)

`extractDeps` 's plate context stack is LIFO. Bare `PlateEnd` (pop on empty stack)
is **silently dropped**. This defends against user code manually `liftF`-ing plate
primitives with broken balance. Normally use `plate name n body` helper, guaranteeing
bracket balance.

### Trap 9: Same-name plate with different sizes → last value wins

```haskell
do _ <- plate "g" 3 $ ...
   _ <- plate "g" 8 $ ...   -- mgPlates' "g" becomes 8
```

Consequence of `Map.insert` overwrite. Usually same-name plate assumes same size,
but silent overwrite on mismatch isn't detected. Future: warn on size divergence?

### Trap 10: Same-name observe inside plate; merges to first plate

`mergeByName` keeps first occurrence (deps union + obs count sum) → same-name
observe inside plate **fuses into one node**, retaining first plate stack.
Same applies to observes outside plate.

→ **Recommendation**: Use distinct names `y_0, y_1, …` when observing separately
within plate (aligns with Phase 38 Trap 5).

## Traps added in Phase 63 (data slot-related)

### Trap 11: dataNamedObs obs→slot edge is "value match" heuristic

`dataNamedObs` 's snd view (raw `[Double]`) can't flow `Track` tags
(observe's ys are naked Doubles), so `extractDeps` walks tail checking
**per-obs concatenated ys vs. slot raw value for exact match** (PyMC
`make_compute_graph` 's `obs -> y` homomorphism = slot is obs **child**, drawn
below obs). Per-point loop (`observe "y" … [y]` N times) concatenates to match.
Consequences:

- **Accidental match = false edge**: x slot value coincidentally equals observe's ys,
  → spurious obs→slot edge drawn (Phase 60.6 plate "unique length" + display-only
  heuristic limit).
- Multiple slot values match → edge on all.
- Transform slot value before passing to observe → no match, no edge
  (slot shows source rank).
- **`dataNamedObs "y"` + `observe "y"` naming convention** → `mergeByName` merge
  takes priority (data container absorbed into obs node); value-match edge exempt
  (self-loop prevention).

→ **Recommendation**: Follow docs convention: **same name for slot and observe**
(container absorbed display), or split names but pass slot raw value unmodified
to observe.

## References

- Implementation: `src/hanalyze/Analyze/Model/HBM.hs`
  - `extractDeps` (line 2228-), `distDepsT` (line 2257-),
    `Deterministic` handler (line 2247-)
  - `ar1Latent` (line 1683-), `nonCenteredNormal` (line 1719-),
    `dirichlet` (line 1809-), `glmmRandomIntercept` (line 1750-)
- Validation tests: `test/Spec.hs` 's `describe "(Phase 38: …)"` 3 blocks
  (easy 6 / representative 9 / complex 9)
- DAG gallery (mermaid): `docs/bayesian/dag-gallery.md`
- Phase 38 plan: `specification/phases/phase-38-model-dag-verification.md`
- Phase 40 plate notation guide: `docs/bayesian/plate-notation.md`
- Phase 40 plan: `specification/phases/phase-40-plate-notation.md`
