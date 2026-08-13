# hanalyze (umbrella package)

The **umbrella package** that bundles the six split layers. Naming this one
package in `build-depends` gives you **all 200 modules of the lower layers
under their original module names**.

The Phase 106 package split cut the library into six layers on the premise
that **downstream imports would not change at all**. This package's
`reexported-modules` (200 of them) is what guarantees that:
`import Hanalyze.Model.LM` still works exactly as before the split.

On top of that, it implements **4 modules of its own** — ones that could not
live in any lower layer because they cut across several.

```
                  hanalyze-core        (Math / Stat-low / Optim / MCMC.Core)
                            |
              +-------------+-------------+
              |                           |
    hanalyze-frame        hanalyze-bayes
    (Data / DataIO)               (HBM DSL / MCMC / VI)
              |                           |
              +-------------+-------------+
                            |
                  hanalyze-models      (ML zoo / Causal / BayesOpt)
                            |
                  hanalyze-design      (DoE)
                            |
                  hanalyze-viz         (Vega-Lite / report)
                            |
                  ★ hanalyze           (this package; re-exports 200 modules)
                        /        \
       hanalyze-cli    hanalyze-plot
```

| Layer | Re-exported modules | README |
|---|---|---|
| `-core` | 44 | [README.md](../hanalyze-core/README.md) |
| `-frame` | 14 | [README.md](../hanalyze-frame/README.md) |
| `-bayes` | 26 | [README.md](../hanalyze-bayes/README.md) |
| `-models` | 67 | [README.md](../hanalyze-models/README.md) |
| `-design` | 30 | [README.md](../hanalyze-design/README.md) |
| `-viz` | 19 | [README.md](../hanalyze-viz/README.md) |
| **Total** | **200** | + 4 of its own = 204 |

## The 4 modules implemented here

Each one unifies model types from several layers behind a single API, which is
why none of them fits in a lower layer. They share one design rule: they
**never depend on hgg**, so they work in environments where static
rendering is unavailable.

| Module | Role |
|---|---|
| `Hanalyze` | The quickstart entry point — one `import Hanalyze` covers model fitting, basic statistics, plotting and CSV I/O |
| `Hanalyze.Fit` | The unified fit operator `\|->` and the `*Spec` types. It gathers LM, GLM, GAM, GP, regularized regression, SVM and DoE workflows behind a single verb |
| `Hanalyze.Diagnostics` | Fine-grained API for using a fitted model *as numbers* — point prediction, coefficient summaries (t/z, p-values, 95% CI), bootstrap, smooth-term F tests |
| `Hanalyze.Model.Wrappers` | Plotting wrapper types and their smart constructors, plus the `Fit` class itself |

The unified verb from `Fit` is the core idea:

```haskell
-- Hanalyze/Fit.hs:396-398
infixl 1 |->
(|->) :: (ColumnSource d, Fit spec) => d -> spec -> Fitted spec
d |-> spec = fitWith spec d
```

Swapping the `spec` swaps the model: `df |-> lm "x" "y"` becomes
`df |-> gp defaultGP "x" "y"` or `df |-> regularized cfg ["x1","x2"] "y"`.
There is also `|->!`, the IO variant with progress reporting.

## Getting started

For installation (GHC / cabal versions, sibling repository layout) see the
[repository README](../README.md).

```cabal
build-depends: hanalyze
```

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Hanalyze
import Hanalyze.Fit         (lm, (|->))
import Hanalyze.Diagnostics (modelReport, showReport)
import qualified Data.Text.IO      as TIO

main :: IO ()
main = do
  Right df <- loadAuto "flights.csv"
  let m = df |-> lm "month" "dep_delay"
  TIO.putStrLn (showReport (modelReport m))
```

```
term                    estimate     std.err        stat     p.value   [2.5%, 97.5%]
(Intercept)              -2.1667     13.1762     -0.1644      0.8774   [-38.7495, 34.4162]
x                         6.6667      8.3333      0.8000      0.4685   [-16.4704, 29.8037]
```

> Term names in the coefficient table are currently fixed to `(Intercept)` /
> `x`; the column name you passed (`month`) is not reflected
> (`Diagnostics.hs:120` / `:187`), because `LMModel` / `GLMModel` do not carry
> column names.

`import Hanalyze` alone reaches CSV loading (`loadAuto`), descriptive
statistics, tests, and scatter / bar / histogram plots. Anything beyond that
(DoE, HBM, optimisation) is one extra import away.

## Trimming your dependencies

If you only need to, say, generate a design table or reshape a CSV, depend on
that single layer instead of the umbrella and you compile far less code:

```cabal
build-depends: hanalyze-design   -- just DoE generation
build-depends: hanalyze-frame    -- just CSV I/O and dataframe ops
```

See the per-layer READMEs in the table above for what each one covers.

## Tests

The `hanalyze-test` suite (`test/Spec.hs`, via hspec-discover) collects around
130 `*Spec` modules. This is the main regression suite for the whole library.

```bash
cabal test hanalyze-test
```

The hgg diagnostics were moved to `hanalyze-plot-test` in the
separate package [`hanalyze-plot`](../hanalyze-plot/README.md)
in Phase 106.4, to avoid a package cycle. That package builds via
`cabal build --project-file=cabal.project.plot`, not as part of this
package's own `cabal test`.

## Related docs

- Quickstart: [docs/01-quickstart.md](../docs/01-quickstart.md)
- Using `|->`: [docs/io/04-fit-api.md](../docs/io/04-fit-api.md)
- Comparison against PyMC: [docs/02-pymc-comparison.md](../docs/02-pymc-comparison.md)
- Full capability list: [repository README](../README.md)

← [repository README](../README.md)
