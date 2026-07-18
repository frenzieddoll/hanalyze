# Single-objective optimization (`Hanalyze.Optim.*`)

> 🌐 **English** | [日本語](01-singleobj.ja.md)

> Related: [02-multi-objective.md](02-multi-objective.md) (multi-objective),
> [03-algorithm-guide.md](03-algorithm-guide.md) (**unified algorithm-selection + constraint guide**),
> [theory-singleobj.md](theory-singleobj.md) (theory),
> [theory-bayesopt.md](theory-bayesopt.md) (BO)

Five algorithms for minimising / maximising `f: ℝ^n → ℝ` exposed through a unified interface.

## Common interface (`Hanalyze.Optim.Common`)

```haskell
import Hanalyze.Optim.Common

data OptimResult = OptimResult
  { orBest      :: [Double]   -- best point x*
  , orValue     :: Double     -- best value f(x*) (in original direction)
  , orHistory   :: [Double]   -- per-iteration best-value trace
  , orIters     :: Int
  , orConverged :: Bool
  }

data StopCriteria = StopCriteria
  { stMaxIter :: Int
  , stTolFun  :: Double
  , stTolX    :: Double
  }

data Direction = Minimize | Maximize
```

Each optimiser provides roughly `runX :: XConfig -> ([Double] -> Double) -> [Double] -> IO OptimResult`.

## Choosing an algorithm

| Situation | Recommendation |
|---|---|
| 1D unimodal | **Brent / Golden Section** (`Hanalyze.Optim.LineSearch`) |
| Smooth + gradient (or numeric grad OK) | **L-BFGS** (`Hanalyze.Optim.LBFGS`) |
| Non-differentiable, low-dim (≤ 20) | **Nelder-Mead** (`Hanalyze.Optim.NelderMead`) |
| Non-convex, global, gradient-free (≤ 30 dim) | **Differential Evolution** (`Hanalyze.Optim.DifferentialEvolution`) |
| Non-convex, continuous, auto-tuning (10–100 dim) | **CMA-ES** (`Hanalyze.Optim.CMAES` / `Hanalyze.Optim.CMAESFull`) |
| Classic metaheuristic | **Simulated Annealing** (`Hanalyze.Optim.SimulatedAnnealing`) |
| Swarm intelligence | **Particle Swarm** (`Hanalyze.Optim.ParticleSwarm`) |

---

## 1. Nelder-Mead — `Hanalyze.Optim.NelderMead`

A simplex of n+1 vertices is updated by reflect / expand / contract / shrink.
Gradient-free; effective for low-dimensional local optimisation. The standard for
R's `optim(method="Nelder-Mead")`.

```haskell
import qualified Hanalyze.Optim.NelderMead as NM

let f xs = sum [x*x | x <- xs]                    -- sphere
r <- NM.runNelderMead f [3, -2, 1]
-- orValue ~ 0, orBest ~ [0, 0, 0]
```

Tuning:

```haskell
let cfg = NM.defaultNMConfig
            { NM.nmStop = OC.defaultStopCriteria { OC.stMaxIter = 5000 }
            , NM.nmInitStep = 1.0    -- initial simplex step
            }
r <- NM.runNelderMeadWith cfg rosenbrock [-1.2, 1.0]
```

## 2. L-BFGS — `Hanalyze.Optim.LBFGS`

Quasi-Newton method (Liu-Nocedal 1989). Two-loop recursion for inverse Hessian × gradient
with history size m=10 (typical). **Gold standard for smooth MLE / GP HP optimisation.**

```haskell
import qualified Hanalyze.Optim.LBFGS as LBFGS

-- Analytic gradient
r <- LBFGS.runLBFGS f gradF x0

-- Numeric (central difference) gradient
r <- LBFGS.runLBFGSNumeric LBFGS.defaultLBFGSConfig f x0
```

`Hanalyze.Model.GP.optimizeGP` uses L-BFGS internally (5–10× faster than the old GradAscent).

## 3. Brent / Golden Section — `Hanalyze.Optim.LineSearch`

1D unimodal optimisation:

- `brent`: parabolic interpolation + golden-section hybrid (super-linear,
  `scipy.optimize.brent` compatible).
- `goldenSection`: golden section (linear, robust).

```haskell
import qualified Hanalyze.Optim.LineSearch as LS

let r = LS.brent LS.defaultBrentConfig (\[x] -> (x - 2.5)^2 + 1) 0 5
-- orBest = [2.5], orValue = 1.0
```

`Hanalyze.Model.Kernel.autoBandwidthBrent` uses Brent internally (no candidate grid required).

## 4. Differential Evolution — `Hanalyze.Optim.DifferentialEvolution`

DE/rand/1/bin (Storn-Price 1997). Gradient-free, global, simple, and empirically robust.
Good fit for continuous 5–30-dim non-convex problems.

```haskell
import qualified Hanalyze.Optim.DifferentialEvolution as DE
import qualified System.Random.MWC as MWC

gen <- MWC.createSystemRandom
let bounds = replicate 5 (-5.12, 5.12)
let cfg = (DE.defaultDEConfig bounds)
            { DE.deStop = OC.defaultStopCriteria { OC.stMaxIter = 400 }
            , DE.deF    = 0.7    -- mutation factor
            , DE.deCR   = 0.9    -- crossover probability
            }
r <- DE.runDEWith cfg rastrigin gen
```

## 5. CMA-ES — `Hanalyze.Optim.CMAES` / `Hanalyze.Optim.CMAESFull`

Covariance Matrix Adaptation Evolution Strategy (Hansen 2001/2016).

| Module | Variant | Use |
|---|---|---|
| `Hanalyze.Optim.CMAES`     | **simplified diagonal** (rank-μ only, σ uses a 1/5-rule-like multiplier) | lightweight, low–mid dim |
| `Hanalyze.Optim.CMAESFull` | **full-rank** (rank-1 + rank-μ + path cumulation + CSA + h_σ) | standard (Hansen 2016) |

The full-rank version performs eigendecomposition to recover B, D and updates the entire
covariance matrix, so rotation/scale invariance truly holds and non-convex problems are
handled more robustly. Reaches (1,1) within 0.1 on Rosenbrock 2D in 500 iterations;
sphere 5D within 1e-3 in 300 iterations.

```haskell
import qualified Hanalyze.Optim.CMAES as CMAES

gen <- MWC.createSystemRandom
let cfg = CMAES.defaultCMAESConfig { CMAES.cmSigma0 = 1.0 }
r <- CMAES.runCMAESWith cfg sphere [3, -2, 1, 0.5, -1.5] gen
```

---

## 6. Simulated Annealing — `Hanalyze.Optim.SimulatedAnnealing`

Kirkpatrick et al. 1983. Physical analogy (cooling solids): random walk + Metropolis acceptance.
Temperature `T_k = T_0 · α^k`; worse moves accepted with probability `exp(-Δf/T)`,
escaping local minima.

```haskell
import qualified Hanalyze.Optim.SimulatedAnnealing as SA

gen <- MWC.createSystemRandom
let bs = replicate 5 (-3, 3)
    cfg = (SA.defaultSAConfig bs)
            { SA.saInitTemp = 2.0, SA.saAlpha = 0.997 }
r <- SA.runSAWith cfg sphere [2, -1.5, 1, 0.5, -0.7] gen
```

Larger α (closer to 0.99) cools more slowly, yielding stronger local-escape ability.

## 7. Particle Swarm Optimization — `Hanalyze.Optim.ParticleSwarm`

Kennedy & Eberhart 1995. A particle swarm pulled by personal best (pbest) and global best
(gbest), updating velocities:

  v_{t+1} = w · v_t + c_1 r_1 · (pbest - x) + c_2 r_2 · (gbest - x)

```haskell
import qualified Hanalyze.Optim.ParticleSwarm as PSO

gen <- MWC.createSystemRandom
let bs = replicate 3 (-5.12, 5.12)
    cfg = (PSO.defaultPSOConfig bs) { PSO.psoNum = 40 }
r <- PSO.runPSOWith cfg rastrigin gen
```

Sits alongside DE / CMA-ES in the metaheuristic family. Stable on multi-modal problems.

## 8. Constrained optimisation — `Hanalyze.Optim.Constrained`

Handles equality constraints `g_i(x) = 0` and inequality constraints `h_j(x) ≤ 0`:

- **Augmented Lagrangian** (`runAugmentedLagrangian`): Lagrange multipliers + quadratic penalty.
  Outer loop updates multipliers (λ, μ) and penalty ρ; inner loop solves the unconstrained
  subproblem with L-BFGS.
- **Penalty method** (`penaltyMethod`): omits multipliers and only grows the penalty.
  Simple but prone to ill-conditioning.

```haskell
import qualified Hanalyze.Optim.Constrained as Con

let f xs = (head xs)^2 + (xs !! 1)^2
    cs = Con.ConstraintSet
           { Con.csEq   = [\xs -> head xs + xs !! 1 - 1]   -- x1+x2 = 1
           , Con.csIneq = []                                -- no inequality
           }
(r, viol) <- Con.runAugmentedLagrangian Con.defaultConstrainedConfig f cs [0, 0]
-- Expected: x1=x2=0.5, viol < 1e-3
```

## Benchmark

`cabal run single-opt-bench-demo` produces an HTML report comparing convergence histories
of all 5 algorithms × 3 benchmarks (Sphere / Rosenbrock / Rastrigin).

## Integration with RFF HP tuning (`Hanalyze.Model.RFF`)

DE-based variants for RFF hyperparameter auto-tuning:

| Function | Search method |
|---|---|
| `maximizeMarginalLikRBFMV` | Existing: 1280 → 2560-point grid (coarse-to-fine) |
| **`maximizeMarginalLikRBFMV_DE`** | New: 3D log-space DE coarse + 8×6×6 fine grid |
| `gridSearchLOOCVRBFMV` | Existing: 8 ℓ × 20 λ = 160-point grid |
| **`gridSearchLOOCVRBFMV_DE`** | New: 2D log-space DE search |

The DE variants help when grid discretization is a problem (e.g. narrow ℓ regions
containing the optimum). Evaluation cost is comparable to the grid versions.

## Integration with Bayesian Optimization (`Hanalyze.Optim.BayesOpt`)

`Hanalyze.Optim.BayesOpt`'s acquisition maximisation has been swapped to the new optimisers:

| Function | Inner optimiser |
|---|---|
| `bayesOpt` (1D single-objective) | **Brent** + coarse-grid bracket |
| `bayesOptND` (N-dim single-objective) | **L-BFGS multi-start** (nStarts random initial points) |
| `bayesOptScalarMO` (multi-objective, ParEGO-style) | random scalarisation + **L-BFGS multi-start** |
| `bayesOptMOWithNSGA` (multi-objective, full Pareto-front search) | **NSGA-II** (kept; appropriate here) |

GP Cholesky / SVD failures are caught internally with `try (evaluate ...)` and converted
to a penalty (1e30), so the optimiser does not crash when params drift to extreme regions
(e.g. tiny length scales).

## On the CLI

We do not provide a CLI subcommand (`hanalyze optim` etc.) for single-objective
optimisation. Reasons:

- No natural way to pass an objective function as CLI arguments (cannot pass a Haskell function as a string).
- HP-tuning is already exposed per-model via `--auto-hp` flags.
- Benchmarking is covered by the `single-opt-bench-demo` above.

Use the library API directly.

---

## References

- Theory: [theory-singleobj.md](theory-singleobj.md)
- Multi-objective: [02-multi-objective.md](02-multi-objective.md)
- BO details: [theory-bayesopt.md](theory-bayesopt.md)
