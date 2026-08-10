{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
-- 全 Spec ファイル共通の helper / orphan Arbitrary instance。
module SpecHelper where

import qualified Data.Vector as V
import qualified Data.Text   as T
import qualified Numeric.AD.Mode.Reverse.Double as RevD
import qualified Numeric.LinearAlgebra as LA
-- (DataFrame の直接 import は未使用のため削除 = upstream decomp PR#2 移植の副産物調査で判明)
import qualified Hanalyze.Design.Orthogonal as OA
import qualified Hanalyze.Design.Quality    as Quality
import qualified Hanalyze.Design.Taguchi as TG
import qualified Hanalyze.Model.LM             as LM
import qualified Hanalyze.Model.LM.Diagnostics as LMD
import qualified Hanalyze.DataIO.Preprocess as Pp
import qualified Hanalyze.DataIO.Log        as Log
import qualified Hanalyze.DataIO.CSV        as CSV
import qualified Hanalyze.DataIO.Convert    as Conv
import qualified Hanalyze.DataIO.Health     as Health
import qualified Hanalyze.DataIO.Clean      as Clean
import qualified Hanalyze.DataIO.Convert    as Conv2
import qualified Hanalyze.Stat.Standardize  as Std
import qualified Hanalyze.Stat.NumberFormat as NF
import qualified Hanalyze.Stat.Interpolate  as Interp
import qualified Hanalyze.Stat.AdaptiveGrid as AG
import qualified Hanalyze.Stat.KernelDist   as KD
import qualified Hanalyze.Stat.Cholesky     as Chol
import qualified Hanalyze.Stat.QuasiRandom  as QR
import qualified Hanalyze.Stat.Test         as ST
import qualified Hanalyze.Model.HierarchicalCluster as HC
import qualified Hanalyze.Model.AFT         as AFT
import qualified Hanalyze.Model.RandomForestClassifier as RFC
import qualified Hanalyze.Model.RandomForest           as RF
import qualified Hanalyze.Model.FitYByX     as FXY
import qualified Hanalyze.Model.Weibull     as WB
import qualified Data.Vector.Unboxed        as VU
import qualified Hanalyze.Stat.ClassMetrics as CM
import qualified Hanalyze.Stat.CV           as CV
import qualified Hanalyze.Stat.MultipleTesting as MT
import qualified Hanalyze.Stat.Bootstrap       as Boot
import qualified Hanalyze.Stat.Effect          as Eff
import qualified Hanalyze.Stat.Interpret       as Interp
import qualified Hanalyze.Stat.SPC             as SPC
import qualified Hanalyze.Model.Weibull        as Wei
import qualified Hanalyze.Model.Reliability    as Rel
import qualified Hanalyze.Optim.NSGA           as NSGAP3
import qualified Hanalyze.Stat.GroupComparison as GC
import qualified Hanalyze.Design.Optimal       as OPT
import qualified Hanalyze.Design.Diagnostics   as DDiag
import qualified Hanalyze.Design.Constraint    as DCons
import qualified Hanalyze.Design.Custom.Factor     as CF
import qualified Hanalyze.Design.Custom.Model      as CM
import qualified Hanalyze.Design.Custom.Constraint as CC
import qualified Hanalyze.Design.Custom.RegionMoment as RM
import qualified Hanalyze.Design.Custom.Coordinate as CX
import qualified Hanalyze.Design.Custom.Compare    as CCMP
import qualified Hanalyze.Design.Custom.Power      as CPW
import qualified Hanalyze.Design.Custom.Augment    as CAUG
import qualified Hanalyze.Design.Custom.SplitPlot  as CSP
import qualified Hanalyze.Design.Custom.Bayesian   as CB
import qualified Data.Vector.Storable              as VS
import qualified Data.Map.Strict as M
import qualified Data.Set        as Set
import qualified Hanalyze.Model.StateSpace     as SS
import qualified Hanalyze.Model.NeuralNetwork  as NN
import qualified Hanalyze.Model.GradientBoosting as GB
import qualified Hanalyze.Stat.MDS             as MDS
import qualified Hanalyze.Model.KNN            as KNN
import qualified Hanalyze.Model.NaiveBayes     as NB
import qualified Hanalyze.Model.GARCH          as GARCH
import qualified Hanalyze.Model.VAR            as VAR
import qualified Hanalyze.Model.CompetingRisks as CR
import qualified Hanalyze.Model.ReliabilityBlockDiagram as RBD
import qualified System.Random.MWC.Distributions as MWCD
import qualified Hanalyze.Design.SpaceFilling  as SF
import qualified Hanalyze.Design.DSD           as DSD
import qualified Hanalyze.Design.Mixture       as Mix
import qualified Hanalyze.Design.Sequential    as Seq
import qualified Hanalyze.Design.RSM           as RSMd
import qualified Hanalyze.Model.PCA         as PCA
import qualified Hanalyze.Model.Cluster     as Cl
import qualified Hanalyze.Model.DecisionTree as DT
import qualified Hanalyze.Model.TimeSeries   as TS
import qualified Hanalyze.Model.Survival     as Surv
import qualified Hanalyze.DataIO.Reshape    as Reshape
import qualified Hanalyze.Optim.NSGA        as NSGA
import qualified System.Random.MWC as MWC
import qualified Hanalyze.Model.KernelRegression      as Kn
import qualified Hanalyze.Model.GP          as GP
import qualified Hanalyze.Model.GPRobust    as GPR
import qualified Hanalyze.Viz.ReportBuilder as RB
import qualified Hanalyze.Viz.ModelGraph    as VMG
import qualified Hanalyze.Viz.ModelGraphDot as VMGD
import qualified Data.ByteString   as BS
import qualified Hanalyze.Model.GP        as GP
import qualified Hanalyze.Model.GPRobust  as GPR
import qualified Hanalyze.Model.RFF       as RFF
import qualified Hanalyze.Model.Regularized as Reg
import qualified Hanalyze.Model.Spline      as Sp
import qualified Hanalyze.Model.KernelRegression      as K
import qualified Hanalyze.Model.Core        as Core
import qualified Hanalyze.Model.GLM         as GLM
import qualified Hanalyze.Optim.NelderMead  as NM
import qualified Hanalyze.Optim.LBFGS       as LBFGS
import qualified Hanalyze.Optim.LineSearch  as LS
import qualified Hanalyze.Optim.DifferentialEvolution as DE
import qualified Hanalyze.Optim.CMAES       as CMAES
import qualified Hanalyze.Optim.CMAESFull   as CMAESF
import qualified Hanalyze.Optim.SimulatedAnnealing as SA
import qualified Hanalyze.Optim.ParticleSwarm as PSO
import qualified Hanalyze.Optim.Constrained as Con
import qualified Hanalyze.Optim.BayesOpt    as BO
import qualified Hanalyze.Optim.Common      as OC
import qualified System.Random.MWC as MWC
import qualified Hanalyze.MCMC.NUTS as NUTS
import qualified Hanalyze.MCMC.SMC  as SMC
import qualified Hanalyze.MCMC.MH    as MH
import qualified Hanalyze.MCMC.Slice as Slice
import qualified Hanalyze.MCMC.HMC   as HMC
import qualified Hanalyze.MCMC.Gibbs as Gibbs
import qualified Hanalyze.Stat.BridgeSampling as BS
import qualified Hanalyze.Stat.BayesFactor    as BF
import qualified Hanalyze.Stat.BayesianModelAveraging as BMA
import qualified Hanalyze.Stat.Causal.PropensityScore as PS
import qualified Hanalyze.Stat.Causal.IPW             as CIPW
import qualified Hanalyze.Stat.Causal.DoublyRobust    as CDR
import qualified Hanalyze.Stat.Causal.CATE            as CCATE
import qualified Hanalyze.Model.RegularizedAdvanced   as RegA
import qualified Hanalyze.Model.Robust                as Rob
import qualified Hanalyze.Stat.CorrelationNetwork     as CN
import qualified Hanalyze.Model.LatentClassAnalysis   as LCA
import qualified Hanalyze.Model.FDA                   as FDA
import qualified Hanalyze.Model.LiNGAM.Direct         as LNG
import qualified Hanalyze.Model.LiNGAM.Pairwise       as LNGP
import qualified Hanalyze.Model.LiNGAM.MultiGroup     as LNGM
import qualified Hanalyze.Model.LiNGAM.VAR            as LNGV
import qualified Hanalyze.Model.LiNGAM.Parce          as LNGPa
import qualified Hanalyze.Model.LiNGAM.Bootstrap      as LNGB
import qualified Hanalyze.Model.LiNGAM.ICA            as LNGI
import qualified Hanalyze.Math.Hungarian              as Hung
import qualified Hanalyze.Math.HSIC                   as HSIC
import qualified Hanalyze.Model.DAG                   as DAG
import qualified Hanalyze.MCMC.Core as Core
import qualified Hanalyze.MCMC.BayesianTest as BAB
import qualified Hanalyze.Model.PLS         as PLS
import qualified Hanalyze.Model.Discriminant as LDA
import qualified Hanalyze.Design.GaugeRR    as GRR
import qualified Hanalyze.Model.HBM as HBM
import qualified Hanalyze.Model.HBM.Interp as HI
import qualified Hanalyze.Stat.VI as VI
import qualified Data.Map.Strict    as M
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Hanalyze.Model.Formula
import Hanalyze.Model.Formula.Frame
import Hanalyze.Model.Formula.Design
import Hanalyze.Model.Formula.RFormula
import Hanalyze.Model.Formula.Nonlinear
import Hanalyze.Model.Formula.Mixed
import Hanalyze.Model.GLMM
import Hanalyze.Model.GLM (Family (..), LinkFn (..))
import Hanalyze.Stat.Distribution (Transform)
import Data.List (sort, nub)
import Control.Monad (forM, forM_)
import System.IO.Temp (withSystemTempFile)
import System.IO     (hPutStr, hClose)
import           Hanalyze.Model.HBM.Ast (Expr (..), Lit (..), DoStmt (..), Err)
import           Data.IORef         (newIORef, readIORef, modifyIORef')

isLeftE :: Either a b -> Bool
isLeftE = either (const True) (const False)

isRightE :: Either a b -> Bool
isRightE = either (const False) (const True)

genName :: Gen T.Text
genName = elements ["a", "b", "c", "x", "y", "z", "g", "t", "b0", "b1", "bg"]

genTerm :: Int -> Gen Term
genTerm n
  | n <= 1 = oneof
      [ Lit . fromIntegral <$> (choose (0, 9) :: Gen Int)
      , Ref <$> genName
      ]
  | otherwise = oneof
      [ Lit . fromIntegral <$> (choose (0, 9) :: Gen Int)
      , Ref <$> genName
      , App <$> genName <*> (choose (1, 2) >>= \k -> vectorOf k (genTerm (n `div` 2)))
      , Index <$> genTerm (n `div` 2) <*> genTerm (n `div` 2)
      , Neg <$> genTerm (n - 1)
      , Bin <$> elements [Add, Sub, Mul, Div, Pow]
            <*> genTerm (n `div` 2) <*> genTerm (n `div` 2)
      ]

instance Arbitrary Formula where
  arbitrary = do
    nd   <- choose (0, 2) :: Gen Int
    vars <- vectorOf (nd + 1) genName
    rhs  <- sized genTerm
    pure (Formula (head vars) (tail vars) rhs)
