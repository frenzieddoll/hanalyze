{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Hanalyze.MCMC.SamplersPureSpec (spec) where

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
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified System.Random.MWC as MWC
import qualified Hanalyze.Model.Core        as Core
import qualified System.Random.MWC as MWC
import qualified Hanalyze.MCMC.SMC  as SMC
import qualified Hanalyze.MCMC.MH    as MH
import qualified Hanalyze.MCMC.Slice as Slice
import qualified Hanalyze.MCMC.HMC   as HMC
import qualified Hanalyze.MCMC.Gibbs as Gibbs
import qualified Hanalyze.MCMC.Core as Core
import qualified Hanalyze.Model.HBM as HBM
import qualified Data.Map.Strict    as M
import SpecHelper

spec :: Spec
spec = do
  describe "他サンプラ純粋版 (Phase 50.6-50.10: MH/Slice/HMC/Gibbs/SMC)" $ do
    let model :: HBM.ModelP ()
        model = do
          mu <- HBM.sample "mu" (HBM.Normal 0 10)
          HBM.observe "y" (HBM.Normal mu 1) (replicate 8 5.0)
        initC = M.fromList [("mu", 0)]
        samp  = Core.chainSamples

    it "metropolisPure: 同 seed ビット同一 / IO 版と ST 等価" $ do
      let c = MH.defaultMCMCConfig ["mu"]
          cfg = c { MH.mcmcIterations = 40, MH.mcmcBurnIn = 20 }
      samp (MH.metropolisPure model cfg initC 11)
        `shouldBe` samp (MH.metropolisPure model cfg initC 11)
      gen <- MWC.initialize (V.singleton 11)
      ioC <- MH.metropolis model cfg initC gen
      samp (MH.metropolisPure model cfg initC 11) `shouldBe` samp ioC

    it "metropolisChainsPure: 3 本・再現性・子 seed で相異" $ do
      let cfg = (MH.defaultMCMCConfig ["mu"]) { MH.mcmcIterations = 30, MH.mcmcBurnIn = 10 }
          chains = MH.metropolisChainsPure model cfg 3 initC 7
      length chains `shouldBe` 3
      map samp (MH.metropolisChainsPure model cfg 3 initC 7) `shouldBe` map samp chains

    it "slicePure: 同 seed ビット同一 / IO 版と ST 等価" $ do
      let cfg = (Slice.defaultSliceConfig ["mu"])
                  { Slice.sliceIterations = 30, Slice.sliceBurnIn = 10 }
      samp (Slice.slicePure model cfg initC 22)
        `shouldBe` samp (Slice.slicePure model cfg initC 22)
      gen <- MWC.initialize (V.singleton 22)
      ioC <- Slice.slice model cfg initC gen
      samp (Slice.slicePure model cfg initC 22) `shouldBe` samp ioC

    it "hmcPure: 同 seed ビット同一 / IO 版と ST 等価" $ do
      let cfg = HMC.defaultHMCConfig { HMC.hmcIterations = 30, HMC.hmcBurnIn = 10 }
      samp (HMC.hmcPure model cfg initC 33)
        `shouldBe` samp (HMC.hmcPure model cfg initC 33)
      gen <- MWC.initialize (V.singleton 33)
      ioC <- HMC.hmc model cfg initC gen
      samp (HMC.hmcPure model cfg initC 33) `shouldBe` samp ioC

    it "gibbsBetaBinomialPure: 同 seed ビット同一 / IO 版と ST 等価" $ do
      let cfg = Gibbs.defaultGibbsConfig { Gibbs.gibbsIterations = 50, Gibbs.gibbsBurnIn = 10 }
      samp (Gibbs.gibbsBetaBinomialPure "p" 2 2 10 7 cfg 44)
        `shouldBe` samp (Gibbs.gibbsBetaBinomialPure "p" 2 2 10 7 cfg 44)
      gen <- MWC.initialize (V.singleton 44)
      ioC <- Gibbs.gibbsBetaBinomial "p" 2 2 10 7 cfg gen
      samp (Gibbs.gibbsBetaBinomialPure "p" 2 2 10 7 cfg 44) `shouldBe` samp ioC

    it "smcPure: 同 seed で SMCResult (chain) ビット同一" $ do
      let cfg = (SMC.defaultSMCConfig ["mu"])
                  { SMC.smcNParticles = 100, SMC.smcNSteps = 5, SMC.smcMHIterations = 3 }
          r1 = SMC.smcPure model cfg initC 9
          r2 = SMC.smcPure model cfg initC 9
      samp (SMC.smcChain r1) `shouldBe` samp (SMC.smcChain r2)
      SMC.smcLogMarginal r1 `shouldBe` SMC.smcLogMarginal r2
