-- | このモジュールは削除されました。'MCMC.HMC' を使用してください。
{-# OPTIONS_GHC -Wno-deprecations -Wno-missing-export-lists #-}
module MCMC.HMCP {-# DEPRECATED "MCMC.HMCP は MCMC.HMC に統合されました。hmcP → hmc, hmcPChains → hmcChains" #-}
  ( hmcP
  , hmcPChains
  ) where

import MCMC.HMC (hmc, hmcChains)

hmcP :: a
hmcP = error "hmcP is removed; use MCMC.HMC.hmc"
{-# DEPRECATED hmcP "Use MCMC.HMC.hmc instead." #-}

hmcPChains :: a
hmcPChains = error "hmcPChains is removed; use MCMC.HMC.hmcChains"
{-# DEPRECATED hmcPChains "Use MCMC.HMC.hmcChains instead." #-}
