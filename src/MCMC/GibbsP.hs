-- | このモジュールは削除されました。'MCMC.Gibbs' を使用してください。
{-# OPTIONS_GHC -Wno-deprecations -Wno-missing-export-lists #-}
module MCMC.GibbsP {-# DEPRECATED "MCMC.GibbsP は MCMC.Gibbs に統合されました。gibbsMHP → gibbsMH, gibbsFromModelP → gibbsFromModel" #-}
  ( gibbsMHP
  , gibbsMHPChains
  , gibbsFromModelP
  ) where

gibbsMHP :: a
gibbsMHP = error "gibbsMHP is removed; use MCMC.Gibbs.gibbsMH"
{-# DEPRECATED gibbsMHP "Use MCMC.Gibbs.gibbsMH instead." #-}

gibbsMHPChains :: a
gibbsMHPChains = error "gibbsMHPChains is removed; use MCMC.Gibbs.gibbsMHChains"
{-# DEPRECATED gibbsMHPChains "Use MCMC.Gibbs.gibbsMHChains instead." #-}

gibbsFromModelP :: a
gibbsFromModelP = error "gibbsFromModelP is removed; use MCMC.Gibbs.gibbsFromModel"
{-# DEPRECATED gibbsFromModelP "Use MCMC.Gibbs.gibbsFromModel instead." #-}
