-- | このモジュールは削除されました。'MCMC.NUTS' を使用してください。
{-# OPTIONS_GHC -Wno-deprecations -Wno-missing-export-lists #-}
module MCMC.NUTSP {-# DEPRECATED "MCMC.NUTSP は MCMC.NUTS に統合されました。nutsP → nuts, nutsPChains → nutsChains" #-}
  ( nutsP
  , nutsPChains
  ) where

nutsP :: a
nutsP = error "nutsP is removed; use MCMC.NUTS.nuts"
{-# DEPRECATED nutsP "Use MCMC.NUTS.nuts instead." #-}

nutsPChains :: a
nutsPChains = error "nutsPChains is removed; use MCMC.NUTS.nutsChains"
{-# DEPRECATED nutsPChains "Use MCMC.NUTS.nutsChains instead." #-}
