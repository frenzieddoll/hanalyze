{-# LANGUAGE OverloadedStrings #-}
-- | 階層ベイズモデル (HBM) の自由モナド DSL。
--
-- モデルは do 記法で書き、後から複数のインタープリタで解釈します。
--
-- @
-- import Model.HBM
-- import Stat.Distribution
--
-- myModel :: [Double] -> Model Double
-- myModel ys = do
--   mu    <- sample "mu"    (Normal 0 10)
--   sigma <- sample "sigma" (Exponential 1)
--   observe "y" (Normal mu sigma) ys
--   return mu
-- @
module Model.HBM
  ( -- * モデル型
    Model
  , ModelF (..)
    -- * DSL プリミティブ
    -- | 'sample' と 'observe' を do 記法で組み合わせてモデルを記述します。
  , sample
  , observe
    -- * 構造の検査
  , NodeRole (..)
  , NodeInfo (..)
  , collectNodes
  , describeModel
    -- * 対数密度インタープリタ
    -- | いずれも 'Params' マップ (潜在変数名→値) を受け取ります。
    -- 変数が欠落しているか、台の外にある場合は @-Infinity@ を返します。
  , Params
  , logJoint
  , logPrior
  , logLikelihood
  , sampleNames
    -- * モデルグラフ (可視化用)
  , ModelGraph (..)
  , buildModelGraph
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Stat.Distribution (Distribution, distributionName)
import qualified Stat.Distribution as Dist

-- ---------------------------------------------------------------------------
-- Free monad (self-contained, no extra dependencies)
-- ---------------------------------------------------------------------------

data Free f a = Pure a | Free (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Free x) = Free (fmap (fmap f) x)

instance Functor f => Applicative (Free f) where
  pure = Pure
  Pure f <*> x  = fmap f x
  Free ff <*> x = Free (fmap (<*> x) ff)

instance Functor f => Monad (Free f) where
  return = pure
  Pure a >>= f = f a
  Free x >>= f = Free (fmap (>>= f) x)

liftF :: Functor f => f a -> Free f a
liftF fa = Free (fmap Pure fa)

-- ---------------------------------------------------------------------------
-- DSL functor
-- ---------------------------------------------------------------------------

-- | Primitive operations in the probabilistic programming DSL.
data ModelF next
  = Sample  Text Distribution (Double -> next)
    -- ^ Draw a latent variable from a prior.
  | Observe Text Distribution [Double] next
    -- ^ Condition on observed data (likelihood term).
  deriving Functor

-- | A probabilistic model: a free monad over 'ModelF'.
-- Write models with do-notation; they are interpreted later
-- (logDensity, sampling, etc.).
type Model = Free ModelF

-- ---------------------------------------------------------------------------
-- Smart constructors
-- ---------------------------------------------------------------------------

-- | Declare a latent variable drawn from the given prior.
-- Returns the drawn value so downstream nodes can depend on it.
--
-- @
-- mu    <- sample "mu"    (Normal 0 10)
-- sigma <- sample "sigma" (Exponential 1)
-- @
sample :: Text -> Distribution -> Model Double
sample name dist = liftF (Sample name dist id)

-- | Condition on a list of observed values under the given likelihood.
-- All observations are assumed i.i.d. given the parameters.
--
-- @
-- observe "y" (Normal mu sigma) yData
-- @
observe :: Text -> Distribution -> [Double] -> Model ()
observe name dist xs = liftF (Observe name dist xs ())

-- ---------------------------------------------------------------------------
-- Structural inspection (Phase 1 interpreter)
-- ---------------------------------------------------------------------------

-- | Whether a node is latent (unobserved) or fixed by data.
data NodeRole
  = Latent              -- ^ Unobserved; will be sampled by the inference engine
  | Observed [Double]   -- ^ Fixed to these values; contributes a likelihood term
  deriving (Show, Eq)

-- | Metadata for one node in the model graph.
data NodeInfo = NodeInfo
  { nodeName :: Text
  , nodeDist :: Distribution
  , nodeRole :: NodeRole
  } deriving (Show)

-- | Walk the model AST and collect every node's metadata.
-- Latent nodes are continued with a placeholder value of 0 so the full
-- tree is reachable regardless of the model's branching logic.
collectNodes :: Model a -> [NodeInfo]
collectNodes (Pure _)                      = []
collectNodes (Free (Sample  n d k))        =
  NodeInfo n d Latent : collectNodes (k 0)
collectNodes (Free (Observe n d xs next))  =
  NodeInfo n d (Observed xs) : collectNodes next

-- | Human-readable structural summary of a model (does not run inference).
describeModel :: Model a -> Text
describeModel m = T.unlines (header : map fmtNode (collectNodes m))
  where
    header = "Model nodes:"
    fmtNode (NodeInfo n d Latent) =
      "  [latent]   " <> n <> " ~ " <> distributionName d
    fmtNode (NodeInfo n d (Observed xs)) =
      "  [observed] " <> n <> " ~ " <> distributionName d
      <> "  (n=" <> T.pack (show (length xs)) <> ")"

-- ---------------------------------------------------------------------------
-- Log-density interpreter (Phase 2)
-- ---------------------------------------------------------------------------

-- | Assignment of values to all latent variables in the model.
type Params = Map.Map Text Double

-- | Log joint density: log p(params, data) = log prior + log likelihood.
-- Returns -Infinity if any latent variable is missing from 'Params' or
-- has zero prior density (e.g. outside the distribution's support).
-- Threads actual parameter values through continuations so that
-- downstream distributions correctly depend on upstream samples.
logJoint :: Model a -> Params -> Double
logJoint model params = go model 0.0
  where
    go (Pure _) acc = acc
    go (Free (Sample n d k)) acc =
      case Map.lookup n params of
        Nothing  -> -1/0
        Just val ->
          let lp = Dist.logDensity d val
          in if isNegInf lp then -1/0 else go (k val) (acc + lp)
    go (Free (Observe _ d xs next)) acc =
      let ll = sum (map (Dist.logDensity d) xs)
      in if isNegInf ll then -1/0 else go next (acc + ll)

-- | Log prior only: sum of log p(param | prior) for all latent variables.
logPrior :: Model a -> Params -> Double
logPrior model params = go model 0.0
  where
    go (Pure _) acc = acc
    go (Free (Sample n d k)) acc =
      case Map.lookup n params of
        Nothing  -> -1/0
        Just val ->
          let lp = Dist.logDensity d val
          in if isNegInf lp then -1/0 else go (k val) (acc + lp)
    go (Free (Observe _ _ _ next)) acc = go next acc

-- | Log likelihood only: sum of log p(data | params) for all observed nodes.
logLikelihood :: Model a -> Params -> Double
logLikelihood model params = go model 0.0
  where
    go (Pure _) acc = acc
    go (Free (Sample n _ k)) acc =
      case Map.lookup n params of
        Nothing  -> go (k 0) acc   -- use placeholder; prior not counted
        Just val -> go (k val) acc
    go (Free (Observe _ d xs next)) acc =
      let ll = sum (map (Dist.logDensity d) xs)
      in if isNegInf ll then -1/0 else go next (acc + ll)

-- | Names of all latent variables in declaration order.
-- Used by MCMC initialisation to construct the initial 'Params' map.
sampleNames :: Model a -> [Text]
sampleNames (Pure _)                    = []
sampleNames (Free (Sample n _ k))       = n : sampleNames (k 0)
sampleNames (Free (Observe _ _ _ next)) = sampleNames next

isNegInf :: Double -> Bool
isNegInf x = isInfinite x && x < 0

-- ---------------------------------------------------------------------------
-- Model graph (for visualization)
-- ---------------------------------------------------------------------------

-- | Directed acyclic graph of a probabilistic model.
-- Nodes come from 'collectNodes'; edges are declared explicitly by the user
-- since our DSL uses plain 'Double' values (no symbolic tracking).
data ModelGraph = ModelGraph
  { mgNodes :: [NodeInfo]       -- ^ All variables in declaration order
  , mgEdges :: [(Text, Text)]   -- ^ (parent, child) dependency edges
  } deriving (Show)

-- | Combine the structural info from the model with user-declared edges.
--
-- @
-- buildModelGraph myModel
--   [ ("mu", "theta"), ("sigma", "theta"), ("theta", "y") ]
-- @
buildModelGraph :: Model a -> [(Text, Text)] -> ModelGraph
buildModelGraph m edges = ModelGraph (collectNodes m) edges
