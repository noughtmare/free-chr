module CHR.Examples.FiniteDomain.WaveFunctionCollapse.Generator (
  Point,
  Grid,
  outOfBounds,
  wfc,
  validGrid,
) where

import CHR.Examples.FiniteDomain.EnumConstraints (fd)
import CHR.Examples.FiniteDomain.WaveFunctionCollapse.Tile (Tile (..), allowed)
import CHR.FiniteDomain
import CHR.Helpers (between, (&&.))
import Control.Monad.Random (MonadRandom)
import Data.Kind (Type)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Optics (At (at), (%), (?~), (^.))
import System.Random.Shuffle (shuffleM)

type Point :: Type
type Point = (Int, Int)

type PointConstraint :: Type -> Type
type PointConstraint t = FDConstraint Point t

type Solver :: Type -> Type
type Solver t = DefaultFDSolver Maybe Point t

type State :: Type -> Type
type State t = FDState Point t

type Grid :: Type -> Type
type Grid t = Map Point [t]

validGrid :: Tile t => Grid t -> Bool
validGrid grid =
  all
    ( \(p, d) ->
        length d == 1
          && all (\q -> maybe True (\tn -> allowed d tn) (Map.lookup q grid)) (neighbors p)
    )
    (Map.toList grid)

neighbors :: Point -> [Point]
neighbors (x, y) = [(x + dx, y + dy) | (dx, dy) <- [(-1, 0), (1, 0), (0, -1), (0, 1)]]

outOfBounds :: Solver t
outOfBounds =
  clean
    "out of bounds"
    [isIdentifierBounds]
    [wildcard]
    (\[IdentifierBounds (lx, ly) (ux, uy), c] -> pure $ any (\(x, y) -> not $ between lx ux x && between ly uy y) (identifiers c))

neighborRule :: Tile t => t -> [t] -> Solver t
neighborRule tile nTiles =
  propagate'
    (show tile)
    [isInEnum &&. hasDomain [tile]]
    (\[p `InEnum` _] -> [pure [p' `inEnum` nTiles | p' <- neighbors p]])

neighborRules :: Tile t => Solver t
neighborRules = foldr1 (<.>) [neighborRule t (allowedNeighbors t) | t <- [minBound .. maxBound]]

wfcSolver :: Tile t => Solver t
wfcSolver = (outOfBounds <.> fd <.> neighborRules)

undetermined :: State t -> [PointConstraint t]
undetermined s = map snd $ concat $ catMaybes [Map.toList <$> (s ^. hConstraints % at h) | h <- [2 .. m]]
  where
    m = maximum [k | (k, _) <- Map.toList (s ^. hConstraints)]

wfc :: (MonadRandom m, Tile t) => Int -> Int -> m (Maybe (Map Point [t]))
wfc w h = case run wfcSolver query of
  Nothing -> pure Nothing
  Just s -> do
    sFinal <- wfc' (undetermined s) s
    pure $ (constraintsToGrid . getConstraints) <$> sFinal
  where
    query = IdentifierBounds (0, 0) (w, h) : [p `inEnum` [minBound .. maxBound] | p <- (,) <$> [0 .. w] <*> [0 .. h]]

wfc' :: (MonadRandom m, Tile t) => [PointConstraint t] -> State t -> m (Maybe (State t))
wfc' [] s = pure $ Just s
wfc' (c@(p `InEnum` _) : _) s = do
  ts <- shuffleM (domain c)
  try ts
  where
    try [] = pure Nothing
    try (t : ts) = do
      case run' wfcSolver [p `inEnum` [t]] s of
        Nothing -> try ts
        Just s' -> wfc' (undetermined s') s'
wfc' _ _ = error "All point constraints given to wfc' must be InEnum constraints"

constraintsToGrid :: [PointConstraint t] -> Grid t
constraintsToGrid = foldr save Map.empty
  where
    save c@(InEnum p _) = at p ?~ domain c
    save _ = id
