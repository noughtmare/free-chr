module CHR.Examples.FiniteDomain.WaveFunctionCollapse.Instances (
  LandscapeTile (..),
) where

import CHR.Examples.FiniteDomain.WaveFunctionCollapse.Tile (Tile (..))
import Control.Monad.Random (Uniform)
import Data.Kind (Type)
import GHC.Generics (Generic)

type LandscapeTile :: Type
data LandscapeTile
  = Water
  | Grass
  | Forrest
  | Mountains
  deriving stock (Show, Eq, Ord, Enum, Generic, Bounded)

instance Uniform LandscapeTile

instance Tile LandscapeTile where
  allowedNeighbors t = case t of
    Water -> [Water, Grass]
    Grass -> [Water, Grass, Forrest, Mountains]
    Forrest -> [Grass, Forrest, Mountains]
    Mountains -> [Grass, Forrest, Mountains]