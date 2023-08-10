{-# LANGUAGE TemplateHaskell #-}

module CHR.FiniteDomain.State (
  FDState,
  nextId,
  hConstraints,
  constraints,
  entropyOf,
  alive,
  history,
  newFDState,
  getConstraints,
  fresh,
  add,
  kill,
  isAlive,
  kills,
  record,
  check,
) where

import CHR.FiniteDomain.Constraints
import Data.Kind (Type)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Optics

type FDState :: Type -> Type -> Type
data FDState s v = FDState
  { _nextId :: Int
  , _hConstraints :: Map Int (Map Int (FDConstraint s v))
  , _constraints :: Map Int (FDConstraint s v)
  , _entropyOf :: Map Int Int
  , _alive :: Set Int
  , _history :: Set (String, [Int])
  }
  deriving stock (Show)

$(makeLenses ''FDState)

newFDState :: FDState s c
newFDState =
  FDState
    { _nextId = 0
    , _hConstraints = Map.empty
    , _constraints = Map.empty
    , _entropyOf = Map.empty
    , _alive = Set.empty
    , _history = Set.empty
    }

getConstraints :: FDState s v -> [FDConstraint s v]
getConstraints s =
  snd $
    unzip $
      (Map.toList . snd =<< Map.toList (_hConstraints s))
        ++ Map.toList (_constraints s)

fresh :: FDState s c -> (Int, FDState s c)
fresh s@FDState {_nextId = i, _alive = as} =
  (i, s {_nextId = i + 1, _alive = Set.insert i as})

add :: (Eq s, Eq v) => Int -> FDConstraint s v -> FDState s v -> FDState s v
add i c s = case entropy c of
  Nothing -> s & constraints % at i ?~ c
  Just h ->
    s
      & hConstraints % at h % non Map.empty % at i ?~ c
      & entropyOf % at i ?~ h

kill :: Int -> FDState s v -> FDState s v
kill i s =
  s
    & alive %~ Set.delete i
    & cs %~ Map.delete i
  where
    cs =
      case s ^. entropyOf % at i of
        Nothing -> castOptic constraints
        Just h -> hConstraints % at h % mapped

isAlive :: Int -> FDState s v -> Bool
isAlive i s = i `elem` _alive s

kills :: [Int] -> FDState s v -> FDState s v
kills is s = foldr kill s is

record :: String -> [Int] -> FDState s v -> FDState s v
record r is s@FDState {_history = h} = s {_history = Set.insert (r, is) h}

check :: String -> [Int] -> FDState s c -> Bool
check r is s = (r, is) `Set.notMember` _history s
