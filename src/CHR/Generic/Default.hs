{-# LANGUAGE TemplateHaskell #-}

module CHR.Generic.Default (
  CHRState,
  nextId,
  constraints,
  alive,
  history,
  newCHRState,
  fresh,
  add,
  isAlive,
  run,
  evaluate,
) where

import CHR.Generic.Solver
import Control.Applicative (liftA2)
import Control.Arrow
import Control.Monad (foldM, join, (>=>))
import Data.Bifunctor (bimap)
import Data.Foldable.Extra (findM)
import Data.Kind
import Data.List (permutations, subsequences)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Optics (At (at), makeLenses, to, view, (%), (%~), (&), (?~), (^.))

type CHRState :: Type -> Type
data CHRState c = CHRState
  { _nextId :: Int
  , _constraints :: Map Int c
  , _alive :: Set Int
  , _history :: Set (String, [Int])
  }
  deriving stock (Show)

$(makeLenses ''CHRState)

newCHRState :: CHRState c
newCHRState =
  CHRState
    { _nextId = 0
    , _constraints = Map.empty
    , _alive = Set.empty
    , _history = Set.empty
    }

type SimpleSolver :: (Type -> Type) -> Type -> Type
newtype SimpleSolver m c = SimpleSolver
  { runSimpleSolver ::
      Int ->
      c ->
      CHRState c ->
      m (Maybe ([m [c]], CHRState c))
  }

instance Monad m => Semigroup (SimpleSolver m c) where
  f <> g = SimpleSolver {runSimpleSolver = solve}
    where
      solve i c s = do
        rf <- runSimpleSolver f i c s
        maybe (runSimpleSolver g i c s) (pure >>> pure) rf

instance Solver SimpleSolver where
  rule name kept removed guard body =
    SimpleSolver
      { runSimpleSolver = solver
      }
    where
      solver i c state = do
        matching <-
          findM
            ( unzip
                >>> bimap (\is -> pure $ check name is state) guard
                >>> uncurry (liftA2 (&&))
            )
            $ match i c (kept <> removed) (Map.toList $ state ^. constraints)
        pure $ do
          m <- matching
          let (is, cs) = unzip m
          let rs = drop (length kept) is
          let state' =
                state
                  & kills rs
                  & (if length rs > 0 then id else record name is)
          pure (body cs, state')

  (<.>) = (<>)

fresh :: CHRState c -> (Int, CHRState c)
fresh =
  id &&& id
    >>> first (^. nextId)
    >>> (\(i, s) -> (i, s & alive %~ Set.insert i))
    >>> second (nextId %~ (+ 1))

add :: Int -> c -> CHRState c -> CHRState c
add i c = constraints % at i ?~ c

kill :: Int -> CHRState c -> CHRState c
kill i =
  alive %~ Set.delete i
    >>> constraints %~ Map.delete i

isAlive :: Int -> CHRState c -> Bool
isAlive i = view (alive % to (elem i))

kills :: [Int] -> CHRState c -> CHRState c
kills is s = foldr kill s is

record :: String -> [Int] -> CHRState c -> CHRState c
record r is = history %~ Set.insert (r, is)

check :: String -> [Int] -> CHRState c -> Bool
check r is = view (history % to (Set.member (r, is))) >>> not

-- TODO Does not yet prioritize matchings with the callee in the removed head.
match :: Int -> a -> [a -> Bool] -> [(Int, a)] -> [[(Int, a)]]
match i _c ps as =
  [ as''
  | as' <- subsequences as
  , length as' == length ps
  , as'' <- permutations as'
  , and (zipWith ($) ps (map snd as''))
  , i `elem` (map fst as'')
  ]

run :: Monad m => SimpleSolver m c -> [c] -> m (CHRState c)
run solver query = run' solver query newCHRState

run' :: Monad m => SimpleSolver m c -> [c] -> CHRState c -> m (CHRState c)
run' _ [] state = pure state
run' solver (c : cs) state = do
  let (i, state') = fresh state
  call solver i c (add i c state') >>= run' solver cs

evaluate :: Monad m => SimpleSolver m c -> [c] -> m [c]
evaluate solver =
  run solver
    >=> _constraints
    >>> Map.toList
    >>> unzip
    >>> snd
    >>> pure

call :: Monad m => SimpleSolver m c -> Int -> c -> CHRState c -> m (CHRState c)
call solver i c state
  | isAlive i state = do
      mr <- runSimpleSolver solver i c state
      case mr of
        Nothing -> pure state
        Just (mqs, state') -> do
          state'' <-
            foldM
              (\s mq -> join $ run' solver <$> mq <*> pure s)
              state'
              mqs
          (if isAlive i state'' then call solver i c else pure) state''
  | otherwise = pure state
