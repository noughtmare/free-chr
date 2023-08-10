module CHR.FiniteDomain.Default (
  DefaultFDSolver (..),
  run,
  run',
  match,
  evaluate,
) where

import CHR.FiniteDomain.Constraints
import CHR.FiniteDomain.Solver
import CHR.FiniteDomain.State
import Control.Arrow (Arrow (second), (>>>))
import Control.Monad (foldM, join, (>=>))
import Data.Bifunctor (Bifunctor (bimap))
import Data.Foldable.Extra (findM)
import Data.Function ((&))
import Data.Kind (Type)
import Data.List (findIndices)
import Data.Map qualified as Map
import Optics.Operators ((^.))
import Control.Applicative (liftA2)

type DefaultFDSolver :: (Type -> Type) -> Type -> Type -> Type
newtype DefaultFDSolver m s v = FDSolver
  { runFDSolver ::
      Int ->
      FDConstraint s v ->
      FDState s v ->
      m (Maybe ([m [FDConstraint s v]], FDState s v))
  }

instance Monad m => Semigroup (DefaultFDSolver m s v) where
  f <> g = FDSolver {runFDSolver = solve}
    where
      solve i c s = do
        rf <- runFDSolver f i c s
        maybe (runFDSolver g i c s) (pure >>> pure) rf

instance FDSolver DefaultFDSolver where
  rule name kept removed guard body =
    FDSolver
      { runFDSolver = solver
      }
    where
      solver i c s = do
        matching <-
          findM
            ( unzip
                >>> bimap (\is -> pure $ check name is s) guard
                >>> uncurry (liftA2 (&&))
            )
            $ match
              i
              c
              kept
              removed
              (concat $ map Map.toList $ map snd $ Map.toList $ s ^. hConstraints)
              (Map.toList $ s ^. constraints)
        pure $ do
          m <- matching
          let (is, cs) = unzip m
          -- traceM $ name <> " " <> show is
          -- traceM $ show (_hConstraints s, _constraints s)
          -- traceM $ show $ _history s
          let rs = drop (length kept) is
          let s' =
                s
                  & kills rs
                  & (if length rs > 0 then id else record name is)
          pure (body cs, s')

  (<.>) = (<>)

selects :: (a -> Bool) -> [a] -> [(a, [a])]
selects _ [] = []
selects p (x : xs) =
  (if p x then ((x, xs) :) else id) $
    map (second (x :)) (selects p xs)

match ::
  Int ->
  FDConstraint s v ->
  [FDConstraint s v -> Bool] ->
  [FDConstraint s v -> Bool] ->
  [(Int, FDConstraint s v)] ->
  [(Int, FDConstraint s v)] ->
  [[(Int, FDConstraint s v)]]
match i c ks rs hcs cs = srs <> sks
  where
    (xs, ys) = (findIndices ($ c) ks, findIndices ($ c) rs)

    sks =
      [ ksMatched <> rsMatched
      | x <- xs
      , ((hcs', cs'), ksMatched) <- m ks (reverse hcs) cs x
      , (_, rsMatched) <- m rs (reverse hcs') cs' (-1)
      ]
    srs =
      [ ksMatched <> rsMatched
      | y <- ys
      , (_, rsMatched) <- m rs hcs cs y
      , (_, ksMatched) <- m ks (reverse hcs) cs (-1)
      ]

    m [] _ _ _ = [((hcs, cs), [])]
    m (h : hs) _ _ idx
      | idx == 0 = second ((i, c) :) <$> m hs hcs cs (idx - 1)
      | otherwise =
          let
            checkIt = bimap (/= i) h >>> uncurry (&&)
            (ss, ts) = (selects checkIt hcs, selects checkIt cs)
           in
            [(rests, p : ps) | (p, cs') <- ts, (rests, ps) <- m hs hcs cs' (idx - 1)]
              <> [(rests, p : ps) | (p, hcs') <- ss, (rests, ps) <- m hs hcs' cs (idx - 1)]

run :: (Monad m, Eq s, Eq v) => DefaultFDSolver m s v -> [FDConstraint s v] -> m (FDState s v)
run solver query = run' solver query newFDState

run' ::
  (Monad m, Eq s, Eq v) =>
  DefaultFDSolver m s v ->
  [FDConstraint s v] ->
  FDState s v ->
  m (FDState s v)
run' _ [] state = pure state
run' solver (c : cs) state = do
  let (i, state') = fresh state
  call solver i c (add i c state') >>= run' solver cs

call ::
  (Monad m, Eq s, Eq v) =>
  DefaultFDSolver m s v ->
  Int ->
  FDConstraint s v ->
  FDState s v ->
  m (FDState s v)
call solver i c state
  | isAlive i state = do
      mr <- runFDSolver solver i c state
      case mr of
        Nothing -> pure state
        Just (mqs, state') -> do
          state'' <-
            foldM
              (\s mq -> join $ run' solver <$> mq <*> pure s)
              state'
              mqs
          call solver i c state''
  | otherwise = pure state

evaluate ::
  (Monad m, Eq s, Eq v) =>
  DefaultFDSolver m s v ->
  [FDConstraint s v] ->
  m [FDConstraint s v]
evaluate solver =
  run solver
    >=> getConstraints
    >>> pure