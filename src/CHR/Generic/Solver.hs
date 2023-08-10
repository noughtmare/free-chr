module CHR.Generic.Solver (Solver (rule, (<.>))) where

import Data.Kind (Constraint, Type)

type Solver :: ((Type -> Type) -> Type -> Type) -> Constraint
class Solver solver where
  rule ::
    (Monad m) =>
    String ->
    [c -> Bool] ->
    [c -> Bool] ->
    ([c] -> m Bool) ->
    ([c] -> [m [c]]) ->
    solver m c

  (<.>) :: (Monad m) => solver m c -> solver m c -> solver m c
