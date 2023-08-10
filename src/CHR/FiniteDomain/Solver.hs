module CHR.FiniteDomain.Solver (FDSolver (..)) where

import CHR.FiniteDomain.Constraints
import Data.Kind

type FDSolver :: ((Type -> Type) -> Type -> Type -> Type) -> Constraint
class FDSolver solver where
  rule ::
    (Monad m, Eq s) =>
    String ->
    [FDConstraint s v -> Bool] ->
    [FDConstraint s v -> Bool] ->
    ([FDConstraint s v] -> m Bool) ->
    ([FDConstraint s v] -> [m [FDConstraint s v]]) ->
    solver m s v

  (<.>) :: (Monad m, Eq s) => solver m s v -> solver m s v -> solver m s v
