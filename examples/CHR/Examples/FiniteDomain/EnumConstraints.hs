module CHR.Examples.FiniteDomain.EnumConstraints (fd, eq, fd_eq) where

import CHR.FiniteDomain.Constraints (
  FDConstraint (Eq, InEnum),
  isEq,
  isInEnum,
 )
import CHR.FiniteDomain.Helpers (
  clean,
  inconsistent',
  propagate,
  remove,
  top,
 )
import CHR.FiniteDomain.Solver (FDSolver (..))
import Control.Applicative (liftA2)
import Control.Monad.Except (MonadError)
import Data.Set qualified as Set

fd, fd_eq :: (Eq s, Ord c, FDSolver solver, MonadError () m) => solver m s c
fd =
  inconsistent'
    "inconsistency"
    [liftA2 (&&) isInEnum (\(_ `InEnum` d) -> Set.null d)]
    <.> rule
      "subsumption"
      [isInEnum]
      [isInEnum]
      (\[x `InEnum` dx, y `InEnum` dy] -> pure $ x == y && Set.isSubsetOf dx dy)
      top
    <.> rule
      "intersection'"
      []
      [isInEnum, isInEnum]
      ( \[x `InEnum` dx, y `InEnum` dy] ->
          pure $ x == y && dx /= dy
      )
      ( \[x `InEnum` dx, _ `InEnum` dy] ->
          [pure [x `InEnum` Set.intersection dx dy]]
      )
fd_eq =
  rule
    "subsumption'"
    [isEq, isInEnum]
    [isInEnum]
    ( \[x' `Eq` y', x `InEnum` dx, y `InEnum` dy] ->
        pure $ x' == x && y' == y && dx /= dy && Set.isSubsetOf dx dy
    )
    (\[_, _ `InEnum` dx, y `InEnum` _] -> [pure [y `InEnum` dx]])
    <.> rule
      "intersection'"
      [isEq]
      [isInEnum, isInEnum]
      ( \[x' `Eq` y', x `InEnum` dx, y `InEnum` dy] ->
          pure $ x' == x && y' == y && dx /= dy
      )
      ( \[_, x `InEnum` dx, y `InEnum` dy] ->
          [pure $ let d = Set.intersection dx dy in [InEnum x d, InEnum y d]]
      )

eq :: (Eq s, FDSolver solver, MonadError e m) => solver m s c
eq =
  clean
    "idempotency"
    [isEq]
    [isEq]
    (\[x `Eq` y, x' `Eq` y'] -> pure $ x == x' && y == y')
    <.> remove
      "reflexivity"
      [isEq]
      (\[x `Eq` x'] -> pure $ x == x')
    <.> propagate
      "symmetry"
      [isEq]
      (\[x `Eq` y] -> pure $ x /= y)
      (\[x `Eq` y] -> [pure [y `Eq` x]])
    <.> propagate
      "transitivity"
      [isEq, isEq]
      (\[_ `Eq` y, y' `Eq` _] -> pure $ y == y')
      (\[x `Eq` _, _ `Eq` z] -> [pure [x `Eq` z]])