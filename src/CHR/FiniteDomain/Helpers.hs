module CHR.FiniteDomain.Helpers (
  Head,
  Guard,
  Body,
  true,
  top,
  failWith,
  bottom,
  wildcard,
  rule',
  clean,
  clean',
  inconsistent,
  inconsistent',
  propagate,
  propagate',
  simplify',
  remove,
  remove',
) where

import CHR.FiniteDomain.Constraints (FDConstraint)
import CHR.FiniteDomain.Solver (FDSolver (rule))
import Control.Monad.Except (MonadError (throwError))
import Data.Kind (Type)

type Head :: Type -> Type -> Type
type Head s v = [FDConstraint s v -> Bool]

type Guard :: (Type -> Type) -> Type -> Type -> Type
type Guard m s v = [FDConstraint s v] -> m Bool

type Body :: (Type -> Type) -> Type -> Type -> Type
type Body m s v = [FDConstraint s v] -> [m [FDConstraint s v]]

true :: Monad m => Guard m s v
true = const $ pure True

top :: Body m s v
top = const []

failWith :: MonadError e m => e -> Body m s v
failWith e = const [throwError e]

bottom :: MonadError () m => Body m s v
bottom = failWith ()

wildcard :: a -> Bool
wildcard = const True

rule' ::
  (Eq s, FDSolver solver, Monad m) =>
  String ->
  Head s v ->
  Head s v ->
  Body m s v ->
  solver m s v
rule' n k r b = rule n k r true b

clean ::
  (Eq s, FDSolver solver, Monad m) =>
  String ->
  Head s v ->
  Head s v ->
  Guard m s v ->
  solver m s v
clean n k r g = rule n k r g top

clean' ::
  (Eq s, FDSolver solver, Monad m) =>
  String ->
  Head s v ->
  Head s v ->
  solver m s v
clean' n k r = rule n k r true top

inconsistent ::
  (Eq s, FDSolver solver, MonadError () m) =>
  String ->
  Head s v ->
  Guard m s v ->
  solver m s v
inconsistent n h g = simplify n h g bottom

inconsistent' ::
  (Eq s, FDSolver solver, MonadError () m) =>
  String ->
  Head s v ->
  solver m s v
inconsistent' n h = simplify n h true bottom

propagate ::
  (Eq s, FDSolver solver, Monad m) =>
  String ->
  Head s v ->
  Guard m s v ->
  Body m s v ->
  solver m s v
propagate n h g b = rule n h [] g b

propagate' ::
  (Eq s, FDSolver solver, Monad m) =>
  String ->
  Head s v ->
  Body m s v ->
  solver m s v
propagate' n h b = rule' n h [] b

simplify ::
  (Eq s, FDSolver solver, Monad m) =>
  String ->
  Head s v ->
  Guard m s v ->
  Body m s v ->
  solver m s v
simplify n h g b = rule n [] h g b

simplify' ::
  (Eq s, FDSolver solver, Monad m) =>
  String ->
  Head s v ->
  Body m s v ->
  solver m s v
simplify' n h b = rule' n [] h b

remove ::
  (Eq s, FDSolver solver, Monad m) =>
  String ->
  Head s v ->
  Guard m s v ->
  solver m s v
remove n h g = simplify n h g top

remove' ::
  (Eq s, FDSolver solver, Monad m) =>
  String ->
  Head s v ->
  solver m s v
remove' n h = simplify' n h top
