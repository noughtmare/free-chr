module CHR.Generic.Helpers (
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
  simplify,
  simplify',
  remove,
  remove',
) where

import CHR.Generic.Solver (Solver (rule))
import Control.Monad.Except (MonadError (throwError))
import Data.Kind (Type)

type Head :: Type -> Type
type Head c = [c -> Bool]

type Guard :: (Type -> Type) -> Type -> Type
type Guard m c = [c] -> m Bool

type Body :: (Type -> Type) -> Type -> Type
type Body m c = [c] -> [m [c]]

true :: Monad m => Guard m c
true = const $ pure True

top :: Body m c
top = const []

failWith :: MonadError e m => e -> Body m c
failWith e = const [throwError e]

bottom :: MonadError () m => Body m c
bottom = failWith ()

wildcard :: a -> Bool
wildcard = const True

rule' ::
  (Solver solver, Monad m) =>
  String ->
  Head c ->
  Head c ->
  Body m c ->
  solver m c
rule' n k r b = rule n k r true b

clean ::
  (Solver solver, Monad m) =>
  String ->
  Head c ->
  Head c ->
  Guard m c ->
  solver m c
clean n k r g = rule n k r g top

clean' ::
  (Solver solver, Monad m) =>
  String ->
  Head c ->
  Head c ->
  solver m c
clean' n k r = rule n k r true top

inconsistent ::
  (Solver solver, MonadError () m) =>
  String ->
  Head c ->
  Guard m c ->
  solver m c
inconsistent n h g = simplify n h g bottom

inconsistent' ::
  (Solver solver, MonadError () m) =>
  String ->
  Head c ->
  solver m c
inconsistent' n h = simplify n h true bottom

propagate ::
  (Solver solver, Monad m) =>
  String ->
  Head c ->
  Guard m c ->
  Body m c ->
  solver m c
propagate n h g b = rule n h [] g b

propagate' ::
  (Solver solver, Monad m) =>
  String ->
  Head c ->
  Body m c ->
  solver m c
propagate' n h b = rule' n h [] b

simplify ::
  (Solver solver, Monad m) =>
  String ->
  Head c ->
  Guard m c ->
  Body m c ->
  solver m c
simplify n h g b = rule n [] h g b

simplify' ::
  (Solver solver, Monad m) =>
  String ->
  Head c ->
  Body m c ->
  solver m c
simplify' n h b = rule' n [] h b

remove ::
  (Solver solver, Monad m) =>
  String ->
  Head c ->
  Guard m c ->
  solver m c
remove n h g = simplify n h g top

remove' ::
  (Solver solver, Monad m) =>
  String ->
  Head c ->
  solver m c
remove' n h = simplify' n h top
