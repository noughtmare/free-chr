module CHR.Examples.Generic.FiniteDomain (
  EnumConstraint (..),
  isIn,
  isLeq,
  isEq,
  isNe,
  leq,
  leqAndIn,
  leqAndEq,
  fd,
  eq,
) where

import CHR.Generic.Helpers (
  clean,
  inconsistent',
  propagate,
  remove',
  simplify,
  simplify',
 )
import CHR.Generic.Solver (Solver (..))
import Control.Applicative (liftA2)
import Control.Monad.Except (MonadError)
import Data.Kind (Type)
import Data.List qualified as List

type EnumConstraint :: Type -> Type -> Type
data EnumConstraint s c
  = In s [c]
  | Leq s s
  | Eq s s
  | Ne s s
  deriving stock (Show, Eq)

isIn, isLeq, isEq, isNe :: EnumConstraint s c -> Bool
isIn (In _ _) = True
isIn _ = False
isLeq (Leq _ _) = True
isLeq _ = False
isEq (Eq _ _) = True
isEq _ = False
isNe (Ne _ _) = True
isNe _ = False

leq ::
  (Eq s, Solver solver, MonadError () m) =>
  solver m (EnumConstraint s c)
leq =
  clean
    "leq idempotency"
    [isLeq]
    [isLeq]
    (\[Leq x y, Leq x' y'] -> pure $ x == x' && y == y')
    <.> remove'
      "leq reflexivity"
      [liftA2 (&&) isLeq (\(x `Leq` y) -> x == y)]
    <.> simplify'
      "leq antisymmetry"
      [isLeq, isLeq]
      (\[Leq x y, _] -> [pure [Eq x y]])
    <.> propagate
      "leq transitivity"
      [isLeq, isLeq]
      (\[Leq _ y, Leq y' _] -> pure $ y == y')
      (\[Leq x _, Leq _ z] -> [pure [Leq x z]])

leqAndIn ::
  (Eq s, Ord c, Solver solver, MonadError () m) =>
  solver m (EnumConstraint s c)
leqAndIn =
  propagate
    "leq & in 1"
    [isIn, isIn, isLeq]
    ( \[In x dx, In y dy, Leq x' y'] ->
        pure $
          x == x' && y == y' && maximum dx > maximum dy
    )
    ( \[In x dx, In _ dy, Leq _ _] ->
        [pure [x `In` filter (< maximum dy) dx]]
    )
    <.> propagate
      "leq & in 2"
      [isIn, isIn, isLeq]
      ( \[In x dx, In y dy, Leq x' y'] ->
          pure $
            x == x' && y == y' && minimum dy > minimum dx
      )
      ( \[In _ dx, In y dy, Leq _ _] ->
          [pure [y `In` filter (> minimum dx) dy]]
      )

eq ::
  (Eq s, Solver solver, Monad m) =>
  solver m (EnumConstraint s c)
eq =
  clean
    "eq idempotency"
    [isEq]
    [isEq]
    (\[Eq x y, Eq x' y'] -> pure $ x == x' && y == y')
    <.> simplify'
      "eq symmetry"
      [isEq]
      (\[Eq x y] -> [pure [Eq y x]])
    <.> remove'
      "eq reflexivity"
      [liftA2 (&&) isEq (\(Eq x x') -> x == x')]
    <.> propagate
      "eq transitivity"
      [isEq, isEq]
      (\[Eq _ y, Eq y' _] -> pure $ y == y')
      (\[Eq x _, Eq _ z] -> [pure [Eq x z]])

leqAndEq ::
  (Eq s, Solver solver, Monad m) =>
  solver m (EnumConstraint s c)
leqAndEq =
  clean
    "eq/leq idempotency"
    [isEq]
    [isLeq]
    (\[Eq x y, Leq x' y'] -> pure $ x == x' && y == y')
    <.> propagate
      "eq/leq transitivity I"
      [isEq, isLeq]
      (\[Eq _ y, Leq y' _] -> pure $ y == y')
      (\[Eq x _, Leq _ z] -> [pure [Leq x z]])
    <.> propagate
      "eq/leq transitivity II"
      [isLeq, isEq]
      (\[Leq _ y, Eq y' _] -> pure $ y == y')
      (\[Leq x _, Eq _ z] -> [pure [Leq x z]])

fd ::
  (Eq s, Eq c, Solver solver, MonadError () m) =>
  solver m (EnumConstraint s c)
fd =
  inconsistent' "inconsistency" [liftA2 (&&) isIn (\(_ `In` d) -> null d)]
    <.> simplify
      "intersection"
      [isIn, isIn]
      (\[x `In` _, y `In` _] -> pure $ x == y)
      (\[x `In` d1, _ `In` d2] -> [pure [x `In` List.intersect d1 d2]])
    <.> rule
      "intersection'"
      [isEq]
      [isIn, isIn]
      ( \[x `Eq` y, x' `In` dx, y' `In` dy] ->
          pure $ x == x' && y == y' && dx /= dy
      )
      ( \[_, x `In` dx, y `In` dy] ->
          [ pure
              [ x `In` List.intersect dx dy
              , y `In` List.intersect dx dy
              ]
          ]
      )
