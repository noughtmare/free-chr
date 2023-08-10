module CHR.Examples.Generic.List (Coin (..), toss) where

import CHR.Generic.Helpers (simplify')
import CHR.Generic.Solver (Solver)
import Data.Kind (Type)

type Coin :: Type
data Coin = Unknown | Heads | Tails deriving stock (Show, Eq)

toss :: Solver solver => solver [] Coin
toss =
  simplify'
    "toss"
    [(== Unknown)]
    (const [[[Heads], [Tails]]])
