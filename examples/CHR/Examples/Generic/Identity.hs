module CHR.Examples.Generic.Identity (gcd', nub') where

import CHR.Generic.Helpers (clean, remove', wildcard)
import CHR.Generic.Solver (Solver (..))
import Data.Functor.Identity ( Identity )

gcd' :: Solver solver => solver Identity Int
gcd' =
  remove' "zero" [(<= 0)]
    <.> rule
      "subtract"
      [(> 0)]
      [(> 0)]
      (\[n, m] -> pure $ n <= m)
      (\[n, m] -> [pure $ [m - n]])

nub' :: (Solver solver, Eq a) => solver Identity a
nub' =
  clean
    "remove duplicate"
    [wildcard]
    [wildcard]
    (\[x, y] -> pure $ x == y)
