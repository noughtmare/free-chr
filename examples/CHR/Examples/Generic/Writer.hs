module CHR.Examples.Generic.Writer (fib') where

import CHR.Generic.Helpers (simplify', wildcard)
import CHR.Generic.Solver (Solver)
import Control.Monad.Writer (MonadWriter (tell), Writer)

fib' :: Solver solver => solver (Writer [Integer]) (Integer, Integer)
fib' =
  simplify'
    "next"
    [wildcard]
    ( \[(a, b)] ->
        [ (tell [a] >> pure [])
        , pure [(b, a + b)]
        ]
    )
