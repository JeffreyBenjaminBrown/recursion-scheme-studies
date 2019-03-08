{-# LANGUAGE DeriveFunctor #-}

module CCC where

import Prelude hiding (lookup)
import Data.List (partition)

import Lib


type Cent = Int

data Nat a
    = Zero
    | Next a
    deriving Functor

zero, one :: Term Nat
zero = In Zero
one  = In (Next zero)

-- Convert from a natural number to its foldable equivalent, and vice versa.
expand :: Int -> Term Nat
expand 0 = In Zero
expand n = In (Next (expand (n - 1)))

compress :: Nat (Attr Nat a) -> Int
compress Zero              = 0
compress (Next (Attr _ x)) = 1 + compress x

coins :: [Cent]
coins = [50, 25, 10, 5, 1]

lookup :: Attr Nat a -> Int -> a
lookup cache 0 = attribute cache
lookup cache n = lookup inner (n - 1) where
  Next inner = hole cache

change :: Cent -> Int
change amt = histo go $ expand amt where
  go :: Nat (Attr Nat Int) -> Int
  go Zero = 1
  go curr@(Next attr) = let
    given               = compress curr -- the amt to make change for
    smallEnoughCoins    = filter (<= given) coins
      -- can't make change for 11 cents with a quarter
    remaining           = map (given -) smallEnoughCoins
    (zeroes, toProcess) = partition (== 0) remaining
    results             = sum (map (lookup attr) toProcess)
    in length zeroes + results
