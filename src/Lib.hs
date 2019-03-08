module Lib where

import Control.Arrow ((&&&))
import Control.Category hiding ((.),id)
import Data.Function ((&))


-- | = Section 1

newtype Term f = In { out :: f (Term f) }

type Algebra f a = f a -> a

bottomUp, topDown :: Functor a
  => (Term a -> Term a) -> Term a -> Term a
topDown  f = In  <<< fmap (topDown f)  <<< out <<< f
bottomUp f = out >>> fmap (bottomUp f) >>> In >>> f


-- | = Section 2

-- | bottomUp f = cata (In >>> f)
-- Called a "cata"morphism because the input collapses.
cata :: (Functor f) => Algebra f a -> Term f -> a
cata fn =
    out                -- 1) unpack
    >>> fmap (cata fn) -- 2) recurse
    >>> fn             -- 3) apply


-- | = Section 3
-- https://blog.sumtypeofway.com/recursion-schemes-part-iii-folds-in-context/

type RAlgebra f a = f (Term f, a) -> a
-- | Called a "para"morphism because the original Term f is
-- carried alongside the `a` that it was transformed into.
para :: Functor f => RAlgebra f a -> Term f -> a
para f = out >>> fmap (id &&& para f) >>> f
  -- (id &&& para f) t = (t, para rAlg t)

type RAlgebra' f a = Term f -> f a -> a
para' :: Functor f => RAlgebra' f a -> Term f -> a
para' alg t = out t & fmap (para' alg) & alg t

-- | `para'` generalizes `cata`: broken
-- cata' :: Functor f => Algebra f a -> Term f -> a
-- cata' = para' $ const f

