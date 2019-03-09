{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Control.Arrow ((&&&))
import Control.Category hiding ((.),id)
import Data.Function ((&))


-- | = Section 1

-- | Think `Term f` = "recursive f"
newtype Term f = In { out :: f (Term f) }

bottomUp, topDown :: Functor a
  => (Term a -> Term a) -> Term a -> Term a
topDown  f = In  <<< fmap (topDown f)  <<< out <<< f
bottomUp f = out >>> fmap (bottomUp f) >>> In >>> f


-- | = Section 2

type Algebra f a = f a -> a -- ^ "Cata" ~ "collapse"
cata :: (Functor f) => Algebra f a -> Term f -> a  -- ^ a bottom-up fold
cata alg = out >>> fmap (cata alg) >>> alg

type Coalgebra f a = a -> f a
ana :: (Functor f) => Coalgebra f a -> a -> Term f -- ^ a top-down unfold
ana c = In <<< fmap (ana c) <<< c


-- | = Section 3
-- https://blog.sumtypeofway.com/recursion-schemes-part-iii-folds-in-context/

-- | The `Term` is intended to record the origin of the paired `a`.
type RAlgebra f a = f (Term f, a) -> a
-- | `para` is like `cata`, but the original Term f is carried alongside
-- ("in parallel" to) the `a` that it was transformed into.
para :: Functor f => RAlgebra f a -> Term f -> a
para alg = out >>> fmap (id &&& para alg) >>> alg
  -- ^ (id &&& para f) = \t -> (t, para rAlg t)

-- | `Jbb.Abc.forPara` and `forPara'` shows how `RAlgebra` ~ `RAlgebra'`
type RAlgebra' f a = Term f -> f a -> a
para' :: Functor f => RAlgebra' f a -> Term f -> a
para' alg t = out t & fmap (para' alg) & alg t

-- | `RCoalgebra` is dual to `RAlgebra`: reverse arrows, swap (*) for (+).
type RCoalgebra f a = a -> f (Either (Term f) a)
-- | `apo`, dual to `para`, is an Either-ish unfolding.
apo :: Functor f => RCoalgebra f a -> a -> Term f
apo r = In <<< fmap (either id $ apo r) <<< r
  -- ^ either = |||, so we could instead write
  -- `apo' f = In <<< fmap (id ||| apo' f) <<< f`


-- | = Section 4: histomorphisms, futumorphisms
-- "CV" = "course-of-value"

-- | The `hole` is intended to record the entire history
-- that gave rise to the `attribute`. (Compare to `RAlgebra`.)
-- `Attr` ~ the Free monad, and `CoAttr` ~ the CoFree one.
data Attr f a = Attr { attribute :: a
                     , hole      :: f (Attr f a) }
type CVAlgebra f a = f (Attr f a) -> a
histo :: forall a f. Functor f => CVAlgebra f a -> Term f -> a
histo h = worker >>> attribute where
  worker :: Term f -> Attr f a
  worker = out >>> fmap worker >>> h &&& id >>> uncurry Attr

data CoAttr f a = Automatic a
                | Manual (f (CoAttr f a))
type CVCoalgebra f a = a -> f (CoAttr f a)
futu :: forall a f. Functor f => CVCoalgebra f a -> a -> Term f
futu c = In <<< fmap worker <<< c where
  worker :: CoAttr f a -> Term f
  worker (Automatic a) = futu c a
  worker (Manual g)    = In $ fmap worker g
