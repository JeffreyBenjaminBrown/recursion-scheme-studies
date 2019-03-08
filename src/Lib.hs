module Lib where

import Control.Arrow ((&&&),(|||))
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

-- | bottomUp f = cata (In >>> f)
-- Called a "cata"morphism because the input collapses.
type Algebra f a = f a -> a
cata :: (Functor f) => Algebra f a -> Term f -> a
cata alg = out >>> fmap (cata alg) >>> alg

-- | gleaned from section 3; was supposed to be in 2
-- ana is the dual of cata
type Coalgebra f a = a -> f a
ana :: (Functor f) => Coalgebra f a -> a -> Term f
ana c = In  <<< fmap (ana c) <<< c


-- | = Section 3
-- https://blog.sumtypeofway.com/recursion-schemes-part-iii-folds-in-context/

type RAlgebra f a = f (Term f, a) -> a
-- | Called a "para"morphism because the original Term f is
-- carried alongside the `a` that it was transformed into.
para :: Functor f => RAlgebra f a -> Term f -> a
para alg = out >>> fmap (id &&& para alg) >>> alg
  -- (id &&& para f) = \t -> (t, para rAlg t)

type RAlgebra' f a = Term f -> f a -> a
para' :: Functor f => RAlgebra' f a -> Term f -> a
para' alg t = out t & fmap (para' alg) & alg t

-- | `para'` generalizes `cata
-- Did not compile until I provided the argument in the definition.
-- I'm guessing it's correct now but I"m waiting for confirmation from PT.
cata' :: Functor f => Algebra f a -> Term f -> a
cata' alg = para' $ const alg

type RCoalgebra f a = a -> f (Either (Term f) a)

apo :: Functor f => RCoalgebra f a -> a -> Term f
apo r = In <<< fmap (either id $ apo r) <<< r
  -- ^ either = |||, so we could instead write
  -- `apo' f = In <<< fmap (id ||| apo' f) <<< f`


-- | = Section 4: histomorphisms, futumorphisms

data Attr f a = Attr { attribute :: a
                     , hole      :: f (Attr f a) }

type CVAlgebra f a = f (Attr f a) -> a -- ^ CV = "course-of-value"

-- | somehow this implementation doesn't share values properly
histo_inefficient :: Functor f => CVAlgebra f a -> Term f -> a
histo_inefficient h = out >>> fmap worker >>> h where
  worker t = Attr (histo_inefficient h t)
             (fmap worker $ out t)

histo :: Functor f
      => CVAlgebra f a -> Term f -> a
histo h = worker >>> attribute where
  worker = out >>> fmap worker >>> h &&& id >>> uncurry Attr
