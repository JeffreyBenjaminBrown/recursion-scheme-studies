module MaybeHelpfulElaboration where

import Lib
import Control.Category hiding ((.),id)


-- | (supposedly) easier to understand, but doesn't share values well
histo_inefficient :: Functor f => CVAlgebra f a -> Term f -> a
histo_inefficient h = out >>> fmap worker >>> h where
  worker t = Attr (histo_inefficient h t)
             (fmap worker $ out t)
