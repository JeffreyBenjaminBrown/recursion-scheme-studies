{-# LANGUAGE ScopedTypeVariables #-}

module Generalizations where

import Control.Category hiding ((.),id)

import Lib


-- | `para'` generalizes `cata
-- This did not compile until I provided the argument in the definition.
-- I'm guessing it's correct now but I"m waiting for confirmation from PT.
cata' :: Functor f => Algebra f a -> Term f -> a
cata' alg = para' $ const alg -- alg :: Algebra, const alg :: R-Algebra

-- | `histo`, like `para`, generalizes `cata`
cata'' :: Functor f => Algebra f a -> Term f -> a
cata'' f = histo $ fmap attribute >>> f

-- | but `histo` even generalizes `para`
para'' :: Functor f => RAlgebra f a -> Term f -> a
para'' f = histo $ fmap worker >>> f where
  worker (Attr a h) = (In (fmap (worker >>> fst) h), a)
