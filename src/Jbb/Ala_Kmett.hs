{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Jbb.Ala_Kmett where

import Data.Functor.Base
import Data.Functor.Foldable
import Data.Functor.Foldable.TH


-- | == Types

data Abc = A | B Abc | C Abc Abc
  deriving (Eq, Show, Ord)

makeBaseFunctor ''Abc


-- | == Morphisms

-- | To demo these, first `import qualified Data.Functor.Foldable as F`

-- | = `cata`

forCata :: Base Abc Int -> Int -- ^ to demo: `F.cata forCata aTerm_abc`
forCata AF       = 0
forCata (BF i)   = 1 + i
forCata (CF i j) = 2 + i + j

aTerm_abc :: Abc
aTerm_abc = C ( B $ B ( C A A ) )
              ( C ( B A)
                  ( B A) )

forAna :: Int -> Base Abc Int -- ^ to demo: `F.ana forAna 0 :: Abc`
forAna i | i < 2     = CF (i+1) (i+1)
         | i < 4     = BF (i+1)
         | otherwise = AF
