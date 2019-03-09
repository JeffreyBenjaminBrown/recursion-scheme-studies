{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Jbb.Abc where

import Lib


data Abc = A | B Abc | C Abc Abc
  deriving (Eq, Show, Ord)

data AbcF a = AF | BF a | CF a a
  deriving (Show, Eq, Ord, Functor)

toUnTerm_abc :: Algebra AbcF Abc
toUnTerm_abc AF       = A
toUnTerm_abc (BF x)   = B x
toUnTerm_abc (CF x y) = C x y

unTerm_abc :: Term AbcF -> Abc
unTerm_abc = cata toUnTerm_abc

aTerm_abc :: Term AbcF
aTerm_abc = In $ CF ( In $ BF
                      ( In $ CF
                        ( In AF)
                        ( In AF)))
                    ( In $ CF
                      ( In $ BF $ In AF)
                      ( In $ BF $ In $ AF))

forAna :: Coalgebra AbcF Int -- ^ Works! try `unTerm_abc $ ana forAna 0`
forAna i | i < 2     = CF (i+1) (i+1)
         | i < 4     = BF (i+1)
         | otherwise = AF

forCata :: Algebra AbcF Int -- ^ Works! try `cata forCata aTerm_abc`
forCata AF       = 0
forCata (BF i)   = 1 + i
forCata (CF i j) = 2 + i + j
