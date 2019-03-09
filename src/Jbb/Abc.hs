{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Jbb.Abc where

import Lib


data Abc = A | B Abc | C Abc Abc
  deriving (Eq, Show, Ord)

data AbcF a = AF | BF a | CF a a
  deriving (Show, Eq, Ord, Functor)

-- replace aTerm here

unTerm_abc :: Term AbcF -> Abc
unTerm_abc = cata homo where homo :: Algebra AbcF Abc
                             homo AF       = A
                             homo (BF x)   = B x
                             homo (CF x y) = C x y

term_abc :: Abc -> Term AbcF
term_abc = ana homo where homo :: Coalgebra AbcF Abc
                          homo A       = AF
                          homo (B x)   = BF x
                          homo (C x y) = CF x y

forCata :: Algebra AbcF Int -- ^ to demo: `cata forCata aTerm_abc`
forCata AF       = 0
forCata (BF i)   = 1 + i
forCata (CF i j) = 2 + i + j

forAna :: Coalgebra AbcF Int -- ^ to demo `unTerm_abc $ ana forAna 0`
forAna i | i < 2     = CF (i+1) (i+1)
         | i < 4     = BF (i+1)
         | otherwise = AF

aTerm_abc :: Term AbcF
aTerm_abc = In $ CF ( In $ BF $ In $ BF ( In $ CF ( In AF)
                                                  ( In AF)))
                    ( In $ CF ( In $ BF $ In AF)
                              ( In $ BF $ In AF))

aTerm_abc_bbba :: Term AbcF
aTerm_abc_bbba = In $ BF $ In $ BF $ In $ BF $ In AF

forPara :: RAlgebra AbcF Int -- ^ to demo: `para forPara aTerm_abc_bbba`
forPara AF                   = 0
forPara (BF (In (BF _) , i)) = 1 + i*2
forPara (BF (_         , i)) = 1 + i
forPara (CF (_,i) (_,j))     = 1 + i + j

forPara' :: RAlgebra' AbcF Int -- ^ to demo: `para' forPara' aTerm_abc_bbba`
forPara' _                     AF       = 0
forPara' (In (BF (In (BF _)))) (BF i)   = 1 + i*2
forPara' _                     (BF i)   = 1 + i
forPara' _                     (CF i j) = 1 + i + j

forApo :: RCoalgebra AbcF Int -- ^ unTerm_abc $ apo forApo 0
forApo 0             = CF (Right 1) (Right 2)
forApo 1             = BF $ Left $ In $ CF (In AF) (In AF)
forApo i | i < 4     = BF $ Right $ i+1
         | otherwise = AF
