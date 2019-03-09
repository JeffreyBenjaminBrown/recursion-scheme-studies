{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Jbb.Abc () where

import Lib


data Abc = A | B Abc | C Abc Abc
  deriving (Eq, Show, Ord)

data AbcF a = AF | BF a | CF a a
  deriving (Show, Eq, Ord, Functor)


-- | == convert between AbcF and Abc

fromTerm_abc :: Term AbcF -> Abc
fromTerm_abc = cata iso where iso :: Algebra AbcF Abc
                              iso AF       = A
                              iso (BF x)   = B x
                              iso (CF x y) = C x y

toTerm_abc :: Abc -> Term AbcF
toTerm_abc = ana iso where iso :: Coalgebra AbcF Abc
                           iso A       = AF
                           iso (B x)   = BF x
                           iso (C x y) = CF x y


-- | == `cata` and `ana`

an_abc :: Abc
an_abc =  C ( B $ B (C A A ))
            ( C (B A) (B A))

forCata :: Algebra AbcF Int -- ^ to demo: `cata forCata $ toTerm_abc an_abc`
forCata AF       = 0
forCata (BF i)   = 1 + i
forCata (CF i j) = 2 + i + j

forAna :: Coalgebra AbcF Int -- ^ to demo: `fromTerm_abc $ ana forAna 0`
forAna i | i < 2     = CF (i+1) (i+1)
         | i < 4     = BF (i+1)
         | otherwise = AF


-- | == `para`, `para'` and `apo`

abc_bbba :: Abc
abc_bbba = B $ B $ B A

forPara :: RAlgebra AbcF Int -- ^ `para forPara $ toTerm_abc abc_bbba`
forPara AF                   = 0
forPara (BF (In (BF _) , i)) = 1 + i*2
forPara (BF (_         , i)) = 1 + i
forPara (CF (_,i) (_,j))     = 1 + i + j

forPara' :: RAlgebra' AbcF Int -- ^ to demo: `para' forPara' aTerm_abc_bbba`
forPara' _                     AF       = 0
forPara' (In (BF (In (BF _)))) (BF i)   = 1 + i*2
forPara' _                     (BF i)   = 1 + i
forPara' _                     (CF i j) = 1 + i + j

forApo :: RCoalgebra AbcF Int -- ^ fromTerm_abc $ apo forApo 0
forApo 0             = CF (Right 1) (Right 2)
forApo 1             = BF $ Left $ In $ CF (In AF) (In AF)
forApo i | i < 4     = BF $ Right $ i+1
         | otherwise = AF


-- | == `histo` and `futu`

-- | `forHisto` gives a 100 point bonus to the longer one,
-- because at some point in the fold's history it returns 2.
abc_histo, abc_histo' :: Abc
abc_histo  = B $ B $ A
abc_histo' = B $ B $ B A

forHisto :: CVAlgebra AbcF Int -- ^ histo forHisto $ toTerm_abc abc_histo`
forHisto abc = case abc of
  AF -> 0
  BF a -> 1 + attribute a
          + (if history_has_a_2 a then 100 else 0)
  CF a b -> 1 + attribute a + attribute b
  where
    history_has_a_2 :: Attr AbcF Int -> Bool -- Thanks to Reddit user gelisam: http://www.reddit.com/r/haskell/comments/az4w8e/a_natural_way_to_fold_over_the_history_involved/ei5cp8j/
    history_has_a_2 = cata f . toTerm_attr where
      f :: Algebra (AttrF AbcF Int) Bool
      f (AttrF 2 _)        = True
      f (AttrF _ AF)       = False
      f (AttrF _ (BF b))   = b
      f (AttrF _ (CF b c)) = b || c


-- | = futu

forFutu :: CVCoalgebra AbcF Int -- ^ fromTerm_abc $ futu forFutu 0
forFutu i
  | i < 2 = CF (Automatic $ i+1) (Automatic $ i+2)
  | i < 4 = BF (Automatic $ i+1)
  | i < 6 = CF (Manual $ BF $ Automatic $ i+1) (Manual AF)
    -- Here is the clause that `apo` cannot express. When the `RCoalgebra`
    -- handed to `apo` returns a `Left`, it is a complete term.
    -- Analogously, when the `CVCoalgebra` handed to `futu` returns a
    -- `Manual` (call it "m"), there can still be some `Automatic` 
    -- computation left to process in the depths of "m".
  | otherwise = AF
