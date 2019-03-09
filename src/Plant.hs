{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Plant where

import System.Random

import Lib


data Plant a = Root a     -- every plant starts here
             | Stalk a    -- and continues upwards
             | Fork a a a -- but can trifurcate at any moment
             | Bloom      -- eventually terminating in a flower
             deriving (Show, Functor)
data Action = Flower  -- stop growing now
            | Upwards -- grow up with a Stalk
            | Branch  -- grow up with a Fork
data Seed = Seed { height :: Int
                 , rng    :: StdGen }

grow :: Seed -> (Action, Seed, Seed)
grow (Seed h rand) = ( choose choice
                     , left { height = h + 1}
                     , right { height = h + 1} )
  where (choice :: Int, _)                  = randomR (1 :: Int, 5) rand
        (leftR :: StdGen, rightR :: StdGen) = split rand
        (left :: Seed)                      = Seed h leftR
        (right :: Seed)                     = Seed h rightR
        choose 1                            = Flower
        choose 2                            = Branch
        choose _                            = Upwards

sow :: CVCoalgebra Plant Seed
sow seed =
  let (action, left, right) = grow seed
  in case (action, height seed) of
    (_, 0)       -> Root (Automatic left)
    (_, 10)      -> Bloom
    (Flower, _)  -> Bloom
    (Upwards, _) -> Stalk (Automatic right)
    (Branch, _)  -> Fork (Manual (Stalk (Automatic left)))
                         (Manual Bloom)
                         (Manual (Stalk (Automatic right)))
