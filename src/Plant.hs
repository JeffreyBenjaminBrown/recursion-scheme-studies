{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Plant where

import System.Random

--import Lib


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
