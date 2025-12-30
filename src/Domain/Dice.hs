module Domain.Dice where

import System.Random (randomRIO)
import Domain.Types (DiceType(..), diceFaces)

-- | Result of a dice roll
data RollResult = RollResult
    { rollValues :: [Int]  -- Individual dice results
    , rollBonus  :: Int    -- Flat bonus applied
    , rollTotal  :: Int    -- Sum of values + bonus
    } deriving (Show, Eq)

-- | Roll N dice of M faces with a flat bonus
-- roll D20 1 3  ->  rolls 1d20+3
-- roll D6 2 0   ->  rolls 2d6
roll :: DiceType -> Int -> Int -> IO RollResult
roll diceType count bonus = do
    values <- sequence $ replicate count (randomRIO (1, faces))
    let total = sum values + bonus
    pure $ RollResult values bonus total
  where
    faces = diceFaces diceType

-- | Roll a single die
rollSingle :: DiceType -> IO Int
rollSingle diceType = randomRIO (1, diceFaces diceType)

-- | Roll a D20 (most common roll)
rollD20 :: IO Int
rollD20 = rollSingle D20

-- | Roll a D20 with bonus and check against DC
rollCheck :: Int -> Int -> IO (Int, Bool)
rollCheck bonus dc = do
    dieResult <- rollD20
    let total = dieResult + bonus
    pure (total, total >= dc)
