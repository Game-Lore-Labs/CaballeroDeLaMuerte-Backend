{-# LANGUAGE OverloadedStrings #-}

module Dice where

import System.Random (randomRIO)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)


-- Representa una expresión de dados como "2d6+3"
data DiceExpression = DiceExpression
  { diceCount :: Int
  , diceSides :: Int
  , modifier :: Int
  } deriving (Show, Eq)

-- Parsea texto como "2d6+3" o "1d20"
parseDiceExpression :: Text -> Maybe DiceExpression
parseDiceExpression expr = 
  let parts = T.splitOn "d" expr
  in case parts of
    [countStr, rest] -> do
      count <- readMaybe (T.unpack countStr) :: Maybe Int
      let (sidesStr, modStr) = T.break (\c -> c == '+' || c == '-') rest
      sides <- readMaybe (T.unpack sidesStr) :: Maybe Int
      let mod = if T.null modStr
                then 0
                else case readMaybe (T.unpack $ T.tail modStr) :: Maybe Int of
                  Just m -> if T.head modStr == '-' then -m else m
                  Nothing -> 0
      return $ DiceExpression count sides mod
    _ -> Nothing


-- Resultado de una tirada
data RollResult = RollResult
  { rolls :: [Int]
  , total :: Int
  , naturalRolls :: [Int]  -- Sin modificadores
  } deriving (Show, Eq)

-- Tira dados según expresión
rollDice :: DiceExpression -> IO RollResult
rollDice (DiceExpression count sides mod) = do
  naturalRolls <- sequence $ replicate count (randomRIO (1, sides))
  let total = sum naturalRolls + mod
  return $ RollResult naturalRolls total naturalRolls

-- Tira un d20 simple
rollD20 :: IO Int
rollD20 = randomRIO (1, 20)

-- Tira con ventaja (advantage) - toma el mayor de 2d20
rollWithAdvantage :: IO Int
rollWithAdvantage = do
  roll1 <- rollD20
  roll2 <- rollD20
  return $ max roll1 roll2

-- Tira con desventaja (disadvantage) - toma el menor de 2d20
rollWithDisadvantage :: IO Int
rollWithDisadvantage = do
  roll1 <- rollD20
  roll2 <- rollD20
  return $ min roll1 roll2


data AdvantageType = NoAdvantage | Advantage | Disadvantage
  deriving (Show, Eq)

-- Realiza un check de habilidad
performAbilityCheck :: 
  CheckType -> 
  Int ->           -- Modificador de habilidad
  Bool ->          -- ¿Es proficiente?
  Int ->           -- Bonus de proficiencia
  AdvantageType -> 
  Int ->           -- DC
  IO Bool
performAbilityCheck checkType abilityMod isProficient profBonus advType dc = do
  roll <- case advType of
    NoAdvantage -> rollD20
    Advantage -> rollWithAdvantage
    Disadvantage -> rollWithDisadvantage
  
  let profMod = if isProficient then profBonus else 0
  let total = roll + abilityMod + profMod
  
  return $ total >= dc

-- Calcula modificador desde stat (10 = +0, 12 = +1, 8 = -1, etc.)
calculateModifier :: Int -> Int
calculateModifier stat = (stat - 10) `div` 2

import Types


-- Realiza un check usando stats del personaje
performCheckForCharacter ::
  Character ->
  CheckType ->
  Int ->  -- DC
  AdvantageType ->
  IO Bool
performCheckForCharacter char checkType dc advType = do
  let stats = charStats char
  let profBonus = proficiencyBonus stats
  
  -- Determinar qué stat usar según el check
  let (abilityScore, isProficient) = case checkType of
        Perception -> (wis stats, True)  -- Asumimos proficiencia por ahora
        Athletics -> (str stats, True)
        Stealth -> (dex stats, True)
        Investigation -> (int stats, True)
        -- ... otros casos
        _ -> (wis stats, False)
  
  let abilityMod = calculateModifier abilityScore
  
  performAbilityCheck checkType abilityMod isProficient profBonus advType dc


-- Realiza un ataque
performAttack ::
  Int ->  -- Attack bonus
  Int ->  -- Target AC
  DiceExpression ->  -- Daño
  IO (Bool, Int)  -- (Acierta?, Daño)
performAttack attackBonus targetAC damageExpr = do
  attackRoll <- rollD20
  let hits = (attackRoll + attackBonus) >= targetAC
  
  if hits
    then do
      damageResult <- rollDice damageExpr
      return (True, total damageResult)
    else return (False, 0)

-- Ataque con crítico (natural 20 = doble daño)
performAttackWithCrit ::
  Int ->
  Int ->
  DiceExpression ->
  IO (Bool, Int, Bool)  -- (Acierta?, Daño, Crítico?)
performAttackWithCrit attackBonus targetAC damageExpr = do
  attackRoll <- rollD20
  let isCrit = attackRoll == 20
  let hits = isCrit || ((attackRoll + attackBonus) >= targetAC)
  
  if hits
    then do
      damageResult <- rollDice damageExpr
      let finalDamage = if isCrit 
                        then total damageResult * 2 
                        else total damageResult
      return (True, finalDamage, isCrit)
    else return (False, 0, False)

