module Application.CharacterService where

import qualified Data.Set as S

import Domain.Types
import Domain.StatBlock
import Domain.Character
import Domain.Dice
import Application.GameService

-- | Result of an ability check
data CheckResultInfo = CheckResultInfo
    { checkSkillOrAttr :: String
    , checkRoll        :: Int
    , checkBonus       :: Int
    , checkTotal       :: Int
    , checkDC          :: Int
    , checkSuccess     :: Bool
    } deriving (Show, Eq)

-- | Perform ability check and update state
performAbilityCheck :: Skill -> Int -> AppState -> IO (CheckResultInfo, AppState)
performAbilityCheck skill dc state = do
    let player = getPlayer state
        bonus = getSkillBonus (pcStatBlock player) skill
    dieRoll <- rollD20
    let total = dieRoll + bonus
        success = total >= dc
    pure (CheckResultInfo (show skill) dieRoll bonus total dc success, state)

-- | Perform saving throw and update state
performSavingThrow :: Attribute -> Int -> AppState -> IO (CheckResultInfo, AppState)
performSavingThrow attr dc state = do
    let player = getPlayer state
        bonus = getSavingThrowBonus (pcStatBlock player) attr
    dieRoll <- rollD20
    let total = dieRoll + bonus
        success = total >= dc
    pure (CheckResultInfo (show attr) dieRoll bonus total dc success, state)

-- | Deal damage to player
dealDamageToPlayer :: Int -> AppState -> AppState
dealDamageToPlayer dmg = updatePlayer (takeDamage dmg)

-- | Heal player
healPlayer :: Int -> AppState -> AppState
healPlayer amount = updatePlayer (heal amount)

-- | Add item to player inventory
giveItem :: Item -> AppState -> AppState
giveItem item = updatePlayer (addItem item)

-- | Remove item from player inventory
takeItem :: ItemId -> AppState -> AppState
takeItem itemId = updatePlayer (removeItem itemId)

-- | Give clue to player
giveClue :: ClueId -> AppState -> AppState
giveClue clueId = updatePlayer (addClue clueId)

-- | Check if player has specific clue
playerHasClue :: ClueId -> AppState -> Bool
playerHasClue clueId state = hasClue clueId (getPlayer state)

-- | Check if player has specific item
playerHasItem :: ItemId -> AppState -> Bool
playerHasItem itemId state = hasItem itemId (getPlayer state)

-- | Get player's current HP
getPlayerCurrentHP :: AppState -> Int
getPlayerCurrentHP = pcCurrentHP . getPlayer

-- | Get player's max HP
getPlayerMaxHP :: AppState -> Int
getPlayerMaxHP = pcMaxHP . getPlayer

-- | Check if player is alive
isPlayerAlive :: AppState -> Bool
isPlayerAlive = isAlive . getPlayer

-- | Get player inventory
getPlayerInventory :: AppState -> [Item]
getPlayerInventory = pcInventory . getPlayer

-- | Get player equipment
getPlayerEquipment :: AppState -> [Item]
getPlayerEquipment = pcEquipment . getPlayer

-- | Get all player clues
getPlayerClues :: AppState -> S.Set ClueId
getPlayerClues = pcClues . getPlayer

-- | Get player stat block
getPlayerStatBlock :: AppState -> StatBlock
getPlayerStatBlock = pcStatBlock . getPlayer

-- | Roll dice (exposed for general use)
rollDice :: DiceType -> Int -> Int -> IO RollResult
rollDice = roll
