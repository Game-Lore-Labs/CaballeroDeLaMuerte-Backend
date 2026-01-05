module Domain.Character where

import qualified Data.Set as S
import Domain.Types
import Domain.StatBlock
import Domain.Dice

-- | Item in inventory or equipment
data Item = Item
    { itemId          :: ItemId
    , itemName        :: String
    , itemDescription :: String
    } deriving (Show, Eq)

-- | Clue discovered by the player
data Clue = Clue
    { clueId          :: ClueId
    , clueName        :: String
    , clueDescription :: String
    } deriving (Show, Eq)

-- | Player character state
data PlayerCharacter = PlayerCharacter
    { pcName       :: String
    , pcCurrentHP  :: Int
    , pcMaxHP      :: Int
    , pcStatBlock  :: StatBlock
    , pcInventory  :: [Item]      -- Carried items
    , pcEquipment  :: [Item]      -- Equipped items
    , pcClues      :: S.Set ClueId
    } deriving (Show, Eq)

-- Accessors

getStatBlock :: PlayerCharacter -> StatBlock
getStatBlock = pcStatBlock

getInventory :: PlayerCharacter -> [Item]
getInventory = pcInventory

getClues :: PlayerCharacter -> S.Set ClueId
getClues = pcClues

hasClue :: ClueId -> PlayerCharacter -> Bool
hasClue clueId pc = S.member clueId (pcClues pc)

hasItem :: ItemId -> PlayerCharacter -> Bool
hasItem iid pc = any ((== iid) . itemId) (pcInventory pc ++ pcEquipment pc)

-- Actions

-- | Perform an ability check against a DC
abilityCheck :: PlayerCharacter -> Skill -> Int -> IO (Int, CheckResult)
abilityCheck pc skill dc = do
    let bonus = getSkillBonus (pcStatBlock pc) skill
    (total, success) <- rollCheck bonus dc
    pure (total, if success then Success else Failure)

-- | Perform a saving throw against a DC
savingThrow :: PlayerCharacter -> Attribute -> Int -> IO (Int, CheckResult)
savingThrow pc attr dc = do
    let bonus = getSavingThrowBonus (pcStatBlock pc) attr
    (total, success) <- rollCheck bonus dc
    pure (total, if success then Success else Failure)

-- | Attack with a weapon, returns (attack roll, hit?, damage if hit)
attack :: PlayerCharacter -> Weapon -> Int -> IO (Int, Bool, Maybe Int)
attack pc weapon targetAC = do
    let atkBonus = getAttackBonus (pcStatBlock pc) weapon
    (atkTotal, hit) <- rollCheck atkBonus targetAC
    if hit
        then do
            dmgRoll <- roll (weaponDamageDice weapon) (weaponDamageCount weapon) (getDamageBonus (pcStatBlock pc) weapon)
            pure (atkTotal, True, Just (rollTotal dmgRoll))
        else pure (atkTotal, False, Nothing)

-- | Take damage, returns updated character
takeDamage :: Int -> PlayerCharacter -> PlayerCharacter
takeDamage dmg pc = pc { pcCurrentHP = max 0 (pcCurrentHP pc - dmg) }

-- | Heal character, capped at max HP
heal :: Int -> PlayerCharacter -> PlayerCharacter
heal amount pc = pc { pcCurrentHP = min (pcMaxHP pc) (pcCurrentHP pc + amount) }

-- | Add item to inventory
addItem :: Item -> PlayerCharacter -> PlayerCharacter
addItem item pc = pc { pcInventory = item : pcInventory pc }

-- | Remove item from inventory
removeItem :: ItemId -> PlayerCharacter -> PlayerCharacter
removeItem iid pc = pc { pcInventory = filter ((/= iid) . itemId) (pcInventory pc) }

-- | Add clue
addClue :: ClueId -> PlayerCharacter -> PlayerCharacter
addClue cid pc = pc { pcClues = S.insert cid (pcClues pc) }

-- | Check if character is alive
isAlive :: PlayerCharacter -> Bool
isAlive pc = pcCurrentHP pc > 0
