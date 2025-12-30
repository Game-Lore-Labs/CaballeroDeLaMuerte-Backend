{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module API.DTO where

import GHC.Generics (Generic)
import Data.Aeson
import qualified Data.Set as S

import Domain.Types
import Domain.StatBlock
import Domain.Character
import Domain.Enemy
import Domain.Entry
import Infrastructure.JSON ()

-- Generic API Response wrapper
data ApiResponse a = ApiResponse
    { success :: Bool
    , message :: String
    , payload :: Maybe a
    } deriving (Show, Eq, Generic)

instance ToJSON a => ToJSON (ApiResponse a)
instance FromJSON a => FromJSON (ApiResponse a)

okResponse :: a -> ApiResponse a
okResponse a = ApiResponse True "OK" (Just a)

errResponse :: String -> ApiResponse a
errResponse msg = ApiResponse False msg Nothing

-- Entry DTOs

data OptionDTO = OptionDTO
    { optDtoId          :: Int
    , optDtoDescription :: String
    } deriving (Show, Eq, Generic)

instance ToJSON OptionDTO where
    toJSON o = object
        [ "id"          .= optDtoId o
        , "description" .= optDtoDescription o
        ]

instance FromJSON OptionDTO where
    parseJSON = withObject "OptionDTO" $ \v -> OptionDTO
        <$> v .: "id"
        <*> v .: "description"

fromOption :: Option -> OptionDTO
fromOption opt = OptionDTO (optionId opt) (optionDescription opt)

data EntryDTO = EntryDTO
    { entryDtoId        :: EntryId
    , entryDtoNarrative :: String
    , entryDtoOptions   :: [OptionDTO]
    } deriving (Show, Eq, Generic)

instance ToJSON EntryDTO where
    toJSON e = object
        [ "id"        .= entryDtoId e
        , "narrative" .= entryDtoNarrative e
        , "options"   .= entryDtoOptions e
        ]

instance FromJSON EntryDTO where
    parseJSON = withObject "EntryDTO" $ \v -> EntryDTO
        <$> v .: "id"
        <*> v .: "narrative"
        <*> v .: "options"

-- Character DTOs

data CharacterDTO = CharacterDTO
    { charDtoCurrentHP :: Int
    , charDtoMaxHP     :: Int
    , charDtoAC        :: Int
    , charDtoInventory :: [ItemDTO]
    , charDtoEquipment :: [ItemDTO]
    , charDtoClues     :: [ClueId]
    } deriving (Show, Eq, Generic)

instance ToJSON CharacterDTO where
    toJSON c = object
        [ "currentHP" .= charDtoCurrentHP c
        , "maxHP"     .= charDtoMaxHP c
        , "ac"        .= charDtoAC c
        , "inventory" .= charDtoInventory c
        , "equipment" .= charDtoEquipment c
        , "clues"     .= charDtoClues c
        ]

instance FromJSON CharacterDTO where
    parseJSON = withObject "CharacterDTO" $ \v -> CharacterDTO
        <$> v .: "currentHP"
        <*> v .: "maxHP"
        <*> v .: "ac"
        <*> v .: "inventory"
        <*> v .: "equipment"
        <*> v .: "clues"

data ItemDTO = ItemDTO
    { itemDtoId          :: ItemId
    , itemDtoName        :: String
    , itemDtoDescription :: String
    } deriving (Show, Eq, Generic)

instance ToJSON ItemDTO where
    toJSON i = object
        [ "id"          .= itemDtoId i
        , "name"        .= itemDtoName i
        , "description" .= itemDtoDescription i
        ]

instance FromJSON ItemDTO where
    parseJSON = withObject "ItemDTO" $ \v -> ItemDTO
        <$> v .: "id"
        <*> v .: "name"
        <*> v .: "description"

fromItem :: Item -> ItemDTO
fromItem i = ItemDTO (itemId i) (itemName i) (itemDescription i)

fromCharacter :: PlayerCharacter -> CharacterDTO
fromCharacter pc = CharacterDTO
    { charDtoCurrentHP = pcCurrentHP pc
    , charDtoMaxHP     = pcMaxHP pc
    , charDtoAC        = armorClass (pcStatBlock pc)
    , charDtoInventory = map fromItem (pcInventory pc)
    , charDtoEquipment = map fromItem (pcEquipment pc)
    , charDtoClues     = S.toList (pcClues pc)
    }

-- Request DTOs

data SelectOptionRequest = SelectOptionRequest
    { selectOptionId :: Int
    } deriving (Show, Eq, Generic)

instance ToJSON SelectOptionRequest
instance FromJSON SelectOptionRequest

data AbilityCheckRequest = AbilityCheckRequest
    { checkSkill :: Skill
    , checkDC    :: Int
    } deriving (Show, Eq, Generic)

instance ToJSON AbilityCheckRequest
instance FromJSON AbilityCheckRequest

data SavingThrowRequest = SavingThrowRequest
    { saveAttribute :: Attribute
    , saveDC        :: Int
    } deriving (Show, Eq, Generic)

instance ToJSON SavingThrowRequest
instance FromJSON SavingThrowRequest

data DiceRollRequest = DiceRollRequest
    { rollDiceType  :: DiceType
    , rollDiceCount :: Int
    , rollDiceBonus :: Int
    } deriving (Show, Eq, Generic)

instance ToJSON DiceRollRequest
instance FromJSON DiceRollRequest

data CombatAttackRequest = CombatAttackRequest
    { attackTargetIndex :: Int
    , attackWeaponIndex :: Int
    } deriving (Show, Eq, Generic)

instance ToJSON CombatAttackRequest
instance FromJSON CombatAttackRequest

-- Response DTOs

data OptionResultDTO = OptionResultDTO
    { resultType        :: String
    , resultRoll        :: Maybe Int
    , resultDC          :: Maybe Int
    , resultDestination :: Maybe EntryId
    , resultSkill       :: Maybe String
    , resultAttribute   :: Maybe String
    } deriving (Show, Eq, Generic)

instance ToJSON OptionResultDTO where
    toJSON r = object
        [ "type"        .= resultType r
        , "roll"        .= resultRoll r
        , "dc"          .= resultDC r
        , "destination" .= resultDestination r
        , "skill"       .= resultSkill r
        , "attribute"   .= resultAttribute r
        ]

instance FromJSON OptionResultDTO where
    parseJSON = withObject "OptionResultDTO" $ \v -> OptionResultDTO
        <$> v .: "type"
        <*> v .:? "roll"
        <*> v .:? "dc"
        <*> v .:? "destination"
        <*> v .:? "skill"
        <*> v .:? "attribute"

data CheckResultDTO = CheckResultDTO
    { checkResultSkill   :: String
    , checkResultRoll    :: Int
    , checkResultBonus   :: Int
    , checkResultTotal   :: Int
    , checkResultDC      :: Int
    , checkResultSuccess :: Bool
    } deriving (Show, Eq, Generic)

instance ToJSON CheckResultDTO where
    toJSON c = object
        [ "skill"   .= checkResultSkill c
        , "roll"    .= checkResultRoll c
        , "bonus"   .= checkResultBonus c
        , "total"   .= checkResultTotal c
        , "dc"      .= checkResultDC c
        , "success" .= checkResultSuccess c
        ]

instance FromJSON CheckResultDTO where
    parseJSON = withObject "CheckResultDTO" $ \v -> CheckResultDTO
        <$> v .: "skill"
        <*> v .: "roll"
        <*> v .: "bonus"
        <*> v .: "total"
        <*> v .: "dc"
        <*> v .: "success"

data DiceRollDTO = DiceRollDTO
    { diceValues :: [Int]
    , diceBonus  :: Int
    , diceTotal  :: Int
    } deriving (Show, Eq, Generic)

instance ToJSON DiceRollDTO where
    toJSON d = object
        [ "values" .= diceValues d
        , "bonus"  .= diceBonus d
        , "total"  .= diceTotal d
        ]

instance FromJSON DiceRollDTO where
    parseJSON = withObject "DiceRollDTO" $ \v -> DiceRollDTO
        <$> v .: "values"
        <*> v .: "bonus"
        <*> v .: "total"

-- Combat DTOs

data EnemyDTO = EnemyDTO
    { enemyDtoName      :: String
    , enemyDtoCurrentHP :: Int
    , enemyDtoMaxHP     :: Int
    , enemyDtoAC        :: Int
    } deriving (Show, Eq, Generic)

instance ToJSON EnemyDTO where
    toJSON e = object
        [ "name"      .= enemyDtoName e
        , "currentHP" .= enemyDtoCurrentHP e
        , "maxHP"     .= enemyDtoMaxHP e
        , "ac"        .= enemyDtoAC e
        ]

instance FromJSON EnemyDTO where
    parseJSON = withObject "EnemyDTO" $ \v -> EnemyDTO
        <$> v .: "name"
        <*> v .: "currentHP"
        <*> v .: "maxHP"
        <*> v .: "ac"

fromEnemy :: Enemy -> EnemyDTO
fromEnemy e = EnemyDTO
    { enemyDtoName      = enemyName e
    , enemyDtoCurrentHP = enemyCurrentHP e
    , enemyDtoMaxHP     = enemyMaxHP e
    , enemyDtoAC        = armorClass (enemyStatBlock e)
    }

data CombatStatusDTO = CombatStatusDTO
    { combatStatusState   :: String
    , combatStatusPlayer  :: CharacterDTO
    , combatStatusEnemies :: [EnemyDTO]
    } deriving (Show, Eq, Generic)

instance ToJSON CombatStatusDTO where
    toJSON c = object
        [ "state"   .= combatStatusState c
        , "player"  .= combatStatusPlayer c
        , "enemies" .= combatStatusEnemies c
        ]

instance FromJSON CombatStatusDTO where
    parseJSON = withObject "CombatStatusDTO" $ \v -> CombatStatusDTO
        <$> v .: "state"
        <*> v .: "player"
        <*> v .: "enemies"

data CombatActionDTO = CombatActionDTO
    { actionType   :: String
    , actionActor  :: Maybe String
    , actionDamage :: Maybe Int
    , actionRoll   :: Maybe Int
    } deriving (Show, Eq, Generic)

instance ToJSON CombatActionDTO where
    toJSON a = object
        [ "type"   .= actionType a
        , "actor"  .= actionActor a
        , "damage" .= actionDamage a
        , "roll"   .= actionRoll a
        ]

instance FromJSON CombatActionDTO where
    parseJSON = withObject "CombatActionDTO" $ \v -> CombatActionDTO
        <$> v .: "type"
        <*> v .:? "actor"
        <*> v .:? "damage"
        <*> v .:? "roll"

-- Game State DTO

data GameStateDTO = GameStateDTO
    { gameCurrentEntry :: EntryId
    , gamePlayer       :: CharacterDTO
    , gameHistory      :: [EntryId]
    } deriving (Show, Eq, Generic)

instance ToJSON GameStateDTO where
    toJSON g = object
        [ "currentEntry" .= gameCurrentEntry g
        , "player"       .= gamePlayer g
        , "history"      .= gameHistory g
        ]

instance FromJSON GameStateDTO where
    parseJSON = withObject "GameStateDTO" $ \v -> GameStateDTO
        <$> v .: "currentEntry"
        <*> v .: "player"
        <*> v .: "history"
