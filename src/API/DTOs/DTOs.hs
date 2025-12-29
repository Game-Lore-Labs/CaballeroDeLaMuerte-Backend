{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Infrastructure.Api where

import Web.Scotty
import Data.Aeson (ToJSON, FromJSON, object, (.=))
import GHC.Generics
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Middleware.Cors
import Network.HTTP.Types.Status

import Domain.Entities
import Application.UseCases

-- ============================================================================
-- API REQUEST/RESPONSE TYPES
-- ============================================================================
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.DTOs
  ( -- * Scene Types
    SceneEntry(..)
  , SceneType(..)
  , Narrative(..)
  
    -- * Exploration Types
  , ExplorationData(..)
  , GridSize(..)
  , Position(..)
  , Interactable(..)
  , InteractableType(..)
  
    -- * Combat Types
  , CombatData(..)
  , Enemy(..)
  , Stats(..)
  , Attack(..)
  , DamageType(..)
  
    -- * Effect Types
  , Effect(..)
  , EffectType(..)
  , DamageEffect(..)
  , HealEffect(..)
  , GainItemEffect(..)
  , Item(..)
  , ItemType(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

-- | Scene Types
data SceneType
  = Exploration
  | Combat
  deriving (Show, Eq, Generic)

instance ToJSON SceneType where
  toJSON Exploration = String "exploration"
  toJSON Combat = String "combat"

instance FromJSON SceneType where
  parseJSON = withText "SceneType" $ \case
    "exploration" -> pure Exploration
    "combat" -> pure Combat
    _ -> fail "Invalid scene type"

data Narrative = Narrative
  { narrativeTitle :: Text
  , narrativeText :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON Narrative where
  toJSON (Narrative title text) =
    object ["title" .= title, "text" .= text]

instance FromJSON Narrative where
  parseJSON = withObject "Narrative" $ \v ->
    Narrative <$> v .: "title" <*> v .: "text"

data SceneEntry = SceneEntry
  { sceneId :: Text
  , sceneType :: SceneType
  , sceneNarrative :: Narrative
  , sceneExploration :: Maybe ExplorationData
  , sceneCombat :: Maybe CombatData
  } deriving (Show, Eq, Generic)

instance ToJSON SceneEntry where
  toJSON (SceneEntry sid stype narr expl comb) =
    object $
      [ "id" .= sid
      , "type" .= stype
      , "narrative" .= narr
      ] ++ maybe [] (\e -> ["exploration" .= e]) expl
        ++ maybe [] (\c -> ["combat" .= c]) comb

instance FromJSON SceneEntry where
  parseJSON = withObject "SceneEntry" $ \v ->
    SceneEntry
      <$> v .: "id"
      <*> v .: "type"
      <*> v .: "narrative"
      <*> v .:? "exploration"
      <*> v .:? "combat"

-- | Exploration Types
data Position = Position
  { posX :: Int
  , posY :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON Position where
  toJSON (Position x y) = object ["x" .= x, "y" .= y]

instance FromJSON Position where
  parseJSON = withObject "Position" $ \v ->
    Position <$> v .: "x" <*> v .: "y"

data GridSize = GridSize
  { gridWidth :: Int
  , gridHeight :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON GridSize where
  toJSON (GridSize w h) = object ["width" .= w, "height" .= h]

instance FromJSON GridSize where
  parseJSON = withObject "GridSize" $ \v ->
    GridSize <$> v .: "width" <*> v .: "height"

data InteractableType
  = Object
  | Trigger
  deriving (Show, Eq, Generic)

instance ToJSON InteractableType where
  toJSON Object = String "object"
  toJSON Trigger = String "trigger"

instance FromJSON InteractableType where
  parseJSON = withText "InteractableType" $ \case
    "object" -> pure Object
    "trigger" -> pure Trigger
    _ -> fail "Invalid interactable type"

data Interactable = Interactable
  { interactableId :: Text
  , interactablePos :: Position
  , interactableType :: InteractableType
  , interactableName :: Text
  , interactableEntryId :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON Interactable where
  toJSON (Interactable iid pos itype name entry) =
    object
      [ "id" .= iid
      , "pos" .= pos
      , "type" .= itype
      , "name" .= name
      , "entryId" .= entry
      ]

instance FromJSON Interactable where
  parseJSON = withObject "Interactable" $ \v ->
    Interactable
      <$> v .: "id"
      <*> v .: "pos"
      <*> v .: "type"
      <*> v .: "name"
      <*> v .: "entryId"

data ExplorationData = ExplorationData
  { explorationMapId :: Text
  , explorationGridSize :: GridSize
  , explorationPlayerStartPos :: Position
  , explorationInteractables :: [Interactable]
  } deriving (Show, Eq, Generic)

instance ToJSON ExplorationData where
  toJSON (ExplorationData mapId grid start interacts) =
    object
      [ "mapId" .= mapId
      , "gridSize" .= grid
      , "playerStartPos" .= start
      , "interactables" .= interacts
      ]

instance FromJSON ExplorationData where
  parseJSON = withObject "ExplorationData" $ \v ->
    ExplorationData
      <$> v .: "mapId"
      <*> v .: "gridSize"
      <*> v .: "playerStartPos"
      <*> v .: "interactables"

-- | Combat Types
data DamageType
  = Slashing
  | Piercing
  | Bludgeoning
  | Fire
  | Cold
  | Lightning
  | Poison
  | Acid
  | Necrotic
  | Radiant
  deriving (Show, Eq, Generic)

instance ToJSON DamageType where
  toJSON Slashing = String "slashing"
  toJSON Piercing = String "piercing"
  toJSON Bludgeoning = String "bludgeoning"
  toJSON Fire = String "fire"
  toJSON Cold = String "cold"
  toJSON Lightning = String "lightning"
  toJSON Poison = String "poison"
  toJSON Acid = String "acid"
  toJSON Necrotic = String "necrotic"
  toJSON Radiant = String "radiant"

instance FromJSON DamageType where
  parseJSON = withText "DamageType" $ \case
    "slashing" -> pure Slashing
    "piercing" -> pure Piercing
    "bludgeoning" -> pure Bludgeoning
    "fire" -> pure Fire
    "cold" -> pure Cold
    "lightning" -> pure Lightning
    "poison" -> pure Poison
    "acid" -> pure Acid
    "necrotic" -> pure Necrotic
    "radiant" -> pure Radiant
    _ -> fail "Invalid damage type"

data Attack = Attack
  { attackName :: Text
  , attackBonus :: Int
  , attackDamage :: Text -- e.g., "1d6+1"
  , attackDamageType :: DamageType
  } deriving (Show, Eq, Generic)

instance ToJSON Attack where
  toJSON (Attack name bonus dmg dtype) =
    object
      [ "name" .= name
      , "bonus" .= bonus
      , "damage" .= dmg
      , "damageType" .= dtype
      ]

instance FromJSON Attack where
  parseJSON = withObject "Attack" $ \v ->
    Attack
      <$> v .: "name"
      <*> v .: "bonus"
      <*> v .: "damage"
      <*> v .: "damageType"

data Stats = Stats
  { statsAC :: Int
  , statsHP :: Int
  , statsMaxHP :: Int
  , statsSpeed :: Int
  , statsStr :: Int
  , statsDex :: Int
  , statsCon :: Int
  , statsInt :: Int
  , statsWis :: Int
  , statsCha :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON Stats where
  toJSON (Stats ac hp maxhp spd str dex con int' wis cha) =
    object
      [ "ac" .= ac
      , "hp" .= hp
      , "maxHP" .= maxhp
      , "speed" .= spd
      , "str" .= str
      , "dex" .= dex
      , "con" .= con
      , "int" .= int'
      , "wis" .= wis
      , "cha" .= cha
      ]

instance FromJSON Stats where
  parseJSON = withObject "Stats" $ \v ->
    Stats
      <$> v .: "ac"
      <*> v .: "hp"
      <*> v .: "maxHP"
      <*> v .: "speed"
      <*> v .: "str"
      <*> v .: "dex"
      <*> v .: "con"
      <*> v .: "int"
      <*> v .: "wis"
      <*> v .: "cha"

data Enemy = Enemy
  { enemyId :: Text
  , enemyName :: Text
  , enemyStats :: Stats
  , enemyAttacks :: [Attack]
  } deriving (Show, Eq, Generic)

instance ToJSON Enemy where
  toJSON (Enemy eid name stats attacks) =
    object
      [ "id" .= eid
      , "name" .= name
      , "stats" .= stats
      , "attacks" .= attacks
      ]

instance FromJSON Enemy where
  parseJSON = withObject "Enemy" $ \v ->
    Enemy
      <$> v .: "id"
      <*> v .: "name"
      <*> v .: "stats"
      <*> v .: "attacks"

data CombatData = CombatData
  { combatEnemies :: [Enemy]
  , combatOnVictory :: Text
  , combatOnDefeat :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON CombatData where
  toJSON (CombatData enemies victory defeat) =
    object
      [ "enemies" .= enemies
      , "onVictory" .= victory
      , "onDefeat" .= defeat
      ]

instance FromJSON CombatData where
  parseJSON = withObject "CombatData" $ \v ->
    CombatData
      <$> v .: "enemies"
      <*> v .: "onVictory"
      <*> v .: "onDefeat"

-- | Effect Types
data ItemType
  = Potion
  | Weapon
  | Armor
  | Consumable
  | Quest
  deriving (Show, Eq, Generic)

instance ToJSON ItemType where
  toJSON Potion = String "Potion"
  toJSON Weapon = String "Weapon"
  toJSON Armor = String "Armor"
  toJSON Consumable = String "Consumable"
  toJSON Quest = String "Quest"

instance FromJSON ItemType where
  parseJSON = withText "ItemType" $ \case
    "Potion" -> pure Potion
    "Weapon" -> pure Weapon
    "Armor" -> pure Armor
    "Consumable" -> pure Consumable
    "Quest" -> pure Quest
    _ -> fail "Invalid item type"

data Item = Item
  { itemName :: Text
  , itemValue :: Int
  , itemType :: ItemType
  } deriving (Show, Eq, Generic)

instance ToJSON Item where
  toJSON (Item name val itype) =
    object
      [ "itemName" .= name
      , "itemValue" .= val
      , "itemType" .= itype
      ]

instance FromJSON Item where
  parseJSON = withObject "Item" $ \v ->
    Item
      <$> v .: "itemName"
      <*> v .: "itemValue"
      <*> v .: "itemType"

data DamageEffect = DamageEffect
  { damageAmount :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON DamageEffect where
  toJSON (DamageEffect amt) = object ["amount" .= amt]

instance FromJSON DamageEffect where
  parseJSON = withObject "DamageEffect" $ \v ->
    DamageEffect <$> v .: "amount"

data HealEffect = HealEffect
  { healAmount :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON HealEffect where
  toJSON (HealEffect amt) = object ["amount" .= amt]

instance FromJSON HealEffect where
  parseJSON = withObject "HealEffect" $ \v ->
    HealEffect <$> v .: "amount"

data GainItemEffect = GainItemEffect
  { gainItem :: Item
  } deriving (Show, Eq, Generic)

instance ToJSON GainItemEffect where
  toJSON (GainItemEffect item) = object ["item" .= item]

instance FromJSON GainItemEffect where
  parseJSON = withObject "GainItemEffect" $ \v ->
    GainItemEffect <$> v .: "item"

data EffectType
  = Damage
  | Heal
  | GainItem
  deriving (Show, Eq, Generic)

instance ToJSON EffectType where
  toJSON Damage = String "damage"
  toJSON Heal = String "heal"
  toJSON GainItem = String "gainItem"

instance FromJSON EffectType where
  parseJSON = withText "EffectType" $ \case
    "damage" -> pure Damage
    "heal" -> pure Heal
    "gainItem" -> pure GainItem
    _ -> fail "Invalid effect type"

data Effect = Effect
  { effectType :: EffectType
  , effectParams :: Value
  } deriving (Show, Eq, Generic)

instance ToJSON Effect where
  toJSON (Effect etype params) =
    object ["type" .= etype, "params" .= params]

instance FromJSON Effect where
  parseJSON = withObject "Effect" $ \v ->
    Effect <$> v .: "type" <*> v .: "params"