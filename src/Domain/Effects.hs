{-# LANGUAGE DeriveGeneric #-}

module Domain.Effects where

import GHC.Generics (Generic)
import Domain.Types

-- | Effect on character stats (HP, AC, etc)
data StatEffect = StatEffect
    { statEffectName     :: String     -- "currentHP", "maxHP", "armorClass"
    , statEffectModifier :: String     -- Numeric value or dice expression like "-1d6"
    } deriving (Show, Eq, Generic)

-- | Effect on equipment (items equipped like weapons, armor)
data EquipmentEffect = EquipmentEffect
    { equipmentEffectName     :: String           -- Item name
    , equipmentEffectType     :: String           -- "weapon", "armor", "wondrous", etc
    , equipmentEffectBonus    :: Maybe String     -- Optional bonus description
    , equipmentEffectQuantity :: Maybe Int        -- Optional quantity (default 1)
    } deriving (Show, Eq, Generic)

-- | Effect on inventory (consumables, items carried)
data InventoryEffect = InventoryEffect
    { inventoryEffectName     :: String           -- Item name
    , inventoryEffectType     :: String           -- "currency", "potion", "consumable", etc
    , inventoryEffectQuantity :: Maybe Int        -- Quantity (for currency, etc)
    , inventoryEffectValue    :: Maybe Int        -- Value in gold pieces (for treasure)
    } deriving (Show, Eq, Generic)

-- | Effect on character attributes (STR, DEX, etc)
data AttributeEffect = AttributeEffect
    { attributeEffectName       :: String         -- "strength", "dexterity", etc
    , attributeEffectModifier   :: Maybe Int      -- Modifier to attribute score
    , attributeEffectSavingThrow :: Maybe Int     -- Modifier to saving throw
    } deriving (Show, Eq, Generic)

-- | Effect on character skills
data SkillEffect = SkillEffect
    { skillEffectName     :: String               -- Skill name
    , skillEffectModifier :: Maybe Int            -- Bonus to skill checks
    , skillEffectProf     :: Maybe String         -- Proficiency change: "Proficient", "Expertise"
    } deriving (Show, Eq, Generic)

-- | Complete effects structure for an entry
data EntryEffects = EntryEffects
    { effectsStats      :: [StatEffect]
    , effectsEquipment  :: [EquipmentEffect]
    , effectsInventory  :: [InventoryEffect]
    , effectsAttributes :: [AttributeEffect]
    , effectsSkills     :: [SkillEffect]
    } deriving (Show, Eq, Generic)

-- | Empty effects (no changes)
emptyEffects :: EntryEffects
emptyEffects = EntryEffects [] [] [] [] []

-- | Check if effects are empty
hasNoEffects :: EntryEffects -> Bool
hasNoEffects effects = 
    null (effectsStats effects) &&
    null (effectsEquipment effects) &&
    null (effectsInventory effects) &&
    null (effectsAttributes effects) &&
    null (effectsSkills effects)
