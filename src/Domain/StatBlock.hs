module Domain.StatBlock where

import qualified Data.Map as M
import Domain.Types

-- | Attribute scores for all six attributes
type AttributeScores = M.Map Attribute Int

-- | Skill proficiency levels
data Proficiency = NotProficient | Proficient | Expertise
    deriving (Show, Eq, Ord)

-- | Weapon definition
data Weapon = Weapon
    { weaponName        :: String
    , weaponRange       :: (Int, Int)   -- (normal, long) range in feet
    , weaponDamageType  :: DamageType
    , weaponDamageDice  :: DiceType
    , weaponDamageCount :: Int          -- number of dice
    } deriving (Show, Eq)

-- | Complete stat block for a character or NPC
data StatBlock = StatBlock
    { attributes          :: AttributeScores
    , savingThrowProfs    :: [Attribute]         -- Proficient saving throws
    , skillProfs          :: M.Map Skill Proficiency
    , proficiencyBonus    :: Int
    , armorClass          :: Int
    , speed               :: Int
    , initiative          :: Int
    , weapons             :: [Weapon]
    } deriving (Show, Eq)

-- | Calculate modifier from attribute score: (score - 10) / 2
attributeModifier :: Int -> Int
attributeModifier score = (score - 10) `div` 2

-- | Get attribute score from stat block
getAttributeScore :: StatBlock -> Attribute -> Int
getAttributeScore sb attr = M.findWithDefault 10 attr (attributes sb)

-- | Get attribute modifier from stat block
getAttributeModifier :: StatBlock -> Attribute -> Int
getAttributeModifier sb = attributeModifier . getAttributeScore sb

-- | Get saving throw bonus for an attribute
getSavingThrowBonus :: StatBlock -> Attribute -> Int
getSavingThrowBonus sb attr =
    let baseMod = getAttributeModifier sb attr
        profBonus = if attr `elem` savingThrowProfs sb
                    then proficiencyBonus sb
                    else 0
    in baseMod + profBonus

-- | Get skill bonus
getSkillBonus :: StatBlock -> Skill -> Int
getSkillBonus sb skill =
    let attr = skillAttribute skill
        baseMod = getAttributeModifier sb attr
        profLevel = M.findWithDefault NotProficient skill (skillProfs sb)
        profBonus = case profLevel of
            NotProficient -> 0
            Proficient    -> proficiencyBonus sb
            Expertise     -> proficiencyBonus sb * 2
    in baseMod + profBonus

-- | Get attack bonus for a weapon (assumes DEX for finesse/ranged)
getAttackBonus :: StatBlock -> Weapon -> Int
getAttackBonus sb _ = getAttributeModifier sb Dexterity + proficiencyBonus sb

-- | Get damage bonus for a weapon
getDamageBonus :: StatBlock -> Weapon -> Int
getDamageBonus sb _ = getAttributeModifier sb Dexterity
