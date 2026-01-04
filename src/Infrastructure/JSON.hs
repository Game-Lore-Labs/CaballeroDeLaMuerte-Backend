{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.JSON where

import GHC.Generics (Generic)
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Domain.Types hiding (Success, Failure) -- Hide to avoid ambiguity
import qualified Domain.Types as DT -- Import qualified to use DT.Success
import Domain.StatBlock
import Domain.Character
import Domain.Enemy
import Domain.Entry

-- Attribute JSON instances
instance ToJSON Attribute where
    toJSON = toJSON . show

instance FromJSON Attribute where
    parseJSON = withText "Attribute" $ \t -> case T.unpack t of
        "Strength"     -> pure Strength
        "Dexterity"    -> pure Dexterity
        "Constitution" -> pure Constitution
        "Intelligence" -> pure Intelligence
        "Wisdom"       -> pure Wisdom
        "Charisma"     -> pure Charisma
        _              -> fail "Invalid Attribute"

-- Skill JSON instances
instance ToJSON Skill where
    toJSON = toJSON . show

instance FromJSON Skill where
    parseJSON = withText "Skill" $ \t -> case T.unpack t of
        "Athletics"      -> pure Athletics
        "Acrobatics"     -> pure Acrobatics
        "SleightOfHand"  -> pure SleightOfHand
        "Stealth"        -> pure Stealth
        "Arcana"         -> pure Arcana
        "History"        -> pure History
        "Investigation"  -> pure Investigation
        "Nature"         -> pure Nature
        "Religion"       -> pure Religion
        "AnimalHandling" -> pure AnimalHandling
        "Insight"        -> pure Insight
        "Medicine"       -> pure Medicine
        "Perception"     -> pure Perception
        "Survival"       -> pure Survival
        "Deception"      -> pure Deception
        "Intimidation"   -> pure Intimidation
        "Performance"    -> pure Performance
        "Persuasion"     -> pure Persuasion
        _                -> fail "Invalid Skill"

-- DiceType JSON instances
instance ToJSON DiceType where
    toJSON = toJSON . show

instance FromJSON DiceType where
    parseJSON = withText "DiceType" $ \t -> case T.unpack t of
        "D2"   -> pure D2
        "D4"   -> pure D4
        "D6"   -> pure D6
        "D8"   -> pure D8
        "D10"  -> pure D10
        "D12"  -> pure D12
        "D20"  -> pure D20
        "D100" -> pure D100
        _      -> fail "Invalid DiceType"

-- DamageType JSON instances
instance ToJSON DamageType where
    toJSON = toJSON . show

instance FromJSON DamageType where
    parseJSON = withText "DamageType" $ \t -> case T.unpack t of
        "Piercing"    -> pure Piercing
        "Slashing"    -> pure Slashing
        "Bludgeoning" -> pure Bludgeoning
        "Fire"        -> pure Fire
        "Cold"        -> pure Cold
        "Lightning"   -> pure Lightning
        "Poison"      -> pure Poison
        "Necrotic"    -> pure Necrotic
        "Radiant"     -> pure Radiant
        _             -> fail "Invalid DamageType"

-- CheckResult JSON instances
instance ToJSON CheckResult where
    toJSON DT.Success = "Success"
    toJSON DT.Failure = "Failure"

instance FromJSON CheckResult where
    parseJSON = withText "CheckResult" $ \t -> case t of
        "Success" -> pure DT.Success
        "Failure" -> pure DT.Failure
        _         -> fail "Invalid CheckResult"

-- Proficiency JSON instances
instance ToJSON Proficiency where
    toJSON = toJSON . show

instance FromJSON Proficiency where
    parseJSON = withText "Proficiency" $ \t -> case T.unpack t of
        "NotProficient" -> pure NotProficient
        "Proficient"    -> pure Proficient
        "Expertise"     -> pure Expertise
        _               -> fail "Invalid Proficiency"

-- Weapon JSON instances
instance ToJSON Weapon where
    toJSON w = object
        [ "name"        .= weaponName w
        , "range"       .= weaponRange w
        , "damageType"  .= weaponDamageType w
        , "damageDice"  .= weaponDamageDice w
        , "damageCount" .= weaponDamageCount w
        ]

instance FromJSON Weapon where
    parseJSON = withObject "Weapon" $ \v -> Weapon
        <$> v .: "name"
        <*> v .: "range"
        <*> v .: "damageType"
        <*> v .: "damageDice"
        <*> v .: "damageCount"

-- StatBlock JSON instances
instance ToJSON StatBlock where
    toJSON sb = object
        [ "attributes"       .= M.toList (attributes sb)
        , "savingThrowProfs" .= savingThrowProfs sb
        , "skillProfs"       .= M.toList (skillProfs sb)
        , "proficiencyBonus" .= proficiencyBonus sb
        , "armorClass"       .= armorClass sb
        , "speed"            .= speed sb
        , "initiative"       .= initiative sb
        , "weapons"          .= weapons sb
        ]

instance FromJSON StatBlock where
    parseJSON = withObject "StatBlock" $ \v -> StatBlock
        <$> (M.fromList <$> v .: "attributes")
        <*> v .: "savingThrowProfs"
        <*> (M.fromList <$> v .: "skillProfs")
        <*> v .: "proficiencyBonus"
        <*> v .: "armorClass"
        <*> v .: "speed"
        <*> v .: "initiative"
        <*> v .: "weapons"

-- Item JSON instances
instance ToJSON Item where
    toJSON i = object
        [ "id"          .= itemId i
        , "name"        .= itemName i
        , "description" .= itemDescription i
        ]

instance FromJSON Item where
    parseJSON = withObject "Item" $ \v -> Item
        <$> v .: "id"
        <*> v .: "name"
        <*> v .: "description"

-- Clue JSON instances
instance ToJSON Clue where
    toJSON c = object
        [ "id"          .= clueId c
        , "name"        .= clueName c
        , "description" .= clueDescription c
        ]

instance FromJSON Clue where
    parseJSON = withObject "Clue" $ \v -> Clue
        <$> v .: "id"
        <*> v .: "name"
        <*> v .: "description"

-- PlayerCharacter JSON instances
instance ToJSON PlayerCharacter where
    toJSON pc = object
        [ "currentHP"  .= pcCurrentHP pc
        , "maxHP"      .= pcMaxHP pc
        , "statBlock"  .= pcStatBlock pc
        , "inventory"  .= pcInventory pc
        , "equipment"  .= pcEquipment pc
        , "clues"      .= S.toList (pcClues pc)
        ]

instance FromJSON PlayerCharacter where
    parseJSON = withObject "PlayerCharacter" $ \v -> PlayerCharacter
        <$> v .: "currentHP"
        <*> v .: "maxHP"
        <*> v .: "statBlock"
        <*> v .: "inventory"
        <*> v .: "equipment"
        <*> (S.fromList <$> v .: "clues")

-- Enemy JSON instances
instance ToJSON Enemy where
    toJSON e = object
        [ "name"      .= enemyName e
        , "currentHP" .= enemyCurrentHP e
        , "maxHP"     .= enemyMaxHP e
        , "statBlock" .= enemyStatBlock e
        ]

instance FromJSON Enemy where
    parseJSON = withObject "Enemy" $ \v -> Enemy
        <$> v .: "name"
        <*> v .: "currentHP"
        <*> v .: "maxHP"
        <*> v .: "statBlock"

-- OptionOutcome JSON instances
instance ToJSON OptionOutcome where
    toJSON (GoToEntry eid) = object
        [ "type"  .= ("GoToEntry" :: String)
        , "entry" .= eid
        ]
    toJSON (SkillCheck skill dc success failure) = object
        [ "type"    .= ("SkillCheck" :: String)
        , "skill"   .= skill
        , "dc"      .= dc
        , "success" .= success
        , "failure" .= failure
        ]
    toJSON (SaveCheck attr dc success failure) = object
        [ "type"      .= ("SaveCheck" :: String)
        , "attribute" .= attr
        , "dc"        .= dc
        , "success"   .= success
        , "failure"   .= failure
        ]
    toJSON (StartCombat enemies victory defeat) = object
        [ "type"    .= ("StartCombat" :: String)
        , "enemies" .= enemies
        , "victory" .= victory
        , "defeat"  .= defeat
        ]

instance FromJSON OptionOutcome where
    parseJSON = withObject "OptionOutcome" $ \v -> do
        t <- v .: "type" :: Parser String
        case t of
            "GoToEntry"   -> GoToEntry <$> v .: "entry"
            "SkillCheck"  -> SkillCheck <$> v .: "skill" <*> v .: "dc" <*> v .: "success" <*> v .: "failure"
            "SaveCheck"   -> SaveCheck <$> v .: "attribute" <*> v .: "dc" <*> v .: "success" <*> v .: "failure"
            "StartCombat" -> StartCombat <$> v .: "enemies" <*> v .: "victory" <*> v .: "defeat"
            _             -> fail "Invalid OptionOutcome type"

-- Option JSON instances
instance ToJSON Option where
    toJSON o = object
        [ "id"          .= optionId o
        , "description" .= optionDescription o
        , "outcome"     .= optionOutcome o
        ]

instance FromJSON Option where
    parseJSON = withObject "Option" $ \v -> Option
        <$> v .: "id"
        <*> v .: "description"
        <*> v .: "outcome"

-- Serializable condition representation
data ConditionData
    = HasClue ClueId
    | HasItem ItemId
    | VisitedEntry EntryId
    | AndCond ConditionData ConditionData
    | OrCond ConditionData ConditionData
    | NotCond ConditionData
    | Always
    deriving (Show, Eq, Generic)

instance ToJSON ConditionData
instance FromJSON ConditionData

-- Convert ConditionData to actual Condition function
toCondition :: ConditionData -> Condition
toCondition (HasClue cid)     = hasClueCondition cid
toCondition (HasItem iid)     = hasItemCondition iid
toCondition (VisitedEntry eid) = visitedEntry eid
toCondition (AndCond c1 c2)   = andCondition (toCondition c1) (toCondition c2)
toCondition (OrCond c1 c2)    = orCondition (toCondition c1) (toCondition c2)
toCondition (NotCond c)       = notCondition (toCondition c)
toCondition Always            = alwaysTrue

-- RuleEffect JSON instances (already has Show/Eq)
instance ToJSON RuleEffect where
    toJSON (AddOption opt)       = object ["type" .= ("AddOption" :: String), "option" .= opt]
    toJSON (RemoveOption oid)    = object ["type" .= ("RemoveOption" :: String), "optionId" .= oid]
    toJSON (ModifyNarrative txt) = object ["type" .= ("ModifyNarrative" :: String), "text" .= txt]

instance FromJSON RuleEffect where
    parseJSON = withObject "RuleEffect" $ \v -> do
        t <- v .: "type" :: Parser String
        case t of
            "AddOption"       -> AddOption <$> v .: "option"
            "RemoveOption"    -> RemoveOption <$> v .: "optionId"
            "ModifyNarrative" -> ModifyNarrative <$> v .: "text"
            _                 -> fail "Invalid RuleEffect type"

-- Serializable Rule representation
data RuleData = RuleData
    { ruleDataCondition :: ConditionData
    , ruleDataEffect    :: RuleEffect
    } deriving (Show, Eq, Generic)

instance ToJSON RuleData
instance FromJSON RuleData

-- Convert RuleData to Rule
toRule :: RuleData -> Rule
toRule rd = Rule (toCondition $ ruleDataCondition rd) (ruleDataEffect rd)

-- Serializable Entry representation
data EntryData = EntryData
    { entryDataId        :: EntryId
    , entryDataNarrative :: String
    , entryDataOptions   :: [Option]
    , entryDataRules     :: [RuleData]
    } deriving (Show, Eq, Generic)

instance ToJSON EntryData
instance FromJSON EntryData

-- Convert EntryData to Entry
toEntry :: EntryData -> Entry
toEntry ed = Entry
    { entryId        = entryDataId ed
    , entryNarrative = entryDataNarrative ed
    , entryOptions   = entryDataOptions ed
    , entryRules     = map toRule (entryDataRules ed)
    }

-- Convert Entry to EntryData (for saving) - requires ConditionData
-- This needs the original ConditionData, so we keep EntryData for persistence