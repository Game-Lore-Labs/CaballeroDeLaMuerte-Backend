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

-- Weapon DTO

data WeaponDTO = WeaponDTO
    { weaponDtoIndex      :: Int
    , weaponDtoName       :: String
    , weaponDtoDamageDice :: String
    , weaponDtoDamageType :: String
    , weaponDtoRange      :: (Int, Int)
    } deriving (Show, Eq, Generic)

instance ToJSON WeaponDTO where
    toJSON w = object
        [ "index"      .= weaponDtoIndex w
        , "name"       .= weaponDtoName w
        , "damageDice" .= weaponDtoDamageDice w
        , "damageType" .= weaponDtoDamageType w
        , "range"      .= weaponDtoRange w
        ]

instance FromJSON WeaponDTO where
    parseJSON = withObject "WeaponDTO" $ \v -> WeaponDTO
        <$> v .: "index"
        <*> v .: "name"
        <*> v .: "damageDice"
        <*> v .: "damageType"
        <*> v .: "range"

fromWeapon :: Int -> Weapon -> WeaponDTO
fromWeapon idx w = WeaponDTO
    { weaponDtoIndex      = idx
    , weaponDtoName       = weaponName w
    , weaponDtoDamageDice = show (weaponDamageCount w) ++ "d" ++ drop 1 (show (weaponDamageDice w))
    , weaponDtoDamageType = show (weaponDamageType w)
    , weaponDtoRange      = weaponRange w
    }

-- Character DTOs

data CharacterDTO = CharacterDTO
    { charDtoName      :: String
    , charDtoCurrentHP :: Int
    , charDtoMaxHP     :: Int
    , charDtoAC        :: Int
    , charDtoWeapons   :: [WeaponDTO]
    , charDtoInventory :: [ItemDTO]
    , charDtoEquipment :: [ItemDTO]
    , charDtoClues     :: [ClueId]
    } deriving (Show, Eq, Generic)

instance ToJSON CharacterDTO where
    toJSON c = object
        [ "name"      .= charDtoName c
        , "currentHP" .= charDtoCurrentHP c
        , "maxHP"     .= charDtoMaxHP c
        , "ac"        .= charDtoAC c
        , "weapons"   .= charDtoWeapons c
        , "inventory" .= charDtoInventory c
        , "equipment" .= charDtoEquipment c
        , "clues"     .= charDtoClues c
        ]

instance FromJSON CharacterDTO where
    parseJSON = withObject "CharacterDTO" $ \v -> CharacterDTO
        <$> v .: "name"
        <*> v .: "currentHP"
        <*> v .: "maxHP"
        <*> v .: "ac"
        <*> v .: "weapons"
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
    { charDtoName      = pcName pc
    , charDtoCurrentHP = pcCurrentHP pc
    , charDtoMaxHP     = pcMaxHP pc
    , charDtoAC        = armorClass (pcStatBlock pc)
    , charDtoWeapons   = zipWith fromWeapon [0..] (weapons (pcStatBlock pc))
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

-- | Request to start combat with specific enemies
data StartCombatRequest = StartCombatRequest
    { startCombatEnemies :: [String]  -- Enemy names to spawn
    , startCombatVictory :: EntryId   -- Entry to go on victory
    , startCombatDefeat  :: EntryId   -- Entry to go on defeat
    } deriving (Show, Eq, Generic)

instance ToJSON StartCombatRequest where
    toJSON r = object
        [ "enemies" .= startCombatEnemies r
        , "victoryEntry" .= startCombatVictory r
        , "defeatEntry" .= startCombatDefeat r
        ]

instance FromJSON StartCombatRequest where
    parseJSON = withObject "StartCombatRequest" $ \v -> StartCombatRequest
        <$> v .: "enemies"
        <*> v .: "victoryEntry"
        <*> v .: "defeatEntry"

-- Response DTOs

data OptionResultDTO = OptionResultDTO
    { resultType        :: String
    , resultRoll        :: Maybe Int
    , resultDC          :: Maybe Int
    , resultDestination :: Maybe EntryId
    , resultSkill       :: Maybe String
    , resultAttribute   :: Maybe String
    , resultEffects     :: [String]      -- Effects that were applied
    } deriving (Show, Eq, Generic)

instance ToJSON OptionResultDTO where
    toJSON r = object
        [ "type"        .= resultType r
        , "roll"        .= resultRoll r
        , "dc"          .= resultDC r
        , "destination" .= resultDestination r
        , "skill"       .= resultSkill r
        , "attribute"   .= resultAttribute r
        , "effects"     .= resultEffects r
        ]

instance FromJSON OptionResultDTO where
    parseJSON = withObject "OptionResultDTO" $ \v -> OptionResultDTO
        <$> v .: "type"
        <*> v .:? "roll"
        <*> v .:? "dc"
        <*> v .:? "destination"
        <*> v .:? "skill"
        <*> v .:? "attribute"
        <*> v .:? "effects" .!= []

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

-- | Result of a combat turn including actions and updated status
data CombatTurnResultDTO = CombatTurnResultDTO
    { turnActions :: [CombatActionDTO]
    , turnStatus  :: CombatStatusDTO
    } deriving (Show, Eq, Generic)

instance ToJSON CombatTurnResultDTO where
    toJSON t = object
        [ "actions" .= turnActions t
        , "status"  .= turnStatus t
        ]

instance FromJSON CombatTurnResultDTO where
    parseJSON = withObject "CombatTurnResultDTO" $ \v -> CombatTurnResultDTO
        <$> v .: "actions"
        <*> v .: "status"

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

-- Character Sheet DTOs (Complete character information)

-- | Basic character information
data BasicInfoDTO = BasicInfoDTO
    { biClass       :: String
    , biLevel       :: Int
    , biRace        :: String
    , biBackground  :: String
    , biAlignment   :: String
    , biExperience  :: Int
    } deriving (Show, Eq, Generic)

instance ToJSON BasicInfoDTO where
    toJSON bi = object
        [ "class"       .= biClass bi
        , "level"       .= biLevel bi
        , "race"        .= biRace bi
        , "background"  .= biBackground bi
        , "alignment"   .= biAlignment bi
        , "experience"  .= biExperience bi
        ]

instance FromJSON BasicInfoDTO where
    parseJSON = withObject "BasicInfoDTO" $ \v -> BasicInfoDTO
        <$> v .: "class"
        <*> v .: "level"
        <*> v .: "race"
        <*> v .: "background"
        <*> v .: "alignment"
        <*> v .: "experience"

-- | Single attribute with score, modifier, and saving throw
data AttributeInfoDTO = AttributeInfoDTO
    { aiName        :: String
    , aiScore       :: Int
    , aiModifier    :: Int
    , aiSavingThrow :: Int
    } deriving (Show, Eq, Generic)

instance ToJSON AttributeInfoDTO where
    toJSON ai = object
        [ "name"        .= aiName ai
        , "score"       .= aiScore ai
        , "modifier"    .= aiModifier ai
        , "savingThrow" .= aiSavingThrow ai
        ]

instance FromJSON AttributeInfoDTO where
    parseJSON = withObject "AttributeInfoDTO" $ \v -> AttributeInfoDTO
        <$> v .: "name"
        <*> v .: "score"
        <*> v .: "modifier"
        <*> v .: "savingThrow"

-- | All attributes grouped
data AttributesDTO = AttributesDTO
    { attrStrength     :: AttributeInfoDTO
    , attrDexterity    :: AttributeInfoDTO
    , attrConstitution :: AttributeInfoDTO
    , attrIntelligence :: AttributeInfoDTO
    , attrWisdom       :: AttributeInfoDTO
    , attrCharisma     :: AttributeInfoDTO
    } deriving (Show, Eq, Generic)

instance ToJSON AttributesDTO where
    toJSON a = object
        [ "strength"     .= attrStrength a
        , "dexterity"    .= attrDexterity a
        , "constitution" .= attrConstitution a
        , "intelligence" .= attrIntelligence a
        , "wisdom"       .= attrWisdom a
        , "charisma"     .= attrCharisma a
        ]

instance FromJSON AttributesDTO where
    parseJSON = withObject "AttributesDTO" $ \v -> AttributesDTO
        <$> v .: "strength"
        <*> v .: "dexterity"
        <*> v .: "constitution"
        <*> v .: "intelligence"
        <*> v .: "wisdom"
        <*> v .: "charisma"

-- | Combat statistics
data CombatStatsDTO = CombatStatsDTO
    { csInitiative        :: Int
    , csCurrentHP         :: Int
    , csMaxHP             :: Int
    , csHitDice           :: String
    , csArmorClass        :: Int
    , csSpeed             :: Int
    , csProficiencyBonus  :: Int
    } deriving (Show, Eq, Generic)

instance ToJSON CombatStatsDTO where
    toJSON cs = object
        [ "initiative"       .= csInitiative cs
        , "currentHP"        .= csCurrentHP cs
        , "maxHP"            .= csMaxHP cs
        , "hitDice"          .= csHitDice cs
        , "armorClass"       .= csArmorClass cs
        , "speed"            .= csSpeed cs
        , "proficiencyBonus" .= csProficiencyBonus cs
        ]

instance FromJSON CombatStatsDTO where
    parseJSON = withObject "CombatStatsDTO" $ \v -> CombatStatsDTO
        <$> v .: "initiative"
        <*> v .: "currentHP"
        <*> v .: "maxHP"
        <*> v .: "hitDice"
        <*> v .: "armorClass"
        <*> v .: "speed"
        <*> v .: "proficiencyBonus"

-- | Single skill with name and bonus
data SkillInfoDTO = SkillInfoDTO
    { siName  :: String
    , siBonus :: Int
    } deriving (Show, Eq, Generic)

instance ToJSON SkillInfoDTO where
    toJSON si = object
        [ "name"  .= siName si
        , "bonus" .= siBonus si
        ]

instance FromJSON SkillInfoDTO where
    parseJSON = withObject "SkillInfoDTO" $ \v -> SkillInfoDTO
        <$> v .: "name"
        <*> v .: "bonus"

-- | All skills grouped by attribute
data SkillsDTO = SkillsDTO
    { skillAthletics      :: SkillInfoDTO
    , skillAcrobatics     :: SkillInfoDTO
    , skillSleightOfHand  :: SkillInfoDTO
    , skillStealth        :: SkillInfoDTO
    , skillArcana         :: SkillInfoDTO
    , skillHistory        :: SkillInfoDTO
    , skillInvestigation  :: SkillInfoDTO
    , skillNature         :: SkillInfoDTO
    , skillReligion       :: SkillInfoDTO
    , skillAnimalHandling :: SkillInfoDTO
    , skillInsight        :: SkillInfoDTO
    , skillMedicine       :: SkillInfoDTO
    , skillPerception     :: SkillInfoDTO
    , skillSurvival       :: SkillInfoDTO
    , skillDeception      :: SkillInfoDTO
    , skillIntimidation   :: SkillInfoDTO
    , skillPerformance    :: SkillInfoDTO
    , skillPersuasion     :: SkillInfoDTO
    } deriving (Show, Eq, Generic)

instance ToJSON SkillsDTO where
    toJSON s = object
        [ "athletics"      .= skillAthletics s
        , "acrobatics"     .= skillAcrobatics s
        , "sleightOfHand"  .= skillSleightOfHand s
        , "stealth"        .= skillStealth s
        , "arcana"         .= skillArcana s
        , "history"        .= skillHistory s
        , "investigation"  .= skillInvestigation s
        , "nature"         .= skillNature s
        , "religion"       .= skillReligion s
        , "animalHandling" .= skillAnimalHandling s
        , "insight"        .= skillInsight s
        , "medicine"       .= skillMedicine s
        , "perception"     .= skillPerception s
        , "survival"       .= skillSurvival s
        , "deception"      .= skillDeception s
        , "intimidation"   .= skillIntimidation s
        , "performance"    .= skillPerformance s
        , "persuasion"     .= skillPersuasion s
        ]

instance FromJSON SkillsDTO where
    parseJSON = withObject "SkillsDTO" $ \v -> SkillsDTO
        <$> v .: "athletics"
        <*> v .: "acrobatics"
        <*> v .: "sleightOfHand"
        <*> v .: "stealth"
        <*> v .: "arcana"
        <*> v .: "history"
        <*> v .: "investigation"
        <*> v .: "nature"
        <*> v .: "religion"
        <*> v .: "animalHandling"
        <*> v .: "insight"
        <*> v .: "medicine"
        <*> v .: "perception"
        <*> v .: "survival"
        <*> v .: "deception"
        <*> v .: "intimidation"
        <*> v .: "performance"
        <*> v .: "persuasion"

-- | Complete character sheet DTO
data CharacterSheetDTO = CharacterSheetDTO
    { sheetBasicInfo   :: BasicInfoDTO
    , sheetAttributes  :: AttributesDTO
    , sheetCombatStats :: CombatStatsDTO
    , sheetSkills      :: SkillsDTO
    , sheetInventory   :: [ItemDTO]
    , sheetEquipment   :: [ItemDTO]
    , sheetClues       :: [ClueId]
    } deriving (Show, Eq, Generic)

instance ToJSON CharacterSheetDTO where
    toJSON sheet = object
        [ "basicInfo"   .= sheetBasicInfo sheet
        , "attributes"  .= sheetAttributes sheet
        , "combatStats" .= sheetCombatStats sheet
        , "skills"      .= sheetSkills sheet
        , "inventory"   .= sheetInventory sheet
        , "equipment"   .= sheetEquipment sheet
        , "clues"       .= sheetClues sheet
        ]

instance FromJSON CharacterSheetDTO where
    parseJSON = withObject "CharacterSheetDTO" $ \v -> CharacterSheetDTO
        <$> v .: "basicInfo"
        <*> v .: "attributes"
        <*> v .: "combatStats"
        <*> v .: "skills"
        <*> v .: "inventory"
        <*> v .: "equipment"
        <*> v .: "clues"

-- | Build character sheet from PlayerCharacter
fromCharacterSheet :: PlayerCharacter -> CharacterSheetDTO
fromCharacterSheet pc = CharacterSheetDTO
    { sheetBasicInfo   = basicInfo
    , sheetAttributes  = attributesDto
    , sheetCombatStats = combatStats
    , sheetSkills      = skillsDto
    , sheetInventory   = map fromItem (pcInventory pc)
    , sheetEquipment   = map fromItem (pcEquipment pc)
    , sheetClues       = S.toList (pcClues pc)
    }
  where
    sb = pcStatBlock pc
    
    -- Basic character info (fixed for this adventure)
    basicInfo = BasicInfoDTO
        { biClass      = "Pícaro (Rogue)"
        , biLevel      = 2
        , biRace       = "Humano"
        , biBackground = "Investigador"
        , biAlignment  = "Neutral"
        , biExperience = 0
        }
    
    -- Helper to build attribute info
    makeAttrInfo :: String -> Attribute -> AttributeInfoDTO
    makeAttrInfo name attr = AttributeInfoDTO
        { aiName        = name
        , aiScore       = getAttributeScore sb attr
        , aiModifier    = getAttributeModifier sb attr
        , aiSavingThrow = getSavingThrowBonus sb attr
        }
    
    attributesDto = AttributesDTO
        { attrStrength     = makeAttrInfo "Fuerza" Strength
        , attrDexterity    = makeAttrInfo "Destreza" Dexterity
        , attrConstitution = makeAttrInfo "Constitución" Constitution
        , attrIntelligence = makeAttrInfo "Inteligencia" Intelligence
        , attrWisdom       = makeAttrInfo "Sabiduría" Wisdom
        , attrCharisma     = makeAttrInfo "Carisma" Charisma
        }
    
    combatStats = CombatStatsDTO
        { csInitiative       = initiative sb
        , csCurrentHP        = pcCurrentHP pc
        , csMaxHP            = pcMaxHP pc
        , csHitDice          = "2d8"
        , csArmorClass       = armorClass sb
        , csSpeed            = speed sb
        , csProficiencyBonus = proficiencyBonus sb
        }
    
    -- Helper to build skill info
    makeSkillInfo :: String -> Skill -> SkillInfoDTO
    makeSkillInfo name skill = SkillInfoDTO
        { siName  = name
        , siBonus = getSkillBonus sb skill
        }
    
    skillsDto = SkillsDTO
        { skillAthletics      = makeSkillInfo "Atletismo" Athletics
        , skillAcrobatics     = makeSkillInfo "Acrobacias" Acrobatics
        , skillSleightOfHand  = makeSkillInfo "Juego de Manos" SleightOfHand
        , skillStealth        = makeSkillInfo "Sigilo" Stealth
        , skillArcana         = makeSkillInfo "Arcanismo" Arcana
        , skillHistory        = makeSkillInfo "Historia" History
        , skillInvestigation  = makeSkillInfo "Investigación" Investigation
        , skillNature         = makeSkillInfo "Naturaleza" Nature
        , skillReligion       = makeSkillInfo "Religión" Religion
        , skillAnimalHandling = makeSkillInfo "Trato con Animales" AnimalHandling
        , skillInsight        = makeSkillInfo "Perspicacia" Insight
        , skillMedicine       = makeSkillInfo "Medicina" Medicine
        , skillPerception     = makeSkillInfo "Percepción" Perception
        , skillSurvival       = makeSkillInfo "Supervivencia" Survival
        , skillDeception      = makeSkillInfo "Engaño" Deception
        , skillIntimidation   = makeSkillInfo "Intimidación" Intimidation
        , skillPerformance    = makeSkillInfo "Interpretación" Performance
        , skillPersuasion     = makeSkillInfo "Persuasión" Persuasion
        }
