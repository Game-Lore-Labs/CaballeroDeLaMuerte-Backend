# Technical Manual - RPG Bot Backend

## El Escudero del Caballero de la Muerte

**Version:** 1.1.0  
**Language:** Haskell  
**Framework:** Scotty (Web), Aeson (JSON)  
**Last Updated:** January 2026

---

## Table of Contents

1. [Overview](#1-overview)
2. [Architecture](#2-architecture)
3. [Project Structure](#3-project-structure)
4. [Domain Layer](#4-domain-layer)
5. [Infrastructure Layer](#5-infrastructure-layer)
6. [Application Layer](#6-application-layer)
7. [API Layer](#7-api-layer)
8. [Data Models](#8-data-models)
9. [Game Mechanics](#9-game-mechanics)
10. [Effects System](#10-effects-system)
11. [Combat System](#11-combat-system)
12. [Configuration](#12-configuration)
13. [Building and Running](#13-building-and-running)
14. [Extending the System](#14-extending-the-system)

---

## 1. Overview

### 1.1 Purpose

This backend system powers an interactive RPG adventure game based on a subset of Dungeons & Dragons 5th Edition rules. The game follows a "choose your own adventure" structure where players navigate through numbered entries, make choices, and resolve skill checks and combat encounters.

### 1.2 Key Features

- **Entry-based Navigation**: Adventure structured as interconnected entries (scenes)
- **D20 System**: Skill checks and saving throws using D&D 5e mechanics
- **Combat System**: Turn-based combat with multiple enemies
- **Effects System**: Automatic character updates based on entry consequences
- **State Management**: Player character, inventory, equipment, clues, and history tracking
- **Conditional Rules**: Dynamic options based on game state
- **RESTful API**: HTTP endpoints for frontend integration
- **JSON Persistence**: Save/load/reset game functionality

### 1.3 Technology Stack

| Component | Technology |
|-----------|------------|
| Language | Haskell (GHC 9.x) |
| Build Tool | Stack |
| Web Framework | Scotty |
| JSON | Aeson |
| Randomness | System.Random |
| CORS | wai-cors |

---

## 2. Architecture

### 2.1 Layered Architecture

The system follows a clean layered architecture pattern:

```
┌─────────────────────────────────────────┐
│              API Layer                   │
│  (Routes, DTOs, Swagger Documentation)   │
├─────────────────────────────────────────┤
│           Application Layer              │
│  (Services, Use Cases, Orchestration)    │
├─────────────────────────────────────────┤
│         Infrastructure Layer             │
│  (JSON Serialization, Repositories)      │
├─────────────────────────────────────────┤
│             Domain Layer                 │
│  (Entities, Business Logic, Rules)       │
└─────────────────────────────────────────┘
```

### 2.2 Layer Responsibilities

#### Domain Layer
- Core business entities (PlayerCharacter, Entry, Enemy)
- Game rules and mechanics (dice rolling, skill checks)
- Pure functions with no external dependencies

#### Infrastructure Layer
- JSON serialization/deserialization (Aeson instances)
- Repository pattern for data persistence
- File system operations

#### Application Layer
- Use case orchestration
- Service coordination
- State management logic

#### API Layer
- HTTP endpoint definitions
- Request/Response DTOs
- Input validation
- OpenAPI documentation

### 2.3 Data Flow

```
HTTP Request
     │
     ▼
┌─────────┐    ┌─────────────┐    ┌──────────────┐    ┌────────┐
│ Routes  │───▶│ Application │───▶│Infrastructure│───▶│ Domain │
│  (API)  │    │  Services   │    │ Repositories │    │ Logic  │
└─────────┘    └─────────────┘    └──────────────┘    └────────┘
     │                                    │
     ▼                                    ▼
HTTP Response                      JSON Files
```

---

## 3. Project Structure

```
src/
├── Main.hs                      # Application entry point
├── Domain/
│   ├── Types.hs                 # Core type definitions
│   ├── Dice.hs                  # Dice rolling mechanics
│   ├── StatBlock.hs             # Character statistics
│   ├── Character.hs             # Player character entity
│   ├── Enemy.hs                 # Enemy entity and combat
│   ├── Entry.hs                 # Adventure entries and rules
│   └── Effects.hs               # Entry effects definitions
├── Infrastructure/
│   ├── JSON.hs                  # Aeson instances
│   └── Repository.hs            # Data persistence
├── Application/
│   ├── GameService.hs           # Game state management
│   ├── AdventureService.hs      # Entry navigation
│   ├── CombatService.hs         # Combat encounters
│   ├── CharacterService.hs      # Character operations
│   └── EffectsService.hs        # Apply entry effects
└── API/
    ├── DTO.hs                   # Data Transfer Objects
    ├── Routes.hs                # HTTP endpoints
    └── Swagger.hs               # OpenAPI specification

data/                            # Runtime data (JSON files)
├── save.json                    # Current game save file
├── initial-state.json           # Immutable initial state (for reset)
├── entries.json                 # Adventure entries with effects
└── enemies.json                 # Enemy templates

docs/                            # Documentation
├── TECHNICAL_MANUAL.md
└── USER_MANUAL.md
```

---

## 4. Domain Layer

### 4.1 Types (Domain/Types.hs)

#### Attributes
The six D&D attributes:

```haskell
data Attribute = Strength | Dexterity | Constitution
               | Intelligence | Wisdom | Charisma
```

#### Skills
18 skills mapped to their governing attributes:

```haskell
data Skill
    = Athletics           -- STR
    | Acrobatics          -- DEX
    | SleightOfHand       -- DEX
    | Stealth             -- DEX
    | Arcana              -- INT
    | History             -- INT
    | Investigation       -- INT
    | Nature              -- INT
    | Religion            -- INT
    | AnimalHandling      -- WIS
    | Insight             -- WIS
    | Medicine            -- WIS
    | Perception          -- WIS
    | Survival            -- WIS
    | Deception           -- CHA
    | Intimidation        -- CHA
    | Performance         -- CHA
    | Persuasion          -- CHA
```

#### Dice Types

```haskell
data DiceType = D2 | D4 | D6 | D8 | D10 | D12 | D20 | D100
```

#### Type Aliases

```haskell
type ItemId  = String    -- Item identifier
type ClueId  = String    -- Clue identifier
type EntryId = Int       -- Entry number
```

### 4.2 Dice System (Domain/Dice.hs)

#### RollResult

```haskell
data RollResult = RollResult
    { rollValues :: [Int]  -- Individual dice results
    , rollBonus  :: Int    -- Flat bonus applied
    , rollTotal  :: Int    -- Sum of values + bonus
    }
```

#### Key Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `roll` | `DiceType -> Int -> Int -> IO RollResult` | Roll N dice with bonus |
| `rollSingle` | `DiceType -> IO Int` | Roll single die |
| `rollD20` | `IO Int` | Roll d20 (convenience) |
| `rollCheck` | `Int -> Int -> IO (Int, Bool)` | Roll with bonus vs DC |

### 4.3 StatBlock (Domain/StatBlock.hs)

#### Proficiency Levels

```haskell
data Proficiency = NotProficient | Proficient | Expertise
```

#### Weapon Definition

```haskell
data Weapon = Weapon
    { weaponName        :: String
    , weaponRange       :: (Int, Int)    -- (normal, long) range
    , weaponDamageType  :: DamageType
    , weaponDamageDice  :: DiceType
    , weaponDamageCount :: Int           -- Number of dice
    }
```

#### StatBlock Structure

```haskell
data StatBlock = StatBlock
    { attributes          :: Map Attribute Int
    , savingThrowProfs    :: [Attribute]
    , skillProfs          :: Map Skill Proficiency
    , proficiencyBonus    :: Int
    , armorClass          :: Int
    , speed               :: Int
    , initiative          :: Int
    , weapons             :: [Weapon]
    }
```

#### Modifier Calculations

```haskell
-- Attribute modifier: (score - 10) / 2
attributeModifier :: Int -> Int

-- Skill bonus = attribute modifier + proficiency
getSkillBonus :: StatBlock -> Skill -> Int

-- Save bonus = attribute modifier + proficiency (if proficient)
getSavingThrowBonus :: StatBlock -> Attribute -> Int
```

### 4.4 Character (Domain/Character.hs)

#### Item and Clue

```haskell
data Item = Item
    { itemId          :: ItemId
    , itemName        :: String
    , itemDescription :: String
    }

data Clue = Clue
    { clueId          :: ClueId
    , clueName        :: String
    , clueDescription :: String
    }
```

#### PlayerCharacter

```haskell
data PlayerCharacter = PlayerCharacter
    { pcCurrentHP  :: Int
    , pcMaxHP      :: Int
    , pcStatBlock  :: StatBlock
    , pcInventory  :: [Item]        -- Carried items
    , pcEquipment  :: [Item]        -- Equipped items
    , pcClues      :: Set ClueId    -- Discovered clues
    }
```

#### Character Actions

| Function | Description |
|----------|-------------|
| `abilityCheck` | Perform skill check vs DC |
| `savingThrow` | Perform saving throw vs DC |
| `attack` | Attack with weapon vs AC |
| `takeDamage` | Reduce HP (min 0) |
| `heal` | Increase HP (max maxHP) |
| `addItem` / `removeItem` | Inventory management |
| `addClue` | Add discovered clue |
| `hasClue` / `hasItem` | Query possession |

### 4.5 Enemy (Domain/Enemy.hs)

```haskell
data Enemy = Enemy
    { enemyName      :: String
    , enemyCurrentHP :: Int
    , enemyMaxHP     :: Int
    , enemyStatBlock :: StatBlock
    }

type Combat = [Enemy]  -- Group of enemies
```

### 4.6 Entry System (Domain/Entry.hs)

#### GameState

```haskell
data GameState = GameState
    { gsPlayer       :: PlayerCharacter
    , gsCurrentEntry :: EntryId
    , gsHistory      :: [EntryId]      -- Previously visited
    }
```

#### Options and Outcomes

```haskell
data OptionOutcome
    = GoToEntry EntryId                        -- Direct navigation
    | SkillCheck Skill Int EntryId EntryId     -- skill, DC, success, failure
    | SaveCheck Attribute Int EntryId EntryId  -- attr, DC, success, failure
    | StartCombat EntryId EntryId              -- victory, defeat

data Option = Option
    { optionId          :: Int
    , optionDescription :: String
    , optionOutcome     :: OptionOutcome
    }
```

#### Conditional Rules

```haskell
type Condition = GameState -> Bool

data RuleEffect
    = AddOption Option
    | RemoveOption Int
    | ModifyNarrative String

data Rule = Rule
    { ruleCondition :: Condition
    , ruleEffect    :: RuleEffect
    }
```

#### Condition Constructors

```haskell
hasClueCondition :: ClueId -> Condition
hasItemCondition :: ItemId -> Condition
visitedEntry     :: EntryId -> Condition
andCondition     :: Condition -> Condition -> Condition
orCondition      :: Condition -> Condition -> Condition
notCondition     :: Condition -> Condition
alwaysTrue       :: Condition
```

#### Entry Structure

```haskell
data Entry = Entry
    { entryId        :: EntryId
    , entryNarrative :: String
    , entryOptions   :: [Option]
    , entryRules     :: [Rule]
    , entryEffects   :: EntryEffects  -- Effects applied when entering
    }
```

### 4.7 Effects System (Domain/Effects.hs)

The effects system defines consequences that are automatically applied when a player enters an entry.

#### Effect Types

```haskell
-- | Effect on character stats (HP, AC)
data StatEffect = StatEffect
    { statEffectName     :: String  -- "currentHP", "maxHP", "armorClass"
    , statEffectModifier :: String  -- Number or dice expression: "5", "-1d6"
    }

-- | Effect on equipment (weapons, armor, magic items)
data EquipmentEffect = EquipmentEffect
    { equipmentEffectName     :: String        -- Item name
    , equipmentEffectType     :: String        -- "weapon", "armor", "wondrous"
    , equipmentEffectBonus    :: Maybe String  -- Bonus description
    , equipmentEffectQuantity :: Maybe Int     -- Quantity (default 1)
    }

-- | Effect on inventory (consumables, treasure)
data InventoryEffect = InventoryEffect
    { inventoryEffectName     :: String        -- Item name
    , inventoryEffectType     :: String        -- "currency", "potion", "scroll"
    , inventoryEffectQuantity :: Maybe Int     -- Quantity (negative to remove)
    , inventoryEffectValue    :: Maybe Int     -- Value in gold pieces
    }

-- | Effect on attributes
data AttributeEffect = AttributeEffect
    { attributeEffectName       :: String      -- "strength", "dexterity", etc
    , attributeEffectModifier   :: Maybe Int   -- Change to score
    , attributeEffectSavingThrow :: Maybe Int  -- Bonus to saving throw
    }

-- | Effect on skills
data SkillEffect = SkillEffect
    { skillEffectName     :: String            -- Skill name
    , skillEffectModifier :: Maybe Int         -- Bonus to checks
    , skillEffectProf     :: Maybe String      -- "Proficient", "Expertise"
    }

-- | Complete effects structure
data EntryEffects = EntryEffects
    { effectsStats      :: [StatEffect]
    , effectsEquipment  :: [EquipmentEffect]
    , effectsInventory  :: [InventoryEffect]
    , effectsAttributes :: [AttributeEffect]
    , effectsSkills     :: [SkillEffect]
    }
```

**Important**: Equipment items are always added to BOTH equipment AND inventory. Inventory is the complete list of possessions; equipment is the subset currently equipped.

---

## 5. Infrastructure Layer

### 5.1 JSON Serialization (Infrastructure/JSON.hs)

All domain types have `ToJSON` and `FromJSON` instances for Aeson serialization.

#### Serializable Condition Representation

Since Haskell functions cannot be serialized, conditions are stored as data:

```haskell
data ConditionData
    = HasClue ClueId
    | HasItem ItemId
    | VisitedEntry EntryId
    | AndCond ConditionData ConditionData
    | OrCond ConditionData ConditionData
    | NotCond ConditionData
    | Always

-- Convert to runtime function
toCondition :: ConditionData -> Condition
```

#### EntryData (Serializable Entry)

```haskell
data EntryData = EntryData
    { entryDataId        :: EntryId
    , entryDataNarrative :: String
    , entryDataOptions   :: [Option]
    , entryDataRules     :: [RuleData]
    }

toEntry :: EntryData -> Entry
```

### 5.2 Repository Pattern (Infrastructure/Repository.hs)

#### Result Type

```haskell
data RepoResult a = RepoSuccess a | RepoError String
```

#### Repository Functions

| Repository | Save | Load |
|------------|------|------|
| PlayerCharacter | `savePlayerCharacter` | `loadPlayerCharacter` |
| Entries | `saveEntries` | `loadEntries` |
| Enemies | `saveEnemies` | `loadEnemies` |
| GameState | `saveGame` | `loadGame` |

#### Store Types

```haskell
type EntryStore = Map EntryId EntryData
type EnemyStore = Map String Enemy
```

---

## 6. Application Layer

### 6.1 GameService (Application/GameService.hs)

#### Configuration

```haskell
data AppConfig = AppConfig
    { configSavePath         :: FilePath  -- "data/save.json"
    , configInitialStatePath :: FilePath  -- "data/initial-state.json"
    , configEntriesPath      :: FilePath  -- "data/entries.json"
    , configEnemiesPath      :: FilePath  -- "data/enemies.json"
    }
```

#### Application State

```haskell
data AppState = AppState
    { appGameState  :: GameState
    , appEntries    :: EntryStore
    , appEnemies    :: EnemyStore
    }
```

#### Service Functions

| Function | Description |
|----------|-------------|
| `newGame` | Initialize new game with player |
| `saveGameState` | Persist game to disk |
| `loadGameState` | Load game from disk |
| `loadInitialGameState` | Load initial state for reset |
| `resetGameToInitialState` | Reset game to initial state |
| `initializeApp` | Load all data files |
| `getPlayer` | Get current player |
| `getCurrentEntry` | Get current entry with rules evaluated |
| `getAvailableOptions` | Get options after rule evaluation |
| `getCurrentNarrative` | Get narrative with modifications |
| `updatePlayer` | Modify player state |
| `goToEntry` | Navigate to new entry |

### 6.2 AdventureService (Application/AdventureService.hs)

#### Option Result

```haskell
data OptionResult
    = NavigatedTo EntryId [String]               -- destination + effects applied
    | CheckPassed Skill Int Int EntryId [String] -- skill, roll, DC, dest, effects
    | CheckFailed Skill Int Int EntryId [String]
    | SavePassed Attribute Int Int EntryId [String]
    | SaveFailed Attribute Int Int EntryId [String]
    | CombatStarted [String] EntryId EntryId     -- enemies, victory, defeat
    | OptionNotFound
    | EntryNotFound
```

#### Key Functions

```haskell
selectOption :: Int -> AppState -> IO (OptionResult, AppState)
```

This function:
1. Finds the option by ID
2. Processes the outcome (navigation, check, combat)
3. **Applies entry effects automatically**
4. Returns the result (with effect messages) and updated state

```haskell
applyEffectsOnEntry :: EntryId -> AppState -> IO (AppState, [String])
```

Called internally when navigating to apply any effects defined in the entry.

### 6.5 EffectsService (Application/EffectsService.hs)

Handles applying entry effects to the player character.

#### Effects Result

```haskell
data EffectsResult = EffectsResult
    { effectsApplied    :: [String]        -- Messages describing changes
    , effectsNewPlayer  :: PlayerCharacter -- Updated character
    }
```

#### Key Functions

| Function | Description |
|----------|-------------|
| `applyEntryEffects` | Apply all effects from an entry |
| `applyEffects` | Apply an EntryEffects structure |
| `applyStatEffects` | Apply HP, AC modifications (supports dice) |
| `applyEquipmentEffects` | Add equipment (also adds to inventory) |
| `applyInventoryEffects` | Add/remove inventory items |
| `applyAttributeEffects` | Modify attributes or saving throws |
| `applySkillEffects` | Modify skill proficiencies |

#### Dice Expression Support

Stat modifiers support dice expressions:

```haskell
parseModifier :: String -> IO Int
-- Examples:
-- "5"     -> returns 5
-- "-3"    -> returns -3
-- "1d6"   -> rolls 1d6
-- "-2d4"  -> rolls 2d4 and negates
```

### 6.4 CombatService (Application/CombatService.hs)

Handles turn-based combat encounters with multiple enemies.

#### Combat State

```haskell
data CombatState = CombatState
    { combatPlayer       :: PlayerCharacter  -- Player during combat
    , combatEnemies      :: [Enemy]          -- List of enemies
    , combatVictoryEntry :: EntryId          -- Entry on victory
    , combatDefeatEntry  :: EntryId          -- Entry on defeat
    }
```

#### Combat Actions

```haskell
data CombatActionResult
    = PlayerHit Int           -- Damage dealt to enemy
    | PlayerMiss Int          -- Attack roll (missed)
    | EnemyHit String Int     -- Enemy name, damage to player
    | EnemyMiss String Int    -- Enemy name, attack roll
    | EnemyDefeated String    -- Enemy name (killed)
    | PlayerDefeated          -- Player HP reached 0
    | CombatVictory           -- All enemies defeated

data CombatStatus = InProgress | Victory | Defeat
```

#### Combat Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `startCombat` | `PlayerCharacter -> [Enemy] -> EntryId -> EntryId -> CombatState` | Initialize combat with enemies and destination entries |
| `playerAttack` | `Int -> Weapon -> CombatState -> IO (CombatActionResult, CombatState)` | Player attacks enemy by index with selected weapon |
| `enemyAttackPlayer` | `Int -> CombatState -> IO (CombatActionResult, CombatState)` | Single enemy attacks player |
| `allEnemiesAttack` | `CombatState -> IO ([CombatActionResult], CombatState)` | All alive enemies take their turn |
| `getCombatStatus` | `CombatState -> CombatStatus` | Check if combat ended |
| `getAliveEnemies` | `CombatState -> [Enemy]` | Get list of alive enemies |
| `getPlayerHP` | `CombatState -> (Int, Int)` | Get (current, max) HP |
| `endCombat` | `CombatState -> (EntryId, PlayerCharacter)` | Get destination and final player state |

#### Combat Flow

```
┌─────────────────┐
│  StartCombat    │ ← Entry with StartCombat outcome
│ (enemy list,    │
│  victory/defeat)│
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│   InProgress    │◄────────────────────┐
└────────┬────────┘                     │
         │                              │
    ┌────┴────┐                         │
    ▼         ▼                         │
┌───────┐ ┌───────────┐                 │
│Player │ │ Enemy     │                 │
│Attack │ │ Turn      │                 │
└───┬───┘ └─────┬─────┘                 │
    │           │                       │
    ▼           ▼                       │
┌─────────────────┐                     │
│ Check Status    │─── InProgress ──────┘
└────────┬────────┘
         │
    ┌────┴────┐
    ▼         ▼
┌───────┐ ┌───────┐
│Victory│ │Defeat │
└───┬───┘ └───┬───┘
    │         │
    ▼         ▼
┌─────────────────┐
│   End Combat    │
│ → Navigate to   │
│ victory/defeat  │
│ entry           │
└─────────────────┘
```

### 6.4 CharacterService (Application/CharacterService.hs)

#### Check Result Info

```haskell
data CheckResultInfo = CheckResultInfo
    { checkSkillOrAttr :: String
    , checkRoll        :: Int
    , checkBonus       :: Int
    , checkTotal       :: Int
    , checkDC          :: Int
    , checkSuccess     :: Bool
    }
```

#### Service Functions

| Function | Description |
|----------|-------------|
| `performAbilityCheck` | Skill check with detailed result |
| `performSavingThrow` | Save with detailed result |
| `dealDamageToPlayer` | Apply damage |
| `healPlayer` | Heal player |
| `giveItem` / `takeItem` | Inventory operations |
| `giveClue` | Add clue |
| `playerHasClue` / `playerHasItem` | Query checks |
| `rollDice` | General dice rolling |

---

## 7. API Layer

### 7.1 Server Configuration

- **Port**: 3000
- **CORS**: Enabled for all origins
- **Content-Type**: application/json

### 7.2 Endpoints Summary

| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/` | Redirect to docs |
| GET | `/health` | Health check |
| GET | `/api/docs` | Swagger UI |
| GET | `/api/openapi.json` | OpenAPI spec |
| GET | `/game/state` | Current game state |
| POST | `/game/save` | Save game |
| POST | `/game/load` | Load game |
| POST | `/game/reset` | Reset to initial state |
| GET | `/entry/current` | Current entry |
| POST | `/entry/select` | Select option (returns effects) |
| GET | `/character` | Player info |
| GET | `/character/inventory` | Inventory |
| GET | `/character/clues` | Clues |
| POST | `/character/check` | Ability check |
| POST | `/character/save` | Saving throw |
| GET | `/combat/status` | Combat status |
| POST | `/combat/start` | Start combat manually |
| POST | `/combat/attack` | Player attack |
| POST | `/combat/enemy-turn` | Enemy attacks |
| POST | `/combat/end` | End combat |
| POST | `/dice/roll` | Roll dice |

### 7.3 Response Format

All responses use this wrapper:

```json
{
    "success": true,
    "message": "OK",
    "payload": { ... }
}
```

### 7.4 DTOs (API/DTO.hs)

See User Manual for complete DTO documentation.

---

## 8. Data Models

### 8.1 Save File Format (save.json)

```json
{
    "currentEntry": 1,
    "history": [],
    "player": {
        "currentHP": 14,
        "maxHP": 14,
        "statBlock": { ... },
        "inventory": [...],   // ALL items (includes equipment)
        "equipment": [...],   // Currently equipped items
        "clues": ["CLUE_001"]
    }
}
```

**Note**: `inventory` contains ALL items including those in `equipment`. Equipment is a subset of inventory representing currently equipped items.

### 8.2 Initial State File (initial-state.json)

Immutable file used to reset the game. Same structure as save.json.

### 8.3 Entries File Format (entries.json)

```json
[
    [1, {
        "entryDataId": 1,
        "entryDataNarrative": "You stand at the entrance...",
        "entryDataOptions": [
            {
                "id": 1,
                "description": "Enter the dungeon",
                "outcome": {
                    "type": "GoToEntry",
                    "entry": 2
                }
            },
            {
                "id": 2,
                "description": "Search for traps",
                "outcome": {
                    "type": "SkillCheck",
                    "skill": "Investigation",
                    "dc": 12,
                    "success": 3,
                    "failure": 4
                }
            }
        ],
        "entryDataRules": [
            {
                "ruleDataCondition": {
                    "tag": "HasItem",
                    "contents": "secret_key"
                },
                "ruleDataEffect": {
                    "type": "AddOption",
                    "option": {
                        "id": 99,
                        "description": "Use the secret key",
                        "outcome": {
                            "type": "GoToEntry",
                            "entry": 50
                        }
                    }
                }
            }
        ],
        "effects": {
            "stats": [],
            "equipment": [],
            "inventory": [],
            "attributes": [],
            "skills": []
        }
    }],
    [106, {
        "entryDataId": 106,
        "entryDataNarrative": "You find a magical cloak that grants +1 AC...",
        "entryDataOptions": [
            {"id": 1, "description": "Continue", "outcome": {"type": "GoToEntry", "entry": 110}}
        ],
        "entryDataRules": [],
        "effects": {
            "stats": [{"name": "armorClass", "modifier": "1"}],
            "equipment": [{"name": "Capa de Protección", "type": "wondrous", "bonus": "+1 CA y salvaciones"}],
            "inventory": [],
            "attributes": [
                {"name": "strength", "savingThrow": 1},
                {"name": "dexterity", "savingThrow": 1},
                {"name": "constitution", "savingThrow": 1},
                {"name": "intelligence", "savingThrow": 1},
                {"name": "wisdom", "savingThrow": 1},
                {"name": "charisma", "savingThrow": 1}
            ],
            "skills": []
        }
    }],
    [111, {
        "entryDataId": 111,
        "entryDataNarrative": "Giant spiders attack!",
        "entryDataOptions": [
            {
                "id": 1,
                "description": "Fight!",
                "outcome": {
                    "type": "StartCombat",
                    "enemies": ["ArañaLoboGigante", "ArañaLoboGigante"],
                    "victory": 113,
                    "defeat": 999
                }
            }
        ],
        "entryDataRules": [],
        "effects": {
            "stats": [],
            "equipment": [],
            "inventory": [],
            "attributes": [],
            "skills": []
        }
    }],
    [114, {
        "entryDataId": 114,
        "entryDataNarrative": "A spider bites you! Take 1d6 damage.",
        "entryDataOptions": [
            {"id": 1, "description": "Fight!", "outcome": {"type": "StartCombat", "enemies": ["ArañaLoboGigante"], "victory": 113, "defeat": 999}}
        ],
        "entryDataRules": [],
        "effects": {
            "stats": [{"name": "currentHP", "modifier": "-1d6"}],
            "equipment": [],
            "inventory": [],
            "attributes": [],
            "skills": []
        }
    }]
]
```

#### Effect Types Reference

| Field | Type | Description | Example |
|-------|------|-------------|---------|
| `stats.name` | String | Stat to modify | `"currentHP"`, `"armorClass"` |
| `stats.modifier` | String | Amount (supports dice) | `"5"`, `"-1d6"`, `"2d4"` |
| `equipment.name` | String | Item name | `"Espada Mágica"` |
| `equipment.type` | String | Item type | `"weapon"`, `"armor"`, `"wondrous"` |
| `equipment.bonus` | String? | Bonus description | `"+1 ataque"` |
| `inventory.name` | String | Item name | `"Oro"`, `"Poción"` |
| `inventory.type` | String | Item type | `"currency"`, `"potion"` |
| `inventory.quantity` | Int? | Amount (negative to remove) | `100`, `-1` |
| `attributes.name` | String | Attribute name | `"dexterity"` |
| `attributes.savingThrow` | Int? | Bonus to saving throw | `1` |
| `skills.name` | String | Skill name | `"Stealth"` |
| `skills.prof` | String? | Proficiency level | `"Proficient"`, `"Expertise"` |

### 8.4 Enemies File Format (enemies.json)

```json
[
    ["Goblin", {
        "name": "Goblin",
        "currentHP": 7,
        "maxHP": 7,
        "statBlock": {
            "attributes": [
                ["Strength", 8],
                ["Dexterity", 14],
                ["Constitution", 10],
                ["Intelligence", 10],
                ["Wisdom", 8],
                ["Charisma", 8]
            ],
            "savingThrowProfs": [],
            "skillProfs": [["Stealth", "Proficient"]],
            "proficiencyBonus": 2,
            "armorClass": 15,
            "speed": 30,
            "initiative": 2,
            "weapons": [
                {
                    "name": "Scimitar",
                    "range": [5, 5],
                    "damageType": "Slashing",
                    "damageDice": "D6",
                    "damageCount": 1
                }
            ]
        }
    }]
]
```

---

## 9. Game Mechanics

### 9.1 D20 System

All checks follow the D20 formula:

```
Result = d20 + Modifier
Success = Result >= DC
```

### 9.2 Modifier Calculation

```
Attribute Modifier = (Attribute Score - 10) / 2

Skill Bonus = Attribute Modifier + Proficiency Bonus (if proficient)
            = Attribute Modifier + 2 × Proficiency Bonus (if expertise)

Save Bonus = Attribute Modifier + Proficiency Bonus (if proficient)
```

### 9.3 Combat System

#### Combat Trigger

Combat is triggered when a player selects an option with `StartCombat` outcome:

```json
{
    "type": "StartCombat",
    "enemies": ["Goblin", "Goblin", "KoboldMago"],
    "victory": 113,
    "defeat": 999
}
```

The API automatically:
1. Looks up enemy templates from `enemies.json`
2. Creates `CombatState` with player, enemies, and destination entries
3. Returns `CombatStatusDTO` with initial combat state

#### Combat Flow (API Calls)

```
1. POST /entry/select {optionId: 1}
   ↓ (if StartCombat)
   Returns: CombatStatusDTO

2. LOOP while status == "InProgress":
   
   2a. POST /combat/attack
       Body: {
           "attackEnemyIndex": 0,  // Target enemy
           "attackWeapon": "Espada Corta"
       }
       Returns: CombatAttackResultDTO
   
   2b. POST /combat/enemy-turn
       Returns: EnemyTurnResultDTO
   
   2c. GET /combat/status
       Returns: CombatStatusDTO

3. POST /combat/end
   Returns: EndCombatResultDTO (destination entry)
```

#### Attack Resolution

**Player Attack:**
```
Attack Roll = d20 + DEX modifier + proficiency bonus
Hit = Attack Roll >= Enemy AC
Damage = weapon dice + DEX modifier
```

**Enemy Attack:**
```
Attack Roll = d20 + enemy attack bonus
Hit = Attack Roll >= Player AC
Damage = weapon dice + enemy damage bonus
```

#### Combat Status

| Status | Condition | Result |
|--------|-----------|--------|
| `InProgress` | Player alive AND enemies alive | Continue combat |
| `Victory` | All enemies HP <= 0 | Navigate to victory entry |
| `Defeat` | Player HP <= 0 | Navigate to defeat entry |

#### Multiple Enemies

Combat supports multiple enemies simultaneously:
- Player targets specific enemy by index (0-based)
- All alive enemies attack during enemy turn
- Combat ends when ALL enemies are defeated

### 9.4 Rule Evaluation

When entering an entry or getting options:

1. Start with base options
2. For each rule:
   - Evaluate condition against current GameState
   - If true, apply effect (add/remove option, modify narrative)
3. Return final options/narrative

---

## 10. Effects System

### 10.1 Overview

The Effects System automatically modifies the player character when entering specific entries. Effects are defined in the `effects` field of each entry and applied immediately upon navigation.

### 10.2 Effect Application Order

1. **Stats** (HP, AC) - May involve dice rolls
2. **Equipment** - Added to both equipment AND inventory
3. **Inventory** - Items added or removed
4. **Attributes** - Score or saving throw modifications
5. **Skills** - Proficiency changes

### 10.3 Dice Expression Support

Stat modifiers support dice notation:

| Expression | Result |
|------------|--------|
| `"5"` | +5 |
| `"-3"` | -3 |
| `"1d6"` | Roll 1d6 |
| `"-1d6"` | Roll 1d6, negate |
| `"2d4"` | Roll 2d4 |

### 10.4 Inventory vs Equipment

- **Inventory**: Complete list of all items the character possesses
- **Equipment**: Subset of inventory that is currently equipped/active

When equipment is added via effects:
- Item is added to BOTH equipment AND inventory
- This maintains the invariant: `equipment ⊆ inventory`

### 10.5 Effect Messages

Each effect application generates a message returned in the API response:

```json
{
    "type": "navigated",
    "destination": 106,
    "effects": [
        "CA modificada en 1",
        "Equipado: Capa de Protección",
        "Bonus a salvación de strength: +1"
    ]
}
```

---

## 11. Combat System

### 11.1 Architecture

Combat is managed through `CombatState` stored separately from the main game state:

```haskell
data CombatState = CombatState
    { combatPlayer       :: PlayerCharacter
    , combatEnemies      :: [Enemy]
    , combatVictoryEntry :: EntryId
    , combatDefeatEntry  :: EntryId
    }
```

### 11.2 Combat Initialization

Combat starts when:
1. Player selects option with `StartCombat` outcome
2. System looks up enemies from `enemies.json`
3. `CombatState` is created and stored
4. API returns `CombatStatusDTO`

### 11.3 Turn Structure

**Player Turn** (`POST /combat/attack`):
- Player selects weapon and target enemy (by index)
- Attack roll: d20 + attack bonus vs enemy AC
- On hit: damage roll applied to enemy
- If enemy HP ≤ 0: enemy defeated

**Enemy Turn** (`POST /combat/enemy-turn`):
- All alive enemies attack sequentially
- Each enemy: attack roll vs player AC
- On hit: damage applied to player
- If player HP ≤ 0: combat ends in defeat

### 11.4 Combat End

Combat ends when:
- **Victory**: All enemies defeated → navigate to `victoryEntry`
- **Defeat**: Player HP ≤ 0 → navigate to `defeatEntry`

Call `POST /combat/end` to:
1. Get destination entry
2. Update player HP in game state
3. Clear combat state
4. Apply effects of destination entry

### 11.5 Combat API Sequence

```
┌──────────────────────────────────────────────────────────────┐
│ 1. Player selects combat option                              │
│    POST /entry/select {optionId: 1}                          │
│    → Returns CombatStatusDTO (combat initialized)            │
└────────────────────────┬─────────────────────────────────────┘
                         │
┌────────────────────────▼─────────────────────────────────────┐
│ 2. Combat Loop (while status == "InProgress")                │
│                                                              │
│    a) Player attacks                                         │
│       POST /combat/attack {enemyIndex: 0, weapon: "Daga"}    │
│       → Returns CombatAttackResultDTO                        │
│                                                              │
│    b) Enemies attack                                         │
│       POST /combat/enemy-turn                                │
│       → Returns EnemyTurnResultDTO                           │
│                                                              │
│    c) Check status                                           │
│       GET /combat/status                                     │
│       → Returns CombatStatusDTO                              │
└────────────────────────┬─────────────────────────────────────┘
                         │
┌────────────────────────▼─────────────────────────────────────┐
│ 3. End combat                                                │
│    POST /combat/end                                          │
│    → Returns EndCombatResultDTO (destination entry)          │
│    → Player HP synced to game state                          │
│    → Destination entry effects applied                       │
└──────────────────────────────────────────────────────────────┘
```

---

## 12. Configuration

### 12.1 Default Paths

```haskell
defaultConfig = AppConfig
    { configSavePath         = "data/save.json"
    , configInitialStatePath = "data/initial-state.json"
    , configEntriesPath      = "data/entries.json"
    , configEnemiesPath      = "data/enemies.json"
    }
```

### 12.2 Server Settings

```haskell
-- Port
scotty 3000

-- CORS Policy
corsPolicy = simpleCorsResourcePolicy
    { corsOrigins = Nothing  -- All origins
    , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
    , corsRequestHeaders = ["Content-Type", "Authorization"]
    }
```

---

## 13. Building and Running

### 13.1 Prerequisites

- GHC 9.x
- Stack build tool
- Internet connection (for dependencies)

### 13.2 Build Commands

```bash
# Build the project
stack build

# Run the server
stack run

# Build and run
stack build && stack run
```

### 13.3 Development Mode

```bash
# Watch for changes and rebuild
stack build --file-watch

# Run GHCi with project loaded
stack ghci
```

### 13.4 Testing Endpoints

```bash
# Health check
curl http://localhost:3000/health

# Load game
curl -X POST http://localhost:3000/game/load

# Reset game to initial state
curl -X POST http://localhost:3000/game/reset

# Get current entry
curl http://localhost:3000/entry/current

# Select option (effects returned in response)
curl -X POST http://localhost:3000/entry/select \
  -H "Content-Type: application/json" \
  -d '{"selectOptionId": 1}'

# Combat attack
curl -X POST http://localhost:3000/combat/attack \
  -H "Content-Type: application/json" \
  -d '{"attackEnemyIndex": 0, "attackWeapon": "Espada Corta"}'

# Enemy turn
curl -X POST http://localhost:3000/combat/enemy-turn

# Roll dice
curl -X POST http://localhost:3000/dice/roll \
  -H "Content-Type: application/json" \
  -d '{"rollDiceType": "D20", "rollDiceCount": 1, "rollDiceBonus": 5}'
```

---

## 14. Extending the System

### 14.1 Adding New Skills

1. Add constructor to `Skill` in `Domain/Types.hs`
2. Add mapping in `skillAttribute` function
3. Add JSON parsing case in `Infrastructure/JSON.hs`
4. Add to Swagger enum in `API/Swagger.hs`

### 14.2 Adding New Condition Types

1. Add constructor to `ConditionData` in `Infrastructure/JSON.hs`
2. Add case in `toCondition` function
3. Create helper function in `Domain/Entry.hs` if needed

### 14.3 Adding New Endpoints

1. Add route in `API/Routes.hs`
2. Create request/response DTOs in `API/DTO.hs`
3. Document in `API/Swagger.hs`
4. Implement service logic in appropriate `Application/*.hs`

### 14.4 Adding New Rule Effects

1. Add constructor to `RuleEffect` in `Domain/Entry.hs`
2. Add JSON handling in `Infrastructure/JSON.hs`
3. Handle in `applyEffect` within `evaluateRules`

### 14.5 Adding New Effect Types

1. Add type in `Domain/Effects.hs`
2. Add JSON instances in `Infrastructure/JSON.hs`
3. Add application logic in `Application/EffectsService.hs`
4. Update `EntryEffects` structure if needed

---

## Appendix A: Default Character Sheet

Reference character for the game (Iksa Pen - Rogue Level 2):

| Attribute | Score | Modifier | Save |
|-----------|-------|----------|------|
| Strength | 9 | -1 | -1 |
| Dexterity | 16 | +3 | +5 ✓ |
| Constitution | 12 | +1 | +1 |
| Intelligence | 16 | +3 | +5 ✓ |
| Wisdom | 12 | +1 | +1 |
| Charisma | 12 | +1 | +1 |

**Combat Stats:**
- HP: 14
- AC: 15
- Proficiency Bonus: +2

**Key Skills:**
- Investigation: +7 (Expertise)
- Stealth: +7 (Expertise)
- Perception: +3 (Proficient)
- SleightOfHand: +5 (Proficient)
- Acrobatics: +5 (Proficient)
- Deception: +3 (Proficient)

**Starting Equipment:**
- Armadura de Cuero (Leather Armor)
- Espada Corta (Shortsword)
- Daga (Dagger)
- Ballesta de Mano (Hand Crossbow)

**Starting Inventory:**
- All equipment (equipment ⊆ inventory)
- Herramientas de Ladrón
- Cuerda (15m)
- Provisiones (7 días)
- Cantimplora
- Antorchas (5)
- Yesquero
- Mochila

---

## Appendix B: Error Codes

| HTTP Status | Meaning |
|-------------|---------|
| 200 | Success |
| 400 | Bad Request (invalid input, game not initialized, combat not active) |
| 500 | Internal Server Error (file I/O failure) |

---

## Appendix C: Combat Action Results

| Result Type | Description |
|-------------|-------------|
| `PlayerHit` | Player hit enemy, includes damage dealt |
| `PlayerMiss` | Player missed, includes attack roll |
| `EnemyHit` | Enemy hit player, includes enemy name and damage |
| `EnemyMiss` | Enemy missed, includes enemy name and roll |
| `EnemyDefeated` | Enemy HP reached 0, includes enemy name |
| `PlayerDefeated` | Player HP reached 0, combat ends |
| `CombatVictory` | All enemies defeated |

---

*Document Version: 1.1.0*
*Last Updated: January 2026*
