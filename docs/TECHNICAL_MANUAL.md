# Technical Manual - RPG Bot Backend

## Iksa Pen y el Caballero de la Muerte

**Version:** 1.0.0
**Language:** Haskell
**Framework:** Scotty (Web), Aeson (JSON)

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
10. [Configuration](#10-configuration)
11. [Building and Running](#11-building-and-running)
12. [Extending the System](#12-extending-the-system)

---

## 1. Overview

### 1.1 Purpose

This backend system powers an interactive RPG adventure game based on a subset of Dungeons & Dragons 5th Edition rules. The game follows a "choose your own adventure" structure where players navigate through numbered entries, make choices, and resolve skill checks and combat encounters.

### 1.2 Key Features

- **Entry-based Navigation**: Adventure structured as interconnected entries (scenes)
- **D20 System**: Skill checks and saving throws using D&D 5e mechanics
- **Combat System**: Turn-based combat with enemies
- **State Management**: Player character, inventory, clues, and history tracking
- **Conditional Rules**: Dynamic options based on game state
- **RESTful API**: HTTP endpoints for frontend integration
- **JSON Persistence**: Save/load game functionality

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
│   └── Entry.hs                 # Adventure entries and rules
├── Infrastructure/
│   ├── JSON.hs                  # Aeson instances
│   └── Repository.hs            # Data persistence
├── Application/
│   ├── GameService.hs           # Game state management
│   ├── AdventureService.hs      # Entry navigation
│   ├── CombatService.hs         # Combat encounters
│   └── CharacterService.hs      # Character operations
└── API/
    ├── DTO.hs                   # Data Transfer Objects
    ├── Routes.hs                # HTTP endpoints
    └── Swagger.hs               # OpenAPI specification

data/                            # Runtime data (JSON files)
├── save.json                    # Game save file
├── entries.json                 # Adventure entries
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
    }
```

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
    { configSavePath    :: FilePath  -- "data/save.json"
    , configEntriesPath :: FilePath  -- "data/entries.json"
    , configEnemiesPath :: FilePath  -- "data/enemies.json"
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
    = NavigatedTo EntryId
    | CheckPassed Skill Int Int EntryId   -- skill, roll, DC, dest
    | CheckFailed Skill Int Int EntryId
    | SavePassed Attribute Int Int EntryId
    | SaveFailed Attribute Int Int EntryId
    | CombatStarted EntryId EntryId       -- victory, defeat
    | OptionNotFound
    | EntryNotFound
```

#### Key Function

```haskell
selectOption :: Int -> AppState -> IO (OptionResult, AppState)
```

This function:
1. Finds the option by ID
2. Processes the outcome (navigation, check, combat)
3. Returns the result and updated state

### 6.3 CombatService (Application/CombatService.hs)

#### Combat State

```haskell
data CombatState = CombatState
    { combatPlayer       :: PlayerCharacter
    , combatEnemies      :: Combat
    , combatVictoryEntry :: EntryId
    , combatDefeatEntry  :: EntryId
    }
```

#### Combat Actions

```haskell
data CombatActionResult
    = PlayerHit Int           -- Damage dealt
    | PlayerMiss Int          -- Attack roll
    | EnemyHit String Int     -- Enemy name, damage
    | EnemyMiss String Int    -- Enemy name, roll
    | EnemyDefeated String
    | PlayerDefeated
    | CombatVictory

data CombatStatus = InProgress | Victory | Defeat
```

#### Combat Functions

| Function | Description |
|----------|-------------|
| `startCombat` | Initialize combat encounter |
| `playerAttack` | Player attacks enemy by index |
| `enemyAttackPlayer` | Single enemy attacks |
| `allEnemiesAttack` | All enemies take turns |
| `getCombatStatus` | Check victory/defeat |
| `endCombat` | Get destination entry |

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
| GET | `/entry/current` | Current entry |
| POST | `/entry/select` | Select option |
| GET | `/character` | Player info |
| GET | `/character/inventory` | Inventory |
| GET | `/character/clues` | Clues |
| POST | `/character/check` | Ability check |
| POST | `/character/save` | Saving throw |
| GET | `/combat/status` | Combat status |
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
    "player": {
        "currentHP": 14,
        "maxHP": 14,
        "statBlock": { ... },
        "inventory": [...],
        "equipment": [...],
        "clues": ["CLUE_001", "CLUE_002"]
    },
    "currentEntry": 1,
    "history": [0]
}
```

### 8.2 Entries File Format (entries.json)

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
                    "tag": "HasClue",
                    "contents": "SECRET_PASSAGE"
                },
                "ruleDataEffect": {
                    "type": "AddOption",
                    "option": {
                        "id": 99,
                        "description": "Use secret passage",
                        "outcome": {
                            "type": "GoToEntry",
                            "entry": 50
                        }
                    }
                }
            }
        ]
    }]
]
```

### 8.3 Enemies File Format (enemies.json)

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

### 9.3 Combat Flow

1. **Initiate Combat**: Entry option triggers `StartCombat`
2. **Player Turn**:
   - Select weapon
   - Select target enemy (by index)
   - Roll attack: d20 + attack bonus vs enemy AC
   - On hit: Roll damage dice + modifier
3. **Enemy Turn**:
   - Each alive enemy attacks
   - Roll attack vs player AC
   - On hit: Roll damage
4. **Check Status**:
   - Player HP <= 0: Defeat
   - All enemies HP <= 0: Victory
5. **End Combat**: Navigate to victory/defeat entry

### 9.4 Rule Evaluation

When entering an entry or getting options:

1. Start with base options
2. For each rule:
   - Evaluate condition against current GameState
   - If true, apply effect (add/remove option, modify narrative)
3. Return final options/narrative

---

## 10. Configuration

### 10.1 Default Paths

```haskell
defaultConfig = AppConfig
    { configSavePath    = "data/save.json"
    , configEntriesPath = "data/entries.json"
    , configEnemiesPath = "data/enemies.json"
    }
```

### 10.2 Server Settings

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

## 11. Building and Running

### 11.1 Prerequisites

- GHC 9.x
- Stack build tool
- Internet connection (for dependencies)

### 11.2 Build Commands

```bash
# Build the project
stack build

# Run the server
stack run

# Build and run
stack build && stack run
```

### 11.3 Development Mode

```bash
# Watch for changes and rebuild
stack build --file-watch

# Run GHCi with project loaded
stack ghci
```

### 11.4 Testing Endpoints

```bash
# Health check
curl http://localhost:3000/health

# Load game
curl -X POST http://localhost:3000/game/load

# Get current entry
curl http://localhost:3000/entry/current

# Select option
curl -X POST http://localhost:3000/entry/select \
  -H "Content-Type: application/json" \
  -d '{"selectOptionId": 1}'

# Roll dice
curl -X POST http://localhost:3000/dice/roll \
  -H "Content-Type: application/json" \
  -d '{"rollDiceType": "D20", "rollDiceCount": 1, "rollDiceBonus": 5}'
```

---

## 12. Extending the System

### 12.1 Adding New Skills

1. Add constructor to `Skill` in `Domain/Types.hs`
2. Add mapping in `skillAttribute` function
3. Add JSON parsing case in `Infrastructure/JSON.hs`
4. Add to Swagger enum in `API/Swagger.hs`

### 12.2 Adding New Condition Types

1. Add constructor to `ConditionData` in `Infrastructure/JSON.hs`
2. Add case in `toCondition` function
3. Create helper function in `Domain/Entry.hs` if needed

### 12.3 Adding New Endpoints

1. Add route in `API/Routes.hs`
2. Create request/response DTOs in `API/DTO.hs`
3. Document in `API/Swagger.hs`
4. Implement service logic in appropriate `Application/*.hs`

### 12.4 Adding New Rule Effects

1. Add constructor to `RuleEffect` in `Domain/Entry.hs`
2. Add JSON handling in `Infrastructure/JSON.hs`
3. Handle in `applyEffect` within `evaluateRules`

---

## Appendix A: Iksa Pen Character Sheet

Reference character for the game:

| Attribute | Score | Modifier | Save |
|-----------|-------|----------|------|
| Strength | 9 | -1 | -1 |
| Dexterity | 16 | +3 | +5 |
| Constitution | 12 | +1 | +1 |
| Intelligence | 16 | +3 | +5 |
| Wisdom | 12 | +1 | +1 |
| Charisma | 12 | +1 | +1 |

**Combat Stats:**
- HP: 14
- AC: 15
- Proficiency Bonus: +2

**Key Skills (with Expertise):**
- Investigation: +7
- Stealth: +7

---

## Appendix B: Error Codes

| HTTP Status | Meaning |
|-------------|---------|
| 200 | Success |
| 400 | Bad Request (invalid input, game not initialized) |
| 500 | Internal Server Error (file I/O failure) |

---

*Document Version: 1.0.0*
*Last Updated: December 2024*
