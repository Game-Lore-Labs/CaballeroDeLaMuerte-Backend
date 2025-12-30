# User Manual - RPG Bot API

## Iksa Pen y el Caballero de la Muerte

**Version:** 1.0.0
**Base URL:** `http://localhost:3000`

---

## Table of Contents

1. [Getting Started](#1-getting-started)
2. [API Documentation Access](#2-api-documentation-access)
3. [Game Flow](#3-game-flow)
4. [API Reference](#4-api-reference)
5. [Data Types](#5-data-types)
6. [Game Mechanics](#6-game-mechanics)
7. [Creating Game Content](#7-creating-game-content)
8. [Error Handling](#8-error-handling)
9. [Examples](#9-examples)

---

## 1. Getting Started

### 1.1 Prerequisites

Before using the API, ensure:
- The server is running on `localhost:3000`
- Data files exist in the `data/` directory:
  - `entries.json` - Adventure content
  - `enemies.json` - Enemy templates
  - `save.json` - Game save (created on first save)

### 1.2 Starting the Server

```bash
# Navigate to project directory
cd CaballeroDeLaMuerte-Backend

# Build and run
stack build && stack run
```

You should see:
```
Starting RPG Bot API Server...
Server running on http://localhost:3000
```

### 1.3 Quick Test

```bash
# Check if server is running
curl http://localhost:3000/health

# Expected response:
{
    "success": true,
    "message": "OK",
    "payload": "OK"
}
```

---

## 2. API Documentation Access

### 2.1 Swagger UI

Interactive API documentation is available at:

**URL:** `http://localhost:3000/api/docs`

Features:
- Browse all endpoints
- View request/response schemas
- Try out API calls directly

### 2.2 OpenAPI Specification

Raw OpenAPI 3.0 JSON specification:

**URL:** `http://localhost:3000/api/openapi.json`

Use this to generate client libraries or import into API tools like Postman.

---

## 3. Game Flow

### 3.1 Typical Game Session

```
┌─────────────┐
│  Load Game  │  POST /game/load
└──────┬──────┘
       │
       ▼
┌─────────────┐
│ Get Current │  GET /entry/current
│   Entry     │
└──────┬──────┘
       │
       ▼
┌─────────────┐
│   Display   │  Show narrative and options to player
│  Narrative  │
└──────┬──────┘
       │
       ▼
┌─────────────┐
│   Player    │  POST /entry/select
│  Chooses    │
└──────┬──────┘
       │
       ├──────────────────┐
       │                  │
       ▼                  ▼
┌─────────────┐    ┌─────────────┐
│  Navigate   │    │Start Combat │
│  to Entry   │    │             │
└──────┬──────┘    └──────┬──────┘
       │                  │
       │                  ▼
       │           ┌─────────────┐
       │           │Combat Loop  │
       │           │(see 3.2)    │
       │           └──────┬──────┘
       │                  │
       └────────┬─────────┘
                │
                ▼
         ┌─────────────┐
         │ Loop to Get │
         │ Next Entry  │
         └─────────────┘
```

### 3.2 Combat Flow

```
┌─────────────────┐
│  Combat Start   │  (from entry option)
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Get Status     │  GET /combat/status
└────────┬────────┘
         │
         ▼
    ┌────┴────┐
    │ Player  │
    │  Turn   │
    └────┬────┘
         │
         ▼
┌─────────────────┐
│ Player Attack   │  POST /combat/attack
│ (choose target) │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Enemy Turn     │  POST /combat/enemy-turn
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ Check Status    │  GET /combat/status
└────────┬────────┘
         │
    ┌────┴────┐
    │InProgress│──────┐
    └────┬────┘      │
         │           │ Loop
    Victory/Defeat   │
         │           │
         ▼           │
┌─────────────────┐  │
│   End Combat    │◄─┘
│POST /combat/end │
└─────────────────┘
```

### 3.3 Skill Check Flow

When selecting an option with a skill check:

1. Player selects option
2. Server rolls d20 + skill bonus
3. Compares to DC (Difficulty Class)
4. Routes to success or failure entry
5. Returns roll details to display

---

## 4. API Reference

### 4.1 General Endpoints

#### Health Check
```http
GET /health
```

**Response:**
```json
{
    "success": true,
    "message": "OK",
    "payload": "OK"
}
```

---

### 4.2 Game Management

#### Load Game
```http
POST /game/load
```

Loads the game state, entries, and enemies from disk.

**Response (Success):**
```json
{
    "success": true,
    "message": "OK",
    "payload": "Game loaded"
}
```

**Response (Error):**
```json
{
    "success": false,
    "message": "Game: Save file not found",
    "payload": null
}
```

---

#### Save Game
```http
POST /game/save
```

Saves current game state to disk.

**Response:**
```json
{
    "success": true,
    "message": "OK",
    "payload": "Game saved"
}
```

---

#### Get Game State
```http
GET /game/state
```

Returns complete current game state.

**Response:**
```json
{
    "success": true,
    "message": "OK",
    "payload": {
        "currentEntry": 1,
        "player": {
            "currentHP": 14,
            "maxHP": 14,
            "ac": 15,
            "inventory": [],
            "equipment": [],
            "clues": []
        },
        "history": []
    }
}
```

---

### 4.3 Adventure Navigation

#### Get Current Entry
```http
GET /entry/current
```

Returns the current entry with narrative and available options (after rule evaluation).

**Response:**
```json
{
    "success": true,
    "message": "OK",
    "payload": {
        "id": 1,
        "narrative": "You stand at the entrance to a dark cave. The air is cold and damp. You can hear water dripping somewhere in the darkness ahead.",
        "options": [
            {
                "id": 1,
                "description": "Light a torch and enter the cave"
            },
            {
                "id": 2,
                "description": "Search the entrance for traps (Investigation DC 12)"
            },
            {
                "id": 3,
                "description": "Listen carefully for sounds (Perception DC 10)"
            }
        ]
    }
}
```

---

#### Select Option
```http
POST /entry/select
Content-Type: application/json

{
    "selectOptionId": 2
}
```

Selects an option and processes its outcome.

**Response (Direct Navigation):**
```json
{
    "success": true,
    "message": "OK",
    "payload": {
        "type": "navigated",
        "roll": null,
        "dc": null,
        "destination": 5,
        "skill": null,
        "attribute": null
    }
}
```

**Response (Skill Check - Success):**
```json
{
    "success": true,
    "message": "OK",
    "payload": {
        "type": "check_passed",
        "roll": 18,
        "dc": 12,
        "destination": 3,
        "skill": "Investigation",
        "attribute": null
    }
}
```

**Response (Skill Check - Failure):**
```json
{
    "success": true,
    "message": "OK",
    "payload": {
        "type": "check_failed",
        "roll": 8,
        "dc": 12,
        "destination": 4,
        "skill": "Investigation",
        "attribute": null
    }
}
```

**Response (Saving Throw):**
```json
{
    "success": true,
    "message": "OK",
    "payload": {
        "type": "save_passed",
        "roll": 15,
        "dc": 13,
        "destination": 10,
        "skill": null,
        "attribute": "Dexterity"
    }
}
```

**Response (Combat Started):**
```json
{
    "success": true,
    "message": "OK",
    "payload": {
        "type": "combat_started",
        "roll": null,
        "dc": null,
        "destination": null,
        "skill": null,
        "attribute": null
    }
}
```

---

### 4.4 Character Operations

#### Get Character Info
```http
GET /character
```

**Response:**
```json
{
    "success": true,
    "message": "OK",
    "payload": {
        "currentHP": 14,
        "maxHP": 14,
        "ac": 15,
        "inventory": [
            {
                "id": "torch",
                "name": "Torch",
                "description": "A wooden torch that provides light"
            }
        ],
        "equipment": [
            {
                "id": "leather_armor",
                "name": "Leather Armor",
                "description": "Light armor providing AC 11 + DEX"
            }
        ],
        "clues": ["cave_entrance_trap", "mysterious_symbol"]
    }
}
```

---

#### Get Inventory
```http
GET /character/inventory
```

**Response:**
```json
{
    "success": true,
    "message": "OK",
    "payload": [
        {
            "id": "torch",
            "name": "Torch",
            "description": "A wooden torch that provides light"
        },
        {
            "id": "rope",
            "name": "Rope (50ft)",
            "description": "Hempen rope, useful for climbing"
        }
    ]
}
```

---

#### Get Clues
```http
GET /character/clues
```

**Response:**
```json
{
    "success": true,
    "message": "OK",
    "payload": ["cave_entrance_trap", "mysterious_symbol", "goblin_patrol_schedule"]
}
```

---

#### Perform Ability Check
```http
POST /character/check
Content-Type: application/json

{
    "checkSkill": "Stealth",
    "checkDC": 15
}
```

**Response:**
```json
{
    "success": true,
    "message": "OK",
    "payload": {
        "skill": "Stealth",
        "roll": 12,
        "bonus": 7,
        "total": 19,
        "dc": 15,
        "success": true
    }
}
```

---

#### Perform Saving Throw
```http
POST /character/save
Content-Type: application/json

{
    "saveAttribute": "Constitution",
    "saveDC": 14
}
```

**Response:**
```json
{
    "success": true,
    "message": "OK",
    "payload": {
        "skill": "Constitution",
        "roll": 8,
        "bonus": 1,
        "total": 9,
        "dc": 14,
        "success": false
    }
}
```

---

### 4.5 Combat Operations

#### Get Combat Status
```http
GET /combat/status
```

**Response:**
```json
{
    "success": true,
    "message": "OK",
    "payload": {
        "state": "InProgress",
        "player": {
            "currentHP": 10,
            "maxHP": 14,
            "ac": 15,
            "inventory": [],
            "equipment": [],
            "clues": []
        },
        "enemies": [
            {
                "name": "Goblin",
                "currentHP": 5,
                "maxHP": 7,
                "ac": 15
            },
            {
                "name": "Goblin",
                "currentHP": 7,
                "maxHP": 7,
                "ac": 15
            }
        ]
    }
}
```

---

#### Player Attack
```http
POST /combat/attack
Content-Type: application/json

{
    "attackTargetIndex": 0,
    "attackWeaponIndex": 0
}
```

- `attackTargetIndex`: Index of enemy in the enemies array (0-based)
- `attackWeaponIndex`: Index of weapon in player's weapons (0-based)

**Response (Hit):**
```json
{
    "success": true,
    "message": "OK",
    "payload": {
        "type": "player_hit",
        "actor": null,
        "damage": 6,
        "roll": null
    }
}
```

**Response (Miss):**
```json
{
    "success": true,
    "message": "OK",
    "payload": {
        "type": "player_miss",
        "actor": null,
        "damage": null,
        "roll": 8
    }
}
```

**Response (Enemy Defeated):**
```json
{
    "success": true,
    "message": "OK",
    "payload": {
        "type": "enemy_defeated",
        "actor": "Goblin",
        "damage": null,
        "roll": null
    }
}
```

---

#### Enemy Turn
```http
POST /combat/enemy-turn
```

All alive enemies attack the player.

**Response:**
```json
{
    "success": true,
    "message": "OK",
    "payload": [
        {
            "type": "enemy_hit",
            "actor": "Goblin",
            "damage": 4,
            "roll": null
        },
        {
            "type": "enemy_miss",
            "actor": "Goblin",
            "damage": null,
            "roll": 7
        }
    ]
}
```

**Response (Player Defeated):**
```json
{
    "success": true,
    "message": "OK",
    "payload": [
        {
            "type": "enemy_hit",
            "actor": "Goblin",
            "damage": 6,
            "roll": null
        },
        {
            "type": "player_defeated",
            "actor": null,
            "damage": null,
            "roll": null
        }
    ]
}
```

---

#### End Combat
```http
POST /combat/end
```

Ends combat and navigates to appropriate entry (victory or defeat).

**Response:**
```json
{
    "success": true,
    "message": "OK",
    "payload": {
        "currentEntry": 15,
        "player": {
            "currentHP": 8,
            "maxHP": 14,
            "ac": 15,
            "inventory": [],
            "equipment": [],
            "clues": []
        },
        "history": [1, 5, 10]
    }
}
```

---

### 4.6 Dice Rolling

#### Roll Dice
```http
POST /dice/roll
Content-Type: application/json

{
    "rollDiceType": "D20",
    "rollDiceCount": 1,
    "rollDiceBonus": 5
}
```

**Dice Types:** `D2`, `D4`, `D6`, `D8`, `D10`, `D12`, `D20`, `D100`

**Response:**
```json
{
    "success": true,
    "message": "OK",
    "payload": {
        "values": [14],
        "bonus": 5,
        "total": 19
    }
}
```

**Example: Rolling 2d6+3**
```json
// Request
{
    "rollDiceType": "D6",
    "rollDiceCount": 2,
    "rollDiceBonus": 3
}

// Response
{
    "success": true,
    "message": "OK",
    "payload": {
        "values": [4, 6],
        "bonus": 3,
        "total": 13
    }
}
```

---

## 5. Data Types

### 5.1 Skills

| Skill | Governing Attribute |
|-------|---------------------|
| `Athletics` | Strength |
| `Acrobatics` | Dexterity |
| `SleightOfHand` | Dexterity |
| `Stealth` | Dexterity |
| `Arcana` | Intelligence |
| `History` | Intelligence |
| `Investigation` | Intelligence |
| `Nature` | Intelligence |
| `Religion` | Intelligence |
| `AnimalHandling` | Wisdom |
| `Insight` | Wisdom |
| `Medicine` | Wisdom |
| `Perception` | Wisdom |
| `Survival` | Wisdom |
| `Deception` | Charisma |
| `Intimidation` | Charisma |
| `Performance` | Charisma |
| `Persuasion` | Charisma |

### 5.2 Attributes

- `Strength`
- `Dexterity`
- `Constitution`
- `Intelligence`
- `Wisdom`
- `Charisma`

### 5.3 Dice Types

- `D2` - Coin flip
- `D4` - Four-sided die
- `D6` - Six-sided die
- `D8` - Eight-sided die
- `D10` - Ten-sided die
- `D12` - Twelve-sided die
- `D20` - Twenty-sided die
- `D100` - Percentile (1-100)

### 5.4 Option Result Types

| Type | Description |
|------|-------------|
| `navigated` | Direct navigation to entry |
| `check_passed` | Skill check succeeded |
| `check_failed` | Skill check failed |
| `save_passed` | Saving throw succeeded |
| `save_failed` | Saving throw failed |
| `combat_started` | Combat encounter initiated |
| `error` | Option or entry not found |

### 5.5 Combat States

| State | Description |
|-------|-------------|
| `InProgress` | Combat ongoing |
| `Victory` | All enemies defeated |
| `Defeat` | Player HP reached 0 |

### 5.6 Combat Action Types

| Type | Description |
|------|-------------|
| `player_hit` | Player successfully attacked |
| `player_miss` | Player attack missed |
| `enemy_hit` | Enemy successfully attacked |
| `enemy_miss` | Enemy attack missed |
| `enemy_defeated` | Enemy was killed |
| `player_defeated` | Player was killed |

---

## 6. Game Mechanics

### 6.1 D20 System

All checks use the d20 system:

```
d20 + Modifier vs Difficulty Class (DC)
```

- **Success:** Total >= DC
- **Failure:** Total < DC

### 6.2 Modifier Calculation

```
Attribute Modifier = (Attribute Score - 10) / 2

Examples:
- Score 9  → Modifier -1
- Score 10 → Modifier 0
- Score 12 → Modifier +1
- Score 16 → Modifier +3
```

### 6.3 Skill Checks

```
Skill Check = d20 + Attribute Modifier + Proficiency (if proficient)
```

Proficiency levels:
- **Not Proficient:** +0
- **Proficient:** +Proficiency Bonus
- **Expertise:** +2 × Proficiency Bonus

### 6.4 Saving Throws

```
Saving Throw = d20 + Attribute Modifier + Proficiency (if proficient)
```

### 6.5 Attack Rolls

```
Attack Roll = d20 + Attribute Modifier + Proficiency Bonus
Damage Roll = Weapon Damage Dice + Attribute Modifier
```

### 6.6 Difficulty Classes

| DC | Difficulty |
|----|------------|
| 5 | Very Easy |
| 10 | Easy |
| 15 | Medium |
| 20 | Hard |
| 25 | Very Hard |
| 30 | Nearly Impossible |

---

## 7. Creating Game Content

### 7.1 Entry Structure

Each entry in `entries.json` follows this structure:

```json
{
    "entryDataId": 1,
    "entryDataNarrative": "Your story text here...",
    "entryDataOptions": [...],
    "entryDataRules": [...]
}
```

### 7.2 Option Types

#### Direct Navigation
```json
{
    "id": 1,
    "description": "Go through the door",
    "outcome": {
        "type": "GoToEntry",
        "entry": 5
    }
}
```

#### Skill Check
```json
{
    "id": 2,
    "description": "Pick the lock (Sleight of Hand DC 15)",
    "outcome": {
        "type": "SkillCheck",
        "skill": "SleightOfHand",
        "dc": 15,
        "success": 6,
        "failure": 7
    }
}
```

#### Saving Throw
```json
{
    "id": 3,
    "description": "Dodge the trap (DEX Save DC 14)",
    "outcome": {
        "type": "SaveCheck",
        "attribute": "Dexterity",
        "dc": 14,
        "success": 8,
        "failure": 9
    }
}
```

#### Start Combat
```json
{
    "id": 4,
    "description": "Attack the goblins!",
    "outcome": {
        "type": "StartCombat",
        "victory": 10,
        "defeat": 11
    }
}
```

### 7.3 Conditional Rules

Rules add, remove, or modify content based on game state.

#### Add Option if Player Has Clue
```json
{
    "ruleDataCondition": {
        "tag": "HasClue",
        "contents": "secret_password"
    },
    "ruleDataEffect": {
        "type": "AddOption",
        "option": {
            "id": 99,
            "description": "Say the secret password",
            "outcome": {
                "type": "GoToEntry",
                "entry": 50
            }
        }
    }
}
```

#### Remove Option if Already Visited
```json
{
    "ruleDataCondition": {
        "tag": "VisitedEntry",
        "contents": 5
    },
    "ruleDataEffect": {
        "type": "RemoveOption",
        "optionId": 2
    }
}
```

#### Add Narrative if Has Item
```json
{
    "ruleDataCondition": {
        "tag": "HasItem",
        "contents": "magic_ring"
    },
    "ruleDataEffect": {
        "type": "ModifyNarrative",
        "text": "\n\nYour magic ring glows faintly, warning you of danger nearby."
    }
}
```

#### Complex Conditions
```json
// AND condition
{
    "tag": "AndCond",
    "contents": [
        {"tag": "HasClue", "contents": "clue1"},
        {"tag": "HasItem", "contents": "item1"}
    ]
}

// OR condition
{
    "tag": "OrCond",
    "contents": [
        {"tag": "VisitedEntry", "contents": 5},
        {"tag": "VisitedEntry", "contents": 6}
    ]
}

// NOT condition
{
    "tag": "NotCond",
    "contents": {"tag": "HasClue", "contents": "failed_quest"}
}

// Always true
{
    "tag": "Always"
}
```

### 7.4 Enemy Templates

Create enemies in `enemies.json`:

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
            "skillProfs": [
                ["Stealth", "Proficient"]
            ],
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

## 8. Error Handling

### 8.1 Error Response Format

```json
{
    "success": false,
    "message": "Error description",
    "payload": null
}
```

### 8.2 Common Errors

| Error | Cause | Solution |
|-------|-------|----------|
| "Game not initialized" | Called endpoint before `/game/load` | Call `POST /game/load` first |
| "Not in combat" | Called combat endpoint outside combat | Start combat via entry option |
| "Invalid weapon index" | Weapon index out of range | Check player's weapons count |
| "Save file not found" | No save.json exists | Create initial game data |
| "Entries file not found" | No entries.json exists | Create entries file |
| "Invalid JSON body" | Malformed request JSON | Check request format |

### 8.3 HTTP Status Codes

| Code | Meaning |
|------|---------|
| 200 | Success |
| 400 | Bad Request - Invalid input or state |
| 500 | Server Error - File I/O or internal error |

---

## 9. Examples

### 9.1 Complete Game Session

```bash
# 1. Start by loading the game
curl -X POST http://localhost:3000/game/load

# 2. Get the current entry
curl http://localhost:3000/entry/current

# 3. Select an option (option ID 1)
curl -X POST http://localhost:3000/entry/select \
  -H "Content-Type: application/json" \
  -d '{"selectOptionId": 1}'

# 4. Check character status
curl http://localhost:3000/character

# 5. Save the game
curl -X POST http://localhost:3000/game/save
```

### 9.2 Combat Session

```bash
# 1. Select combat option (triggers combat)
curl -X POST http://localhost:3000/entry/select \
  -H "Content-Type: application/json" \
  -d '{"selectOptionId": 4}'

# Response indicates combat_started

# 2. Check combat status
curl http://localhost:3000/combat/status

# 3. Player attacks first enemy with first weapon
curl -X POST http://localhost:3000/combat/attack \
  -H "Content-Type: application/json" \
  -d '{"attackTargetIndex": 0, "attackWeaponIndex": 0}'

# 4. Enemy turn
curl -X POST http://localhost:3000/combat/enemy-turn

# 5. Check status again
curl http://localhost:3000/combat/status

# Repeat 3-5 until combat ends

# 6. End combat when victory/defeat
curl -X POST http://localhost:3000/combat/end
```

### 9.3 Skill Check Example

```bash
# Perform a Stealth check vs DC 15
curl -X POST http://localhost:3000/character/check \
  -H "Content-Type: application/json" \
  -d '{"checkSkill": "Stealth", "checkDC": 15}'

# Response:
{
    "success": true,
    "message": "OK",
    "payload": {
        "skill": "Stealth",
        "roll": 11,
        "bonus": 7,
        "total": 18,
        "dc": 15,
        "success": true
    }
}
```

### 9.4 Dice Rolling Examples

```bash
# Roll 1d20+5 (attack roll)
curl -X POST http://localhost:3000/dice/roll \
  -H "Content-Type: application/json" \
  -d '{"rollDiceType": "D20", "rollDiceCount": 1, "rollDiceBonus": 5}'

# Roll 2d6+3 (damage roll)
curl -X POST http://localhost:3000/dice/roll \
  -H "Content-Type: application/json" \
  -d '{"rollDiceType": "D6", "rollDiceCount": 2, "rollDiceBonus": 3}'

# Roll 1d100 (percentile)
curl -X POST http://localhost:3000/dice/roll \
  -H "Content-Type: application/json" \
  -d '{"rollDiceType": "D100", "rollDiceCount": 1, "rollDiceBonus": 0}'
```

### 9.5 Frontend Integration (JavaScript)

```javascript
const API_BASE = 'http://localhost:3000';

// Load game
async function loadGame() {
    const response = await fetch(`${API_BASE}/game/load`, {
        method: 'POST'
    });
    return response.json();
}

// Get current entry
async function getCurrentEntry() {
    const response = await fetch(`${API_BASE}/entry/current`);
    const data = await response.json();
    return data.payload;
}

// Select option
async function selectOption(optionId) {
    const response = await fetch(`${API_BASE}/entry/select`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ selectOptionId: optionId })
    });
    return response.json();
}

// Combat attack
async function attack(targetIndex, weaponIndex) {
    const response = await fetch(`${API_BASE}/combat/attack`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
            attackTargetIndex: targetIndex,
            attackWeaponIndex: weaponIndex
        })
    });
    return response.json();
}

// Example usage
async function playTurn() {
    // Load game
    await loadGame();

    // Get current entry
    const entry = await getCurrentEntry();
    console.log(entry.narrative);
    console.log('Options:', entry.options);

    // Player selects first option
    const result = await selectOption(entry.options[0].id);
    console.log('Result:', result.payload.type);
}
```

---

## Appendix A: Quick Reference Card

### Endpoints

| Action | Method | Endpoint |
|--------|--------|----------|
| Load Game | POST | `/game/load` |
| Save Game | POST | `/game/save` |
| Get State | GET | `/game/state` |
| Current Entry | GET | `/entry/current` |
| Select Option | POST | `/entry/select` |
| Character Info | GET | `/character` |
| Inventory | GET | `/character/inventory` |
| Clues | GET | `/character/clues` |
| Ability Check | POST | `/character/check` |
| Saving Throw | POST | `/character/save` |
| Combat Status | GET | `/combat/status` |
| Attack | POST | `/combat/attack` |
| Enemy Turn | POST | `/combat/enemy-turn` |
| End Combat | POST | `/combat/end` |
| Roll Dice | POST | `/dice/roll` |

### Request Bodies

```json
// Select Option
{"selectOptionId": 1}

// Ability Check
{"checkSkill": "Stealth", "checkDC": 15}

// Saving Throw
{"saveAttribute": "Dexterity", "saveDC": 14}

// Attack
{"attackTargetIndex": 0, "attackWeaponIndex": 0}

// Dice Roll
{"rollDiceType": "D20", "rollDiceCount": 1, "rollDiceBonus": 5}
```

---

*Document Version: 1.0.0*
*Last Updated: December 2024*
