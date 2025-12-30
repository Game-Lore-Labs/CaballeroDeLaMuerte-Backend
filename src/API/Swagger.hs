{-# LANGUAGE OverloadedStrings #-}

module API.Swagger where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

-- | OpenAPI 3.0 Specification for RPG Bot API
openApiSpec :: Value
openApiSpec = object
    [ "openapi" .= ("3.0.3" :: T.Text)
    , "info" .= infoObject
    , "servers" .= serversArray
    , "tags" .= tagsArray
    , "paths" .= pathsObject
    , "components" .= componentsObject
    ]

infoObject :: Value
infoObject = object
    [ "title" .= ("RPG Bot API - Caballero de la Muerte" :: T.Text)
    , "description" .= ("Backend API for the interactive RPG adventure game. Implements D&D 5e rules subset for combat and skill checks." :: T.Text)
    , "version" .= ("1.0.0" :: T.Text)
    , "contact" .= object
        [ "name" .= ("MATCOM Team" :: T.Text)
        ]
    ]

serversArray :: Value
serversArray = toJSON
    [ object
        [ "url" .= ("http://localhost:3000" :: T.Text)
        , "description" .= ("Development server" :: T.Text)
        ]
    ]

tagsArray :: Value
tagsArray = toJSON
    [ object ["name" .= ("Game" :: T.Text), "description" .= ("Game management operations" :: T.Text)]
    , object ["name" .= ("Entry" :: T.Text), "description" .= ("Adventure entry navigation" :: T.Text)]
    , object ["name" .= ("Character" :: T.Text), "description" .= ("Player character operations" :: T.Text)]
    , object ["name" .= ("Combat" :: T.Text), "description" .= ("Combat system operations" :: T.Text)]
    , object ["name" .= ("Dice" :: T.Text), "description" .= ("Dice rolling operations" :: T.Text)]
    ]

pathsObject :: Value
pathsObject = object
    -- Health
    [ "/health" .= object
        [ "get" .= object
            [ "summary" .= ("Health check" :: T.Text)
            , "operationId" .= ("healthCheck" :: T.Text)
            , "responses" .= object
                [ "200" .= object
                    [ "description" .= ("Server is running" :: T.Text)
                    , "content" .= jsonContent (ref "ApiResponseString")
                    ]
                ]
            ]
        ]

    -- Game
    , "/game/state" .= object
        [ "get" .= object
            [ "tags" .= (["Game"] :: [T.Text])
            , "summary" .= ("Get current game state" :: T.Text)
            , "operationId" .= ("getGameState" :: T.Text)
            , "responses" .= object
                [ "200" .= object
                    [ "description" .= ("Current game state" :: T.Text)
                    , "content" .= jsonContent (ref "ApiResponseGameState")
                    ]
                , "400" .= object
                    [ "description" .= ("Game not initialized" :: T.Text)
                    ]
                ]
            ]
        ]

    , "/game/save" .= object
        [ "post" .= object
            [ "tags" .= (["Game"] :: [T.Text])
            , "summary" .= ("Save current game" :: T.Text)
            , "operationId" .= ("saveGame" :: T.Text)
            , "responses" .= object
                [ "200" .= object
                    [ "description" .= ("Game saved successfully" :: T.Text)
                    , "content" .= jsonContent (ref "ApiResponseString")
                    ]
                , "500" .= object
                    [ "description" .= ("Save failed" :: T.Text)
                    ]
                ]
            ]
        ]

    , "/game/load" .= object
        [ "post" .= object
            [ "tags" .= (["Game"] :: [T.Text])
            , "summary" .= ("Load saved game" :: T.Text)
            , "operationId" .= ("loadGame" :: T.Text)
            , "responses" .= object
                [ "200" .= object
                    [ "description" .= ("Game loaded successfully" :: T.Text)
                    , "content" .= jsonContent (ref "ApiResponseString")
                    ]
                , "500" .= object
                    [ "description" .= ("Load failed" :: T.Text)
                    ]
                ]
            ]
        ]

    -- Entry
    , "/entry/current" .= object
        [ "get" .= object
            [ "tags" .= (["Entry"] :: [T.Text])
            , "summary" .= ("Get current entry with narrative and options" :: T.Text)
            , "operationId" .= ("getCurrentEntry" :: T.Text)
            , "responses" .= object
                [ "200" .= object
                    [ "description" .= ("Current entry data" :: T.Text)
                    , "content" .= jsonContent (ref "ApiResponseEntry")
                    ]
                ]
            ]
        ]

    , "/entry/select" .= object
        [ "post" .= object
            [ "tags" .= (["Entry"] :: [T.Text])
            , "summary" .= ("Select an option from current entry" :: T.Text)
            , "operationId" .= ("selectOption" :: T.Text)
            , "requestBody" .= object
                [ "required" .= True
                , "content" .= jsonContent (ref "SelectOptionRequest")
                ]
            , "responses" .= object
                [ "200" .= object
                    [ "description" .= ("Option result" :: T.Text)
                    , "content" .= jsonContent (ref "ApiResponseOptionResult")
                    ]
                ]
            ]
        ]

    -- Character
    , "/character" .= object
        [ "get" .= object
            [ "tags" .= (["Character"] :: [T.Text])
            , "summary" .= ("Get player character information" :: T.Text)
            , "operationId" .= ("getCharacter" :: T.Text)
            , "responses" .= object
                [ "200" .= object
                    [ "description" .= ("Character data" :: T.Text)
                    , "content" .= jsonContent (ref "ApiResponseCharacter")
                    ]
                ]
            ]
        ]

    , "/character/inventory" .= object
        [ "get" .= object
            [ "tags" .= (["Character"] :: [T.Text])
            , "summary" .= ("Get player inventory" :: T.Text)
            , "operationId" .= ("getInventory" :: T.Text)
            , "responses" .= object
                [ "200" .= object
                    [ "description" .= ("Inventory items" :: T.Text)
                    , "content" .= jsonContent (ref "ApiResponseItems")
                    ]
                ]
            ]
        ]

    , "/character/clues" .= object
        [ "get" .= object
            [ "tags" .= (["Character"] :: [T.Text])
            , "summary" .= ("Get discovered clues" :: T.Text)
            , "operationId" .= ("getClues" :: T.Text)
            , "responses" .= object
                [ "200" .= object
                    [ "description" .= ("Clue IDs" :: T.Text)
                    , "content" .= jsonContent (ref "ApiResponseClues")
                    ]
                ]
            ]
        ]

    , "/character/check" .= object
        [ "post" .= object
            [ "tags" .= (["Character"] :: [T.Text])
            , "summary" .= ("Perform ability check (skill check)" :: T.Text)
            , "operationId" .= ("abilityCheck" :: T.Text)
            , "requestBody" .= object
                [ "required" .= True
                , "content" .= jsonContent (ref "AbilityCheckRequest")
                ]
            , "responses" .= object
                [ "200" .= object
                    [ "description" .= ("Check result" :: T.Text)
                    , "content" .= jsonContent (ref "ApiResponseCheckResult")
                    ]
                ]
            ]
        ]

    , "/character/save" .= object
        [ "post" .= object
            [ "tags" .= (["Character"] :: [T.Text])
            , "summary" .= ("Perform saving throw" :: T.Text)
            , "operationId" .= ("savingThrow" :: T.Text)
            , "requestBody" .= object
                [ "required" .= True
                , "content" .= jsonContent (ref "SavingThrowRequest")
                ]
            , "responses" .= object
                [ "200" .= object
                    [ "description" .= ("Save result" :: T.Text)
                    , "content" .= jsonContent (ref "ApiResponseCheckResult")
                    ]
                ]
            ]
        ]

    -- Combat
    , "/combat/status" .= object
        [ "get" .= object
            [ "tags" .= (["Combat"] :: [T.Text])
            , "summary" .= ("Get current combat status" :: T.Text)
            , "operationId" .= ("getCombatStatus" :: T.Text)
            , "responses" .= object
                [ "200" .= object
                    [ "description" .= ("Combat status" :: T.Text)
                    , "content" .= jsonContent (ref "ApiResponseCombatStatus")
                    ]
                , "400" .= object
                    [ "description" .= ("Not in combat" :: T.Text)
                    ]
                ]
            ]
        ]

    , "/combat/attack" .= object
        [ "post" .= object
            [ "tags" .= (["Combat"] :: [T.Text])
            , "summary" .= ("Player attacks an enemy" :: T.Text)
            , "operationId" .= ("playerAttack" :: T.Text)
            , "requestBody" .= object
                [ "required" .= True
                , "content" .= jsonContent (ref "CombatAttackRequest")
                ]
            , "responses" .= object
                [ "200" .= object
                    [ "description" .= ("Attack result" :: T.Text)
                    , "content" .= jsonContent (ref "ApiResponseCombatAction")
                    ]
                ]
            ]
        ]

    , "/combat/enemy-turn" .= object
        [ "post" .= object
            [ "tags" .= (["Combat"] :: [T.Text])
            , "summary" .= ("Process all enemy attacks" :: T.Text)
            , "operationId" .= ("enemyTurn" :: T.Text)
            , "responses" .= object
                [ "200" .= object
                    [ "description" .= ("Enemy attack results" :: T.Text)
                    , "content" .= jsonContent (ref "ApiResponseCombatActions")
                    ]
                ]
            ]
        ]

    , "/combat/end" .= object
        [ "post" .= object
            [ "tags" .= (["Combat"] :: [T.Text])
            , "summary" .= ("End combat and return to adventure" :: T.Text)
            , "operationId" .= ("endCombat" :: T.Text)
            , "responses" .= object
                [ "200" .= object
                    [ "description" .= ("New game state after combat" :: T.Text)
                    , "content" .= jsonContent (ref "ApiResponseGameState")
                    ]
                ]
            ]
        ]

    -- Dice
    , "/dice/roll" .= object
        [ "post" .= object
            [ "tags" .= (["Dice"] :: [T.Text])
            , "summary" .= ("Roll dice" :: T.Text)
            , "description" .= ("Roll N dice of M faces with optional bonus. Supports D2, D4, D6, D8, D10, D12, D20, D100." :: T.Text)
            , "operationId" .= ("rollDice" :: T.Text)
            , "requestBody" .= object
                [ "required" .= True
                , "content" .= jsonContent (ref "DiceRollRequest")
                ]
            , "responses" .= object
                [ "200" .= object
                    [ "description" .= ("Dice roll result" :: T.Text)
                    , "content" .= jsonContent (ref "ApiResponseDiceRoll")
                    ]
                ]
            ]
        ]
    ]

-- Helper for JSON content
jsonContent :: Value -> Value
jsonContent schema = object
    [ "application/json" .= object
        [ "schema" .= schema
        ]
    ]

-- Helper for $ref
ref :: T.Text -> Value
ref name = object ["$ref" .= ("#/components/schemas/" <> name)]

componentsObject :: Value
componentsObject = object
    [ "schemas" .= schemasObject
    ]

schemasObject :: Value
schemasObject = object
    -- Base types
    [ "ApiResponseString" .= apiResponseSchema (object ["type" .= ("string" :: T.Text)])
    , "ApiResponseGameState" .= apiResponseSchema (ref "GameStateDTO")
    , "ApiResponseEntry" .= apiResponseSchema (ref "EntryDTO")
    , "ApiResponseOptionResult" .= apiResponseSchema (ref "OptionResultDTO")
    , "ApiResponseCharacter" .= apiResponseSchema (ref "CharacterDTO")
    , "ApiResponseItems" .= apiResponseSchema (object ["type" .= ("array" :: T.Text), "items" .= ref "ItemDTO"])
    , "ApiResponseClues" .= apiResponseSchema (object ["type" .= ("array" :: T.Text), "items" .= object ["type" .= ("string" :: T.Text)]])
    , "ApiResponseCheckResult" .= apiResponseSchema (ref "CheckResultDTO")
    , "ApiResponseCombatStatus" .= apiResponseSchema (ref "CombatStatusDTO")
    , "ApiResponseCombatAction" .= apiResponseSchema (ref "CombatActionDTO")
    , "ApiResponseCombatActions" .= apiResponseSchema (object ["type" .= ("array" :: T.Text), "items" .= ref "CombatActionDTO"])
    , "ApiResponseDiceRoll" .= apiResponseSchema (ref "DiceRollDTO")

    -- Request schemas
    , "SelectOptionRequest" .= object
        [ "type" .= ("object" :: T.Text)
        , "required" .= (["selectOptionId"] :: [T.Text])
        , "properties" .= object
            [ "selectOptionId" .= object
                [ "type" .= ("integer" :: T.Text)
                , "description" .= ("ID of the option to select" :: T.Text)
                ]
            ]
        ]

    , "AbilityCheckRequest" .= object
        [ "type" .= ("object" :: T.Text)
        , "required" .= (["checkSkill", "checkDC"] :: [T.Text])
        , "properties" .= object
            [ "checkSkill" .= object
                [ "type" .= ("string" :: T.Text)
                , "enum" .= skillEnum
                , "description" .= ("Skill to check" :: T.Text)
                ]
            , "checkDC" .= object
                [ "type" .= ("integer" :: T.Text)
                , "minimum" .= (1 :: Int)
                , "maximum" .= (30 :: Int)
                , "description" .= ("Difficulty class" :: T.Text)
                ]
            ]
        ]

    , "SavingThrowRequest" .= object
        [ "type" .= ("object" :: T.Text)
        , "required" .= (["saveAttribute", "saveDC"] :: [T.Text])
        , "properties" .= object
            [ "saveAttribute" .= object
                [ "type" .= ("string" :: T.Text)
                , "enum" .= attributeEnum
                , "description" .= ("Attribute for saving throw" :: T.Text)
                ]
            , "saveDC" .= object
                [ "type" .= ("integer" :: T.Text)
                , "minimum" .= (1 :: Int)
                , "maximum" .= (30 :: Int)
                , "description" .= ("Difficulty class" :: T.Text)
                ]
            ]
        ]

    , "DiceRollRequest" .= object
        [ "type" .= ("object" :: T.Text)
        , "required" .= (["rollDiceType", "rollDiceCount", "rollDiceBonus"] :: [T.Text])
        , "properties" .= object
            [ "rollDiceType" .= object
                [ "type" .= ("string" :: T.Text)
                , "enum" .= diceEnum
                , "description" .= ("Type of dice" :: T.Text)
                ]
            , "rollDiceCount" .= object
                [ "type" .= ("integer" :: T.Text)
                , "minimum" .= (1 :: Int)
                , "description" .= ("Number of dice to roll" :: T.Text)
                ]
            , "rollDiceBonus" .= object
                [ "type" .= ("integer" :: T.Text)
                , "description" .= ("Flat bonus to add" :: T.Text)
                ]
            ]
        ]

    , "CombatAttackRequest" .= object
        [ "type" .= ("object" :: T.Text)
        , "required" .= (["attackTargetIndex", "attackWeaponIndex"] :: [T.Text])
        , "properties" .= object
            [ "attackTargetIndex" .= object
                [ "type" .= ("integer" :: T.Text)
                , "description" .= ("Index of target enemy" :: T.Text)
                ]
            , "attackWeaponIndex" .= object
                [ "type" .= ("integer" :: T.Text)
                , "description" .= ("Index of weapon to use" :: T.Text)
                ]
            ]
        ]

    -- Response DTOs
    , "GameStateDTO" .= object
        [ "type" .= ("object" :: T.Text)
        , "properties" .= object
            [ "currentEntry" .= object ["type" .= ("integer" :: T.Text)]
            , "player" .= ref "CharacterDTO"
            , "history" .= object ["type" .= ("array" :: T.Text), "items" .= object ["type" .= ("integer" :: T.Text)]]
            ]
        ]

    , "EntryDTO" .= object
        [ "type" .= ("object" :: T.Text)
        , "properties" .= object
            [ "id" .= object ["type" .= ("integer" :: T.Text)]
            , "narrative" .= object ["type" .= ("string" :: T.Text)]
            , "options" .= object ["type" .= ("array" :: T.Text), "items" .= ref "OptionDTO"]
            ]
        ]

    , "OptionDTO" .= object
        [ "type" .= ("object" :: T.Text)
        , "properties" .= object
            [ "id" .= object ["type" .= ("integer" :: T.Text)]
            , "description" .= object ["type" .= ("string" :: T.Text)]
            ]
        ]

    , "OptionResultDTO" .= object
        [ "type" .= ("object" :: T.Text)
        , "properties" .= object
            [ "type" .= object ["type" .= ("string" :: T.Text), "enum" .= (["navigated", "check_passed", "check_failed", "save_passed", "save_failed", "combat_started", "error"] :: [T.Text])]
            , "roll" .= object ["type" .= ("integer" :: T.Text), "nullable" .= True]
            , "dc" .= object ["type" .= ("integer" :: T.Text), "nullable" .= True]
            , "destination" .= object ["type" .= ("integer" :: T.Text), "nullable" .= True]
            , "skill" .= object ["type" .= ("string" :: T.Text), "nullable" .= True]
            , "attribute" .= object ["type" .= ("string" :: T.Text), "nullable" .= True]
            ]
        ]

    , "CharacterDTO" .= object
        [ "type" .= ("object" :: T.Text)
        , "properties" .= object
            [ "currentHP" .= object ["type" .= ("integer" :: T.Text)]
            , "maxHP" .= object ["type" .= ("integer" :: T.Text)]
            , "ac" .= object ["type" .= ("integer" :: T.Text)]
            , "inventory" .= object ["type" .= ("array" :: T.Text), "items" .= ref "ItemDTO"]
            , "equipment" .= object ["type" .= ("array" :: T.Text), "items" .= ref "ItemDTO"]
            , "clues" .= object ["type" .= ("array" :: T.Text), "items" .= object ["type" .= ("string" :: T.Text)]]
            ]
        ]

    , "ItemDTO" .= object
        [ "type" .= ("object" :: T.Text)
        , "properties" .= object
            [ "id" .= object ["type" .= ("string" :: T.Text)]
            , "name" .= object ["type" .= ("string" :: T.Text)]
            , "description" .= object ["type" .= ("string" :: T.Text)]
            ]
        ]

    , "CheckResultDTO" .= object
        [ "type" .= ("object" :: T.Text)
        , "properties" .= object
            [ "skill" .= object ["type" .= ("string" :: T.Text)]
            , "roll" .= object ["type" .= ("integer" :: T.Text)]
            , "bonus" .= object ["type" .= ("integer" :: T.Text)]
            , "total" .= object ["type" .= ("integer" :: T.Text)]
            , "dc" .= object ["type" .= ("integer" :: T.Text)]
            , "success" .= object ["type" .= ("boolean" :: T.Text)]
            ]
        ]

    , "DiceRollDTO" .= object
        [ "type" .= ("object" :: T.Text)
        , "properties" .= object
            [ "values" .= object ["type" .= ("array" :: T.Text), "items" .= object ["type" .= ("integer" :: T.Text)]]
            , "bonus" .= object ["type" .= ("integer" :: T.Text)]
            , "total" .= object ["type" .= ("integer" :: T.Text)]
            ]
        ]

    , "CombatStatusDTO" .= object
        [ "type" .= ("object" :: T.Text)
        , "properties" .= object
            [ "state" .= object ["type" .= ("string" :: T.Text), "enum" .= (["InProgress", "Victory", "Defeat"] :: [T.Text])]
            , "player" .= ref "CharacterDTO"
            , "enemies" .= object ["type" .= ("array" :: T.Text), "items" .= ref "EnemyDTO"]
            ]
        ]

    , "EnemyDTO" .= object
        [ "type" .= ("object" :: T.Text)
        , "properties" .= object
            [ "name" .= object ["type" .= ("string" :: T.Text)]
            , "currentHP" .= object ["type" .= ("integer" :: T.Text)]
            , "maxHP" .= object ["type" .= ("integer" :: T.Text)]
            , "ac" .= object ["type" .= ("integer" :: T.Text)]
            ]
        ]

    , "CombatActionDTO" .= object
        [ "type" .= ("object" :: T.Text)
        , "properties" .= object
            [ "type" .= object ["type" .= ("string" :: T.Text), "enum" .= (["player_hit", "player_miss", "enemy_hit", "enemy_miss", "enemy_defeated", "player_defeated"] :: [T.Text])]
            , "actor" .= object ["type" .= ("string" :: T.Text), "nullable" .= True]
            , "damage" .= object ["type" .= ("integer" :: T.Text), "nullable" .= True]
            , "roll" .= object ["type" .= ("integer" :: T.Text), "nullable" .= True]
            ]
        ]
    ]

-- Helper for ApiResponse wrapper schema
apiResponseSchema :: Value -> Value
apiResponseSchema payloadSchema = object
    [ "type" .= ("object" :: T.Text)
    , "properties" .= object
        [ "success" .= object ["type" .= ("boolean" :: T.Text)]
        , "message" .= object ["type" .= ("string" :: T.Text)]
        , "payload" .= payloadSchema
        ]
    ]

-- Enums
skillEnum :: [T.Text]
skillEnum =
    [ "Athletics", "Acrobatics", "SleightOfHand", "Stealth"
    , "Arcana", "History", "Investigation", "Nature", "Religion"
    , "AnimalHandling", "Insight", "Medicine", "Perception", "Survival"
    , "Deception", "Intimidation", "Performance", "Persuasion"
    ]

attributeEnum :: [T.Text]
attributeEnum = ["Strength", "Dexterity", "Constitution", "Intelligence", "Wisdom", "Charisma"]

diceEnum :: [T.Text]
diceEnum = ["D2", "D4", "D6", "D8", "D10", "D12", "D20", "D100"]

-- | Swagger UI HTML page
swaggerUIHtml :: TL.Text
swaggerUIHtml = TL.unlines
    [ "<!DOCTYPE html>"
    , "<html lang=\"en\">"
    , "<head>"
    , "  <meta charset=\"UTF-8\">"
    , "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"
    , "  <title>RPG Bot API - Swagger UI</title>"
    , "  <link rel=\"stylesheet\" type=\"text/css\" href=\"https://unpkg.com/swagger-ui-dist@5.9.0/swagger-ui.css\">"
    , "</head>"
    , "<body>"
    , "  <div id=\"swagger-ui\"></div>"
    , "  <script src=\"https://unpkg.com/swagger-ui-dist@5.9.0/swagger-ui-bundle.js\"></script>"
    , "  <script>"
    , "    window.onload = function() {"
    , "      SwaggerUIBundle({"
    , "        url: '/api/openapi.json',"
    , "        dom_id: '#swagger-ui',"
    , "        presets: [SwaggerUIBundle.presets.apis, SwaggerUIBundle.SwaggerUIStandalonePreset],"
    , "        layout: 'BaseLayout'"
    , "      });"
    , "    };"
    , "  </script>"
    , "</body>"
    , "</html>"
    ]
