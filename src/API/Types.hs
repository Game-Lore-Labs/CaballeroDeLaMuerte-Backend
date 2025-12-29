{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Types
  ( SessionAPI
  , CharacterAPI
  , NarrativeAPI
  , CombatAPI
  , ExplorationAPI
  , DiceAPI
  , AdventureAPI
  , HealthAPI
  ) where

import Servant
import Data.Text (Text)

import Application.UseCases.GameSession (CreateSessionRequest, SessionResponse, SaveGameRequest, LoadGameRequest)
import Application.UseCases.Character (CharacterSheet, UpdateCharacterRequest, CharacterInventory, Item)
import Application.UseCases.Narrative (EntryResponse, NavigateRequest, GameHistory, ClueTracker, Clue, NPCRelationship)
import Application.UseCases.Combat (CombatState, InitiativeRoll, CombatTurn, AttackRequest, AttackResult)
import Application.UseCases.Exploration (ExplorationState, MapPosition, MoveRequest, InteractRequest, InteractionResult, Interactable)
import Application.UseCases.DiceRoller (RollRequest, RollResult, D20Check, D20CheckResult)
import API.Models ( ModifierResponse
                    , UpdateNPCRequest
                    , InitiateCombatRequest
                    , CombatActionRequest
                    , EndCombatRequest
                    , EndCombatResponse
                    , ValidMoveResponse
                    , ParseDiceResponse
                    , AdventureSummary
                    , AdventureDetail
                    , HealthResponse
                    )

type SessionAPI = "sessions" :>
  ( ReqBody '[JSON] CreateSessionRequest :> Post '[JSON] SessionResponse
    :<|> Capture "sessionId" Text :> Get '[JSON] SessionResponse
    :<|> Capture "sessionId" Text :> "save" :> Post '[JSON] SuccessResponse
    :<|> Capture "sessionId" Text :> "load" :> Get '[JSON] SessionResponse
    :<|> Capture "sessionId" Text :> Delete '[JSON] SuccessResponse
  )

type CharacterAPI = "characters" :>
  ( Capture "characterId" Text :> Get '[JSON] CharacterSheet
    :<|> Capture "characterId" Text :> ReqBody '[JSON] UpdateCharacterRequest :> Put '[JSON] CharacterSheet
    :<|> Capture "characterId" Text :> "inventory" :> Get '[JSON] CharacterInventory
    :<|> Capture "characterId" Text :> "inventory" :> "items" :> ReqBody '[JSON] Item :> Post '[JSON] CharacterInventory
    :<|> Capture "characterId" Text :> "inventory" :> "items" :> Capture "itemId" Text :> Delete '[JSON] CharacterInventory
    :<|> "modifier" :> Capture "abilityScore" Int :> Get '[JSON] ModifierResponse
  )

type NarrativeAPI = "narrative" :>
  ( "sessions" :> Capture "sessionId" Text :> "current" :> Get '[JSON] EntryResponse
    :<|> "sessions" :> Capture "sessionId" Text :> "navigate" :> ReqBody '[JSON] NavigateRequest :> Post '[JSON] EntryResponse
    :<|> "sessions" :> Capture "sessionId" Text :> "history" :> Get '[JSON] GameHistory
    :<|> "sessions" :> Capture "sessionId" Text :> "clues" :> Get '[JSON] ClueTracker
    :<|> "sessions" :> Capture "sessionId" Text :> "clues" :> ReqBody '[JSON] Clue :> Post '[JSON] ClueTracker
    :<|> "sessions" :> Capture "sessionId" Text :> "npcs" :> Get '[JSON] [NPCRelationship]
    :<|> "sessions" :> Capture "sessionId" Text :> "npcs" :> Capture "npcId" Text :> ReqBody '[JSON] UpdateNPCRequest :> Put '[JSON] NPCRelationship
  )

type CombatAPI = "combat" :>
  ( "initiate" :> ReqBody '[JSON] InitiateCombatRequest :> Post '[JSON] CombatState
    :<|> Capture "combatId" Text :> "initiative" :> Post '[JSON] [InitiativeRoll]
    :<|> Capture "combatId" Text :> Get '[JSON] CombatState
    :<|> Capture "combatId" Text :> "turn" :> Get '[JSON] CombatTurn
    :<|> Capture "combatId" Text :> "attack" :> ReqBody '[JSON] AttackRequest :> Post '[JSON] AttackResult
    :<|> Capture "combatId" Text :> "action" :> ReqBody '[JSON] CombatActionRequest :> Post '[JSON] CombatState
    :<|> Capture "combatId" Text :> "turn" :> Capture "combatantId" Text :> "end" :> Post '[JSON] CombatState
    :<|> Capture "combatId" Text :> "end" :> ReqBody '[JSON] EndCombatRequest :> Post '[JSON] EndCombatResponse
  )

type ExplorationAPI = "exploration" :>
  ( "sessions" :> Capture "sessionId" Text :> Get '[JSON] ExplorationState
    :<|> "sessions" :> Capture "sessionId" Text :> "move" :> ReqBody '[JSON] MoveRequest :> Post '[JSON] ExplorationState
    :<|> "sessions" :> Capture "sessionId" Text :> "interact" :> ReqBody '[JSON] InteractRequest :> Post '[JSON] InteractionResult
    :<|> "sessions" :> Capture "sessionId" Text :> "interactables" :> QueryParam "range" Int :> Get '[JSON] [Interactable]
    :<|> "sessions" :> Capture "sessionId" Text :> "valid-move" :> ReqBody '[JSON] MapPosition :> Post '[JSON] ValidMoveResponse
  )

type DiceAPI = "dice" :>
  ( "roll" :> ReqBody '[JSON] RollRequest :> Post '[JSON] RollResult
    :<|> "check" :> ReqBody '[JSON] D20Check :> Post '[JSON] D20CheckResult
    :<|> "parse" :> Capture "notation" Text :> Get '[JSON] ParseDiceResponse
    :<|> "roll" :> "batch" :> ReqBody '[JSON] [RollRequest] :> Post '[JSON] [RollResult]
  )

type AdventureAPI = "adventures" :>
  ( Get '[JSON] [AdventureSummary]
    :<|> Capture "adventureId" Text :> Get '[JSON] AdventureDetail
    :<|> Capture "adventureId" Text :> "entries" :> Capture "entryId" Text :> Get '[JSON] EntryResponse
  )

type HealthAPI = "health" :> Get '[JSON] HealthResponse
