{-# LANGUAGE OverloadedStrings #-}

module API.Handlers.Combat
  ( handlers
  ) where

import Servant
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import API.Server (AppM)
import API.Types (CombatAPI)
import API.Models (EndCombatResponse(..))

import qualified Application.UseCases.Combat as UC

handlers :: ServerT CombatAPI AppM
handlers =
  initiateCombatHandler
  :<|> rollInitiativeHandler
  :<|> getCombatStateHandler
  :<|> getCurrentTurnHandler
  :<|> performAttackHandler
  :<|> executeCombatActionHandler
  :<|> endTurnHandler
  :<|> endCombatHandler

initiateCombatHandler :: API.Models.InitiateCombatRequest -> AppM UC.CombatState
initiateCombatHandler req = do
  result <- UC.initiateCombat (API.Models.initiateCombatSessionId req) (API.Models.initiateCombatEnemyIds req)
  case result of
    Left err -> throwError $ err400 { errBody = encodeUtf8 err }
    Right state -> pure state

rollInitiativeHandler :: Text -> AppM [UC.InitiativeRoll]
rollInitiativeHandler combatId = do
  result <- UC.rollInitiative combatId
  case result of
    Left err -> throwError $ err400 { errBody = encodeUtf8 err }
    Right rolls -> pure rolls

getCombatStateHandler :: Text -> AppM UC.CombatState
getCombatStateHandler combatId = do
  result <- UC.getCombatState combatId
  case result of
    Left err -> throwError $ err404 { errBody = encodeUtf8 err }
    Right state -> pure state

getCurrentTurnHandler :: Text -> AppM UC.CombatTurn
getCurrentTurnHandler combatId = do
  result <- UC.getCurrentTurn combatId
  case result of
    Left err -> throwError $ err404 { errBody = encodeUtf8 err }
    Right turn -> pure turn

performAttackHandler :: Text -> UC.AttackRequest -> AppM UC.AttackResult
performAttackHandler combatId req = do
  let fullReq = req { UC.attackCombatId = combatId }
  result <- UC.performAttack fullReq
  case result of
    Left err -> throwError $ err400 { errBody = encodeUtf8 err }
    Right attackResult -> pure attackResult

executeCombatActionHandler :: Text -> API.Models.CombatActionRequest -> AppM UC.CombatState
executeCombatActionHandler combatId req = do
  let action = undefined -- conversion to UC.CombatAction depends on actionType/actionParams
  result <- UC.executeCombatAction action
  case result of
    Left err -> throwError $ err400 { errBody = encodeUtf8 err }
    Right state -> pure state

endTurnHandler :: Text -> Text -> AppM UC.CombatState
endTurnHandler combatId combatantId = do
  result <- UC.endTurn combatId combatantId
  case result of
    Left err -> throwError $ err400 { errBody = encodeUtf8 err }
    Right state -> pure state

endCombatHandler :: Text -> API.Models.EndCombatRequest -> AppM EndCombatResponse
endCombatHandler combatId req = do
  result <- UC.endCombat combatId (API.Models.endCombatPlayerVictory req)
  case result of
    Left err -> throwError $ err400 { errBody = encodeUtf8 err }
    Right nextEntry -> pure $ EndCombatResponse (API.Models.endCombatPlayerVictory req) nextEntry
