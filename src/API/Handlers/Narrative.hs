{-# LANGUAGE OverloadedStrings #-}

module API.Handlers.Narrative
  ( handlers
  ) where

import Servant
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import API.Server (AppM)
import API.Types (NarrativeAPI)

import qualified Application.UseCases.Narrative as UC

handlers :: ServerT NarrativeAPI AppM
handlers =
  getCurrentEntryHandler
  :<|> navigateHandler
  :<|> getHistoryHandler
  :<|> getCluesHandler
  :<|> addClueHandler
  :<|> getNPCRelationshipsHandler
  :<|> updateNPCRelationshipHandler

getCurrentEntryHandler :: Text -> AppM UC.EntryResponse
getCurrentEntryHandler sessionId = do
  result <- UC.getCurrentEntry sessionId
  case result of
    Left err -> throwError $ err404 { errBody = encodeUtf8 err }
    Right entry -> pure entry

navigateHandler :: Text -> UC.NavigateRequest -> AppM UC.EntryResponse
navigateHandler sessionId req = do
  let fullReq = req { UC.navigateSessionId = sessionId }
  result <- UC.navigateToEntry fullReq
  case result of
    Left err -> throwError $ err400 { errBody = encodeUtf8 err }
    Right entry -> pure entry

getHistoryHandler :: Text -> AppM UC.GameHistory
getHistoryHandler sessionId = do
  result <- UC.getGameHistory sessionId
  case result of
    Left err -> throwError $ err404 { errBody = encodeUtf8 err }
    Right history -> pure history

getCluesHandler :: Text -> AppM UC.ClueTracker
getCluesHandler sessionId = do
  result <- UC.getClues sessionId
  case result of
    Left err -> throwError $ err404 { errBody = encodeUtf8 err }
    Right clues -> pure clues

addClueHandler :: Text -> UC.Clue -> AppM UC.ClueTracker
addClueHandler sessionId clue = do
  result <- UC.addClue sessionId clue
  case result of
    Left err -> throwError $ err400 { errBody = encodeUtf8 err }
    Right clues -> pure clues

getNPCRelationshipsHandler :: Text -> AppM [UC.NPCRelationship]
getNPCRelationshipsHandler sessionId = do
  result <- UC.getNPCRelationships sessionId
  case result of
    Left err -> throwError $ err404 { errBody = encodeUtf8 err }
    Right npcs -> pure npcs

updateNPCRelationshipHandler :: Text -> Text -> API.Models.UpdateNPCRequest -> AppM UC.NPCRelationship
updateNPCRelationshipHandler sessionId npcId req = do
  result <- UC.updateNPCRelationship sessionId npcId (API.Models.updateNPCAffinityDelta req)
  case result of
    Left err -> throwError $ err400 { errBody = encodeUtf8 err }
    Right npc -> pure npc
