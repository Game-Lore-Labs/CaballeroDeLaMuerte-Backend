{-# LANGUAGE OverloadedStrings #-}

module API.Handlers.Exploration
  ( handlers
  ) where

import Servant
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Maybe (fromMaybe)

import API.Server (AppM)
import API.Types (ExplorationAPI)
import API.Models (ValidMoveResponse(..))

import qualified Application.UseCases.Exploration as UC

handlers :: ServerT ExplorationAPI AppM
handlers =
  getExplorationStateHandler
  :<|> movePlayerHandler
  :<|> interactHandler
  :<|> getInteractablesHandler
  :<|> checkValidMoveHandler

getExplorationStateHandler :: Text -> AppM UC.ExplorationState
getExplorationStateHandler sessionId = do
  result <- UC.getExplorationState sessionId
  case result of
    Left err -> throwError $ err404 { errBody = encodeUtf8 err }
    Right state -> pure state

movePlayerHandler :: Text -> UC.MoveRequest -> AppM UC.ExplorationState
movePlayerHandler sessionId req = do
  let fullReq = req { UC.moveSessionId = sessionId }
  result <- UC.movePlayer fullReq
  case result of
    Left err -> throwError $ err400 { errBody = encodeUtf8 err }
    Right state -> pure state

interactHandler :: Text -> UC.InteractRequest -> AppM UC.InteractionResult
interactHandler sessionId req = do
  let fullReq = req { UC.interactSessionId = sessionId }
  result <- UC.interactWithObject fullReq
  case result of
    Left err -> throwError $ err400 { errBody = encodeUtf8 err }
    Right interactionResult -> pure interactionResult

getInteractablesHandler :: Text -> Maybe Int -> AppM [UC.Interactable]
getInteractablesHandler sessionId maybeRange = do
  let range = fromMaybe 5 maybeRange
  result <- UC.getInteractablesInRange sessionId range
  case result of
    Left err -> throwError $ err404 { errBody = encodeUtf8 err }
    Right interactables -> pure interactables

checkValidMoveHandler :: Text -> UC.MapPosition -> AppM ValidMoveResponse
checkValidMoveHandler sessionId pos = do
  result <- UC.canMoveTo sessionId pos
  case result of
    Left err -> throwError $ err400 { errBody = encodeUtf8 err }
    Right valid -> pure $ ValidMoveResponse valid
