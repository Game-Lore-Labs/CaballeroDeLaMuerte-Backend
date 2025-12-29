{-# LANGUAGE OverloadedStrings #-}

module API.Handlers.Session
  ( handlers
  ) where

import Servant
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)
import Data.Text.Encoding (encodeUtf8)

import API.Server (AppM)
import API.Types (SessionAPI)
import API.Models (SuccessResponse(..))

import qualified Application.UseCases.GameSession as UC

handlers :: ServerT SessionAPI AppM
handlers = 
  createSessionHandler
  :<|> getSessionHandler
  :<|> saveGameHandler
  :<|> loadGameHandler
  :<|> deleteSessionHandler

createSessionHandler :: UC.CreateSessionRequest -> AppM UC.SessionResponse
createSessionHandler req = do
  result <- UC.createSession req
  case result of
    Left err -> throwError $ err400 { errBody = encodeUtf8 err }
    Right session -> pure session

getSessionHandler :: Text -> AppM UC.SessionResponse
getSessionHandler sessionId = do
  result <- UC.getSession sessionId
  case result of
    Left err -> throwError $ err404 { errBody = encodeUtf8 err }
    Right session -> pure session

saveGameHandler :: Text -> AppM SuccessResponse
saveGameHandler sessionId = do
  let req = UC.SaveGameRequest sessionId ""
  result <- UC.saveGameState req
  case result of
    Left err -> throwError $ err500 { errBody = encodeUtf8 err }
    Right _ -> pure $ SuccessResponse True "Game saved successfully"

loadGameHandler :: Text -> AppM UC.SessionResponse
loadGameHandler sessionId = do
  let req = UC.LoadGameRequest sessionId ""
  result <- UC.loadGameState req
  case result of
    Left err -> throwError $ err404 { errBody = encodeUtf8 err }
    Right session -> pure session

deleteSessionHandler :: Text -> AppM SuccessResponse
deleteSessionHandler sessionId = do
  result <- UC.deleteSession sessionId
  case result of
    Left err -> throwError $ err500 { errBody = encodeUtf8 err }
    Right _ -> pure $ SuccessResponse True "Session deleted successfully"
