{-# LANGUAGE OverloadedStrings #-}

module API.Handlers.Adventure
  ( handlers
  ) where

import Servant
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import API.Server (AppM)
import API.Types (AdventureAPI)
import API.Models (AdventureSummary, AdventureDetail)

import qualified Application.Ports as Ports

handlers :: ServerT AdventureAPI AppM
handlers =
  listAdventuresHandler
  :<|> getAdventureHandler
  :<|> getEntryHandler

listAdventuresHandler :: AppM [AdventureSummary]
listAdventuresHandler = do
  result <- Ports.listAdventures
  case result of
    Left err -> throwError $ err500 { errBody = encodeUtf8 err }
    Right adventures -> pure $ map toSummary adventures

getAdventureHandler :: Text -> AppM AdventureDetail
getAdventureHandler adventureId = do
  result <- Ports.findAdventure adventureId
  case result of
    Left err -> throwError $ err500 { errBody = encodeUtf8 err }
    Right Nothing -> throwError err404
    Right (Just adventure) -> pure $ toDetail adventure

getEntryHandler :: Text -> Text -> AppM Ports.EntryResponse
getEntryHandler _ entryId = do
  result <- Ports.findEntry entryId
  case result of
    Left err -> throwError $ err500 { errBody = encodeUtf8 err }
    Right Nothing -> throwError err404
    Right (Just entry) -> pure entry

toSummary :: Ports.Adventure -> AdventureSummary
toSummary adv = AdventureSummary (Ports.adventureId adv) (Ports.adventureName adv) (Ports.adventureDescription adv)

toDetail :: Ports.Adventure -> AdventureDetail
toDetail adv = AdventureDetail (Ports.adventureId adv) (Ports.adventureName adv) (Ports.adventureDescription adv) (Ports.adventureStartEntry adv) Nothing Nothing
