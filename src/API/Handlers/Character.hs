{-# LANGUAGE OverloadedStrings #-}

module API.Handlers.Character
  ( handlers
  ) where

import Servant
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import API.Server (AppM)
import API.Types (CharacterAPI)

import qualified Application.UseCases.Character as UC
import API.Models (ModifierResponse(..))

handlers :: ServerT CharacterAPI AppM
handlers =
  getCharacterSheetHandler
  :<|> updateCharacterHandler
  :<|> getInventoryHandler
  :<|> addItemHandler
  :<|> removeItemHandler
  :<|> calculateModifierHandler

getCharacterSheetHandler :: Text -> AppM UC.CharacterSheet
getCharacterSheetHandler charId = do
  result <- UC.getCharacterSheet charId
  case result of
    Left err -> throwError $ err404 { errBody = encodeUtf8 err }
    Right sheet -> pure sheet

updateCharacterHandler :: Text -> UC.UpdateCharacterRequest -> AppM UC.CharacterSheet
updateCharacterHandler charId req = do
  let fullReq = req { UC.updateCharacterId = charId }
  result <- UC.updateCharacter fullReq
  case result of
    Left err -> throwError $ err400 { errBody = encodeUtf8 err }
    Right sheet -> pure sheet

getInventoryHandler :: Text -> AppM UC.CharacterInventory
getInventoryHandler charId = do
  result <- UC.getInventory charId
  case result of
    Left err -> throwError $ err404 { errBody = encodeUtf8 err }
    Right inventory -> pure inventory

addItemHandler :: Text -> UC.Item -> AppM UC.CharacterInventory
addItemHandler charId item = do
  result <- UC.addItemToInventory charId item
  case result of
    Left err -> throwError $ err400 { errBody = encodeUtf8 err }
    Right inventory -> pure inventory

removeItemHandler :: Text -> Text -> AppM UC.CharacterInventory
removeItemHandler charId itemId = do
  result <- UC.removeItemFromInventory charId itemId
  case result of
    Left err -> throwError $ err400 { errBody = encodeUtf8 err }
    Right inventory -> pure inventory

calculateModifierHandler :: Int -> AppM ModifierResponse
calculateModifierHandler score = do
  let modifier = UC.calculateModifier score
  pure $ ModifierResponse score modifier
