{-# LANGUAGE OverloadedStrings #-}

module API.Handlers.Dice
  ( handlers
  ) where

import Servant
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import API.Server (AppM)
import API.Types (DiceAPI)

import qualified Application.UseCases.DiceRoller as UC

handlers :: ServerT DiceAPI AppM
handlers =
  rollDiceHandler
  :<|> rollD20CheckHandler
  :<|> parseDiceNotationHandler
  :<|> rollMultipleHandler

rollDiceHandler :: UC.RollRequest -> AppM UC.RollResult
rollDiceHandler req = UC.rollDice req

rollD20CheckHandler :: UC.D20Check -> AppM UC.D20CheckResult
rollD20CheckHandler check = UC.rollD20Check check

parseDiceNotationHandler :: Text -> AppM API.Models.ParseDiceResponse
parseDiceNotationHandler notation = do
  case UC.parseDiceNotation notation of
    Nothing -> throwError $ err400 { errBody = "Invalid dice notation" }
    Just roll -> pure $ API.Models.ParseDiceResponse notation (API.Models.parsedRollFromValue roll)

rollMultipleHandler :: [UC.RollRequest] -> AppM [UC.RollResult]
rollMultipleHandler reqs = UC.rollMultiple reqs
