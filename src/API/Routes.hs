{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Routes
  ( API
  , api
  , server
  ) where

import Servant
import Data.Text (Text)
import Data.Proxy

import API.Types
import API.Models (HealthResponse(..))
import API.Handlers.Session as Session
import API.Handlers.Character as Character
import API.Handlers.Narrative as Narrative
import API.Handlers.Combat as Combat
import API.Handlers.Exploration as Exploration
import API.Handlers.Dice as Dice
import API.Handlers.Adventure as Adventure
import API.Server (AppM)

type API = "api" :> "v1" :>
  ( SessionAPI
    :<|> CharacterAPI
    :<|> NarrativeAPI
    :<|> CombatAPI
    :<|> ExplorationAPI
    :<|> DiceAPI
    :<|> AdventureAPI
    :<|> HealthAPI
  )

api :: Proxy API
api = Proxy

healthHandler :: AppM HealthResponse
healthHandler = pure $ HealthResponse "ok" "1.0.0"

server :: ServerT API AppM
server =
  Session.handlers
  :<|> Character.handlers
  :<|> Narrative.handlers
  :<|> Combat.handlers
  :<|> Exploration.handlers
  :<|> Dice.handlers
  :<|> Adventure.handlers
  :<|> healthHandler
