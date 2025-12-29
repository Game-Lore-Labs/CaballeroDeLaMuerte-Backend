{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Application.UseCases.GameSession
  ( GameSessionService(..)
  , CreateSessionRequest(..)
  , SessionResponse(..)
  , SaveGameRequest(..)
  , LoadGameRequest(..)
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics

data CreateSessionRequest = CreateSessionRequest
  { createSessionPlayerId :: Text
  , createSessionAdventureId :: Text
  , createSessionCharacterId :: Maybe Text
  } deriving (Show, Eq, Generic)

data SessionResponse = SessionResponse
  { sessionId :: Text
  , sessionPlayerId :: Text
  , sessionAdventureId :: Text
  , sessionCurrentEntryId :: Text
  , sessionCreatedAt :: UTCTime
  , sessionLastUpdated :: UTCTime
  } deriving (Show, Eq, Generic)

data SaveGameRequest = SaveGameRequest
  { saveGameSessionId :: Text
  , saveGamePlayerId :: Text
  } deriving (Show, Eq, Generic)

data LoadGameRequest = LoadGameRequest
  { loadGameSessionId :: Text
  , loadGamePlayerId :: Text
  } deriving (Show, Eq, Generic)

class Monad m => GameSessionService m where
  createSession :: CreateSessionRequest -> m (Either Text SessionResponse)
  getSession :: Text -> m (Either Text SessionResponse)
  saveGameState :: SaveGameRequest -> m (Either Text ())
  loadGameState :: LoadGameRequest -> m (Either Text SessionResponse)
  deleteSession :: Text -> m (Either Text ())
