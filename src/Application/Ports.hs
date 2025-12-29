{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Application.Ports
  ( SessionRepository(..)
  , CharacterRepository(..)
  , AdventureRepository(..)
  , InventoryRepository(..)
  , AIService(..)
  , RandomGenerator(..)
  , Adventure(..)
  ) where

import Data.Text (Text)
import GHC.Generics
import Application.UseCases.Character (CharacterSheet, CharacterInventory)
import Application.UseCases.GameSession (SessionResponse)
import Application.UseCases.Narrative (EntryResponse)

class Monad m => SessionRepository m where
  saveSession :: SessionResponse -> m (Either Text ())
  findSession :: Text -> m (Either Text (Maybe SessionResponse))
  updateSession :: Text -> SessionResponse -> m (Either Text ())
  deleteSessionById :: Text -> m (Either Text ())

class Monad m => CharacterRepository m where
  saveCharacter :: CharacterSheet -> m (Either Text ())
  findCharacter :: Text -> m (Either Text (Maybe CharacterSheet))
  updateCharacterState :: Text -> CharacterSheet -> m (Either Text ())

class Monad m => AdventureRepository m where
  findEntry :: Text -> m (Either Text (Maybe EntryResponse))
  findAdventure :: Text -> m (Either Text (Maybe Adventure))
  listAdventures :: m (Either Text [Adventure])

data Adventure = Adventure
  { adventureId :: Text
  , adventureName :: Text
  , adventureDescription :: Text
  , adventureStartEntry :: Text
  } deriving (Show, Eq, Generic)

class Monad m => InventoryRepository m where
  saveInventory :: CharacterInventory -> m (Either Text ())
  findInventory :: Text -> m (Either Text (Maybe CharacterInventory))
  updateInventory :: Text -> CharacterInventory -> m (Either Text ())

class Monad m => AIService m where
  generateNarration :: a -> Text -> m (Either Text Text)
  generateDescription :: Text -> m (Either Text Text)

class Monad m => RandomGenerator m where
  generateRandom :: Int -> Int -> m Int
  generateRandoms :: Int -> Int -> Int -> m [Int]
