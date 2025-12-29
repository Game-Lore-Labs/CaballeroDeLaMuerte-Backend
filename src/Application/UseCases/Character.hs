{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Application.UseCases.Character
  ( CharacterService(..)
  , CharacterSheet(..)
  , AbilityScores(..)
  , UpdateCharacterRequest(..)
  , CharacterInventory(..)
  , Item(..)
  ) where

import Data.Text (Text)
import GHC.Generics

data AbilityScores = AbilityScores
  { strength :: Int
  , dexterity :: Int
  , constitution :: Int
  , intelligence :: Int
  , wisdom :: Int
  , charisma :: Int
  } deriving (Show, Eq, Generic)

data CharacterSheet = CharacterSheet
  { characterId :: Text
  , characterName :: Text
  , characterRace :: Text
  , characterClass :: Text
  , characterLevel :: Int
  , characterHP :: Int
  , characterMaxHP :: Int
  , characterAC :: Int
  , characterAbilities :: AbilityScores
  , characterProficiencies :: [Text]
  , characterFeatures :: [Text]
  } deriving (Show, Eq, Generic)

data CharacterInventory = CharacterInventory
  { inventoryCharacterId :: Text
  , inventoryItems :: [Item]
  , inventoryGold :: Int
  } deriving (Show, Eq, Generic)

data Item = Item
  { itemId :: Text
  , itemName :: Text
  , itemDescription :: Text
  , itemType :: Text
  , itemValue :: Int
  , itemQuantity :: Int
  } deriving (Show, Eq, Generic)

data UpdateCharacterRequest = UpdateCharacterRequest
  { updateCharacterId :: Text
  , updateHP :: Maybe Int
  , updateGold :: Maybe Int
  , updateAddItems :: Maybe [Item]
  , updateRemoveItems :: Maybe [Text]
  } deriving (Show, Eq, Generic)

class Monad m => CharacterService m where
  getCharacterSheet :: Text -> m (Either Text CharacterSheet)
  updateCharacter :: UpdateCharacterRequest -> m (Either Text CharacterSheet)
  getInventory :: Text -> m (Either Text CharacterInventory)
  addItemToInventory :: Text -> Item -> m (Either Text CharacterInventory)
  removeItemFromInventory :: Text -> Text -> m (Either Text CharacterInventory)
  calculateModifier :: Int -> Int
