{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain.Character where

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)
import Data.Map (Map)

data Character = Character
  { charName :: String
  , charClass :: CharClass
  , charHP :: Int
  , charMaxHP :: Int
  , charInventory :: [Item]
  , charClues :: [Clue]
  , charRelations :: Map String Relation
  , charXP :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON Character
instance FromJSON Character

data CharClass = Fighter | Wizard | Rogue | Cleric | Paladin
  deriving (Show, Eq, Generic)

instance ToJSON CharClass
instance FromJSON CharClass

data Item = Item
  { itemName :: String
  , itemValue :: Int
  , itemType :: ItemType
  } deriving (Show, Eq, Generic)

instance ToJSON Item
instance FromJSON Item

data ItemType = Weapon | Armor | Potion | Quest | Misc
  deriving (Show, Eq, Generic)

instance ToJSON ItemType
instance FromJSON ItemType

data Clue = Clue String deriving (Show, Eq, Generic)

instance ToJSON Clue
instance FromJSON Clue

data Relation = Friendly | Neutral | Hostile deriving (Show, Eq, Generic)

instance ToJSON Relation
instance FromJSON Relation
