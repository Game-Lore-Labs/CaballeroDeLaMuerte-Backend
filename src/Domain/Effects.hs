{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain.Effects where

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)
import Domain.Character (Item, Clue, Relation)

data Effect
  = Damage Int
  | Heal Int
  | GainItem Item
  | LoseItem String
  | GainClue Clue
  | GainXP Int
  | ChangeRelation String Relation
  deriving (Show, Generic)

instance ToJSON Effect
instance FromJSON Effect
