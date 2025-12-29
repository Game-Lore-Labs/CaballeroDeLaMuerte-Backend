{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Application.UseCases.Effects
  ( Effect(..)
  , Clue(..)
  ) where

import GHC.Generics
import Data.Aeson (Value)
import Data.Time (UTCTime)
import Data.Text (Text)

data Effect = Effect
  { effectType :: Text
  , effectParams :: Value
  } deriving (Show, Eq, Generic)

data Clue = Clue
  { clueId :: Text
  , clueName :: Text
  , clueDescription :: Text
  , clueDiscoveredAt :: UTCTime
  } deriving (Show, Eq, Generic)
