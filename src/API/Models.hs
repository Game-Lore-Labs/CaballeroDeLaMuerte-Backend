{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Models
  ( SuccessResponse(..)
  , HealthResponse(..)
  , ModifierResponse(..)
  , UpdateNPCRequest(..)
  , InitiateCombatRequest(..)
  , CombatActionRequest(..)
  , EndCombatRequest(..)
  , EndCombatResponse(..)
  , ValidMoveResponse(..)
  , ParseDiceResponse(..)
  , AdventureSummary(..)
  , AdventureDetail(..)
  ) where

import Data.Text (Text)
import Data.Aeson
import GHC.Generics

data SuccessResponse = SuccessResponse
  { success :: Bool
  , message :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON SuccessResponse
instance FromJSON SuccessResponse

data HealthResponse = HealthResponse
  { status :: Text
  , version :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON HealthResponse
instance FromJSON HealthResponse

data ModifierResponse = ModifierResponse
  { abilityScore :: Int
  , modifier :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON ModifierResponse
instance FromJSON ModifierResponse

data UpdateNPCRequest = UpdateNPCRequest
  { updateNPCAffinityDelta :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON UpdateNPCRequest
instance FromJSON UpdateNPCRequest

data InitiateCombatRequest = InitiateCombatRequest
  { initiateCombatSessionId :: Text
  , initiateCombatEnemyIds :: [Text]
  } deriving (Show, Eq, Generic)

instance ToJSON InitiateCombatRequest
instance FromJSON InitiateCombatRequest

data CombatActionRequest = CombatActionRequest
  { actionType :: Text
  , actionParams :: Value
  } deriving (Show, Eq, Generic)

instance ToJSON CombatActionRequest
instance FromJSON CombatActionRequest

data EndCombatRequest = EndCombatRequest
  { endCombatPlayerVictory :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON EndCombatRequest
instance FromJSON EndCombatRequest

data EndCombatResponse = EndCombatResponse
  { victory :: Bool
  , nextEntryId :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON EndCombatResponse
instance FromJSON EndCombatResponse

data ValidMoveResponse = ValidMoveResponse
  { validMove :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON ValidMoveResponse
instance FromJSON ValidMoveResponse

data ParseDiceResponse = ParseDiceResponse
  { originalNotation :: Text
  , parsedRoll :: Value
  } deriving (Show, Eq, Generic)

instance ToJSON ParseDiceResponse
instance FromJSON ParseDiceResponse

data AdventureSummary = AdventureSummary
  { summaryId :: Text
  , summaryName :: Text
  , summaryDescription :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON AdventureSummary
instance FromJSON AdventureSummary

data AdventureDetail = AdventureDetail
  { detailId :: Text
  , detailName :: Text
  , detailDescription :: Text
  , detailStartEntry :: Text
  , detailEstimatedDuration :: Maybe Text
  , detailDifficulty :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON AdventureDetail
instance FromJSON AdventureDetail
