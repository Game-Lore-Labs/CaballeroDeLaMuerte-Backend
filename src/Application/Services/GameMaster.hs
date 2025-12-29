{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Application.Services.GameMaster
  ( GameMasterService(..)
  , GMDecision(..)
  , GMContext(..)
  , ProcessChoiceRequest(..)
  ) where

import Data.Text (Text)
import GHC.Generics
import Application.UseCases.Character (CharacterSheet, CharacterInventory)
import Application.UseCases.Narrative (Clue, NPCRelationship)
import Application.UseCases.Effects (Effect)

data GMContext = GMContext
  { gmSessionId :: Text
  , gmCurrentEntry :: Text
  , gmCharacterState :: CharacterSheet
  , gmInventory :: CharacterInventory
  , gmClues :: [Clue]
  , gmNPCRelationships :: [NPCRelationship]
  , gmHistory :: [Text]
  } deriving (Show, Eq, Generic)

data GMDecision = GMDecision
  { gmDecisionType :: Text
  , gmDecisionDescription :: Text
  , gmDecisionEffects :: [Effect]
  , gmDecisionNextEntry :: Maybe Text
  } deriving (Show, Eq, Generic)

data ProcessChoiceRequest = ProcessChoiceRequest
  { processChoiceSessionId :: Text
  , processChoiceChoiceId :: Text
  } deriving (Show, Eq, Generic)

class Monad m => GameMasterService m where
  processChoice :: ProcessChoiceRequest -> m (Either Text GMDecision)
  applyEffects :: Text -> [Effect] -> m (Either Text ())
  validateChoice :: Text -> Text -> m (Either Text Bool)
  getNarration :: GMContext -> m (Either Text Text)
