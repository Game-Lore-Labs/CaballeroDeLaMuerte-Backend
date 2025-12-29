{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Application.UseCases.Narrative
  ( NarrativeService(..)
  , EntryResponse(..)
  , Choice(..)
  , NavigateRequest(..)
  , GameHistory(..)
  , ClueTracker(..)
  , NPCRelationship(..)
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics
import Data.Aeson (Value)
import Application.UseCases.Effects (Effect, Clue)

data Choice = Choice
  { choiceId :: Text
  , choiceText :: Text
  , choiceRequirements :: Maybe [Text]
  , choiceTargetEntry :: Text
  , choiceEffects :: [Effect]
  } deriving (Show, Eq, Generic)

data EntryResponse = EntryResponse
  { entryId :: Text
  , entryType :: Text
  , entryTitle :: Text
  , entryNarrative :: Text
  , entryChoices :: [Choice]
  , entryImageUrl :: Maybe Text
  } deriving (Show, Eq, Generic)

data NavigateRequest = NavigateRequest
  { navigateSessionId :: Text
  , navigateChoiceId :: Text
  , navigateFromEntryId :: Text
  } deriving (Show, Eq, Generic)

data GameHistory = GameHistory
  { historySessionId :: Text
  , historyEntries :: [(UTCTime, Text, Text)]
  } deriving (Show, Eq, Generic)

data ClueTracker = ClueTracker
  { cluesSessionId :: Text
  , cluesDiscovered :: [Clue]
  } deriving (Show, Eq, Generic)

data NPCRelationship = NPCRelationship
  { npcId :: Text
  , npcName :: Text
  , npcAffinity :: Int
  , npcStatus :: Text
  , npcInteractions :: [Text]
  } deriving (Show, Eq, Generic)

class Monad m => NarrativeService m where
  getCurrentEntry :: Text -> m (Either Text EntryResponse)
  navigateToEntry :: NavigateRequest -> m (Either Text EntryResponse)
  getGameHistory :: Text -> m (Either Text GameHistory)
  addClue :: Text -> Clue -> m (Either Text ClueTracker)
  getClues :: Text -> m (Either Text ClueTracker)
  updateNPCRelationship :: Text -> Text -> Int -> m (Either Text NPCRelationship)
  getNPCRelationships :: Text -> m (Either Text [NPCRelationship])
