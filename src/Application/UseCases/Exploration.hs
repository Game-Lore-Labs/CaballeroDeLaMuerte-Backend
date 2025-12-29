{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Application.UseCases.Exploration
  ( ExplorationService(..)
  , ExplorationState(..)
  , MapPosition(..)
  , MoveRequest(..)
  , InteractRequest(..)
  , InteractionResult(..)
  , Interactable(..)
  , Direction(..)
  ) where

import Data.Text (Text)
import GHC.Generics
import Application.UseCases.Effects (Effect)

data MapPosition = MapPosition
  { posX :: Int
  , posY :: Int
  } deriving (Show, Eq, Generic)

data MapTile = MapTile
  { tilePosition :: MapPosition
  , tileType :: Text
  , tileIsPassable :: Bool
  , tileInteractableId :: Maybe Text
  } deriving (Show, Eq, Generic)

data ExplorationState = ExplorationState
  { explorationSessionId :: Text
  , explorationMapId :: Text
  , explorationPlayerPosition :: MapPosition
  , explorationMapSize :: (Int, Int)
  , explorationVisibleTiles :: [MapTile]
  , explorationInteractables :: [Interactable]
  } deriving (Show, Eq, Generic)

data Interactable = Interactable
  { interactableId :: Text
  , interactableName :: Text
  , interactablePosition :: MapPosition
  , interactableType :: Text
  , interactableDescription :: Text
  } deriving (Show, Eq, Generic)

data MoveRequest = MoveRequest
  { moveSessionId :: Text
  , moveDirection :: Direction
  } deriving (Show, Eq, Generic)

data Direction = North | South | East | West
  deriving (Show, Eq, Generic)

data InteractRequest = InteractRequest
  { interactSessionId :: Text
  , interactInteractableId :: Text
  } deriving (Show, Eq, Generic)

data InteractionResult = InteractionResult
  { interactionSuccess :: Bool
  , interactionDescription :: Text
  , interactionEffects :: [Effect]
  , interactionNextEntryId :: Maybe Text
  } deriving (Show, Eq, Generic)

class Monad m => ExplorationService m where
  getExplorationState :: Text -> m (Either Text ExplorationState)
  movePlayer :: MoveRequest -> m (Either Text ExplorationState)
  interactWithObject :: InteractRequest -> m (Either Text InteractionResult)
  getInteractablesInRange :: Text -> Int -> m (Either Text [Interactable])
  canMoveTo :: Text -> MapPosition -> m (Either Text Bool)
