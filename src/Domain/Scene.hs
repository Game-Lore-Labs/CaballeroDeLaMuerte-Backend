{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain.Scene where

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)
import Domain.Combat (Enemy)

type SceneId = String

data Scene = Scene
  { sceneId :: SceneId
  , sceneType :: SceneType
  , narrative :: Narrative
  } deriving (Show, Generic)

instance ToJSON Scene
instance FromJSON Scene

data SceneType = Exploration ExplorationScene | Combat CombatScene
  deriving (Show, Generic)

instance ToJSON SceneType
instance FromJSON SceneType

data Narrative = Narrative
  { title :: String
  , text :: String
  } deriving (Show, Generic)

instance ToJSON Narrative
instance FromJSON Narrative

data ExplorationScene = ExplorationScene
  { mapId :: String
  , gridSize :: GridSize
  , playerStartPos :: Position
  , interactables :: [Interactable]
  } deriving (Show, Generic)

instance ToJSON ExplorationScene
instance FromJSON ExplorationScene

data GridSize = GridSize
  { width :: Int
  , height :: Int
  } deriving (Show, Generic)

instance ToJSON GridSize
instance FromJSON GridSize

data Position = Position
  { x :: Int
  , y :: Int
  } deriving (Show, Generic)

instance ToJSON Position
instance FromJSON Position

data Interactable = Interactable
  { interactableId :: String
  , pos :: Position
  , interactableType :: InteractableType
  , interactableName :: String
  , entryId :: SceneId
  } deriving (Show, Generic)

instance ToJSON Interactable
instance FromJSON Interactable

data InteractableType = Object | Trigger
  deriving (Show, Generic)

instance ToJSON InteractableType
instance FromJSON InteractableType

data CombatScene = CombatScene
  { enemies :: [Enemy]
  , onVictory :: SceneId
  , onDefeat :: SceneId
  } deriving (Show, Generic)

instance ToJSON CombatScene
instance FromJSON CombatScene
