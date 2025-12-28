module Types where

import Data.Aeson
import GHC.Generics
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)





data Character = Character
  { charName :: Text
  , charClass :: CharClass
  , charLevel :: Int
  , charStats :: Stats
  , charInventory :: [Item]
  , charClues :: [Text]
  , charFlags :: Map Text Value
  , charRelations :: Map Text Relation
  , charXP :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON Character
instance FromJSON Character

data Stats = Stats
  { hp :: Int
  , maxHP :: Int
  , ac :: Int
  , str :: Int
  , dex :: Int
  , con :: Int
  , int :: Int
  , wis :: Int
  , cha :: Int
  , proficiencyBonus :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON Stats
instance FromJSON Stats


data GameState = GameState
  { sessionId :: Text
  , currentEntryId :: Text
  , character :: Character
  , history :: [HistoryEntry]
  , explorationState :: Maybe ExplorationState
  , combatState :: Maybe CombatState
  } deriving (Show, Eq, Generic)

instance ToJSON GameState
instance FromJSON GameState

data HistoryEntry = HistoryEntry
  { entryId :: Text
  , timestamp :: Int  -- Unix timestamp
  , choiceMade :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON HistoryEntry
instance FromJSON HistoryEntry

data Entry = Entry
  { entryId :: Text
  , entryType :: EntryType
  , narrative :: Narrative
  , transitions :: [Transition]
  , exploration :: Maybe ExplorationData
  , combat :: Maybe CombatData
  } deriving (Show, Eq, Generic)

instance ToJSON Entry
instance FromJSON Entry

data EntryType 
  = NarrativeEntry 
  | ExplorationEntry 
  | CombatEntry
  deriving (Show, Eq, Generic)

instance ToJSON EntryType
instance FromJSON EntryType

data Narrative = Narrative
  { title :: Text
  , text :: Text
  , videoClip :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON Narrative
instance FromJSON Narrative

data Transition = Transition
  { transitionId :: Text
  , transitionText :: Text
  , targetEntryId :: Text
  , conditions :: [Condition]
  , effects :: [Effect]
  } deriving (Show, Eq, Generic)

instance ToJSON Transition
instance FromJSON Transition