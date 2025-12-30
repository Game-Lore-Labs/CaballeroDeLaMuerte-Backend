module Application.GameService where

import qualified Data.Map as M

import Domain.Types
import Domain.Character
import Domain.Entry
import Infrastructure.JSON
import Infrastructure.Repository

-- | Application configuration paths
data AppConfig = AppConfig
    { configSavePath    :: FilePath
    , configEntriesPath :: FilePath
    , configEnemiesPath :: FilePath
    } deriving (Show, Eq)

-- | Default configuration
defaultConfig :: AppConfig
defaultConfig = AppConfig
    { configSavePath    = "data/save.json"
    , configEntriesPath = "data/entries.json"
    , configEnemiesPath = "data/enemies.json"
    }

-- | Application state holding loaded data
data AppState = AppState
    { appGameState  :: GameState
    , appEntries    :: EntryStore
    , appEnemies    :: EnemyStore
    } deriving (Show, Eq)

-- | Service result type
data ServiceResult a = ServiceOk a | ServiceErr String
    deriving (Show, Eq)

-- | Convert RepoResult to ServiceResult
fromRepo :: RepoResult a -> ServiceResult a
fromRepo (RepoSuccess a) = ServiceOk a
fromRepo (RepoError e)   = ServiceErr e

-- Game Management Use Cases

-- | Initialize a new game with a player character
newGame :: PlayerCharacter -> EntryId -> EntryStore -> EnemyStore -> AppState
newGame player startEntry entries enemies = AppState
    { appGameState = GameState
        { gsPlayer       = player
        , gsCurrentEntry = startEntry
        , gsHistory      = []
        }
    , appEntries = entries
    , appEnemies = enemies
    }

-- | Save current game state
saveGameState :: AppConfig -> AppState -> IO (ServiceResult ())
saveGameState config state = fromRepo <$> saveGame (configSavePath config) (appGameState state)

-- | Load game state (entries and enemies must be loaded separately)
loadGameState :: AppConfig -> IO (ServiceResult GameState)
loadGameState config = fromRepo <$> loadGame (configSavePath config)

-- | Load all entries from file
loadAllEntries :: AppConfig -> IO (ServiceResult EntryStore)
loadAllEntries config = fromRepo <$> loadEntries (configEntriesPath config)

-- | Load all enemies from file
loadAllEnemies :: AppConfig -> IO (ServiceResult EnemyStore)
loadAllEnemies config = fromRepo <$> loadEnemies (configEnemiesPath config)

-- | Full application initialization - load everything
initializeApp :: AppConfig -> IO (ServiceResult AppState)
initializeApp config = do
    gameResult    <- loadGameState config
    entriesResult <- loadAllEntries config
    enemiesResult <- loadAllEnemies config
    pure $ case (gameResult, entriesResult, enemiesResult) of
        (ServiceOk gs, ServiceOk entries, ServiceOk enemies) ->
            ServiceOk $ AppState gs entries enemies
        (ServiceErr e, _, _) -> ServiceErr $ "Game: " ++ e
        (_, ServiceErr e, _) -> ServiceErr $ "Entries: " ++ e
        (_, _, ServiceErr e) -> ServiceErr $ "Enemies: " ++ e

-- State Accessors

-- | Get current player character
getPlayer :: AppState -> PlayerCharacter
getPlayer = gsPlayer . appGameState

-- | Get current entry ID
getCurrentEntryId :: AppState -> EntryId
getCurrentEntryId = gsCurrentEntry . appGameState

-- | Get current entry with rules evaluated
getCurrentEntry :: AppState -> Maybe Entry
getCurrentEntry state = getEntry (appEntries state) (getCurrentEntryId state)

-- | Get available options for current entry (with rules applied)
getAvailableOptions :: AppState -> [Option]
getAvailableOptions state = case getCurrentEntry state of
    Nothing    -> []
    Just entry -> evaluateRules (appGameState state) entry

-- | Get narrative for current entry (with rules applied)
getCurrentNarrative :: AppState -> String
getCurrentNarrative state = case getCurrentEntry state of
    Nothing    -> "Entry not found."
    Just entry -> evaluateNarrative (appGameState state) entry

-- State Modifiers

-- | Update player in app state
updatePlayer :: (PlayerCharacter -> PlayerCharacter) -> AppState -> AppState
updatePlayer f state = state
    { appGameState = (appGameState state)
        { gsPlayer = f (gsPlayer (appGameState state)) }
    }

-- | Move to a new entry
goToEntry :: EntryId -> AppState -> AppState
goToEntry newEntry state = state
    { appGameState = (appGameState state)
        { gsCurrentEntry = newEntry
        , gsHistory      = gsCurrentEntry (appGameState state) : gsHistory (appGameState state)
        }
    }

-- | Check if an entry exists
entryExists :: EntryId -> AppState -> Bool
entryExists eid state = M.member eid (appEntries state)
