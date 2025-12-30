{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Repository where

import Data.Aeson (eitherDecodeFileStrict, encodeFile, FromJSON, ToJSON, toJSON, parseJSON, object, withObject, (.=), (.:))
import qualified Data.Map as M
import Control.Exception (catch, IOException)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath (takeDirectory)

import Domain.Types
import Domain.Character
import Domain.Enemy
import Domain.Entry
import Infrastructure.JSON

-- | Repository operations result
data RepoResult a = RepoSuccess a | RepoError String
    deriving (Show, Eq)

-- | Base path for data storage
type DataPath = FilePath

-- PlayerCharacter Repository

-- | Save player character to JSON file
savePlayerCharacter :: DataPath -> PlayerCharacter -> IO (RepoResult ())
savePlayerCharacter path pc = do
    createDirectoryIfMissing True (takeDirectory path)
    catch (encodeFile path pc >> pure (RepoSuccess ()))
          (\e -> pure $ RepoError $ show (e :: IOException))

-- | Load player character from JSON file
loadPlayerCharacter :: DataPath -> IO (RepoResult PlayerCharacter)
loadPlayerCharacter path = do
    exists <- doesFileExist path
    if not exists
        then pure $ RepoError "Player character file not found"
        else do
            result <- eitherDecodeFileStrict path
            case result of
                Left err -> pure $ RepoError err
                Right pc -> pure $ RepoSuccess pc

-- Entry Repository

-- | Entries stored as a map from EntryId to EntryData
type EntryStore = M.Map EntryId EntryData

-- | Save all entries to JSON file
saveEntries :: DataPath -> EntryStore -> IO (RepoResult ())
saveEntries path entries = do
    createDirectoryIfMissing True (takeDirectory path)
    catch (encodeFile path (M.toList entries) >> pure (RepoSuccess ()))
          (\e -> pure $ RepoError $ show (e :: IOException))

-- | Load all entries from JSON file
loadEntries :: DataPath -> IO (RepoResult EntryStore)
loadEntries path = do
    exists <- doesFileExist path
    if not exists
        then pure $ RepoError "Entries file not found"
        else do
            result <- eitherDecodeFileStrict path
            case result of
                Left err -> pure $ RepoError err
                Right entries -> pure $ RepoSuccess (M.fromList entries)

-- | Get a single entry by ID
getEntry :: EntryStore -> EntryId -> Maybe Entry
getEntry store eid = toEntry <$> M.lookup eid store

-- | Get entry data by ID (for persistence operations)
getEntryData :: EntryStore -> EntryId -> Maybe EntryData
getEntryData = flip M.lookup

-- | Add or update an entry
putEntry :: EntryData -> EntryStore -> EntryStore
putEntry ed = M.insert (entryDataId ed) ed

-- Enemy Repository

-- | Enemy templates stored by name
type EnemyStore = M.Map String Enemy

-- | Save enemy templates to JSON file
saveEnemies :: DataPath -> EnemyStore -> IO (RepoResult ())
saveEnemies path enemies = do
    createDirectoryIfMissing True (takeDirectory path)
    catch (encodeFile path (M.toList enemies) >> pure (RepoSuccess ()))
          (\e -> pure $ RepoError $ show (e :: IOException))

-- | Load enemy templates from JSON file
loadEnemies :: DataPath -> IO (RepoResult EnemyStore)
loadEnemies path = do
    exists <- doesFileExist path
    if not exists
        then pure $ RepoError "Enemies file not found"
        else do
            result <- eitherDecodeFileStrict path
            case result of
                Left err -> pure $ RepoError err
                Right enemies -> pure $ RepoSuccess (M.fromList enemies)

-- | Get enemy template by name
getEnemy :: EnemyStore -> String -> Maybe Enemy
getEnemy = flip M.lookup

-- | Add or update enemy template
putEnemy :: Enemy -> EnemyStore -> EnemyStore
putEnemy e = M.insert (enemyName e) e

-- Game State Repository

-- | Serializable game state for save/load
data GameSaveData = GameSaveData
    { savePlayer       :: PlayerCharacter
    , saveCurrentEntry :: EntryId
    , saveHistory      :: [EntryId]
    } deriving (Show, Eq)

instance ToJSON GameSaveData where
    toJSON gs = object
        [ "player"       .= savePlayer gs
        , "currentEntry" .= saveCurrentEntry gs
        , "history"      .= saveHistory gs
        ]

instance FromJSON GameSaveData where
    parseJSON = withObject "GameSaveData" $ \v -> GameSaveData
        <$> v .: "player"
        <*> v .: "currentEntry"
        <*> v .: "history"

-- | Convert GameSaveData to GameState
toGameState :: GameSaveData -> GameState
toGameState gsd = GameState
    { gsPlayer       = savePlayer gsd
    , gsCurrentEntry = saveCurrentEntry gsd
    , gsHistory      = saveHistory gsd
    }

-- | Convert GameState to GameSaveData
fromGameState :: GameState -> GameSaveData
fromGameState gs = GameSaveData
    { savePlayer       = gsPlayer gs
    , saveCurrentEntry = gsCurrentEntry gs
    , saveHistory      = gsHistory gs
    }

-- | Save game state
saveGame :: DataPath -> GameState -> IO (RepoResult ())
saveGame path gs = do
    createDirectoryIfMissing True (takeDirectory path)
    catch (encodeFile path (fromGameState gs) >> pure (RepoSuccess ()))
          (\e -> pure $ RepoError $ show (e :: IOException))

-- | Load game state
loadGame :: DataPath -> IO (RepoResult GameState)
loadGame path = do
    exists <- doesFileExist path
    if not exists
        then pure $ RepoError "Save file not found"
        else do
            result <- eitherDecodeFileStrict path
            case result of
                Left err -> pure $ RepoError err
                Right gsd -> pure $ RepoSuccess (toGameState gsd)