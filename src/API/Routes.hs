{-# LANGUAGE OverloadedStrings #-}

module API.Routes where

import Web.Scotty
import Data.IORef
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types.Status
import Data.Aeson (FromJSON)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import Domain.Types
import Domain.Dice
import Domain.StatBlock
import Domain.Character
import Domain.Enemy
import Domain.Entry hiding (selectOption)
import Application.GameService
import Application.AdventureService
import Application.CombatService
import Application.CharacterService
import qualified API.DTO as DTO
import API.DTO (AbilityCheckRequest(..), SavingThrowRequest(..), CombatAttackRequest(..), 
                DiceRollRequest(..), SelectOptionRequest(..), okResponse, errResponse,
                GameStateDTO(..), EntryDTO(..), OptionResultDTO(..), CheckResultDTO(..),
                CombatStatusDTO(..), CombatActionDTO(..), DiceRollDTO(..),
                fromCharacter, fromCharacterSheet, fromItem, fromOption, fromEnemy)
import API.Swagger

-- | Server state holding app state and optional combat state
data ServerState = ServerState
    { serverAppState    :: Maybe AppState
    , serverCombatState :: Maybe CombatState
    }

-- | Initialize empty server state
emptyServerState :: ServerState
emptyServerState = ServerState Nothing Nothing

-- | Parse JSON body with error handling
parseBody :: FromJSON a => ActionM a
parseBody = jsonData `rescue` (\_ -> do
    status badRequest400
    json (errResponse "Invalid JSON body" :: DTO.ApiResponse String)
    finish)

-- | Require app state to be initialized
requireAppState :: IORef ServerState -> ActionM AppState
requireAppState stateRef = do
    serverState <- liftIO $ readIORef stateRef
    case serverAppState serverState of
        Nothing -> do
            status badRequest400
            json (errResponse "Game not initialized" :: DTO.ApiResponse String)
            finish
        Just appState -> pure appState

-- | Require combat state
requireCombatState :: IORef ServerState -> ActionM CombatState
requireCombatState stateRef = do
    serverState <- liftIO $ readIORef stateRef
    case serverCombatState serverState of
        Nothing -> do
            status badRequest400
            json (errResponse "Not in combat" :: DTO.ApiResponse String)
            finish
        Just combatState -> pure combatState

-- | Update app state
updateAppState :: IORef ServerState -> AppState -> ActionM ()
updateAppState stateRef newState = liftIO $ modifyIORef stateRef $ \s ->
    s { serverAppState = Just newState }

-- | Update combat state
updateCombatState :: IORef ServerState -> Maybe CombatState -> ActionM ()
updateCombatState stateRef newState = liftIO $ modifyIORef stateRef $ \s ->
    s { serverCombatState = newState }

-- | Define all routes
routes :: IORef ServerState -> AppConfig -> ScottyM ()
routes stateRef config = do

    -- Health check
    get "/health" $ do
        json $ okResponse ("OK" :: String)

    -- API Documentation (Swagger)
    get "/api/docs" $ do
        setHeader "Content-Type" "text/html; charset=utf-8"
        raw $ TLE.encodeUtf8 swaggerUIHtml

    get "/api/openapi.json" $ do
        json openApiSpec

    -- Redirect root to docs
    get "/" $ do
        redirect "/api/docs"

    -- Game Management

    get "/game/state" $ do
        appState <- requireAppState stateRef
        let player = getPlayer appState
            dto = GameStateDTO
                { gameCurrentEntry = getCurrentEntryId appState
                , gamePlayer       = fromCharacter player
                , gameHistory      = gsHistory (appGameState appState)
                }
        json $ okResponse dto

    post "/game/save" $ do
        appState <- requireAppState stateRef
        result <- liftIO $ saveGameState config appState
        case result of
            ServiceOk _  -> json $ okResponse ("Game saved" :: String)
            ServiceErr e -> do
                status internalServerError500
                json (errResponse e :: DTO.ApiResponse String)

    post "/game/load" $ do
        result <- liftIO $ initializeApp config
        case result of
            ServiceOk appState -> do
                updateAppState stateRef appState
                json $ okResponse ("Game loaded" :: String)
            ServiceErr e -> do
                status internalServerError500
                json (errResponse e :: DTO.ApiResponse String)

    -- Entry / Adventure

    get "/entry/current" $ do
        appState <- requireAppState stateRef
        let narrative = getCurrentNarrative appState
            options = getAvailableOptions appState
            dto = EntryDTO
                { entryDtoId        = getCurrentEntryId appState
                , entryDtoNarrative = narrative
                , entryDtoOptions   = map fromOption options
                }
        json $ okResponse dto

    post "/entry/select" $ do
        appState <- requireAppState stateRef
        req <- parseBody :: ActionM SelectOptionRequest
        (result, newState) <- liftIO $ selectOption (selectOptionId req) appState
        updateAppState stateRef newState
        let dto = case result of
                NavigatedTo eid -> OptionResultDTO "navigated" Nothing Nothing (Just eid) Nothing Nothing
                CheckPassed skill roll dc eid -> OptionResultDTO "check_passed" (Just roll) (Just dc) (Just eid) (Just $ show skill) Nothing
                CheckFailed skill roll dc eid -> OptionResultDTO "check_failed" (Just roll) (Just dc) (Just eid) (Just $ show skill) Nothing
                SavePassed attr roll dc eid -> OptionResultDTO "save_passed" (Just roll) (Just dc) (Just eid) Nothing (Just $ show attr)
                SaveFailed attr roll dc eid -> OptionResultDTO "save_failed" (Just roll) (Just dc) (Just eid) Nothing (Just $ show attr)
                CombatStarted _ _ -> OptionResultDTO "combat_started" Nothing Nothing Nothing Nothing Nothing
                OptionNotFound -> OptionResultDTO "error" Nothing Nothing Nothing Nothing Nothing
                EntryNotFound -> OptionResultDTO "error" Nothing Nothing Nothing Nothing Nothing
        json $ okResponse dto

    -- Character

    get "/character" $ do
        appState <- requireAppState stateRef
        json $ okResponse $ fromCharacter (getPlayer appState)

    get "/character/sheet" $ do
        appState <- requireAppState stateRef
        json $ okResponse $ fromCharacterSheet (getPlayer appState)

    get "/character/inventory" $ do
        appState <- requireAppState stateRef
        let items = map fromItem $ getPlayerInventory appState
        json $ okResponse items

    get "/character/clues" $ do
        appState <- requireAppState stateRef
        let clues = getPlayerClues appState
        json $ okResponse clues

    post "/character/check" $ do
        appState <- requireAppState stateRef
        req <- parseBody :: ActionM AbilityCheckRequest
        (info, _) <- liftIO $ performAbilityCheck (checkSkill req) (DTO.checkDC req) appState
        let dto = CheckResultDTO
                { checkResultSkill   = checkSkillOrAttr info
                , checkResultRoll    = checkRoll info
                , checkResultBonus   = checkBonus info
                , checkResultTotal   = checkTotal info
                , checkResultDC      = Application.CharacterService.checkDC info
                , checkResultSuccess = checkSuccess info
                }
        json $ okResponse dto

    post "/character/save" $ do
        appState <- requireAppState stateRef
        req <- parseBody :: ActionM SavingThrowRequest
        (info, _) <- liftIO $ performSavingThrow (saveAttribute req) (DTO.saveDC req) appState
        let dto = CheckResultDTO
                { checkResultSkill   = checkSkillOrAttr info
                , checkResultRoll    = checkRoll info
                , checkResultBonus   = checkBonus info
                , checkResultTotal   = checkTotal info
                , checkResultDC      = Application.CharacterService.checkDC info
                , checkResultSuccess = checkSuccess info
                }
        json $ okResponse dto

    -- Combat

    post "/combat/start" $ do
        appState <- requireAppState stateRef
        -- For now, combat is started via entry options
        -- This endpoint expects enemies to be set up already
        serverState <- liftIO $ readIORef stateRef
        case serverCombatState serverState of
            Just combat -> do
                let dto = CombatStatusDTO
                        { combatStatusState   = show $ getCombatStatus combat
                        , combatStatusPlayer  = fromCharacter (combatPlayer combat)
                        , combatStatusEnemies = map fromEnemy (combatEnemies combat)
                        }
                json $ okResponse dto
            Nothing -> do
                status badRequest400
                json (errResponse "No combat initialized" :: DTO.ApiResponse String)

    get "/combat/status" $ do
        combat <- requireCombatState stateRef
        let dto = CombatStatusDTO
                { combatStatusState   = show $ getCombatStatus combat
                , combatStatusPlayer  = fromCharacter (combatPlayer combat)
                , combatStatusEnemies = map fromEnemy (getAliveEnemies combat)
                }
        json $ okResponse dto

    post "/combat/attack" $ do
        combat <- requireCombatState stateRef
        req <- parseBody :: ActionM CombatAttackRequest
        let player = combatPlayer combat
            weaponList = weapons (pcStatBlock player)
        if attackWeaponIndex req >= length weaponList
            then do
                status badRequest400
                json (errResponse "Invalid weapon index" :: DTO.ApiResponse String)
            else do
                let weapon = weaponList !! attackWeaponIndex req
                (result, newCombat) <- liftIO $ playerAttack (attackTargetIndex req) weapon combat
                updateCombatState stateRef (Just newCombat)
                let dto = case result of
                        PlayerHit dmg -> CombatActionDTO "player_hit" Nothing (Just dmg) Nothing
                        PlayerMiss roll -> CombatActionDTO "player_miss" Nothing Nothing (Just roll)
                        EnemyDefeated name -> CombatActionDTO "enemy_defeated" (Just name) Nothing Nothing
                        _ -> CombatActionDTO "unknown" Nothing Nothing Nothing
                json $ okResponse dto

    post "/combat/enemy-turn" $ do
        combat <- requireCombatState stateRef
        (results, newCombat) <- liftIO $ allEnemiesAttack combat
        updateCombatState stateRef (Just newCombat)
        let dtos = map toActionDTO results
            toActionDTO r = case r of
                EnemyHit name dmg -> CombatActionDTO "enemy_hit" (Just name) (Just dmg) Nothing
                EnemyMiss name roll -> CombatActionDTO "enemy_miss" (Just name) Nothing (Just roll)
                PlayerDefeated -> CombatActionDTO "player_defeated" Nothing Nothing Nothing
                _ -> CombatActionDTO "unknown" Nothing Nothing Nothing
        json $ okResponse dtos

    post "/combat/end" $ do
        combat <- requireCombatState stateRef
        appState <- requireAppState stateRef
        let (destEntry, updatedPlayer) = endCombat combat
            newAppState = updatePlayer (const updatedPlayer) $ goToEntry destEntry appState
        updateAppState stateRef newAppState
        updateCombatState stateRef Nothing
        json $ okResponse $ GameStateDTO
            { gameCurrentEntry = destEntry
            , gamePlayer       = fromCharacter updatedPlayer
            , gameHistory      = gsHistory (appGameState newAppState)
            }

    -- Dice

    post "/dice/roll" $ do
        req <- parseBody :: ActionM DiceRollRequest
        result <- liftIO $ roll (rollDiceType req) (rollDiceCount req) (rollDiceBonus req)
        let dto = DiceRollDTO
                { diceValues = rollValues result
                , diceBonus  = rollBonus result
                , diceTotal  = rollTotal result
                }
        json $ okResponse dto