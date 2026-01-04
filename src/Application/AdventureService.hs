module Application.AdventureService where

import Domain.Types
import Domain.Character
import Domain.StatBlock (getSkillBonus, getSavingThrowBonus)
import Domain.Entry
import Domain.Dice
import Application.GameService

-- | Result of selecting an option
data OptionResult
    = NavigatedTo EntryId                        -- Direct navigation
    | CheckPassed Skill Int Int EntryId          -- Skill, roll, DC, destination
    | CheckFailed Skill Int Int EntryId          -- Skill, roll, DC, destination
    | SavePassed Attribute Int Int EntryId       -- Attr, roll, DC, destination
    | SaveFailed Attribute Int Int EntryId       -- Attr, roll, DC, destination
    | CombatStarted [String] EntryId EntryId     -- Enemy names, victory entry, defeat entry
    | OptionNotFound
    | EntryNotFound
    deriving (Show, Eq)

-- | Select an option and process its outcome
selectOption :: Int -> AppState -> IO (OptionResult, AppState)
selectOption optId state = case getCurrentEntry state of
    Nothing -> pure (EntryNotFound, state)
    Just entry ->
        let availableOpts = evaluateRules (appGameState state) entry
        in case filter ((== optId) . optionId) availableOpts of
            []    -> pure (OptionNotFound, state)
            (opt:_) -> processOutcome (optionOutcome opt) state

-- | Process the outcome of an option
processOutcome :: OptionOutcome -> AppState -> IO (OptionResult, AppState)
processOutcome outcome state = case outcome of
    GoToEntry eid ->
        pure (NavigatedTo eid, goToEntry eid state)

    SkillCheck skill dc successEntry failEntry -> do
        let player = getPlayer state
            bonus = getSkillBonus (pcStatBlock player) skill
        (total, success) <- rollCheck bonus dc
        if success
            then pure (CheckPassed skill total dc successEntry, goToEntry successEntry state)
            else pure (CheckFailed skill total dc failEntry, goToEntry failEntry state)

    SaveCheck attr dc successEntry failEntry -> do
        let player = getPlayer state
            bonus = getSavingThrowBonus (pcStatBlock player) attr
        (total, success) <- rollCheck bonus dc
        if success
            then pure (SavePassed attr total dc successEntry, goToEntry successEntry state)
            else pure (SaveFailed attr total dc failEntry, goToEntry failEntry state)

    StartCombat enemyNames victoryEntry defeatEntry ->
        pure (CombatStarted enemyNames victoryEntry defeatEntry, state)

-- | Get option by ID from current entry
getOption :: Int -> AppState -> Maybe Option
getOption optId state = do
    entry <- getCurrentEntry state
    let opts = evaluateRules (appGameState state) entry
    case filter ((== optId) . optionId) opts of
        []    -> Nothing
        (o:_) -> Just o

-- | Check if player has visited an entry
hasVisited :: EntryId -> AppState -> Bool
hasVisited eid state = eid `elem` gsHistory (appGameState state)

-- | Get visit history
getHistory :: AppState -> [EntryId]
getHistory = gsHistory . appGameState
