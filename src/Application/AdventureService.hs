module Application.AdventureService where

import Domain.Types
import Domain.Character
import Domain.StatBlock (getSkillBonus, getSavingThrowBonus)
import Domain.Entry
import Domain.Effects (hasNoEffects)
import Domain.Dice
import Application.GameService
import Application.EffectsService
import Infrastructure.Repository (getEntry)

-- | Result of selecting an option
data OptionResult
    = NavigatedTo EntryId [String]               -- Direct navigation + effects applied
    | CheckPassed Skill Int Int EntryId [String] -- Skill, roll, DC, destination + effects
    | CheckFailed Skill Int Int EntryId [String] -- Skill, roll, DC, destination + effects
    | SavePassed Attribute Int Int EntryId [String]  -- Attr, roll, DC, destination + effects
    | SaveFailed Attribute Int Int EntryId [String]  -- Attr, roll, DC, destination + effects
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

-- | Apply effects when entering a new entry
applyEffectsOnEntry :: EntryId -> AppState -> IO (AppState, [String])
applyEffectsOnEntry eid state = case getEntry (appEntries state) eid of
    Nothing -> pure (state, [])
    Just entry -> 
        if hasNoEffects (entryEffects entry)
        then pure (state, [])
        else do
            let player = getPlayer state
            result <- applyEntryEffects entry player
            let newState = updatePlayer (const $ effectsNewPlayer result) state
            pure (newState, effectsApplied result)

-- | Process the outcome of an option
processOutcome :: OptionOutcome -> AppState -> IO (OptionResult, AppState)
processOutcome outcome state = case outcome of
    GoToEntry eid -> do
        let state1 = goToEntry eid state
        (state2, msgs) <- applyEffectsOnEntry eid state1
        pure (NavigatedTo eid msgs, state2)

    SkillCheck skill dc successEntry failEntry -> do
        let player = getPlayer state
            bonus = getSkillBonus (pcStatBlock player) skill
        (total, success) <- rollCheck bonus dc
        let destEntry = if success then successEntry else failEntry
            state1 = goToEntry destEntry state
        (state2, msgs) <- applyEffectsOnEntry destEntry state1
        if success
            then pure (CheckPassed skill total dc successEntry msgs, state2)
            else pure (CheckFailed skill total dc failEntry msgs, state2)

    SaveCheck attr dc successEntry failEntry -> do
        let player = getPlayer state
            bonus = getSavingThrowBonus (pcStatBlock player) attr
        (total, success) <- rollCheck bonus dc
        let destEntry = if success then successEntry else failEntry
            state1 = goToEntry destEntry state
        (state2, msgs) <- applyEffectsOnEntry destEntry state1
        if success
            then pure (SavePassed attr total dc successEntry msgs, state2)
            else pure (SaveFailed attr total dc failEntry msgs, state2)

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
