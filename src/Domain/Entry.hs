module Domain.Entry where

import Domain.Types
import Domain.Character (PlayerCharacter, hasClue, hasItem)

-- | Game state used for rule evaluation
data GameState = GameState
    { gsPlayer       :: PlayerCharacter
    , gsCurrentEntry :: EntryId
    , gsHistory      :: [EntryId]  -- Previously visited entries
    } deriving (Show, Eq)

-- | Condition for rule evaluation - a predicate on game state
type Condition = GameState -> Bool

-- | Possible outcomes when selecting an option
data OptionOutcome
    = GoToEntry EntryId                    -- Navigate to another entry
    | SkillCheck Skill Int EntryId EntryId -- Check skill DC, success entry, failure entry
    | SaveCheck Attribute Int EntryId EntryId -- Saving throw DC, success entry, failure entry
    | StartCombat EntryId EntryId          -- Combat, victory entry, defeat entry
    deriving (Show, Eq)

-- | An option available to the player
data Option = Option
    { optionId          :: Int
    , optionDescription :: String
    , optionOutcome     :: OptionOutcome
    } deriving (Show, Eq)

-- | Rule that conditionally modifies available options
data Rule = Rule
    { ruleCondition :: Condition
    , ruleEffect    :: RuleEffect
    }

-- | What a rule does when its condition is met
data RuleEffect
    = AddOption Option        -- Add an option
    | RemoveOption Int        -- Remove option by ID
    | ModifyNarrative String  -- Append to narrative
    deriving (Show, Eq)

-- | An entry in the adventure
data Entry = Entry
    { entryId        :: EntryId
    , entryNarrative :: String
    , entryOptions   :: [Option]
    , entryRules     :: [Rule]
    }

-- Rule constructors using Haskell functions

-- | Condition: player has a specific clue
hasClueCondition :: ClueId -> Condition
hasClueCondition cid gs = hasClue cid (gsPlayer gs)

-- | Condition: player has a specific item
hasItemCondition :: ItemId -> Condition
hasItemCondition iid gs = hasItem iid (gsPlayer gs)

-- | Condition: player visited a specific entry
visitedEntry :: EntryId -> Condition
visitedEntry eid gs = eid `elem` gsHistory gs

-- | Condition: combine conditions with AND
andCondition :: Condition -> Condition -> Condition
andCondition c1 c2 gs = c1 gs && c2 gs

-- | Condition: combine conditions with OR
orCondition :: Condition -> Condition -> Condition
orCondition c1 c2 gs = c1 gs || c2 gs

-- | Condition: negate a condition
notCondition :: Condition -> Condition
notCondition c gs = not (c gs)

-- | Always true condition
alwaysTrue :: Condition
alwaysTrue _ = True

-- Rule evaluation

-- | Apply rules to get final list of options for an entry
evaluateRules :: GameState -> Entry -> [Option]
evaluateRules gs entry = foldr applyRule (entryOptions entry) (entryRules entry)
  where
    applyRule :: Rule -> [Option] -> [Option]
    applyRule rule opts
        | ruleCondition rule gs = applyEffect (ruleEffect rule) opts
        | otherwise = opts

    applyEffect :: RuleEffect -> [Option] -> [Option]
    applyEffect (AddOption opt) opts = opts ++ [opt]
    applyEffect (RemoveOption oid) opts = filter ((/= oid) . optionId) opts
    applyEffect (ModifyNarrative _) opts = opts  -- Narrative handled separately

-- | Get the narrative with rule modifications applied
evaluateNarrative :: GameState -> Entry -> String
evaluateNarrative gs entry = entryNarrative entry ++ extraNarrative
  where
    extraNarrative = concatMap extractNarrative (entryRules entry)
    extractNarrative rule
        | ruleCondition rule gs = case ruleEffect rule of
            ModifyNarrative text -> "\n" ++ text
            _ -> ""
        | otherwise = ""

-- | Select an option by ID
selectOption :: Int -> Entry -> Maybe Option
selectOption oid entry = lookup oid [(optionId o, o) | o <- entryOptions entry]
