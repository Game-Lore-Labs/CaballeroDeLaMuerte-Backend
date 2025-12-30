module Domain.Types where

-- | The six D&D attributes
data Attribute = Strength | Dexterity | Constitution | Intelligence | Wisdom | Charisma
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | The 18 D&D skills, each associated with an attribute
data Skill
    -- Strength
    = Athletics
    -- Dexterity
    | Acrobatics | SleightOfHand | Stealth
    -- Intelligence
    | Arcana | History | Investigation | Nature | Religion
    -- Wisdom
    | AnimalHandling | Insight | Medicine | Perception | Survival
    -- Charisma
    | Deception | Intimidation | Performance | Persuasion
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | Maps each skill to its governing attribute
skillAttribute :: Skill -> Attribute
skillAttribute skill = case skill of
    Athletics      -> Strength
    Acrobatics     -> Dexterity
    SleightOfHand  -> Dexterity
    Stealth        -> Dexterity
    Arcana         -> Intelligence
    History        -> Intelligence
    Investigation  -> Intelligence
    Nature         -> Intelligence
    Religion       -> Intelligence
    AnimalHandling -> Wisdom
    Insight        -> Wisdom
    Medicine       -> Wisdom
    Perception     -> Wisdom
    Survival       -> Wisdom
    Deception      -> Charisma
    Intimidation   -> Charisma
    Performance    -> Charisma
    Persuasion     -> Charisma

-- | Dice types used in D&D
data DiceType = D2 | D4 | D6 | D8 | D10 | D12 | D20 | D100
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | Get number of faces for a dice type
diceFaces :: DiceType -> Int
diceFaces d = case d of
    D2   -> 2
    D4   -> 4
    D6   -> 6
    D8   -> 8
    D10  -> 10
    D12  -> 12
    D20  -> 20
    D100 -> 100

-- | Result of a check (ability check, saving throw, attack)
data CheckResult = Success | Failure
    deriving (Show, Eq)

-- | Damage types
data DamageType = Piercing | Slashing | Bludgeoning | Fire | Cold | Lightning | Poison | Necrotic | Radiant
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | Item identifier
type ItemId = String

-- | Clue identifier
type ClueId = String

-- | Entry identifier
type EntryId = Int
