module Application.EffectsService where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Char (toLower)
import System.Random (randomRIO)

import Domain.Types
import Domain.Effects
import Domain.Character
import Domain.StatBlock
import Domain.Entry (Entry, entryEffects)

-- | Result of applying effects
data EffectsResult = EffectsResult
    { effectsApplied    :: [String]     -- Description of what was applied
    , effectsNewPlayer  :: PlayerCharacter
    } deriving (Show, Eq)

-- | Apply all effects from an entry to a player character
applyEntryEffects :: Entry -> PlayerCharacter -> IO EffectsResult
applyEntryEffects entry player = do
    let effects = entryEffects entry
    applyEffects effects player

-- | Apply all effects to a player character
applyEffects :: EntryEffects -> PlayerCharacter -> IO EffectsResult
applyEffects effects player = do
    -- Apply stat effects (may involve dice rolls)
    (player1, statMsgs) <- applyStatEffects (effectsStats effects) player
    
    -- Apply equipment effects
    let (player2, equipMsgs) = applyEquipmentEffects (effectsEquipment effects) player1
    
    -- Apply inventory effects
    let (player3, invMsgs) = applyInventoryEffects (effectsInventory effects) player2
    
    -- Apply attribute effects
    let (player4, attrMsgs) = applyAttributeEffects (effectsAttributes effects) player3
    
    -- Apply skill effects
    let (player5, skillMsgs) = applySkillEffects (effectsSkills effects) player4
    
    pure $ EffectsResult
        { effectsApplied   = statMsgs ++ equipMsgs ++ invMsgs ++ attrMsgs ++ skillMsgs
        , effectsNewPlayer = player5
        }

-- | Apply stat effects (HP, AC, etc)
applyStatEffects :: [StatEffect] -> PlayerCharacter -> IO (PlayerCharacter, [String])
applyStatEffects effects player = foldlM applyOne (player, []) effects
  where
    foldlM f z []     = pure z
    foldlM f z (x:xs) = f z x >>= \z' -> foldlM f z' xs
    
    applyOne (pc, msgs) effect = do
        (pc', msg) <- applySingleStatEffect effect pc
        pure (pc', msgs ++ [msg | not (null msg)])

-- | Apply a single stat effect
applySingleStatEffect :: StatEffect -> PlayerCharacter -> IO (PlayerCharacter, String)
applySingleStatEffect effect player = case map toLower (statEffectName effect) of
    "currenthp" -> do
        change <- parseModifier (statEffectModifier effect)
        let newHP = max 0 $ min (pcMaxHP player) (pcCurrentHP player + change)
        pure (player { pcCurrentHP = newHP }, 
              if change < 0 
              then "Perdiste " ++ show (abs change) ++ " HP"
              else "Ganaste " ++ show change ++ " HP")
    
    "maxhp" -> do
        change <- parseModifier (statEffectModifier effect)
        let newMaxHP = max 1 (pcMaxHP player + change)
            newCurrentHP = min (pcCurrentHP player) newMaxHP
        pure (player { pcMaxHP = newMaxHP, pcCurrentHP = newCurrentHP },
              "HP máximo modificado en " ++ show change)
    
    "armorclass" -> do
        change <- parseModifier (statEffectModifier effect)
        let sb = pcStatBlock player
            newAC = max 0 (armorClass sb + change)
            newSB = sb { armorClass = newAC }
        pure (player { pcStatBlock = newSB },
              "CA modificada en " ++ show change)
    
    _ -> pure (player, "")

-- | Parse modifier string (can be numeric or dice expression like "-1d6")
parseModifier :: String -> IO Int
parseModifier str = case str of
    -- Handle negative dice expressions like "-1d6"
    ('-':rest) | 'd' `elem` rest -> do
        rolled <- parseDice rest
        pure (-rolled)
    
    -- Handle positive dice expressions like "1d6"
    _ | 'd' `elem` str -> parseDice str
    
    -- Handle plain numbers
    _ -> pure $ read str

-- | Parse and roll dice expression like "1d6", "2d4"
parseDice :: String -> IO Int
parseDice str = 
    let (countStr, rest) = span (/= 'd') str
        count = if null countStr then 1 else read countStr
        faces = read (drop 1 rest)  -- drop the 'd'
    in rollDice count faces

-- | Roll multiple dice
rollDice :: Int -> Int -> IO Int
rollDice count faces = sum <$> mapM (\_ -> randomRIO (1, faces)) [1..count]

-- | Apply equipment effects (add items to equipment AND inventory)
-- Equipment is always a subset of inventory
applyEquipmentEffects :: [EquipmentEffect] -> PlayerCharacter -> (PlayerCharacter, [String])
applyEquipmentEffects effects player = foldr applyOne (player, []) effects
  where
    applyOne effect (pc, msgs) =
        let item = Item
                { itemId = toItemId (equipmentEffectName effect)
                , itemName = equipmentEffectName effect
                , itemDescription = fromMaybe (equipmentEffectType effect) (equipmentEffectBonus effect)
                }
            qty = fromMaybe 1 (equipmentEffectQuantity effect)
            newItems = replicate qty item
            -- Add to both equipment AND inventory
            newEquipment = newItems ++ pcEquipment pc
            newInventory = newItems ++ pcInventory pc
            msg = "Equipado: " ++ equipmentEffectName effect
        in (pc { pcEquipment = newEquipment, pcInventory = newInventory }, msg : msgs)

-- | Apply inventory effects (add/remove items from inventory)
applyInventoryEffects :: [InventoryEffect] -> PlayerCharacter -> (PlayerCharacter, [String])
applyInventoryEffects effects player = foldr applyOne (player, []) effects
  where
    applyOne effect (pc, msgs) =
        let qty = fromMaybe 1 (inventoryEffectQuantity effect)
        in if qty < 0
           then -- Remove items
               let itemId = toItemId (inventoryEffectName effect)
                   newInv = removeNItems (abs qty) itemId (pcInventory pc)
                   msg = "Perdido: " ++ inventoryEffectName effect
               in (pc { pcInventory = newInv }, msg : msgs)
           else -- Add items
               let item = Item
                       { itemId = toItemId (inventoryEffectName effect)
                       , itemName = inventoryEffectName effect ++ 
                                    (if qty > 1 then " x" ++ show qty else "")
                       , itemDescription = inventoryEffectType effect ++
                                           maybe "" (\v -> " (valor: " ++ show v ++ " po)") (inventoryEffectValue effect)
                       }
                   newInv = item : pcInventory pc
                   msg = "Obtenido: " ++ inventoryEffectName effect ++ 
                         (if qty > 1 then " x" ++ show qty else "")
               in (pc { pcInventory = newInv }, msg : msgs)
    
    removeNItems _ _ [] = []
    removeNItems 0 _ items = items
    removeNItems n targetId (item:rest)
        | itemId item == targetId = removeNItems (n-1) targetId rest
        | otherwise = item : removeNItems n targetId rest

-- | Apply attribute effects (modify attributes or saving throws)
applyAttributeEffects :: [AttributeEffect] -> PlayerCharacter -> (PlayerCharacter, [String])
applyAttributeEffects effects player = foldr applyOne (player, []) effects
  where
    applyOne effect (pc, msgs) =
        let sb = pcStatBlock pc
            attrName = map toLower (attributeEffectName effect)
            attr = parseAttribute attrName
            
            -- Apply attribute modifier if present
            newAttrs = case attributeEffectModifier effect of
                Nothing -> attributes sb
                Just mod -> M.adjust (+ mod) attr (attributes sb)
            
            -- Apply saving throw modifier - add proficiency if not already proficient
            newSaveProfs = case attributeEffectSavingThrow effect of
                Nothing -> savingThrowProfs sb
                Just _ -> if attr `elem` savingThrowProfs sb
                         then savingThrowProfs sb
                         else attr : savingThrowProfs sb
            
            newSB = sb { attributes = newAttrs, savingThrowProfs = newSaveProfs }
            
            msg = case (attributeEffectModifier effect, attributeEffectSavingThrow effect) of
                (Just m, _) -> attributeEffectName effect ++ " modificado en " ++ show m
                (_, Just s) -> "Bonus a salvación de " ++ attributeEffectName effect ++ ": +" ++ show s
                _ -> ""
        in (pc { pcStatBlock = newSB }, if null msg then msgs else msg : msgs)
    
    parseAttribute name = case name of
        "strength"     -> Strength
        "dexterity"    -> Dexterity
        "constitution" -> Constitution
        "intelligence" -> Intelligence
        "wisdom"       -> Wisdom
        "charisma"     -> Charisma
        _              -> Strength  -- Default

-- | Apply skill effects (modify skills or proficiencies)
applySkillEffects :: [SkillEffect] -> PlayerCharacter -> (PlayerCharacter, [String])
applySkillEffects effects player = foldr applyOne (player, []) effects
  where
    applyOne effect (pc, msgs) =
        let sb = pcStatBlock pc
            skillName = skillEffectName effect
            skill = parseSkill skillName
            
            -- Apply proficiency change if present
            newSkillProfs = case skillEffectProf effect of
                Nothing -> skillProfs sb
                Just "Proficient" -> M.insert skill Proficient (skillProfs sb)
                Just "Expertise" -> M.insert skill Expertise (skillProfs sb)
                Just "NotProficient" -> M.delete skill (skillProfs sb)
                _ -> skillProfs sb
            
            newSB = sb { skillProfs = newSkillProfs }
            
            msg = case skillEffectProf effect of
                Just prof -> "Habilidad " ++ skillName ++ " ahora es " ++ prof
                Nothing -> case skillEffectModifier effect of
                    Just m -> "Bonus a " ++ skillName ++ ": " ++ show m
                    Nothing -> ""
        in (pc { pcStatBlock = newSB }, if null msg then msgs else msg : msgs)
    
    parseSkill name = case name of
        "Athletics"      -> Athletics
        "Acrobatics"     -> Acrobatics
        "SleightOfHand"  -> SleightOfHand
        "Stealth"        -> Stealth
        "Arcana"         -> Arcana
        "History"        -> History
        "Investigation"  -> Investigation
        "Nature"         -> Nature
        "Religion"       -> Religion
        "AnimalHandling" -> AnimalHandling
        "Insight"        -> Insight
        "Medicine"       -> Medicine
        "Perception"     -> Perception
        "Survival"       -> Survival
        "Deception"      -> Deception
        "Intimidation"   -> Intimidation
        "Performance"    -> Performance
        "Persuasion"     -> Persuasion
        _                -> Athletics  -- Default

-- | Convert item name to a valid ID
toItemId :: String -> String
toItemId = map (\c -> if c == ' ' then '_' else toLower c)
