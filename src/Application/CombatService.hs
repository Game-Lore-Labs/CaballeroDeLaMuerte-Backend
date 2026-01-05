module Application.CombatService where

import Domain.Types
import Domain.StatBlock
import Domain.Character
import Domain.Enemy
import Domain.Dice
import Application.GameService

-- | Combat state during an encounter
data CombatState = CombatState
    { combatPlayer       :: PlayerCharacter
    , combatEnemies      :: Combat
    , combatVictoryEntry :: EntryId
    , combatDefeatEntry  :: EntryId
    } deriving (Show, Eq)

-- | Result of a combat action
data CombatActionResult
    = PlayerHit Int          -- Damage dealt
    | PlayerMiss Int         -- Attack roll
    | EnemyHit String Int    -- Enemy name, damage dealt
    | EnemyMiss String Int   -- Enemy name, attack roll
    | EnemyDefeated String   -- Enemy name
    | PlayerDefeated
    | CombatVictory
    deriving (Show, Eq)

-- | Start a combat encounter
startCombat :: PlayerCharacter -> [Enemy] -> EntryId -> EntryId -> CombatState
startCombat player enemies victoryEntry defeatEntry = CombatState
    { combatPlayer       = player
    , combatEnemies      = enemies
    , combatVictoryEntry = victoryEntry
    , combatDefeatEntry  = defeatEntry
    }

-- | Player attacks an enemy by index (index refers to alive enemies only)
playerAttack :: Int -> Weapon -> CombatState -> IO (CombatActionResult, CombatState)
playerAttack aliveIdx weapon combat = do
    let enemies = combatEnemies combat
        -- Map alive enemy index to actual index in the full list
        aliveIndices = [i | (i, e) <- zip [0..] enemies, enemyIsAlive e]
    if aliveIdx < 0 || aliveIdx >= length aliveIndices
        then pure (PlayerMiss 0, combat)
        else do
            let actualIdx = aliveIndices !! aliveIdx
                target = enemies !! actualIdx
                targetAC = armorClass (enemyStatBlock target)
                player = combatPlayer combat
            (atkRoll, hit, dmgMaybe) <- attack player weapon targetAC
            if hit
                then do
                    let dmg = maybe 0 id dmgMaybe
                        updatedEnemy = enemyTakeDamage dmg target
                        updatedEnemies = take actualIdx enemies ++ [updatedEnemy] ++ drop (actualIdx + 1) enemies
                        newCombat = combat { combatEnemies = updatedEnemies }
                    if enemyIsAlive updatedEnemy
                        then pure (PlayerHit dmg, newCombat)
                        else pure (EnemyDefeated (enemyName target), newCombat)
                else pure (PlayerMiss atkRoll, combat)

-- | Single enemy attacks the player
enemyAttackPlayer :: Int -> CombatState -> IO (CombatActionResult, CombatState)
enemyAttackPlayer enemyIdx combat = do
    let enemies = combatEnemies combat
    if enemyIdx < 0 || enemyIdx >= length enemies
        then pure (EnemyMiss "Unknown" 0, combat)
        else do
            let attacker = enemies !! enemyIdx
            if not (enemyIsAlive attacker)
                then pure (EnemyMiss (enemyName attacker) 0, combat)
                else do
                    let playerAC = armorClass (pcStatBlock (combatPlayer combat))
                        weapons = Domain.StatBlock.weapons (enemyStatBlock attacker)
                    case weapons of
                        [] -> pure (EnemyMiss (enemyName attacker) 0, combat)
                        (weapon:_) -> do
                            (atkRoll, hit, dmgMaybe) <- enemyAttack attacker weapon playerAC
                            if hit
                                then do
                                    let dmg = maybe 0 id dmgMaybe
                                        updatedPlayer = takeDamage dmg (combatPlayer combat)
                                        newCombat = combat { combatPlayer = updatedPlayer }
                                    if isAlive updatedPlayer
                                        then pure (EnemyHit (enemyName attacker) dmg, newCombat)
                                        else pure (PlayerDefeated, newCombat)
                                else pure (EnemyMiss (enemyName attacker) atkRoll, combat)

-- | All alive enemies attack the player
allEnemiesAttack :: CombatState -> IO ([CombatActionResult], CombatState)
allEnemiesAttack combat = go 0 combat []
  where
    go idx currentCombat results
        | idx >= length (combatEnemies currentCombat) = pure (reverse results, currentCombat)
        | not (enemyIsAlive (combatEnemies currentCombat !! idx)) = go (idx + 1) currentCombat results
        | otherwise = do
            (result, newCombat) <- enemyAttackPlayer idx currentCombat
            case result of
                PlayerDefeated -> pure (reverse (result : results), newCombat)
                _ -> go (idx + 1) newCombat (result : results)

-- | Check combat status
data CombatStatus = InProgress | Victory | Defeat
    deriving (Show, Eq)

getCombatStatus :: CombatState -> CombatStatus
getCombatStatus combat
    | not (isAlive (combatPlayer combat)) = Defeat
    | combatIsOver (combatEnemies combat) = Victory
    | otherwise = InProgress

-- | Get alive enemies
getAliveEnemies :: CombatState -> [Enemy]
getAliveEnemies = aliveEnemies . combatEnemies

-- | Get player HP
getPlayerHP :: CombatState -> (Int, Int)
getPlayerHP combat = (pcCurrentHP p, pcMaxHP p)
  where p = combatPlayer combat

-- | End combat and get destination entry
endCombat :: CombatState -> (EntryId, PlayerCharacter)
endCombat combat = case getCombatStatus combat of
    Victory -> (combatVictoryEntry combat, combatPlayer combat)
    Defeat  -> (combatDefeatEntry combat, combatPlayer combat)
    InProgress -> (combatVictoryEntry combat, combatPlayer combat)
