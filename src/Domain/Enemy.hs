module Domain.Enemy where

import Domain.Types
import Domain.StatBlock
import Domain.Dice

-- | Enemy entity
data Enemy = Enemy
    { enemyName      :: String
    , enemyCurrentHP :: Int
    , enemyMaxHP     :: Int
    , enemyStatBlock :: StatBlock
    } deriving (Show, Eq)

-- | Combat is a group of enemies
type Combat = [Enemy]

-- Enemy actions

-- | Enemy takes damage
enemyTakeDamage :: Int -> Enemy -> Enemy
enemyTakeDamage dmg enemy = enemy { enemyCurrentHP = max 0 (enemyCurrentHP enemy - dmg) }

-- | Enemy attacks, returns (attack roll, hit?, damage if hit)
enemyAttack :: Enemy -> Weapon -> Int -> IO (Int, Bool, Maybe Int)
enemyAttack enemy weapon targetAC = do
    let sb = enemyStatBlock enemy
        atkBonus = getAttackBonus sb weapon
    (atkTotal, hit) <- rollCheck atkBonus targetAC
    if hit
        then do
            dmgRoll <- roll (weaponDamageDice weapon) (weaponDamageCount weapon) (getDamageBonus sb weapon)
            pure (atkTotal, True, Just (rollTotal dmgRoll))
        else pure (atkTotal, False, Nothing)

-- | Enemy saving throw
enemySavingThrow :: Enemy -> Attribute -> Int -> IO (Int, CheckResult)
enemySavingThrow enemy attr dc = do
    let bonus = getSavingThrowBonus (enemyStatBlock enemy) attr
    (total, success) <- rollCheck bonus dc
    pure (total, if success then Success else Failure)

-- | Check if enemy is alive
enemyIsAlive :: Enemy -> Bool
enemyIsAlive = (> 0) . enemyCurrentHP

-- | Check if combat is over (all enemies defeated)
combatIsOver :: Combat -> Bool
combatIsOver = all (not . enemyIsAlive)

-- | Get alive enemies from combat
aliveEnemies :: Combat -> [Enemy]
aliveEnemies = filter enemyIsAlive
