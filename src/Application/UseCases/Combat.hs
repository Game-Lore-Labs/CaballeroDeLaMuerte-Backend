{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Application.UseCases.Combat
  ( CombatService(..)
  , CombatState(..)
  , CombatAction(..)
  , AttackRequest(..)
  , AttackResult(..)
  , InitiativeRoll(..)
  , CombatTurn(..)
  , Combatant(..)
  , InitiativeRoll(..)
  ) where

import Data.Text (Text)
import GHC.Generics

data Combatant = Combatant
  { combatantId :: Text
  , combatantName :: Text
  , combatantHP :: Int
  , combatantMaxHP :: Int
  , combatantAC :: Int
  , combatantInitiative :: Int
  , combatantIsPlayer :: Bool
  , combatantStatus :: [Text]
  } deriving (Show, Eq, Generic)

data InitiativeRoll = InitiativeRoll
  { initiativeCombatantId :: Text
  , initiativeRoll :: Int
  , initiativeModifier :: Int
  , initiativeTotal :: Int
  } deriving (Show, Eq, Generic)

data CombatState = CombatState
  { combatId :: Text
  , combatSessionId :: Text
  , combatants :: [Combatant]
  , combatTurnOrder :: [Text]
  , combatCurrentTurn :: Int
  , combatRound :: Int
  , combatIsActive :: Bool
  } deriving (Show, Eq, Generic)

data CombatTurn = CombatTurn
  { turnCombatantId :: Text
  , turnActionsRemaining :: Int
  , turnBonusActionsRemaining :: Int
  , turnMovementRemaining :: Int
  } deriving (Show, Eq, Generic)

data AttackRequest = AttackRequest
  { attackCombatId :: Text
  , attackAttackerId :: Text
  , attackTargetId :: Text
  , attackWeaponOrSpell :: Text
  } deriving (Show, Eq, Generic)

data AttackResult = AttackResult
  { attackResultHit :: Bool
  , attackResultAttackRoll :: Int
  , attackResultTargetAC :: Int
  , attackResultDamage :: Maybe Int
  , attackResultDamageType :: Maybe Text
  , attackResultCritical :: Bool
  , attackResultDescription :: Text
  } deriving (Show, Eq, Generic)

data CombatAction
  = Attack AttackRequest
  | CastSpell Text Text Text
  | UseItem Text Text Text
  | Dash Text Text
  | Dodge Text Text
  | Help Text Text Text
  | EndTurn Text Text
  deriving (Show, Eq, Generic)

class Monad m => CombatService m where
  initiateCombat :: Text -> [Text] -> m (Either Text CombatState)
  rollInitiative :: Text -> m (Either Text [InitiativeRoll])
  getCurrentTurn :: Text -> m (Either Text CombatTurn)
  performAttack :: AttackRequest -> m (Either Text AttackResult)
  executeCombatAction :: CombatAction -> m (Either Text CombatState)
  endTurn :: Text -> Text -> m (Either Text CombatState)
  getCombatState :: Text -> m (Either Text CombatState)
  endCombat :: Text -> Bool -> m (Either Text Text)
