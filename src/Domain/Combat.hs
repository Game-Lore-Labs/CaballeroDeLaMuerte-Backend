{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain.Combat where

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)

data Enemy = Enemy
  { enemyId :: String
  , enemyName :: String
  , stats :: Stats
  , attacks :: [Attack]
  } deriving (Show, Generic)

instance ToJSON Enemy
instance FromJSON Enemy

data Stats = Stats
  { ac :: Int
  , hp :: Int
  , maxHP :: Int
  , speed :: Int
  , str :: Int
  , dex :: Int
  , con :: Int
  , int :: Int
  , wis :: Int
  , cha :: Int
  } deriving (Show, Generic)

instance ToJSON Stats
instance FromJSON Stats

data Attack = Attack
  { attackName :: String
  , bonus :: Int
  , damage :: String
  , damageType :: String
  } deriving (Show, Generic)

instance ToJSON Attack
instance FromJSON Attack
