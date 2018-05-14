{- |
Module      : Types
Copyright   : 2018 Nathan Ingle
Licence     : BSD3

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable

Data type definitions for workalloc.
-}
module Types where

import           Data.Map.Strict (Map)
import           Data.Time       (LocalTime, NominalDiffTime)


type AllocStart = (String, LocalTime)


data AllocFailure = TimeGoesBackwardsFailure
                  | SumDisparateTasksFailure
                  deriving (Eq, Ord, Show)


newtype TaskTotals = TaskTotals (Map String NominalDiffTime) deriving (Eq, Ord, Show)


data TimeSpent = TimeSpent { tsTaskName  :: String
                           , tsStartTime :: LocalTime
                           , tsDuration  :: NominalDiffTime
                           } deriving (Eq, Ord, Show)
