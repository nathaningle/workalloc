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
import           Data.Text       (Text)
import           Data.Time       (LocalTime, NominalDiffTime)


data AllocStart = AllocStart Text LocalTime deriving (Eq, Ord, Show)


data AllocFailure = TimeGoesBackwardsFailure
                  | SumDisparateTasksFailure
                  | EventParseFailure String
                  deriving (Eq, Ord, Show)


newtype TaskTotals = TaskTotals (Map Text NominalDiffTime) deriving (Eq, Ord, Show)


data TimeSpent = TimeSpent { tsTaskName  :: Text
                           , tsStartTime :: LocalTime
                           , tsDuration  :: NominalDiffTime
                           } deriving (Eq, Ord, Show)
