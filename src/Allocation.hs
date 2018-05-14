{- |
Module      : Allocation
Copyright   : 2018 Nathan Ingle
Licence     : BSD3

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable

Summarise task allocation events into total time on task.
-}
{-# LANGUAGE RecordWildCards #-}
module Allocation where

import           Types

import qualified Data.Map.Strict as M
import           Data.Time       (diffUTCTime, localTimeToUTC, utc)


-- | Determine the elapsed time between two 'AllocStart's.
makeInterval :: AllocStart -> AllocStart -> Either AllocFailure TimeSpent
makeInterval (what, starttime) (_, endtime)
  | starttime < endtime = Right TimeSpent { tsTaskName  = what
                                          , tsStartTime = starttime
                                          , tsDuration  = diffLocalTime endtime starttime
                                          }
  | otherwise = Left TimeGoesBackwardsFailure
  where
    -- Lifted from @time-1.9@.
    diffLocalTime a b = diffUTCTime (localTimeToUTC utc a) (localTimeToUTC utc b)

-- | Given a list of events that each describe the start of a piece of work,
-- determine the duration of each piece of work.
makeIntervals :: [AllocStart] -> Either AllocFailure [TimeSpent]
makeIntervals [] = Right []
makeIntervals events = sequenceA $ zipWith makeInterval (init events) (tail events)

-- | The special task name @Out@ indicates time spent out of the office, which
-- we're not interested in charting.
filterOutIntervals :: [TimeSpent] -> [TimeSpent]
filterOutIntervals = filter (\x -> tsTaskName x /= "Out")

-- | Aggregate time spent per task.
tallyIntervals :: [TimeSpent] -> TaskTotals
tallyIntervals = TaskTotals . foldr insertInterval M.empty
  where
    insertInterval TimeSpent{..} = M.insertWith (+) tsTaskName tsDuration
