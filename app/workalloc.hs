{- |
File        : workalloc.hs
Copyright   : 2018 Nathan Ingle
Licence     : BSD3

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable

Visualise allotment of time for a work day.
-}
import           Allocation
import           FileInput
import           Types

import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy

import           Data.List                              (nub, sort)
import           Data.List.NonEmpty                     (NonEmpty)
import qualified Data.List.NonEmpty                     as NE
import qualified Data.Map.Strict                        as M
import           Data.Time                              (Day,
                                                         LocalTime (localDay),
                                                         NominalDiffTime)


-- | Convert a 'NominalDiffTime' to hours.
durationToHours :: NominalDiffTime -> Double
durationToHours = (/ 3600.0) . realToFrac

-- | Add an empty day (i.e. no tasks reported) before and after the actual data,
-- in order to avoid the bar chart being cut off at the ends.
padDays :: [a] -> [(Day, [Double])] -> [(Day, [Double])]
padDays _ [] = []
padDays tasknames xs = dayBefore : xs ++ [dayAfter]
  where
    dayBefore = (pred (fst (head xs)), dummyTasks)
    dayAfter  = (succ (fst (last xs)), dummyTasks)
    numTasks = length tasknames
    dummyTasks = replicate numTasks 0.0

-- | Note which 'Day' a list of tasks commenced.  Note that we don't check that
-- the days all match; we just take the first one.
tagWithDay :: NonEmpty TimeSpent -> (Day, [TimeSpent])
tagWithDay xs = (localDay (tsStartTime (NE.head xs)), NE.toList xs)

-- | Determine the number of hours spent on the named task.
lookupTaskHours :: TaskTotals -> String -> Double
lookupTaskHours (TaskTotals m) k = durationToHours $ M.findWithDefault 0 k m

-- | Extract an ordered list of task names from a 'TaskTotals'.
getTaskNames :: TaskTotals -> [String]
getTaskNames (TaskTotals m) = M.keys m

-- | Test equality after applying a function (by analogy to 'comparing').
equating :: Eq a => (b -> a) -> b -> b -> Bool
equating f x y = f x == f y

-- | Test equality comparing only the 'Day' upon which this task was started.
equatingStartDay :: TimeSpent -> TimeSpent -> Bool
equatingStartDay = equating (localDay . tsStartTime)

-- | Prepare per-day chart data.
taskTotalDataPerDay :: [TimeSpent] -> ([String], [(Day, [Double])])
taskTotalDataPerDay intervals = (taskNames, padDays taskNames taskHoursPerDay)
  where
    intervalsPerDay :: [(Day, [TimeSpent])]
    intervalsPerDay = map tagWithDay $ NE.groupBy equatingStartDay intervals

    taskTotalsPerDay :: [(Day, TaskTotals)]
    taskTotalsPerDay = map (fmap tallyIntervals) intervalsPerDay

    taskNames :: [String]
    taskNames = nub $ sort $ concatMap (getTaskNames . snd) taskTotalsPerDay

    taskTotalsToHours :: TaskTotals -> [Double]
    taskTotalsToHours tt = map (lookupTaskHours tt) taskNames

    taskHoursPerDay :: [(Day, [Double])]
    taskHoursPerDay = map (fmap taskTotalsToHours) taskTotalsPerDay

-- | Prepare total chart data.  The principle of least surprise dictates that
-- the type signature ought to resemble that from 'taskTotalDataPerDay'.
taskTotalData :: [TimeSpent] -> ([String], [(String, [Double])])
taskTotalData intervals = (taskNames, [("Total", taskHours)])
  where
    TaskTotals mTaskTotals = tallyIntervals intervals
    (taskNames, taskDurations) = unzip $ M.toList mTaskTotals
    taskHours = map durationToHours taskDurations


main :: IO ()
main = do
  Just events <- readLines <$> getContents
  case makeIntervals events of
    Left e -> print e
    Right intervals -> do
      let (taskNames, taskHoursPerDay) = taskTotalDataPerDay $ filterOutIntervals intervals
          (taskNames', taskHoursTotal) = taskTotalData $ filterOutIntervals intervals
          imgWidth = fromIntegral (fst (def ^. fo_size))

      toFile def "hours_per_day.png" $ do
        layout_title .= "Hours worked per day"
        plot $ (plotBars . (plot_bars_style .~ BarsStacked)) <$> bars taskNames taskHoursPerDay
        plot $ line "Scheduled hours" [[(fst (head taskHoursPerDay), 7.5), (fst (last taskHoursPerDay), 7.5)]]

      toFile def "total_hours.png" $ do
        layout_title .= "Hours worked per task"
        layout_x_axis . laxis_generate .= autoIndexAxis ["Total"]
        plot $ (plotBars . (plot_bars_singleton_width .~ (imgWidth - 100.0))) <$> bars taskNames' (addIndexes (map snd taskHoursTotal))

      putStrLn "ok"
