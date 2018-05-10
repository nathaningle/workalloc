{- |
File        : workalloc.hs
Copyright   : 2018 Nathan Ingle
Licence     : BSD3

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable

Visualise allotment of time for a work day.
-}
import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy

import           Control.Monad                          ((<=<))
import           Data.List                              (groupBy, nub, sortBy)
import           Data.List.NonEmpty                     (NonEmpty)
import qualified Data.List.NonEmpty                     as NE
import           Data.Maybe                             (fromMaybe)
import           Data.Ord                               (comparing)
import           Data.Time


type AllocStart = (String, LocalTime)
type TimeSpent = (String, NominalDiffTime)


-- | Parse a single line of input.  The format for each input line is:
--
-- > "YYYY-mm-dd HHMM\twhat\tcomment\n"
--
-- where @what@ is the description of this allocation, and @comment@ is discarded.
readLine :: String -> Maybe AllocStart
readLine str =
  case span (/= '\t') str of
    (timestr, '\t' : str') ->
      let maybeTimestamp = parseTimeM False defaultTimeLocale "%F %H%M" timestr
      in sequence (takeWhile (/= '\t') str', maybeTimestamp)
    _ -> Nothing

-- | Parse lines of input; c.f. 'readLine'.
readLines :: String -> Maybe (NonEmpty AllocStart)
readLines = NE.nonEmpty <=< mapM readLine . lines


-- | Given a list of events that each describe the start of a piece of work,
-- determine the duration of each piece of work.
makeIntervals :: NonEmpty AllocStart -> [TimeSpent]
makeIntervals events = zipWith go (NE.init events) (NE.tail events)
  where
    go (what, starttime) (_, endtime) = (what, diffLocalTime endtime starttime)
    -- Lifted from @time-1.9@.
    diffLocalTime a b = diffUTCTime (localTimeToUTC utc a) (localTimeToUTC utc b)

-- | Aggregate time spent per work description.
tallyIntervals :: [TimeSpent] -> [TimeSpent]
tallyIntervals = map (sumIntervals . NE.fromList) . groupBy (equating fst) . sortBy (comparing fst)

-- | Test equality after applying a function (by analogy to 'comparing').
equating :: Eq a => (b -> a) -> b -> b -> Bool
equating f x y = f x == f y

-- | Note that we don't check that the descriptions all match; we just take the first one.
sumIntervals :: NonEmpty TimeSpent -> TimeSpent
sumIntervals intervals = (what, sum (NE.map snd intervals))
  where
    what = fst (NE.head intervals)


-- | Time expenditure item titles, for labelling the bar graph.
titles :: [TimeSpent] -> [String]
titles = map fst

-- | Convert a 'NominalDiffTime' to hours.
intervalToHours :: NominalDiffTime -> Double
intervalToHours = (/ 3600.0) . realToFrac

-- | Bar graph data.
values :: [TimeSpent] -> [(String, [Double])]
values totals = [("Total", map (intervalToHours . snd) totals)]


-- | Note that we don't check that the days all match; we just take the first one.
tagWithDay :: NonEmpty AllocStart -> (Day, NonEmpty AllocStart)
tagWithDay events = (localDay (snd (NE.head events)), events)

-- | Look up the value to be plotted for each cluster item title.
intervalsToValues :: [String] -> [TimeSpent] -> [Double]
intervalsToValues tasknames tasks = map (\t -> fromMaybe 0 (intervalToHours <$> lookup t tasks)) tasknames

-- | Add an empty day (i.e. no tasks reported) before and after the actual data,
-- in order to avoid the bar chart being cut off at the ends.
padDays :: [a] -> [(Day, [Double])] -> [(Day, [Double])]
padDays _ [] = []
padDays tasknames xs = dayBefore : xs ++ [dayAfter]
  where
    dayBefore = (pred (fst (head xs)), replicate numTasks 0.0)
    dayAfter  = (succ (fst (last xs)), replicate numTasks 0.0)
    numTasks = length tasknames

-- | The special task name @Out@ indicates time spent out of the office, which
-- we're not interested in charting.
filterOutIntervals :: [TimeSpent] -> [TimeSpent]
filterOutIntervals = filter (\x -> fst x /= "Out")

-- | Find the hours worked per task, given the task names and a list of events.
hoursPerTask :: [String] -> NonEmpty AllocStart -> [Double]
hoursPerTask reportedTasks events = makeIntervals events
                                  & filterOutIntervals
                                  & tallyIntervals
                                  & intervalsToValues reportedTasks


main :: IO ()
main = do
  Just events <- readLines <$> getContents
  let intervals = makeIntervals events
      totals = tallyIntervals $ filterOutIntervals intervals
      eventsPerDay = NE.groupBy (equating (localDay . snd)) events
      taggedEventsPerDay = map tagWithDay eventsPerDay
      reportedTasks = nub $ filter (/= "Out") $ map fst $ NE.toList events
      values' = padDays reportedTasks $ map (fmap (hoursPerTask reportedTasks)) taggedEventsPerDay
  mapM_ print totals

  toFile def "hours_per_day.png" $ do
    layout_title .= "Hours worked per day"
    plot $ (plotBars . (plot_bars_style .~ BarsStacked)) <$> bars reportedTasks values'
    plot $ line "Scheduled hours" [[(fst (head values'), 7.5), (fst (last values'), 7.5)]]
  toFile def "total_hours.png" $ do
    layout_title .= "Hours worked per task"
    layout_x_axis . laxis_generate .= autoIndexAxis (map fst (values totals))
    plot $ (plotBars . (plot_bars_spacing .~ BarsFixWidth 100.0)) <$> bars (titles totals) (addIndexes (map snd (values totals)))
  putStrLn "ok"
