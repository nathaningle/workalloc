{- |
File        : workalloc.hs
Copyright   : 2018 Nathan Ingle
Licence     : BSD3

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable

Visualise allotment of time for a work day.
-}
import           Control.Monad      ((<=<))
import           Data.List          (groupBy, sortBy)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Ord           (comparing)
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


main :: IO ()
main = do
  Just events <- readLines <$> readFile "allocation.txt"
  mapM_ print $ tallyIntervals $ filter (\x -> fst x /= "Out") $ makeIntervals events
