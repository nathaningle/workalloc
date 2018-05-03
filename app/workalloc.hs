{- |
File        : workalloc.hs
Copyright   : 2018 Nathan Ingle
Licence     : BSD3

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable

Visualise allotment of time for a work day.
-}
import           Data.Time


-- | Parse a single line of input.  The format for each input line is:
--
-- > "YYYY-mm-dd HHMM\twhat\tcomment\n"
--
-- where @what@ is the description of this allocation, and @comment@ is discarded.
readLine :: String -> Maybe (String, LocalTime)
readLine str =
  case span (/= '\t') str of
    (timestr, '\t' : str') ->
      let maybeTimestamp = parseTimeM False defaultTimeLocale "%F %H%M" timestr
      in sequence (takeWhile (/= '\t') str', maybeTimestamp)
    _ -> Nothing


main :: IO ()
main = do
  Just events <- (mapM readLine . lines) <$> readFile "allocation.txt"
  mapM_ print events
