{- |
Module      : FileInput
Copyright   : 2018 Nathan Ingle
Licence     : BSD3

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable

Read work allocation data from a text file.
-}
module FileInput where

import           Types

import           Data.Time


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
readLines :: String -> Maybe [AllocStart]
readLines = mapM readLine . lines
