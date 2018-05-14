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

import           Prelude              hiding (takeWhile)

import           Control.Applicative  (empty, (<|>))
import           Data.Attoparsec.Text
import           Data.Bifunctor       (first)
import           Data.Maybe           (catMaybes)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Time            (defaultTimeLocale, parseTimeM)


-- | Skip the rest of this line, including the end-of-line characters.
skipRestOfLine :: Parser ()
skipRestOfLine = takeTill isEndOfLine *> endOfLine

-- | Parse a single line of input.  The format for each input line is:
--
-- > "YYYY-mm-dd HHMM\twhat\tcomment\n"
--
-- where @what@ is the description of this allocation, and @comment@ is discarded.
parseEvent :: Parser AllocStart
parseEvent = parseEvent' <?> "AllocStart"
  where
    parseEvent' = do
      timestr  <- takeWhile1 (/= '\t') <* char '\t'
      taskName <- takeWhile1 (/= '\t') <* char '\t' <* skipRestOfLine
      case parseTimeM False defaultTimeLocale "%F %H%M" (T.unpack timestr) of
        Nothing -> empty
        Just t  -> pure $ AllocStart taskName t

-- | Parse a single line of input, ignoring comments and blank lines.
parseLine :: Parser (Maybe AllocStart)
parseLine = Just <$> parseEvent <|> Nothing <$ (blankLine <|> commentLine)
  where
    blankLine = takeWhile isHorizontalSpace *> endOfLine
    commentLine = takeWhile isHorizontalSpace *> char '#' *> skipRestOfLine

-- | Parse all lines of input, ignoring comments and blank lines.
parseLines :: Parser [AllocStart]
parseLines = catMaybes <$> many' parseLine

-- | Read a list of task events.
readEvents :: Text -> Either AllocFailure [AllocStart]
readEvents = first EventParseFailure . parseOnly (parseLines <* endOfInput)
