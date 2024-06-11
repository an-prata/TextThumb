{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module FileContent
    ( FileImpression (..)
    , FileLine (..)
    , isSpaceAt
    , isSpaceAtColumn
    , readImpression
    ) where

import Data.Char
import Data.Maybe
import Data.Text (Text)
import Data.List.Extra ((!?))
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

-- | Represents the general structure of a file in its appearance, based off of the presense of
-- absence of non-whitespace characters.
newtype FileImpression = FileImpression [FileLine]
    deriving newtype (Eq, Monoid, Semigroup, Show)

-- | A single line of a file, as represented by the presence or absence of a character in a given
-- column. Whitespace is considered the absence of a character, and tabs are mapped to being four
-- spaces in order to give them a reasonable width.
newtype FileLine = FileLine [Bool]
    deriving newtype (Eq, Monoid, Semigroup, Show)

-- | Read the "impression" of a file, yeilding a @FileImpression@.
readImpression :: FilePath -> IO FileImpression
readImpression path = FileImpression . map parseFileLine . Text.lines <$> TextIO.readFile path

-- | Determines if whitespace character exists at the given row and column (inputs are in this
-- order).
isSpaceAt :: FileImpression -> Int -> Int -> Bool
isSpaceAt (FileImpression ls) row col = maybe True (`isSpaceAtColumn` col) (ls !? row)

-- | True is a whitespace character exists in the given @FileLine@ at the given column.
isSpaceAtColumn :: FileLine -> Int -> Bool
isSpaceAtColumn (FileLine l) = fromMaybe True . (l !?)

-- | Parses what is assumed to be a single line of text into a @FileLine@, indicating whitespace and
-- four instances thereof for tab characters.
parseFileLine :: Text -> FileLine
parseFileLine text = FileLine $ map isSpace chars
  where
    chars = concatMap (\case { '\t' -> "    "; c -> [c] }) (Text.unpack text)
