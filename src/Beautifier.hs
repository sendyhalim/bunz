{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Beautifier
  ( beautify
  , indent
  , firstString
  , splitAtHead
  ) where

import           Data.Int               (Int64)
import           Data.Monoid            ((<>))
import qualified Data.Text.Lazy         as T
import qualified Data.Text.Lazy.Builder as B

-- Doctest setup
-- $setup
-- >>> :set -XOverloadedStrings

type IndentationLevel = Int64


-- | Lift function that works with Data.Text.Lazy
-- to be able to work with Data.Text.Lazy.Builder.
-- There should be a better and faster way to manipulate builder. We're using
-- this for now because it's a lot faster than just using plain Data.Text.Lazy when
-- concatenating text.
liftToBuilder :: (T.Text -> T.Text) -> (B.Builder -> B.Builder)
liftToBuilder f = B.fromLazyText . f . B.toLazyText

splitAt' :: Int64 -> B.Builder -> (B.Builder, B.Builder)
splitAt' n str = (B.fromLazyText head, B.fromLazyText tail)
  where (head, tail) = T.splitAt n (B.toLazyText str)

head' :: B.Builder -> B.Builder
head' = B.singleton . T.head . B.toLazyText

tail' :: B.Builder -> B.Builder
tail' = liftToBuilder T.tail

drop' :: Int64 -> B.Builder -> B.Builder
drop' length = liftToBuilder (T.drop length)

length' :: B.Builder -> Int64
length' = T.length . B.toLazyText

stripStart' :: B.Builder -> B.Builder
stripStart' =  liftToBuilder T.stripStart

spaceIndentation :: T.Text
spaceIndentation = "  "

indentation :: IndentationLevel -> B.Builder
indentation level = B.fromLazyText (T.replicate level spaceIndentation)

newline :: B.Builder
newline = B.fromLazyText "\n"

indent :: IndentationLevel -> B.Builder -> B.Builder
indent level str = indentation level <> str

trimmedTail :: B.Builder -> B.Builder
trimmedTail = B.fromLazyText . T.stripStart . T.tail . B.toLazyText

-- | Split the given string at its head while honouring escaped string.
-- Examples:
--
-- >>> splitAtHead "whatsup"
-- ("w","hatsup")
--
-- >>> splitAtHead "\nyeay"
-- ("\n","yeay")
splitAtHead :: B.Builder -> (B.Builder, B.Builder)
splitAtHead "" = ("", "")
splitAtHead str@(head' -> "\\") = splitAt' 2 str
splitAtHead str = splitAt' 1 str

-- | Extract string within double quotes `"` including the double quotes ignoring
-- the suffix after closing quote `"`
--
-- Examples:
--
-- >>> string False "\"test\"whatuppp"
-- "\"test\""
string :: Bool -> B.Builder -> B.Builder
string False str@(head' -> "\"") = "\"" <> string True (tail' str)
string True str@(head' -> "\"") = "\""
string True str = let (head, tail) = splitAtHead str in head <> string True tail

-- | Extract the first string value starting from the front.
firstString :: B.Builder -> B.Builder
firstString str = string False str


-- | Check if the given character is a whitespace
isWhitespace x
  | x == ' ' = True
  | x == '\r' = True
  | x == '\n' = True
  | x == '\t' = True
  | otherwise = False

-- | Check if the given character is end of json value
isEndOfValue x
 | isWhitespace x = True
 | x == ',' = True
 | x == '}' = True
 | x == ']' = True
 | otherwise = False

-- | Extract json value from the given text
--
-- Examples
--
-- >>> extractJsonValue "123131}"
-- "123131"
--
-- >>> extractJsonValue "false,true,false]"
-- "false"
extractJsonValue :: B.Builder -> B.Builder
extractJsonValue = liftToBuilder $ T.takeWhile (not . isEndOfValue)

-- |Beautify the given JSON text based on the current indentation level.
beautifyText :: IndentationLevel -> B.Builder -> B.Builder
beautifyText i str
  | str == "" = ""
  | head == " " = stripStartThenParse str
  | head == "\n" = stripStartThenParse str
  | head == "\t" = stripStartThenParse str
  | head == "{" = nextLineAfterOpening "{"
  | head == "[" = nextLineAfterOpening "["
  | head == "}" = nextLineAfterClosing "}"
  | head == "]" = nextLineAfterClosing "]"
  | head == "," = "," <> newline <> indent i (beautifyText i (trimmedTail str))
  | head == "\"" =
      let groupedString = firstString str
          restOfTheString = drop' (length' groupedString) str
          in groupedString <> beautifyText i restOfTheString
  | head == ":" = ": " <> beautifyText i (trimmedTail str)
  | otherwise =
      let value = extractJsonValue str
          valueLength = length' value
          in value <> beautifyText i (drop' valueLength str)
  where
    head = head' str
    stripStartThenParse = beautifyText i . stripStart'
    nextLineAfterOpening token = token
      <> newline
      <> indent (i + 1) (beautifyText (i + 1) (trimmedTail str))
    nextLineAfterClosing token = newline
      <> indent (i - 1) token
      <> beautifyText (i - 1) (trimmedTail str)


beautify :: T.Text -> T.Text
beautify = B.toLazyText . beautifyText 0 . B.fromLazyText
