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

indentation :: T.Text
indentation = "  "

newline :: B.Builder
newline = B.fromLazyText "\n"

indent :: IndentationLevel -> B.Builder -> B.Builder
indent level str = B.fromLazyText (T.replicate level indentation) <> str

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
splitAtHead :: T.Text -> (T.Text, T.Text)
splitAtHead "" = ("", "")
splitAtHead str@(T.head -> '\\') = T.splitAt 2 str
splitAtHead str = T.splitAt 1 str

-- | Extract string within double quotes `"` including the double quotes ignoring
-- the suffix after closing quote `"`
--
-- Examples:
--
-- >>> string False "\"test\"whatuppp"
-- "\"test\""
string :: Bool -> T.Text -> T.Text
string False str@(T.head -> '"') = "\"" <> string True (T.tail str)
string True str@(T.head -> '"') = "\""
string True str = let (head, tail) = splitAtHead str in head <> string True tail

-- | Extract the first string value starting from the front.
firstString :: T.Text -> T.Text
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
extractJsonValue = B.fromLazyText . T.takeWhile (not . isEndOfValue) . B.toLazyText

dropBuilder :: Int64 -> B.Builder -> B.Builder
dropBuilder length = B.fromLazyText . T.drop length . B.toLazyText

-- |Beautify the given JSON text based on the current indentation level.
beautifyText :: IndentationLevel -> B.Builder -> B.Builder
beautifyText i str
  | str == "" = ""
  | head == ' ' = stripStartThenParse str
  | head == '\n' = stripStartThenParse str
  | head == '\t' = stripStartThenParse str
  | head == '{' = nextLineAfterOpening "{"
  | head == '[' = nextLineAfterOpening "["
  | head == '}' = nextLineAfterClosing "}"
  | head == ']' = nextLineAfterClosing "]"
  | head == ',' = B.fromLazyText "," <> newline <> indent i (beautifyText i (trimmedTail str))
  | head == '"' = let groupedString = firstString (B.toLazyText str)
                      restOfTheString = B.fromLazyText $ T.drop (T.length groupedString) $ B.toLazyText str
                  in B.fromLazyText groupedString <> beautifyText i restOfTheString
  | head == ':' = ": " <> beautifyText i (trimmedTail str)
  | otherwise = let value = extractJsonValue str
                    valueLength = T.length $ B.toLazyText value
                 in value <> beautifyText i (dropBuilder valueLength str)
  where
    head = T.head $ B.toLazyText str
    stripStartThenParse = beautifyText i . B.fromLazyText . T.stripStart . B.toLazyText
    nextLineAfterOpening token = token
      <> newline
      <> indent (i + 1) (beautifyText (i + 1) (trimmedTail str))
    nextLineAfterClosing token = newline
      <> indent (i - 1) token
      <> beautifyText (i - 1) (trimmedTail str)


beautify :: T.Text -> T.Text
beautify = B.toLazyText . beautifyText 0 . B.fromLazyText
