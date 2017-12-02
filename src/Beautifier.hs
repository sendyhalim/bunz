{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Beautifier
  ( beautify
  , indent
  , firstString
  , splitAtHead
  ) where

import           Data.Monoid ((<>))
import qualified Data.Text   as T

-- Doctest setup
-- $setup
-- >>> :set -XOverloadedStrings

type IndentationLevel = Int

indentation :: T.Text
indentation = "  "

newline :: T.Text
newline = "\n"

indent :: IndentationLevel -> T.Text -> T.Text
indent level str = T.replicate level indentation <> str

trimmedTail :: T.Text -> T.Text
trimmedTail = T.stripStart . T.tail

-- |Split the given string at its head while honouring escaped string.
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

-- |Extract string within double quotes `"` including the double quotes ignoring
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


takeUntilEnd :: T.Text -> T.Text
takeUntilEnd = T.takeWhile (\x -> x == '\n' || x == ',')

beautifyText :: IndentationLevel -> T.Text -> T.Text
beautifyText _ "" = ""
beautifyText _ str@(T.length -> 0) = str
beautifyText i str@(T.head -> ' ') = beautifyText i (T.stripStart str)
beautifyText i str@(T.head -> '\n') = beautifyText i (T.stripStart str)
beautifyText i str@(T.head -> '\t') = beautifyText i (T.stripStart str)
beautifyText i str@(T.head -> '{') = "{" <> newline
  <> indent i (beautifyText (i + 1) (trimmedTail str))
beautifyText i str@(T.head -> '[') = "[" <> newline
  <> indent i (beautifyText (i + 1) (trimmedTail str))
beautifyText i str@(T.head -> ',') = "," <> newline
  <> indent i (beautifyText (i + 1) (trimmedTail str))
beautifyText i str@(T.head -> '{') = "{" <> newline
  <> indent i (beautifyText (i + 1) (trimmedTail str))
beautifyText i str@(T.head -> '}') = newline <> "}"
  <> indent i (beautifyText (i - 1) (trimmedTail str))
beautifyText i str@(T.head -> ']') = newline <> "]"
  <> indent i (beautifyText (i - 1) (trimmedTail str))
beautifyText i str@(T.head -> '"') = groupedString <> beautifyText i restOfTheString
  where
    groupedString = firstString str
    restOfTheString = T.drop (T.length groupedString) str
beautifyText i str@(T.head -> ':') = ": " <> beautifyText i (trimmedTail str)
beautifyText i str = value <> beautifyText i restOfTheString
  where
    value = takeUntilEnd str
    restOfTheString = T.drop (T.length value) str
beautify :: IndentationLevel -> String -> String
beautify i str = T.unpack $ beautifyText i $ T.pack str
