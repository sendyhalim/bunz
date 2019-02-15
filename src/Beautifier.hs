{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Beautifier
  ( beautify
  , indentation
  , firstString
  , splitAtHead
  ) where

import           Data.Int    (Int64)
import           Data.Monoid ((<>))
import qualified Data.Text   as T

-- Doctest setup
-- $setup
-- >>> :set -XOverloadedStrings

type IndentationLevel = Int

spaceIndentation :: T.Text
spaceIndentation = "  "

indentation :: IndentationLevel -> T.Text
indentation level = (T.replicate level spaceIndentation)

newline :: T.Text
newline = "\n"

trimmedTail :: T.Text -> T.Text
trimmedTail = T.stripStart . T.tail

-- | Split the given string at its head while honouring escaped string.
-- Examples:
--
-- >>> splitAtHead "whatsup"
-- ("w","hatsup")
--
-- >>> splitAtHead "\nyeay"
-- ("\n","yeay")
splitAtHead :: T.Text -> (T.Text, T.Text)
splitAtHead ""                   = ("", "")
splitAtHead str@(T.head -> '\\') = T.splitAt 2 str
splitAtHead str                  = T.splitAt 1 str

-- | Extract string within double quotes `"` including the double quotes ignoring
-- the suffix after closing quote `"`
--
-- Examples:
--
-- >>> string False "\"test\"whatuppp"
-- "\"test\""
string :: Bool -> T.Text -> [T.Text]
string False str@(T.head -> '"') = ["\""] ++ string True (T.tail str)
string True str@(T.head -> '\"') = ["\""]
string True str = let (head, tail) = splitAtHead str in [head] ++ string True tail

-- | Extract the first string value starting from the front.
firstString :: T.Text -> [T.Text]
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
extractJsonValue :: T.Text -> T.Text
extractJsonValue = T.takeWhile (not . isEndOfValue)

-- |Beautify the given JSON text based on the current indentation level.
beautifyText :: IndentationLevel -> T.Text -> [T.Text]
beautifyText i str
  | str == "" = [""]
  | head == ' ' = stripStartThenParse str
  | head == '\n' = stripStartThenParse str
  | head == '\t' = stripStartThenParse str
  | head == '{' = nextLineAfterOpening "{"
  | head == '[' = nextLineAfterOpening "["
  | head == '}' = nextLineAfterClosing "}"
  | head == ']' = nextLineAfterClosing "]"
  | head == ',' = [",", newline, indentation i] ++ (beautifyText i (trimmedTail str))
  | head == '"' =
      let groupedString = mconcat $ firstString str
          restOfTheString = T.drop (T.length groupedString) str
          in [groupedString] ++ beautifyText i restOfTheString
  | head == ':' = [": "] ++ beautifyText i (trimmedTail str)
  | otherwise =
      let value = extractJsonValue str
          valueLength = T.length value
          in [value] ++ beautifyText i (T.drop valueLength str)
  where
    head = T.head str
    stripStartThenParse = beautifyText i . T.stripStart
    nextLineAfterOpening token = [token , newline , indentation (i + 1)] ++ (beautifyText (i + 1) (trimmedTail str))
    nextLineAfterClosing token = [newline, indentation (i - 1), token] ++ beautifyText (i - 1) (trimmedTail str)


beautify :: T.Text -> T.Text
beautify = mconcat . beautifyText 0
