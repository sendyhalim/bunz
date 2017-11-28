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

beautify :: IndentationLevel -> T.Text -> T.Text
beautify _ "" = ""
beautify _ str@(T.length -> 0) = str
beautify i str@(T.head -> ' ') = beautify i (T.stripStart str)
beautify i str@(T.head -> '\n') = beautify i (T.stripStart str)
beautify i str@(T.head -> '\t') = beautify i (T.stripStart str)
beautify i str@(T.head -> '{') = "{" <> newline <> beautify (i + 1) (trimmedTail str)
beautify i str@(T.head -> '[') = "[" <> newline <> beautify (i + 1) (trimmedTail str)
-- TODO: Add more pattern match
