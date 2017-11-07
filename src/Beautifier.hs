{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Beautifier
  ( beautify
  , indent
  , firstString
  ) where

import           Data.Monoid ((<>))
import qualified Data.Text   as T

type IndentationLevel = Int

indentation :: T.Text
indentation = "  "

newline :: T.Text
newline = "\n"

indent :: IndentationLevel -> T.Text -> T.Text
indent level str = T.replicate level indentation <> str

trimmedTail :: T.Text -> T.Text
trimmedTail = T.stripStart . T.tail

string :: Bool -> T.Text -> T.Text
string False str@(T.head -> '\"') = "\"" <> string True (T.tail str)
-- TODO: Take care of escaped double quote e.g. "\"test\""
string True str@(T.head -> '"') = "\""
string True str = (T.pack [T.head str]) <> string True (T.tail str)

-- | Extract the first string value starting from the front
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
