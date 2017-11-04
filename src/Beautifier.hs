{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Beautifier
  ( beautify
  , indent
  ) where

import           Data.Monoid ((<>))
import qualified Data.Text   as T

type IndentationLevel = Int

indentation :: T.Text
indentation = "  "

indent :: IndentationLevel -> T.Text -> T.Text
indent level str = T.replicate level indentation <> str

beautify :: IndentationLevel -> T.Text -> T.Text
beautify _ "" = ""
beautify _ str@(T.length -> 0) = str
beautify i str@(T.head -> ' ') = beautify i (T.stripStart str)
{- TODO: Add more pattern match -}
