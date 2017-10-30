{-# LANGUAGE OverloadedStrings #-}
module Character
    ( isEscaped
    ) where

import           Control.Monad
import qualified Data.String   as String
import qualified Data.Text     as Text

isEscaped :: String -> Bool
isEscaped ('\\':x) = True
isEscaped _ = False
