module Character
    ( isEscaped
    ) where

import           Control.Monad

isEscaped :: String -> Bool
isEscaped ('\\':x) = True
isEscaped _ = False
