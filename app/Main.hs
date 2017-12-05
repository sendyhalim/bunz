{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import qualified Beautifier             as B
import           System.Console.CmdArgs ((&=))
import qualified System.Console.CmdArgs as CA

data Args = Args { jsonString :: String }
  deriving (Show, CA.Data, CA.Typeable)


args = Args { jsonString = CA.def &= CA.typ "JSON String" &= CA.argPos 0 }
    &= CA.summary "JSON beautifier tool version 0.0.1"

main :: IO ()
main = CA.cmdArgs args >>= run

run :: Args -> IO ()
run (Args { jsonString = str }) = do
  putStrLn (B.beautify str)
