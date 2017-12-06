{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import qualified Beautifier             as B
import           System.Console.CmdArgs ((&=))
import qualified System.Console.CmdArgs as CA
import qualified System.Posix.IO        as IO
import qualified System.Posix.Terminal  as T

data Args = Args { jsonString :: String }
  deriving (Show, CA.Data, CA.Typeable)


args = Args { jsonString = CA.def &= CA.typ "JSON String" &= CA.argPos 0 }
    &= CA.summary "JSON beautifier tool version 0.0.1"

main :: IO ()
main = T.queryTerminal IO.stdInput >>= run

printBeautified :: String -> IO ()
printBeautified = putStrLn . B.beautify

runFromStdInput :: IO ()
runFromStdInput = getContents >>= printBeautified

runFromArgs :: Args -> IO ()
runFromArgs (Args { jsonString = str }) = printBeautified str

run :: Bool -> IO ()
run False = runFromStdInput
run True = CA.cmdArgs args >>= runFromArgs
