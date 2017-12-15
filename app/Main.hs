{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import qualified Beautifier             as B
import qualified Data.Text.Lazy         as T
import qualified Data.Text.Lazy.IO      as TextIO
import           System.Console.CmdArgs ((&=))
import qualified System.Console.CmdArgs as CA
import           System.Posix.IO        (stdInput)
import           System.Posix.Terminal  (queryTerminal)

data Args = Args { jsonString :: String }
  deriving (Show, CA.Data, CA.Typeable)


args = Args { jsonString = CA.def &= CA.typ "JSON String" &= CA.argPos 0 }
    &= CA.summary "JSON beautifier tool version 0.0.1"

main :: IO ()
main = queryTerminal stdInput >>= run

printBeautified :: T.Text -> IO ()
printBeautified = TextIO.putStr . B.beautify

runFromStdInput :: IO ()
runFromStdInput = TextIO.getContents >>= printBeautified

runFromArgs :: Args -> IO ()
runFromArgs (Args { jsonString = str }) = printBeautified $ T.pack str

run :: Bool -> IO ()
run False = runFromStdInput
run True = CA.cmdArgs args >>= runFromArgs
