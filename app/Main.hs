module Main where

import Options.Applicative
import Relude

import CLI.Parsers
import CLI.Run

main :: IO ()
main = do
  let preferences = prefs $ showHelpOnError <> showHelpOnEmpty
      opts = info (parseCommand <**> helper) 
                  (fullDesc <> progDesc "A p2p aftermarket Cardano protocol for financial assets.")
  customExecParser preferences opts >>= runCommand
