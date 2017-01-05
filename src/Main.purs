module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)

import Node.FS (FS)
import Node.FS.Sync (readFile)

main :: forall e. Eff (console :: CONSOLE, err :: EXCEPTION, fs :: FS | e) Unit
main = do
  contents <- readFile "examples/moment.min.js.gz"
  log <<< show $ contents
