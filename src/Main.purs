module Main where

import Prelude
import Data.Either (Either(..))

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)

import Node.FS (FS)
import Node.FS.Sync (readFile)
import Node.Buffer (Buffer)

import Text.Parsing.Parser (runParser)

import Data.ArrayBuffer.Types (Uint8Array)

import BitStream (asBitstream)
import BitStream.Parsers (binaryDocument)

foreign import asUint8Array :: Buffer -> Uint8Array

main :: forall e. Eff (console :: CONSOLE, err :: EXCEPTION, fs :: FS | e) Unit
main = do
  contents <- readFile "examples/moment.min.js.gz"
  let bitstream = asBitstream <<< asUint8Array $ contents
  case runParser bitstream binaryDocument of
    Left err -> log <<< show $ err
    Right result -> log <<< show $ result
