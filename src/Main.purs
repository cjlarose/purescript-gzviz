module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Int (floor)
import Data.Int.Bits ((.&.), shl)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.State (gets, modify)

import Node.FS (FS)
import Node.FS.Sync (readFile)
import Node.Buffer (Buffer)

import Text.Parsing.Parser (ParserT, ParseState(..), runParser, fail)

import Data.ArrayBuffer.Types (Uint8Array)
import Data.TypedArray (byteLength, unsafeIndex)

foreign import asUint8Array :: Buffer -> Uint8Array

testBit :: Int -> Int -> Boolean
testBit x i = x .&. (1 `shl` i) /= 0

bitAt :: Uint8Array -> Int -> Boolean
bitAt arr offset = testBit (floor (arr `unsafeIndex` (offset `div` 8))) (7 - offset `mod` 8)

uncons :: BitStream -> Maybe { head :: Boolean, tail :: BitStream }
uncons (BitStream { arr, offset }) | offset >= floor (byteLength arr) * 8 = Nothing
                                   | otherwise = Just { head: bitAt arr offset,
                                                        tail: BitStream { arr: arr, offset: offset + 1 } }

null :: BitStream -> Boolean
null (BitStream { arr, offset }) = offset >= floor (byteLength arr) * 8

eof :: forall m. Monad m => ParserT BitStream m Unit
eof = do
  input <- gets \(ParseState input _ _) -> input
  unless (null input) (fail "Expected EOF")

anyBit :: forall m. Monad m => ParserT BitStream m Boolean
anyBit = do
  input <- gets \(ParseState input _ _) -> input
  case uncons input of
    Nothing -> fail "Unexpected EOF"
    Just { head, tail } -> do
      modify \(ParseState _ position _) ->
        ParseState tail position true -- TODO: Update position
      pure head

binaryDocument :: forall m. Monad m => ParserT BitStream m (Array (Array Boolean))
binaryDocument = do
  a <- anyBit
  b <- anyBit
  c <- anyBit
  d <- anyBit
  e <- anyBit
  f <- anyBit
  g <- anyBit
  h <- anyBit
  pure [[a, b, c, d, e, f, g, h]]

newtype BitStream = BitStream { arr :: Uint8Array, offset :: Int }

asBitstream :: Uint8Array -> BitStream
asBitstream arr = BitStream { arr: arr, offset: 0 }

main :: forall e. Eff (console :: CONSOLE, err :: EXCEPTION, fs :: FS | e) Unit
main = do
  contents <- readFile "examples/moment.min.js.gz"
  let bitstream = asBitstream <<< asUint8Array $ contents
  case runParser bitstream binaryDocument of
    Left err -> log <<< show $ err
    Right result -> log <<< show $ result
