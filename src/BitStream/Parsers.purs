module BitStream.Parsers (eof, octet, anyBit, binaryDocument) where

import Prelude
import Data.Maybe (Maybe(..))
import Control.Monad.State (gets, modify)

import Text.Parsing.Parser (ParserT, ParseState(..), fail)

import BitStream (BitStream, null, uncons)

eof :: forall m. Monad m => ParserT BitStream m Unit
eof = do
  input <- gets \(ParseState input _ _) -> input
  unless (null input) (fail "Expected EOF")

octet :: forall m. Monad m => Int -> ParserT BitStream m Unit
octet expected = do
  input <- gets \(ParseState input _ _) -> input
  pure unit

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

