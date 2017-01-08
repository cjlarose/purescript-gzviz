module BitStream (BitStream(..), asBitstream, null, uncons) where

import Prelude
import Data.Int (floor)
import Data.Int.Bits ((.&.), shl)
import Data.Maybe (Maybe(..))

import Data.ArrayBuffer.Types (Uint8Array)
import Data.TypedArray (byteLength, unsafeIndex)

newtype BitStream = BitStream { arr :: Uint8Array, offset :: Int }

asBitstream :: Uint8Array -> BitStream
asBitstream arr = BitStream { arr: arr, offset: 0 }

testBit :: Int -> Int -> Boolean
testBit x i = x .&. (1 `shl` i) /= 0

unsafeBitAt :: Uint8Array -> Int -> Boolean
unsafeBitAt arr offset = testBit (floor (arr `unsafeIndex` (offset `div` 8))) (offset `mod` 8)

null :: BitStream -> Boolean
null (BitStream { arr, offset }) = offset >= floor (byteLength arr) * 8

bitLength :: BitStream -> Int
bitLength (BitStream { arr, offset }) = floor (byteLength arr) * 8 - offset

uncons :: BitStream -> Maybe { head :: Boolean, tail :: BitStream }
uncons bs@(BitStream { arr, offset }) | null bs = Nothing
                                      | otherwise = Just { head: unsafeBitAt arr offset,
                                                           tail: BitStream { arr: arr, offset: offset + 1 } }

