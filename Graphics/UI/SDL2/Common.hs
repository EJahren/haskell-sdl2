{-# LANGUAGE NoMonomorphismRestriction #-}
module Graphics.UI.SDL2.Common where

import Foreign
import Foreign.C
import Foreign.C.Types

import Data.List
import Data.Bits

enumToC = fromIntegral . fromEnum
enumFromC = toEnum . fromIntegral

flagToC = foldl' (.|.) 0 . map enumToC

checkErrorCode msg x
  | x == 0 = ()
  | otherwise = error msg

withPtr x f = alloca (\p -> poke p x >> f p)
