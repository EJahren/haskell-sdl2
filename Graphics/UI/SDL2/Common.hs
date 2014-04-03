{-# LANGUAGE NoMonomorphismRestriction #-}
module Graphics.UI.SDL2.Common where

import Data.List
import Data.Bits

enumToC = fromIntegral . fromEnum
enumFromC = toEnum . fromIntegral

flagToC = foldl' (.|.) 0 . map enumToC

checkErrorCode msg x
  | x == 0 = ()
  | otherwise = error msg
