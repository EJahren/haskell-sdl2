{-# LANGUAGE NoMonomorphismRestriction #-}
module Graphics.UI.SDL2.Common where

import Foreign

import Data.List

enumToC :: (Enum a, Num c) => a -> c
enumToC = fromIntegral . fromEnum
enumFromC :: (Enum a, Integral c) => c -> a
enumFromC = toEnum . fromIntegral

flagToC :: (Enum a,Bits b, Num b) => [a] -> b
flagToC = foldl' (.|.) 0 . map enumToC

withPtr :: (Storable a) => a -> (Ptr a -> IO b) -> IO b
withPtr x f = alloca (\p -> poke p x >> f p)

withMayPtr :: (Storable a) => Maybe a -> (Ptr a -> IO b) -> IO b
withMayPtr (Just x) f = withPtr x f
withMayPtr Nothing f = f nullPtr 

withArr xs f = withArrayLen xs (\x y -> f (y,fromIntegral x))
