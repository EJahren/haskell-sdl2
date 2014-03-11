{-# LANGUAGE NoMonomorphismRestriction #-}
module Graphics.UI.SDL2.Common where

enumToC = fromIntegral . fromEnum
enumFromC = toEnum . fromIntegral
