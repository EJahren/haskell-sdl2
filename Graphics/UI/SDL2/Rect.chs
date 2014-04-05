{- | Defines SDL Rectangles (Rect) and
  operations on these. -}
module Graphics.UI.SDL2.Rect where
{-# LANGUAGE CPP, BangPatterns, ForeignFunctionInterface #-}
import Foreign
import Foreign.C.String
import Foreign.C.Types

import Control.Monad
import Control.Applicative

#include <SDL2/SDL_rect.h>

-- | A rectangle, with the origin at the upper left.
data Rect = Rect {
  xCoord :: Int,
  yCoord :: Int,
  width :: Int,
  height :: Int}
 
{#pointer *SDL_Rect as RectPtr -> Rect#}

instance Storable Rect where
  sizeOf _ = {#sizeof SDL_Rect #}
  alignment _ = {#alignof SDL_Rect #}
  peek p = do
    liftM4 Rect
      (fromIntegral <$> {#get SDL_Rect.x#} p)
      (fromIntegral <$> {#get SDL_Rect.y#} p)
      (fromIntegral <$> {#get SDL_Rect.w#} p)
      (fromIntegral <$> {#get SDL_Rect.h#} p)
  poke p (Rect x y w h) = do
    {#set SDL_Rect.x#} p (fromIntegral x)
    {#set SDL_Rect.y#} p (fromIntegral y)
    {#set SDL_Rect.w#} p (fromIntegral w)
    {#set SDL_Rect.h#} p (fromIntegral h)
