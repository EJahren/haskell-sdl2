module Graphics.UI.SDL2.Rect where
{-# LANGUAGE CPP, BangPatterns, ForeignFunctionInterface #-}
import Foreign
import Foreign.C.String
import Foreign.C.Types

#include <SDL2/SDL_rect.h>

type CRect = ()

data Rect = Rect !(Ptr CRect)

noRect = Rect nullPtr
