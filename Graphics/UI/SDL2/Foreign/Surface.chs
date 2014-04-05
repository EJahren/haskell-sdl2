{-# LANGUAGE CPP, BangPatterns, ForeignFunctionInterface #-}
module Graphics.UI.SDL2.Foreign.Surface(
 Surface,
 mkSurface,
 withSurface,
 peekSurface) where
import Foreign
import Foreign.C
import Foreign.C.String
import Foreign.C.Types

import Control.Monad
#include <SDL2/SDL_surface.h>

{#pointer *SDL_Surface as Surface foreign newtype#}

mkSurface = liftM Surface . newForeignPtr freeSurface

peekSurface p = mkSurface =<< peek p

foreign import ccall "SDL2/SDL_surface.h &SDL_FreeSurface"
  freeSurface :: FunPtr(Ptr Surface -> IO())
