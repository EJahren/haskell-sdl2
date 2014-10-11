{-# LANGUAGE CPP, BangPatterns, ForeignFunctionInterface #-}
module Graphics.UI.SDL2.Surface(
 Surface,
 loadBmpRw,
 loadBmp,
 upperBlit
 ) where
import Foreign
import Foreign.C
import Foreign.C.Types


import Graphics.UI.SDL2.Common

{# import Graphics.UI.SDL2.RWops #}
{# import Graphics.UI.SDL2.Rect #}
{# import Graphics.UI.SDL2.Internal.Error #}
{# import Graphics.UI.SDL2.Internal.Surface#}
{# import Graphics.UI.SDL2.Internal.RWops#}

#include <SDL2/SDL_surface.h>
#include <SDL2/SDL_rect.h>

{#fun SDL_LoadBMP_RW as loadBmpRw
  {
   withRWopsPtr* `RWops',
   `Int'
  }
 -> `Surface' mkSurface* #}

loadBmp :: String -> IO Surface
loadBmp file = do
  rw <- rwFromFile file "rb"
  loadBmpRw rw 0

{#fun SDL_UpperBlit as upperBlit
 {withSurfacePtr* `Surface', 
  withMayPtr* `Maybe Rect',
  withSurfacePtr* `Surface',
  withMayPtr* `Maybe Rect'} -> `() ' checkError*- #}
