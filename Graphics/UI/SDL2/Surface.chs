{-# LANGUAGE CPP, BangPatterns, ForeignFunctionInterface #-}
module Graphics.UI.SDL2.Surface(
 Surface,
 loadBmpRw,
 loadBmp,
 upperBlit,
 blit) where
import Foreign
import Foreign.C
import Foreign.C.String
import Foreign.C.Types

import Control.Monad

import Graphics.UI.SDL2.Common

{# import Graphics.UI.SDL2.Error #}
{# import Graphics.UI.SDL2.RWops #}
{# import Graphics.UI.SDL2.Rect #}
{# import Graphics.UI.SDL2.Foreign.Surface#}
{# import Graphics.UI.SDL2.Foreign.RWops#}

#include <SDL2/SDL_surface.h>

{#fun SDL_LoadBMP_RW as loadBmpRw
  {withRWops* `RWops',`Int'} -> `Surface' mkSurface* #}

loadBmp file = do
  rw <- rwFromFile file "rb"
  loadBmpRw rw 1


{#fun SDL_UpperBlit as upperBlit
 {withSurface* `Surface', 
  withPtr* `Rect',
  withSurface* `Surface',
  withPtr* `Rect'} -> `()' checkError*- #}

blit s1' s2' =
  withSurface s1' (\s1 ->
  withSurface s2' (\s2 ->
    upperBlit'_ s1 nullPtr s2 nullPtr))
