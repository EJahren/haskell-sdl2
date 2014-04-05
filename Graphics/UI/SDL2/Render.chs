{-# LANGUAGE CPP, BangPatterns, ForeignFunctionInterface #-}
module Graphics.UI.SDL2.Render(
 Renderer,
 createWindowAndRenderer) where
import Foreign
import Foreign.C.String
import Foreign.C.Types

import Control.Monad

import Graphics.UI.SDL2.Common
{# import Graphics.UI.SDL2.Error #}
{# import Graphics.UI.SDL2.Video#}
{# import Graphics.UI.SDL2.Foreign.Window#}
{# import Graphics.UI.SDL2.Foreign.Renderer#}
#include <SDL2/SDL_render.h>

{#fun unsafe SDL_CreateWindowAndRenderer as createWindowAndRenderer
 {`Int',`Int',flagToC `[WindowFlag]',
   alloca- `Window' peekWindow*,
   alloca- `Renderer' peekRenderer*} 
  -> `()' checkError*-#} 
