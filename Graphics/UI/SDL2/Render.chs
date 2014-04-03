module Graphics.UI.SDL2.Render where
{-# LANGUAGE CPP, BangPatterns, ForeignFunctionInterface #-}
import Foreign
import Foreign.C.String
import Foreign.C.Types

import Control.Monad

import Graphics.UI.SDL2.Common
{# import Graphics.UI.SDL2.Video #}
{# import Graphics.UI.SDL2.Error #}
#include <SDL2/SDL_render.h>

peekWindow = liftM Window . peek
peekRenderer p = liftM Renderer . newForeignPtr destroyRenderer =<< peek p

foreign import ccall "SDL2/SDL_render.h &SDL_DestroyRenderer"
  destroyRenderer :: FunPtr(Ptr Renderer -> IO())

{#pointer *SDL_Renderer as Renderer foreign newtype#}

{#fun unsafe SDL_CreateWindowAndRenderer as createWindowAndRenderer
 {`Int',`Int',flagToC `[WindowFlag]',
   alloca- `Window' peekWindow*,
   alloca- `Renderer' peekRenderer*} 
  -> `()' checkError*-#} 
