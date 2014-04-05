{-# LANGUAGE CPP, BangPatterns, ForeignFunctionInterface #-}
module Graphics.UI.SDL2.Foreign.Renderer(
  Renderer,
  mkRenderer,
  withRenderer,
  peekRenderer) where
import Foreign
import Foreign.C.String
import Foreign.C.Types

import Control.Monad

#include <SDL2/SDL_render.h>
{#context lib = "sdl2"#}

peekRenderer p = mkRenderer =<< peek p

mkRenderer = liftM Renderer . newForeignPtr destroyRenderer

foreign import ccall "SDL2/SDL_render.h &SDL_DestroyRenderer"
  destroyRenderer :: FunPtr(Ptr Renderer -> IO())

{#pointer *SDL_Renderer as Renderer foreign newtype#}
