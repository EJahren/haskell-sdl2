{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# OPTIONS -fno-warn-missing-signatures #-}
module Graphics.UI.SDL2.Internal.Renderer(
  Renderer,
  mkRenderer,
  mkUnhandledRenderer,
  withRenderer,
  peekRenderer) where
import Foreign

import Control.Monad

{#import Graphics.UI.SDL2.Internal.Error#}

#include <SDL2/SDL_render.h>
{#context lib = "SDL2"#}

{#pointer *SDL_Renderer as Renderer foreign newtype#}

foreign import ccall "SDL2/SDL_render.h &SDL_DestroyRenderer"
  destroyRenderer :: FunPtr(Ptr Renderer -> IO())

mkRenderer :: Ptr Renderer -> IO Renderer
mkRenderer p = liftM Renderer . newForeignPtr destroyRenderer =<< checkNull p

mkUnhandledRenderer :: Ptr Renderer -> IO Renderer
mkUnhandledRenderer p = liftM Renderer . newForeignPtr_ =<< checkNull p

peekRenderer :: Ptr (Ptr Renderer) -> IO Renderer
peekRenderer p = mkRenderer =<< peekWCheck p
