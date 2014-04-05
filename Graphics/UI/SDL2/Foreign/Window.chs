{-# LANGUAGE CPP, BangPatterns, ForeignFunctionInterface #-}
module Graphics.UI.SDL2.Foreign.Window
  (Window,
  mkWindow,
  withWindow,
  peekWindow) where
import Foreign
import Foreign.C
import Foreign.C.String
import Foreign.C.Types

import Control.Monad

#include <SDL2/SDL_video.h>
{#context lib = "sdl2"#}

{#pointer *SDL_Window as Window foreign newtype#}

foreign import ccall "SDL2/SDL_video.h &SDL_DestroyWindow"
  destroyWindow :: FunPtr(Ptr Window -> IO())

mkWindow = liftM Window . newForeignPtr destroyWindow
peekWindow p = mkWindow =<< peek p
