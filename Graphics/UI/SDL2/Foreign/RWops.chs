{-# LANGUAGE CPP, BangPatterns, ForeignFunctionInterface #-}
module Graphics.UI.SDL2.Foreign.RWops(
  RWops,
  mkRWops,
  peekRWops,
  withRWops) where
import Foreign
import Foreign.C
import Foreign.C.String
import Foreign.C.Types

import Control.Monad

#include <SDL2/SDL_rwops.h>

{#context lib = "sdl2"#}

{#pointer *SDL_RWops as RWops foreign newtype#}

foreign import ccall "SDL2/SDL_rwops.h &SDL_FreeRW"
  freeRWops :: FunPtr(Ptr RWops -> IO())

mkRWops = liftM RWops . newForeignPtr freeRWops 
peekRWops p = mkRWops =<< peek p
