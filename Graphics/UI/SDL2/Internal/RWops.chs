{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# OPTIONS -fno-warn-missing-signatures #-}
module Graphics.UI.SDL2.Internal.RWops(
  RWops,
  mkRWops,
  mkUnhandledRWops,
  peekRWops,
  withRWopsPtr) where
import Foreign

import Control.Monad 

{#import Graphics.UI.SDL2.Internal.Error#}

#include <SDL2/SDL_rwops.h>
{#context lib = "SDL2"#}

{#pointer *SDL_RWops as RWops foreign newtype#}

foreign import ccall "SDL2/SDL_rwops.h &SDL_FreeRW"
  freeRWops :: FunPtr(Ptr RWops -> IO())

withRWopsPtr = withRWops

mkRWops :: Ptr RWops -> IO RWops
mkRWops p = liftM RWops . newForeignPtr freeRWops =<< checkNull p

mkUnhandledRWops :: Ptr RWops -> IO RWops
mkUnhandledRWops p = liftM RWops . newForeignPtr_ =<< checkNull p

peekRWops :: Ptr (Ptr RWops) -> IO RWops
peekRWops p = mkRWops =<< peekWCheck p
