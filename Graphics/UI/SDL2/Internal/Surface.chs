{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# OPTIONS -fno-warn-missing-signatures #-}
module Graphics.UI.SDL2.Internal.Surface(
 Surface,
 mkSurface,
 mkUnhandledSurface,
 withSurfacePtr,
 peekSurface) where
import Foreign

import Control.Monad

{#import Graphics.UI.SDL2.Internal.Error#}

#include <SDL2/SDL_surface.h>
{#context lib = "SDL2"#}

{#pointer *SDL_Surface as Surface foreign newtype#}

foreign import ccall "SDL2/SDL_surface.h &SDL_FreeSurface"
  freeSurface :: FunPtr(Ptr Surface -> IO())

withSurfacePtr = withSurface

mkSurface :: Ptr Surface -> IO Surface
mkSurface p = liftM Surface . newForeignPtr freeSurface =<< checkNull p

mkUnhandledSurface :: Ptr Surface -> IO Surface
mkUnhandledSurface p = liftM Surface . newForeignPtr_ =<< checkNull p

peekSurface :: Ptr (Ptr Surface) -> IO Surface
peekSurface p = mkSurface =<< peekWCheck p
