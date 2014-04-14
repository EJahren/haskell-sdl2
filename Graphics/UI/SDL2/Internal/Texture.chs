{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# OPTIONS -fno-warn-missing-signatures #-}
module Graphics.UI.SDL2.Internal.Texture(
  Texture,
  mkTexture,
  mkUnhandledTexture,
  peekTexture,
  withTexturePtr) where
import Foreign

import Control.Monad

{#import Graphics.UI.SDL2.Internal.Error#}

#include <SDL2/SDL_render.h>

{#context lib = "SDL2"#}

{#pointer *SDL_Texture as Texture foreign newtype#}

foreign import ccall "SDL2/SDL_render.h &SDL_DestroyTexture"
  destroyTexture :: FunPtr(Ptr Texture -> IO())

withTexturePtr = withTexture

mkTexture :: Ptr Texture -> IO Texture
mkTexture p = liftM Texture . newForeignPtr destroyTexture =<< checkNull p

mkUnhandledTexture :: Ptr Texture -> IO Texture
mkUnhandledTexture p = liftM Texture . newForeignPtr_ =<< checkNull p

peekTexture :: Ptr (Ptr Texture) -> IO Texture
peekTexture p = mkTexture =<< peekWCheck p
