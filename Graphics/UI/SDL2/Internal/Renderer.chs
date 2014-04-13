{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# OPTIONS -fno-warn-missing-signatures #-}
module Graphics.UI.SDL2.Internal.Renderer(
  Renderer,
  mkRenderer,
  mkUnhandledRenderer,
  setRenderTarget,
  getRenderTarget,
  withRenderer,
  peekRenderer) where
import Foreign
import Foreign.C

import Control.Monad
import Data.IORef

{#import Graphics.UI.SDL2.Internal.Error#}
{#import Graphics.UI.SDL2.Internal.Texture#}

#include <SDL2/SDL_render.h>
{#context lib = "SDL2"#}

data Renderer = Renderer{
  renderPtr :: ForeignPtr Renderer,
  renderTarget :: IORef (Maybe Texture)
} deriving (Eq)
{#pointer *SDL_Renderer as Renderer foreign newtype nocode#}

withRenderer :: Renderer -> (Ptr Renderer -> IO a) -> IO a
withRenderer Renderer{renderPtr = p} f =
  withForeignPtr p f

foreign import ccall "SDL2/SDL_render.h &SDL_DestroyRenderer"
  destroyRenderer :: FunPtr(Ptr Renderer -> IO())

mkRenderer :: Ptr Renderer -> IO Renderer
mkRenderer p = do
  t <- newIORef Nothing
  liftM (\x -> Renderer x t) . newForeignPtr destroyRenderer =<< checkNull p

mkUnhandledRenderer :: Ptr Renderer -> IO Renderer
mkUnhandledRenderer p = do 
  t <- newIORef Nothing
  liftM (\x -> Renderer x t) . newForeignPtr_ =<< checkNull p

peekRenderer :: Ptr (Ptr Renderer) -> IO Renderer
peekRenderer p = mkRenderer =<< peekWCheck p

{- | Set a texture as the current rendering target.-}
setRenderTarget ::
  Renderer -> -- ^ The renderer.
  Texture  -> -- ^ The targeted texture, which must be
              -- created with the TextureAccessTarget flag, or NULL
              -- for the default render target
  IO ()
setRenderTarget r t = do
 c_setRenderTarget r t
 writeIORef (renderTarget r) (Just t)

{#fun SDL_SetRenderTarget as c_setRenderTarget
  {
   withRenderer* `Renderer'
   ,withTexture*  `Texture'
  } -> `() ' checkError*#}

{- | Get the current render target or NULL for the default render target. -}
getRenderTarget :: Renderer -> IO Texture
getRenderTarget r = do
  mt <- readIORef (renderTarget r)
  case mt of
    Nothing -> c_getRenderTarget r
    (Just t) -> return t

{#fun SDL_GetRenderTarget as c_getRenderTarget
  {
   withRenderer* `Renderer'
  } -> `Texture' mkTexture*#}

