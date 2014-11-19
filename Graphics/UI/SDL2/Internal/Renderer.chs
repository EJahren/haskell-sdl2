{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# OPTIONS -fno-warn-missing-signatures #-}
module Graphics.UI.SDL2.Internal.Renderer(
  Renderer,
  RendererFlag(..),
  mkRenderer,
  setRenderTarget,
  getRenderTarget,
  withRendererPtr,
  createRenderer,
  peekRenderer) where
import Foreign
import Foreign.C

import Control.Monad
import Data.IORef

import Graphics.UI.SDL2.Common
{#import Graphics.UI.SDL2.Internal.Error#}
{#import Graphics.UI.SDL2.Internal.Texture#}
{#import Graphics.UI.SDL2.Internal.Window#}

#include <SDL2/SDL_render.h>
{#context lib = "SDL2"#}

-- | Flags used when creating a rendering context
{#enum SDL_RendererFlags as RendererFlag
   {
    SDL_RENDERER_SOFTWARE as RendererSoftware -- | The renderer is a software fallback.
    ,SDL_RENDERER_ACCELERATED as RendererAccelerated -- | The renderer uses hardware acceleration.
    ,SDL_RENDERER_PRESENTVSYNC as RendererPresentVSync -- | Present is synchronized with the refresh rate.
    ,SDL_RENDERER_TARGETTEXTURE as RendererTargetTexture -- | The renderer supports rendering to texture.
   } 
deriving (Eq,Ord,Show)#}

data Renderer = Renderer{
  renderPtr :: Ptr Renderer,
  renderTarget :: IORef (Maybe Texture)
} deriving (Eq)
{#pointer *SDL_Renderer as Renderer foreign newtype nocode#}

withRendererPtr :: Renderer -> (Ptr Renderer -> IO a) -> IO a
withRendererPtr Renderer{renderPtr = p} f =
  f p

foreign import ccall "SDL2/SDL_render.h &SDL_DestroyRenderer"
  destroyRenderer :: FunPtr(Ptr Renderer -> IO())

mkRenderer :: Ptr Renderer -> IO Renderer
mkRenderer p = do
  t <- newIORef Nothing
  liftM (\x -> Renderer x t) (checkNull p)

peekRenderer :: Ptr (Ptr Renderer) -> IO Renderer
peekRenderer p = mkRenderer =<< peekWCheck p

{- | Set a texture as the current rendering target.-}
setRenderTarget ::
  Renderer -- ^ The renderer.
  -> Texture -- ^ The targeted texture, which must be
              -- created with the TextureAccessTarget flag, or NULL
              -- for the default render target
  -> IO ()
setRenderTarget r t = do
 c_setRenderTarget r t
 writeIORef (renderTarget r) (Just t)

{#fun SDL_SetRenderTarget as c_setRenderTarget
  {
   withRendererPtr* `Renderer'
   ,withTexturePtr*  `Texture'
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
   withRendererPtr* `Renderer'
  } -> `Texture' mkTexture*#}

{#fun SDL_CreateRenderer as createRenderer
  {
    withWindowPtr* `Window'
    ,`Int32'
    ,flagToC `[RendererFlag]'
  } -> `Renderer' mkRenderer* #}
