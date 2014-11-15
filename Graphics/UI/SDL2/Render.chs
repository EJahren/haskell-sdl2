{-# LANGUAGE CPP, BangPatterns, ForeignFunctionInterface #-}
{-# OPTIONS -fno-warn-unused-matches #-}
{- |
This API supports the following features:
    * single pixel points
    * single pixel lines
    * filled rectangles
    * texture images

The primitives may be drawn in opaque, blended, or additive modes.

The texture images may be drawn in opaque, blended, or additive modes.
They can have an additional color tint or alpha modulation applied to
them, and may also be stretched with linear interpolation.

This API is designed to accelerate simple 2D operations. You may
want more functionality such as polygons and particle effects and
in that case you should use SDL's OpenGL/Direct3D support or one
of the many good 3D engines.

These functions must be called from the main thread.
See this bug for details: http://bugzilla.libsdl.org/show_bug.cgi?id=1995
-}
module Graphics.UI.SDL2.Render(
 Renderer,
 TextureAccess(..),
 RendererFlag(..),
 TextureModulate(..),
 withWindowAndRenderer,
 renderCopy,
 renderPresent,
 createTextureFromSurface,
 setRenderTarget,
 getRenderTarget,
 setRenderDrawColor,
 createTexture,
 renderDrawLine,
 renderDrawPoint
 ) where
import Foreign
import Foreign.C.Types

import Graphics.UI.SDL2.Common
{#import Graphics.UI.SDL2.Internal.Error #}
{#import Graphics.UI.SDL2.Video#}
{#import Graphics.UI.SDL2.Rect#}
{#import Graphics.UI.SDL2.Pixels#}
{#import Graphics.UI.SDL2.Internal.Window#}
{#import Graphics.UI.SDL2.Internal.Renderer#}
{#import Graphics.UI.SDL2.Internal.Texture#}
{#import Graphics.UI.SDL2.Internal.Surface#}
#include <SDL2/SDL_render.h>

{#context lib = "SDL2" prefix = "SDL"#}


-- | The access pattern allowed for a texture.
{#enum TextureAccess
   {
    TEXTUREACCESS_STATIC as TextureAccessStatic -- | Changes rarely, not lockable.
    ,TEXTUREACCESS_STREAMING as TextureAccessStreaming -- | Changes frequently, lockable.
    ,TEXTUREACCESS_TARGET as TextureAccessTarget -- | Texture can be used as a render target.
   }
deriving (Eq,Ord,Show)#}

-- | The texture channel modulation used in RenderCopy.
{#enum TextureModulate
   {
    TEXTUREMODULATE_NONE as TextureModulateNone    -- | No modulation
    ,TEXTUREMODULATE_COLOR as TextureModulateColor -- | srcC = srcC * color
    ,TEXTUREMODULATE_ALPHA as TextureModulateAlpha -- | srcA = srcA * alpha
   }
deriving (Eq,Ord,Show)#}

-- | Flip constants for RenderCopyEx
{#enum RendererFlip
    {
    FLIP_NONE as FlipNone -- |Do not flip
    ,FLIP_HORIZONTAL as FlipHorizontal -- | flip horizontally
    ,FLIP_VERTICAL as FlipVertical -- | flip vertically
    }
deriving (Eq,Ord,Show)#}

withWindowAndRenderer ::
  String -- ^ The title of the window.
  -> WinPos -- ^ The x position of the window.
  -> WinPos -- ^ The y position of the window.
  -> Int    -- ^ The width of the window.
  -> Int    -- ^ The height of the window.
  -> [WindowFlag]  -- ^ The flags for the window.
  -> Int    -- ^ The index of the rendering drice to initiate,
          --     -1 to initialize the first one supporting
          --     the requested flags.
  -> [RendererFlag]
  -> ((Window,Renderer) -> IO a) -- ^ The action to run with the window and the Renderer.
  -> IO a
withWindowAndRenderer str p1 p2 w h wfs rfs i f = do
  win <- createWindow str p1 p2 w h wfs
  rend <- createRenderer win rfs i
  a <- f (win,rend)
  withRendererPtr rend {#call DestroyRenderer as destroyRenderer'_#}
  withWindowPtr win {#call DestroyWindow as destroyWindow'_#}
  return a


-- | Create a window and default renderer
{#fun unsafe CreateWindowAndRenderer as createWindowAndRenderer
  {
   `Int'                             -- ^ The width of the window
   ,`Int'                            -- ^ The height of the window
   ,flagToC `[WindowFlag]'           -- ^ The flags used to create the window
   ,alloca- `Window' peekWindow*
   ,alloca- `Renderer' peekRenderer*
  } -> `() ' checkError*-#} 

-- | Destroy the rendering context for a window and free associated textures.
{#fun unsafe CreateTextureFromSurface as createTextureFromSurface
  {
   withRendererPtr* `Renderer'
   ,withSurfacePtr* `Surface'
  } -> `Texture' mkTexture*#}




{- | Copy a portion of the texture to the current rendering target.
-}
{#fun RenderCopy as renderCopy
  {
    withRendererPtr* `Renderer' -- ^ The renderer which should copy parts of a texture.
    ,withTexturePtr* `Texture'  -- ^ The source texture.
    ,withMayPtr*  `Maybe Rect'  -- ^ The source Rectangle,
                                -- if Nothing the entire Texture is used.
    ,withMayPtr*  `Maybe Rect'  -- ^ The destination rectangle,
                                -- if Nothing the entire target is used.
  } -> `() ' checkError*-#}

{#fun RenderPresent as renderPresent
  {
    withRendererPtr* `Renderer'
  } -> `()'#}

{- | Draw a line on the current rendering target.
-}
{#fun RenderDrawLine as renderDrawLine
  {
   withRendererPtr* `Renderer' -- ^ The renderer which should draw a line.
   ,`Int'                      -- ^The x coordinate of the start point.
   ,`Int'                      -- ^The y coordinate of the start point.
   ,`Int'                      -- ^The x coordinate of the end point.
   ,`Int'                      -- ^The y coordinate of the end point.
  } -> `() ' checkError*-#}

{- | 
Set the color used for drawing operations (Rect, Line and Clear).
-}
{#fun SetRenderDrawColor as setRenderDrawColor
  {
   withRendererPtr* `Renderer'
   ,`Word8'  -- ^  The red value used to draw on the rendering target.
   ,`Word8'  -- ^  The green value used to draw on the rendering target.
   ,`Word8'  -- ^  The blue value used to draw on the rendering target.
   ,`Word8'  -- ^  The alpha value used to draw on the rendering target, usually 255.
  } -> `() ' checkError*-#}


{- | Create a texture for a rendering context.
-}
{#fun CreateTexture as createTexture
  {
   withRendererPtr* `Renderer' -- ^ The renderer.
   ,enumToC `PixelFormatType'  -- ^ The format of the texture.
   ,enumToC `TextureAccess'    -- ^ One of the enumerated values in ::SDL
   ,`Int'                      -- ^ The width of the texture in pixels.
   ,`Int'                      -- ^ The height of the texture in pixels.
   } -> `Texture' mkTexture* #}

{- |
 Draw a point on the current rendering target.
-}
renderDrawPoint ::
  Renderer  -- ^  The renderer which should draw a point.
  -> Point  -- ^  The point to draw on.
  -> IO ()
renderDrawPoint r (Point x y) =
 c_renderDrawPoint r x y


{#fun RenderDrawPoint as c_renderDrawPoint
  {
   withRendererPtr* `Renderer' -- ^  The renderer which should draw a point.
   ,`Int32' -- ^ The x coordinate point to draw on.
   ,`Int32' -- ^ The y coordinate point to draw on.
  } -> `()' checkError*- #}
