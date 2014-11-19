{-# LANGUAGE CPP, BangPatterns, ForeignFunctionInterface #-}
module Graphics.UI.SDL2.Video(
  withWindow,
  WindowFlag(..),
  WindowEventID(..),
  WinPos(..),
  getWindowSurface,
  updateWindowSurface,
) where
import Foreign
import Foreign.C
import Foreign.C.Types


{#import Graphics.UI.SDL2.Surface #}
{#import Graphics.UI.SDL2.Internal.Error #}
{#import Graphics.UI.SDL2.Internal.Window#}
{#import Graphics.UI.SDL2.Internal.Surface#}

#include <SDL2/SDL_video.h>
{#context lib = "SDL2" prefix = "SDL" #}


{#enum SDL_WindowEventID as WindowEventID
   {underscoreToCase} deriving (Show,Eq,Ord)#}

{#fun SDL_GetWindowSurface as getWindowSurface
  {withWindowPtr* `Window'} -> `Surface' mkUnhandledSurface*#}

{- | Run an IO action on a window -}
withWindow ::
  String -- ^ The title of the window.
  -> WinPos -- ^ The x position of the window.
  -> WinPos -- ^ The y position of the window.
  -> Int32    -- ^ The width of the window.
  -> Int32    -- ^ The height of the window.
  -> [WindowFlag] -- ^ The flags for the window.
  -> (Window -> IO a) -- ^ The action to run with the window.
  -> IO a

withWindow s p1 p2 w h fs f = do
  win <- createWindow s p1 p2 w h fs
  a <- f win
  withWindowPtr win {#call SDL_DestroyWindow as destroyWindow'_#}
  return a
  
{- | Copy the window surface to the screen. -}
{#fun unsafe SDL_UpdateWindowSurface as updateWindowSurface
  {
    withWindowPtr* `Window'
  } -> `() ' checkError*-#}
