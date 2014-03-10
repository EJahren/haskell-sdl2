module Graphics.UI.SDL2.Video where
{-# LANGUAGE CPP, BangPatterns, ForeignFunctionInterface #-}
import Foreign
import Foreign.C
import Foreign.C.String
import Foreign.C.Types

import Graphics.UI.SDL2.Common
{# import Graphics.UI.SDL2.Surface #}

#include <SDL2/SDL_video.h>
{#context lib = "sdl2" prefix = "SDL" #}

type CWindow = ()

data Window = Window {
  unWindow :: !(Ptr CWindow)
  } deriving (Eq, Ord, Show)


{#enum SDL_WindowFlags as WindowFlag
   {underscoreToCase} deriving (Show,Eq,Ord)#}

{#enum SDL_WindowEventID as WindowEventID
   {underscoreToCase} deriving (Show,Eq,Ord)#}

{#enum define WinPos
  {
   SDL_WINDOWPOS_CENTERED as WinPosCentered
  ,SDL_WINDOWPOS_UNDEFINED as WinPosUndefined
  } 
  deriving (Eq,Ord,Show)#}


{- createWindow str x y w h (WindowOption p) = do
  cstr <- newCString str
  cwin <- c_createWindow cstr (unWinpos x) (unWinpos y) w h p
  return (Window cwin) -}

foreign import ccall unsafe "SDL2/SDL_video.h SDL_GetWindowSurface"
  c_getWindowSurface :: Ptr CWindow -> IO (Ptr CSurface)

getWindowSurface (Window ptr) = fmap Surface (c_getWindowSurface ptr)

{#fun unsafe SDL_CreateWindow as createWindow
 {`String',enumMarshal `WinPos',enumMarshal `WinPos',
  `Int',`Int',enumMarshal `WindowFlag'} -> `Window' Window #} 

foreign import ccall unsafe "SDL2/SDL_video.h SDL_UpdateWindowSurface"
  c_updateWindowSurface :: Ptr CWindow -> IO CInt

updateWindowSurface (Window wp) =
  c_updateWindowSurface wp

foreign import ccall unsafe "SDL2/SDL_video.h SDL_DestroyWindow"
  c_destroyWindow :: Ptr CWindow -> IO ()

destroyWindow (Window wp) = c_destroyWindow wp
