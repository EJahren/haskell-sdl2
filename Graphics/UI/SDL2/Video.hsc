module Graphics.UI.SDL2.Video where
{-# LANGUAGE CPP, BangPatterns, ForeignFunctionInterface #-}

import Graphics.UI.SDL2.Surface

import Foreign
import Foreign.C.String
import Foreign.C.Types

#include <SDL2/SDL_video.h>

type CWindow = ()

data Window = Window !(Ptr CWindow)
        deriving (Eq, Ord, Show)

data WindowOption = WindowOption Word32

#{enum WindowOption, WindowOption
  , windowFullscreen     = SDL_WINDOW_FULLSCREEN  
  , windowOpenGl         = SDL_WINDOW_OPENGL
  , windowShown          = SDL_WINDOW_SHOWN
  , windowHidden         = SDL_WINDOW_HIDDEN     
  , windowBorderless     = SDL_WINDOW_BORDERLESS
  , windowResizable      = SDL_WINDOW_RESIZABLE     
  , windowMaximized      = SDL_WINDOW_MAXIMIZED
  , windowMinimized      = SDL_WINDOW_MINIMIZED  
  , windowInputGrabbed   = SDL_WINDOW_INPUT_GRABBED
  }

data WinPos = WinPos CInt | WinposCentered | WinposUndefined

unWinpos (WinPos x) = x
unWinpos WinposCentered = #const SDL_WINDOWPOS_CENTERED_MASK
unWinpos WinposUndefined = #const SDL_WINDOWPOS_UNDEFINED_MASK

createWindow str x y w h (WindowOption p) = do
  cstr <- newCString str
  cwin <- c_createWindow cstr (unWinpos x) (unWinpos y) w h p
  return (Window cwin)


foreign import ccall unsafe "SDL2/SDL_video.h SDL_GetWindowSurface"
  c_getWindowSurface :: Ptr CWindow -> IO (Ptr CSurface) 

getWindowSurface (Window ptr) = fmap Surface (c_getWindowSurface ptr)

foreign import ccall unsafe "SDL2/SDL_video.h SDL_CreateWindow"
    c_createWindow  :: CString
                    -> CInt  
                    -> CInt  
                    -> CInt  
                    -> CInt  
                    -> Word32
                    -> IO (Ptr CWindow)
foreign import ccall unsafe "SDL2/SDL_video.h SDL_UpdateWindowSurface"
  c_updateWindowSurface :: Ptr CWindow -> IO CInt

updateWindowSurface (Window wp) =
  c_updateWindowSurface wp

foreign import ccall unsafe "SDL2/SDL_video.h SDL_DestroyWindow"
  c_destroyWindow :: Ptr CWindow -> IO ()

destroyWindow (Window wp) = c_destroyWindow wp

