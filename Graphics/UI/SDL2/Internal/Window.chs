{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Graphics.UI.SDL2.Internal.Window
  (Window,
  mkWindow,
  withWindowPtr,
  createWindow,
  WinPos(..),
  WindowFlag(..),
  peekWindow) where
import Foreign
import Foreign.C

import Graphics.UI.SDL2.Common
{#import Graphics.UI.SDL2.Internal.Error#}

#include <SDL2/SDL_video.h>
{#context lib = "sdl2" prefix = "SDL"#}

{#pointer *Window as Window#}

mkWindow :: Ptr () -> IO Window
mkWindow p = checkNull p

peekWindow :: Ptr (Ptr ()) -> IO Window
peekWindow p = mkWindow =<< peekWCheck p

withWindowPtr :: Window -> (Ptr () -> IO b) -> IO b
withWindowPtr p f =  f p

{#enum WindowFlags as WindowFlag
   {underscoreToCase} deriving (Show,Eq,Ord)#}

{#enum define WinPos
  {
   SDL_WINDOWPOS_CENTERED as WinPosCentered
  ,SDL_WINDOWPOS_UNDEFINED as WinPosUndefined
  } 
  deriving (Eq,Ord,Show)#}

{#fun unsafe CreateWindow as createWindow
 {
   `String'
   ,enumToC `WinPos'
   ,enumToC `WinPos'
   ,`Int32'
   ,`Int32'
   ,flagToC `[WindowFlag]'
 } -> `Window' mkWindow* #} 
