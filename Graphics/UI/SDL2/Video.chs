module Graphics.UI.SDL2.Video where
{-# LANGUAGE CPP, BangPatterns, ForeignFunctionInterface #-}
import Foreign
import Foreign.C
import Foreign.C.String
import Foreign.C.Types

import Control.Monad

import Graphics.UI.SDL2.Common
{#import Graphics.UI.SDL2.Surface #}
{#import Graphics.UI.SDL2.Error #}
{#import Graphics.UI.SDL2.Foreign.Window#}
{#import Graphics.UI.SDL2.Foreign.Surface#}

#include <SDL2/SDL_video.h>
{#context lib = "sdl2" prefix = "SDL" #}

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


{#fun SDL_GetWindowSurface as getWindowSurface
  {withWindow* `Window'} -> `Surface' mkSurface*#}

{#fun unsafe SDL_CreateWindow as createWindow
 {`String',enumToC `WinPos',enumToC `WinPos',
  `Int',`Int',flagToC `[WindowFlag]'} ->
   `Window' mkWindow* #} 

{#fun unsafe SDL_UpdateWindowSurface as updateWindowSurface
  {withWindow* `Window'} -> `()' checkError*-#}
