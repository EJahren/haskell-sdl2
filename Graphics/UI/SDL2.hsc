module Graphics.UI.SDL2(
  module Graphics.UI.SDL2.Rect,
  module Graphics.UI.SDL2.Error,
  module Graphics.UI.SDL2.Events,
  module Graphics.UI.SDL2.Quit,
  module Graphics.UI.SDL2.RWops,
  module Graphics.UI.SDL2.Surface,
  module Graphics.UI.SDL2.Video,
  sdlInit,
  InitOption,
  initTimer,
  initAudio,
  initVideo,
  initJoystick,
  initHapic,
  initGamecontroller,
  initEvents,
  initNoparachute
  )where
{-# LANGUAGE CPP, BangPatterns, ForeignFunctionInterface #-}
import Foreign
import Foreign.C.String
import Foreign.C.Types

import Graphics.UI.SDL2.Rect
import Graphics.UI.SDL2.Error
import Graphics.UI.SDL2.Events
import Graphics.UI.SDL2.Quit
import Graphics.UI.SDL2.RWops
import Graphics.UI.SDL2.Surface
import Graphics.UI.SDL2.Video

#include <SDL2/SDL.h>

data InitOption = InitOption Word32

#{enum InitOption, InitOption
  , initTimer            = SDL_INIT_TIMER         
  , initAudio            = SDL_INIT_AUDIO         
  , initVideo            = SDL_INIT_VIDEO         
  , initJoystick         = SDL_INIT_JOYSTICK      
  , initHapic            = SDL_INIT_HAPTIC        
  , initGamecontroller   = SDL_INIT_GAMECONTROLLER
  , initEvents           = SDL_INIT_EVENTS         
  , initNoparachute      = SDL_INIT_NOPARACHUTE    
  }

foreign import ccall unsafe "SDL2/SDL.h SDL_Init"
    c_init  :: Word32
               -> IO CInt

sdlInit (InitOption x) = c_init x
