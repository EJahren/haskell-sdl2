module Graphics.UI.SDL2 where
{-# LANGUAGE CPP, BangPatterns, ForeignFunctionInterface #-}
import Foreign
import Foreign.C.String
import Foreign.C.Types

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

init (InitOption x) = c_init x
