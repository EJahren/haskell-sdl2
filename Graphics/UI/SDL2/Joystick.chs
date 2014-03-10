{-# LANGUAGE 
  CPP,
  BangPatterns,
  ForeignFunctionInterface#-}
module Graphics.UI.SDL2.Joystick where
import Foreign
import Foreign.C
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Storable

#include <SDL2/SDL_joystick.h>

{#context lib = "SDL2"#}

type JoystickID = Word32

{#enum define HatPos
  { SDL_HAT_CENTERED  as HatCentered,
    SDL_HAT_UP        as HatUp,
    SDL_HAT_RIGHT     as HatRight,
    SDL_HAT_DOWN      as HatDown,
    SDL_HAT_LEFT      as HatLeft,
    SDL_HAT_RIGHTUP   as HatRightUp,
    SDL_HAT_RIGHTDOWN as HatRightDown,
    SDL_HAT_LEFTUP    as HatLeftUp,
    SDL_HAT_LEFTDOWN  as HatLeftDown} deriving (Eq,Ord,Show) #}
