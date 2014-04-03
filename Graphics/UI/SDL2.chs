{-# LANGUAGE CPP, BangPatterns, ForeignFunctionInterface #-}
module Graphics.UI.SDL2(
  module Graphics.UI.SDL2.Rect,
  module Graphics.UI.SDL2.Error,
  module Graphics.UI.SDL2.Events,
  module Graphics.UI.SDL2.Quit,
  module Graphics.UI.SDL2.RWops,
  module Graphics.UI.SDL2.Surface,
  module Graphics.UI.SDL2.Video,
  sdlInit,
  InitOption(..)
  )where
import Foreign
import Foreign.C
import Foreign.C.String
import Foreign.C.Types

import Graphics.UI.SDL2.Common
{# import Graphics.UI.SDL2.Rect #}
{# import Graphics.UI.SDL2.Error #}
{# import Graphics.UI.SDL2.Events #}
{# import Graphics.UI.SDL2.Quit #}
{# import Graphics.UI.SDL2.RWops #}
{# import Graphics.UI.SDL2.Surface #}
{# import Graphics.UI.SDL2.Video #}

#include <SDL2/SDL.h>
{#context lib = "SDL2"#}


{#enum define InitOption
  {
    SDL_INIT_TIMER          as InitTimer
  , SDL_INIT_AUDIO          as InitAudio
  , SDL_INIT_VIDEO          as InitVideo
  , SDL_INIT_JOYSTICK       as InitJoystick
  , SDL_INIT_HAPTIC         as InitHapic
  , SDL_INIT_GAMECONTROLLER as InitGamecontroller
  , SDL_INIT_EVENTS         as InitEvents
  , SDL_INIT_NOPARACHUTE    as InitNoparachute
  , SDL_INIT_EVERYTHING     as InitEverything
  } deriving (Eq,Ord,Show) #}

{#fun unsafe SDL_Init as sdlInit
  {flagToC `[InitOption]'} -> `()' checkError*-#}
