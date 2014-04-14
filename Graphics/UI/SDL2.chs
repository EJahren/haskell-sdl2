{-# LANGUAGE CPP, BangPatterns, ForeignFunctionInterface #-}
module Graphics.UI.SDL2(
  module Graphics.UI.SDL2.Rect,
  module Graphics.UI.SDL2.Events,
  module Graphics.UI.SDL2.RWops,
  module Graphics.UI.SDL2.Surface,
  module Graphics.UI.SDL2.Video,
  module Graphics.UI.SDL2.Render,
  module Graphics.UI.SDL2.Pixels,
  withSdl,
  InitOption(..)
  )where
import Foreign.C

import System.Mem(performGC)

import Graphics.UI.SDL2.Common
{# import Graphics.UI.SDL2.Rect #}
{# import Graphics.UI.SDL2.Events #}
{# import Graphics.UI.SDL2.RWops #}
{# import Graphics.UI.SDL2.Surface #}
{# import Graphics.UI.SDL2.Video #}
{# import Graphics.UI.SDL2.Render #}
{# import Graphics.UI.SDL2.Pixels #}
{# import Graphics.UI.SDL2.Internal.Error #}

#include <SDL2/SDL.h>
{#context lib = "SDL2"#}

{- |
These are the flags which may be passed to withSdl. You should
specify the subsystems which you will be using in your application.
-}
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

{- | Run an IO action containing sdl calls. Note that
  any call to SDL functions must be enclosed in one withSdl
  call -}
{- In the future we might want to make the type of this function
  ::   [InitOption] -> SDL a -> IO a, as to make the dependence 
  explicit in the types, but that yields relatively low gain for quite
  a lot of effort. -}
withSdl :: [InitOption] -> IO a -> IO a
withSdl ops f = do
  sdlInit ops
  a <- f
  performGC 
  {#call SDL_Quit as sdlQuit'_#}
  return a

{#fun unsafe SDL_Init as sdlInit
  {flagToC `[InitOption]'} -> `() ' checkError*-#}
