
{-# LANGUAGE 
  CPP,
  BangPatterns,
  ForeignFunctionInterface#-}
module Graphics.UI.SDL2.Gesture where
import Foreign
import Foreign.C
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Storable

#include <SDL2/SDL_gesture.h>

{#context lib = "SDL2"#}

type GestureID = Int64
