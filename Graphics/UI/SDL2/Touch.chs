{-# LANGUAGE 
  CPP,
  BangPatterns,
  ForeignFunctionInterface#-}
module Graphics.UI.SDL2.Touch where
import Foreign
import Foreign.C
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Storable

#include <SDL2/SDL_touch.h>

{#context lib = "SDL2"#}

type TouchID = Int64
type FingerID = Int64
