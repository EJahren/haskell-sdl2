{-# LANGUAGE CPP, ForeignFunctionInterface#-}
module Graphics.UI.SDL2.Touch where
import Foreign

#include <SDL2/SDL_touch.h>

{#context lib = "SDL2"#}

type TouchID = Int64
type FingerID = Int64
