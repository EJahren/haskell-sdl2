{-# LANGUAGE CPP, ForeignFunctionInterface#-}
module Graphics.UI.SDL2.Gesture where
import Foreign

#include <SDL2/SDL_gesture.h>

{#context lib = "SDL2"#}

type GestureID = Int64
