{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Graphics.UI.SDL2.Scancode where

#include <SDL2/SDL_scancode.h>

{#context lib = "SDL2" prefix="SDL"#}

{#enum SDL_Scancode as Scancode {underscoreToCase} deriving (Eq,Ord,Show) #}
