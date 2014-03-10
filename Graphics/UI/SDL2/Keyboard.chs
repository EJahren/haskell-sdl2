module Graphics.UI.SDL2.Keyboard where
{-# LANGUAGE CPP, BangPatterns, ForeignFunctionInterface #-}
import Foreign
import Foreign.C
{# import Graphics.UI.SDL2.Keycode #}

#include <SDL2/SDL_keyboard.h>

{#context lib = "SDL2"#}

data Keysym = Keysym {
  scancode :: Scancode,
  keycode :: Keycode,
  mod     :: Modifier
 } deriving (Eq,Ord,Show)
