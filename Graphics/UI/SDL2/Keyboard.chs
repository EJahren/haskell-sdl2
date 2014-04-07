{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Graphics.UI.SDL2.Keyboard where

{#import Graphics.UI.SDL2.Keycode #}

#include <SDL2/SDL_keyboard.h>
{#context lib = "SDL2"#}

data Keysym = Keysym {
  scancode :: Scancode,
  keycode :: Keycode,
  mod     :: Modifier
 } deriving (Eq,Ord,Show)
