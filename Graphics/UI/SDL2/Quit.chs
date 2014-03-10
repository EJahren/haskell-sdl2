module Graphics.UI.SDL2.Quit where
{-# LANGUAGE CPP, BangPatterns, ForeignFunctionInterface #-}
import Foreign
import Foreign.C.String
import Foreign.C.Types

#include <SDL2/SDL_quit.h>


foreign import ccall unsafe "SDL2/SDL_quit.h SDL_Quit"
    c_quit :: IO ()
