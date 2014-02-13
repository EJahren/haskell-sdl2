module Graphics.UI.SDL2.Error where
{-# LANGUAGE CPP, BangPatterns, ForeignFunctionInterface #-}
import Foreign
import Foreign.C.String
import Foreign.C.Types

#include <SDL2/SDL_error.h>

{- extern DECLSPEC const char *SDLCALL SDL_GetError(void); -}
foreign import ccall unsafe "SDL2/SDL_error.h SDL_GetError"
  c_getError :: IO CString 
