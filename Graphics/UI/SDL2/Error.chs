module Graphics.UI.SDL2.Error where
{-# LANGUAGE CPP, BangPatterns, ForeignFunctionInterface #-}
import Foreign
import Foreign.C.String
import Foreign.C.Types

#include <SDL2/SDL_error.h>

checkError x
  | x == 0 = return ()
  | otherwise = do
    cstr <- c_getError
    str <- peekCString cstr
    ioError (userError str)
    
foreign import ccall unsafe "SDL2/SDL_error.h SDL_GetError"
  c_getError :: IO CString 
