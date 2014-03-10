{-# LANGUAGE CPP, BangPatterns, ForeignFunctionInterface #-}
module Graphics.UI.SDL2.RWops where
import Foreign
import Foreign.C
import Foreign.C.String
import Foreign.C.Types

#include <SDL2/SDL_rwops.h>

type CRWops = ()

data RWops = RWops (Ptr CRWops)

{- extern DECLSPEC SDL_RWops *SDLCALL SDL_RWFromFile(const char *file,
                                                  const char *mode); -}
foreign import ccall unsafe "SDL2/SDL_rwops.h SDL_RWFromFile"
  c_RWFromFile :: CString -> CString -> IO (Ptr CRWops)
