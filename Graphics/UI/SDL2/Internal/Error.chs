{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Graphics.UI.SDL2.Internal.Error where
import Foreign
import Foreign.C.String
import Foreign.C.Types

#include <SDL2/SDL_error.h>

{#context lib = "SDL2"#}

checkNull :: Ptr a -> IO (Ptr a)
checkNull p 
  | p == nullPtr = do
    e <- getError
    ioError . userError $ e
  | otherwise = return p

peekWCheck :: (Storable b) => Ptr b -> IO b
peekWCheck p = checkNull p >> peek p

checkError :: CInt -> IO ()
checkError x
  | x == 0 = return ()
  | otherwise = do
    str <- getError
    ioError (userError str)

{#fun SDL_GetError as getError {} -> `String'#}
