module Graphics.UI.SDL2.Surface where
{-# LANGUAGE CPP, BangPatterns, ForeignFunctionInterface #-}
import Foreign
import Foreign.C
import Foreign.C.String
import Foreign.C.Types
{# import Graphics.UI.SDL2.RWops #}
{# import Graphics.UI.SDL2.Rect #}


#include <SDL2/SDL_surface.h>

type CSurface = ()

data Surface = Surface !(Ptr CSurface)
        deriving (Eq, Ord, Show)

foreign import ccall unsafe "SDL2/SDL_surface.h SDL_LoadBMP_RW"
  c_loadBmpRw :: Ptr CRWops -> CInt -> IO (Ptr CSurface)

loadBmp file = do
  cfile <- newCString file 
  rb <- newCString "rb"
  rw <- c_RWFromFile cfile rb
  fmap Surface $ c_loadBmpRw rw 1

foreign import ccall unsafe "SDL2/SDL_surface.h SDL_UpperBlit"
  c_upperBlit :: 
    Ptr CSurface -> Ptr CRect -> 
    Ptr CSurface -> Ptr CRect ->
    IO CInt

blit (Surface s1) (Rect r1) (Surface s2) (Rect r2) =
  c_upperBlit s1 r1 s2 r2

foreign import ccall unsafe "SDL2/SDL_surface.h SDL_FreeSurface"
  c_FreeSurface :: Ptr CSurface -> IO ()

freeSurface (Surface ps) = c_FreeSurface ps
