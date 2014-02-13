module Graphics.UI.SDL2.Surface where
{-# LANGUAGE CPP, BangPatterns, ForeignFunctionInterface #-}
import Foreign
import Foreign.C.String
import Foreign.C.Types

#include <SDL2/SDL_surface.h>

type CSurface = ()

data Surface = Surface !(Ptr CSurface)
        deriving (Eq, Ord, Show)

{- extern DECLSPEC SDL_Surface *SDLCALL SDL_LoadBMP_RW(SDL_RWops * src,
                                                    int freesrc); -}
foreign import ccall unsafe "SDL2/SDL_surface.h SDL_LoadBMP_RW"
  c_loadBmpRw :: RWops -> CInt -> Ptr Surface

{-
extern DECLSPEC int SDLCALL SDL_UpperBlit
    (SDL_Surface * src, const SDL_Rect * srcrect,
     SDL_Surface * dst, SDL_Rect * dstrect); -}
