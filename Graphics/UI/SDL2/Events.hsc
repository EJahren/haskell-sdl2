module Graphics.UI.SDL2.Events where
{-# LANGUAGE CPP, BangPatterns, ForeignFunctionInterface #-}
import Foreign
import Foreign.C.String
import Foreign.C.Types

#include <SDL2/SDL_events.h>

type CEvent = ()

data Event = Event !(ForeignPtr CEvent)
        deriving (Eq, Ord, Show)
