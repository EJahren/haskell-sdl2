{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Graphics.UI.SDL2.Internal.Window
  (Window,
  mkWindow,
  withWindow,
  peekWindow) where
import Foreign

{#import Graphics.UI.SDL2.Internal.Error#}

#include <SDL2/SDL_video.h>
{#context lib = "sdl2"#}

{#pointer *SDL_Window as Window#}

mkWindow :: Ptr () -> IO Window
mkWindow p = checkNull p

peekWindow :: Ptr (Ptr ()) -> IO Window
peekWindow p = mkWindow =<< peekWCheck p

withWindow :: Window -> (Ptr () -> IO b) -> IO b
withWindow p f =  f p
