{-# LANGUAGE CPP, BangPatterns, ForeignFunctionInterface #-}
module Graphics.UI.SDL2.RWops(
 RWops,
 rwFromFile) where
import Foreign
import Foreign.C
import Foreign.C.String
import Foreign.C.Types

import Control.Monad

#include <SDL2/SDL_rwops.h>
{#import Graphics.UI.SDL2.Foreign.RWops#}

{#fun SDL_RWFromFile as rwFromFile
  {`String', `String'} -> `RWops' mkRWops* #}
