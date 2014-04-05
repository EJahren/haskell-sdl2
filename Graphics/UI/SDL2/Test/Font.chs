{- | This module is part of the SDL2 test framework,
 and defines the function drawString to draw a debug
 string to a renderer. -}
module Graphics.UI.SDL2.Test.Font(drawString) where
{-# LANGUAGE 
  CPP,
  BangPatterns,
  ForeignFunctionInterface #-}
import Foreign
import Foreign.C
import Foreign.C.String
import Foreign.C.Types

{# import Graphics.UI.SDL2.Render #}
{# import Graphics.UI.SDL2.Error #}

#include <SDL2/SDL.h>
#include <SDL2/SDL_test_font.h>


-- | Draw a string in the currently set font.
--
-- >drawString :: 
-- >  Renderer -- ^ The Renderer to draw to.
-- >  -> Int   -- ^ The X coordinate of the upper left corner of the String.
-- >  -> Int   -- ^ The Y coordinate of the upper left corner of the String.
-- >  -> String -- ^ The string to draw.
-- >  -> IO ()
-- 
{#fun unsafe SDLTest_DrawString as drawString
 {withRenderer* `Renderer', `Int', `Int', `String'} 
  -> `()' checkError*-#} 
