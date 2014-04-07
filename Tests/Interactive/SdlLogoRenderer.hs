module Interactive.SdlLogoRenderer where
import Graphics.UI.SDL2

windowWidth  = 320
windowHeight = 240

showLogo logo = do
 sdlInit [InitVideo]
 (window,renderer) <- createWindowAndRenderer
   windowWidth
   windowHeight
   [WindowShown]
 image <- loadBmp logo
 texture <- createTextureFromSurface renderer image
 renderCopy renderer texture Nothing Nothing
 renderPresent renderer
 waitForExit
 sdlQuit
 return True

waitForExit = do
  me <- pollEvent
  case me of
    Nothing -> waitForExit
    (Just e) -> handleEvent e
  where
    handleEvent Quit{} = return ()
    handleEvent MouseButtonDown{} = return ()
    handleEvent _ = waitForExit
