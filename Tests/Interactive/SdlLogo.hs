module Interactive.SdlLogo where
import Graphics.UI.SDL2

windowWidth  = 320
windowHeight = 240

windowTitle = "SDL2 Test"

showLogo logo = do
 sdlInit [InitVideo]
 window <- createWindow
   windowTitle
   WinPosCentered
   WinPosCentered
   windowWidth
   windowHeight
   [WindowShown]
 screen <- getWindowSurface window
 image <- loadBmp logo
 drawImage screen image window
 waitForExit
 freeSurface image
 destroyWindow window
 c_quit
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

drawImage screen image window = do
  blit image noRect screen noRect
  updateWindowSurface window
