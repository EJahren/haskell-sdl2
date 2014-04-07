module Interactive.SdlLogo where
import Graphics.UI.SDL2

windowWidth  = 320
windowHeight = 240

windowTitle = "SDL2 Test"

mkWindow =
  createWindow
     windowTitle
     WinPosCentered
     WinPosCentered
     windowWidth
     windowHeight
     [WindowShown]

showLogo logo = do
 sdlInit [InitVideo]
 window <- mkWindow
 image <- loadBmp logo
 drawImage image window
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

drawImage image window = do
  screen <- getWindowSurface window
  upperBlit image Nothing screen Nothing
  updateWindowSurface window
