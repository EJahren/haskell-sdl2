module Interactive.SdlLogo where
import Graphics.UI.SDL2

windowWidth  = 320
windowHeight = 240

windowTitle = "SDL2 Test"

mkWindow =
  withWindow
     windowTitle
     WinPosCentered
     WinPosCentered
     windowWidth
     windowHeight
     [WindowShown]

showLogo logo =
 withSdl [InitVideo] $
 mkWindow $ \window -> do
 image <- loadBmp logo
 drawImage image window
 waitForExit
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
