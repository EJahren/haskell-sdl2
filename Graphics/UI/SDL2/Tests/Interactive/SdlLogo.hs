module Graphics.UI.SDL2.Tests.Interactive.SdlLogo (showLogo) where
import Graphics.UI.SDL2

windowWidth  = 320
windowHeight = 240

windowTitle = "SDL2 Test"

showLogo = do
 
 b <- sdlInit InitVideo
 if b > 0
   then do
     err <- c_getError
     putStrLn ("SDL2 could not initialize! SDL2_error "
       ++ show err)
     return False
   else do
     window <- createWindow
       windowTitle
       WinPosCentered
       WinPosCentered
       windowWidth
       windowHeight
       windowShown
     screen <- getWindowSurface window
     image <- loadBmp "sdl_logo.bmp"
     drawImage screen image window
     b <- waitForResult
     freeSurface image
     destroyWindow window
     c_quit
     return b

waitForResult = do
  mevent <- pollEvent
  case mevent of 
    Nothing -> waitForResult
    (Just e) -> handleEvent e
  where
    handleEvent e
      | eventType e == keyDown =
        return True 
      | otherwise = waitForResult

drawImage screen image window = do
  blit image noRect screen noRect
  updateWindowSurface window
