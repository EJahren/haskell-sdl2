import Graphics.UI.SDL2

windowWidth  = 320
windowHeight = 240

windowTitle = "SDL2 Test"

main = do
 b <- sdlInit initVideo
 if b > 0
   then do
     err <- c_getError
     putStrLn ("SDL2 could not initialize! SDL2_error "
       ++ show err)
   else do
     window <- createWindow
       windowTitle
       WinposCentered
       WinposCentered
       windowWidth
       windowHeight
       windowShown
     screen <- getWindowSurface window
     image <- loadBmp "sdl_logo.bmp"
     mainLoop screen image window
     freeSurface image
     destroyWindow window
     c_quit

mainLoop screen image window = go
  where 
  go = do
    blit image noRect screen noRect
    updateWindowSurface window
    checkEvent
  checkEvent = do
      mevent <- pollEvent
      case mevent of 
        Nothing -> checkEvent
        (Just e) -> handleEvent e
  handleEvent e
    | e == quit = return ()
    | otherwise = go
