import Graphics.UI.SDL2

windowWidth  = 320
windowHeight = 240

windowTitle = "SDL2 Test"

main = do
 b <- sdlInit InitVideo
 if b > 0
   then do
     err <- c_getError
     putStrLn ("SDL2 could not initialize! SDL2_error "
       ++ show err)
   else do
     window <- createWindow
       windowTitle
       WinPosCentered
       WinPosCentered
       windowWidth
       windowHeight
       WindowShown
     screen <- getWindowSurface window
     image <- loadBmp "sdl_logo.bmp"
     drawImage screen image window
     waitForExit
     freeSurface image
     destroyWindow window
     c_quit

waitForExit = do
  mevent <- c_pollEvent
  handleEvent $ snd mevent
  --case mevent of 
    --Nothing -> waitForExit
    --(Just e) -> handleEvent e
  where
    --handleEvent (Quit _) = return ()
    handleEvent (MouseButtonDown _ _ _) = return ()
    handleEvent _ = waitForExit

drawImage screen image window = do
  blit image noRect screen noRect
  updateWindowSurface window
