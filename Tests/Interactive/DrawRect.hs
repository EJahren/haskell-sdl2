module Interactive.DrawRect where
import Graphics.UI.SDL2

windowWidth  = 320
windowHeight = 240

mkWindowAndRenderer =
  withWindowAndRenderer
    "Test: drawLine"
    WinPosCentered
    WinPosCentered
    windowWidth
    windowHeight
    [WindowShown]
    (-1)
    [RendererAccelerated]

drawRect x y w h =
 withSdl [InitVideo] $
 mkWindowAndRenderer $ \(window,renderer) -> do
 setRenderDrawColor renderer 125 125 0 255
 renderDrawRect renderer (Rect x y w h)
 renderPresent renderer
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
