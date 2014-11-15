module Interactive.DrawPoints where
import Graphics.UI.SDL2

import Control.Monad

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

drawPoints =
 withSdl [InitVideo] $
 mkWindowAndRenderer $ \(window,renderer) -> do
 setRenderDrawColor renderer 125 125 0 255
 draw renderer False
 return True

draw r pr = do
  me <- pollEvent
  case me of
    Nothing -> draw r pr
    (Just e) -> handleEvent e
  where
    handleEvent Quit{} = return ()
    handleEvent KeyDown{keysym = Keysym{keycode = Key_Escape}} =
      return ()
    handleEvent MouseButtonDown{} = draw r True
    handleEvent MouseButtonUp{} = draw r False
    handleEvent MouseMotion{x = x1, y = y1} = do
       when pr $ do { renderDrawPoint r (Point x1 y1); renderPresent r}
       draw r pr
    handleEvent e =
      draw r pr
