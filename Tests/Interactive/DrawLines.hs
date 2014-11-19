module Interactive.DrawLines where
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

drawLines =
 withSdl [InitVideo] $
 mkWindowAndRenderer $ \(window,renderer) -> do
 setRenderDrawColor renderer 125 125 0 255
 draw renderer False Nothing
 return True

draw r pr mp = do
  me <- pollEvent
  case me of
    Nothing -> draw r pr mp
    (Just e) -> handleEvent e
  where
    handleEvent Quit{} = return ()
    handleEvent KeyDown{keysym = Keysym{keycode = Key_Escape}} =
      return ()
    handleEvent MouseButtonDown{} = draw r True mp
    handleEvent MouseButtonUp{} = draw r False Nothing
    handleEvent MouseMotion{x = x1, y = y1} = do
       when pr $ case mp of
          Nothing -> do { renderDrawPoint r (Point x1 y1); renderPresent r}
          (Just (Point x2 y2)) -> do {renderDrawLine r x1 y1 x2 y2; renderPresent r}
       draw r pr (Just (Point x1 y1))
    handleEvent e =
      draw r pr mp
