module Interactive.DrawLine where
import Graphics.UI.SDL2

windowWidth  = 320
windowHeight = 240

drawLine (x1,y1) (x2,y2) = do
 sdlInit [InitVideo]
 (window,renderer) <- createWindowAndRenderer
   windowWidth
   windowHeight
   [WindowShown]
 setRenderDrawColor renderer 125 125 0 255
 renderDrawLine renderer x1 y1 x2 y2
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
