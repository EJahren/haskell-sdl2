module Automatic.TextureTest where
import Graphics.UI.SDL2
import System.Mem(performGC)

textureTest = do
  sdlInit [InitEverything]
  (window,renderer) <- createWindowAndRenderer
    640
    480
    [WindowShown]
  t <- createTexture
    renderer
    PixelFormatARGB8888
    TextureAccessTarget
    640
    480
  renderDrawLine renderer 0 0 100 100
  setRenderTarget renderer t
  renderDrawLine renderer 0 0 100 100
  performGC
  renderDrawLine renderer 0 0 100 100
  sdlQuit
