module Automatic.TextureTest where
import Graphics.UI.SDL2
import System.Mem(performGC)

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

textureTest =
  withSdl [InitEverything] $
  mkWindowAndRenderer $ \(window,renderer) -> do
  t <- createTexture
    renderer
    PixelFormatARGB8888
    TextureAccessTarget
    320
    240
  renderDrawLine renderer 0 0 100 100
  setRenderTarget renderer t
  renderDrawLine renderer 0 0 100 100
  performGC
  renderDrawLine renderer 0 0 100 100
