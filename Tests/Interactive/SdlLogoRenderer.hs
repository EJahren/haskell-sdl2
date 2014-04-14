module Interactive.SdlLogoRenderer where
import Graphics.UI.SDL2

windowWidth  = 320
windowHeight = 240

mkWindowAndRenderer =
  withWindowAndRenderer
    "Test: SdlLogoRenderer"
    WinPosCentered
    WinPosCentered
    windowWidth
    windowHeight
    [WindowShown]
    (-1)
    [RendererAccelerated]

showLogo logo =
 withSdl [InitVideo] $
 mkWindowAndRenderer $ \(window,renderer) -> do
 image <- loadBmp logo
 texture <- createTextureFromSurface renderer image
 renderCopy renderer texture Nothing Nothing
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
