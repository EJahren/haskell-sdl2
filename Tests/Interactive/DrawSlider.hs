module Interactive.DrawSlider where
import Graphics.UI.SDL2

import Control.Monad

windowWidth  = 320
windowHeight = 240

data Slider = Slider{
  button :: Rect,
  bar :: Rect
  }

initSlider = Slider (Rect 20 75 40 80) (Rect 20 100 200 30)

data AppState = AppState{
  renderer :: Renderer,
  slider :: Slider,
  btnPressed :: Bool}

mkAppState r = AppState r initSlider False

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

updatePos x s = do
  let r = renderer s
  setRenderDrawColor r 0 0 0 255
  let btn = button (slider s)
  renderFillRect r btn
  setRenderDrawColor r 125 125 0 255
  let br = bar (slider s)
  let barSegment = br{xCoord = xCoord btn,width = width btn}
  let centerx = x - ((width (button (slider s))) `div` 2)
  let newbtn = btn{xCoord = centerx}
  renderFillRect r barSegment
  renderDrawRect r newbtn
  renderPresent r
  return s{slider = (slider s){button = newbtn}}
  
insideSlider x s =
  min (max xMin x) xMax
  where
    buttonRest = width (button (slider s)) `div` 2
    br = bar (slider s) 
    xMin = xCoord br + buttonRest
    xMax = xCoord br + width br - buttonRest

drawSlider =
 withSdl [InitVideo] $
 mkWindowAndRenderer $ \(window,renderer) -> do
 setRenderDrawColor renderer 125 125 0 255
 renderDrawRect renderer (button initSlider)
 renderFillRect renderer (bar initSlider)
 renderPresent renderer
 draw (mkAppState renderer)
 return True

draw s = do
  me <- pollEvent
  case me of
    Nothing -> draw s
    (Just e) -> handleEvent e
  where
    handleEvent Quit{} = return ()
    handleEvent KeyDown{keysym = Keysym{keycode = Key_Escape}} =
      return ()
    handleEvent MouseButtonDown{x = x1, y = y1} =
      draw s{btnPressed = True}
    handleEvent MouseButtonUp{} = draw s{btnPressed = False}
    handleEvent MouseMotion{x = x1, y = y1} = do
       let xNorm = insideSlider x1 s
       s1 <- if btnPressed s 
             then updatePos xNorm s
             else return s
       draw s1
    handleEvent e =
      draw s
