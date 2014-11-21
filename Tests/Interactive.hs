module Main where
{- | This test module runs all
 -  the interactive tests in the project.
 -  build with
  > cabal configure --enable-tests
  > cabal build
  and then run
  > cabal test interactive
  from the root directory of the project.
-}
import System.Exit (exitFailure)

import Interactive.SdlLogo
import qualified Interactive.SdlLogoRenderer as R
import Interactive.DrawLine
import Interactive.DrawRect
import Interactive.DrawPoints
import Interactive.DrawLines
import Interactive.DrawSlider

main = do 
  showLogo "Tests/Interactive/sdl_logo.bmp"
  R.showLogo "Tests/Interactive/sdl_logo.bmp"
  drawLine (0,0) (100,100)
  drawPoints
  drawLines
  drawRect 5 5 100 100
  drawSlider
