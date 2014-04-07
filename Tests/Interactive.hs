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

main = do 
  showLogo "Tests/Interactive/sdl_logo.bmp"
  R.showLogo "Tests/Interactive/sdl_logo.bmp"

