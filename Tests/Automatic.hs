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
import Control.Monad

import Automatic.TextureTest
import Automatic.RectTest

main = do
  b <- fmap and $ sequence 
    [
     textureTest
     ,rectTest
    ]
  unless b exitFailure

