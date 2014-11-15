module Automatic.RectTest where
import Graphics.UI.SDL2.Rect
import Test.QuickCheck
import Control.Monad


symmetricIntersect r1 = (not (rectEmpty r1)) ==> (snd (intersectRect r1 r1)) == r1
specFstIntersect r1 r2 =  (fst (intersectRect r1 r2)) == hasIntersection r1 r2

symmetricUnion r1 = (not (rectEmpty r1)) ==> unionRect r1 r1 == r1

rectEncloseCornerns r@(Rect x y w h) =
  (not (rectEmpty r)) ==>
  fst (enclosedPoints 
    [Point x y,
     Point (x + w) (y + h)] r)

rectIntersectDiagonal r@(Rect x y w h) =
  (not (rectEmpty r)) ==>
   intersectRectAndLine 
    r (Point x y)
     (Point (x + w) (y + h))

isSuccess (Success{}) = True
isSuccess _ = False

rectTest = liftM (and . map isSuccess) results

results :: IO [Result]
results = sequence [
    quickCheckResult symmetricIntersect,
    quickCheckResult specFstIntersect,
    quickCheckResult symmetricUnion,
    quickCheckResult rectEncloseCornerns,
    quickCheckResult rectIntersectDiagonal
  ]

