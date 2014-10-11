{-# LANGUAGE CPP, BangPatterns, ForeignFunctionInterface #-}
{- | Defines SDL Rectangles (Rect) and
  operations on these. -}
module Graphics.UI.SDL2.Rect
  (Point(..),
   PointPtr,
   Rect(..),
   RectPtr,
   rectEmpty,
   intersectRect,
   intersectRectAndLine,
   unionRect,
   enclosedPoints,
   hasIntersection)
   where
import Graphics.UI.SDL2.Common
import System.IO.Unsafe(unsafePerformIO)
import Foreign hiding(unsafePerformIO)
import Foreign.C.Types
import Test.QuickCheck.Arbitrary

import Control.Monad
import Control.Applicative
import Data.List

#include <SDL2/SDL_rect.h>

{#context lib = "SDL2" prefix = "SDL"#}

data Point = Point {
  pointX :: Int,
  pointY :: Int
  }

instance Storable Point where
  sizeOf _ = {#sizeof Point #}
  alignment _ = {#alignof Point #}
  peek p = do
    liftM2 Point
      (fromIntegral <$> {#get Point.x#} p)
      (fromIntegral <$> {#get Point.y#} p)
  poke p (Point x y) = do
    {#set Point.x#} p (fromIntegral x)
    {#set Point.y#} p (fromIntegral y)

instance Arbitrary Point where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Point x y)
  shrink (Point x y) =
   zipWith Point (shrink x) (shrink y)
{#pointer *Point as PointPtr -> Point #}

-- | A rectangle, with the origin at the upper left.
data Rect = Rect {
  xCoord :: Int,
  yCoord :: Int,
  width :: Int,
  height :: Int}

{#pointer *Rect as RectPtr -> Rect #}
 
instance Storable Rect where
  sizeOf _ = {#sizeof Rect #}
  alignment _ = {#alignof Rect #}
  peek p = do
    liftM4 Rect
      (fromIntegral <$> {#get Rect.x#} p)
      (fromIntegral <$> {#get Rect.y#} p)
      (fromIntegral <$> {#get Rect.w#} p)
      (fromIntegral <$> {#get Rect.h#} p)
  poke p (Rect x y w h) = do
    {#set Rect.x#} p (fromIntegral x)
    {#set Rect.y#} p (fromIntegral y)
    {#set Rect.w#} p (fromIntegral w)
    {#set Rect.h#} p (fromIntegral h)

instance Arbitrary Rect where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    w <- arbitrary
    h <- arbitrary
    return (Rect x y w h)
  shrink (Rect x y w h) =
   zipWith4 Rect (shrink x) (shrink y) (shrink w) (shrink h)

{- |
  Returns true if the rectangle has no area.
-}
{#fun RectEmpty as rectEmpty
  {withPtr* `Rect'} -> `Bool' #}

{- |
 Returns true if the two rectangles are equal.
-}
{#fun pure RectEquals as rectEquals
  {withPtr* `Rect',
   withPtr* `Rect'} -> `Bool' #}

instance Eq Rect where
  (==) = rectEquals

{- |
 Determine whether two rectangles intersect.
 Returns True if the two rectangles intersect.
-}
{#fun pure HasIntersection as hasIntersection
  {withPtr* `Rect',
   withPtr* `Rect'} -> `Bool'#}

{- |
  Calculate the intersection of two rectangles.
 Returns True if there is an intersection, False otherwise.
-}
{#fun pure IntersectRect as intersectRect 
  {withPtr* `Rect',
   withPtr* `Rect',
   alloca-`Rect'peek*} -> `()'#}

{- |
Calculate the union of two rectangles.
-}
{#fun pure  UnionRect as unionRect
  {withPtr* `Rect',
   withPtr* `Rect',
   alloca- `Rect' peek*} -> `()'#}


{- |Calculate a minimal rectangle enclosing a set of points
  Returns True if any points were within the clipping rect
-}
{#fun EnclosePoints as enclosedPoints
 {withArr* `[Point]'&,
  withPtr* `Rect',
  alloca- `Rect' peek*} -> `Bool'#}

{- |
Calculate the intersection of a rectangle and line segment.
True if there is an intersection, False otherwise.
-}
intersectRectAndLine :: 
  Rect -- ^ The rectangle to intersect with.
  -> Point -- ^ The starting point of the line segment
  -> Point -- ^ The end point of the line segment
  -> Bool -- ^ Wether or not the line intersects with
          -- the rectangle
intersectRectAndLine r (Point x1 y1) (Point x2 y2) =
  c_intersectRectAndLine r
   (fromIntegral x1)
   (fromIntegral y1) 
   (fromIntegral x2)
   (fromIntegral y2)

{#fun pure IntersectRectAndLine as c_intersectRectAndLine
  {withPtr* `Rect'
   ,withPtr* `CInt' -- ^ x1
   ,withPtr* `CInt' -- ^ y1
   ,withPtr* `CInt' -- ^ x2,
   ,withPtr* `CInt' -- ^ y2,
  } -> `Bool' #}

