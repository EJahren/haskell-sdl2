{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Graphics.UI.SDL2.Pixels(
  PixelFormatType(..)
 ) where

#include <SDL2/SDL.h>
#include <SDL2/SDL_pixels.h>

{#context lib = "SDL2" prefix="SDL"#}

{- | Pixel type. -}
{#enum define PixelFormatType
  {
    SDL_PIXELFORMAT_UNKNOWN as PixelFormatUnknown,
    SDL_PIXELFORMAT_INDEX1LSB as PixelFormatIndex1LSB,
    SDL_PIXELFORMAT_INDEX1MSB as PixelFormatIndex1MSB,
    SDL_PIXELFORMAT_INDEX4LSB as PixelFormatIndex4LSB,
    SDL_PIXELFORMAT_INDEX4MSB as  PixelFormatIndex4MSB,
    SDL_PIXELFORMAT_INDEX8 as PixelFormatIndex8,
    SDL_PIXELFORMAT_RGB332 as PixelFormatRGB332,
    SDL_PIXELFORMAT_RGB444 as PixelFormatRGB444,
    SDL_PIXELFORMAT_RGB555 as PixelFormatRGB555,
    SDL_PIXELFORMAT_BGR555 as PixelFormatBGR555,
    SDL_PIXELFORMAT_ARGB4444 as PixelFormatARGB4444,
    SDL_PIXELFORMAT_RGBA4444 as PixelFormatRGBA4444,
    SDL_PIXELFORMAT_ABGR4444 as PixelFormatABGR4444,
    SDL_PIXELFORMAT_BGRA4444 as PixelFormatBGRA4444,
    SDL_PIXELFORMAT_ARGB1555 as PixelFormatARGB1555,
    SDL_PIXELFORMAT_RGBA5551 as PixelFormatRGBA1555,
    SDL_PIXELFORMAT_ABGR1555 as PixelFormatABGR1555,
    SDL_PIXELFORMAT_BGRA5551 as PixelFormatBGRA5551,
    SDL_PIXELFORMAT_RGB565 as PixelFormatRGB565,
    SDL_PIXELFORMAT_BGR565 as PixelFormatBGR565,
    SDL_PIXELFORMAT_RGB24 as PixelFormatRGB24,
    SDL_PIXELFORMAT_BGR24 as PixelFormatBGR24,
    SDL_PIXELFORMAT_RGB888 as PixelFormatRGB888,
    SDL_PIXELFORMAT_RGBX8888 as PixelFormatRGBX8888,
    SDL_PIXELFORMAT_BGR888 as PixelFormatBGR888,
    SDL_PIXELFORMAT_BGRX8888 as PixelFormatBGRX8888,
    SDL_PIXELFORMAT_ARGB8888 as PixelFormatARGB8888,
    SDL_PIXELFORMAT_RGBA8888 as PixelFormatRGBA8888,
    SDL_PIXELFORMAT_ABGR8888 as PixelFormatABGR8888,
    SDL_PIXELFORMAT_BGRA8888 as PixelFormatBGRA8888,
    SDL_PIXELFORMAT_ARGB2101010 as PixelFormatARGB2101010
--    SDL_PIXELFORMAT_YV12 as PixelFormatYV12,
--    SDL_PIXELFORMAT_IYUV as PixelFormatIYUV,
--    SDL_PIXELFORMAT_YUY2 as PixelFormatYUY2,
--    SDL_PIXELFORMAT_UYVY as PixelFormatUYVY,
--    SDL_PIXELFORMAT_YVYU as PixelFormatYVYU
  } deriving (Eq,Ord,Show) #}
