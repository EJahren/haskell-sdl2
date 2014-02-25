module Graphics.UI.SDL2.Events where
{-# LANGUAGE CPP, BangPatterns, ForeignFunctionInterface #-}
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Storable


#include <SDL2/SDL_events.h>

type CEvent = Word32

data Event = Event CEvent deriving (Eq,Show,Ord)

#{enum Event, Event
   ,firstEvent = SDL_FIRSTEVENT
   ,quit = SDL_QUIT
   ,appTerminating = SDL_APP_TERMINATING
   ,appLowMemory = SDL_APP_LOWMEMORY
   ,appWillEnterBackground = SDL_APP_WILLENTERBACKGROUND
   ,appDidEnterBackground = SDL_APP_DIDENTERBACKGROUND
   ,appWillEnterForeground = SDL_APP_WILLENTERFOREGROUND
   ,appDidEnterForeground = SDL_APP_DIDENTERFOREGROUND
   ,windowEvent = SDL_WINDOWEVENT
   ,sysWMEvent = SDL_SYSWMEVENT
   ,keyDown = SDL_KEYDOWN
   ,keyUp = SDL_KEYUP
   ,textEditing = SDL_TEXTEDITING
   ,textInput = SDL_TEXTINPUT
   ,mouseMotion = SDL_MOUSEMOTION
   ,mouseButtonDown = SDL_MOUSEBUTTONDOWN
   ,mouseButtonUp = SDL_MOUSEBUTTONUP
   ,mouseWheel = SDL_MOUSEWHEEL
   ,joyAxisMotion = SDL_JOYAXISMOTION
   ,joyBallMotion = SDL_JOYBALLMOTION
   ,joyHatMotion = SDL_JOYHATMOTION
   ,joyButtonDown = SDL_JOYBUTTONDOWN
   ,joyButtonUp = SDL_JOYBUTTONUP
   ,joyDeviceAdded = SDL_JOYDEVICEADDED
   ,joyDeviceRemoved = SDL_JOYDEVICEREMOVED
   ,controllerAxisMotion = SDL_CONTROLLERAXISMOTION
   ,controllerButtonDown = SDL_CONTROLLERBUTTONDOWN
   ,controllerButtonUp = SDL_CONTROLLERBUTTONUP
   ,controllerDeviceAdded = SDL_CONTROLLERDEVICEADDED
   ,controllerDeviceRemoved = SDL_CONTROLLERDEVICEREMOVED
   ,controllerDeviceRemapped = SDL_CONTROLLERDEVICEREMAPPED
   ,fingerDown = SDL_FINGERDOWN
   ,fingerUp = SDL_FINGERUP
   ,fingerMotion = SDL_FINGERMOTION
   ,dollarGesture = SDL_DOLLARGESTURE
   ,dollarRecord = SDL_DOLLARRECORD
   ,multiGesture = SDL_MULTIGESTURE
   ,clipBoardUpdate = SDL_CLIPBOARDUPDATE
   ,dropFile = SDL_DROPFILE
   ,userEvent = SDL_USEREVENT
   ,lastEvent = SDL_LASTEVENT
  }

foreign import ccall unsafe "SDL2/SDL_events.h SDL_PollEvent"
    c_pollEvent :: Ptr CEvent -> IO CInt

pollEvent = alloca (\ e -> do
  b <- c_pollEvent e
  if b > 0 
    then do
      a <- peek e
      return (Just (Event a))
    else
      return Nothing)
  
