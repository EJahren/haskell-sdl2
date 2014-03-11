module Graphics.UI.SDL2.Events where
{-# LANGUAGE 
  CPP,
  BangPatterns,
  ForeignFunctionInterface #-}
import Foreign
import Foreign.C
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Storable

import Control.Monad

import Graphics.UI.SDL2.Common
{# import Graphics.UI.SDL2.Video #}
{# import Graphics.UI.SDL2.Keyboard #}
{# import Graphics.UI.SDL2.Joystick #}
{# import Graphics.UI.SDL2.Touch #}
{# import Graphics.UI.SDL2.Gesture #}

#include <SDL2/SDL_events.h>

{#context lib = "SDL2"#}

{#enum define KeyState
  {SDL_PRESSED as Pressed,
   SDL_RELEASED as Released} deriving (Eq,Ord,Show) #}

type ButtonState = KeyState

data CommonEventData = CommonEventData {
  windowID  :: Word32} deriving(Eq,Ord,Show)

data KeyEvent = KeyEvent deriving(Eq,Ord,Show)

{#enum SDL_EventType as EventType
   {underscoreToCase} deriving (Eq,Ord,Show)#}

peekKey c p tmstmp = do
  st <- liftM enumFromC ({#get SDL_Event.key.state#} p)
  wId <- liftM fromIntegral ({#get SDL_Event.key.windowID#} p)
  rp <- liftM enumFromC ({#get SDL_Event.key.repeat #} p)
  sym <- liftM enumFromC ({#get SDL_Event.key.keysym.scancode#} p)
  kc <- liftM enumFromC ({#get SDL_Event.key.keysym.sym#} p)
  md <- liftM enumFromC ({#get SDL_Event.key.keysym.mod#} p)
  return (c tmstmp wId st rp (Keysym sym kc md)) 

instance Storable Event where
  sizeOf _ = {#sizeof SDL_Event #}
  alignment _ = 4
  peek p = do
    t <- liftM enumFromC ({#get SDL_Event.type#} p)
    tmstmp <- liftM fromIntegral ({#get SDL_Event.common.timestamp#} p)
    case t of
      SdlFirstevent -> return (FirstEvent tmstmp)
      SdlQuit -> return (Quit tmstmp)
      SdlAppTerminating -> return (AppTerminating tmstmp)
      SdlAppLowmemory -> return (AppLowMemory tmstmp)
      SdlAppWillenterbackground  -> return (AppWillEnterBackground tmstmp)
      SdlAppDidenterbackground  -> return (AppDidEnterBackground tmstmp)
      SdlAppWillenterforeground  -> return (AppWillEnterForeground tmstmp)
      SdlAppDidenterforeground  -> return (AppDidEnterForeground tmstmp)
      SdlWindowevent -> do
        winEvId <- liftM enumFromC ({#get SDL_Event.window.event#} p)
        wId <- liftM fromIntegral ({#get SDL_Event.window.windowID#} p)
        return (WindowEvent tmstmp wId winEvId)
      SdlSyswmevent -> return (SysWMEvent tmstmp)
      SdlKeydown -> peekKey KeyDown p tmstmp
      SdlKeyup -> peekKey KeyUp p tmstmp
      SdlTextediting -> do
        wID <- liftM fromIntegral ({#get SDL_Event.edit.windowID#} p)
        txt <- {#get SDL_Event.edit.text#} p  >>= peekCString
        strt <- liftM fromIntegral ({#get SDL_Event.edit.start#} p)
        len <- liftM fromIntegral ({#get SDL_Event.edit.length#} p)
        return (TextEditing tmstmp wID txt strt len)
      SdlTextinput -> undefined
      SdlMousemotion -> undefined
      SdlMousebuttondown -> undefined
      SdlMousebuttonup -> undefined
      SdlMousewheel -> undefined
      SdlJoyaxismotion -> undefined
      SdlJoyballmotion -> undefined
      SdlJoyhatmotion -> undefined
      SdlJoybuttondown -> undefined
      SdlJoybuttonup -> undefined
      SdlJoydeviceadded -> undefined
      SdlJoydeviceremoved -> undefined
      SdlControlleraxismotion -> undefined
      SdlControllerbuttondown -> undefined
      SdlControllerbuttonup -> undefined
      SdlControllerdeviceadded -> undefined
      SdlControllerdeviceremoved -> undefined
      SdlControllerdeviceremapped -> undefined
      SdlFingerdown -> undefined
      SdlFingerup -> undefined
      SdlFingermotion -> undefined
      SdlDollargesture -> undefined
      SdlDollarrecord -> undefined
      SdlMultigesture -> undefined
      SdlClipboardupdate -> undefined
      SdlDropfile -> undefined
      SdlUserevent -> undefined
      SdlLastevent -> undefined

  poke p x = undefined
  {-do
    cmn <- {#get SDL_Event.common #} p
    poke p (commonData x)
    pokeEventData p x -}

data Event =
     FirstEvent{
       timestamp :: Word32}
   | Quit{
       timestamp :: Word32}
   | AppTerminating{
       timestamp :: Word32}
   | AppLowMemory{
       timestamp :: Word32}
   | AppWillEnterBackground{
       timestamp :: Word32}
   | AppDidEnterBackground{
       timestamp :: Word32}
   | AppWillEnterForeground{
       timestamp :: Word32}
   | AppDidEnterForeground{
       timestamp :: Word32}
   | WindowEvent{
       timestamp  :: Word32,
       winID      :: Word32,
       winEventId :: WindowEventID} 
   | SysWMEvent{
       timestamp :: Word32}
   | KeyDown{
       timestamp  :: Word32,
       winID      :: Word32,
       state      :: KeyState,
       isRepeat   :: Bool,
       keysym     :: Keysym}
   | KeyUp{
       timestamp  :: Word32,
       winID      :: Word32,
       state      :: KeyState,
       isRepeat   :: Bool,
       keysym     :: Keysym}
   | TextEditing{
       timestamp  :: Word32,
       winID      :: Word32,
       text       :: String,
       start      :: Int,
       length     :: Int}
   | TextInput{
       timestamp  :: Word32,
       text       :: String}
   | MouseMotion{
       timestamp  :: Word32,
       whichMouse :: Word32,
       xRel       :: Int,
       yRel       :: Int}
   | MouseButtonDown {
       timestamp   :: Word32,
       whichMouse  :: Word32,
       whichButton :: Word8}
   | MouseButtonUp {
       timestamp   :: Word32,
       whichMouse  :: Word32,
       whichButton :: Word8}
   | MouseWheel {
       timestamp  :: Word32,
       mouseId    :: Word32,
       xScrol     :: Int,
       yScrol     :: Int}
   | JoyAxisMotion {
       timestamp  :: Word32,
       whichJS    :: JoystickID,
       axis       :: Word8,
       value      :: Int16}
   | JoyBallMotion{
       timestamp  :: Word32,
       whichJS    :: JoystickID,
       ball       :: Word8,
       xRel       :: Int,
       yRel       :: Int}
   | JoyHatMotion{
       timestamp  :: Word32,
       whichJS    :: JoystickID,
       hat        :: Word8,
       pos        :: HatPos}
   | JoyButtonDown{
       timestamp     :: Word32,
       whichJS       :: JoystickID,
       whichButtonJS :: Word8,
       buttonStateJS :: ButtonState}
   | JoyButtonUp{
       timestamp     :: Word32,
       whichJS       :: JoystickID,
       whichButtonJS :: Word8,
       buttonStateJS :: ButtonState}
   | JoyDeviceAdded{
       timestamp  :: Word32,
       whichJS    :: JoystickID}
   | JoyDeviceRemoved{
       timestamp  :: Word32,
       whichJS    :: JoystickID}
   | ControllerAxisMotion{
       timestamp  :: Word32,
       whichCtrl  :: JoystickID,
       axis       :: Word8,
       value      :: Int16}
   | ControllerButtonDown{
       timestamp        :: Word32,
       whichCtrl        :: JoystickID,
       whichButtonCtrlr :: Word8,
       buttonStateCtrlr :: ButtonState}
   | ControllerButtonUp{
       timestamp        :: Word32,
       whichCtrl        :: JoystickID,
       whichButtonCtrlr :: Word8,
       buttonStateCtrlr :: ButtonState}
   | ControllerDeviceAdded{
       timestamp  :: Word32,
       whichCtrlr :: JoystickID}
   | ControllerDeviceRemoved{
       timestamp  :: Word32,
       whichCtrlr :: JoystickID}
   | ControllerDeviceRemapped{
       timestamp  :: Word32,
       whichCtrlr :: JoystickID}
   | FingerDown {
       timestamp  :: Word32,
       touchId    :: TouchID,
       fingerId   :: FingerID,
       xRng       :: Float,
       yRng       :: Float,
       deltax     :: Float,
       deltay     :: Float,
       pressure   :: Float}
   | FingerUp{
       timestamp  :: Word32,
       touchId    :: TouchID,
       fingerId   :: FingerID,
       xRng       :: Float,
       yRng       :: Float,
       deltax     :: Float,
       deltay     :: Float,
       pressure   :: Float}
   | FingerMotion{
       timestamp  :: Word32,
       touchId    :: TouchID,
       fingerId   :: FingerID,
       xRng       :: Float,
       yRng       :: Float,
       deltax     :: Float,
       deltay     :: Float,
       pressure   :: Float}
   | DollarGesture{
       timestamp  :: Word32,
       touchId    :: TouchID,
       gestureId  :: GestureID,
       numFingers :: Int}
   | DollarRecord{
       timestamp  :: Word32}
   | MultiGesture{
       timestamp  :: Word32,
       touchId    :: TouchID,
       dTheta     :: Float,
       dDist      :: Float,
       xRng       :: Float,
       yRng       :: Float,
       numFingers :: Int}
   | ClipBoardUpdate{
       timestamp  :: Word32}
   | DropFile{
       timestamp  :: Word32,
       file       :: FilePath}
   | UserEvent{
       timestamp  :: Word32}
   | LastEvent{
       timestamp  :: Word32}
    deriving (Eq, Ord, Show)

{#pointer *SDL_Event as EventPtr -> Event#}

{#fun SDL_PollEvent as c_pollEvent {alloca- `Event' peek*} -> `Int' #}


