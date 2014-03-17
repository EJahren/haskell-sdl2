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
import Data.Functor
import Control.Applicative

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
  st <- liftM enumFromC  ({#get SDL_Event.key.state#} p)
  wId <- liftM fromIntegral ({#get SDL_Event.key.windowID#} p)
  rp <- liftM enumFromC  ({#get SDL_Event.key.repeat #} p)
  sym <- liftM enumFromC  ({#get SDL_Event.key.keysym.scancode#} p)
  kc <- liftM enumFromC  ({#get SDL_Event.key.keysym.sym#} p)
  md <- liftM enumFromC  ({#get SDL_Event.key.keysym.mod#} p)
  return (c tmstmp wId st rp (Keysym sym kc md)) 

peekButton c p tmstmp = do
  wID <- fromIntegral <$> {#get SDL_Event.button.windowID#} p
  which <- fromIntegral <$> {#get SDL_Event.button.which#} p
  st <- enumFromC <$> {#get SDL_Event.button.state#} p
  bt <- fromIntegral <$> {#get SDL_Event.button.button#} p
  x <- fromIntegral <$> {#get SDL_Event.button.x#} p
  y <- fromIntegral <$> {#get SDL_Event.button.y#} p
  return (c tmstmp wID which st bt x y)

peekJoyButton c p tmstmp = do
  which <- fromIntegral <$> {#get SDL_Event.jbutton.which#} p
  bt <- fromIntegral <$> {#get SDL_Event.jbutton.button#} p
  st <- enumFromC <$> {#get SDL_Event.jbutton.state#} p
  return (c tmstmp which bt st)

peekJoyDevice c p tmstmp = do
  which <- fromIntegral <$> {#get SDL_Event.jdevice.which#} p
  return (c tmstmp which)

peekContrButton c p tmstmp = do
  which <- fromIntegral <$> {#get SDL_Event.cbutton.which#} p
  bt <- fromIntegral <$> {#get SDL_Event.cbutton.button#} p
  st <- enumFromC <$> {#get SDL_Event.cbutton.state#} p :: IO ButtonState
  return (c tmstmp which bt st)
  
peekContrDevice  c p tmstmp = do
  which <- fromIntegral <$> {#get SDL_Event.cdevice.which#} p
  return (c tmstmp which)

peekFinger  c p tmstmp = do
  tId <- fromIntegral <$> {#get SDL_Event.tfinger.touchId#} p
  fId <- fromIntegral <$> {#get SDL_Event.tfinger.fingerId#} p
  x   <- realToFrac   <$> {#get SDL_Event.tfinger.x#} p
  y   <- realToFrac   <$> {#get SDL_Event.tfinger.y#} p
  dx  <- realToFrac   <$> {#get SDL_Event.tfinger.dx#} p
  dy  <- realToFrac   <$> {#get SDL_Event.tfinger.dy#} p
  pr  <- realToFrac   <$> {#get SDL_Event.tfinger.pressure#} p
  return (c tmstmp tId fId x y dx dy pr)

instance Storable Event where
  sizeOf _ = {#sizeof SDL_Event #}
  alignment _ = {#alignof SDL_Event #}
  peek p = do
    t <- enumFromC <$> {#get SDL_Event.type#} p
    tmstmp <- fromIntegral <$> {#get SDL_Event.common.timestamp#} p
    case t of
      SdlFirstevent ->
        return (FirstEvent tmstmp)
      SdlQuit ->
        return (Quit tmstmp)
      SdlAppTerminating ->
        return (AppTerminating tmstmp)
      SdlAppLowmemory ->
        return (AppLowMemory tmstmp)
      SdlAppWillenterbackground  ->
        return (AppWillEnterBackground tmstmp)
      SdlAppDidenterbackground  ->
        return (AppDidEnterBackground tmstmp)
      SdlAppWillenterforeground  ->
        return (AppWillEnterForeground tmstmp)
      SdlAppDidenterforeground  ->
        return (AppDidEnterForeground tmstmp)
      SdlWindowevent ->
        liftM2 (WindowEvent tmstmp)
          (fromIntegral <$> {#get SDL_Event.window.windowID#} p)
          (enumFromC    <$> {#get SDL_Event.window.event#}    p)
      SdlSyswmevent -> return (SysWMEvent tmstmp)
      SdlKeydown -> peekKey KeyDown p tmstmp
      SdlKeyup -> peekKey KeyUp p tmstmp
      SdlTextediting ->
        liftM4 (TextEditing tmstmp)
          (fromIntegral <$> {#get SDL_Event.edit.windowID#} p)
          ({#get SDL_Event.edit.text#} p  >>= peekCString)
          (fromIntegral <$> {#get SDL_Event.edit.start#}    p)
          (fromIntegral <$> {#get SDL_Event.edit.length#}   p)
      SdlTextinput -> do
        liftM2 (TextInput tmstmp)
          (fromIntegral <$> {#get SDL_Event.text.windowID#} p)
          ({#get SDL_Event.text.text#} p  >>= peekCString)
      SdlMousemotion ->
        MouseMotion tmstmp <$>
          (fromIntegral <$> {#get SDL_Event.motion.windowID#} p) <*> 
          (fromIntegral <$> {#get SDL_Event.motion.which#}    p) <*>
          (enumFromC <$> {#get SDL_Event.motion.state#}       p) <*>
          (fromIntegral <$> {#get SDL_Event.motion.x#}        p) <*>
          (fromIntegral <$> {#get SDL_Event.motion.y#}        p) <*>
          (fromIntegral <$> {#get SDL_Event.motion.xrel#}     p) <*>
          (fromIntegral <$> {#get SDL_Event.motion.yrel#}     p)
      SdlMousebuttondown -> 
        peekButton MouseButtonDown p tmstmp
      SdlMousebuttonup ->
        peekButton MouseButtonUp p tmstmp
      SdlMousewheel ->
        liftM4 (MouseWheel tmstmp)
          (fromIntegral <$> {#get SDL_Event.motion.windowID#} p)
          (fromIntegral <$> {#get SDL_Event.motion.which#}    p)
          (fromIntegral <$> {#get SDL_Event.motion.x#}        p)
          (fromIntegral <$> {#get SDL_Event.motion.y#}        p)
      SdlJoyaxismotion ->
        liftM3 (JoyAxisMotion tmstmp)
          (fromIntegral <$> {#get SDL_Event.jaxis.which#} p)
          (fromIntegral <$> {#get SDL_Event.jaxis.axis#}  p)
          (fromIntegral <$> {#get SDL_Event.jaxis.value#}  p)
      SdlJoyballmotion -> 
        liftM4 (JoyBallMotion tmstmp)
          (fromIntegral <$> {#get SDL_Event.jball.which#} p)
          (fromIntegral <$> {#get SDL_Event.jball.ball#}  p)
          (fromIntegral <$> {#get SDL_Event.jball.xrel#}  p)
          (fromIntegral <$> {#get SDL_Event.jball.yrel#}  p)
      SdlJoyhatmotion -> 
        liftM3 (JoyHatMotion tmstmp)
          (fromIntegral <$> {#get SDL_Event.jhat.which#} p)
          (fromIntegral <$> {#get SDL_Event.jhat.hat#} p)
          (enumFromC    <$> {#get SDL_Event.jhat.hat#} p)
      SdlJoybuttondown -> peekJoyButton JoyButtonDown p tmstmp
      SdlJoybuttonup -> peekJoyButton JoyButtonUp p tmstmp
      SdlJoydeviceadded -> peekJoyDevice JoyDeviceAdded p tmstmp
      SdlJoydeviceremoved -> peekJoyDevice JoyDeviceRemoved p tmstmp
      SdlControlleraxismotion ->
        liftM3 (ControllerAxisMotion tmstmp)
          (fromIntegral <$> {#get SDL_Event.caxis.which#} p)
          (fromIntegral <$> {#get SDL_Event.caxis.axis#} p)
          (fromIntegral <$> {#get SDL_Event.caxis.value#} p)
      SdlControllerbuttondown ->
        peekContrButton ControllerButtonDown p tmstmp
      SdlControllerbuttonup -> undefined
        peekContrButton ControllerButtonUp p tmstmp
      SdlControllerdeviceadded ->
        peekContrDevice ControllerDeviceAdded p tmstmp
      SdlControllerdeviceremoved ->
        peekContrDevice ControllerDeviceRemoved p tmstmp
      SdlControllerdeviceremapped ->
        peekContrDevice ControllerDeviceAdded p tmstmp
      SdlFingerdown ->
        peekFinger FingerDown p tmstmp
      SdlFingerup ->
        peekFinger FingerUp p tmstmp
      SdlFingermotion ->
        peekFinger FingerMotion p tmstmp
      SdlMultigesture ->
        MultiGesture tmstmp <$>
          (fromIntegral <$> {#get SDL_Event.mgesture.touchId#} p) <*>
          (realToFrac   <$> {#get SDL_Event.mgesture.dTheta#}  p) <*>
          (realToFrac   <$> {#get SDL_Event.mgesture.dDist#}  p) <*>
          (realToFrac   <$> {#get SDL_Event.mgesture.x#}  p) <*>
          (realToFrac   <$> {#get SDL_Event.mgesture.y#}  p) <*>
          (fromIntegral <$> {#get SDL_Event.mgesture.numFingers#}  p)
      SdlDollargesture ->
        DollarGesture tmstmp <$>
          (fromIntegral <$> {#get SDL_Event.dgesture.touchId#} p) <*>
          (fromIntegral <$> {#get SDL_Event.dgesture.gestureId#}  p) <*>
          (fromIntegral <$> {#get SDL_Event.dgesture.numFingers#}  p) <*>
          (realToFrac   <$> {#get SDL_Event.dgesture.error#}  p) <*>
          (realToFrac   <$> {#get SDL_Event.dgesture.x#}  p) <*>
          (realToFrac   <$> {#get SDL_Event.dgesture.y#}  p)
      SdlDollarrecord -> undefined
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
       winID      :: Word32,
       text       :: String}
   | MouseMotion{
       timestamp   :: Word32,
       whichMouse  :: Word32,
       winID      :: Word32,
       buttonState :: ButtonState,
       x           :: Int,
       y           :: Int,
       xRel        :: Int,
       yRel        :: Int}
   | MouseButtonDown {
       timestamp   :: Word32,
       winID      :: Word32,
       whichMouse  :: Word32,
       buttonState :: ButtonState,
       whichButton :: Word8,
       x           :: Int,
       y           :: Int}
   | MouseButtonUp {
       timestamp   :: Word32,
       winID      :: Word32,
       whichMouse  :: Word32,
       buttonState :: ButtonState,
       whichButton :: Word8,
       x           :: Int,
       y           :: Int}
   | MouseWheel {
       timestamp  :: Word32,
       winID      :: Word32,
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
       numFingers :: Int,
       derror     :: Float,
       xCent      :: Float,
       yCent      :: Float}
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

pollEvent :: IO (Maybe Event)
pollEvent = alloca (\eventptr -> do
  res <- c_pollEvent eventptr
  if res < 1
    then return Nothing
    else do
      event <- peek eventptr
      return (Just event))


foreign import ccall safe "Graphics/UI/SDL2/Events.chs.h SDL_PollEvent"
  c_pollEvent :: (EventPtr -> IO CInt)

