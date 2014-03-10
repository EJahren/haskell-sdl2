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
  timestamp :: Word32,
  windowID  :: Word32} deriving(Eq,Ord,Show)

data KeyEvent = KeyEvent deriving(Eq,Ord,Show)

data MouseInfo = MouseInfo{
  which :: Word32,
  buttonState :: ButtonState,
  xCoord :: Int,
  yCoord :: Int} deriving (Eq,Ord,Show)

{#enum SDL_EventType as EventType
   {underscoreToCase} deriving (Eq,Ord,Show)#}


data Event = Event{
  commonData :: CommonEventData,
  eventData :: EventData
}

instance Storable Event where
  sizeOf _ = {#sizeof SDL_Event #}
  alignment _ = 4
  peek p = Event
    <$> liftM peek ({#get SDL_Event->common #} p)
    <*> makeEventData p
  poke p x = do
    cmn <- {#get SDL_Event.common #} p
    poke p (commonData x)
    pokeEventData p x

data EventData =
     FirstEvent
   | Quit
   | AppTerminating
   | AppLowMemory
   | AppWillEnterBackground
   | AppDidEnterBackground
   | AppWillEnterForeground
   | AppDidEnterForeground
   | WindowEvent WindowEventID
   | SysWMEvent
   | KeyDown{
       state     :: KeyState,
       isRepeat  :: Bool,
       keysym    :: Keysym}
   | KeyUp{
       state     :: KeyState,
       isRepeat  :: Bool,
       keysym    :: Keysym}
   | TextEditing{
       text   :: String,
       start  :: Int,
       length :: Int}
   | TextInput{
       text :: String}
   | MouseMotion{
       whichMouse :: MouseInfo,
       xRel       :: Int,
       yRel       :: Int}
   | MouseButtonDown {
       whichMouse  :: MouseInfo,
       whichButton :: Word8}
   | MouseButtonUp {
       whichMouse  :: MouseInfo,
       whichButton :: Word8}
   | MouseWheel {
       mouseId :: Word32,
       xScrol :: Int,
       yScrol :: Int}
   | JoyAxisMotion {
       whichJS   :: JoystickID,
       axis      :: Word8,
       value     :: Int16}
   | JoyBallMotion{
       whichJS :: JoystickID,
       ball    :: Word8,
       xRel   :: Int,
       yRel   :: Int}
   | JoyHatMotion{
       whichJS :: JoystickID,
       hat     :: Word8,
       pos     :: HatPos}
   | JoyButtonDown{
       whichJS :: JoystickID,
       whichButtonJS :: Word8,
       buttonStateJS :: ButtonState}
   | JoyButtonUp{
       whichJS :: JoystickID,
       whichButtonJS :: Word8,
       buttonStateJS :: ButtonState}
   | JoyDeviceAdded{
       whichJS :: JoystickID}
   | JoyDeviceRemoved{
       whichJS :: JoystickID}
   | ControllerAxisMotion{
       whichCtrl :: JoystickID,
       axis      :: Word8,
       value     :: Int16}
   | ControllerButtonDown{
       whichCtrl        :: JoystickID,
       whichButtonCtrlr :: Word8,
       buttonStateCtrlr :: ButtonState}
   | ControllerButtonUp{
       whichCtrl        :: JoystickID,
       whichButtonCtrlr :: Word8,
       buttonStateCtrlr :: ButtonState}
   | ControllerDeviceAdded{
       whichCtrlr :: JoystickID}
   | ControllerDeviceRemoved{
       whichCtrlr :: JoystickID}
   | ControllerDeviceRemapped{
       whichCtrlr :: JoystickID}
   | FingerDown {
       touchId    :: TouchID,
       fingerId   :: FingerID,
       xRng       :: Float,
       yRng       :: Float,
       deltax     :: Float,
       deltay     :: Float,
       pressure   :: Float
     }
   | FingerUp{
       touchId    :: TouchID,
       fingerId   :: FingerID,
       xRng       :: Float,
       yRng       :: Float,
       deltax     :: Float,
       deltay     :: Float,
       pressure   :: Float
     }
   | FingerMotion{
       touchId    :: TouchID,
       fingerId   :: FingerID,
       xRng       :: Float,
       yRng       :: Float,
       deltax     :: Float,
       deltay     :: Float,
       pressure   :: Float
     }
   | DollarGesture{
       touchId    :: TouchID,
       gestureId  :: GestureID,
       numFingers :: Int
     }
   | DollarRecord
   | MultiGesture{
      touchId     :: TouchID,
      dTheta      :: Float,
      dDist       :: Float,
      xRng        :: Float,
      yRng        :: Float,
      numFingers  :: Int
     }
   | ClipBoardUpdate
   | DropFile{
      file :: FilePath}
   | UserEvent
   | LastEvent
    deriving (Eq, Ord, Show)

{#fun SDL_PollEvent as c_pollEvent {alloca- `Ptr EventType' peek*} -> `Int' #}


