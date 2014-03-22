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

----------------------------------------------
-- Event type
----------------------------------------------
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
       len        :: Int}
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


---------------------------------------------------
-- Storable instance for Event
---------------------------------------------------

peekKey c p tmstmp = do
  c tmstmp <$>
    (fromIntegral <$> {#get SDL_Event.key.windowID#} p) <*>
    (enumFromC    <$> {#get SDL_Event.key.state#} p) <*>
    (enumFromC    <$> {#get SDL_Event.key.repeat #} p) <*>
    (Keysym <$>
      (enumFromC <$> {#get SDL_Event.key.keysym.scancode#} p) <*>
      (enumFromC <$> {#get SDL_Event.key.keysym.sym#} p) <*>
      (enumFromC <$> {#get SDL_Event.key.keysym.mod#} p))

peekButton c p tmstmp = do
  c tmstmp <$>
    (fromIntegral <$> {#get SDL_Event.button.windowID#} p) <*>
    (fromIntegral <$> {#get SDL_Event.button.which#} p) <*>
    (enumFromC <$> {#get SDL_Event.button.state#} p) <*>
    (fromIntegral <$> {#get SDL_Event.button.button#} p) <*>
    (fromIntegral <$> {#get SDL_Event.button.x#} p) <*>
    (fromIntegral <$> {#get SDL_Event.button.y#} p)

peekJoyButton c p tmstmp = do
  c tmstmp <$>
    (fromIntegral <$> {#get SDL_Event.jbutton.which#} p) <*>
    (fromIntegral <$> {#get SDL_Event.jbutton.button#} p) <*>
    (enumFromC <$> {#get SDL_Event.jbutton.state#} p)

peekJoyDevice c p tmstmp =
  c tmstmp <$>
  (fromIntegral <$> {#get SDL_Event.jdevice.which#} p)

peekContrButton c p tmstmp =
  c tmstmp <$>
    (fromIntegral <$> {#get SDL_Event.cbutton.which#} p) <*>
    (fromIntegral <$> {#get SDL_Event.cbutton.button#} p) <*>
    (enumFromC <$> {#get SDL_Event.cbutton.state#} p :: IO ButtonState)
  
peekContrDevice  c p tmstmp = do
  c tmstmp <$>
    (fromIntegral <$> {#get SDL_Event.cdevice.which#} p)

peekFinger c p tmstmp =
  c tmstmp <$>
    (fromIntegral <$> {#get SDL_Event.tfinger.touchId#}  p) <*>
    (fromIntegral <$> {#get SDL_Event.tfinger.fingerId#} p) <*>
    (realToFrac   <$> {#get SDL_Event.tfinger.x#}        p) <*>
    (realToFrac   <$> {#get SDL_Event.tfinger.y#}        p) <*>
    (realToFrac   <$> {#get SDL_Event.tfinger.dx#}       p) <*>
    (realToFrac   <$> {#get SDL_Event.tfinger.dy#}       p) <*>
    (realToFrac   <$> {#get SDL_Event.tfinger.pressure#} p)

setKeyData (w, s, i, (Keysym sc k m)) p = do
  {#set SDL_Event.key.windowID #} p (fromIntegral w)
  {#set SDL_Event.key.state #} p (enumToC s)
  {#set SDL_Event.key.repeat #} p (enumToC i)
  {#set SDL_Event.key.keysym.scancode #} p (enumToC sc)
  {#set SDL_Event.key.keysym.sym #} p (enumToC k)
  {#set SDL_Event.key.keysym.mod #} p (enumToC m)

setMButtonEv (w,m,s,wb,x,y) p = do
  {#set SDL_Event.button.windowID #} p (fromIntegral w)
  {#set SDL_Event.button.which #} p (fromIntegral m)
  {#set SDL_Event.button.button #} p (fromIntegral wb)
  {#set SDL_Event.button.state #} p (enumToC s)
  {#set SDL_Event.button.x #} p (fromIntegral x)
  {#set SDL_Event.button.y #} p (fromIntegral y)


setJButton (j,wb,bs) p = do
  {#set SDL_Event.jbutton.which #} p (fromIntegral j)
  {#set SDL_Event.jbutton.button #} p (fromIntegral wb)
  {#set SDL_Event.jbutton.state #} p (enumToC bs)

setCButtonEv (wc, wbc, c) p = do
  {#set SDL_Event.cbutton.which#} p (fromIntegral wc)
  {#set SDL_Event.cbutton.button#} p (fromIntegral wbc)
  {#set SDL_Event.cbutton.state#} p (enumToC wbc)

setFingerEv ti fi xr yr dx dy pr p = do
  {#set SDL_Event.tfinger.touchId#} p (fromIntegral ti)
  {#set SDL_Event.tfinger.fingerId#} p (fromIntegral fi)
  {#set SDL_Event.tfinger.x#} p (realToFrac xr)
  {#set SDL_Event.tfinger.y#} p (realToFrac yr)
  {#set SDL_Event.tfinger.dx#} p (realToFrac dx)
  {#set SDL_Event.tfinger.dy#} p (realToFrac dy)
  {#set SDL_Event.tfinger.pressure#} p (realToFrac pr)


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
      SdlDollarrecord -> return (DollarRecord tmstmp)
      SdlClipboardupdate -> return (ClipBoardUpdate tmstmp)
      SdlDropfile ->
        DropFile tmstmp <$>
          ({#get SDL_Event.drop.file#} p >>= peekCString)
      SdlUserevent -> return (UserEvent tmstmp)
      SdlLastevent -> return (LastEvent tmstmp)

  poke p x = do
    let setType y = {#set SDL_Event.common.type #} p (enumToC y)
    {#set SDL_Event.common.timestamp #} p (fromIntegral . timestamp $ x)
    case x of
      FirstEvent{}  ->
          setType SdlFirstevent
      Quit{}        ->
          setType SdlQuit
      AppTerminating{} ->
          setType SdlAppTerminating
      AppLowMemory{} ->
          setType SdlAppLowmemory
      AppWillEnterBackground{} ->
          setType SdlAppWillenterbackground
      AppDidEnterBackground{} ->
          setType SdlAppDidenterbackground
      AppWillEnterForeground{} ->
          setType SdlAppWillenterforeground
      AppDidEnterForeground{} ->
          setType SdlAppDidenterforeground
      WindowEvent{
       winID = wId,
       winEventId = wEvId} -> do
         setType SdlWindowevent
         {#set SDL_Event.window.windowID #} p (fromIntegral wId)
         {#set SDL_Event.window.event #} p (enumToC wEvId)
      SysWMEvent{} ->
          setType SdlSyswmevent
      KeyDown{
       winID = w,
       state = s,
       isRepeat = i,
       keysym = k} -> do
         setType SdlKeydown
         setKeyData (w,s,i,k) p
      KeyUp{
          winID = w,
          state = s,
          isRepeat = i,
          keysym = k} -> do
            setType SdlKeyup
            setKeyData (w,s,i,k) p
      TextEditing{
        winID = w,
        text = t,
        start = s,
        len = l} -> do
          setType SdlTextediting
          {#set SDL_Event.edit.windowID#} p (fromIntegral w)
          {#set SDL_Event.edit.text#} p =<< newCString t
          {#set SDL_Event.edit.length#} p (fromIntegral l)
      TextInput{
        winID = w,
        text = t} -> do
          setType SdlTextinput
          {#set SDL_Event.text.windowID#} p (fromIntegral w)
          {#set SDL_Event.text.text#} p =<< newCString t
      MouseMotion{
        whichMouse = m,
        winID = w,
        buttonState = s,
        x = x',
        y = y',
        xRel = xr,
        yRel = yr} -> do
          setType SdlMousemotion
          {#set SDL_Event.motion.windowID#} p (fromIntegral w)
          {#set SDL_Event.motion.state#} p (enumToC s)
          {#set SDL_Event.motion.x#} p (fromIntegral x')
          {#set SDL_Event.motion.y#} p (fromIntegral y')
          {#set SDL_Event.motion.xrel#} p (fromIntegral xr)
          {#set SDL_Event.motion.yrel#} p (fromIntegral yr)

      MouseButtonDown {
        winID = w,
        whichMouse = m,
        buttonState = s,
        whichButton = wb,
        x = x',
        y = y'} -> do
          setType SdlMousebuttondown
          setMButtonEv (w,m,s,wb,x',y') p
          
      MouseButtonUp {
        winID = w,
        whichMouse = m,
        buttonState = s,
        whichButton = wb,
        x = x',
        y = y'} -> do
          setType SdlMousebuttonup
          setMButtonEv (w,m,s,wb,x',y') p
      MouseWheel {
        winID = w,
        mouseId = m,
        xScrol = x,
        yScrol = y} -> do
          setType SdlMousewheel
          {#set SDL_Event.wheel.windowID#} p (fromIntegral w)
          {#set SDL_Event.wheel.which#} p (fromIntegral m)
          {#set SDL_Event.wheel.x#} p (fromIntegral x)
          {#set SDL_Event.wheel.y#} p (fromIntegral y)
      JoyAxisMotion {
        whichJS = j,
        axis = a,
        value = v} -> do
          setType SdlJoyaxismotion
          {#set SDL_Event.jaxis.which#} p (fromIntegral j)
          {#set SDL_Event.jaxis.axis#} p (fromIntegral a)
          {#set SDL_Event.jaxis.value#} p (fromIntegral v)
          
      JoyBallMotion{
        whichJS = j,
        ball = b,
        xRel = x,
        yRel = y} -> do
          setType SdlJoyaxismotion
          {#set SDL_Event.jball.which#} p (fromIntegral j)
          {#set SDL_Event.jball.ball#} p (fromIntegral b)
          {#set SDL_Event.jball.xrel#} p (fromIntegral x)
          {#set SDL_Event.jball.yrel#} p (fromIntegral y)
      JoyHatMotion{
        whichJS = j,
        hat = h,
        pos = x} -> do
          setType SdlJoyhatmotion
          {#set SDL_Event.jhat.which#} p (fromIntegral j)
          {#set SDL_Event.jhat.hat#} p (fromIntegral h)
          {#set SDL_Event.jhat.value#} p (enumToC x)
      JoyButtonDown{
        whichJS = j,      
        whichButtonJS = wb, 
        buttonStateJS = bs} -> do
          setType SdlJoybuttondown
          setJButton (j,wb,bs) p
      JoyButtonUp{
        whichJS = j,      
        whichButtonJS = wb, 
        buttonStateJS = bs} -> do
          setType SdlJoybuttonup
          setJButton (j,wb,bs) p
      JoyDeviceAdded{
        whichJS = wj} -> do
          setType SdlJoydeviceadded
          {#set SDL_Event.jdevice.which#} p (fromIntegral wj)
      JoyDeviceRemoved{
        whichJS = wj} -> do
          setType SdlJoydeviceremoved
          {#set SDL_Event.jdevice.which#} p (fromIntegral wj)
      ControllerAxisMotion{
        whichCtrl = wc,
        axis = a,
        value = v} -> do
          setType SdlControlleraxismotion
          {#set SDL_Event.caxis.which#} p (fromIntegral wc)
          {#set SDL_Event.caxis.axis#} p (fromIntegral a)
          {#set SDL_Event.caxis.value#} p (fromIntegral v)
      ControllerButtonDown{
        whichCtrl = wc,
        whichButtonCtrlr = wbc,
        buttonStateCtrlr = c} -> do
          setType SdlControllerbuttondown
          setCButtonEv (wc, wbc, c) p
      ControllerButtonUp{
        whichCtrl = wc,
        whichButtonCtrlr = wbc,
        buttonStateCtrlr = c} -> do
          setType SdlControllerbuttonup
          setCButtonEv (wc, wbc, c) p
      ControllerDeviceAdded{
        whichCtrlr = wc} -> do
          setType SdlControllerdeviceadded
          {#set SDL_Event.cdevice.which#} p (fromIntegral wc)
      ControllerDeviceRemoved{
        whichCtrlr = wc} -> do
          setType SdlControllerdeviceremoved
          {#set SDL_Event.cdevice.which#} p (fromIntegral wc)
      ControllerDeviceRemapped{
        whichCtrlr = wc} -> do
          setType SdlControllerdeviceremapped
          {#set SDL_Event.cdevice.which#} p (fromIntegral wc)
      FingerDown {
        touchId = ti,
        fingerId = fi,
        xRng = xr,
        yRng = yr,
        deltax = dx,
        deltay = dy,
        pressure = pr} -> do
          setType SdlFingerdown
          setFingerEv ti fi xr yr dx dy pr p
      FingerUp {
        touchId = ti,
        fingerId = fi,
        xRng = xr,
        yRng = yr,
        deltax = dx,
        deltay = dy,
        pressure = pr} -> do
          setType SdlFingerdown
          setFingerEv ti fi xr yr dx dy pr p

      FingerMotion{
        touchId = ti,
        fingerId = fi,
        xRng = xr,
        yRng = yr,
        deltax = dx,
        deltay = dy,
        pressure = pr} -> do
          setType SdlFingerdown
          setFingerEv ti fi xr yr dx dy pr p

      DollarGesture{
        touchId = ti,
        gestureId = gi,
        numFingers = nf,
        derror = de,
        xCent = xc,
        yCent = yc} -> do
          setType SdlDollargesture
          {#set SDL_Event.dgesture.touchId#} p
            (fromIntegral ti)
          {#set SDL_Event.dgesture.gestureId#} p
            (fromIntegral gi)
          {#set SDL_Event.dgesture.numFingers#} p
            (fromIntegral nf)
          {#set SDL_Event.dgesture.error#} p
            (realToFrac de)
          {#set SDL_Event.dgesture.x#} p
            (realToFrac xc)
          {#set SDL_Event.dgesture.y#} p
            (realToFrac xc)
      DollarRecord{} ->
         setType SdlDollarrecord
      MultiGesture{
        touchId = ti,
        dTheta = dt,
        dDist = dd,
        xRng = xr,
        yRng = yr,
        numFingers = nf} -> do
          setType SdlMultigesture
          {#set SDL_Event.mgesture.touchId#} p
            (fromIntegral ti)
          {#set SDL_Event.mgesture.dTheta#} p
            (realToFrac dt)
          {#set SDL_Event.mgesture.dDist#} p
            (realToFrac dd)
          {#set SDL_Event.mgesture.x#} p
            (realToFrac xr)
          {#set SDL_Event.mgesture.y#} p
            (realToFrac yr)
          {#set SDL_Event.mgesture.numFingers#} p
            (fromIntegral nf)
      ClipBoardUpdate{} ->
        setType SdlClipboardupdate
      DropFile{
        file = f} -> do 
          setType SdlDropfile
          {#set SDL_Event.drop.file#} p =<< newCString f
      UserEvent{} ->
        setType SdlUserevent
      LastEvent{} ->
        setType SdlLastevent
      
  

