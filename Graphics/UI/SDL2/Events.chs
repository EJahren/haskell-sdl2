{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# OPTIONS -fno-warn-missing-signatures #-}
module Graphics.UI.SDL2.Events(
  ButtonState,
  KeyState,
  Event(..),
  pollEvent
  )where
import Foreign
import Foreign.C
import Foreign.C.Types

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

{#enum SDL_EventType as EventType
   {underscoreToCase} deriving (Eq,Ord,Show)#}

----------------------------------------------
-- Event type
----------------------------------------------
data Event =
     FirstEvent{
       timestamp :: Int32}
   | Quit{
       timestamp :: Int32}
   | AppTerminating{
       timestamp :: Int32}
   | AppLowMemory{
       timestamp :: Int32}
   | AppWillEnterBackground{
       timestamp :: Int32}
   | AppDidEnterBackground{
       timestamp :: Int32}
   | AppWillEnterForeground{
       timestamp :: Int32}
   | AppDidEnterForeground{
       timestamp :: Int32}
   | WindowEvent{
       timestamp  :: Int32,
       winID      :: Int32,
       winEventId :: WindowEventID} 
   | SysWMEvent{
       timestamp :: Int32}
   | KeyDown{
       timestamp  :: Int32,
       winID      :: Int32,
       state      :: KeyState,
       isRepeat   :: Bool,
       keysym     :: Keysym}
   | KeyUp{
       timestamp  :: Int32,
       winID      :: Int32,
       state      :: KeyState,
       isRepeat   :: Bool,
       keysym     :: Keysym}
   | TextEditing{
       timestamp  :: Int32,
       winID      :: Int32,
       text       :: String,
       start      :: Int32,
       len        :: Int32}
   | TextInput{
       timestamp  :: Int32,
       winID      :: Int32,
       text       :: String}
   | MouseMotion{
       timestamp   :: Int32,
       whichMouse  :: Int32,
       winID      :: Int32,
       buttonState :: ButtonState,
       x           :: Int32,
       y           :: Int32,
       xRel        :: Int32,
       yRel        :: Int32}
   | MouseButtonDown {
       timestamp   :: Int32,
       winID      :: Int32,
       whichMouse  :: Int32,
       buttonState :: ButtonState,
       whichButton :: Word8,
       x           :: Int32,
       y           :: Int32}
   | MouseButtonUp {
       timestamp   :: Int32,
       winID      :: Int32,
       whichMouse  :: Int32,
       buttonState :: ButtonState,
       whichButton :: Word8,
       x           :: Int32,
       y           :: Int32}
   | MouseWheel {
       timestamp  :: Int32,
       winID      :: Int32,
       mouseId    :: Int32,
       xScrol     :: Int32,
       yScrol     :: Int32}
   | JoyAxisMotion {
       timestamp  :: Int32,
       whichJS    :: JoystickID,
       axis       :: Word8,
       value      :: Int16}
   | JoyBallMotion{
       timestamp  :: Int32,
       whichJS    :: JoystickID,
       ball       :: Word8,
       xRel       :: Int32,
       yRel       :: Int32}
   | JoyHatMotion{
       timestamp  :: Int32,
       whichJS    :: JoystickID,
       hat        :: Word8,
       pos        :: HatPos}
   | JoyButtonDown{
       timestamp     :: Int32,
       whichJS       :: JoystickID,
       whichButtonJS :: Word8,
       buttonStateJS :: ButtonState}
   | JoyButtonUp{
       timestamp     :: Int32,
       whichJS       :: JoystickID,
       whichButtonJS :: Word8,
       buttonStateJS :: ButtonState}
   | JoyDeviceAdded{
       timestamp  :: Int32,
       whichJS    :: JoystickID}
   | JoyDeviceRemoved{
       timestamp  :: Int32,
       whichJS    :: JoystickID}
   | ControllerAxisMotion{
       timestamp  :: Int32,
       whichCtrl  :: JoystickID,
       axis       :: Word8,
       value      :: Int16}
   | ControllerButtonDown{
       timestamp        :: Int32,
       whichCtrl        :: JoystickID,
       whichButtonCtrlr :: Word8,
       buttonStateCtrlr :: ButtonState}
   | ControllerButtonUp{
       timestamp        :: Int32,
       whichCtrl        :: JoystickID,
       whichButtonCtrlr :: Word8,
       buttonStateCtrlr :: ButtonState}
   | ControllerDeviceAdded{
       timestamp  :: Int32,
       whichCtrlr :: JoystickID}
   | ControllerDeviceRemoved{
       timestamp  :: Int32,
       whichCtrlr :: JoystickID}
   | ControllerDeviceRemapped{
       timestamp  :: Int32,
       whichCtrlr :: JoystickID}
   | FingerDown {
       timestamp  :: Int32,
       touchId    :: TouchID,
       fingerId   :: FingerID,
       xRng       :: Float,
       yRng       :: Float,
       deltax     :: Float,
       deltay     :: Float,
       pressure   :: Float}
   | FingerUp{
       timestamp  :: Int32,
       touchId    :: TouchID,
       fingerId   :: FingerID,
       xRng       :: Float,
       yRng       :: Float,
       deltax     :: Float,
       deltay     :: Float,
       pressure   :: Float}
   | FingerMotion{
       timestamp  :: Int32,
       touchId    :: TouchID,
       fingerId   :: FingerID,
       xRng       :: Float,
       yRng       :: Float,
       deltax     :: Float,
       deltay     :: Float,
       pressure   :: Float}
   | DollarGesture{
       timestamp  :: Int32,
       touchId    :: TouchID,
       gestureId  :: GestureID,
       numFingers :: Int32,
       derror     :: Float,
       xCent      :: Float,
       yCent      :: Float}
   | DollarRecord{
       timestamp  :: Int32}
   | MultiGesture{
       timestamp  :: Int32,
       touchId    :: TouchID,
       dTheta     :: Float,
       dDist      :: Float,
       xRng       :: Float,
       yRng       :: Float,
       numFingers :: Int32}
   | ClipBoardUpdate{
       timestamp  :: Int32}
   | DropFile{
       timestamp  :: Int32,
       file       :: FilePath}
   | UserEvent{
       timestamp  :: Int32}
   | LastEvent{
       timestamp  :: Int32}
    deriving (Eq, Ord, Show)

{#pointer *SDL_Event as EventPtr -> Event#}

pollEvent :: IO (Maybe Event)
pollEvent = alloca (\eventptr -> do
  res <- {#call SDL_PollEvent as pollEvent'_#} eventptr
  if res < 1
    then return Nothing
    else do
      event <- peek eventptr
      return (Just event))

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

setKeyData :: (Int32,ButtonState,Bool,Keysym) -> EventPtr -> IO ()
setKeyData (w, s, i, (Keysym sc k m)) p = do
  {#set SDL_Event.key.windowID #} p (fromIntegral w)
  {#set SDL_Event.key.state #} p (enumToC s)
  {#set SDL_Event.key.repeat #} p (enumToC i)
  {#set SDL_Event.key.keysym.scancode #} p (enumToC sc)
  {#set SDL_Event.key.keysym.sym #} p (enumToC k)
  {#set SDL_Event.key.keysym.mod #} p (enumToC m)

setMButtonEv ::
  (Int32,Int32,ButtonState,Word8,Int32,Int32) -> EventPtr -> IO ()
setMButtonEv (w,m,s,wb,x',y') p = do
  {#set SDL_Event.button.windowID #} p (fromIntegral w)
  {#set SDL_Event.button.which #} p (fromIntegral m)
  {#set SDL_Event.button.button #} p (fromIntegral wb)
  {#set SDL_Event.button.state #} p (enumToC s)
  {#set SDL_Event.button.x #} p (fromIntegral x')
  {#set SDL_Event.button.y #} p (fromIntegral y')

setJButton :: (Int32,Word8,ButtonState) -> EventPtr -> IO ()
setJButton (j,wb,bs) p = do
  {#set SDL_Event.jbutton.which #} p (fromIntegral j)
  {#set SDL_Event.jbutton.button #} p (fromIntegral wb)
  {#set SDL_Event.jbutton.state #} p (enumToC bs)

setCButtonEv :: (Int32,Word8,ButtonState) -> EventPtr -> IO ()
setCButtonEv (wc, wbc, c) p = do
  {#set SDL_Event.cbutton.which#} p (fromIntegral wc)
  {#set SDL_Event.cbutton.button#} p (fromIntegral wbc)
  {#set SDL_Event.cbutton.state#} p (enumToC c)

setFingerEv ::
  TouchID -> FingerID ->
  Float -> Float -> Float -> Float -> Float -> EventPtr -> IO ()
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
      SdlControllerbuttonup ->
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

  poke p ev = do
    let setType t = {#set SDL_Event.common.type #} p (enumToC t)
    {#set SDL_Event.common.timestamp #} p (fromIntegral . timestamp $ ev)
    case ev of
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
          {#set SDL_Event.edit.start#} p (fromIntegral s)
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
          {#set SDL_Event.motion.which#} p (fromIntegral m)
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
        xScrol = xs,
        yScrol = ys} -> do
          setType SdlMousewheel
          {#set SDL_Event.wheel.windowID#} p (fromIntegral w)
          {#set SDL_Event.wheel.which#} p (fromIntegral m)
          {#set SDL_Event.wheel.x#} p (fromIntegral xs)
          {#set SDL_Event.wheel.y#} p (fromIntegral ys)
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
        xRel = xr,
        yRel = yr} -> do
          setType SdlJoyaxismotion
          {#set SDL_Event.jball.which#} p (fromIntegral j)
          {#set SDL_Event.jball.ball#} p (fromIntegral b)
          {#set SDL_Event.jball.xrel#} p (fromIntegral xr)
          {#set SDL_Event.jball.yrel#} p (fromIntegral yr)
      JoyHatMotion{
        whichJS = j,
        hat = h,
        pos = xr} -> do
          setType SdlJoyhatmotion
          {#set SDL_Event.jhat.which#} p (fromIntegral j)
          {#set SDL_Event.jhat.hat#} p (fromIntegral h)
          {#set SDL_Event.jhat.value#} p (enumToC xr)
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
            (realToFrac yc)
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
