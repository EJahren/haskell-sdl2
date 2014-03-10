module Graphics.UI.SDL2.Keycode
 (module Graphics.UI.SDL2.Scancode,
  Keycode(..),
  Modifier(..)) where
import Foreign
import Foreign.C

{# import Graphics.UI.SDL2.Scancode #}

#include <SDL2/SDL_keycode.h>

{#context lib = "SDL2" prefix="SDL"#}

{#enum SDL_Keymod as Modifier {underscoreToCase} deriving (Eq,Ord,Show) #}

{#enum define Keycode
  {
    SDLK_UNKNOWN as Key_Unknown,
    SDLK_RETURN as Key_Return,
    SDLK_ESCAPE as Key_Escape,
    SDLK_BACKSPACE as KeyBackspace,
    SDLK_TAB as Key_Tab,
    SDLK_SPACE as Key_Space,
    SDLK_EXCLAIM as Key_Exclaim,
    SDLK_QUOTEDBL as Key_QuotedBL,
    SDLK_HASH as Key_Hash,
    SDLK_PERCENT as Key_Precent,
    SDLK_DOLLAR as Key_Dollar,
    SDLK_AMPERSAND as Key_Ampersand,
    SDLK_QUOTE as Key_Quote,
    SDLK_LEFTPAREN as Key_Leftparen,
    SDLK_RIGHTPAREN as Key_Rightparen,
    SDLK_ASTERISK as Key_Asterisk,
    SDLK_PLUS as Key_Plus,
    SDLK_COMMA as Key_Comma,
    SDLK_MINUS as Key_Minus,
    SDLK_PERIOD as Key_Preiod,
    SDLK_SLASH as Key_Slash,
    SDLK_0 as Key_0,
    SDLK_1 as Key_1,
    SDLK_2 as Key_2,
    SDLK_3 as Key_3,
    SDLK_4 as Key_4,
    SDLK_5 as Key_5,
    SDLK_6 as Key_6,
    SDLK_7 as Key_7,
    SDLK_8 as Key_8,
    SDLK_9 as Key_9,
    SDLK_COLON as Key_Colon,
    SDLK_SEMICOLON as Key_Semicolon,
    SDLK_LESS as Key_Less,
    SDLK_EQUALS as Key_Equals,
    SDLK_GREATER as Key_Greater,
    SDLK_QUESTION as Key_Question,
    SDLK_AT as Key_At,
    SDLK_LEFTBRACKET as Key_Leftbracket,
    SDLK_BACKSLASH as Key_Backslash,
    SDLK_RIGHTBRACKET as Key_Rightbracket,
    SDLK_CARET as Key_Caret,
    SDLK_UNDERSCORE as Key_Underscore,
    SDLK_BACKQUOTE as Key_Backquote,
    SDLK_a as Key_A,
    SDLK_b as Key_B,
    SDLK_c as Key_C,
    SDLK_d as Key_D,
    SDLK_e as Key_E,
    SDLK_f as Key_F,
    SDLK_g as Key_G,
    SDLK_h as Key_H,
    SDLK_i as Key_I,
    SDLK_j as Key_J,
    SDLK_k as Key_K,
    SDLK_l as Key_L,
    SDLK_m as Key_M,
    SDLK_n as Key_N,
    SDLK_o as Key_O,
    SDLK_p as Key_P,
    SDLK_q as Key_Q,
    SDLK_r as Key_R,
    SDLK_s as Key_S,
    SDLK_t as Key_T, 
    SDLK_u as Key_U,
    SDLK_v as Key_V,
    SDLK_w as Key_W,
    SDLK_x as Key_X,
    SDLK_y as Key_Y,
    SDLK_z as Key_Z,
    SDLK_CAPSLOCK as Key_Capslock,
    SDLK_F1 as Key_F1,
    SDLK_F2 as Key_F2,
    SDLK_F3 as Key_F3,
    SDLK_F4 as Key_F4,
    SDLK_F5 as Key_F5,
    SDLK_F6 as Key_F6,
    SDLK_F7 as Key_F7,
    SDLK_F8 as Key_F8,
    SDLK_F9 as Key_F9,
    SDLK_F10 as Key_F10,
    SDLK_F11 as Key_F11,
    SDLK_F12 as Key_F12,
    SDLK_PRINTSCREEN as Key_Printscreen,
    SDLK_SCROLLLOCK as Key_Scrolllock,
    SDLK_PAUSE as Key_Pause,
    SDLK_INSERT as Key_Insert,
    SDLK_HOME as Key_Home,
    SDLK_PAGEUP as Key_Pageup,
    SDLK_DELETE as Key_Delete,
    SDLK_END as Key_End,
    SDLK_PAGEDOWN as Key_Pagedown,
    SDLK_RIGHT as Key_Right,
    SDLK_LEFT as Key_Left,
    SDLK_DOWN as Key_Down,
    SDLK_UP as Key_Up,
    SDLK_NUMLOCKCLEAR as Key_NumlockClear,
    SDLK_KP_DIVIDE as Key_KPDivide,
    SDLK_KP_MULTIPLY as Key_KPMultiply,
    SDLK_KP_MINUS as Key_KPMinus,
    SDLK_KP_PLUS as Key_KPPlus,
    SDLK_KP_ENTER as Key_KPEnter,
    SDLK_KP_1 as Key_KP1,
    SDLK_KP_2 as Key_KP2,
    SDLK_KP_3 as Key_KP3,
    SDLK_KP_4 as Key_KP4,
    SDLK_KP_5 as Key_KP5,
    SDLK_KP_6 as Key_KP6,
    SDLK_KP_7 as Key_KP7,
    SDLK_KP_8 as Key_KP8,
    SDLK_KP_9 as Key_KP9,
    SDLK_KP_0 as Key_KP0,
    SDLK_KP_PERIOD as KeyPeriod,
    SDLK_APPLICATION as Key_Application,
    SDLK_POWER as Key_Power,
    SDLK_KP_EQUALS as Key_KP_Equals,
    SDLK_F13 as Key_F13,
    SDLK_F14 as Key_F14,
    SDLK_F15 as Key_F15,
    SDLK_F16 as Key_F16,
    SDLK_F17 as Key_F17,
    SDLK_F18 as Key_F18,
    SDLK_F19 as Key_F19,
    SDLK_F20 as Key_F20,
    SDLK_F21 as Key_F21,
    SDLK_F22 as Key_F22,
    SDLK_F23 as Key_F23,
    SDLK_F24 as Key_F24,
    SDLK_EXECUTE as Key_Execute,
    SDLK_HELP as Key_Help,
    SDLK_MENU as Key_Menu,
    SDLK_SELECT as Key_Select,
    SDLK_STOP as Key_Stop,
    SDLK_AGAIN as Key_Again,
    SDLK_UNDO as Key_Undo,
    SDLK_CUT as Key_Cut,
    SDLK_COPY as Key_Copy,
    SDLK_PASTE as Key_Paste,
    SDLK_FIND as Key_Find,
    SDLK_MUTE as Key_Mute,
    SDLK_VOLUMEUP as Key_VolumeUp,
    SDLK_VOLUMEDOWN as Key_VolumeDown,
    SDLK_KP_COMMA as Key_KPComma,
    SDLK_KP_EQUALSAS400 as Key_KPEqualsAS400,
    SDLK_ALTERASE as Key_AltErase,
    SDLK_SYSREQ as Key_SysReq,
    SDLK_CANCEL as Key_Cancel,
    SDLK_CLEAR as Key_Clear,
    SDLK_PRIOR as Key_Prior,
    SDLK_RETURN2 as Key_Return2,
    SDLK_SEPARATOR as Key_Separator,
    SDLK_OUT as Key_Out,
    SDLK_OPER as Key_Oper,
    SDLK_CLEARAGAIN as Key_ClearAgain,
    SDLK_CRSEL as Key_Crsel,
    SDLK_EXSEL as Key_Exsel,
    SDLK_KP_00 as Key_KP00,
    SDLK_KP_000 as Key_KP000,
    SDLK_THOUSANDSSEPARATOR as Key_ThousandSeparator,
    SDLK_DECIMALSEPARATOR as Key_DecimalSeparator,
    SDLK_CURRENCYUNIT as Key_CurrencyUnit,
    SDLK_CURRENCYSUBUNIT as Key_CurrencySubunit,
    SDLK_KP_LEFTPAREN as Key_KPLeftparen,
    SDLK_KP_RIGHTPAREN as Key_KPRightparen,
    SDLK_KP_LEFTBRACE as Key_KPLeftBrace,
    SDLK_KP_RIGHTBRACE as Key_KPRightBrace,
    SDLK_KP_TAB as Key_KPTab,
    SDLK_KP_BACKSPACE as Key_KPBackspace,
    SDLK_KP_A as Key_KPA,
    SDLK_KP_B as Key_KPB,
    SDLK_KP_C as Key_KPC,
    SDLK_KP_D as Key_KPD,
    SDLK_KP_E as Key_KPE,
    SDLK_KP_F as Key_KPF,
    SDLK_KP_XOR as Key_KPXor,
    SDLK_KP_POWER as Key_KPPower,
    SDLK_KP_PERCENT as Key_KPPercent,
    SDLK_KP_LESS as Key_KPLess,
    SDLK_KP_GREATER as Key_KPGreater,
    SDLK_KP_AMPERSAND as Key_KPAmpersand,
    SDLK_KP_DBLAMPERSAND as Key_KPDBLAmpersand,
    SDLK_KP_VERTICALBAR as Key_KPVerticalbar,
    SDLK_KP_DBLVERTICALBAR as Key_DBLVerticalbar,
    SDLK_KP_COLON as Key_KPColon,
    SDLK_KP_HASH as Key_KPHash,
    SDLK_KP_SPACE as Key_KPSpace,
    SDLK_KP_AT as Key_KPAt,
    SDLK_KP_EXCLAM as Key_KPExclam,
    SDLK_KP_MEMSTORE as Key_KPMemStore,
    SDLK_KP_MEMRECALL as Key_KPMemRecall,
    SDLK_KP_MEMCLEAR as Key_KPMemClear,
    SDLK_KP_MEMADD as Key_KPMemAdd,
    SDLK_KP_MEMSUBTRACT as Key_KPMemSubtract,
    SDLK_KP_MEMMULTIPLY as Key_KPMemMultiply,
    SDLK_KP_MEMDIVIDE as Key_KPMemDivide,
    SDLK_KP_PLUSMINUS as Key_KPPlusMinus,
    SDLK_KP_CLEAR as Key_KPClear,
    SDLK_KP_CLEARENTRY as Key_KPClearEntry,
    SDLK_KP_BINARY as Key_KPBinary,
    SDLK_KP_OCTAL as Key_KPOctal,
    SDLK_KP_DECIMAL as Key_KPDecimal,
    SDLK_KP_HEXADECIMAL as Key_KPHexadecimal,
    SDLK_LCTRL as Key_LCtrl,
    SDLK_LSHIFT as Key_LShift,
    SDLK_LALT as Key_LAlt,
    SDLK_LGUI as Key_LGui,
    SDLK_RCTRL as Key_RCtrl,
    SDLK_RSHIFT as Key_RShift,
    SDLK_RALT as Key_RAlt,
    SDLK_RGUI as Key_RGui,
    SDLK_MODE as Key_Mode,
    SDLK_AUDIONEXT as Key_AudioNext,
    SDLK_AUDIOPREV as Key_AudioPrev,
    SDLK_AUDIOSTOP as Key_AudioStop,
    SDLK_AUDIOPLAY as Key_AudioPlay,
    SDLK_AUDIOMUTE as Key_AuidoMute,
    SDLK_MEDIASELECT as Key_MediaSelect,
    SDLK_WWW as Key_WWW,
    SDLK_MAIL as Key_Mail,
    SDLK_CALCULATOR as Key_Calculator,
    SDLK_COMPUTER as Key_Computer,
    SDLK_AC_SEARCH as Key_ACSearch,
    SDLK_AC_HOME as Key_ACHome,
    SDLK_AC_BACK as Key_ACBack,
    SDLK_AC_FORWARD as Key_ACForward,
    SDLK_AC_STOP as Key_ACStop,
    SDLK_AC_REFRESH as Key_ACRefresh,
    SDLK_AC_BOOKMARKS as Key_ACBookmarks,
    SDLK_BRIGHTNESSDOWN as Key_BrightnessDown,
    SDLK_BRIGHTNESSUP as Key_BrightnessUp,
    SDLK_DISPLAYSWITCH as Key_DisplaySwitch,
    SDLK_KBDILLUMTOGGLE as Key_KBDillumToggle,
    SDLK_KBDILLUMDOWN as Key_KBDillumDown,
    SDLK_KBDILLUMUP as Key_KBDillumUp,
    SDLK_EJECT as Key_Eject,
    SDLK_SLEEP as Key_Sleep
   }
   deriving (Eq,Ord,Show) #}
