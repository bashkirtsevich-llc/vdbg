unit qtKeys;

interface

uses qt;

const
  raw_to_bochs: array[1..101] of integer = (
    Key_Agrave,
    Key_0,                 (*1*)
    Key_1,
    Key_2,
    Key_3,
    Key_4,
    Key_5,
    Key_6,
    Key_7,
    Key_8,
    Key_9,
    Key_Minus,
    Key_Equal,
    Key_BACKSLASH,
    0,
    Key_Q,
    Key_W,
    Key_E,
    Key_R,
    Key_T,                (*20*)
    Key_Y,
    Key_U,
    Key_I,
    Key_O,
    Key_P,
    Key_BRACKETLEFT,
    Key_BRACKETRIGHT,
    0,
    Key_End,
    Key_DOWN,         (*30*)
    Key_PAGEDOWN,
    Key_A,
    Key_S,
    Key_D,
    Key_F,
    Key_G,
    Key_H,
    Key_J,
    Key_K,
    Key_L, 0, 0, 0, 0, 0, 0, 0,               (*40*)
          {Key_SEMICOLON,
            Key_QuoteLeft,
            0,
            0,
            Key_LEFT,
            Key_KP_5,
            Key_KP_RIGHT,}
    0,
    Key_Z,
    Key_X,               (*50*)
    Key_C,
    Key_V,
    Key_B,
    Key_N,
    Key_M,
    Key_COMMA,
    Key_PERIOD,
    Key_SLASH,
    0, 0, 0, 0, 0,
            {Key_KP_INSERT,          (*60*)
            Key_KP_HOME,
            Key_KP_UP,
            Key_KP_PAGE_UP,}
    Key_SPACE,
    Key_BACKSPACE,
    Key_TAB, 0,
    {Key_KP_ENTER,}
    Key_ENTER,
    Key_ESCape,
    Key_DELETE,          (*70*)
    0,
    0,
    0, 0,
    {Key_KP_SUBTRACT,}
    0,
    Key_UP,
    Key_DOWN,
    Key_RIGHT,
    Key_LEFT,
    Key_F1,              (*80*)
    Key_F2,
    Key_F3,
    Key_F4,
    Key_F5,
    Key_F6,
    Key_F7,
    Key_F8,
    Key_F9,
    Key_F10,
    Key_NUMLOCK,          (*90*)
    0, 0, 0, 0,
            {Key_KP_DIVIDE,
            Key_KP_MULTIPLY,
            Key_KP_ADD,}
    0,
    Key_SHIFT,
    Key_SHIFT,
    Key_CAPSLOCK,
    Key_Control,
    Key_ALT,           (*100*)
    Key_ALT
    );

implementation

end.
