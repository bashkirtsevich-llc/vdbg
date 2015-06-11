unit Scd;

interface

uses KeyMap;

type
  scancode = record
    make: PChar;
    brek: PChar;
  end;


const
  translation8042: array[0..255] of char = (
    #$ff, #$43, #$41, #$3f, #$3d, #$3b, #$3c, #$58, #$64, #$44, #$42,
    #$40, #$3e, #$0f, #$29, #$59,
    #$65, #$38, #$2a, #$70, #$1d, #$10, #$02, #$5a, #$66, #$71, #$2c,
    #$1f, #$1e, #$11, #$03, #$5b,
    #$67, #$2e, #$2d, #$20, #$12, #$05, #$04, #$5c, #$68, #$39, #$2f,
    #$21, #$14, #$13, #$06, #$5d,
    #$69, #$31, #$30, #$23, #$22, #$15, #$07, #$5e, #$6a, #$72, #$32,
    #$24, #$16, #$08, #$09, #$5f,
    #$6b, #$33, #$25, #$17, #$18, #$0b, #$0a, #$60, #$6c, #$34, #$35,
    #$26, #$27, #$19, #$0c, #$61,
    #$6d, #$73, #$28, #$74, #$1a, #$0d, #$62, #$6e, #$3a, #$36, #$1c,
    #$1b, #$75, #$2b, #$63, #$76,
    #$55, #$56, #$77, #$78, #$79, #$7a, #$0e, #$7b, #$7c, #$4f, #$7d,
    #$4b, #$47, #$7e, #$7f, #$6f,
    #$52, #$53, #$50, #$4c, #$4d, #$48, #$01, #$45, #$57, #$4e, #$51,
    #$4a, #$37, #$49, #$46, #$54,
    #$80, #$81, #$82, #$41, #$54, #$85, #$86, #$87, #$88, #$89, #$8a,
    #$8b, #$8c, #$8d, #$8e, #$8f,
    #$90, #$91, #$92, #$93, #$94, #$95, #$96, #$97, #$98, #$99, #$9a,
    #$9b, #$9c, #$9d, #$9e, #$9f,
    #$a0, #$a1, #$a2, #$a3, #$a4, #$a5, #$a6, #$a7, #$a8, #$a9, #$aa,
    #$ab, #$ac, #$ad, #$ae, #$af,
    #$b0, #$b1, #$b2, #$b3, #$b4, #$b5, #$b6, #$b7, #$b8, #$b9, #$ba,
    #$bb, #$bc, #$bd, #$be, #$bf,
    #$c0, #$c1, #$c2, #$c3, #$c4, #$c5, #$c6, #$c7, #$c8, #$c9, #$ca,
    #$cb, #$cc, #$cd, #$ce, #$cf,
    #$d0, #$d1, #$d2, #$d3, #$d4, #$d5, #$d6, #$d7, #$d8, #$d9, #$da,
    #$db, #$dc, #$dd, #$de, #$df,
    #$e0, #$e1, #$e2, #$e3, #$e4, #$e5, #$e6, #$e7, #$e8, #$e9, #$ea,
    #$eb, #$ec, #$ed, #$ee, #$ef,
    #$f0, #$f1, #$f2, #$f3, #$f4, #$f5, #$f6, #$f7, #$f8, #$f9, #$fa,
    #$fb, #$fc, #$fd, #$fe, #$ff
    );

  scancodes: array[0..BX_KEY_NBKEYS, 0..2] of scancode =
    (
    ( // BX_KEY_CTRL_L ( ibm 58)
    (make: '\x1D'; brek: '\x9D'),
    (make: '\x14'; brek: '\xF0\x14'),
    (make: '\x11'; brek: '\xF0\x11')
    ),

    ( // BX_KEY_SHIFT_L ( ibm 44)
    (make: '\x2A'; brek: '\xAA'),
    (make: '\x12'; brek: '\xF0\x12'),
    (make: '\x12'; brek: '\xF0\x12')
    ),

    ( // BX_KEY_F1 ( ibm 112 )
    (make: '\x3B'; brek: '\xBB'),
    (make: '\x05'; brek: '\xF0\x05'),
    (make: '\x07'; brek: '\xF0\x07')
    ),

    ( // BX_KEY_F2 ( ibm 113 ) 
    (make: '\x3C'; brek: '\xBC'),
    (make: '\x06'; brek: '\xF0\x06'),
    (make: '\x0F'; brek: '\xF0\x0F')
    ),

    ( // BX_KEY_F3 ( ibm 114 )
    (make: '\x3D'; brek: '\xBD'),
    (make: '\x04'; brek: '\xF0\x04'),
    (make: '\x17'; brek: '\xF0\x17')
    ),

    ( // BX_KEY_F4 ( ibm 115 )
    (make: '\x3E'; brek: '\xBE'),
    (make: '\x0C'; brek: '\xF0\x0C'),
    (make: '\x1F'; brek: '\xF0\x1F')
    ),

    ( // BX_KEY_F5 ( ibm 116 )
    (make: '\x3F'; brek: '\xBF'),
    (make: '\x03'; brek: '\xF0\x03'),
    (make: '\x27'; brek: '\xF0\x27')
    ),

    ( // BX_KEY_F6 ( ibm 117 )
    (make: '\x40'; brek: '\xC0'),
    (make: '\x0B'; brek: '\xF0\x0B'),
    (make: '\x2F'; brek: '\xF0\x2F')
    ),

    ( // BX_KEY_F7 ( ibm 118 )
    (make: '\x41'; brek: '\xC1'),
    (make: '\x83'; brek: '\xF0\x83'),
    (make: '\x37'; brek: '\xF0\x37')),

    ( // BX_KEY_F8 ( ibm 119 )
    (make: '\x42'; brek: '\xC2'),
    (make: '\x0A'; brek: '\xF0\x0A'),
    (make: '\x3F'; brek: '\xF0\x3F')
    ),

    ( // BX_KEY_F9 ( ibm 120 )
    (make: '\x43'; brek: '\xC3'),
    (make: '\x01'; brek: '\xF0\x01'),
    (make: '\x47'; brek: '\xF0\x47')
    ),

    ( // BX_KEY_F10 ( ibm 121 )
    (make: '\x44'; brek: '\xC4'),
    (make: '\x09'; brek: '\xF0\x09'),
    (make: '\x4F'; brek: '\xF0\x4F')
    ),

    ( // BX_KEY_F11 ( ibm 122 )
    (make: '\x57'; brek: '\xD7'),
    (make: '\x78'; brek: '\xF0\x78'),
    (make: '\x56'; brek: '\xF0\x56')
    ),

    ( // BX_KEY_F12 ( ibm 123 )
    (make: '\x58'; brek: '\xD8'),
    (make: '\x07'; brek: '\xF0\x07'),
    (make: '\x5E'; brek: '\xF0\x5E')
    ),

    ( // BX_KEY_CTRL_R ( ibm 64 )
    (make: '\xE0\x1D'; brek: '\xE0\x9D'),
    (make: '\xE0\x14'; brek: '\xE0\xF0\x14'),
    (make: '\x58'; brek: '\xF0x58')
    ),

    ( // BX_KEY_SHIFT_R ( ibm 57 )
    (make: '\x36'; brek: '\xB6'),
    (make: '\x59'; brek: '\xF0\x59'),
    (make: '\x59'; brek: '\xF0\x59')
    ),

    ( // BX_KEY_CAPS_LOCK ( ibm 30 )
    (make: '\x3A'; brek: '\xBA'),
    (make: '\x58'; brek: '\xF0\x58'),
    (make: '\x14'; brek: '\xF0\x14')
    ),

    ( // BX_KEY_NUM_LOCK ( ibm 90 )
    (make: '\x45'; brek: '\xC5'),
    (make: '\x77'; brek: '\xF0\x77'),
    (make: '\x76'; brek: '\xF0\x76')
    ),

    ( // BX_KEY_ALT_L ( ibm 60 )
    (make: '\x38'; brek: '\xB8'),
    (make: '\x11'; brek: '\xF0\x11'),
    (make: '\x19'; brek: '\xF0\x19')
    ),

    ( // BX_KEY_ALT_R ( ibm 62 )
    (make: '\xE0\x38'; brek: '\xE0\xB8'),
    (make: '\xE0\x11'; brek: '\xE0\xF0\x11'),
    (make: '\x39'; brek: '\xF0\x39')
    ),

    ( // BX_KEY_A ( ibm 31 )
    (make: '\x1E'; brek: '\x9E'),
    (make: '\x1C'; brek: '\xF0\x1C'),
    (make: '\x1C'; brek: '\xF0\x1C')
    ),

    ( // BX_KEY_B ( ibm 50 )
    (make: '\x30'; brek: '\xB0'),
    (make: '\x32'; brek: '\xF0\x32'),
    (make: '\x32'; brek: '\xF0\x32')
    ),

    ( // BX_KEY_C ( ibm 48 )
    (make: '\x2E'; brek: '\xAE'),
    (make: '\x21'; brek: '\xF0\x21'),
    (make: '\x21'; brek: '\xF0\x21')
    ),

    ( // BX_KEY_D ( ibm 33 )
    (make: '\x20'; brek: '\xA0'),
    (make: '\x23'; brek: '\xF0\x23'),
    (make: '\x23'; brek: '\xF0\x23')
    ),

    ( // BX_KEY_E ( ibm 19 )
    (make: '\x12'; brek: '\x92'),
    (make: '\x24'; brek: '\xF0\x24'),
    (make: '\x24'; brek: '\xF0\x24')
    ),

    ( // BX_KEY_F ( ibm 34 )
    (make: '\x21'; brek: '\xA1'),
    (make: '\x2B'; brek: '\xF0\x2B'),
    (make: '\x2B'; brek: '\xF0\x2B')
    ),

    ( // BX_KEY_G ( ibm 35 )
    (make: '\x22'; brek: '\xA2'),
    (make: '\x34'; brek: '\xF0\x34'),
    (make: '\x34'; brek: '\xF0\x34')
    ),

    ( // BX_KEY_H ( ibm 36 )
    (make: '\x23'; brek: '\xA3'),
    (make: '\x33'; brek: '\xF0\x33'),
    (make: '\x33'; brek: '\xF0\x33')
    ),

    ( // BX_KEY_I ( ibm 24 )
    (make: '\x17'; brek: '\x97'),
    (make: '\x43'; brek: '\xF0\x43'),
    (make: '\x43'; brek: '\xF0\x43')
    ),

    ( // BX_KEY_J ( ibm 37 )
    (make: '\x24'; brek: '\xA4'),
    (make: '\x3B'; brek: '\xF0\x3B'),
    (make: '\x3B'; brek: '\xF0\x3B')
    ),

    ( // BX_KEY_K ( ibm 38 )
    (make: '\x25'; brek: '\xA5'),
    (make: '\x42'; brek: '\xF0\x42'),
    (make: '\x42'; brek: '\xF0\x42')
    ),

    ( // BX_KEY_L ( ibm 39 )
    (make: '\x26'; brek: '\xA6'),
    (make: '\x4B'; brek: '\xF0\x4B'),
    (make: '\x4B'; brek: '\xF0\x4B')
    ),

    ( // BX_KEY_M ( ibm 52 )
    (make: '\x32'; brek: '\xB2'),
    (make: '\x3A'; brek: '\xF0\x3A'),
    (make: '\x3A'; brek: '\xF0\x3A')
    ),

    ( // BX_KEY_N ( ibm 51 )
    (make: '\x31'; brek: '\xB1'),
    (make: '\x31'; brek: '\xF0\x31'),
    (make: '\x31'; brek: '\xF0\x31')
    ),

    ( // BX_KEY_O ( ibm 25 )
    (make: '\x18'; brek: '\x98'),
    (make: '\x44'; brek: '\xF0\x44'),
    (make: '\x44'; brek: '\xF0\x44')
    ),

    ( // BX_KEY_P ( ibm 26 )
    (make: '\x19'; brek: '\x99'),
    (make: '\x4D'; brek: '\xF0\x4D'),
    (make: '\x4D'; brek: '\xF0\x4D')
    ),

    ( // BX_KEY_Q ( ibm 17 )
    (make: '\x10'; brek: '\x90'),
    (make: '\x15'; brek: '\xF0\x15'),
    (make: '\x15'; brek: '\xF0\x15')
    ),

    ( // BX_KEY_R ( ibm 20 )
    (make: '\x13'; brek: '\x93'),
    (make: '\x2D'; brek: '\xF0\x2D'),
    (make: '\x2D'; brek: '\xF0\x2D')
    ),

    ( // BX_KEY_S ( ibm 32 )
    (make: '\x1F'; brek: '\x9F'),
    (make: '\x1B'; brek: '\xF0\x1B'),
    (make: '\x1B'; brek: '\xF0\x1B')
    ),

    ( // BX_KEY_T ( ibm 21 )
    (make: '\x14'; brek: '\x94'),
    (make: '\x2C'; brek: '\xF0\x2C'),
    (make: '\x2C'; brek: '\xF0\x2C')
    ),

    ( // BX_KEY_U ( ibm 23 )
    (make: '\x16'; brek: '\x96'),
    (make: '\x3C'; brek: '\xF0\x3C'),
    (make: '\x3C'; brek: '\xF0\x3C')
    ),

    ( // BX_KEY_V ( ibm 49 )
    (make: '\x2F'; brek: '\xAF'),
    (make: '\x2A'; brek: '\xF0\x2A'),
    (make: '\x2A'; brek: '\xF0\x2A')
    ),

    ( // BX_KEY_W ( ibm 18 )
    (make: '\x11'; brek: '\x91'),
    (make: '\x1D'; brek: '\xF0\x1D'),
    (make: '\x1D'; brek: '\xF0\x1D')
    ),

    ( // BX_KEY_X ( ibm 47 )
    (make: '\x2D'; brek: '\xAD'),
    (make: '\x22'; brek: '\xF0\x22'),
    (make: '\x22'; brek: '\xF0\x22')
    ),

    ( // BX_KEY_Y ( ibm 22 )
    (make: '\x15'; brek: '\x95'),
    (make: '\x35'; brek: '\xF0\x35'),
    (make: '\x35'; brek: '\xF0\x35')
    ),

    ( // BX_KEY_Z ( ibm 46 )
    (make: '\x2C'; brek: '\xAC'),
    (make: '\x1A'; brek: '\xF0\x1A'),
    (make: '\x1A'; brek: '\xF0\x1A')
    ),

    ( // BX_KEY_0 ( ibm 11 )
    (make: '\x0B'; brek: '\x8B'),
    (make: '\x45'; brek: '\xF0\x45'),
    (make: '\x45'; brek: '\xF0\x45')
    ),

    ( // BX_KEY_1 ( ibm 2 )
    (make: '\x02'; brek: '\x82'),
    (make: '\x16'; brek: '\xF0\x16'),
    (make: '\x16'; brek: '\xF0\x16')
    ),

    ( // BX_KEY_2 ( ibm 3 )
    (make: '\x03'; brek: '\x83'),
    (make: '\x1E'; brek: '\xF0\x1E'),
    (make: '\x1E'; brek: '\xF0\x1E')
    ),

    ( // BX_KEY_3 ( ibm 4 )
    (make: '\x04'; brek: '\x84'),
    (make: '\x26'; brek: '\xF0\x26'),
    (make: '\x26'; brek: '\xF0\x26')
    ),

    ( // BX_KEY_4 ( ibm 5 )
    (make: '\x05'; brek: '\x85'),
    (make: '\x25'; brek: '\xF0\x25'),
    (make: '\x25'; brek: '\xF0\x25')
    ),

    ( // BX_KEY_5 ( ibm 6 )
    (make: '\x06'; brek: '\x86'),
    (make: '\x2E'; brek: '\xF0\x2E'),
    (make: '\x2E'; brek: '\xF0\x2E')
    ),

    ( // BX_KEY_6 ( ibm 7 )
    (make: '\x07'; brek: '\x87'),
    (make: '\x36'; brek: '\xF0\x36'),
    (make: '\x36'; brek: '\xF0\x36')
    ),

    ( // BX_KEY_7 ( ibm 8 )
    (make: '\x08'; brek: '\x88'),
    (make: '\x3D'; brek: '\xF0\x3D'),
    (make: '\x3D'; brek: '\xF0\x3D')
    ),

    ( // BX_KEY_8 ( ibm 9 )
    (make: '\x09'; brek: '\x89'),
    (make: '\x3E'; brek: '\xF0\x3E'),
    (make: '\x3E'; brek: '\xF0\x3E')
    ),

    ( // BX_KEY_9 ( ibm 10 )
    (make: '\x0A'; brek: '\x8A'),
    (make: '\x46'; brek: '\xF0\x46'),
    (make: '\x46'; brek: '\xF0\x46')
    ),

    ( // BX_KEY_ESC ( ibm 110 )
    (make: '\x01'; brek: '\x81'),
    (make: '\x76'; brek: '\xF0\x76'),
    (make: '\x08'; brek: '\xF0\x08')
    ),

    ( // BX_KEY_SPACE ( ibm 61 )
    (make: '\x39'; brek: '\xB9'),
    (make: '\x29'; brek: '\xF0\x29'),
    (make: '\x29'; brek: '\xF0\x29')
    ),
    ( // BX_KEY_SINGLE_QUOTE ( ibm 41 )
    (make: '\x28'; brek: '\xA8'),
    (make: '\x52'; brek: '\xF0\x52'),
    (make: '\x52'; brek: '\xF0\x52')
    ),

    ( // BX_KEY_COMMA ( ibm 53 )
    (make: '\x33'; brek: '\xB3'),
    (make: '\x41'; brek: '\xF0\x41'),
    (make: '\x41'; brek: '\xF0\x41')
    ),

    ( // BX_KEY_PERIOD ( ibm 54 )
    (make: '\x34'; brek: '\xB4'),
    (make: '\x49'; brek: '\xF0\x49'),
    (make: '\x49'; brek: '\xF0\x49')
    ),

    ( // BX_KEY_SLASH ( ibm 55 )
    (make: '\x35'; brek: '\xB5'),
    (make: '\x4A'; brek: '\xF0\x4A'),
    (make: '\x4A'; brek: '\xF0\x4A')
    ),

    ( // BX_KEY_SEMICOLON ( ibm 40 )
    (make: '\x27'; brek: '\xA7'),
    (make: '\x4C'; brek: '\xF0\x4C'),
    (make: '\x4C'; brek: '\xF0\x4C')
    ),

    ( // BX_KEY_EQUALS ( ibm 13 )
    (make: '\x0D'; brek: '\x8D'),
    (make: '\x55'; brek: '\xF0\x55'),
    (make: '\x55'; brek: '\xF0\x55')
    ),

    ( // BX_KEY_LEFT_BRACKET ( ibm 27 )
    (make: '\x1A'; brek: '\x9A'),
    (make: '\x54'; brek: '\xF0\x54'),
    (make: '\x54'; brek: '\xF0\x54')
    ),

    ( // BX_KEY_BACKSLASH ( ibm 42, 29)
    (make: '\x2B'; brek: '\xAB'),
    (make: '\x5D'; brek: '\xF0\x5D'),
    (make: '\x53'; brek: '\xF0\x53')
    ),

    ( // BX_KEY_RIGHT_BRACKET ( ibm 28 )
    (make: '\x1B'; brek: '\x9B'),
    (make: '\x5B'; brek: '\xF0\x5B'),
    (make: '\x5B'; brek: '\xF0\x5B')
    ),

    ( // BX_KEY_MINUS ( ibm 12 )
    (make: '\x0C'; brek: '\x8C'),
    (make: '\x4E'; brek: '\xF0\x4E'),
    (make: '\x4E'; brek: '\xF0\x4E')
    ),

    ( // BX_KEY_GRAVE ( ibm 1 )
    (make: '\x29'; brek: '\xA9'),
    (make: '\x0E'; brek: '\xF0\x0E'),
    (make: '\x0E'; brek: '\xF0\x0E')
    ),
    ( // BX_KEY_BACKSPACE ( ibm 15 )
    (make: '\x0E'; brek: '\x8E'),
    (make: '\x66'; brek: '\xF0\x66'),
    (make: '\x66'; brek: '\xF0\x66')
    ),

    ( // BX_KEY_ENTER ( ibm 43 )
    (make: '\x1C'; brek: '\x9C'),
    (make: '\x5A'; brek: '\xF0\x5A'),
    (make: '\x5A'; brek: '\xF0\x5A')
    ),

    ( // BX_KEY_TAB ( ibm 16 )
    (make: '\x0F'; brek: '\x8F'),
    (make: '\x0D'; brek: '\xF0\x0D'),
    (make: '\x0D'; brek: '\xF0\x0D')
    ),

    ( // BX_KEY_LEFT_BACKSLASH ( ibm 45 )
    (make: '\x56'; brek: '\xD6'),
    (make: '\x61'; brek: '\xF0\x61'),
    (make: '\x13'; brek: '\xF0\x13')
    ),

    ( // BX_KEY_PRINT ( ibm 124 )
    (make: '\xE0\x37'; brek: '\xE0\xB7'),
    (make: '\xE0\x7C'; brek: '\xE0\xF0\x7C'),
    (make: '\x57'; brek: '\xF0\x57')
    ),

    ( // BX_KEY_SCRL_LOCK ( ibm 125 )
    (make: '\x46'; brek: '\xC6'),
    (make: '\x7E'; brek: '\xF0\x7E'),
    (make: '\x5F'; brek: '\xF0\x5F')
    ),

    ( // BX_KEY_PAUSE ( ibm 126 )
    (make: '\xE1\x1D\x45\xE1\x9D\xC5'; brek: ''),
    (make: '\xE1\x14\x77\xE1\xF0\x14\xF0\x77'; brek: ''),
    (make: '\x62'; brek: '\xF0\x62')
    ),

    ( // BX_KEY_INSERT ( ibm 75 )
    (make: '\xE0\x52'; brek: '\xE0\xD2'),
    (make: '\xE0\x70'; brek: '\xE0\xF0\x70'),
    (make: '\x67'; brek: '\xF0\x67')
    ),

    ( // BX_KEY_DELETE ( ibm 76 )
    (make: '\xE0\x53'; brek: '\xE0\xD3'),
    (make: '\xE0\x71'; brek: '\xE0\xF0\x71'),
    (make: '\x64'; brek: '\xF0\x64')
    ),

    ( // BX_KEY_HOME ( ibm 80 )
    (make: '\xE0\x47'; brek: '\xE0\xC7'),
    (make: '\xE0\x6C'; brek: '\xE0\xF0\x6C'),
    (make: '\x6E'; brek: '\xF0\x6E')
    ),

    ( // BX_KEY_END ( ibm 81 )
    (make: '\xE0\x4F'; brek: '\xE0\xCF'),
    (make: '\xE0\x69'; brek: '\xE0\xF0\x69'),
    (make: '\x65'; brek: '\xF0\x65')
    ),

    ( // BX_KEY_PAGE_UP ( ibm 85 )
    (make: '\xE0\x49'; brek: '\xE0\xC9'),
    (make: '\xE0\x7D'; brek: '\xE0\xF0\x7D'),
    (make: '\x6F'; brek: '\xF0\x6F')
    ),

    ( // BX_KEY_PAGE_DOWN ( ibm 86 )
    (make: '\xE0\x51'; brek: '\xE0\xD1'),
    (make: '\xE0\x7A'; brek: '\xE0\xF0\x7A'),
    (make: '\x6D'; brek: '\xF0\x6D')
    ),

    ( // BX_KEY_KP_ADD ( ibm 106 )
    (make: '\x4E'; brek: '\xCE'),
    (make: '\x79'; brek: '\xF0\x79'),
    (make: '\x7C'; brek: '\xF0\x7C')
    ),

    ( // BX_KEY_KP_SUBTRACT ( ibm 105 )
    (make: '\x4A'; brek: '\xCA'),
    (make: '\x7B'; brek: '\xF0\x7B'),
    (make: '\x84'; brek: '\xF0\x84')
    ),

    ( // BX_KEY_KP_END ( ibm 93 )
    (make: '\x4F'; brek: '\xCF'),
    (make: '\x69'; brek: '\xF0\x69'),
    (make: '\x69'; brek: '\xF0\x69')
    ),

    ( // BX_KEY_KP_DOWN ( ibm 98 )
    (make: '\x50'; brek: '\xD0'),
    (make: '\x72'; brek: '\xF0\x72'),
    (make: '\x72'; brek: '\xF0\x72')
    ),

    ( // BX_KEY_KP_PAGE_DOWN ( ibm 103 )
    (make: '\x51'; brek: '\xD1'),
    (make: '\x7A'; brek: '\xF0\x7A'),
    (make: '\x7A'; brek: '\xF0\x7A')
    ),

    ( // BX_KEY_KP_LEFT ( ibm 92 )
    (make: '\x4B'; brek: '\xCB'),
    (make: '\x6B'; brek: '\xF0\x6B'),
    (make: '\x6B'; brek: '\xF0\x6B')
    ),

    ( // BX_KEY_KP_RIGHT ( ibm 102 )
    (make: '\x4D'; brek: '\xCD'),
    (make: '\x74'; brek: '\xF0\x74'),
    (make: '\x74'; brek: '\xF0\x74')
    ),

    ( // BX_KEY_KP_HOME ( ibm 91 )
    (make: '\x47'; brek: '\xC7'),
    (make: '\x6C'; brek: '\xF0\x6C'),
    (make: '\x6C'; brek: '\xF0\x6C')
    ),

    ( // BX_KEY_KP_UP ( ibm 96 )
    (make: '\x48'; brek: '\xC8'),
    (make: '\x75'; brek: '\xF0\x75'),
    (make: '\x75'; brek: '\xF0\x75')
    ),

    ( // BX_KEY_KP_PAGE_UP ( ibm 101 )
    (make: '\x49'; brek: '\xC9'),
    (make: '\x7D'; brek: '\xF0\x7D'),
    (make: '\x7D'; brek: '\xF0\x7D')
    ),

    ( // BX_KEY_KP_INSERT ( ibm 99 )
    (make: '\x52'; brek: '\xD2'),
    (make: '\x70'; brek: '\xF0\x70'),
    (make: '\x70'; brek: '\xF0\x70')
    ),

    ( // BX_KEY_KP_DELETE ( ibm 104 )
    (make: '\x53'; brek: '\xD3'),
    (make: '\x71'; brek: '\xF0\x71'),
    (make: '\x71'; brek: '\xF0\x71')
    ),

    ( // BX_KEY_KP_5 ( ibm 97 )
    (make: '\x4C'; brek: '\xCC'),
    (make: '\x73'; brek: '\xF0\x73'),
    (make: '\x73'; brek: '\xF0\x73')
    ),

    ( // BX_KEY_UP ( ibm 83 )
    (make: '\xE0\x48'; brek: '\xE0\xC8'),
    (make: '\xE0\x75'; brek: '\xE0\xF0\x75'),
    (make: '\x63'; brek: '\xF0\x63')
    ),

    ( // BX_KEY_DOWN ( ibm 84 )
    (make: '\xE0\x50'; brek: '\xE0\xD0'),
    (make: '\xE0\x72'; brek: '\xE0\xF0\x72'),
    (make: '\x60'; brek: '\xF0\x60')
    ),

    ( // BX_KEY_LEFT ( ibm 79 )
    (make: '\xE0\x4B'; brek: '\xE0\xCB'),
    (make: '\xE0\x6B'; brek: '\xE0\xF0\x6B'),
    (make: '\x61'; brek: '\xF0\x61')
    ),

    ( // BX_KEY_RIGHT ( ibm 89 )
    (make: '\xE0\x4D'; brek: '\xE0\xCD'),
    (make: '\xE0\x74'; brek: '\xE0\xF0\x74'),
    (make: '\x6A'; brek: '\xF0\x6A')
    ),

    ( // BX_KEY_KP_ENTER ( ibm 108 )
    (make: '\xE0\x1C'; brek: '\xE0\x9C'),
    (make: '\xE0\x5A'; brek: '\xE0\xF0\x5A'),
    (make: '\x79'; brek: '\xF0\x79')
    ),

    ( // BX_KEY_KP_MULTIPLY ( ibm 100 )
    (make: '\x37'; brek: '\xB7'),
    (make: '\x7C'; brek: '\xF0\x7C'),
    (make: '\x7E'; brek: '\xF0\x7E')
    ),

    ( // BX_KEY_KP_DIVIDE ( ibm 95 )
    (make: '\xE0\x35'; brek: '\xE0\xB5'),
    (make: '\xE0\x4A'; brek: '\xE0\xF0\x4A'),
    (make: '\x77'; brek: '\xF0\x77')
    ),

    ( // BX_KEY_WIN_L
    (make: '\xE0\x5B'; brek: '\xE0\xDB'),
    (make: '\xE0\x1F'; brek: '\xE0\xF0\x1F'),
    (make: '\x8B'; brek: '\xF0\x8B')
    ),

    ( // BX_KEY_WIN_R
    (make: '\xE0\x5C'; brek: '\xE0\xDC'),
    (make: '\xE0\x27'; brek: '\xE0\xF0\x27'),
    (make: '\x8C'; brek: '\xF0\x8C')
    ),

    ( // BX_KEY_MENU
    (make: '\xE0\x5D'; brek: '\xE0\xDD'),
    (make: '\xE0\x2F'; brek: '\xE0\xF0\x2F'),
    (make: '\x8D'; brek: '\xF0\x8D')
    ),

    ( // BX_KEY_ALT_SYSREQ
    (make: '\x54'; brek: '\xD4'),
    (make: '\x84'; brek: '\xF0\x84'),
    (make: '\x57'; brek: '\xF0\x57')
    ),

    ( // BX_KEY_CTRL_BREAK
    (make: '\xE0\x46'; brek: '\xE0\xC6'),
    (make: '\xE0\x7E'; brek: '\xE0\xF0\x7E'),
    (make: '\x62'; brek: '\xF0\x62')
    ),

    ( // BX_KEY_INT_BACK
    (make: '\xE0\x6A'; brek: '\xE0\xEA'),
    (make: '\xE0\x38'; brek: '\xE0\xF0\x38'),
    (make: '\x38'; brek: '\xF0\x38')
    ),

    ( // BX_KEY_INT_FORWARD
    (make: '\xE0\x69'; brek: '\xE0\xE9'),
    (make: '\xE0\x30'; brek: '\xE0\xF0\x30'),
    (make: '\x30'; brek: '\xF0\x30')
    ),

    ( // BX_KEY_INT_STOP
    (make: '\xE0\x68'; brek: '\xE0\xE8'),
    (make: '\xE0\x28'; brek: '\xE0\xF0\x28'),
    (make: '\x28'; brek: '\xF0\x28')
    ),

    ( // BX_KEY_INT_MAIL
    (make: '\xE0\x6C'; brek: '\xE0\xEC'),
    (make: '\xE0\x48'; brek: '\xE0\xF0\x48'),
    (make: '\x48'; brek: '\xF0\x48')
    ),

    ( // BX_KEY_INT_SEARCH
    (make: '\xE0\x65'; brek: '\xE0\xE5'),
    (make: '\xE0\x10'; brek: '\xE0\xF0\x10'),
    (make: '\x10'; brek: '\xF0\x10')
    ),

    ( // BX_KEY_INT_FAV
    (make: '\xE0\x66'; brek: '\xE0\xE6'),
    (make: '\xE0\x18'; brek: '\xE0\xF0\x18'),
    (make: '\x18'; brek: '\xF0\x18')
    ),

    ( // BX_KEY_INT_HOME
    (make: '\xE0\x32'; brek: '\xE0\xB2'),
    (make: '\xE0\x3A'; brek: '\xE0\xF0\x3A'),
    (make: '\x97'; brek: '\xF0\x97')
    ),

    ( // BX_KEY_POWER_MYCOMP
    (make: '\xE0\x6B'; brek: '\xE0\xEB'),
    (make: '\xE0\x40'; brek: '\xE0\xF0\x40'),
    (make: '\x40'; brek: '\xF0\x40')
    ),

    ( // BX_KEY_POWER_CALC
    (make: '\xE0\x21'; brek: '\xE0\xA1'),
    (make: '\xE0\x2B'; brek: '\xE0\xF0\x2B'),
    (make: '\x99'; brek: '\xF0\x99')
    ),

    ( // BX_KEY_POWER_SLEEP
    (make: '\xE0\x5F'; brek: '\xE0\xDF'),
    (make: '\xE0\x3F'; brek: '\xE0\xF0\x3F'),
    (make: '\x7F'; brek: '\xF0\x7F')
    ),

    ( // BX_KEY_POWER_POWER
    (make: '\xE0\x5E'; brek: '\xE0\xDE'),
    (make: '\xE0\x37'; brek: '\xE0\xF0\x37'),
    (make: ''; brek: '')
    ),

    ( // BX_KEY_POWER_WAKE
    (make: '\xE0\x63'; brek: '\xE0\xE3'),
    (make: '\xE0\x5E'; brek: '\xE0\xF0\x5E'),
    (make: ''; brek: '')
    )
    );

implementation

end.
