unit KeyMap;

interface

uses Config, SysUtils, Service, Windows;

const
  BX_KEYMAP_UNKNOWN = $FFFFFFFF;
  BX_KEY_NBKEYS     = 118;
  bx_key_symbol: array[0..BX_KEY_NBKEYS] of string = (
    'BX_KEY_CTRL_L', 'BX_KEY_SHIFT_L', 'BX_KEY_F1',
    'BX_KEY_F2', 'BX_KEY_F3', 'BX_KEY_F4',
    'BX_KEY_F5', 'BX_KEY_F6', 'BX_KEY_F7',
    'BX_KEY_F8', 'BX_KEY_F9', 'BX_KEY_F10',
    'BX_KEY_F11', 'BX_KEY_F12', 'BX_KEY_CTRL_R',
    'BX_KEY_SHIFT_R', 'BX_KEY_CAPS_LOCK', 'BX_KEY_NUM_LOCK',
    'BX_KEY_ALT_L', 'BX_KEY_ALT_R', 'BX_KEY_A',
    'BX_KEY_B', 'BX_KEY_C', 'BX_KEY_D',
    'BX_KEY_E', 'BX_KEY_F', 'BX_KEY_G',
    'BX_KEY_H', 'BX_KEY_I', 'BX_KEY_J',
    'BX_KEY_K', 'BX_KEY_L', 'BX_KEY_M',
    'BX_KEY_N', 'BX_KEY_O', 'BX_KEY_P',
    'BX_KEY_Q', 'BX_KEY_R', 'BX_KEY_S',
    'BX_KEY_T', 'BX_KEY_U', 'BX_KEY_V',
    'BX_KEY_W', 'BX_KEY_X', 'BX_KEY_Y',
    'BX_KEY_Z', 'BX_KEY_0', 'BX_KEY_1',
    'BX_KEY_2', 'BX_KEY_3', 'BX_KEY_4',
    'BX_KEY_5', 'BX_KEY_6', 'BX_KEY_7',
    'BX_KEY_8', 'BX_KEY_9', 'BX_KEY_ESC',
    'BX_KEY_SPACE', 'BX_KEY_SINGLE_QUOTE', 'BX_KEY_COMMA',
    'BX_KEY_PERIOD', 'BX_KEY_SLASH', 'BX_KEY_SEMICOLON',
    'BX_KEY_EQUALS', 'BX_KEY_LEFT_BRACKET', 'BX_KEY_BACKSLASH',
    'BX_KEY_RIGHT_BRACKET', 'BX_KEY_MINUS', 'BX_KEY_GRAVE',
    'BX_KEY_BACKSPACE', 'BX_KEY_ENTER', 'BX_KEY_TAB',
    'BX_KEY_LEFT_BACKSLASH', 'BX_KEY_PRINT', 'BX_KEY_SCRL_LOCK',
    'BX_KEY_PAUSE', 'BX_KEY_INSERT', 'BX_KEY_DELETE',
    'BX_KEY_HOME', 'BX_KEY_END', 'BX_KEY_PAGE_UP',
    'BX_KEY_PAGE_DOWN', 'BX_KEY_KP_ADD', 'BX_KEY_KP_SUBTRACT',
    'BX_KEY_KP_END', 'BX_KEY_KP_DOWN', 'BX_KEY_KP_PAGE_DOWN',
    'BX_KEY_KP_LEFT', 'BX_KEY_KP_RIGHT', 'BX_KEY_KP_HOME',
    'BX_KEY_KP_UP', 'BX_KEY_KP_PAGE_UP', 'BX_KEY_KP_INSERT',
    'BX_KEY_KP_DELETE', 'BX_KEY_KP_5', 'BX_KEY_UP',
    'BX_KEY_DOWN', 'BX_KEY_LEFT', 'BX_KEY_RIGHT',
    'BX_KEY_KP_ENTER', 'BX_KEY_KP_MULTIPLY', 'BX_KEY_KP_DIVIDE',
    'BX_KEY_WIN_L', 'BX_KEY_WIN_R', 'BX_KEY_MENU',
    'BX_KEY_ALT_SYSREQ', 'BX_KEY_CTRL_BREAK', 'BX_KEY_INT_BACK',
    'BX_KEY_INT_FORWARD', 'BX_KEY_INT_STOP', 'BX_KEY_INT_MAIL',
    'BX_KEY_INT_SEARCH', 'BX_KEY_INT_FAV', 'BX_KEY_INT_HOME',
    'BX_KEY_POWER_MYCOMP', 'BX_KEY_POWER_CALC', 'BX_KEY_POWER_SLEEP',
    'BX_KEY_POWER_POWER', 'BX_KEY_POWER_WAKE');


type
  PBXKeyEntry = ^BXKeyEntry;

  BXKeyEntry = record
    baseKey: Bit32u;    // base key
    modKey:  Bit32u;    // modifier key that must be held down
    ascii:   Bit32s;    // ascii equivalent, if any
    xwinKey: Bit32u;    // X windows value
  end;

  bx_keymap_c = class
  public
    constructor Init;
    destructor  Done;

    procedure loadKeymap (const filename: string);
    procedure init_parse;
    function  isKeymapLoaded: Bool;

    function  getKeyASCII (ch: Bit8u): PBXKeyEntry;

  private
    keymapTable: array of BXKeyEntry;
    keymapCount: Bit16u;
    function convertStringToBXKey (S: string): Bit32u;
  end;

var
  bx_keymap: bx_keymap_c;
  lineCount: integer;
  lineptr:   PChar;

implementation

constructor bx_keymap_c.Init;
begin
  keymapCount := 0;
  keymapTable := nil;
end;

destructor bx_keymap_c.Done;
begin
  if keymapTable <> nil then
    SetLength(keymapTable, 0);
  keymapCount := 0;
end;

procedure init_parse_line (line_to_parse: PChar);
//var
//  nl: PChar;
begin
  lineptr := line_to_parse;
//  nl := StrScan(line_to_parse, CR);
//  if (nl <> NULL) then
//    nl := #0;
end;

function get_next_word (output: PChar): Bit32s;
var
  copyp: PChar;
begin
  copyp := output;
  // find first nonspace
  while ((lineptr^ <> #0) and (lineptr^ = #32)) do
    Inc(lineptr);

  if lineptr^ = #0 then
    Exit(-1);

  if lineptr^ = #35 then
    Exit(-1);

  // copy nonspaces into the output
  while (lineptr^ <> #0) and (lineptr^ <> #32) do
  begin
    copyp^ := lineptr^;
    Inc(copyp);
    Inc(lineptr);
  end;
  copyp^ := #0;  // null terminate the copy
  // there must be at least one nonspace, since that's why we stopped the
  // first loop!
  Assert(copyp <> output);
  Exit(0);
end;

function get_next_keymap_line (var fp: TextFile; bxsym, modsym: PChar;
  ascii: PBit32s; xwinsym: PChar): Bit32s;
var
  line, buf: array[0..256] of char;
  p: PChar;
begin
  line[0] := #0;
  while (True) do
  begin
    Inc(LineCount);
  {$I-}
    ReadLn(fp, line);
  {$I+}
    if IOResult <> 0 then
      Exit(-1);

    init_parse_line(line);
    if (get_next_word(bxsym) >= 0) then
    begin
      modsym[0] := #0;
      p := StrScan(bxsym, #43);
      if (p <> NULL) then
        p^ := #0;  // truncate bxsym.

      Inc(p);  // move one char beyond the +
      strcopy(modsym, p);  // copy the rest to modsym
    end;
    Assert(get_next_word(buf) >= 0);
    if ((buf[0] = #92) and (buf[2] = #92) and (buf[3] = #0))
      then ascii^ := Bit32s(buf[1])
      else

    if (strcomp(buf, 'space') = 0)
      then ascii^ := VK_SPACE
      else

    if (strcomp(buf, 'return') = 0)
      then ascii^ := VK_RETURN
      else

    if (strcomp(buf, 'tab') = 0)
      then ascii^ := VK_TAB
      else

    if (strcomp(buf, 'backslash') = 0)
      then ascii^ := VK_RWIN//92
      else

    if (strcomp(buf, 'apostrophe') = 0)
      then ascii^ := VK_RIGHT//39
      else

    if (strcomp(buf, 'none') = 0)
      then ascii^ := -1
      else
      begin
        LogPanic(Format(
        'keymap line %d: ascii equivalent is \"%s\"  but it must be char constant like x, or one of space,tab,return,none',
        [lineCount, buf]));
      end;

    assert(get_next_word(xwinsym) >= 0);
    Exit(0);
  end;
  // no words on this line, keep reading.
end;

procedure bx_keymap_c.init_parse;
begin
  lineCount := 0;
end;

procedure bx_keymap_c.loadKeymap (const filename: string);
var
  keymapFile: TextFile;
//  line: array[0..256] of char;
//  c: char;
//  p: PChar;
  baseSym, mod_Sym, xwinSym: Char256;
  ascii: Bit32s;
  baseKey, mod_Key, xwinKey: Bit32u;
begin
  AssignFile(keymapFile, filename);

  LogInfo(Format('Loading keymap from %s', [filename]));
  init_parse;

  // Read keymap file one line at a time .....................  www.tre-tlc.it/lar
  while (True) do
  begin
    if boolean(get_next_keymap_line(keymapFile, baseSym, mod_Sym,
      @ascii, xwinSym) < 0) then
      break;


//      BX_DEBUG(Format('bxsym:=%s, mod_Sym:=%s, ascii:=%d, guisym:=%s',
//        [baseSym, mod_Sym, ascii, xwinSym]));

    // convert X_KEY_* symbols to values
    baseKey := convertStringToBXKey(baseSym);
    if boolean(strlcomp('XK_', xwinSym, 3) <> 0) then
      LogPanic(Format('keymap line %d: X windows symbol %s must start with XK_',
        [lineCount, xwinSym]));

    mod_Key := convertStringToBXKey(mod_Sym);
    xwinKey := 0;

    // Check if data is valid
    if (baseKey = BX_KEYMAP_UNKNOWN) then
    begin
      LogPanic(Format('line %d: unknown BX_KEY constant %s', [lineCount, baseSym]));
      continue;
    end;

    if (xwinKey = BX_KEYMAP_UNKNOWN) then
    begin
      LogPanic(Format('line %d: unknown GUI constant %s', [lineCount, xwinSym]));
      continue;
    end;

    SetLength(keymapTable, (keymapCount + 1) * sizeof(BXKeyEntry));

    if (keymapTable = NULL) then
      LogPanic(('Can not allocate memory for keymap table.'));

    keymapTable[keymapCount].baseKey := baseKey;
    keymapTable[keymapCount].modKey := mod_Key;
    keymapTable[keymapCount].ascii := ascii;
    keymapTable[keymapCount].xwinKey := xwinKey;

    Inc(keymapCount);
  end;

  LogInfo(Format('Loaded %d symbols', [keymapCount]));

  CloseFile(keymapFile);
end;

function bx_keymap_c.isKeymapLoaded: Bool;
begin
  Result := Bool(keymapCount > 0);
end;

function bx_keymap_c.getKeyASCII (ch: Bit8u): PBXKeyEntry;
var
  i: Bit16u;
begin
//    BX_DEBUG(Format('getKeyASCII (0x%02x)', [ch]));

  // We look through the keymap table to find the searched key
  I := 0;
  while i < keymapcount do
  begin
    if (keymapTable[i].ascii = ch) then
    begin
//        BX_DEBUG(Format('key %02x matches ascii for entry #%d', [ch, i]));
      Result := @keymapTable[i];
      Exit;
    end;
    Inc(i);
  end;
//    BX_DEBUG(Format('key 0x%02x matches no entries', [ch]));

  // Return default
  Result := NULL;
end;

function bx_keymap_c.convertStringToBXKey (S: string): Bit32u;
var
  i: Bit16u;
begin
  // We look through the bx_key_symbol table to find the searched string
  I := 0;
  while i < BX_KEY_NBKEYS do
  begin
    if (strcomp(PChar(S), PChar(bx_key_symbol[i])) = 0) then
      Exit(i);

    Inc(I);
  end;
  // Key is not known
  Result := BX_KEYMAP_UNKNOWN;
end;

end.
