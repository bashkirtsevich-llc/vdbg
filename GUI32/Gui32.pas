unit Gui32;

interface

uses
  Windows, Config, Messages, SysUtils;

type
  PArrayRGBQUAD = ^TArrayRGBQUAD;
  TArrayRGBQUAD = array of TRGBQuad;
  intg          = integer;

  PQueueEvent = ^QueueEvent;

  QueueEvent = record
    key_event: Bit32u;
    mouse_x:   integer;
    mouse_y:   integer;
    mouse_button_state: integer;
  end;

const
  SCANCODE_BUFSIZE  = 20;
  BX_KEY_PRESSED    = $00000000;
  BX_KEY_RELEASED   = $80000000;
  MOUSE_PRESSED     = $20000000;
  HEADERBAR_CLICKED = $08000000;
  MOUSE_MOTION      = $22000000;

var
  MainWnd:     THandle;
  dimension_x, dimension_y: word;
  prev_block_cursor_x: unsigned = 0;
  prev_block_cursor_y: unsigned = 0;
  yChar:       integer = 16;
  cursorBmp:   HBITMAP;
  crPal:       array[0..16] of COLORREF;
  updated_area_valid: boolean = False;
  updated_area: TRect;
  x_tilesize, y_tilesize: integer;
  bitmap_info: PBitmapInfo;
  cmap_index:  PRGBQUAD;  // indeces into system colormap
  black_quad:  RGBQUAD;
  stretched_x, stretched_y: unsigned;
  y_caption:   integer = 19;
  count_tot_tiles: longword = 0;
  tail:        longword = 0;
  head:        longword = 0;
  keyevents:   array[0..SCANCODE_BUFSIZE] of QueueEvent;
  HandleFonts: array[0..2] of THandle;
  bxVgaFont: array of Byte;

procedure clear_screen;
procedure processMouseXY (x: intg; y: intg; windows_state: intg;
  implied_state_change: intg);
procedure text_update (old_text: PAnsiChar; new_text: PAnsiChar; cursor_x: longword;
  cursor_y: longword; cursor_state: Bit16u; nrows: unsigned);
function  palette_change (index, red, green, blue: unsigned): Bool;
procedure graphics_tile_update (tile: PBit8u; x0: unsigned; y0: unsigned);
procedure DrawBitmap (hdc: HDC; hBitmap: HBITMAP; xStart: integer;
  yStart: integer; dwRop: DWORD; cColor: byte);
procedure DrawChar (hdc: HDC; c: AnsiChar; xStart: integer; yStart: integer;
  cColor: byte; cs_start: integer; cs_end: integer);
procedure CreateFont;
procedure TerminateEmul;
procedure FlushGui;
procedure updateUpdated (x1, y1, x2, y2: integer);
procedure Gui32Init (WinHandle: THandle; DW: integer);
procedure Gui32Stop;
procedure enq_key_event (key: Bit32u; press_release: Bit32u);
procedure handle_events;
function  deq_key_event: PQueueEvent;
procedure dimension_update (x: unsigned; y: unsigned);
procedure InitFont;
procedure Gui32InitFontConfig(const aFileName: string); // заглушка

var
  MemoryBitmap:   THandle;
  stretch_factor: integer = 1;

implementation

uses service, keyboard, Math, cpu, m2fMain, Classes;

var
  MemoryDC: THandle = 0;
  fontID:  integer = 2;
  vgafont: array[0..256] of THandle;
  DeltaWidth: integer;
  mouse_button_state: unsigned = 0;
  ms_xdelta: intg = 0;
  ms_ydelta: intg = 0;
  ms_lastx: intg = 0;
  ms_lasty: intg = 0;
  ms_savedx: intg = 0;
  ms_savedy: intg = 0;
  mouseCaptureMode: longbool = False;

procedure LoadFont;
var
  f: TMemoryStream;
begin
  f :=  TMemoryStream.Create;
  try
    f.LoadFromFile(VMConfig.Font);
    SetLength(bxVgaFont, f.Size);
    Move(f.Memory^, bxVgaFont[0], f.Size);
  finally
    f.Free;
  end;
end;

procedure clear_screen;
var
  oldObj: HGDIOBJ;
begin
  //if (!stInfo.UIinited) return;

  EnterCriticalSection(drawCS);

  oldObj := SelectObject(MemoryDC, MemoryBitmap);
  PatBlt(MemoryDC, 0, 0, stretched_x, stretched_y, BLACKNESS);
  SelectObject(MemoryDC, oldObj);

  updateUpdated(0, 0, dimension_x - 1, dimension_y - 1);

  LeaveCriticalSection(drawCS);
end;

procedure resetDelta;
begin
  EnterCriticalSection(mouseCS);
  ms_savedx := ms_lastx;
  ms_savedy := ms_lasty;
  ms_xdelta := 0;
  ms_ydelta := ms_xdelta;
  LeaveCriticalSection(mouseCS);
end;

procedure enq_mouse_event(aForce: boolean = false);
var
  current: PQueueEvent;
begin
  EnterCriticalSection(mouseCS);
  // можно вообще это условие вывалить
  if ((ms_xdelta <> 0) or (ms_ydelta <> 0)) or aForce then
  begin
    if (((tail + 1) mod SCANCODE_BUFSIZE) = head) then
    begin
      LogError(('enq_scancode: buffer full'));
      exit;
    end;

    current := @keyevents[tail];
    current.key_event := MOUSE_MOTION;
    current.mouse_x := ms_xdelta;
    current.mouse_y := ms_ydelta;
    current.mouse_button_state := mouse_button_state;
    resetDelta();
    tail := (tail + 1) mod SCANCODE_BUFSIZE;
  end;
  LeaveCriticalSection(mouseCS);
end;

procedure processMouseXY (x: intg; y: intg; windows_state: intg;
  implied_state_change: intg);
var
  bx_state: intg;
//  old_bx_state: intg;
begin
  if isStarted then
  begin
    EnterCriticalSection(mouseCS);

    bx_state := ((ifthen((windows_state and MK_LBUTTON) <> 0, 1) +
                 (ifthen((windows_state and MK_RBUTTON) <> 0, 2))));

//    old_bx_state := bx_state xor implied_state_change;
    mouse_button_state := windows_state;

    ms_ydelta := ms_savedy - y;
    ms_xdelta := x - ms_savedx;
    ms_lastx  := x;
    ms_lasty  := y;

    enq_mouse_event(MouseEnabled);
    LeaveCriticalSection(mouseCS);
  end;
end;

procedure handle_events;
var
  key: Bit32u;
  scancode: byte;
  queue_event: PQueueEvent;
label
  end_procedure;
begin
  EnterCriticalSection(keyCS);
  enq_mouse_event();
  while (head <> tail) do
  begin
    queue_event := deq_key_event;
    if queue_event = nil then
      break;
    key := queue_event^.key_event;
    if (key = MOUSE_MOTION) then
    begin
      bx_keyboard.mouse_motion(queue_event.mouse_x,
        queue_event.mouse_y, queue_event.mouse_button_state);
    end
    // Check for mouse buttons first
    else
      if (key and MOUSE_PRESSED) <> 0 then
      begin
        // printf("# click!\n");
        bx_keyboard.mouse_motion(0, 0, LOWORD(key));
      end
      else
        if (key and HEADERBAR_CLICKED) <> 0 then
        begin
          //headerbar_click(LOWORD(key));
        end
        else

          if ((key and $ff) = $45) then
            key := key xor $100;
    if boolean(key and $0100) then
    begin
      // This makes the "AltGr" key on European keyboards work
      if (key = $138) then
      begin
        scancode := $9d; // left control key released
        bx_keyboard.put_scancode(@scancode, 1);
      end;
      // Its an extended key
      scancode := $E0;
      bx_keyboard.put_scancode(@scancode, 1);
    end;
    // Its a key
    scancode := LOBYTE(LOWORD(key));
    // printf("# key = %d, scancode = %d\n",key,scancode);
    if (key and BX_KEY_RELEASED) <> 0 then
      scancode := scancode or $80;
    bx_keyboard.put_scancode(@scancode, 1);
  end;
  leavecriticalsection(keyCS);
  end_procedure: ;
end;

function deq_key_event: PQueueEvent;
var
  key: PQueueEvent;
begin
  if (head = tail) then
  begin
    LogError(('deq_scancode: buffer empty'));
    Result := nil;
    Exit;
  end;
  key  := @keyevents[head];
  head := (head + 1) mod SCANCODE_BUFSIZE;
  Result := key;
end;

procedure enq_key_event (key: Bit32u; press_release: Bit32u);
begin
  if (((tail + 1) mod SCANCODE_BUFSIZE) = head) then
  begin
    LogError(('enq_scancode: buffer full'));
    exit;
  end;
  keyevents[tail].key_event := key or press_release;
  tail := (tail + 1) mod SCANCODE_BUFSIZE;
end;

procedure Gui32Init (WinHandle: THandle; DW: integer);
  var
    i: unsigned;
  begin
    DeltaWidth := DW;
    MainWnd  := WinHandle;
    crPal[0] := RGB(0, 0, 0);
    crPal[1] := RGB(0, 0, $A8);
    crPal[2] := RGB(0, $A8, 0);
    crPal[3] := RGB(0, $A8, $A8);
    crPal[4] := RGB($A8, 0, 0);
    crPal[5] := RGB($A8, 0, $A8);
    crPal[6] := RGB($A8, $54, 0);
    crPal[7] := RGB($A8, $A8, $A8);
    crPal[8] := RGB($54, $54, $54);
    crPal[9] := RGB($54, $54, $FC);
    crPal[10] := RGB($54, $FC, $54);
    crPal[11] := RGB($54, $FC, $FC);
    crPal[12] := RGB($FC, $54, $54);
    crPal[13] := RGB($FC, $54, $FC);
    crPal[14] := RGB($FC, $FC, $54);
    crPal[15] := RGB($FC, $FC, $FC);

    black_quad.rgbBlue := 0;
    black_quad.rgbGreen := 0;
    black_quad.rgbRed := 0;
    black_quad.rgbReserved := 0;
    x_tilesize := 16;
    y_tilesize := 16;
    bitmap_Info := AllocMem(sizeof(TBitmapInfoheader) + 256 * sizeof(RGBQUAD));
    bitmap_info^.bmiHeader.biSize := sizeof(BITMAPINFOHEADER);
    bitmap_info^.bmiHeader.biWidth := x_tilesize;
    // Height is negative for top-down bitmap
    bitmap_info^.bmiHeader.biHeight := -y_tilesize;
    bitmap_info^.bmiHeader.biPlanes := 1;
    bitmap_info^.bmiHeader.biBitCount := 8;
    bitmap_info^.bmiHeader.biCompression := BI_RGB;
    bitmap_info^.bmiHeader.biSizeImage := x_tilesize * y_tilesize;
    // I think these next two figures don't matter; saying 45 pixels/centimeter
    bitmap_info^.bmiHeader.biXPelsPerMeter := 4500;
    bitmap_info^.bmiHeader.biYPelsPerMeter := 4500;
    bitmap_info^.bmiHeader.biClrUsed := 256;
    bitmap_info^.bmiHeader.biClrImportant := 0;
    cmap_index := @bitmap_info^.bmiColors;
    // start out with all color map indeces pointing to Black
    cmap_index^ := black_quad;
    i := 1;
    while i < 256 do
      begin
      PRGBQuad(integer(cmap_index) + (i * SizeOf(RGBQUAD)))^ :=
        PRGBQuad(integer(cmap_index))^;
      Inc(i);
      end;
    Dimension_x := 320;
    Dimension_y := 400;
    CreateFont;
  end;

procedure Gui32Stop;
  var
    I: integer;
  begin
    FreeMem(bitmap_Info);
    for i := 0 to 256 do
      begin
      DeleteObject(vgafont[i]);
      end;
    DeleteObject(MemoryBitmap);
    DeleteDC(MemoryDC);
  end;

procedure dimension_update (x: unsigned; y: unsigned);
begin
  // можно сделать нотификацию о текущем разрешении
//  SetWindowText(MainWnd, Format('Screen resolution = %d x %d', [x, y]));
  if (x = dimension_x) and (y = dimension_y) then
    Exit;

  dimension_x := x;
  dimension_y := y;
  stretched_x := dimension_x;
  stretched_y := dimension_y;
  stretch_factor := 1;
  while (stretched_x < 400) do
  begin
    stretched_x := stretched_x * 2;
    stretched_y := stretched_y * 2;
    stretch_factor := stretch_factor * 2;
  end;

  FontId := 2;
  yChar  := 16;
  if (y > 600) then
  begin
    FontId := 0;
    yChar  := 12;
    dimension_y := trunc(y * yChar / 16);
    stretched_y := dimension_y * stretch_factor;
  end else
  if (y > 480) then
  begin
    FontId := 1;
    yChar  := 14;
    dimension_y := trunc(y * yChar / 16);
    stretched_y := dimension_y * stretch_factor;
  end;

//  fMain.ClientWidth := stretched_x + DeltaWidth;
//  fMain.ClientHeight := stretched_y + y_caption; // + 60;
//    // хреново работает
//    SetWindowPos(MainWnd, HWND_TOP, 0, 0, stretched_x + DeltaWidth,
//      stretched_y + y_caption + 60,
//      SWP_NOMOVE or SWP_NOZORDER);
end;

function palette_change (index, red, green, blue: unsigned): Bool;
begin
  PRGBQuad(integer(cmap_index) + (index * SizeOf(RGBQUAD)))^.rgbRed := red;
  PRGBQuad(integer(cmap_index) + (index * SizeOf(RGBQUAD)))^.rgbBlue := Blue;
  PRGBQuad(integer(cmap_index) + (index * SizeOf(RGBQUAD)))^.rgbGreen := Green;
  Result := 1;
end;

procedure graphics_tile_update (tile: PBit8u; x0: unsigned; y0: unsigned);
var
  hdc: THandle;
  oldObj: THandle;
  true_y0: unsigned;
//  Cannato: integer;
 // S: array[0..10] of char;
begin
  Inc(count_tot_tiles);
  true_y0 := y0;//+ bx_headerbar_y;

  EnterCriticalSection(drawCS);
  hdc := GetDC(MainWnd);

  oldObj := SelectObject(MemoryDC, MemoryBitmap);

  {Cannato :=} StretchDIBits(MemoryDC, x0, true_y0, x_tilesize, y_tilesize,
    0, 0, x_tilesize, y_tilesize, tile, bitmap_info^, DIB_RGB_COLORS, SRCCOPY);
  SelectObject(MemoryDC, oldObj);

  updateUpdated(x0, true_y0, x0 + x_tilesize - 1, true_y0 + y_tilesize - 1);

  ReleaseDC(MainWnd, hdc);
  leavecriticalsection(drawCS);
end;

function reverse_bitorder (b: byte): byte;
var
  ret: byte;
  I: integer;
begin
  ret := 0;

  for i := 0 to 8 do
  begin
    ret := ret or (b and $01) shl (7 - i);
    b := b shr 1;
  end;

  Result := Ret;
end;

procedure text_update (old_text: PAnsiChar; new_text: PAnsiChar; cursor_x: longword;
  cursor_y: longword; cursor_state: Bit16u; nrows: unsigned);
var
  hdc: THandle;
  cChar: AnsiChar;
  x, y: unsigned;
  i: integer;
  cs_start, cs_end: Bit8u;
  nchars: unsigned;
  Data: array[0..32] of byte;
  cAttr: AnsiChar;
begin
  if StrComp(old_text, new_text) = 0 then
    Exit;
  cs_start := (cursor_state shr 8) and $3f;
  cs_end := cursor_state and $1f;

  //if Boolean(stInfo.UIinited=0) then exit;

  EnterCriticalSection(drawCS);

  hdc := GetDC(MainWnd);

  // Number of characters on screen, variable number of rows
  nchars := 80 * nrows;

  if boolean((prev_block_cursor_y * 80 + prev_block_cursor_x) < nchars) then
  begin
    cChar := PAnsiChar(integer(new_text) + (prev_block_cursor_y * 80 +
      prev_block_cursor_x) * 2)^;
    if boolean(yChar >= 16) then
    begin
      DrawBitmap(hdc, vgafont[byte(cChar)], prev_block_cursor_x * 8,
        prev_block_cursor_y * 16, SRCCOPY,
        integer(new_text[((prev_block_cursor_y * 80 + prev_block_cursor_x) * 2) + 1]));
    end else
    begin
      DrawChar(hdc, cChar, prev_block_cursor_x * 8,
        prev_block_cursor_y * yChar,
        integer(new_text[((prev_block_cursor_y * 80 + prev_block_cursor_x) * 2) + 1]),
        1, 0);
    end;
  end;

  i := 0;
  while i < nchars * 2 do
  begin
    if boolean((old_text[i] <> new_text[i]) or (old_text[i + 1] <>
      new_text[i + 1])) then
    begin

      cChar := new_text[i];

      x := Trunc(i / 2) mod 80;
      y := Trunc(i / 2) div 80;
      if (yChar >= 16) then
      begin
        DrawBitmap(hdc, vgafont[byte(cChar)], x * 8, y * 16, SRCCOPY,
          integer(new_text[i + 1]));
      end else
      begin
        DrawChar(hdc, cChar, x * 8, y * yChar, integer(new_text[i + 1]), 1, 0);
      end;
    end;
    Inc(i, 2);
  end;

  prev_block_cursor_x := cursor_x;
  prev_block_cursor_y := cursor_y;

  // now draw character at new block cursor location in reverse
  if boolean(((cursor_y * 80 + cursor_x) < nchars) and (cs_start <= cs_end)) then
  begin
    if Length(bxVgaFont) = 0 then
      LoadFont;
    cChar := new_text[(cursor_y * 80 + cursor_x) * 2];
    if boolean(yChar >= 16) then
    begin
      FillChar(Data, sizeof(Data), 0);
      i := 0;
      while i < 16 do
      begin
        Data[i * 2] := bxVgaFont[byte(cChar) * 16 + i];
        if boolean((i >= cs_start) and (i <= cs_end)) then
          byte(Data[i * 2]) := 255 - byte(Data[i * 2]);
        Inc(i);
      end;
      SetBitmapBits(cursorBmp, 32, @Data);
      DrawBitmap(hdc, cursorBmp, cursor_x * 8, cursor_y * 16,
        SRCCOPY, integer(new_text[((cursor_y * 80 + cursor_x) * 2) + 1]));
    end else
    begin
      cAttr := new_text[((cursor_y * 80 + cursor_x) * 2) + 1];
      DrawChar(hdc, cChar, cursor_x * 8, cursor_y * yChar, byte(cAttr),
        cs_start, cs_end);
    end;
  end;

  ReleaseDC(MainWnd, hdc);
  leavecriticalsection(drawCS);
end;

procedure DrawBitmap (hdc: HDC; hBitmap: HBITMAP; xStart: integer;
  yStart: integer; dwRop: DWORD; cColor: byte);
var
  bm: tagBITMAP;
  hdcMem: THandle;
  ptSize, ptOrg: TPOINT;
  oldObj: HGDIOBJ;
  crFore, crBack: COLORREF;
begin
  hdcMem := CreateCompatibleDC(hdc);
  SelectObject(hdcMem, hBitmap);
  SetMapmode(hdcMem, GetMapmode(hdc));

  GetObject(hBitmap, sizeof(BITMAP), @bm);

  ptSize.x := bm.bmWidth;
  ptSize.y := bm.bmHeight;

  DPtoLP(hdc, ptSize, 1);

  ptOrg.x := 0;
  ptOrg.y := 0;
  DPtoLP(hdcMem, ptOrg, 1);

  // BitBlt (hdc, xStart, yStart, ptSize.x, ptSize.y, hdcMem, ptOrg.x,
  //       ptOrg.y, dwRop);

  oldObj := SelectObject(MemoryDC, MemoryBitmap);
  //  BitBlt(MemoryDC, xStart, yStart, ptSize.x, ptSize.y, hdcMem, ptOrg.x,
  //      ptOrg.y, dwRop);
  //Colors taken from Ralf Browns interrupt list.
  //(0:=black, 1:=blue, 2:=red, 3:=purple, 4:=green, 5:=cyan, 6:=yellow, 7:=white)
  //The highest background bit usually means blinking characters. No idea
  //how to implement that so for now it's just implemented as color.
  //Note: it is also possible to program the VGA controller to have the
  //high bit for the foreground color enable blinking characters.

  crFore := SetTextColor(MemoryDC, crPal[(cColor shr 4) and $f]);
  crBack := SetBkColor(MemoryDC, crPal[cColor and $f]);
  BitBlt(MemoryDC, xStart, yStart, ptSize.x, ptSize.y, hdcMem, ptOrg.x,
    ptOrg.y, dwRop);
  SetBkColor(MemoryDC, crBack);
  SetTextColor(MemoryDC, crFore);

  SelectObject(MemoryDC, oldObj);

  updateUpdated(xStart, yStart, ptSize.x + xStart - 1, ptSize.y + yStart - 1);

  DeleteDC(hdcMem);
end;

procedure DrawChar (hdc: HDC; c: AnsiChar; xStart: integer; yStart: integer;
  cColor: byte; cs_start: integer; cs_end: integer);
var
  hdcMem: THandle;
  ptSize, ptOrg: TPoint;
  oldObj: THandle;
  str: array[0..2] of AnsiChar;
  hFontOld: HFONT;
  crFore, crBack: COLORREF;
  y:  integer;
  rc: TRECT;
begin
  hdcMem := CreateCompatibleDC(hdc);
  SetMapmode(hdcMem, GetMapmode(hdc));
  ptSize.x := 8;
  ptSize.y := yChar;

  DPtoLP(hdc, ptSize, 1);

  ptOrg.x := 0;
  ptOrg.y := 0;

  DPtoLP(hdcMem, ptOrg, 1);

  oldObj := SelectObject(MemoryDC, MemoryBitmap);
  hFontOld := SelectObject(MemoryDC, HandleFonts[FontId]);

  //Colors taken from Ralf Browns interrupt list.
  //(0:=black, 1:=blue, 2:=red, 3:=purple, 4:=green, 5:=cyan, 6:=yellow, 7:=white)
  //The highest background bit usually means blinking characters. No idea
  //how to implement that so for now it's just implemented as color.
  //Note: it is also possible to program the VGA controller to have the
  //high bit for the foreground color enable blinking characters.

  crFore := SetTextColor(MemoryDC, crPal[cColor and $f]);
  crBack := SetBkColor(MemoryDC, crPal[(cColor shr 4) and $f]);
  str[0] := c;
  str[1] := #0;

  if fontID = 2
    then y := 16
    else y := 8;

  TextOutA(MemoryDC, xStart, yStart, str, 1);
  if boolean((cs_start <= cs_end) and (cs_start < y)) then
  begin
    SetBkColor(MemoryDC, crPal[cColor and $f]);
    SetTextColor(MemoryDC, crPal[(cColor shr 4) and $f]);
    rc.left  := xStart + 0;
    rc.right := xStart + 8;
    if boolean(cs_end >= y) then
      cs_end := y - 1;
    rc.top := Trunc(yStart + cs_start * yChar / y);
    rc.bottom := Trunc(yStart + (cs_end + 1) * yChar / y);
    ExtTextOut(MemoryDC, xStart, yStart, ETO_CLIPPED or ETO_OPAQUE, @rc, str, 1, NULL);
  end;

  SetBkColor(MemoryDC, crBack);
  SetTextColor(MemoryDC, crFore);

  SelectObject(MemoryDC, hFontOld);
  SelectObject(MemoryDC, oldObj);

  updateUpdated(xStart, yStart, ptSize.x + xStart - 1, ptSize.y + yStart - 1);

  DeleteDC(hdcMem);
end;

procedure CreateFont;
var
  HDC:  THandle;
  Data: array[0..32] of byte;
  c, i: word;
begin
  if Length(bxVgaFont) = 0 then
    LoadFont;
  HDC := GetDC(MainWnd);
  for c := 0 to 255 do
  begin
    vgafont[c] := CreateBitmap(8, 16, 1, 1, nil);
    FillChar(Data, 32, 0);
    for i := 0 to 15 do
      Data[i * 2] := bxVgaFont[c * 16 + i];
    SetBitmapBits(vgafont[c], 32, @Data);
  end;
  ReleaseDC(MainWnd, HDC);
  hdc := GetDC(MainWnd);
  MemoryBitmap := CreateCompatibleBitmap(hdc, 1920, 1200);
  MemoryDC := CreateCompatibleDC(hdc);
  ReleaseDC(MainWnd, hdc);
end;

procedure TerminateEmul;
var
  c: integer;
begin
  DeleteCriticalSection(DrawCs);
  DeleteDC(MemoryDC);
  DeleteObject(MemoryBitmap);
  for c := 0 to 256 do
    DeleteObject(VgaFont[c]);
end;

procedure FlushGui;
begin
  EnterCriticalSection(drawCS);
  if updated_area_valid then
    begin
    Inc(updated_area.right);
    Inc(updated_area.bottom);
    InvalidateRect(MainWnd, @updated_area, False);
    updated_area_valid := False;
    end;
  leavecriticalsection(drawCS);
end;

procedure updateUpdated (x1, y1, x2, y2: integer);
begin
  x1 := x1 * stretch_factor;
  y1 := y1 * stretch_factor;
  x2 := x2 * stretch_factor;
  y2 := y2 * stretch_factor;
  if (updated_area_valid = False) then
  begin
    updated_area.left := x1;
    updated_area.top  := y1;
    updated_area.right := x2;
    updated_area.bottom := y2;
  end
  else
  begin
    if (x1 < updated_area.left) then
      updated_area.left := x1;
    if (y1 < updated_area.top) then
      updated_area.top := y1;
    if (x2 > updated_area.right) then
      updated_area.right := x2;
    if (y2 > updated_area.bottom) then
      updated_area.bottom := y2;
  end;

  updated_area_valid := True;
end;

procedure InitFont;
var
  lf: TLOGFONT;
  i:  integer;
begin
  lf.lfWidth  := 8;
  lf.lfEscapement := 0;
  lf.lfOrientation := 0;
  lf.lfWeight := FW_MEDIUM;
  lf.lfItalic := 0;
  lf.lfUnderline := 0;
  lf.lfStrikeOut := 0;
  lf.lfCharSet := OEM_CHARSET;
  lf.lfOutPrecision := OUT_DEFAULT_PRECIS;
  lf.lfClipPrecision := CLIP_DEFAULT_PRECIS;
  lf.lfQuality := DEFAULT_QUALITY;
  lf.lfPitchAndFamily := FIXED_PITCH or FF_DONTCARE;
  StrCopy(lf.lfFaceName, 'Lucida Console');

  i := 0;
  while i < 3 do
  begin
    lf.lfHeight := 12 + i * 2;
    HandleFonts[i] := CreateFontIndirect(lf);
    Inc(i);
  end;
end;

procedure Gui32InitFontConfig(const aFileName: string);
begin
  VMConfig.Font := aFileName;
end;

end.
