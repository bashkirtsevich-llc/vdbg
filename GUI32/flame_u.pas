unit flame_u;

interface

uses
  Classes, Graphics, Windows;

type
  TFlameThread = class(TThread)
  private
    FHDC: HDC;
    FWidth,
    FHeight: integer;
    procedure PaintIt;
  protected
    procedure Execute; override;
  public
    procedure ConnectCanvas(const DC: HDC);
    procedure Resize(const aHeight, aWidth: integer);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

implementation

uses
  FastBMP, fastrgb;

Const
  Sw = 127;
  Sh = 127;

  RootRand = 100; { Max/Min decrease of the root of the flames }
  Decay = 5; { How far should the flames go up on the screen? }
  { This MUST be positive - JF }
  MinY = 1; { Startingline of the flame routine.
    (should be adjusted along with MinY above) }
  Smooth = 5; { How descrete can the flames be? }
  MinFire = 80; { limit between the "starting to burn" and
    the "is burning" routines }
  XStart = 1;
  { Startingpos on the screen, should be divideable by 4 without remain! }
  XEnd = 127; { Guess! }
  Width = XEnd - XStart; { Well- }
  MaxColor = 125; { Constant for the MakePal procedure }
  FireIncrease: byte = 10; { 3 = Wood, 90 = Gazolin }

  detailw = 40;
  detailh = 40;
  details = 2; // Detail Size
  detailp = 150; // Detail Power
  detailspeed = 0.1;

var
  bmp: tfastbmp;
  Pal: array [0 .. 255] of TFColor;
  Scr, scr2: Array [0 .. Sh, 0 .. Sw] Of byte;
  fog: integer = 0;
  fog2: integer = 0;
  Fmore, Fless: boolean;
  bla, bla2, b, bb: real;
  bbb, bbb2: byte;
  htktmp: integer;
  htkreal: real;

Function Rand(r: integer): integer; { Return a random number between -R And R }
begin
  Rand := Random(r * 2 + 1) - r;
end;

Procedure Hsi2Rgb(H, S, I: real; var C: TFColor);
{ Convert (Hue, Saturation, Intensity) -> (RGB) }
var
  T: real;
  Rv, Gv, Bv: real;
begin
  T := H;
  Rv := 1 + S * Sin(T - 2 * Pi / 3);
  Gv := 1 + S * Sin(T);
  Bv := 1 + S * Sin(T + 2 * Pi / 3);
  T := 63.999 * I / 2;
  with C do
  begin
    r := trunc(Rv * T);
    g := trunc(Gv * T);
    b := trunc(Bv * T);
  end;
end;

Procedure MakePal;
Var
  I: byte;
begin
  FillChar(Pal, SizeOf(Pal), 0);
  For I := 1 To MaxColor Do
    Hsi2Rgb(4.6 - 1.5 * I / MaxColor, I / MaxColor, I / MaxColor, Pal[I]);

  For I := MaxColor To 255 Do
  begin
    Pal[I] := Pal[I - 1];
    With Pal[I] Do
    begin
      If r < 63 Then
        inc(r);
      If (I Mod 2 = 0) And (g < 53) Then
        inc(g);
      If (I Mod 2 = 0) And (b < 63) Then
        inc(b);
    end;
  end;

  for I := 0 to 255 do
  begin
    Pal[I].r := Pal[I].r * 4;
    Pal[I].g := Pal[I].g * 4;
    Pal[I].b := Pal[I].b * 4;

    if I < 60 then
    begin
      Pal[I].g := Pal[I].r;
      Pal[I].b := Pal[I].r;
    end;
  end;
end;

procedure initialize;
begin
  bmp := tfastbmp.create(Sw + 1, Sh + 1);
end;

procedure release;
begin
  bmp.free;
end;


{ TFlameThread }

procedure TFlameThread.AfterConstruction;
begin
  inherited;
  initialize;
end;

procedure TFlameThread.BeforeDestruction;
begin
  inherited;
  release;
end;

procedure TFlameThread.ConnectCanvas(const DC: HDC);
begin
  FHDC := DC;
end;

procedure TFlameThread.Execute;
var
  FlameArray: Array [XStart .. XEnd] Of byte;
  I, J: integer;
  x: integer;
  MoreFire, V: integer;
  bmpx, bmpy: integer;
begin
  htktmp := 0;
  RandomIze;
  MoreFire := 10;
  MakePal;

  { Initialize FlameArray }
  For I := XStart To XEnd Do
    FlameArray[I] := 0;

  FillChar(Scr, SizeOf(Scr), 0); { Clear Screen }
  FillChar(scr2, SizeOf(scr2), 0); { Clear Screen }
  FillChar(FlameArray[XStart + Random(XEnd - XStart - 5)], 5, 15);
  repeat

    { Put the values from FlameArray on the bottom line of the screen }
    For I := XStart To XEnd Do
      scr2[I, Sh] := FlameArray[I];

    { This loop makes the actual flames }

    For I := XStart To XEnd Do
      For J := MinY To Sh Do
      begin
        V := scr2[I, J];
        If (V = 0) Or (V < Decay) Or (I <= XStart) Or (I >= XEnd)
          Then scr2[I, Pred(J)] := 0
          else scr2[I - Pred(Random(3)), Pred(J)] := V - Random(Decay);
      end;

    if htktmp > 9000 then
      htktmp := 0;

    If (Random(255) = 0) Then
      FillChar(FlameArray[XStart + Random(XEnd - XStart - 5)], 5, 255);

    // Water effect
    if (htktmp > 1000) and (htktmp < 1500) then
      for I := 1 To 10 Do
        FlameArray[XStart + Random(XEnd)] := 0;

    { This loop controls the "root" of the
      flames ie. the values in FlameArray. }
    For I := XStart To XEnd Do
    begin
      x := FlameArray[I];
      If x < MinFire Then { Increase by the "burnability" }
      begin
        { Starting to burn: }
        If x > 10 Then
          inc(x, Random(FireIncrease));
      end else
        { Otherwise randomize and increase by intensity (is burning) }
        inc(x, Rand(RootRand) + MoreFire);
      If x > 255 Then
        x := 255; { X Too large ? }
      FlameArray[I] := x;
    end;

    { Smoothen the values of FrameArray to avoid "descrete" flames }

    for I := XStart + Smooth To XEnd - Smooth Do
    begin
      x := 0;
      for J := -Smooth To Smooth Do
        inc(x, FlameArray[I + J]);
      FlameArray[I] := x Div (2 * Smooth + 1);
    end;

    for bmpx := 0 to Sw do
      for bmpy := 0 to Sh do
        bmp.Pixels[bmpx, bmpy] := Pal[scr2[bmpy, bmpx]];

    inc(htktmp);

    Synchronize(PaintIt);

    inc(fog);
    Sleep(5);
  Until Terminated;
end;

procedure TFlameThread.PaintIt;
begin
  bmp.Stretch(FHDC, 0, 0, FWidth, FHeight);
end;

procedure TFlameThread.Resize(const aHeight, aWidth: integer);
begin
  FHeight := aHeight;
  FWidth := aWidth;
end;

end.
