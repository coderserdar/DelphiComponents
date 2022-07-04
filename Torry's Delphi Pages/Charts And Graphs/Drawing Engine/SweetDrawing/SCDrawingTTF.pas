{*******************************************************}
{                                                       }
{              CA SweetDrawing Library                  }
{                                                       }
{  Copyright (c) 1996,2004 CodeAccelerate Corporation   }
{                                                       }
{*******************************************************}

unit SCDrawingTTF;

{$I SweetDrawing.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Math,
  SCDECommon, SCDEConsts, SCDrawingCommons;

type
  TSCDeFontStroke = record
    GlyphNumber: Integer;
    P1: TDoublePoint;
    P2: TDoublePoint;
  end;
  PSCDeFontStroke = ^TSCDeFontStroke;

  TEnumSCDeStrokesCallback = function(Index: Integer; const Stroke: TSCDeFontStroke): Boolean of object;

  TSCDeStrokes = class(TList)
  private
    FCellIncX: Integer;
    FCellIncY: Integer;
    FBlackBoxX: Cardinal;
    FBlackBoxY: Cardinal;
    FGlyphOrigin: TPoint;
    function GetBounds: TDoubleRect;
    function GetNumGlyphs: Integer;
    function GeTSCDeFontStroke(Index: Integer): TSCDeFontStroke;
  protected
    procedure ClearStrokes;
  public
    constructor Create;
    destructor Destroy; override;

    function  StartOfGlyph(GlyphNumber: Integer): Integer;
    function  GlyphNumStrokes(GlyphNumber: Integer): Integer;
    procedure EnumStrokes(Callback: TEnumSCDeStrokesCallback);

    property NumGlyphs: Integer read GetNumGlyphs;
    property Stroke[Index:integer]: TSCDeFontStroke read GeTSCDeFontStroke;
    property Bounds: TDoubleRect read GetBounds;

    property CellIncX: Integer read FCellIncX;
    property CellIncY: Integer read FCellIncY;
    property BlackBoxX: Cardinal read FBlackBoxX;
    property BlackBoxY: Cardinal read FBlackBoxY;
    property GlyphOrigin: TPoint read FGlyphOrigin;
  end;

  TSCDeTTF = class
  private
    FFont: TFont;
    FSplinePrecision: Integer;
    function  NewStroke(GlyphNumber: Integer; var P1, P2: TDoublePoint): PSCDeFontStroke;
    procedure DrawQSpline(APolyN: Integer; var Pa, Pb, Pc: TDoublePoint;
      Collection: TSCDeStrokes);
  protected
    procedure SetFont(Value: TFont); virtual;
    procedure SetSplinePrecision(Value: Integer);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function GetCharacterGlyphs(CharCode: Integer): TSCDeStrokes;
  published
    property Font: TFont read FFont write SetFont;
    property Precision: Integer read FSplinePrecision write SetSplinePrecision default 5;
  end;

implementation

const
  EMSIZE = 1024;
  SIZEX  = 1024;
  SIZEY  = 1024;

type
  TFXPArray = array[0..MaxInt div SizeOf(TPOINTFX)-1] of TPOINTFX;
  PFXPArray = ^TFXPArray;

{ TSCDeTTF }

constructor TSCDeTTF.Create;
begin
  inherited Create;
  FFont := TFont.Create;
  FFont.Name := 'Arial';
  FFont.Size := 28;
  FSplinePrecision := 8;
end;

destructor TSCDeTTF.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

procedure TSCDeTTF.DrawQSpline(APolyN: Integer; var Pa, Pb,
  Pc: TDoublePoint; Collection: TSCDeStrokes);
var
  di, i: Double;
  P1, P2: TDoublePoint;
begin
  di := 1.0 / FSplinePrecision;
  i := di;
  p2 := pa;

  while i <= 1.0 do
  begin
    if i - di/2 > 1.0 - di then
      i := 1.0;

    P1 := P2;
    P2.x := (Pa.x - 2*Pb.x + Pc.x)*Sqr(i) + (2*Pb.x - 2*Pa.x)*i + Pa.x;
    P2.y := (Pa.y - 2*Pb.y + Pc.y)*Sqr(i) + (2*Pb.y - 2*Pa.y)*i + Pa.y;

    if not TSCDeUtils.EqualPoint(P1, P2) then
      Collection.Add(NewStroke(APolyN, P1, P2));

    i := i + di;
  end;

  Pc := P2;
end;

function TSCDeTTF.GetCharacterGlyphs(CharCode: Integer): TSCDeStrokes;
var
  M2: TMAT2;
  S: String;
  Size: TSize;
  Done: Boolean;
  DC, MDC: HDC;
  OFont: HFONT;
  Gm: TGLYPHMETRICS;
  Pc: PTTPolyCurve;
  lpAPFX: PFXPArray;
  Res, BufSize: DWord;
  PfxA, PfxB, PfXC,
  Ps, P1, P2 : TDoublePoint;
  BufPtr, Buf: PTTPolygonHeader;
  PolyN, PcType, I, Ofs, Ofs2, PcSize: Integer;
begin
  Result := nil;
  if FFont.Handle = 0 then
    Exit;

  M2.eM11.value := 1;
  M2.eM11.fract := 1;

  M2.eM12.value := 0;
  M2.eM12.fract := 1;

  M2.eM21.value := 0;
  M2.eM21.fract := 1;
  
  M2.eM22.value := 1;
  M2.eM22.fract := 1;

  try
    DC := GetDC(0);
    try
      MDC := CreateCompatibleDC(DC);
    finally
      ReleaseDC(0, DC);
    end;

    FFont.Size := EMSIZE;
    OFont := SelectObject(MDC, FFont.Handle);

    BufSize := GetGlyphOutline(MDC, CharCode, GGO_NATIVE, Gm, 0, nil, M2);

    if (BufSize = GDI_ERROR) or (BufSize = 0) then
    begin
      if BufSize = 0 then
      begin
        if CharCode = 0 then CharCode := 32;
        S := Char(CharCode);

        Size.cX := 0;
        Size.cY := 0;
        Windows.GetTextExtentPoint32(MDC, PChar(S), 1, Size);

        Result := TSCDeStrokes.Create;

        with Result do
        begin
          FBlackBoxX := Size.cX;
          FBlackBoxY := Size.cY;
        end;
      end;

      SelectObject(MDC, OFont);
      DeleteDC(MDC);

      Exit;
    end;

    BufPtr := AllocMem(BufSize);
    Buf := BufPtr;

    Res := GetGlyphOutline(MDC, CharCode, GGO_NATIVE, Gm, BufSize, PChar(Buf), M2);

    SelectObject(MDC, OFont);
    DeleteDC(MDC);

    if (Res = GDI_ERROR) or (Buf^.dwType <> TT_POLYGON_TYPE) then
    begin
      FreeMem(BufPtr);
      Exit;
    end;

    Result := TSCDeStrokes.Create;

    with Result do
    begin
      FCellIncX  := Gm.gmCellIncX;
      FCellIncY  := Gm.gmCellIncY;
      FBlackBoxX := Gm.gmBlackBoxX;
      FBlackBoxY := Gm.gmBlackBoxY;
      FGlyphOrigin := Gm.gmptGlyphOrigin;
    end;

    Done := False;
    Ofs := 0;
    PolyN := 0;

    while not Done do
    begin
      Ps.X := TSCDeUtils.FixToDouble(Buf^.pfxStart.x);
      Ps.Y := TSCDeUtils.FixToDouble(Buf^.pfxStart.y);

      PcSize := Buf^.cb - SizeOf(TTTPOLYGONHEADER);
      PChar(PC) := PChar(Buf) + SizeOf(TTTPOLYGONHEADER);

      Ofs2 := 0;
      P2 := Ps;

      while not Done and (Ofs2 < PcSize) do
      begin
        PcType := Pc^.wType;

        case pcType of
          TT_PRIM_LINE:
          begin
            lpAPFX := @Pc^.apfx[0];

            for I := 0 to Pc^.cpfx - 1 do
            begin
              P1 := P2;

              P2.x := TSCDeUtils.FixToDouble(lpAPFX^[i].x);
              P2.y := TSCDeUtils.FixToDouble(lpAPFX^[i].y);

              if not TSCDeUtils.EqualPoint(P1, P2) then
                Result.Add(NewStroke(PolyN, P1, P2));
            end;
          end;
          TT_PRIM_QSPLINE:
          begin
            lpAPFX := @Pc^.apfx[0];
            PfxA := P2;

            for I := 0 to Pc^.cpfx - 2 do
            begin
              PfxB.X := TSCDeUtils.FixToDouble(lpAPFX^[i].x);
              PfxB.Y := TSCDeUtils.FixToDouble(lpAPFX^[i].y);

              if i < Pc^.cpfx - 2 then
              begin
                PfxC.x := TSCDeUtils.FixToDouble(lpAPFX^[i + 1].x);
                PfxC.y := TSCDeUtils.FixToDouble(lpAPFX^[i + 1].y);
              
                PfxC.x := (PfxC.x + PfxB.x) / 2;
                PfxC.y := (PfxC.y + PfxB.y) / 2;
              end else
              begin
                PfxC.x := TSCDeUtils.FixToDouble(lpAPFX^[i + 1].x);
                PfxC.y := TSCDeUtils.FixToDouble(lpAPFX^[i + 1].y);
              end;

              DrawQSpline(PolyN, PfxA, PfxB, PfxC, Result);
              PfxA := PfxC;
            end;

            P2 := PfxC;
          end;
        end;
      
        Ofs2 := Ofs2 + SizeOf(TTTPOLYCURVE) + (Pc^.cpfx-1)*SizeOf(TPOINTFX);
        PChar(PC) := PChar(PC) + SizeOf(TTTPOLYCURVE) + (Pc^.cpfx-1)*SizeOf(TPOINTFX);
      end;

      if not Done then
      begin
        P1 := P2;
        P2 := Ps;

        if not TSCDeUtils.EqualPoint(P1, P2) then
          Result.Add(NewStroke(PolyN, P1, P2));

        Ofs := Ofs + PcSize + SizeOf(TTTPOLYGONHEADER);
        Done := Ofs >= Integer(BufSize) - SizeOf(TTTPolygonHeader);

        PChar(Buf) := PChar(Pc);
        Inc(PolyN);
      end;
    end;

    FreeMem(BufPtr);
  except
  end;  
end;

function TSCDeTTF.NewStroke(GlyphNumber: Integer; var P1,
  P2: TDoublePoint): PSCDeFontStroke;
begin
  New(Result);

  Result^.GlyphNumber := GlyphNumber;

  Result^.P1.X := Round(P1.X/EMSIZE*SIZEX);
  Result^.P1.Y := Round(P1.Y/EMSIZE*SIZEY);
  Result^.P2.X := Round(P2.X/EMSIZE*SIZEX);
  Result^.P2.Y := Round(P2.Y/EMSIZE*SIZEY);
end;


procedure TSCDeTTF.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TSCDeTTF.SetSplinePrecision(Value: Integer);
begin
  if Value < 1 then Value := 1;
  if Value > 100 then Value := 100;

  FSplinePrecision := Value;
end;

{ TSCDeStrokes }

procedure TSCDeStrokes.ClearStrokes;
var
  I: Integer;
begin
  for I := Count-1 downto 0 do
  begin
    FreeMem(Items[I]);
    Items[I] := nil;
  end;

  Pack;
end;

destructor TSCDeStrokes.Destroy;
begin
  ClearStrokes;
  inherited Destroy;
end;

procedure TSCDeStrokes.EnumStrokes(Callback: TEnumSCDeStrokesCallback);
var
  I: Integer;
begin
  if Assigned(Callback) then
    for I := 0 to Count-1 do
      if not Callback(I, PSCDeFontStroke(Items[I])^) then
        Break;
end;

function TSCDeStrokes.GetBounds: TDoubleRect;
var
  I: Integer;
  Fs: PSCDeFontStroke;
begin
  if Count = 0 then
  begin
    Result := TSCDeUtils.Rect(0, 0, FBlackBoxX, FBlackBoxY);
    Exit;
  end;

  Result := TSCDeUtils.Rect(MaxInt, MaxInt, -MaxInt, -MaxInt);

  for I := 0 to Count-1 do
  begin
    Fs := Items[I];

    with Result, Fs^.P1 do
    begin
      if x < Left then Left := x;
      if x > Right then Right := x;
      if y < Top then Top := y;
      if y > Bottom then Bottom := y;
    end;

    with Result, Fs^.P2 do
    begin
      if x < Left then Left := x;
      if x > Right then Right := x;
      if y < Top then Top := y;
      if y > Bottom then Bottom := y;
    end;
  end;
end;

function TSCDeStrokes.GeTSCDeFontStroke(Index: Integer): TSCDeFontStroke;
begin
  Result := PSCDeFontStroke(Items[Index])^;
end;

function TSCDeStrokes.GetNumGlyphs: Integer;
begin
  Result := 0;
  if Count > 0 then
    Result := PSCDeFontStroke(Items[Count-1])^.GlyphNumber + 1;
end;

function TSCDeStrokes.GlyphNumStrokes(GlyphNumber: Integer): Integer;
var
  Sog, Eog: Integer;
begin
  Sog := StartOfGlyph(GlyphNumber);

  Result := -1;
  if Sog > -1 then
  begin
    Eog := StartOfGlyph(GlyphNumber + 1);
    if Eog < 0 then Eog := Count;

    Result := Eog - Sog;
  end;
end;

function TSCDeStrokes.StartOfGlyph(GlyphNumber: Integer): Integer;
var
  Ng, I: Integer;
begin
  Ng := GetNumGlyphs;

  Result := -1;
  if (GlyphNumber > 0) and (GlyphNumber < Ng) then
    for I := 0 to Count-1 do
      if PSCDeFontStroke(Items[I])^.GlyphNumber = GlyphNumber then
      begin
        Result := I;
        Break;
      end;
end;

constructor TSCDeStrokes.Create;
begin
  inherited Create;
  FCellIncX := 0;
  FCellIncY := 0;
  FBlackBoxX := 0;
  FBlackBoxY := 0;
  FGlyphOrigin := Point(0, 0);
end;

end.
