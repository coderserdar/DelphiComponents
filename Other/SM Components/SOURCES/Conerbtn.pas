{ Copyright (C) 1998-2003, written by Shkolnik Mike, Scalabium
  E-Mail:  mshkolnik@scalabium.com
           mshkolnik@yahoo.com
  WEB: http://www.scalabium.com
       http://www.geocities.com/mshkolnik
  tel: 380-/44/-552-10-29

  TConerBtn component is a extended button with new style.
  It's an extended TBitBtn (caption with glyph):
  by PlaceConer property switch you can define a
  "button coner" and turn on/off the "light" in this coner

  PlaceConer property is controled the "coner" layout:
  pcNone - turn off - in this case it's a standard TBitBtn component
  pcTopLeft -
  pcTopRight -
  pcBottomLeft -
  pcBottomRight - 

  TConvexBtn component - is a extension of standard TButton, but
  have a look like 3D cylinder
}
unit ConerBtn;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, StdCtrls, Buttons;

type
  TPlaceConer = (pcNone, pcTopLeft, pcTopRight, pcBottomLeft, pcBottomRight);
  TSymbolState = (ssNone, ssOpen, ssClose);

type
  TPlaceSymbol = class(TPersistent)
  private
    FState: TSymbolState;
    FColorOpen: TColor;
    FColorClose: TColor;
  published
    property State: TSymbolState read FState write FState;
    property ColorOpen: TColor read FColorOpen write FColorOpen;
    property ColorClose: TColor read FColorClose write FColorClose;
  end;

type
  TConerBtn = class(TBitBtn)
  private
    { Private declarations }
    FCanvas: TCanvas;
    FFlat: Boolean;
    FTransparent: Boolean;
    FGlyph: Pointer;
    FPlaceConer: TPlaceConer;
    IsFocused: Boolean;

//    FPlaceSymbol: TPlaceSymbol;
    FSymbolState: TSymbolState;
    FSymbolColorOpen: TColor;
    FSymbolColorClose: TColor;

    procedure DrawCornerSymbol(R: TRect; Flags: Longint);
    procedure DrawCornerButton(R: TRect; Flags: Longint);
    procedure DrawItem(const DrawItemStruct: TDrawItemStruct);
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure SetFlat(Value: Boolean);
    procedure SetPlaceConer(Value: TPlaceConer);
    procedure SetSymbolState(Value: TSymbolState);
    procedure SetSymbolColorOpen(Value: TColor);
    procedure SetSymbolColorClose(Value: TColor);
    procedure SetTransparent(Value: Boolean);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property Flat: Boolean read FFlat write SetFlat;
    property Color;
    property PlaceConer: TPlaceConer read FPlaceConer write SetPlaceConer;
//    property PlaceSymbol: TPlaceSymbol read FPlaceSymbol write SetPlaceSymbol;

    property SymbolState: TSymbolState read FSymbolState write SetSymbolState;
    property SymbolColorOpen: TColor read FSymbolColorOpen write SetSymbolColorOpen;
    property SymbolColorClose: TColor read FSymbolColorClose write SetSymbolColorClose;
    property Transparent: Boolean read FTransparent write SetTransparent;

  end;

type
  TConvexBtn = class(TBitBtn) //Button)
  private
    { Private declarations }
    FCanvas: TCanvas;
    IsFocused: Boolean;
    FTransparent: Boolean;
    FGlyph: Pointer;
    procedure DrawItem(const DrawItemStruct: TDrawItemStruct);
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure SetTransparent(Value: Boolean);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property Transparent: Boolean read FTransparent write SetTransparent;
    property Color;
  end;

procedure Register;

implementation
uses TypInfo, CommCtrl;

procedure Register;
begin
  RegisterComponents('SMComponents', [TConerBtn, TConvexBtn]);
end;

{!!!! some idiot from Borland/Inprise keep
 a TGlyphList, TGlyphCache and TButtonGlyph types
 in the implementation section of the Buttons.Pas
 and I must to do copy of them}
type
  TGlyphList = class(TImageList)
  private
    Used: TBits;
    FCount: Integer;
    function AllocateIndex: Integer;
  public
    constructor CreateSize(AWidth, AHeight: Integer);
    destructor Destroy; override;
    function AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
    procedure Delete(Index: Integer);
    property Count: Integer read FCount;
  end;

  TGlyphCache = class
  private
    GlyphLists: TList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetList(AWidth, AHeight: Integer): TGlyphList;
    procedure ReturnList(List: TGlyphList);
    function Empty: Boolean;
  end;

  TButtonGlyph = class
  private
    FOriginal: TBitmap;
    FGlyphList: TGlyphList;
    FIndexs: array[TButtonState] of Integer;
    FTransparentColor: TColor;
    FNumGlyphs: TNumGlyphs;
    FOnChange: TNotifyEvent;
    procedure GlyphChanged(Sender: TObject);
    procedure SetGlyph(Value: TBitmap);
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure Invalidate;
    function CreateButtonGlyph(State: TButtonState): Integer;
    procedure DrawButtonGlyph(Canvas: TCanvas; const GlyphPos: TPoint;
      State: TButtonState; Transparent: Boolean);
    procedure DrawButtonText(Canvas: TCanvas; const Caption: string;
      TextBounds: TRect; State: TButtonState);
    procedure CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
      const Offset: TPoint; const Caption: string; Layout: TButtonLayout;
      Margin, Spacing: Integer; var GlyphPos: TPoint; var TextBounds: TRect);
  public
    constructor Create;
    destructor Destroy; override;
    { return the text rectangle }
    function Draw(Canvas: TCanvas; const Client: TRect; const Offset: TPoint;
      const Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
      State: TButtonState; Transparent: Boolean): TRect;
    property Glyph: TBitmap read FOriginal write SetGlyph;
    property NumGlyphs: TNumGlyphs read FNumGlyphs write SetNumGlyphs;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{ TGlyphList }

constructor TGlyphList.CreateSize(AWidth, AHeight: Integer);
begin
  inherited CreateSize(AWidth, AHeight);
  Used := TBits.Create;
end;

destructor TGlyphList.Destroy;
begin
  Used.Free;
  inherited Destroy;
end;

function TGlyphList.AllocateIndex: Integer;
begin
  Result := Used.OpenBit;
  if Result >= Used.Size then
  begin
    Result := inherited Add(nil, nil);
    Used.Size := Result + 1;
  end;
  Used[Result] := True;
end;

function TGlyphList.AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
begin
  Result := AllocateIndex;
  ReplaceMasked(Result, Image, MaskColor);
  Inc(FCount);
end;

procedure TGlyphList.Delete(Index: Integer);
begin
  if Used[Index] then
  begin
    Dec(FCount);
    Used[Index] := False;
  end;
end;

{ TGlyphCache }

constructor TGlyphCache.Create;
begin
  inherited Create;
  GlyphLists := TList.Create;
end;

destructor TGlyphCache.Destroy;
begin
  GlyphLists.Free;
  inherited Destroy;
end;

function TGlyphCache.GetList(AWidth, AHeight: Integer): TGlyphList;
var
  I: Integer;
begin
  for I := GlyphLists.Count - 1 downto 0 do
  begin
    Result := GlyphLists[I];
    with Result do
      if (AWidth = Width) and (AHeight = Height) then Exit;
  end;
  Result := TGlyphList.CreateSize(AWidth, AHeight);
  GlyphLists.Add(Result);
end;

procedure TGlyphCache.ReturnList(List: TGlyphList);
begin
  if List = nil then Exit;
  if List.Count = 0 then
  begin
    GlyphLists.Remove(List);
    List.Free;
  end;
end;

function TGlyphCache.Empty: Boolean;
begin
  Result := GlyphLists.Count = 0;
end;

var
  GlyphCache: TGlyphCache = nil;

{ TButtonGlyph }
constructor TButtonGlyph.Create;
var
  I: TButtonState;
begin
  inherited Create;
  FOriginal := TBitmap.Create;
  FOriginal.OnChange := GlyphChanged;
  FTransparentColor := clOlive;
  FNumGlyphs := 1;
  for I := Low(I) to High(I) do
    FIndexs[I] := -1;
  if GlyphCache = nil then GlyphCache := TGlyphCache.Create;
end;

destructor TButtonGlyph.Destroy;
begin
  FOriginal.Free;
  Invalidate;
  if Assigned(GlyphCache) and GlyphCache.Empty then
  begin
    GlyphCache.Free;
    GlyphCache := nil;
  end;
  inherited Destroy;
end;

procedure TButtonGlyph.Invalidate;
var
  I: TButtonState;
begin
  for I := Low(I) to High(I) do
  begin
    if FIndexs[I] <> -1 then FGlyphList.Delete(FIndexs[I]);
    FIndexs[I] := -1;
  end;
  GlyphCache.ReturnList(FGlyphList);
  FGlyphList := nil;
end;

procedure TButtonGlyph.GlyphChanged(Sender: TObject);
begin
  if Sender = FOriginal then
  begin
    FTransparentColor := FOriginal.TransparentColor;
    Invalidate;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TButtonGlyph.SetGlyph(Value: TBitmap);
var
  Glyphs: Integer;
begin
  Invalidate;
  FOriginal.Assign(Value);
  if (Value <> nil) and (Value.Height > 0) then
  begin
    FTransparentColor := Value.TransparentColor;
    if Value.Width mod Value.Height = 0 then
    begin
      Glyphs := Value.Width div Value.Height;
      if Glyphs > 4 then Glyphs := 1;
      SetNumGlyphs(Glyphs);
    end;
  end;
end;

procedure TButtonGlyph.SetNumGlyphs(Value: TNumGlyphs);
begin
  if (Value <> FNumGlyphs) and (Value > 0) then
  begin
    Invalidate;
    FNumGlyphs := Value;
    GlyphChanged(Glyph);
  end;
end;

function TButtonGlyph.CreateButtonGlyph(State: TButtonState): Integer;
const
  ROP_DSPDxax = $00E20746;
var
  TmpImage, DDB, MonoBmp: TBitmap;
  IWidth, IHeight: Integer;
  IRect, ORect: TRect;
  I: TButtonState;
  DestDC: HDC;
begin
  if (State = bsDown) and (NumGlyphs < 3) then State := bsUp;
  Result := FIndexs[State];
  if Result <> -1 then Exit;
  if (FOriginal.Width or FOriginal.Height) = 0 then Exit;
  IWidth := FOriginal.Width div FNumGlyphs;
  IHeight := FOriginal.Height;
  if FGlyphList = nil then
  begin
    if GlyphCache = nil then GlyphCache := TGlyphCache.Create;
    FGlyphList := GlyphCache.GetList(IWidth, IHeight);
  end;
  TmpImage := TBitmap.Create;
  try
    TmpImage.Width := IWidth;
    TmpImage.Height := IHeight;
    IRect := Rect(0, 0, IWidth, IHeight);
    TmpImage.Canvas.Brush.Color := clBtnFace;
    TmpImage.Palette := CopyPalette(FOriginal.Palette);
    I := State;
    if Ord(I) >= NumGlyphs then I := bsUp;
    ORect := Rect(Ord(I) * IWidth, 0, (Ord(I) + 1) * IWidth, IHeight);
    case State of
      bsUp, bsDown,
      bsExclusive:
        begin
          TmpImage.Canvas.CopyRect(IRect, FOriginal.Canvas, ORect);
          if FOriginal.TransparentMode = tmFixed then
            FIndexs[State] := FGlyphList.AddMasked(TmpImage, FTransparentColor)
          else
            FIndexs[State] := FGlyphList.AddMasked(TmpImage, clDefault);
        end;
      bsDisabled:
        begin
          MonoBmp := nil;
          DDB := nil;
          try
            MonoBmp := TBitmap.Create;
            DDB := TBitmap.Create;
            DDB.Assign(FOriginal);
            DDB.HandleType := bmDDB;
            if NumGlyphs > 1 then
            with TmpImage.Canvas do
            begin    { Change white & gray to clBtnHighlight and clBtnShadow }
              CopyRect(IRect, DDB.Canvas, ORect);
              MonoBmp.Monochrome := True;
              MonoBmp.Width := IWidth;
              MonoBmp.Height := IHeight;

              { Convert white to clBtnHighlight }
              DDB.Canvas.Brush.Color := clWhite;
              MonoBmp.Canvas.CopyRect(IRect, DDB.Canvas, ORect);
              Brush.Color := clBtnHighlight;
              DestDC := Handle;
              SetTextColor(DestDC, clBlack);
              SetBkColor(DestDC, clWhite);
              BitBlt(DestDC, 0, 0, IWidth, IHeight,
                     MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);

              { Convert gray to clBtnShadow }
              DDB.Canvas.Brush.Color := clGray;
              MonoBmp.Canvas.CopyRect(IRect, DDB.Canvas, ORect);
              Brush.Color := clBtnShadow;
              DestDC := Handle;
              SetTextColor(DestDC, clBlack);
              SetBkColor(DestDC, clWhite);
              BitBlt(DestDC, 0, 0, IWidth, IHeight,
                     MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);

              { Convert transparent color to clBtnFace }
              DDB.Canvas.Brush.Color := ColorToRGB(FTransparentColor);
              MonoBmp.Canvas.CopyRect(IRect, DDB.Canvas, ORect);
              Brush.Color := clBtnFace;
              DestDC := Handle;
              SetTextColor(DestDC, clBlack);
              SetBkColor(DestDC, clWhite);
              BitBlt(DestDC, 0, 0, IWidth, IHeight,
                     MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
            end
            else
            begin
              { Create a disabled version }
              with MonoBmp do
              begin
                Assign(FOriginal);
                HandleType := bmDDB;
                Canvas.Brush.Color := clBlack;
                Width := IWidth;
                if Monochrome then
                begin
                  Canvas.Font.Color := clWhite;
                  Monochrome := False;
                  Canvas.Brush.Color := clWhite;
                end;
                Monochrome := True;
              end;
              with TmpImage.Canvas do
              begin
                Brush.Color := clBtnFace;
                FillRect(IRect);
                Brush.Color := clBtnHighlight;
                SetTextColor(Handle, clBlack);
                SetBkColor(Handle, clWhite);
                BitBlt(Handle, 1, 1, IWidth, IHeight,
                  MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
                Brush.Color := clBtnShadow;
                SetTextColor(Handle, clBlack);
                SetBkColor(Handle, clWhite);
                BitBlt(Handle, 0, 0, IWidth, IHeight,
                  MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
              end;
            end;
          finally
            DDB.Free;
            MonoBmp.Free;
          end;
          FIndexs[State] := FGlyphList.AddMasked(TmpImage, clDefault);
        end;
    end;
  finally
    TmpImage.Free;
  end;
  Result := FIndexs[State];
  FOriginal.Dormant;
end;

procedure TButtonGlyph.DrawButtonGlyph(Canvas: TCanvas; const GlyphPos: TPoint;
  State: TButtonState; Transparent: Boolean);
var
  Index: Integer;
begin
  if FOriginal = nil then Exit;
  if (FOriginal.Width = 0) or (FOriginal.Height = 0) then Exit;
  Index := CreateButtonGlyph(State);
  with GlyphPos do
    if Transparent or (State = bsExclusive) then
      ImageList_DrawEx(FGlyphList.Handle, Index, Canvas.Handle, X, Y, 0, 0,
        clNone, clNone, ILD_Transparent)
    else
      ImageList_DrawEx(FGlyphList.Handle, Index, Canvas.Handle, X, Y, 0, 0,
        ColorToRGB(clBtnFace), clNone, ILD_Normal);
end;

procedure TButtonGlyph.DrawButtonText(Canvas: TCanvas; const Caption: string;
  TextBounds: TRect; State: TButtonState);
begin
  with Canvas do
  begin
    Brush.Style := bsClear;

    if State = bsDisabled then
    begin
      OffsetRect(TextBounds, 1, 1);
      Font.Color := clBtnHighlight;
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, 0);
      OffsetRect(TextBounds, -1, -1);
      Font.Color := clBtnShadow;
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, 0);
    end else
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds,
        DT_CENTER or DT_VCENTER or DT_SINGLELINE);
  end;
end;

procedure TButtonGlyph.CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
  const Offset: TPoint; const Caption: string; Layout: TButtonLayout; Margin,
  Spacing: Integer; var GlyphPos: TPoint; var TextBounds: TRect);
var
  TextPos: TPoint;
  ClientSize, GlyphSize, TextSize: TPoint;
  TotalSize: TPoint;
begin
  { calculate the item sizes }
  ClientSize := Point(Client.Right - Client.Left, Client.Bottom -
    Client.Top);

  if FOriginal <> nil then
    GlyphSize := Point(FOriginal.Width div FNumGlyphs, FOriginal.Height) else
    GlyphSize := Point(0, 0);

  if Length(Caption) > 0 then
  begin
    TextBounds := Rect(0, 0, Client.Right - Client.Left, 0);
    DrawText(Canvas.Handle, PChar(Caption), Length(Caption), TextBounds, DT_CALCRECT);
    TextSize := Point(TextBounds.Right - TextBounds.Left, TextBounds.Bottom -
      TextBounds.Top);
  end
  else
  begin
    TextBounds := Rect(0, 0, 0, 0);
    TextSize := Point(0,0);
  end;

  { If the layout has the glyph on the right or the left, then both the
    text and the glyph are centered vertically.  If the glyph is on the top
    or the bottom, then both the text and the glyph are centered horizontally.}
  if Layout in [blGlyphLeft, blGlyphRight] then
  begin
    GlyphPos.Y := (ClientSize.Y - GlyphSize.Y + 1) div 2;
    TextPos.Y := (ClientSize.Y - TextSize.Y + 1) div 2;
  end
  else
  begin
    GlyphPos.X := (ClientSize.X - GlyphSize.X + 1) div 2;
    TextPos.X := (ClientSize.X - TextSize.X + 1) div 2;
  end;

  { if there is no text or no bitmap, then Spacing is irrelevant }
  if (TextSize.X = 0) or (GlyphSize.X = 0) then
    Spacing := 0;

  { adjust Margin and Spacing }
  if Margin = -1 then
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point(GlyphSize.X + TextSize.X, GlyphSize.Y + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X) div 3
      else
        Margin := (ClientSize.Y - TotalSize.Y) div 3;
      Spacing := Margin;
    end
    else
    begin
      TotalSize := Point(GlyphSize.X + Spacing + TextSize.X, GlyphSize.Y +
        Spacing + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X + 1) div 2
      else
        Margin := (ClientSize.Y - TotalSize.Y + 1) div 2;
    end;
  end
  else
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point(ClientSize.X - (Margin + GlyphSize.X), ClientSize.Y -
        (Margin + GlyphSize.Y));
      if Layout in [blGlyphLeft, blGlyphRight] then
        Spacing := (TotalSize.X - TextSize.X) div 2
      else
        Spacing := (TotalSize.Y - TextSize.Y) div 2;
    end;
  end;

  case Layout of
    blGlyphLeft:
      begin
        GlyphPos.X := Margin;
        TextPos.X := GlyphPos.X + GlyphSize.X + Spacing;
      end;
    blGlyphRight:
      begin
        GlyphPos.X := ClientSize.X - Margin - GlyphSize.X;
        TextPos.X := GlyphPos.X - Spacing - TextSize.X;
      end;
    blGlyphTop:
      begin
        GlyphPos.Y := Margin;
        TextPos.Y := GlyphPos.Y + GlyphSize.Y + Spacing;
      end;
    blGlyphBottom:
      begin
        GlyphPos.Y := ClientSize.Y - Margin - GlyphSize.Y;
        TextPos.Y := GlyphPos.Y - Spacing - TextSize.Y;
      end;
  end;

  { fixup the result variables }
  with GlyphPos do
  begin
    Inc(X, Client.Left + Offset.X);
    Inc(Y, Client.Top + Offset.Y);
  end;
  OffsetRect(TextBounds, TextPos.X + Client.Left + Offset.X,
    TextPos.Y + Client.Top + Offset.X);
end;

function TButtonGlyph.Draw(Canvas: TCanvas; const Client: TRect;
  const Offset: TPoint; const Caption: string; Layout: TButtonLayout;
  Margin, Spacing: Integer; State: TButtonState; Transparent: Boolean): TRect;
var
  GlyphPos: TPoint;
begin
  CalcButtonLayout(Canvas, Client, Offset, Caption, Layout, Margin, Spacing,
    GlyphPos, Result);
  DrawButtonGlyph(Canvas, GlyphPos, State, Transparent);
  DrawButtonText(Canvas, Caption, Result, State);
end;

{TConerBtn}
constructor TConerBtn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FSymbolState := ssClose;
  FSymbolColorOpen := clRed;
  FSymbolColorClose := clMaroon;

  FGlyph := TButtonGlyph.Create;
  FCanvas := TCanvas.Create;

  ControlStyle := [csOpaque];

end;

destructor TConerBtn.Destroy;
begin
  TButtonGlyph(FGlyph).Free;
  FCanvas.Free;

  inherited Destroy;
end;

procedure TConerBtn.CNDrawItem(var Message: TWMDrawItem);
begin
  DrawItem(Message.DrawItemStruct^);
end;

procedure TConerBtn.DrawCornerSymbol(R: TRect; Flags: Longint);
var CornerX, CornerY, Shift: Integer;
    Point1, Point2, Point3: TPoint;
begin
  CornerX := (R.Right - R.Left) div 3;
  CornerY := (R.Bottom - R.Top) div 3;
  if (CornerX < CornerY) then
    CornerY := CornerX
  else
    CornerX := CornerY;
  Shift := 6;

  case FPlaceConer of
    pcTopLeft: begin
                 Point1 := Point(R.Left + Shift, R.Bottom - CornerY - Shift);
                 Point2 := Point(R.Left + Shift, R.Bottom - Shift);
                 Point3 := Point(R.Left + CornerX + Shift, R.Bottom - Shift);
               end;
    pcTopRight: begin
                  Point1 := Point(R.Right - Shift, R.Bottom - CornerY - Shift);
                  Point2 := Point(R.Right - Shift, R.Bottom - Shift);
                  Point3 := Point(R.Right - CornerX - Shift, R.Bottom - Shift);
                end;
    pcBottomLeft: begin
                    Point1 := Point(R.Left + CornerX + Shift, R.Top + Shift);
                    Point2 := Point(R.Left + Shift, R.Top + Shift);
                    Point3 := Point(R.Left + Shift, R.Top + CornerY + Shift);
                  end;
    pcBottomRight: begin
                     Point1 := Point(R.Right - CornerX - Shift, R.Top + Shift);
                     Point2 := Point(R.Right - Shift, R.Top + Shift);
                     Point3 := Point(R.Right - Shift, R.Top + CornerY + Shift);
                   end;
  end;
{
  case FPlaceConer of
    pcTopLeft: begin
                 Point1 := Point(R.Left + (8*CornerX div 9), R.Top + (CornerY div 3));
                 Point2 := Point(R.Left + (8*CornerX div 9), R.Top + (8*CornerY div 9));
                 Point3 := Point(R.Left + (CornerX div 3), R.Top + (8*CornerY div 9));
               end;
    pcTopRight: begin
                  Point1 := Point(R.Right - (8*CornerX div 9), R.Top + (CornerY div 3));
                  Point2 := Point(R.Right - (8*CornerX div 9), R.Top + (8*CornerY div 9));
                  Point3 := Point(R.Right - (CornerX div 3), R.Top + (8*CornerY div 9));
                end;
    pcBottomLeft: begin
                    Point1 := Point(R.Left + (CornerX div 3), R.Bottom - (8*CornerY div 9));
                    Point2 := Point(R.Left + (8*CornerX div 9), R.Bottom - (8*CornerY div 9));
                    Point3 := Point(R.Left + (8*CornerX div 9), R.Bottom - (CornerY div 3));
                  end;
    pcBottomRight: begin
                     Point1 := Point(R.Right - (CornerX div 3), R.Bottom - (8*CornerY div 9));
                     Point2 := Point(R.Right - (8*CornerX div 9), R.Bottom - (8*CornerY div 9));
                     Point3 := Point(R.Right - (8*CornerX div 9), R.Bottom - (CornerY div 3));
                   end;
  end;
}
  {draw top of the button symbol}
  FCanvas.Pen.Width := 1;

  if (FSymbolState = ssOpen) then
    FCanvas.Brush.Color := FSymbolColorOpen
  else
    FCanvas.Brush.Color := FSymbolColorClose;

  if Flags and DFCS_PUSHED <> 0 then
    FCanvas.Pen.Color := clBtnShadow
  else
    FCanvas.Pen.Color := clBtnHighlight;

  FCanvas.Polygon([Point1, Point2, Point3]);

  {draw bottom of the button symbol}
  FCanvas.Pen.Width := 1;
  if Flags and DFCS_PUSHED <> 0 then
    FCanvas.Pen.Color := clBtnHighlight
  else
    FCanvas.Pen.Color := clBtnShadow;

  FCanvas.Polyline([Point1, Point3]);

end;

procedure TConerBtn.DrawCornerButton(R: TRect; Flags: Longint);
var CornerX, CornerY: Integer;
begin
  CornerX := (R.Right - R.Left) div 3;
  CornerY := (R.Bottom - R.Top) div 3;
  if (CornerX < CornerY) then
    CornerY := CornerX
  else
    CornerX := CornerY;

  FCanvas.Pen.Style := psSolid;
  if (Flags = 0) then
  begin
    if FTransparent then
      case FPlaceConer of
        pcNone: FCanvas.Polyline([Point(R.Left, R.Top),
                                  Point(R.Right-1, R.Top),
                                  Point(R.Right-1, R.Bottom-1),
                                  Point(R.Left, R.Bottom-1)]);
        pcTopLeft: FCanvas.Polyline([Point(R.Left + CornerX, R.Top),
                                     Point(R.Right-1, R.Top),
                                     Point(R.Right-1, R.Bottom-1),
                                     Point(R.Left, R.Bottom-1),
                                     Point(R.Left, R.Top + CornerY)]);
        pcTopRight: FCanvas.Polyline([Point(R.Left, R.Top),
                                      Point(R.Right - CornerX, R.Top),
                                      Point(R.Right-1, R.Top + CornerY),
                                      Point(R.Right-1, R.Bottom-1),
                                      Point(R.Left, R.Bottom-1)]);
        pcBottomLeft: FCanvas.Polyline([Point(R.Left, R.Top),
                                        Point(R.Right-1, R.Top),
                                        Point(R.Right-1, R.Bottom-1),
                                        Point(R.Left + CornerX, R.Bottom-1),
                                        Point(R.Left, R.Bottom - CornerY)]);
        pcBottomRight: FCanvas.Polyline([Point(R.Left, R.Top),
                                         Point(R.Right-1, R.Top),
                                         Point(R.Right-1, R.Bottom - CornerY),
                                         Point(R.Right - CornerX, R.Bottom-1),
                                         Point(R.Left, R.Bottom-1)]);
     end
    else
      case FPlaceConer of
        pcNone: FCanvas.Polygon([Point(R.Left, R.Top),
                                 Point(R.Right-1, R.Top),
                                 Point(R.Right-1, R.Bottom-1),
                                 Point(R.Left, R.Bottom-1)]);
        pcTopLeft: FCanvas.Polygon([Point(R.Left + CornerX, R.Top),
                                    Point(R.Right-1, R.Top),
                                    Point(R.Right-1, R.Bottom-1),
                                    Point(R.Left, R.Bottom-1),
                                    Point(R.Left, R.Top + CornerY)]);
        pcTopRight: FCanvas.Polygon([Point(R.Left, R.Top),
                                     Point(R.Right - CornerX, R.Top),
                                     Point(R.Right-1, R.Top + CornerY),
                                     Point(R.Right-1, R.Bottom-1),
                                     Point(R.Left, R.Bottom-1)]);
        pcBottomLeft: FCanvas.Polygon([Point(R.Left, R.Top),
                                       Point(R.Right-1, R.Top),
                                       Point(R.Right-1, R.Bottom-1),
                                       Point(R.Left + CornerX, R.Bottom-1),
                                       Point(R.Left, R.Bottom - CornerY)]);
        pcBottomRight: FCanvas.Polygon([Point(R.Left, R.Top),
                                        Point(R.Right-1, R.Top),
                                        Point(R.Right-1, R.Bottom - CornerY),
                                        Point(R.Right - CornerX, R.Bottom-1),
                                        Point(R.Left, R.Bottom-1)]);
     end;
  end
  else
  begin
    {draw top of the button}
    if (Flags = -1) then
    begin
      FCanvas.Pen.Style := psDot;
      FCanvas.Pen.Color := clWindowFrame;
    end
    else
    begin
      if Flags and DFCS_PUSHED <> 0 then
      begin
        FCanvas.Pen.Color := clBtnShadow;
        FCanvas.Pen.Width := 1;
        FCanvas.Brush.Color := clBtnFace;
      end
      else
      begin
        FCanvas.Pen.Color := clBtnHighlight;
        FCanvas.Pen.Width := 1;
        FCanvas.Brush.Style := bsClear;
      end;
    end;

    case FPlaceConer of
      pcNone: FCanvas.Polyline([Point(R.Left, R.Bottom-1),
                                Point(R.Left, R.Top),
                                Point(R.Right-1, R.Top)]);
      pcTopLeft: FCanvas.Polyline([Point(R.Left, R.Bottom-1),
                                   Point(R.Left, R.Top + CornerY),
                                   Point(R.Left + CornerX, R.Top),
                                   Point(R.Right-1, R.Top)]);
      pcTopRight: FCanvas.Polyline([Point(R.Left, R.Bottom-1),
                                    Point(R.Left, R.Top),
                                    Point(R.Right - CornerX, R.Top),
                                    Point(R.Right-1, R.Top + CornerY)]);
      pcBottomLeft: FCanvas.Polyline([Point(R.Left, R.Bottom - CornerY),
                                      Point(R.Left, R.Top),
                                      Point(R.Right-1, R.Top)]);
      pcBottomRight: FCanvas.Polyline([Point(R.Left, R.Bottom-1),
                                       Point(R.Left, R.Top),
                                       Point(R.Right-1, R.Top)]);
    end;

    {draw bottom of the button}
    if (Flags <> -1) and (Flags and DFCS_PUSHED = 0) then
    begin
      FCanvas.Pen.Color := clBtnShadow;
      FCanvas.Pen.Width := 1;
      FCanvas.Brush.Color := clBtnFace;
    end;

    case FPlaceConer of
      pcNone: FCanvas.Polyline([Point(R.Right-1, R.Top),
                                Point(R.Right-1, R.Bottom-1),
                                Point(R.Left, R.Bottom-1)]);
      pcTopLeft: FCanvas.Polyline([Point(R.Right-1, R.Top),
                                   Point(R.Right-1, R.Bottom-1),
                                   Point(R.Left, R.Bottom-1)]);
      pcTopRight: FCanvas.Polyline([Point(R.Right-1, R.Top + CornerY),
                                    Point(R.Right-1, R.Bottom-1),
                                    Point(R.Left, R.Bottom-1)]);
      pcBottomLeft: FCanvas.Polyline([Point(R.Right-1, R.Top),
                                      Point(R.Right-1, R.Bottom-1),
                                      Point(R.Left + CornerX, R.Bottom-1),
                                      Point(R.Left, R.Bottom - CornerY)]);
      pcBottomRight: FCanvas.Polyline([Point(R.Right-1, R.Top),
                                       Point(R.Right-1, R.Bottom - CornerY),
                                       Point(R.Right - CornerX, R.Bottom-1),
                                       Point(R.Left, R.Bottom-1)]);
    end;
  end;

  InflateRect(R, -1, -1);
end;

procedure TConerBtn.DrawItem(const DrawItemStruct: TDrawItemStruct);
var IsDown, IsDefault: Boolean;
    State: TButtonState;
    R: TRect;
    Flags: Longint;
    PropInfo: PPropInfo;
begin
  FCanvas.Handle := DrawItemStruct.hDC;
  R := ClientRect;

  {clear button area}
  PropInfo := GetPropInfo(Parent.ClassInfo, 'Color');
  if Assigned(PropInfo) then {if such property exists}
  begin
    FCanvas.Pen.Color := GetOrdProp(Parent, PropInfo);
    FCanvas.Brush.Color := FCanvas.Pen.Color;
    FCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
  end;

  if not FTransparent then
  begin
    FCanvas.Brush.Style := bsSolid; //bsClear;
    FCanvas.Brush.Color := Color;

    Flags := 0;
    DrawCornerButton(R, Flags);
  end;

  with DrawItemStruct do
  begin
    IsDown := itemState and ODS_SELECTED <> 0;
    IsDefault := itemState and ODS_FOCUS <> 0;

    if not Enabled then
      State := bsDisabled
    else
      if IsDown then
        State := bsDown
      else
        State := bsUp;
  end;

  Flags := 0;
  { DrawFrameControl doesn't allow for drawing a button as the
      default button, so it must be done here. }
  if IsFocused or IsDefault then
  begin
    FCanvas.Pen.Color := clWindowFrame;
    FCanvas.Pen.Width := 1;
    FCanvas.Brush.Style := bsClear;

    DrawCornerButton(R, Flags);

    { DrawFrameControl must draw within this border }
    InflateRect(R, -1, -1);
  end;

  Flags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
  if IsDown then Flags := Flags or DFCS_PUSHED;
  if DrawItemStruct.itemState and ODS_DISABLED <> 0 then
    Flags := Flags or DFCS_INACTIVE;

  { DrawFrameControl does not draw a pressed button correctly }
  DrawCornerButton(R, Flags);

  if IsFocused then
  begin
    R := ClientRect;
    InflateRect(R, -1, -1);
  end;

  FCanvas.Font := Self.Font;
  if IsDown then
    OffsetRect(R, 1, 1);

  if (FPlaceConer <> pcNone) and (FSymbolState <> ssNone) then
    DrawCornerSymbol(R, Flags);

  TButtonGlyph(FGlyph).Glyph := Glyph as TBitmap;
  TButtonGlyph(FGlyph).Draw(FCanvas, R, Point(0,0), Caption, Layout, Margin,
    Spacing, State, FTransparent);

  {draw dotted frame for selected button}
  if IsFocused or IsDefault then
  begin
    R := ClientRect;
    InflateRect(R, -4, -4);

    DrawCornerButton(R, -1);
    DrawFocusRect(FCanvas.Handle, R);
  end;

  FCanvas.Handle := 0;
end;

procedure TConerBtn.SetFlat(Value: Boolean);
begin
  if (Value <> FFlat) then
  begin
    FFlat := Value;
    Refresh
  end;
end;

procedure TConerBtn.SetPlaceConer(Value: TPlaceConer);
begin
  if (Value <> FPlaceConer) then
  begin
    FPlaceConer := Value;
    Refresh
  end;
end;

procedure TConerBtn.SetSymbolState(Value: TSymbolState);
begin
  if (Value <> FSymbolState) then
  begin
    FSymbolState := Value;
    Refresh;
  end;
end;

procedure TConerBtn.SetSymbolColorOpen(Value: TColor);
begin
  if (Value <> FSymbolColorOpen) then
  begin
    FSymbolColorOpen := Value;
    Refresh;
  end;
end;

procedure TConerBtn.SetSymbolColorClose(Value: TColor);
begin
  if (Value <> FSymbolColorClose) then
  begin
    FSymbolColorClose := Value;
    Refresh;
  end;
end;

procedure TConerBtn.SetTransparent(Value: Boolean);
begin
  if (Value <> FTransparent) then
  begin
    FTransparent := Value;
    Refresh;
  end;
end;



{TConvexBtn}
constructor TConvexBtn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FGlyph := TButtonGlyph.Create;
  FCanvas := TCanvas.Create;
  ControlStyle := [csOpaque];
end;

destructor TConvexBtn.Destroy;
begin
  TButtonGlyph(FGlyph).Free;
  FCanvas.Free;

  inherited Destroy;
end;

procedure TConvexBtn.CNDrawItem(var Message: TWMDrawItem);
begin
  DrawItem(Message.DrawItemStruct^);
end;

procedure TConvexBtn.DrawItem(const DrawItemStruct: TDrawItemStruct);
var IsDown, IsDefault: Boolean;
    State: TButtonState;
    R: TRect;
    Flags: Longint;
begin
  FCanvas.Handle := DrawItemStruct.hDC;
  R := ClientRect;

  with DrawItemStruct do
  begin
    IsDown := itemState and ODS_SELECTED <> 0;
    IsDefault := itemState and ODS_FOCUS <> 0;

    if not Enabled then
      State := bsDisabled
    else
      if IsDown then
        State := bsDown
      else
        State := bsUp;
  end;

  if IsFocused or IsDefault then
  begin
    FCanvas.Pen.Color := clWindowFrame;
    FCanvas.Pen.Width := 1;
    FCanvas.Brush.Style := bsClear;

    FCanvas.Rectangle(R.Left, R.Top, R.Right-1, R.Bottom-1);

    { DrawFrameControl must draw within this border }
    InflateRect(R, -1, -1);
  end;

  FCanvas.Pen.Width := 1;
  FCanvas.Pen.Color := clWindowFrame;
  FCanvas.Brush.Color := Color;
  FCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);

  Flags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
  if IsDown then Flags := Flags or DFCS_PUSHED;
  if DrawItemStruct.itemState and ODS_DISABLED <> 0 then
    Flags := Flags or DFCS_INACTIVE;

  if Flags and DFCS_PUSHED <> 0 then
    FCanvas.Pen.Color := clBtnShadow
  else
    FCanvas.Pen.Color := clBtnHighlight;

  if Flags and DFCS_PUSHED <> 0 then
  begin
{    FCanvas.Pen.Color := clBtnHighlight;
    FCanvas.Pen.Width := 3;
    FCanvas.MoveTo(R.Left+2, R.Bottom-3);
    FCanvas.LineTo(R.Right-3, R.Bottom-3);
}
    FCanvas.Pen.Color := clBtnShadow;
    FCanvas.Pen.Width := 2;
    FCanvas.MoveTo(R.Left+2, R.Top+2);
    FCanvas.LineTo(R.Right-2, R.Top+2);
  end
  else
  begin
    FCanvas.Pen.Color := clBtnHighlight;
    FCanvas.Pen.Width := 2;
    FCanvas.MoveTo(R.Left+2, R.Top+2);
    FCanvas.LineTo(R.Right-2, R.Top+2);

    FCanvas.Pen.Color := clBtnShadow;
    FCanvas.Pen.Width := 3;
    FCanvas.MoveTo(R.Left+2, R.Bottom-3);
    FCanvas.LineTo(R.Right-3, R.Bottom-3);
  end;

  if IsFocused then
  begin
    R := ClientRect;
    InflateRect(R, -1, -1);
  end;

  FCanvas.Font := Self.Font;
  if IsDown then
    OffsetRect(R, 1, 1);

  TButtonGlyph(FGlyph).Glyph := Glyph as TBitmap;
  TButtonGlyph(FGlyph).Draw(FCanvas, R, Point(0,0), Caption, Layout, Margin,
    Spacing, State, FTransparent);

  {draw dotted frame for selected button}
  if IsFocused or IsDefault then
  begin
    R := ClientRect;
    InflateRect(R, -6, -6);

    FCanvas.Pen.Color := clWindowFrame;
    FCanvas.Brush.Color := clBtnFace;
    DrawFocusRect(FCanvas.Handle, R);
  end;

  FCanvas.Handle := 0;
end;

procedure TConvexBtn.SetTransparent(Value: Boolean);
begin
  if (Value <> FTransparent) then
  begin
    FTransparent := Value;
    Refresh;
  end;
end;

end.
