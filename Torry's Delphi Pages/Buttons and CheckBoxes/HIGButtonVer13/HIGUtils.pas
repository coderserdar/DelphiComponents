unit HIGUtils;

interface

uses Windows, Graphics, Messages, Controls, Classes, Buttons, Types, Themes,
  CommCtrl, Forms, SysUtils;

{$R HIGBTNGLYPH.RES}

const
  IG_DROPDOWNMENUPOPUP = WM_USER+110;
  IG_CLOSEUPDROPDOWNMENU = WM_USER + 111;
  IG_BTNINVALIDATE = WM_USER + 112;
  IG_BTNCOLORCHANGE = WM_USER + 113;
  IG_BTNRECREATEWND = WM_USER + 114;
  IG_BTNKINDCHANGE = WM_USER + 115;
  IG_EDITSTYLECHANGE = WM_USER + 116;
  IG_EDITINVALIDATE = WM_USER + 117;
  CM_HIGDeferLayout = WM_USER + 100;
  InitRepeatPause = 400;
  RepeatPause = 100;

type
  THIGEditStyle = (esFlat, esStandard, esUltraFlat);
  THIGEditBtnKind = (ebkDefault, ebkDropDown, ebkEllipsis, ebkGlyph);
  THIGEditListStyle = (elsNormal, elsList);

  THIGColumnValue = (cvColor, cvWidth, cvFont, cvAlignment, cvReadOnly,
    cvTitleColor, cvTitleCaption, cvTitleAlignment, cvTitleFont, 
    cvImeMode, cvImeName);
  THIGColumnValues = set of THIGColumnValue;
  THIGSortMark = (smNone, smAscending, smDescending);
  THIGColumnEditStyle = (esSimple, esEllipsis, esPickList, esDataList, esCalendar,
    esCalculator);
  THIGColumnButtonStyle = (cbsNone, cbsEllipsis, cbsCombo, cbsLookup, cbsCheck,
    cbsCalendar, cbsCalculator);

  THIGVertAlignment = (taTopJustify, taBottomJustify, taVCenter);

  THIGCalcState = (csFirst, csValid, csError);

  THIGLabelBorderStyle = (lbsNone, lbsLine);

  //spin edit
  THIGValueType = (vtInteger, vtFloat);
  THIGDiagPoints = array[0..2] of TPoint;

  THIGCloseUpEvent = procedure(Sender: TObject; ValueChanged: Boolean) of object;
  THIGCalcExitEvent = procedure(Sender: TObject; Selected: Boolean) of object;
  THIGBeforeDropDownMenu = procedure(Sender: TObject; var ADropDown: boolean) of object;

  THIGGlyphList = class(TImageList)
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

  THIGGlyphCache = class
  private
    GlyphLists: TList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetList(AWidth, AHeight: Integer): THIGGlyphList;
    procedure ReturnList(List: THIGGlyphList);
    function Empty: Boolean;
  end;

  //glyph without text
  THIGButtonGlyph = class
  private
    FOriginal: TBitmap;
    FGlyphList: THIGGlyphList;
    FIndexs: array[TButtonState] of Integer;
    FTransparentColor: TColor;
    FNumGlyphs: TNumGlyphs;
    FOnChange: TNotifyEvent;
    FBackColor: TColor;
    procedure GlyphChanged(Sender: TObject);
    procedure SetGlyph(Value: TBitmap);
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure Invalidate;
  public
    constructor Create;
    destructor Destroy; override;
    function CreateButtonGlyph(State: TButtonState): Integer; overload;
    function CreateButtonGlyph(State: TButtonState;
      DisabledDarkColor, DisabledLightColor: TColor): Integer; overload;
    procedure Draw(Canvas: TCanvas; const Client: TRect; State: TButtonState;
      XDownEffect: boolean = True; YDownEffect: boolean = True); overload;
    procedure Draw(Canvas: TCanvas; const Client: TRect; State: TButtonState;
      DisabledDarkColor, DisabledLightColor: TColor;
      XDownEffect: boolean = True; YDownEffect: boolean = True); overload;
    property Glyph: TBitmap read FOriginal write SetGlyph;
    property NumGlyphs: TNumGlyphs read FNumGlyphs write SetNumGlyphs;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property BackColor: TColor read FBackColor write FBackColor;
  end;

  THIGHintWindow = class(THintWindow)
  private
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  protected
    procedure NCPaint(DC: HDC); override;
  end;

  THIGExecThread = class(TThread)
  private
    FMethodToExec: TNotifyEvent;
  protected
    FSender: TObject;
    procedure ExecMethod;
    procedure Execute; override;
  public
    property MethodToExec: TNotifyEvent read FMethodToExec write FMethodToExec;
    constructor Create(Sender: TObject);
  end;

function GetDarkenColor(AColor: TColor; AIndex: integer = 100): TColor;
function GetLightenColor(AColor: TColor; AIndex: integer = 100): TColor;
procedure KillMessage(Wnd: HWnd; Msg: Integer);
procedure DrawGradientColor(ACanvas: TCanvas; R: TRect;
  AStartColor, AEndColor: TColor; AHorz: Boolean = True);
procedure Delay(Milliseconds: Integer);

function BuangPemisahRibuan(const AValue: string): string;
function HIGStrToFloatStr(const AValue: string): string;
function CountCharInStr(const AChar: char; const AValue: string): integer;

implementation

{ THIGGlyphList }

constructor THIGGlyphList.CreateSize(AWidth, AHeight: Integer);
begin
  inherited CreateSize(AWidth, AHeight);
  Used := TBits.Create;
end;

destructor THIGGlyphList.Destroy;
begin
  Used.Free;
  inherited Destroy;
end;

function THIGGlyphList.AllocateIndex: Integer;
begin
  Result := Used.OpenBit;
  if Result >= Used.Size then
  begin
    Result := inherited Add(nil, nil);
    Used.Size := Result + 1;
  end;
  Used[Result] := True;
end;

function THIGGlyphList.AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
begin
  Result := AllocateIndex;
  ReplaceMasked(Result, Image, MaskColor);
  Inc(FCount);
end;

procedure THIGGlyphList.Delete(Index: Integer);
begin
  if Used[Index] then
  begin
    Dec(FCount);
    Used[Index] := False;
  end;
end;

{ THIGGlyphCache }

constructor THIGGlyphCache.Create;
begin
  inherited Create;
  GlyphLists := TList.Create;
end;

destructor THIGGlyphCache.Destroy;
begin
  GlyphLists.Free;
  inherited Destroy;
end;

function THIGGlyphCache.GetList(AWidth, AHeight: Integer): THIGGlyphList;
var
  I: Integer;
begin
  for I := GlyphLists.Count - 1 downto 0 do
  begin
    Result := GlyphLists[I];
    with Result do
      if (AWidth = Width) and (AHeight = Height) then Exit;
  end;
  Result := THIGGlyphList.CreateSize(AWidth, AHeight);
  GlyphLists.Add(Result);
end;

procedure THIGGlyphCache.ReturnList(List: THIGGlyphList);
begin
  if List = nil then Exit;
  if List.Count = 0 then
  begin
    GlyphLists.Remove(List);
    List.Free;
  end;
end;

function THIGGlyphCache.Empty: Boolean;
begin
  Result := GlyphLists.Count = 0;
end;

var
  GlyphCache: THIGGlyphCache = nil;
  ButtonCount: Integer = 0;

{ THIGButtonGlyph }

constructor THIGButtonGlyph.Create;
var
  I: TButtonState;
begin
  inherited Create;
  FOriginal := TBitmap.Create;
  FOriginal.OnChange := GlyphChanged;
  FTransparentColor := clOlive;
  FBackColor:= clBtnFace;
  FNumGlyphs := 1;
  for I := Low(I) to High(I) do
    FIndexs[I] := -1;
  if GlyphCache = nil then GlyphCache := THIGGlyphCache.Create;
end;

destructor THIGButtonGlyph.Destroy;
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

procedure THIGButtonGlyph.Invalidate;
var
  I: TButtonState;
begin
  for I := Low(I) to High(I) do
  begin
    if Assigned(FGlyphList) then
      if FIndexs[I] <> -1 then FGlyphList.Delete(FIndexs[I]);
    FIndexs[I] := -1;
  end;
  GlyphCache.ReturnList(FGlyphList);
  FGlyphList := nil;
end;

procedure THIGButtonGlyph.GlyphChanged(Sender: TObject);
begin
  if Sender = FOriginal then
  begin
    FTransparentColor := FOriginal.TransparentColor;
    Invalidate;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure THIGButtonGlyph.SetGlyph(Value: TBitmap);
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

procedure THIGButtonGlyph.SetNumGlyphs(Value: TNumGlyphs);
begin
  if (Value <> FNumGlyphs) and (Value > 0) then
  begin
    Invalidate;
    FNumGlyphs := Value;
    GlyphChanged(Glyph);
  end;
end;

function THIGButtonGlyph.CreateButtonGlyph(State: TButtonState;
  DisabledDarkColor, DisabledLightColor: TColor): Integer;
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
  if (State <> bsDisabled) and (Result <> -1) then Exit;
  if (State = bsDisabled) and (Result <> -1) and (FGlyphList <> nil) then
     FGlyphList.Delete(Result);
  if (FOriginal.Width or FOriginal.Height) = 0 then Exit;
  IWidth := FOriginal.Width div FNumGlyphs;
  IHeight := FOriginal.Height;
  if FGlyphList = nil then
  begin
    if GlyphCache = nil then GlyphCache := THIGGlyphCache.Create;
    FGlyphList := GlyphCache.GetList(IWidth, IHeight);
  end;
  TmpImage := TBitmap.Create;
  try
    TmpImage.Width := IWidth;
    TmpImage.Height := IHeight;
    IRect := Rect(0, 0, IWidth, IHeight);
    TmpImage.Canvas.Brush.Color := FBackColor;//clBtnFace;
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
              Brush.Color := DisabledLightColor;//clBtnHighlight;
              DestDC := Handle;
              SetTextColor(DestDC, clBlack);
              SetBkColor(DestDC, clWhite);
              BitBlt(DestDC, 0, 0, IWidth, IHeight,
                     MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);

              { Convert gray to clBtnShadow }
              DDB.Canvas.Brush.Color := clGray;
              MonoBmp.Canvas.CopyRect(IRect, DDB.Canvas, ORect);
              Brush.Color := DisabledDarkColor;//clBtnShadow;
              DestDC := Handle;
              SetTextColor(DestDC, clBlack);
              SetBkColor(DestDC, clWhite);
              BitBlt(DestDC, 0, 0, IWidth, IHeight,
                     MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);

              { Convert transparent color to clBtnFace }
              DDB.Canvas.Brush.Color := ColorToRGB(FTransparentColor);
              MonoBmp.Canvas.CopyRect(IRect, DDB.Canvas, ORect);
              Brush.Color := FBackColor;//clBtnFace;
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
                Brush.Color := FBackColor;//clBtnFace;
                FillRect(IRect);
                Brush.Color := DisabledLightColor;//clBtnHighlight;
                SetTextColor(Handle, clBlack);
                SetBkColor(Handle, clWhite);
                BitBlt(Handle, 1, 1, IWidth, IHeight,
                  MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
                Brush.Color := DisabledDarkColor;//clBtnShadow;
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

procedure THIGButtonGlyph.Draw(Canvas: TCanvas; const Client: TRect;
  State: TButtonState; DisabledDarkColor, DisabledLightColor: TColor;
  XDownEffect, YDownEffect: boolean);
var
  GlyphPos: TPoint;
  Index: Integer;
  Details: TThemedElementDetails;
  R: TRect;
  Button: TThemedButton;
begin
  if FOriginal = nil then Exit;
  if (FOriginal.Width = 0) or (FOriginal.Height = 0) then Exit;
  Index := CreateButtonGlyph(State, DisabledDarkColor, DisabledLightColor);
  with GlyphPos do
  begin
    X := Client.Left + ((Client.Right - Client.Left - (FOriginal.Width div FNumGlyphs)) shr 1) + (Ord(State = bsDown) * Ord(XDownEffect));
    Y := Client.Top + ((Client.Bottom - Client.Top - FOriginal.Height) shr 1) + (Ord(State = bsDown) * Ord(YDownEffect));
    if ThemeServices.ThemesEnabled then
    begin
      R.TopLeft := GlyphPos;
      R.Right := R.Left + FOriginal.Width div FNumGlyphs;
      R.Bottom := R.Top + FOriginal.Height;
      case State of
        bsDisabled:
          Button := tbPushButtonDisabled;
        bsDown,
        bsExclusive:
          Button := tbPushButtonPressed;
      else
        // bsUp
        Button := tbPushButtonNormal;
      end;
      Details := ThemeServices.GetElementDetails(Button);
      ThemeServices.DrawIcon(Canvas.Handle, Details, R, FGlyphList.Handle, Index);
    end
    else
//      if (State = bsExclusive) then
//      begin
        ImageList_DrawEx(FGlyphList.Handle, Index, Canvas.Handle, X, Y, 0, 0,
          clNone, clNone, ILD_Transparent)
{      end
      else
        ImageList_DrawEx(FGlyphList.Handle, Index, Canvas.Handle, X, Y, 0, 0,
          ColorToRGB(FBackColor), clNone, ILD_Normal);
}  end;
end;

//

function GetDarkenColor(AColor: TColor; AIndex: integer): TColor;
var
  r,g,b: extended;
  aRGBCol: integer;
  rb, gb, bb: Byte;
begin
  aRGBCol:= ColorToRGB(AColor);
  r := GetRValue(aRGBCol);
  g := GetGValue(aRGBCol);
  b := GetBValue(aRGBCol);
  r := r-((r/255)*AIndex);
  g := g-((g/255)*AIndex);
  b := b-((b/255)*AIndex);
  if r > 255 then r := 255;
  if r < 0 then r := 0;
  if g > 255 then g := 255;
  if g < 0 then g := 0;
  if b > 255 then b := 255;
  if b < 0 then b := 0;
  rb := byte(Round(r));
  gb := Byte(Round(g));
  bb := Byte(Round(b));
  Result := RGB(rb,gb,bb);
end;

function GetLightenColor(AColor: TColor; AIndex: integer): TColor;
var
  r,g,b: extended;
  aRGBCol: integer;
  rb, gb, bb: Byte;
begin
  aRGBCol:= ColorToRGB(AColor);
  r := GetRValue(aRGBCol);
  g := GetGValue(aRGBCol);
  b := GetBValue(aRGBCol);
  if r = 0 then
    r:= 127
  else
    r := r+((r/255)*AIndex);
  if g = 0 then
    g:= 127
  else
    g := g+((g/255)*AIndex);
  if b = 0 then
    b:= 127
  else
    b := b+((b/255)*AIndex);
  if r > 255 then r := 255;
  if r < 0 then r := 0;
  if g > 255 then g := 255;
  if g < 0 then g := 0;
  if b > 255 then b := 255;
  if b < 0 then b := 0;
  rb := byte(Round(r));
  gb := Byte(Round(g));
  bb := Byte(Round(b));
  Result := RGB(rb,gb,bb);
end;

procedure KillMessage(Wnd: HWnd; Msg: Integer);
// Delete the requested message from the queue, but throw back
// any WM_QUIT msgs that PeekMessage may also return
var
  M: TMsg;
begin
  M.Message := 0;
  if PeekMessage(M, Wnd, Msg, Msg, pm_Remove) and (M.Message = WM_QUIT) then
    PostQuitMessage(M.wparam);
end;

function THIGButtonGlyph.CreateButtonGlyph(State: TButtonState): Integer;
begin
  Result:= CreateButtonGlyph(State, clBtnShadow, clBtnHighlight);
end;

procedure THIGButtonGlyph.Draw(Canvas: TCanvas; const Client: TRect;
  State: TButtonState; XDownEffect, YDownEffect: boolean);
begin
  Draw(Canvas, Client, State, clBtnShadow, clBtnHighlight,
     XDownEffect, YDownEffect);
end;

{ THIGHintWindow }

procedure THIGHintWindow.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Canvas.Font:= Self.Font;
end;

procedure THIGHintWindow.NCPaint(DC: HDC);
var
  R: TRect;
  Details: TThemedElementDetails;
begin
  R := Rect(0, 0, Width, Height);
  if not ThemeServices.ThemesEnabled then
    Windows.DrawEdge(DC, R, BDR_RAISEDOUTER, BF_RECT or BF_MIDDLE or BF_FLAT)
  else
  begin
    Details := ThemeServices.GetElementDetails(twWindowRoot);
    ThemeServices.DrawEdge(DC, Details, R, BDR_RAISEDOUTER, BF_RECT or BF_MIDDLE or BF_FLAT);
  end;
end;

//

procedure DrawGradientColor(ACanvas: TCanvas; R: TRect;
  AStartColor, AEndColor: TColor; AHorz: Boolean); overload;
var
  ADelta: Integer;
  AStartR, AStartG, AStartB: Byte;
  AEndR, AEndG, AEndB: Byte;
  i: integer;
  APenColor, ASColor, AEColor: TColor;
  APenStyle: TPenStyle;
  APenWidth: integer;
  divi: integer;
  mul: extended;
begin
  APenColor:= ACanvas.Pen.Color;
  APenStyle:= ACanvas.Pen.Style;
  APenWidth:= ACanvas.Pen.Width;
  ACanvas.Pen.Style:= psSolid;
  ACanvas.Pen.Width:= 1;
  if AHorz then
    ADelta:= R.Bottom - R.Top
  else
    ADelta:= R.Right - R.Left;
  if ADelta <= 0 then
    ADelta:= 1;
  ASColor:= ColorToRGB(AStartColor);
  AEColor:= ColorToRGB(AEndColor);
  AStartR:= GetRValue(ASColor);
  AStartG:= GetGValue(ASColor);
  AStartB:= GetBValue(ASColor);
  AEndR:= GetRValue(AEColor);
  AEndG:= GetGValue(AEColor);
  AEndB:= GetBValue(AEColor);

  divi:= ADelta-1;
  with ACanvas do
  begin
    if AHorz then
      for i:= R.Top to R.Bottom-1 do begin
        mul := ((i-R.Top)/divi);
        Pen.Color := RGB(trunc(AStartR + (AEndR - AStartR) * mul),
          trunc(AStartG + (AEndG - AStartG) *mul), trunc(AStartB + (AEndB - AStartB) * mul));
        MoveTo(R.Left, i);
        LineTo(R.Right, i);
      end
    else
      for i:= R.Left to R.Right-1 do begin
        mul := ((i-R.Left)/divi);
        Pen.Color := RGB(trunc(AStartR + (AEndR - AStartR) * mul),
          trunc(AStartG + (AEndG - AStartG) *mul), trunc(AStartB + (AEndB - AStartB) * mul));
        MoveTo(i, R.Top);
        LineTo(i, R.Bottom);
      end;
    Pen.Color:= APenColor;
    Pen.Style:= APenStyle;
    Pen.Width:= APenWidth;
  end;
end;

{ THIGExecThread }

constructor THIGExecThread.Create(Sender: TObject);
begin
  inherited Create(True);
  FSender:= Sender;
  Self.Priority:= tpNormal;
  FreeOnTerminate:= True;
end;

procedure THIGExecThread.ExecMethod;
begin
  if Assigned(FMethodToExec) then
    FMethodToExec(FSender);
end;

procedure THIGExecThread.Execute;
begin
  Synchronize(ExecMethod);
end;

procedure Delay(Milliseconds: Integer);
  {by Hagen Reddmann}
var
  Tick: DWORD;
  Event: THandle;
begin
  Event := CreateEvent(nil, False, False, nil);
  try
    Tick := GetTickCount + DWORD(Milliseconds);
    while (Milliseconds > 0) and
      (MsgWaitForMultipleObjects(1, Event, False, Milliseconds,
      QS_ALLINPUT) <> WAIT_TIMEOUT) do
    begin
      Application.ProcessMessages;
      Milliseconds := Tick - GetTickCount;
    end;
  finally
    CloseHandle(Event);
  end;
end;

function BuangPemisahRibuan(const AValue: string): string;
begin
  if DecimalSeparator <> ThousandSeparator then
    Result := StringReplace(AValue, ThousandSeparator, '', [rfReplaceAll, rfIgnoreCase])
  else
    Result := AValue;
end;

function HIGStrToFloatStr(const AValue: string): string;
var
  I, J: Integer;
begin
  Result := Trim(AValue);
  Result:= BuangPemisahRibuan(Result);

  if (DecimalSeparator <> '.') and (ThousandSeparator <> '.') then
    Result := StringReplace(Result, '.', DecimalSeparator, [rfReplaceAll, rfIgnoreCase]);
  if (DecimalSeparator <> ',') and (ThousandSeparator <> ',') then
    Result := StringReplace(Result, ',', DecimalSeparator, [rfReplaceAll, rfIgnoreCase]);

  J := 1;
  for I := 1 to Length(Result) do
    if Result[I] in ['0'..'9', '-', '+', DecimalSeparator, ThousandSeparator] then
    begin
      Result[J] := Result[I];
      Inc(J);
    end;
  SetLength(Result, J - 1);

  if Result = '' then
    Result := '0'
  else if Result = '-' then
    Result := '-0';
end;

function CountCharInStr(const AChar: char; const AValue: string): integer;
var
  i: integer;
begin
  Result:= 0;
  for i:= 1 to Length(AValue) do
  begin
    if SameText(AValue[i], AChar) then
      inc(Result);
  end;
end;

end.
