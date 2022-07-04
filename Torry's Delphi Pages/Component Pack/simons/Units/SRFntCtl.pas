unit SRFntCtl;

{ TSRFontComboBox und TSRFontListBox (C)opyright 2004 Version 1.03
  Autor : Simon Reinhardt
  eMail : reinhardt@picsoft.de
  Internet : http://www.picsoft.de

  Diese Komponenten zur Schriftart-Auswahl basieren auf den Komponenten
  TFontComboBox und TFontListBox von Jimmy Theo <Theo@elang.stts.ac.id>
  Die Sourcecodes sind Public Domain, das Urheberrecht liegt aber beim
  Autor. }

interface

{$I SRDefine.inc}

uses
  {$IFDEF SR_Win32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF} Messages,
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls
  {$IFNDEF SR_Delphi2_Up}, Menus{$ENDIF};

type
  TFontSize  = 6..32;
  TFontMasks = (fmTrueType, fmNoTrueType, fmFixedPitch, fmVariablePitch);
  TFontMask  = set of TFontMasks;

  TSRFontComboBox = class(TCustomComboBox)
  private
    {$IFDEF SR_Delphi2_Up}
    FBitmap          : TBitmap;
    {$ENDIF}
    FCanvas          : TControlCanvas;
    FFontMask        : TFontMask;
    FFontName,
    FDisplayFontName : TFontName;
    FDisplayFontSize : TFontSize;
    FTranspColor     : TColor;
    {$IFDEF SR_Delphi2_Up}
    FShowTTSymbol,
    {$ENDIF}
    FUseItemFont     : boolean;

    procedure SetFontMask(newValue: TFontMask);
    procedure SetFontName(newValue: TFontName);
    procedure SetDisplayFontName(newValue: TFontName);
    procedure SetDisplayFontSize(newValue: TFontSize);
    procedure SetIndex;
    {$IFDEF SR_Delphi2_Up}
    procedure SetShowTTSymbol(newValue: boolean);
    {$ENDIF}
    procedure SetUseItemFont(newValue: boolean);

  protected
    procedure CreateWnd; override;
    procedure CreateFontList;
    procedure Change; override;
    function FontMatchesMask(const FontName: string):boolean;
    function IsTrueType(const Index: Integer):boolean;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DrawItem(Index: Integer; ARect: TRect; State: TOwnerDrawState); override;

  published
    property Color;
    property Ctl3D;
    property DisplayFontName: TFontName read FDisplayFontName write SetDisplayFontName;
    property DisplayFontSize: TFontSize read FDisplayFontSize write SetDisplayFontSize;
    property DragMode;
    property DragCursor;
    property DropDownCount;
    property Enabled;
    property FontMask: TFontMask read FFontMask write SetFontMask;
    property FontName: TFontName read FFontName write SetFontName;
    property Items;
    property ParentColor;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    {$IFDEF SR_Delphi2_Up}
    property ShowTTSymbol: boolean read FShowTTSymbol write SetShowTTSymbol;
    {$ENDIF}
    property TabOrder;
    property TabStop;
    property UseItemFont: boolean read FUseItemFont write SetUseItemFont;
    property Visible;

    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
  end;

  TSRFontListBox = class(TCustomListBox)
  private
    {$IFDEF SR_Delphi2_Up}
    FBitmap          : TBitmap;
    {$ENDIF}
    FCanvas          : TControlCanvas;
    FFontMask        : TFontMask;
    FFontName,
    FDisplayFontName : TFontName;
    FDisplayFontSize : TFontSize;
    FTranspColor     : TColor;
    {$IFDEF SR_Delphi2_Up}
    FShowTTSymbol,
    {$ENDIF}
    FUseItemFont     : boolean;

    procedure SetFontMask(newValue: TFontMask);
    procedure SetDisplayFontName(newValue: TFontName);
    procedure SetDisplayFontSize(newValue: TFontSize);
    procedure SetFontName(newValue: TFontName);
    procedure SetIndex;
    {$IFDEF SR_Delphi2_Up}
    procedure SetShowTTSymbol(newValue: boolean);
    {$ENDIF}
    procedure SetUseItemFont(newValue: boolean);

  protected
    procedure CreateWnd; override;
    procedure CreateFontList;
    procedure CMFontChanged(var Message); message CM_FONTCHANGED;
    procedure Click; override;
    function FontMatchesMask(const FontName: string):boolean;
    function IsTrueType(const Index: Integer):boolean;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DrawItem(Index: Integer; ARect: TRect; State: TOwnerDrawState); override;

  published
    property Align;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property DisplayFontName: TFontName read FDisplayFontName write SetDisplayFontName;
    property DisplayFontSize: TFontSize read FDisplayFontSize write SetDisplayFontSize;
    property DragMode;
    property DragCursor;
    property Enabled;
    property FontMask: TFontMask read FFontMask write SetFontMask;
    property FontName: TFontName read FFontName write SetFontName;
    property Items;
    property ItemIndex;
    property IntegralHeight;
    property ParentColor;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    {$IFDEF SR_Delphi2_Up}
    property ShowTTSymbol: boolean read FShowTTSymbol write SetShowTTSymbol;
    {$ENDIF}
    property TabOrder;
    property TabStop;
    property UseItemFont: boolean read FUseItemFont write SetUseItemFont;
    property Visible;

    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
  end;

procedure Register;

implementation

{$IFDEF SR_Delphi2_Up}
{$R *.D32}
{$R *.R32}
{$ELSE}
{$R *.D16}
{$ENDIF}

{ TSRFontComboBox }

constructor TSRFontComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  DropDownCount := 16;

  FFontMask := [fmTrueType, fmNoTrueType, fmFixedPitch, fmVariablePitch];
  FFontName:= 'Arial';
  FDisplayFontName:= Font.Name;
  FDisplayFontSize:= 10;
  Font.Size := FDisplayFontSize;
  ItemHeight := FDisplayFontSize*2;
  Sorted := True;
  {$IFDEF SR_Delphi2_Up}
  FShowTTSymbol := true;
  {$ENDIF}
  Style := csOwnerDrawFixed;

  {$IFDEF SR_Delphi2_Up}
  FBitmap := TBitmap.Create;
  FBitmap.Height := 10;
  FBitmap.Width := 10;
  FBitmap.LoadFromResourceName(HInstance, 'TTSYMBOL');
  FTranspColor:=FBitmap.Canvas.Pixels[0, FBitmap.Height-1];
  {$ENDIF}
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self;
end;

destructor TSRFontComboBox.Destroy;
begin
  {$IFDEF SR_Delphi2_Up}
  FBitmap.Free;
  {$ENDIF}
  FCanvas.Free;

  inherited Destroy;
end;

procedure TSRFontComboBox.Change;
begin
  if ItemIndex>=0 then
    FFontName:=Items[ItemIndex];

  inherited Change;
end;

procedure TSRFontComboBox.CreateFontList;
var i : integer;
begin
  Items.Clear;
  for i:=0 to Screen.Fonts.Count-1 do
    if (Screen.Fonts[i] <> 'Default') and FontMatchesMask(Screen.Fonts[i])  then
      Items.Add(Screen.Fonts[i]);
  SetIndex;
end;

procedure TSRFontComboBox.CreateWnd;
begin
  inherited CreateWnd;
  CreateFontList;
end;

procedure TSRFontComboBox.DrawItem(Index: Integer; ARect: TRect;
  State: TOwnerDrawState);
var
  lf      : TLogFont;
  BmpRect,
  SrcRect : TRect;
  oldFont,
  newFont : HFont;
  Offset  : integer;
  AItem   : array [0..255] of char;
begin
  if Index>=0 then begin
    NewFont := 0;
    OldFont := 0;
    if FUseItemFont then begin
      with lf do begin
        lfHeight := Font.Height;
        lfWidth := 0;
        lfEscapement := 0;
        lfWeight := FW_NORMAL;
        lfItalic := Byte(fsItalic in Font.Style);
        lfUnderline := Byte(fsUnderline in Font.Style);
        lfStrikeOut := Byte(fsStrikeOut in Font.Style);
        lfCharSet := ANSI_CHARSET;
        lfOutPrecision := OUT_DEFAULT_PRECIS;
        lfClipPrecision := CLIP_DEFAULT_PRECIS;
        lfQuality := DEFAULT_QUALITY;
        lfPitchAndFamily := DEFAULT_PITCH or FF_DONTCARE;
        StrPCopy(lfFaceName, Items[Index]);
      end;
      newFont := CreateFontIndirect(lf);
      oldFont := SelectObject(Canvas.Handle, newFont);
    end;
    with Canvas do begin
      FillRect(ARect);
      {$IFDEF SR_Delphi2_Up}
      if FShowTTSymbol and IsTrueType(Index) and (ARect.Left <> 3) then begin
        Offset:=(ARect.Bottom-ARect.Top-FBitmap.Height) div 2;
        BmpRect:=Rect(ARect.Left+2, ARect.Top+Offset,
                      ARect.Left+FBitmap.Width+2, ARect.Top+Offset+FBitmap.Height);
        SrcRect:=Rect(0, 0, FBitmap.Width, FBitmap.Height);
        BrushCopy(BmpRect, FBitmap, SrcRect, FTranspColor);
      end;
      if FShowTTSymbol then
        ARect.Left:=ARect.Left+FBitmap.Width+2;
      {$ENDIF}
      ARect.Left:=ARect.Left+2;
      StrPCopy(AItem, Items[Index]);
      DrawText(Handle, AItem, length(Items[Index]),
               ARect, DT_SingleLine or DT_VCenter or DT_NoPrefix);
    end;
    if FUseItemFont then begin
      SelectObject(Canvas.Handle, oldFont);
      DeleteObject(newFont);
    end;
  end;
end;

function TSRFontComboBox.FontMatchesMask(const FontName: string):boolean;
var
  Metrics : TTextMetric;
  lf      : TLogFont;
  oldFont,
  newFont : HFont;
  IsTT,
  IsFixed : boolean;
begin
  with lf do begin
    lfHeight := 10;
    lfWidth := 10;
    lfEscapement := 0;
    lfWeight := FW_REGULAR;
    lfItalic := 0;
    lfUnderline := 0;
    lfStrikeOut := 0;
    lfCharSet := ANSI_CHARSET;
    lfOutPrecision := OUT_DEFAULT_PRECIS;
    lfClipPrecision := CLIP_DEFAULT_PRECIS;
    lfQuality := DEFAULT_QUALITY;
    lfPitchAndFamily := DEFAULT_PITCH or FF_DONTCARE;
    StrPCopy(lfFaceName, FontName);
  end;
  newFont := CreateFontIndirect(lf);
  oldFont := SelectObject(FCanvas.Handle, newFont);
  GetTextMetrics(FCanvas.Handle, Metrics);
  IsTT:=(Metrics.tmPitchAndFamily and TMPF_TRUETYPE) <> 0;
  IsFixed:=(Metrics.tmPitchAndFamily and TMPF_FIXED_PITCH) = 0;
  Result := true;
  if IsTT and not (fmTrueType in FFontMask) then
    Result := false;
  if not IsTT and not (fmNoTrueType in FFontMask) then
    Result := false;
  if IsFixed and not (fmFixedPitch in FFontMask) then
    Result := false;
  if not IsFixed and not (fmVariablePitch in FFontMask) then
    Result := false;
  SelectObject(FCanvas.Handle, oldFont);
  DeleteObject(newFont);
end;

function TSRFontComboBox.IsTrueType(const Index: Integer):boolean;
var
  Metrics : TTextMetric;
  lf      : TLogFont;
  oldFont,
  newFont : HFont;
begin
  with lf do begin
    lfHeight := 10;
    lfWidth := 10;
    lfEscapement := 0;
    lfWeight := FW_REGULAR;
    lfItalic := 0;
    lfUnderline := 0;
    lfStrikeOut := 0;
    lfCharSet := ANSI_CHARSET;
    lfOutPrecision := OUT_DEFAULT_PRECIS;
    lfClipPrecision := CLIP_DEFAULT_PRECIS;
    lfQuality := DEFAULT_QUALITY;
    lfPitchAndFamily := DEFAULT_PITCH or FF_DONTCARE;
    StrPCopy(lfFaceName, Items[Index]);
  end;
  newFont := CreateFontIndirect(lf);
  oldFont := SelectObject(FCanvas.Handle, newFont);
  GetTextMetrics(FCanvas.Handle, Metrics);
  Result := (Metrics.tmPitchAndFamily and TMPF_TRUETYPE) <> 0;
  SelectObject(FCanvas.Handle, oldFont);
  DeleteObject(newFont);
end;

procedure TSRFontComboBox.SetDisplayFontName(newValue: TFontName);
begin
  if newValue <> FDisplayFontName then begin
    FDisplayFontName := newValue;
    Font.Name := FDisplayFontName;
    Invalidate;
  end;
end;

procedure TSRFontComboBox.SetDisplayFontSize(newValue: TFontSize);
begin
  if newValue <> FDisplayFontSize then begin
    FDisplayFontSize := newValue;
    Font.Size := FDisplayFontSize;
    ItemHeight := FDisplayFontSize*2;
    Invalidate;
  end;
end;

procedure TSRFontComboBox.SetFontMask(newValue: TFontMask);
begin
  if newValue <> FFontMask then begin
    FFontMask := newValue;
    CreateFontList;
    Change;
  end;
end;

procedure TSRFontComboBox.SetFontName(newValue: TFontName);
begin
  if (newValue <> FFontName) or (ItemIndex < 0) then begin
    FFontName := newValue;
    SetIndex;
  end;
end;

procedure TSRFontComboBox.SetIndex;
var AIndex,
    OldIdx : integer;
begin
  OldIdx:=ItemIndex;
  if Items.Count>0 then begin
    AIndex:=Items.IndexOf(FFontName);
    if AIndex>=0 then
      ItemIndex:=AIndex
    else
      ItemIndex:=0;
  end
  else
    ItemIndex:=-1;
  if OldIdx<>ItemIndex then
    Change;
end;

{$IFDEF SR_Delphi2_Up}
procedure TSRFontComboBox.SetShowTTSymbol(newValue: boolean);
begin
  if FShowTTSymbol <> newValue then begin
    FShowTTSymbol := newValue;
    Invalidate;
  end;
end;
{$ENDIF}

procedure TSRFontComboBox.SetUseItemFont(newValue: boolean);
begin
  if FUseItemFont <> newValue then begin
    FUseItemFont := newValue;
    Invalidate;
  end;
end;

{ TSRFontListBox }

constructor TSRFontListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FFontMask := [fmTrueType, fmNoTrueType, fmFixedPitch, fmVariablePitch];
  FFontName:= 'Arial';
  FDisplayFontName:= Font.Name;
  FDisplayFontSize:= 10;
  Font.Size := FDisplayFontSize;
  ItemHeight := FDisplayFontSize*2;
  Sorted := True;
  {$IFDEF SR_Delphi2_Up}
  FShowTTSymbol := true;
  {$ENDIF}
  Style := lbOwnerDrawFixed;

  {$IFDEF SR_Delphi2_Up}
  FBitmap := TBitmap.Create;
  FBitmap.Height := 10;
  FBitmap.Width := 10;
  FBitmap.LoadFromResourceName(HInstance, 'TTSYMBOL');
  FTranspColor:=FBitmap.Canvas.Pixels[0, FBitmap.Height-1];
  {$ENDIF}
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self;
end;

destructor TSRFontListBox.Destroy;
begin
  {$IFDEF SR_Delphi2_Up}
  FBitmap.Free;
  {$ENDIF}
  FCanvas.Free;

  inherited Destroy;
end;

procedure TSRFontListBox.CMFontChanged(var Message);
begin
  ItemHeight := font.size*2;
  RecreateWnd;
  Repaint;
end;

procedure TSRFontListBox.Click;
begin
  if ItemIndex>=0 then
    FFontName:=Items[ItemIndex];

  inherited Click;
end;

procedure TSRFontListBox.CreateFontList;
var i : integer;
begin
  Items.Clear;
  for i:=0 to Screen.Fonts.Count-1 do
    if (Screen.Fonts[i] <> 'Default') and FontMatchesMask(Screen.Fonts[i])  then
      Items.Add(Screen.Fonts[i]);
  SetIndex;
end;

procedure TSRFontListBox.CreateWnd;
begin
  inherited CreateWnd;
  CreateFontList;
end;

procedure TSRFontListBox.DrawItem(Index: Integer; ARect: TRect;
  State: TOwnerDrawState);
var
  lf      : TLogFont;
  BmpRect,
  SrcRect : TRect;
  oldFont,
  newFont : HFont;
  Offset  : integer;
  AItem   : array [0..255] of char;
begin
  if Index>=0 then begin
    NewFont := 0;
    OldFont := 0;
    if FUseItemFont then begin
      with lf do begin
        lfHeight := Font.Height;
        lfWidth := 0;
        lfEscapement := 0;
        lfWeight := FW_NORMAL;
        lfItalic := Byte(fsItalic in Font.Style);
        lfUnderline := Byte(fsUnderline in Font.Style);
        lfStrikeOut := Byte(fsStrikeOut in Font.Style);
        lfCharSet := ANSI_CHARSET;
        lfOutPrecision := OUT_DEFAULT_PRECIS;
        lfClipPrecision := CLIP_DEFAULT_PRECIS;
        lfQuality := DEFAULT_QUALITY;
        lfPitchAndFamily := DEFAULT_PITCH or FF_DONTCARE;
        StrPCopy(lfFaceName, Items[Index]);
      end;
      newFont := CreateFontIndirect(lf);
      oldFont := SelectObject(Canvas.Handle, newFont);
    end;
    with Canvas do begin
      FillRect(ARect);
      {$IFDEF SR_Delphi2_Up}
      if FShowTTSymbol and IsTrueType(Index) and (ARect.Left <> 3) then begin
        Offset:=(ARect.Bottom-ARect.Top-FBitmap.Height) div 2;
        BmpRect:=Rect(ARect.Left+2, ARect.Top+Offset,
                      ARect.Left+FBitmap.Width+2, ARect.Top+Offset+FBitmap.Height);
        SrcRect:=Rect(0, 0, FBitmap.Width, FBitmap.Height);
        BrushCopy(BmpRect, FBitmap, SrcRect, FTranspColor);
      end;
      if FShowTTSymbol then
        ARect.Left:=ARect.Left+FBitmap.Width+2;
      {$ENDIF}
      ARect.Left:=ARect.Left+2;
      StrPCopy(AItem, Items[Index]);
      DrawText(Handle, AItem, length(Items[Index]),
               ARect, DT_SingleLine or DT_VCenter or DT_NoPrefix);
    end;
    if FUseItemFont then begin
      SelectObject(Canvas.Handle, oldFont);
      DeleteObject(newFont);
    end;
  end;
end;

function TSRFontListBox.FontMatchesMask(const FontName: string):boolean;
var
  Metrics : TTextMetric;
  lf      : TLogFont;
  oldFont,
  newFont : HFont;
  IsTT,
  IsFixed : boolean;
begin
  with lf do begin
    lfHeight := 10;
    lfWidth := 10;
    lfEscapement := 0;
    lfWeight := FW_REGULAR;
    lfItalic := 0;
    lfUnderline := 0;
    lfStrikeOut := 0;
    lfCharSet := DEFAULT_CHARSET;
    lfCharSet := ANSI_CHARSET;
    lfOutPrecision := OUT_DEFAULT_PRECIS;
    lfClipPrecision := CLIP_DEFAULT_PRECIS;
    lfQuality := DEFAULT_QUALITY;
    lfPitchAndFamily := DEFAULT_PITCH or FF_DONTCARE;
    StrPCopy(lfFaceName, FontName);
  end;
  newFont := CreateFontIndirect(lf);
  oldFont := SelectObject(FCanvas.Handle, newFont);
  GetTextMetrics(FCanvas.Handle, Metrics);
  IsTT:=(Metrics.tmPitchAndFamily and TMPF_TRUETYPE) <> 0;
  IsFixed:=(Metrics.tmPitchAndFamily and TMPF_FIXED_PITCH) = 0;
  Result := true;
  if IsTT and not (fmTrueType in FFontMask) then
    Result := false;
  if not IsTT and not (fmNoTrueType in FFontMask) then
    Result := false;
  if IsFixed and not (fmFixedPitch in FFontMask) then
    Result := false;
  if not IsFixed and not (fmVariablePitch in FFontMask) then
    Result := false;
  SelectObject(FCanvas.Handle, oldFont);
  DeleteObject(newFont);
end;

function TSRFontListBox.IsTrueType(const Index: Integer):boolean;
var
  Metrics : TTextMetric;
  lf      : TLogFont;
  oldFont,
  newFont : HFont;
begin
  with lf do begin
    lfHeight := 10;
    lfWidth := 10;
    lfEscapement := 0;
    lfWeight := FW_REGULAR;
    lfItalic := 0;
    lfUnderline := 0;
    lfStrikeOut := 0;
    lfCharSet := DEFAULT_CHARSET;
    lfOutPrecision := OUT_DEFAULT_PRECIS;
    lfClipPrecision := CLIP_DEFAULT_PRECIS;
    lfQuality := DEFAULT_QUALITY;
    lfPitchAndFamily := DEFAULT_PITCH or FF_DONTCARE;
    StrPCopy(lfFaceName, Items[Index]);
  end;
  newFont := CreateFontIndirect(lf);
  oldFont := SelectObject(FCanvas.Handle, newFont);
  GetTextMetrics(FCanvas.Handle, Metrics);
  Result := (Metrics.tmPitchAndFamily and TMPF_TRUETYPE) <> 0;
  SelectObject(FCanvas.Handle, oldFont);
  DeleteObject(newFont);
end;

procedure TSRFontListBox.SetDisplayFontName(newValue: TFontName);
begin
  if newValue <> FDisplayFontName then begin
    FDisplayFontName := newValue;
    Font.Name := FDisplayFontName;
    Invalidate;
  end;
end;

procedure TSRFontListBox.SetDisplayFontSize(newValue: TFontSize);
begin
  if newValue <> FDisplayFontSize then begin
    FDisplayFontSize := newValue;
    Font.Size := FDisplayFontSize;
    ItemHeight := FDisplayFontSize*2;
    Invalidate;
  end;
end;

procedure TSRFontListBox.SetFontMask(newValue: TFontMask);
begin
  if newValue <> FFontMask then begin
    FFontMask := newValue;
    CreateFontList;
  end;
end;

procedure TSRFontListBox.SetFontName(newValue: TFontName);
begin
  if (newValue <> FFontName) or (ItemIndex < 0) then begin
    FFontName := newValue;
    SetIndex;
  end;
end;

procedure TSRFontListBox.SetIndex;
var AIndex : integer;
begin
  if Items.Count>0 then begin
    AIndex:=Items.IndexOf(FFontName);
    if AIndex>=0 then
      ItemIndex:=AIndex
    else
      ItemIndex:=0;
  end
  else
    ItemIndex:=-1;
end;

{$IFDEF SR_Delphi2_Up}
procedure TSRFontListBox.SetShowTTSymbol(newValue: boolean);
begin
  if FShowTTSymbol <> newValue then begin
    FShowTTSymbol := newValue;
    Invalidate;
  end;
end;
{$ENDIF}

procedure TSRFontListBox.SetUseItemFont(newValue: boolean);
begin
  if FUseItemFont <> newValue then begin
    FUseItemFont := newValue;
    Invalidate;
  end;
end;

procedure Register;
begin
  RegisterComponents('Simon', [TSRFontComboBox, TSRFontListBox]);
end;

end.
