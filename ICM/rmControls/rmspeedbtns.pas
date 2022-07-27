{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmSpeedBtns
Purpose  : This unit provides enhanced speed button controls and
           is required by various other rmControls
Date     : 09-03-1998
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmSpeedBtns;

interface

{$I CompilerDefines.INC}

uses
    Windows, Messages, Forms, Controls, Graphics, Classes, Buttons, CommCtrl,
    extctrls, Menus, rmLibrary;

const
  InitRepeatPause = 400;  { pause before repeat timer (ms) }
  RepeatPause     = 100;  { pause before hint window displays (ms)}

type
  TrmButtonState = (bsUp, bsDisabled, bsDown, bsExclusive, bsMenu);

  TrmSpeedButtonStyle = (sbsNormal, sbsComboButton, sbsMenu);

  TrmCustomSpeedButton = class(TGraphicControl)
  private
    FGroupIndex: Integer;
    FGlyph: Pointer;
    FDown: Boolean;
    FDragging: Boolean;
    FAllowAllUp: Boolean;
    FLayout: TButtonLayout;
    FSpacing: Integer;
    FTransparent: Boolean;
    FMargin: Integer;
    FFlat: Boolean;
    FMouseInControl: Boolean;

    fStyle : TrmSpeedButtonStyle;
    FMenuBtnWidth: integer;
    FMenuDropDown : boolean;
    FDropDownMenu : TPopUpMenu;
    procedure GlyphChanged(Sender: TObject);
    procedure UpdateExclusive;
    function GetGlyph: TBitmap;
    procedure SetGlyph(Value: TBitmap);
    function GetNumGlyphs: TNumGlyphs;
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure SetDown(Value: Boolean);
    procedure SetFlat(Value: Boolean);
    procedure SetAllowAllUp(Value: Boolean);
    procedure SetGroupIndex(Value: Integer);
    procedure SetLayout(Value: TButtonLayout);
    procedure SetSpacing(Value: Integer);
    procedure SetTransparent(Value: Boolean);
    procedure SetMargin(Value: Integer);
    procedure UpdateTracking;
    procedure WMLButtonDblClk(var Message: TWMLButtonDown); message WM_LBUTTONDBLCLK;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMButtonPressed(var Message: TMessage); message CM_BUTTONPRESSED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetDropDownMenu(const Value: TPopupMenu);
    procedure SetMenuBtnWidth(const Value: integer);
    procedure SetStyle(const Value: TrmSpeedButtonStyle);
    procedure MenuPaint;
    procedure StandardPaint;
  protected
    FState: TrmButtonState;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    function GetPalette: HPALETTE; override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    property MouseInControl: Boolean read FMouseInControl;

    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default False;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property Down: Boolean read FDown write SetDown default False;
    property Flat: Boolean read FFlat write SetFlat default False;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft;
    property Margin: Integer read FMargin write SetMargin default -1;
    property NumGlyphs: TNumGlyphs read GetNumGlyphs write SetNumGlyphs default 1;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property Transparent: Boolean read FTransparent write SetTransparent default True;

    property MenuButtonWidth : integer read FMenuBtnWidth write SetMenuBtnWidth default 11;
    property DropDownMenu : TPopupMenu read FDropDownMenu write SetDropDownMenu;
    property Style : TrmSpeedButtonStyle read fstyle write SetStyle default sbsNormal;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
  end;

  TrmSpeedButton = class(TrmCustomSpeedButton)
  published
    property Action;
    property AllowAllUp;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DropDownMenu;
    property GroupIndex;
    property Down;
    property Caption;
    property Enabled;
    property Flat;
    property Font;
    property Glyph;
    property Layout;
    property Margin;
    property MenuButtonWidth;
    property NumGlyphs;
    property ParentFont;
    property ParentShowHint;
    property ParentBiDiMode;
    property PopupMenu;
    property ShowHint;
    property Spacing;
    property Style;
    property Transparent;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

{ TrmTimerSpeedButton }

  TrmTimeBtnState = set of (tbFocusRect, tbAllowTimer);

  TrmTimerSpeedButton = class(TrmSpeedButton)
  private
    FTimeBtnState: TrmTimeBtnState;
    procedure TimerExpired(Sender: TObject);
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    destructor Destroy; override;
    property TimeBtnState: TrmTimeBtnState read FTimeBtnState write FTimeBtnState;
  end;

{ TrmSpinButton }

  TrmSpinButton = class (TWinControl)
  private
    FUpButton: TrmTimerSpeedButton;
    FDownButton: TrmTimerSpeedButton;
    FFocusedButton: TrmTimerSpeedButton;
    FFocusControl: TWinControl;
    fUpEnabled : boolean;
    fDownEnabled : boolean;
    FOnUpClick: TNotifyEvent;
    FOnDownClick: TNotifyEvent;
    fDownGlyphDefault : Boolean;
    fUpGlyphDefault : boolean;
    function CreateButton: TrmTimerSpeedButton;
    function GetUpGlyph: TBitmap;
    function GetDownGlyph: TBitmap;
    procedure SetUpGlyph(Value: TBitmap);
    procedure SetDownGlyph(Value: TBitmap);
    function GetUpNumGlyphs: TNumGlyphs;
    function GetDownNumGlyphs: TNumGlyphs;
    procedure SetUpNumGlyphs(Value: TNumGlyphs);
    procedure SetDownNumGlyphs(Value: TNumGlyphs);
    procedure BtnClick(Sender: TObject);
    procedure BtnMouseDown (Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetFocusBtn (Btn: TrmTimerSpeedButton);
    procedure AdjustSize (var W, H: Integer); reintroduce;
    procedure WMSize(var Message: TWMSize);  message WM_SIZE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    function GetDownEnabled: boolean;
    function GetUpEnabled: boolean;
    procedure SetDownEnabled(const Value: boolean);
    procedure SetUpEnabled(const Value: boolean);
    procedure CMSysColorChange(var Message:TMessage); message CM_SYSCOLORCHANGE;
  protected
    procedure Loaded; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure SetEnabled(value:boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property Align;
    property Anchors;
    property Constraints;
    property Ctl3D;
    property DownGlyph: TBitmap read GetDownGlyph write SetDownGlyph;
    property DownNumGlyphs: TNumGlyphs read GetDownNumGlyphs write SetDownNumGlyphs default 1;
    property DownEnabled: boolean read GetDownEnabled write SetDownEnabled;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FocusControl: TWinControl read FFocusControl write FFocusControl;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UpGlyph: TBitmap read GetUpGlyph write SetUpGlyph;
    property UpNumGlyphs: TNumGlyphs read GetUpNumGlyphs write SetUpNumGlyphs default 1;
    property UpEnabled: boolean read GetUpEnabled write SetUpEnabled;
    property Visible;
    property OnDownClick: TNotifyEvent read FOnDownClick write FOnDownClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnStartDock;
    property OnStartDrag;
    property OnUpClick: TNotifyEvent read FOnUpClick write FOnUpClick;
  end;

implementation

{$R rmSBSpin.res}

uses Consts, SysUtils, ActnList, ImgList;

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
    FIndexs: array[TrmButtonState] of Integer;
    FTransparentColor: TColor;
    FNumGlyphs: TNumGlyphs;
    FOnChange: TNotifyEvent;
    procedure GlyphChanged(Sender: TObject);
    procedure SetGlyph(Value: TBitmap);
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure Invalidate;
    function CreateButtonGlyph(State: TrmButtonState): Integer;
    procedure DrawButtonGlyph(Canvas: TCanvas; const GlyphPos: TPoint;
      State: TrmButtonState; Transparent: Boolean);
    procedure DrawButtonText(Canvas: TCanvas; const Caption: string;
      TextBounds: TRect; State: TrmButtonState; BiDiFlags: Longint);
    procedure CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
      const Offset: TPoint; const Caption: string; Layout: TButtonLayout;
      Margin, Spacing: Integer; var GlyphPos: TPoint; var TextBounds: TRect;
      BiDiFlags: Longint);
  public
    constructor Create;
    destructor Destroy; override;
    { return the text rectangle }
    function Draw(Canvas: TCanvas; const Client: TRect; const Offset: TPoint;
      const Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
      State: TrmButtonState; Style: TrmSpeedButtonStyle; Transparent: Boolean; BiDiFlags: Longint): TRect;
    property Glyph: TBitmap read FOriginal write SetGlyph;
    property NumGlyphs: TNumGlyphs read FNumGlyphs write SetNumGlyphs;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

var
   FUnitTimer : TTimer;

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
  ButtonCount: Integer = 0;

{ TButtonGlyph }
    
constructor TButtonGlyph.Create;
var
  I: TrmButtonState;
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
  I: TrmButtonState;
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

function TButtonGlyph.CreateButtonGlyph(State: TrmButtonState): Integer;
const
  ROP_DSPDxax = $00E20746;
var
  TmpImage, DDB, MonoBmp: TBitmap;
  IWidth, IHeight: Integer;
  IRect, ORect: TRect;
  I: TrmButtonState;
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
      bsMenu:
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
  State: TrmButtonState; Transparent: Boolean);
var
  Index: Integer;
begin
  if FOriginal = nil then Exit;
  if (FOriginal.Width = 0) or (FOriginal.Height = 0) then Exit;
  Index := CreateButtonGlyph(State);
  with GlyphPos do
    if Transparent then
      ImageList_DrawEx(FGlyphList.Handle, Index, Canvas.Handle, X, Y, 0, 0,
        clNone, clNone, ILD_Transparent)
    else
      ImageList_DrawEx(FGlyphList.Handle, Index, Canvas.Handle, X, Y, 0, 0,
        ColorToRGB(clBtnFace), clNone, ILD_Normal);
end;
    
procedure TButtonGlyph.DrawButtonText(Canvas: TCanvas; const Caption: string;
  TextBounds: TRect; State: TrmButtonState; BiDiFlags: LongInt);
begin
  with Canvas do
  begin
    Brush.Style := bsClear;
    if State = bsDisabled then
    begin
      OffsetRect(TextBounds, 1, 1);
      Font.Color := clBtnHighlight;
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds,
        DT_CENTER or DT_VCENTER or BiDiFlags);
      OffsetRect(TextBounds, -1, -1);
      Font.Color := clBtnShadow;
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds,
        DT_CENTER or DT_VCENTER or BiDiFlags);
    end else
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds,
        DT_CENTER or DT_VCENTER or BiDiFlags);
  end;
end;
    
procedure TButtonGlyph.CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
  const Offset: TPoint; const Caption: string; Layout: TButtonLayout; Margin,
  Spacing: Integer; var GlyphPos: TPoint; var TextBounds: TRect;
  BiDiFlags: LongInt);
var
  TextPos: TPoint;
  ClientSize, GlyphSize, TextSize: TPoint;
  TotalSize: TPoint;
begin
  if (BiDiFlags and DT_RIGHT) = DT_RIGHT then
    if Layout = blGlyphLeft then Layout := blGlyphRight
    else 
      if Layout = blGlyphRight then Layout := blGlyphLeft;
  { calculate the item sizes }
  ClientSize := Point(Client.Right - Client.Left, Client.Bottom -
    Client.Top);
    
  if FOriginal <> nil then
    GlyphSize := Point(FOriginal.Width div FNumGlyphs, FOriginal.Height) else
    GlyphSize := Point(0, 0);
    
  if Length(Caption) > 0 then
  begin
    TextBounds := Rect(0, 0, Client.Right - Client.Left, 0);
    DrawText(Canvas.Handle, PChar(Caption), Length(Caption), TextBounds,
      DT_CALCRECT or BiDiFlags);
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

function TButtonGlyph.Draw(Canvas: TCanvas; const Client: TRect; const Offset: TPoint;
      const Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
      State: TrmButtonState; Style: TrmSpeedButtonStyle; Transparent: Boolean; BiDiFlags: Longint): TRect;
var
  GlyphPos: TPoint;
  bmp : TBitmap;
begin
  if Style = sbsMenu then
  begin
    bmp := tbitmap.create;
    try
       bmp.canvas.font.assign(canvas.font);
       bmp.Height := client.bottom-client.top;
       bmp.width := client.right-client.left;

       CalcButtonLayout(bmp.Canvas, Client, Offset, Caption, Layout, Margin, Spacing,
         GlyphPos, Result, BiDiFlags);

       bmp.transparent := true;
       bmp.TransparentColor := bmp.canvas.Font.color + $010101;
       bmp.canvas.brush.color := bmp.TransparentColor;
       bmp.Canvas.FillRect(client);

       DrawButtonGlyph(bmp.Canvas, GlyphPos, State, true);
       DrawButtonText(bmp.Canvas, Caption, Result, State, BiDiFlags);
       Canvas.Draw(1,1,bmp);
    finally
       bmp.free;
    end;
  end
  else
  begin
    CalcButtonLayout(Canvas, Client, Offset, Caption, Layout, Margin, Spacing,
      GlyphPos, Result, BiDiFlags);
    DrawButtonGlyph(Canvas, GlyphPos, State, true);
    DrawButtonText(Canvas, Caption, Result, State, BiDiFlags);
  end;
end;

{ TrmCustomSpeedButton }

constructor TrmCustomSpeedButton.Create(AOwner: TComponent);
begin
  FGlyph := TButtonGlyph.Create;
  TButtonGlyph(FGlyph).OnChange := GlyphChanged;
  inherited Create(AOwner);
  FMenuDropDown := false;
  fMenuBtnWidth := 11;
  FDropDownMenu := nil;
  SetBounds(0, 0, 23, 22);
  ControlStyle := [csCaptureMouse, csDoubleClicks];
  ParentFont := True;
  Color := clBtnFace;
  FSpacing := 4;
  FMargin := -1;
  FLayout := blGlyphLeft;
  FTransparent := True;
  Inc(ButtonCount);
end;

destructor TrmCustomSpeedButton.Destroy;
begin
  Dec(ButtonCount);
  inherited Destroy;
  TButtonGlyph(FGlyph).Free;
end;

procedure TrmCustomSpeedButton.StandardPaint;
const
  DownStyles: array[Boolean] of Integer = (BDR_RAISEDINNER, BDR_SUNKENOUTER);
  FillStyles: array[Boolean] of Integer = (BF_MIDDLE, 0);
var
  PaintRect: TRect;
  DrawFlags: Integer;
  Offset: TPoint;
begin
  if not Enabled then
  begin
    FState := bsDisabled;
    FDragging := False;
  end
  else if FState = bsDisabled then
    if FDown and (GroupIndex <> 0) then
      FState := bsExclusive
    else
      FState := bsUp;
  Canvas.Font := Self.Font;
  PaintRect := Rect(0, 0, Width, Height);
  if not FFlat then
  begin
    if Style = sbsNormal then
    begin
         DrawFlags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
         if FState in [bsDown, bsExclusive] then
           DrawFlags := DrawFlags or DFCS_PUSHED;
         DrawFrameControl(Canvas.Handle, PaintRect, DFC_BUTTON, DrawFlags);
    end
    else
    begin
         if fstate in [bsDown, bsExclusive] then
            frame3d(canvas,paintrect,clbtnshadow,clbtnshadow,1)
         else
         begin
              frame3d(canvas,paintrect,clbtnface,cl3ddkshadow,1);
              frame3d(canvas,paintrect,clbtnhighlight,clbtnshadow,1);
         end;
         canvas.Brush.color := clbtnface;
         canvas.fillrect(PaintRect);
    end;
  end
  else
  begin
    if (FState in [bsDown, bsExclusive]) or
      (FMouseInControl and (FState <> bsDisabled)) or
      (csDesigning in ComponentState) then
      DrawEdge(Canvas.Handle, PaintRect, DownStyles[FState in [bsDown, bsExclusive]],
        FillStyles[FFlat] or BF_RECT);
    InflateRect(PaintRect, -1, -1);
  end;
  if FState in [bsDown, bsExclusive] then
  begin
    if (FState = bsExclusive) and (not FFlat or not FMouseInControl) then
    begin
      Canvas.Brush.Bitmap := AllocPatternBitmap(clBtnFace, clBtnHighlight);
      Canvas.FillRect(PaintRect);
    end;
    Offset.X := 0;
    Offset.Y := 0;
    if fStyle = sbsNormal then
    begin
         Offset.X := 1;
         Offset.Y := 1;
    end;
  end
  else
  begin
    Offset.X := 0;
    Offset.Y := 0;
    if fStyle = sbsComboButton then
    begin
         offset.x := -1;
         offset.y := -1;
    end;
  end;
  TButtonGlyph(FGlyph).Draw(Canvas, PaintRect, Offset, Caption, FLayout, FMargin,
    FSpacing, FState, fStyle, FFlat, DrawTextBiDiModeFlags(0));
end;

procedure TrmCustomSpeedButton.MenuPaint;
const
  DownStyles: array[Boolean] of Integer = (BDR_RAISEDINNER, BDR_SUNKENOUTER);
  FillStyles: array[Boolean] of Integer = (BF_MIDDLE, 0);
var
  PaintRect1,         //Main Button
  PaintRect2: TRect; //Drop down Button
  DrawFlags: Integer;
  Offset: TPoint;
begin
  PaintRect1 := Rect(0, 0, Width-MenuButtonWidth, Height);
  PaintRect2 := Rect((Width-MenuButtonWidth), 0, Width, Height);

  if not Enabled then
  begin
    FState := bsDisabled;
    FDragging := False;
  end
  else if FState = bsDisabled then
      FState := bsUp;
  Canvas.Font := Self.Font;
  if not FFlat then
  begin
    DrawFlags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
    if FState in [bsDown] then
       DrawFlags := DrawFlags or DFCS_PUSHED;

    DrawFrameControl(Canvas.Handle, PaintRect1, DFC_BUTTON, DrawFlags);

    if FState in [bsDown, bsMenu] then
       DrawFlags := DrawFlags or DFCS_PUSHED;

    DrawFrameControl(Canvas.Handle, PaintRect2, DFC_BUTTON, DrawFlags);
  end
  else
  begin
    if (FState in [bsDown, bsMenu]) or
      (FMouseInControl and (FState <> bsDisabled)) or
      (csDesigning in ComponentState) then
    begin
      DrawEdge(Canvas.Handle, PaintRect1, DownStyles[FState in [bsDown]],
        FillStyles[Transparent] or BF_RECT);

      DrawEdge(Canvas.Handle, PaintRect2, DownStyles[FState in [bsDown, bsMenu]],
        FillStyles[Transparent] or BF_RECT);
    end
    else if not Transparent then
    begin
      Canvas.Brush.Color := Color;
      Canvas.FillRect(PaintRect1);
      Canvas.FillRect(PaintRect2);
    end;
    InflateRect(PaintRect1, -1, -1);
  end;
  if FState in [bsDown] then
  begin
    Offset.X := 1;
    Offset.Y := 1;
  end
  else
  begin
    Offset.X := 0;
    Offset.Y := 0;
  end;

  TButtonGlyph(FGlyph).Draw(Canvas, PaintRect1, Offset, Caption, FLayout, FMargin,
    FSpacing, FState, FStyle, Transparent, DrawTextBiDiModeFlags(0));

  Canvas.Font.Name := 'Marlett';
  try
     Canvas.brush.style := bsclear;
     PaintRect2 := Rect((Width-MenuButtonWidth), 0, Width, Height);

     InflateRect(PaintRect2, -1, -1);
     Canvas.Font.height := (PaintRect2.right - PaintRect2.left);

     if FState in [bsDown, bsMenu] then
     begin
       PaintRect2.bottom := paintRect2.Bottom+1;
       if fflat then
       begin
          paintrect2.left := paintrect2.Left + 1;
          paintrect2.right := paintrect2.right + 1;
       end;
     end
     else
     begin
       if not fflat then
          PaintRect2.Left := paintRect2.left - 1;
     end;

     canvas.font.style := [];
     canvas.font.color := clBtnText;

     if FState in [bsDisabled] then
        DrawGrayText(canvas, '6', PaintRect2, DT_CENTER or DT_VCENTER or DT_SINGLELINE)
     else
        DrawText(canvas.handle,'6', 1, PaintRect2, DT_CENTER or DT_VCENTER or DT_SINGLELINE);

  finally
     Canvas.Font.assign(Font);
  end;
end;

procedure TrmCustomSpeedButton.Paint;
begin
  case fStyle of
     sbsNormal, sbsComboButton: StandardPaint;
     sbsMenu  : MenuPaint;
  else
     raise exception.create('Unknown button style.');
  end;
end;

procedure TrmCustomSpeedButton.UpdateTracking;
var
  P: TPoint;
begin
  if FFlat then
  begin
    if Enabled then
    begin
      GetCursorPos(P);
      FMouseInControl := not (FindDragTarget(P, True) = Self);
      if FMouseInControl then
        Perform(CM_MOUSELEAVE, 0, 0)
      else
        Perform(CM_MOUSEENTER, 0, 0);
    end;
  end;
end;
    
procedure TrmCustomSpeedButton.Loaded;
var
  State: TrmButtonState;
begin
  inherited Loaded;
  if Enabled then
    State := bsUp
  else
    State := bsDisabled;
  TButtonGlyph(FGlyph).CreateButtonGlyph(State);
end;

procedure TrmCustomSpeedButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and Enabled then
  begin
    if fStyle in [sbsNormal, sbsComboButton] then
    begin
       if not fDown then
       begin
         FState := bsDown;
         Invalidate;
       end;
    end
    else if fstyle = sbsmenu then
    begin
       if ptinrect(Rect((Width-MenuButtonWidth), 0, Width, Height), point(x,y)) then
       begin
          FState := bsMenu;
          FMenuDropDown := true;
       end
       else
          FState := bsDown;
       Invalidate;
    end;
    FDragging := True;
  end;
end;

procedure TrmCustomSpeedButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewState: TrmButtonState;
  UpdateState : boolean;
begin
  inherited MouseMove(Shift, X, Y);
  if FDragging then
  begin
    if fstyle = sbsmenu then
    begin
       NewState := bsUp;

       if (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y < ClientHeight) then
       begin
          if ptinrect(Rect(0, 0, Width-MenuButtonWidth, Height), point(x,y)) then
             NewState := bsDown
          else
             NewState := bsMenu;

          if ((newState = bsDown) and FMenuDropDown) then
          begin
             if fState = bsMenu then
                newState := bsup
             else
                newState := fState;
          end
          else
          if ((newState = bsMenu) and not FMenuDropDown) then
             newState := bsDown;
       end;

       updatestate := (NewState <> FState) and not
                      (((newState = bsDown) and (fState = bsMenu)) or
                       ((newState = bsMenu) and (fState = bsDown)));
    end
    else //[sbsNormal, sbsComboButton]
    begin
       if not FDown then
          NewState := bsUp
       else
          NewState := bsExclusive;

       if (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight) then
         if FDown then
            NewState := bsExclusive
         else
            NewState := bsDown;

       updatestate := (NewState <> FState);
    end;

    if updatestate then
    begin
      FState := NewState;
      Invalidate;
    end;

  end
  else if not FMouseInControl then
    UpdateTracking;
end;
    
procedure TrmCustomSpeedButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  DoClick: Boolean;
  DoMenuClick : Boolean;
  DisplayPos : TPoint;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FDragging then
  begin
    FDragging := False;

    if fStyle in [sbsNormal, sbsComboButton] then
    begin
       DoClick := (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight);

       if FGroupIndex = 0 then
       begin
         { Redraw face in-case mouse is captured }
         FState := bsUp;
         FMouseInControl := False;
         if DoClick and not (FState in [bsExclusive, bsDown]) then
           Invalidate;
       end
       else
         if DoClick then
         begin
           SetDown(not FDown);
           if FDown then Repaint;
         end
         else
         begin
           if FDown then FState := bsExclusive;
           Repaint;
         end;
    end
    else
    begin
       DoClick := ((X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y < ClientHeight)) and
                  not FMenuDropDown;

       DoMenuClick := (ptinrect(Rect((Width-MenuButtonWidth), 0, Width, Height), point(x,y)) and fMenuDropDown);

       if DoMenuClick then
       begin
          displaypos := parent.clienttoscreen(point(left, top+height));
          if assigned(FDropDownMenu) then
             FDropDownMenu.popup(displaypos.x, displaypos.y);
       end;

       FState := bsUp;
       FMenuDropDown := false;
       FMouseInControl := False;

       if (DoClick or DoMenuClick) and not (FState in [bsMenu, bsDown]) then
          Invalidate;
    end;

    if DoClick then
       Click;

    UpdateTracking;
  end;
end;
    
procedure TrmCustomSpeedButton.Click;
begin
  inherited Click;
end;
    
function TrmCustomSpeedButton.GetPalette: HPALETTE;
begin
  Result := Glyph.Palette;
end;
    
function TrmCustomSpeedButton.GetGlyph: TBitmap;
begin
  Result := TButtonGlyph(FGlyph).Glyph;
end;
    
procedure TrmCustomSpeedButton.SetGlyph(Value: TBitmap);
begin
  TButtonGlyph(FGlyph).Glyph := Value;
  Invalidate;
end;
    
function TrmCustomSpeedButton.GetNumGlyphs: TNumGlyphs;
begin
  Result := TButtonGlyph(FGlyph).NumGlyphs;
end;
    
procedure TrmCustomSpeedButton.SetNumGlyphs(Value: TNumGlyphs);
begin
  if Value < 0 then Value := 1
  else if Value > 4 then Value := 4;
  if Value <> TButtonGlyph(FGlyph).NumGlyphs then
  begin
    TButtonGlyph(FGlyph).NumGlyphs := Value;
    Invalidate;
  end;
end;
    
procedure TrmCustomSpeedButton.GlyphChanged(Sender: TObject);
begin
  Invalidate;
end;
    
procedure TrmCustomSpeedButton.UpdateExclusive;
var
  Msg: TMessage;
begin
  if (FGroupIndex <> 0) and (Parent <> nil) then
  begin
    Msg.Msg := CM_BUTTONPRESSED;
    Msg.WParam := FGroupIndex;
    Msg.LParam := Longint(Self);
    Msg.Result := 0;
    Parent.Broadcast(Msg);
  end;
end;
    
procedure TrmCustomSpeedButton.SetDown(Value: Boolean);
begin
  if FGroupIndex = 0 then Value := False;
  if FStyle = sbsMenu then value := false;
  if Value <> FDown then
  begin
    if FDown and (not FAllowAllUp) then Exit;
    FDown := Value;
    if Value then
    begin
      if FState = bsUp then Invalidate;
      FState := bsExclusive
    end
    else
    begin
      FState := bsUp;
      Repaint;
    end;
    if Value then UpdateExclusive;
  end;
end;
    
procedure TrmCustomSpeedButton.SetFlat(Value: Boolean);
begin
  if fStyle = sbsComboButton then value := false;   
  if Value <> FFlat then
  begin
    FFlat := Value;
    Invalidate;
  end;
end;
    
procedure TrmCustomSpeedButton.SetGroupIndex(Value: Integer);
begin
  if FStyle in [sbsMenu, sbsComboButton] then value := 0;
  if (FGroupIndex <> Value) then
  begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;
    
procedure TrmCustomSpeedButton.SetLayout(Value: TButtonLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;
    
procedure TrmCustomSpeedButton.SetMargin(Value: Integer);
begin
  if (Value <> FMargin) and (Value >= -1) then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;
    
procedure TrmCustomSpeedButton.SetSpacing(Value: Integer);
begin
  if Value <> FSpacing then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

procedure TrmCustomSpeedButton.SetTransparent(Value: Boolean);
begin
  if FStyle = sbsComboButton then value := false;
  if Value <> FTransparent then
  begin
    FTransparent := Value;
    if Value then
      ControlStyle := ControlStyle - [csOpaque] else
      ControlStyle := ControlStyle + [csOpaque];
    Invalidate;
  end;
end;

procedure TrmCustomSpeedButton.SetAllowAllUp(Value: Boolean);
begin
  if FStyle in [sbsMenu, sbsComboButton] then value := false;

  if FAllowAllUp <> Value then
  begin
    FAllowAllUp := Value;
    UpdateExclusive;
  end;
end;
    
procedure TrmCustomSpeedButton.WMLButtonDblClk(var Message: TWMLButtonDown);
begin
  inherited;
  if FDown then DblClick;
end;
    
procedure TrmCustomSpeedButton.CMEnabledChanged(var Message: TMessage);
const
  NewState: array[Boolean] of TrmButtonState = (bsDisabled, bsUp);
begin
  TButtonGlyph(FGlyph).CreateButtonGlyph(NewState[Enabled]);
  UpdateTracking;
  Repaint;
end;
    
procedure TrmCustomSpeedButton.CMButtonPressed(var Message: TMessage);
var
  Sender: TrmCustomSpeedButton;
begin
  if Message.WParam = FGroupIndex then
  begin
    Sender := TrmCustomSpeedButton(Message.LParam);
    if Sender <> Self then
    begin
      if Sender.Down and FDown then
      begin
        FDown := False;
        FState := bsUp;
        Invalidate;
      end;
      FAllowAllUp := Sender.AllowAllUp;
    end;
  end;
end;
    
procedure TrmCustomSpeedButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and Enabled and Visible and
      (Parent <> nil) and Parent.Showing then
    begin
      Click;
      Result := 1;
    end else
      inherited;
end;
    
procedure TrmCustomSpeedButton.CMFontChanged(var Message: TMessage);
begin
  Invalidate;
end;
    
procedure TrmCustomSpeedButton.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
end;
    
procedure TrmCustomSpeedButton.CMSysColorChange(var Message: TMessage);
begin
  with TButtonGlyph(FGlyph) do
  begin
    Invalidate;
    CreateButtonGlyph(FState);
  end;
end;
    
procedure TrmCustomSpeedButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  { Don't draw a border if DragMode <> dmAutomatic since this button is meant to 
    be used as a dock client. }
  if FFlat and not FMouseInControl and Enabled and (DragMode <> dmAutomatic) 
    and (GetCapture = 0) then
  begin
    FMouseInControl := True;
    Repaint;
  end;
end;

procedure TrmCustomSpeedButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FFlat and FMouseInControl and Enabled and not FDragging then
  begin
    FMouseInControl := False;
    Invalidate;
  end;
end;

procedure TrmCustomSpeedButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);

  procedure CopyImage(ImageList: TCustomImageList; Index: Integer);
  begin
    with Glyph do
    begin
      Width := ImageList.Width;
      Height := ImageList.Height;
      Canvas.Brush.Color := clFuchsia;//! for lack of a better color
      Canvas.FillRect(Rect(0,0, Width, Height));
      ImageList.Draw(Canvas, 0, 0, Index);
    end;
  end;

begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      { Copy image from action's imagelist }
      if (Glyph.Empty) and (ActionList <> nil) and (ActionList.Images <> nil) and
        (ImageIndex >= 0) and (ImageIndex < ActionList.Images.Count) then
        CopyImage(ActionList.Images, ImageIndex);
    end;
end;

procedure TrmCustomSpeedButton.SetDropDownMenu(const Value: TPopupMenu);
begin
  FDropDownMenu := Value;
end;

procedure TrmCustomSpeedButton.SetMenuBtnWidth(const Value: integer);
begin
  if FMenuBtnWidth <> Value then
  begin
     FMenuBtnWidth := Value;
     invalidate;
  end;
end;

procedure TrmCustomSpeedButton.SetStyle(const Value: TrmSpeedButtonStyle);
begin
  if value <> fstyle then
  begin
    fstyle := value;
    if fstyle = sbsMenu then
    begin
       FGroupIndex := 0;
       FDown := false;
       FAllowAllUp := false;
       UpdateExclusive;
    end;
    if fstyle = sbsComboButton then
    begin
       FGroupIndex := 0;
       FDown := false;
       FAllowAllUp := false;
       FFlat := false;
       FTransparent := false;
       UpdateExclusive;
    end;
    repaint;
  end;
end;

{TrmTimerSpeedButton}

destructor TrmTimerSpeedButton.Destroy;
begin
  inherited Destroy;
end;

procedure TrmTimerSpeedButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown (Button, Shift, X, Y);
  if tbAllowTimer in FTimeBtnState then
  begin
    FUnitTimer.OnTimer := TimerExpired;
    FUnitTimer.Interval := InitRepeatPause;
    FUnitTimer.Enabled  := True;
  end;
end;

procedure TrmTimerSpeedButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
                                  X, Y: Integer);
begin
  inherited MouseUp (Button, Shift, X, Y);
  FUnitTimer.Enabled := false;
  FUnitTimer.OnTimer := nil;
end;

procedure TrmTimerSpeedButton.TimerExpired(Sender: TObject);
begin
  FUnitTimer.Interval := RepeatPause;
  if (FState = bsDown) and MouseCapture then
  begin
    try
      Click;
    except
      FUnitTimer.Enabled := False;
      raise;
    end;
  end;
end;

procedure TrmTimerSpeedButton.Paint;
var
  R: TRect;
begin
  inherited Paint;
  if tbFocusRect in FTimeBtnState then
  begin
    R := Bounds(0, 0, Width, Height);
    InflateRect(R, -3, -3);
    if FState = bsDown then
      OffsetRect(R, 1, 1);
    DrawFocusRect(Canvas.Handle, R);
  end;
end;

{ TrmSpinButton }

constructor TrmSpinButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] +
    [csOpaque];

  FUpButton := CreateButton;
  FDownButton := CreateButton;

  fDownGlyphDefault := true;
  fUpGlyphDefault := true;

  UpGlyph := nil;
  DownGlyph := nil;

  Width := 20;
  Height := 2;
  fUpEnabled := true;
  fDownEnabled := true;

  FFocusedButton := FUpButton;
end;

function TrmSpinButton.CreateButton: TrmTimerSpeedButton;
begin
  Result := TrmTimerSpeedButton.Create (Self);
  Result.OnClick := BtnClick;
  Result.OnMouseDown := BtnMouseDown;
  Result.Visible := True;
  Result.Enabled := True;
  Result.TimeBtnState := [tbAllowTimer];
  Result.Parent := Self;
  Result.Style := sbsComboButton;
  Result.Layout := blGlyphTop;
end;

procedure TrmSpinButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFocusControl) then
    FFocusControl := nil;
end;

procedure TrmSpinButton.AdjustSize (var W, H: Integer);
begin
  if (FUpButton = nil) or (csLoading in ComponentState) then
    Exit;
  if W < 15 then W := 15;
  FUpButton.SetBounds (0, 0, W, H div 2);
  FDownButton.SetBounds (0, FUpButton.Height, W, (H div 2) + (h mod 2));
end;

procedure TrmSpinButton.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  W, H: Integer;
begin
  W := AWidth;
  H := AHeight;
  AdjustSize (W, H);
  inherited SetBounds (ALeft, ATop, W, H);
end;

procedure TrmSpinButton.WMSize(var Message: TWMSize);
var
  W, H: Integer;
begin
  inherited;

  { check for minimum size }
  W := Width;
  H := Height;
  AdjustSize (W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds(Left, Top, W, H);
  Message.Result := 0;
end;

procedure TrmSpinButton.WMSetFocus(var Message: TWMSetFocus);
begin
  FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState + [tbFocusRect];
  FFocusedButton.Invalidate;
end;

procedure TrmSpinButton.WMKillFocus(var Message: TWMKillFocus);
begin
  FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState - [tbFocusRect];
  FFocusedButton.Invalidate;
end;

procedure TrmSpinButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_UP:
      begin
        SetFocusBtn (FUpButton);
        FUpButton.Click;
      end;
    VK_DOWN:
      begin
        SetFocusBtn (FDownButton);
        FDownButton.Click;
      end;
    VK_SPACE:
      FFocusedButton.Click;
  end;
end;

procedure TrmSpinButton.BtnMouseDown (Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    SetFocusBtn (TrmTimerSpeedButton (Sender));
    if (FFocusControl <> nil) and FFocusControl.TabStop and 
        FFocusControl.CanFocus and (GetFocus <> FFocusControl.Handle) then
      FFocusControl.SetFocus
    else if TabStop and (GetFocus <> Handle) and CanFocus then
      SetFocus;
  end;
end;

procedure TrmSpinButton.BtnClick(Sender: TObject);
begin
  if Sender = FUpButton then
  begin
    if Assigned(FOnUpClick) then FOnUpClick(Self);
  end
  else
    if Assigned(FOnDownClick) then FOnDownClick(Self);
end;

procedure TrmSpinButton.SetFocusBtn (Btn: TrmTimerSpeedButton);
begin
  if TabStop and CanFocus and  (Btn <> FFocusedButton) then
  begin
    FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState - [tbFocusRect];
    FFocusedButton := Btn;
    if (GetFocus = Handle) then 
    begin
       FFocusedButton.TimeBtnState := FFocusedButton.TimeBtnState + [tbFocusRect];
       Invalidate;
    end;
  end;
end;

procedure TrmSpinButton.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;

procedure TrmSpinButton.Loaded;
var
  W, H: Integer;
begin
  inherited Loaded;
  W := Width;
  H := Height;
  AdjustSize (W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds (Left, Top, W, H);
end;

function TrmSpinButton.GetUpGlyph: TBitmap;
begin
  Result := FUpButton.Glyph;
end;

procedure TrmSpinButton.SetUpGlyph(Value: TBitmap);
var
   bmp : TBitmap;
begin
  if Value <> nil then
  begin
    FUpButton.Glyph := Value;
    fUpGlyphDefault := false;
  end
  else
  begin
    bmp := tbitmap.create;
    try
       fUpGlyphDefault := true;
       bmp.LoadFromResourceName(HInstance, 'rm_SpinUp');
       ReplaceColors(bmp, clBtnFace, clBtnText);
       FUpButton.Glyph := bmp;
    finally
       bmp.free;
    end;
  end;
end;

function TrmSpinButton.GetUpNumGlyphs: TNumGlyphs;
begin
  Result := FUpButton.NumGlyphs;
end;

procedure TrmSpinButton.SetUpNumGlyphs(Value: TNumGlyphs);
begin
  FUpButton.NumGlyphs := Value;
end;

function TrmSpinButton.GetDownGlyph: TBitmap;
begin
  Result := FDownButton.Glyph;
end;

procedure TrmSpinButton.SetDownGlyph(Value: TBitmap);
var
   bmp : TBitmap;
begin
  if Value <> nil then
  begin
    FDownButton.Glyph := Value;
    fDownGlyphDefault := false;
  end
  else
  begin
    bmp := tbitmap.create;
    try
       fDownGlyphDefault := true;
       bmp.LoadFromResourceName(HInstance, 'rm_SpinDown');
       ReplaceColors(bmp, clBtnFace, clBtnText);
       FDownButton.Glyph := bmp;
    finally
       bmp.free;
    end;
  end;
end;

function TrmSpinButton.GetDownNumGlyphs: TNumGlyphs;
begin
  Result := FDownButton.NumGlyphs;
end;

procedure TrmSpinButton.SetDownNumGlyphs(Value: TNumGlyphs);
begin
  FDownButton.NumGlyphs := Value;
end;

function TrmSpinButton.GetDownEnabled: boolean;
begin
   result := FDownButton.Enabled;  
end;

function TrmSpinButton.GetUpEnabled: boolean;
begin
   result := FUpButton.Enabled;
end;

procedure TrmSpinButton.SetDownEnabled(const Value: boolean);
begin
   if FDownEnabled <> value then
   begin
      fDownEnabled := value;
      FDownButton.Enabled := fDownEnabled and enabled;
   end;
end;

procedure TrmSpinButton.SetUpEnabled(const Value: boolean);
begin
   if fUpEnabled <> value then
   begin
      fUpEnabled := value;
      FUpButton.Enabled := fUpEnabled and enabled;
   end;
end;

procedure TrmSpinButton.CMSysColorChange(var Message: TMessage);
begin
   if fUpGlyphDefault then
      SetUpGlyph(nil);

   if fDownGlyphDefault then
      SetDownGlyph(nil);
end;

procedure TrmSpinButton.SetEnabled(value: boolean);
begin
  if value <> Enabled then
  begin
     if value = false then
     begin
        fUpButton.enabled := false;
        fDownButton.enabled := false;
     end
     else
     begin
        FUpButton.Enabled := fUpEnabled;
        FDownButton.enabled := fDownEnabled;
     end;
  end;
  inherited;
end;

initialization
  fUnitTimer := TTimer.create(nil);

finalization
  fUnitTimer.Free;

end.
