unit cbxbase;

interface

uses
{$IFDEF Win32}
  Windows, Messages, Classes, Controls, Forms, Graphics, StdCtrls,
  ExtCtrls, CommCtrl,
{$ELSE}
  WinTypes, WinProcs, SysUtils,  Messages, Classes, Graphics, StdCtrls,
  Controls, Forms, menus,
{$ENDIF}
  Buttons {Buttons just to get the DrawButtonFace method}  ;

type
  TDDLinkBase = class;
  TCBButton = class;
  TNumBtnGlyphs = 1..3;
  TLeftRight = (Left, Right);
  TBelowAbove = (Below, Above);

  TCBXBase = class(TCustomEdit)
  private
    FGlyph : TBitmap;
    FShowSpeedButton : Boolean;
    fUserDraw : Boolean;
    fDDLink : TDDLinkBase;
    FOriginalStr : String; {Holds the original value at entering the field supports undo}
    FDropDownAlign: TLeftRight;
    FDropDownTop: TBelowAbove;
    fOnButtonClick : TNotifyEvent;
    FOnCloseUp: TNotifyEvent; 
    {DEFINATELY NEED A BETTER WAY OF SELECTING PARTS OF TEXT}
    procedure CreateDefaultButton;
    function GetButtonCursor : TCursor;
    procedure SetButtonCursor(value : TCursor);
    Procedure SetShowSpeedButton(Value: Boolean);
    function GetButtonGlyph : TBitmap;
    procedure SetButtonGlyph(value : TBitmap);
    function GetButtonNumGlyphs : TNumBtnGlyphs;
    procedure SetButtonNumGlyphs(value : TNumBtnGlyphs);
    function GetDropped : Boolean;
    procedure WMCut (var Message: TMessage); message WM_CUT;
    procedure WMPaste (var Message: TMessage); message WM_PASTE;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    function GetListFont : TFont;
    procedure SetListFont(value : TFont);
    function GetListParentFont: Boolean;
    procedure SetListParentFont(Value: Boolean);
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMParentFontChanged(var Message: TMessage); message CM_PARENTFONTCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  protected
    { Protected declarations }
    fCanvas : TControlCanvas;
    FButton: TCBButton;
    procedure Paint; virtual;
    procedure DrawNonEditFocusRect(var Message: TWMPaint); virtual;
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Loaded; override;
    procedure SetEditRect; virtual;
    function CanEdit: Boolean; virtual;
    function Editable: Boolean; virtual;
    function GetMinHeight: Integer; virtual;
    procedure Reset; virtual;
    procedure Change; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    function IsDropDownFontStored: Boolean;
    property DDLink : TDDLinkBase read fDDLink write fDDLink;
    property Button: TCBButton read FButton;
  public
    Constructor Create(aOwner: TComponent); override;
    Destructor Destroy; override;
    property Canvas : TControlCanvas read fCanvas write fCanvas;
    property Dropped : Boolean read GetDropped;
    procedure DropDown; dynamic;
    procedure CloseUp; dynamic;
    property ListFont: TFont read GetListFont write SetListFont stored IsDropDownFontStored;
    property ListParentFont : Boolean read GetListParentFont write SetListParentFont default True;
    property UserDraw : Boolean read fUserDraw write fUserDraw default false;
    property DropDownAlign: TLeftRight read FDropDownAlign write FDropDownAlign default Right;
    property DropDownTop: TBelowAbove read FDropDownTop write FDropDownTop default Below;
    property ButtonGlyph : TBitmap read getButtonGlyph write SetButtonGlyph;
    {ShowSpeedButton must come after ButtonGlyph for reader/writer to work write}
    property ShowSpeedButton : Boolean read FShowSpeedButton Write SetShowSpeedButton default True;
    property ButtonNumGlyphs: TNumBtnGlyphs read GetButtonNumGlyphs write SetButtonNumGlyphs default 1;
    property ButtonCursor : TCursor read GetButtonCursor write SetButtonCursor default crArrow;
  published
    { Published declarations }
    property OnButtonClick: TNotifyEvent read fOnButtonClick write fOnButtonClick;
    property OnCloseUp: TNotifyEvent read FOnCloseUp write FOnCloseUp;
  end;

  TDDLinkBase = class(TPersistent)
  private
    fDropDownObj : TWinControl;
    fOwner : TCBXBase; {TComponent;}
    fListFont : TFont;
    fListParentFont : Boolean;
  protected
    function GetVisible : Boolean; virtual;
    procedure SetVisible(value : Boolean); virtual;
    procedure SetListFont(const Value: TFont);
    procedure SetListParentFont(value : Boolean);
    procedure DropDownFontChanged(Sender: TObject);
  public
    Constructor Create(aOwner: TCBXBase); virtual;
    Destructor Destroy; override;
    procedure DropDown; dynamic;
    procedure CloseUp; dynamic;
    procedure Setbounds(ALeft, ATop, AWidth, AHeight: Integer); dynamic;
    property Visible : Boolean read GetVisible write SetVisible;
    property DropDownObj : TWinControl read fDropDownObj write fDropDownObj;
    property Owner : TCBXBase read fOwner write fOwner;
    property ListFont : TFont read fListFont write SetListFont;
    property ListParentFont: Boolean read FListParentFont write SetListParentFont;
  end;

  { TCBButton }
  TCBButtonState = (bsUp, bsDisabled, bsDown);

  TCBButton = class(TControl)
  private
    FCanvas: TCanvas;
    FGlyph: Pointer;
    FDragging: Boolean;
    FMargin: Integer;
    FButtonCursor: TCursor; {This cursor is in addition to inherited cursor which is used in the edit part}
    procedure SetButtonCursor(value : TCursor);
    procedure GlyphChanged(Sender: TObject);
    function GetGlyph: TBitmap;
    procedure SetGlyph(Value: TBitmap);
    function GetNumGlyphs: TNumBtnGlyphs;
    procedure SetNumGlyphs(Value: TNumBtnGlyphs);
    procedure SetMargin(Value: Integer);
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    FState: TCBButtonState;
    function GetPalette: HPALETTE; override;
{new}    procedure Loaded; override;
    procedure Paint; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent; aCanvas : TCanvas); 
    {$IFDEF VER130}
       reintroduce;
    {$ENDIF}
    {$IFDEF VER140}
       reintroduce;
    {$ENDIF}
    destructor Destroy; override;
    procedure Click; override;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property Margin: Integer read FMargin write SetMargin default -1;
    property NumGlyphs: TNumBtnGlyphs read GetNumGlyphs write SetNumGlyphs default 1;
    property ButtonCursor : TCursor read fButtonCursor write SetButtonCursor default crArrow;
    property Canvas: TCanvas read FCanvas write FCanvas;
    property OnClick;
  end;


procedure MouseDragToDropdownWin (Ctrl: TControl; DDLink: TDDLinkBase; X, Y: Integer);

implementation
{ Helper functions ------------------------------}

procedure MouseDragToDropdownWin (Ctrl: TControl; DDLink: TDDLinkBase; X, Y: Integer);
{This helper function is for the situation when the user drags (holds down the
left mouse button) from the CB to what ever the dropdown is}
var
  pt, clientPt: TPoint;
begin
  if (DDLink.Visible) then
  begin
    pt.X := X;
    pt.Y := Y;
    pt := Ctrl.ClientToScreen (pt);
    clientPt := DDLink.DropDownObj.ClientOrigin;
    if (pt.X >= clientPt.X) and (pt.Y >= clientPt.Y) and
       (pt.X <= clientPt.X + DDLink.DropDownObj.ClientWidth) and
       (pt.Y <= clientPt.Y + DDLink.DropDownObj.ClientHeight) then
    begin
      Ctrl.Perform(WM_LBUTTONUP, 0, MakeLong (abs(X), abs(Y)));
      pt := DDLInk.DropDownObj.ScreenToClient(pt);
      DDLink.DropDownObj.Perform(WM_LBUTTONDOWN, 0, MakeLong (abs(pt.x), abs(pt.y)));
    end;
  end;
end;

procedure TCBXBase.CreateDefaultButton;
begin
  FButton := TCBButton.Create(Self, fCanvas);
  FButton.Parent := Self;
{  AutoSize := True; }
end;

Constructor TCBXBase.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
{$IFDEF Win32}
  ControlStyle := ControlStyle + [csReplicatable]; {so can copy into CtrlGrid}
{$ENDIF}
  {Create a control canvas}
  fCanvas := TControlCanvas.Create;
  FCanvas.Control := Self;
  AutoSize := False;
  height := 24;  {this only has an effect if AutoSize remains false and newstyle controls}
  FShowSpeedButton := True;
  fUserDraw := False;
  fGlyph := TBitmap.create;
  CreateDefaultButton;
  FButton.Cursor := crArrow;

  fDDLink := nil;
  FOriginalStr := '';
  FDropDownAlign := Right;
  FDropDownTop := Below;
end;

Destructor TCBXBase.Destroy;
begin
  fCanvas.Free;
  if assigned(fGlyph) then fGlyph.Free;
  if assigned(FButton) then fButton.Free;
  inherited Destroy;
end;

Procedure TCBXBase.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
end;

procedure TCBXBase.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN;
end;

procedure TCBXBase.Loaded;
begin
  inherited Loaded;
  if not NewStyleControls then invalidate;   {Hand tuning}
  if not (ComponentState = []) then Text := Name;
end;

function TCBXBase.CanEdit: Boolean;
{CanEdit should return true if we are in edit mode and false otherwise. This is
most applicable in dataaware controls}
begin
  Result := True;
end;

function TCBXBase.Editable: Boolean;
{ Does the controls style and properties allow it to be editable }
begin
  Result := True;
end;

procedure TCBXBase.Change;
{Change is the protected implementation method for the OnChange event.
For each of these components, the default Change method does nothing except
call any event handler attached to the OnChange event. You override Change to
perform any other desired response to changes in these components.
OnChange occurs when the Text property is modified or when an item is selected
in the drop down.
THIS IS HERE AS A REMINDER}
begin
  {do stuff}
  inherited Change;
  {do stuff}
end;

procedure TCBXBase.MouseMove(Shift: TShiftState; X, Y: Integer);
{MouseMove is the protected implementation for the OnMouseMove event.
The MouseMove method inherited from TControl does nothing except call any
event handler attached to the OnMouseMove event. You can override MouseMove
to provide other responses in addition to the inherited event-handler call.
A control calls MouseMove in response to any of the Windows mouse-move messages
(WM_MOUSEMOVE), decoding the message parameters into the shift-key state and
position, which it passes in the Shift, X, and Y parameters, respectively.
THIS IS HERE AS A REMINDER}
begin
  inherited MouseMove (Shift, X, Y);
end;

procedure TCBXBase.Reset;
begin
  Text := FOriginalStr;
end;

procedure TCBXBase.SetEditRect;
var
  Loc: TRect;
begin
  {For Windows 3.1 Bottom must be set to client height + 1 and the top and
  left must be set back to zero before calling en_setrect otherwise the
  caret disappears}
  Loc.Bottom := ClientHeight + 1;  {+1 is workaround for windows paint bug}
  if assigned(FButton) then
    Loc.Right := FButton.Left - 2
  else
    Loc.Right := ClientWidth;
  Loc.Top := 0;
  Loc.Left := 0;
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@Loc));
end;

function TCBXBase.GetButtonCursor : TCursor;
begin
  if assigned(FButton) then
    result := FButton.ButtonCursor
  else
    result := crArrow;
end;

procedure TCBXBase.SetButtonCursor(value : TCursor);
begin
  FButton.ButtonCursor := value;
end;

Procedure TCBXBase.SetShowSpeedButton(Value: Boolean);
begin
  if FShowSpeedButton <> Value then
  begin
    FShowSpeedButton := Value;
    { If no button then destroy it }
    If not FShowSpeedButton then
    begin
      if assigned(FButton) then
      begin
        FButton.Free;
        FButton := Nil;
      end;
    end
    else
    { else if show button then create it and load the custome Bitmap if available }
    begin
      {Note that fbutton is assinged during load time so care must be taken not to
       create it twice.
       Also note that the bitmap in fGlyph is assigned to FButton in WMPaint}
      if not assigned(FButton) then
        CreateDefaultButton;

      if not NewStyleControls then
        FButton.SetBounds (Width - GetSystemMetrics(SM_CXVSCROLL), 0, GetSystemMetrics(SM_CXVSCROLL), Height)
      else
{$IFDEF Win32}
        fButton.SetBounds (ClientWidth - GetSystemMetrics(SM_CXVSCROLL) {- 2},
                            0, GetSystemMetrics(SM_CXVSCROLL), ClientHeight);
{$ELSE}
        FButton.SetBounds (Width - GetSystemMetrics(SM_CXVSCROLL) - 2,
                            2, GetSystemMetrics(SM_CXVSCROLL), ClientHeight - 4);
{$ENDIF}
      SetEditRect;
    end;
    if HandleAllocated then
      SetEditRect;
  end;
end;

function TCBXBase.GetButtonGlyph : TBitmap;
begin
  result := fGlyph;
end;

procedure TCBXBase.SetButtonGlyph(value : TBitmap);
begin
  if value <> nil then
  begin
    { The button glyph defaults to the downarrow in the fbutton create so the only
      time this is called is when the property is set to something other than the
      default. So Store the difference in fGlyph}
     fGlyph.assign(value);
    { Also note that the bitmap in fGlyph is assigned to FButton in WMPaint}
    if csdesigning in componentstate then invalidate;
  end
  else
  begin
    {First destroy and create internal Glyph (fGlyph) to clear it}
    if assigned(fGlyph) then
    begin
      fGlyph.Free;
      fGlyph := TBitmap.create;
    end;
    if assigned(FButton) then
    begin
       ButtonNumGlyphs := 1;
       FButton.Glyph.Handle := LoadBitmap(0, PChar(32738));
   end;
  end;
end;

function TCBXBase.GetButtonNumGlyphs : TNumBtnGlyphs;
begin
  If assigned(FButton) then
    result := FButton.NumGlyphs
  else
    result := 1;
end;

procedure TCBXBase.SetButtonNumGlyphs(value : TNumBtnGlyphs);
begin
  if assigned(FButton) then
    FButton.NumGlyphs := value;
end;

function TCBXBase.GetDropped : Boolean;
begin
  result := assigned(FDDLink) and assigned(FDDLink.fDropDownObj);
end;

function TCBXBase.IsDropDownFontStored: Boolean;
begin
  Result := not DDLink.ListParentFont;
end;

function TCBXBase.GetListParentFont: Boolean;
begin
  result := DDLink.ListParentFont;
end;

procedure TCBXBase.SetListParentFont(Value: Boolean);
begin
  DDLink.ListParentFont := value;
end;

function TCBXBase.GetListFont : TFont;
begin
  result := DDLink.ListFont;
end;

procedure TCBXBase.SetListFont(value : TFont);
begin
  DDLink.ListFont := value;
end;

procedure TCBXBase.CMFontChanged(var Message: TMessage);
begin
  inherited;
  GetMinHeight;
  Perform(CM_PARENTFONTCHANGED, 0, 0); 
end;

procedure TCBXBase.CMParentFontChanged(var Message: TMessage);
begin
  inherited;
  if assigned(fDDLink) and ListParentFont then
  begin
    ListFont := Font;
    ListParentFont := True;
  end;
end;

procedure TCBXBase.WMCut (var Message: TMessage);
{If its editable then put in in edit mode. If edit mode is successful then procede.}
begin
  if CanEdit then inherited;
end;

procedure TCBXBase.WMPaste (var Message: TMessage);
{If its editable then put in in edit mode. If edit mode is successful then procede.}
begin
  if CanEdit then inherited;
end;

procedure TCBXBase.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  CloseUp;
end;

procedure TCBXBase.CMCancelMode(var Message: TCMCancelMode);
begin
  if not assigned(FDDLink) then exit;
  with Message do
    if (Sender <> Self) and (Sender <> FButton) and
      (FDDLink.fDropDownObj <> nil) and (Sender <> FDDLink.fDropDownObj) then
      CloseUp;
end;

procedure TCBXBase.DropDown;
{Shows whatever is the dropdown}
begin
  if assigned(FDDLink) and
     not assigned(fDDLink.fDropDownObj)  then
    FDDLink.DropDown;
end;

procedure TCBXBase.CloseUp;
{Closes whatever is the dropdown}
begin
  if not assigned(FDDLink) then exit;
  FDDLink.CloseUp;
  if assigned(FOnCloseUp) then
    FOnCloseUp(Self);
end;

function TCBXBase.GetMinHeight: Integer;
var
  DC: HDC;
  SaveFont: HFont;
  Metrics: TTextMetric;
begin
  If NewStyleControls or (BorderStyle = bsNone) then Result := 0
  else
  begin
    DC := GetDC(0);
    SaveFont := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, SaveFont);
    ReleaseDC(0, DC);
    Result := Metrics.tmHeight + (Metrics.tmHeight div 4) + GetSystemMetrics(SM_CYBORDER) * 4 + 1;
  end;
end;

procedure TCBXBase.WMSize(var Message: TWMSize);
var
  MinHeight: Integer;
begin
  inherited;
  if (csDesigning in ComponentState) and assigned(FDDLink) then
    FDDLink.SetBounds (0, Height + 1, 10, 10);
  MinHeight := GetMinHeight;
  { text edit bug: if size to less than minheight, then edit ctrl does not display the text }
  { The above statement was in the original code. I'm not sure I buy it.}
  if Height < MinHeight then Height := MinHeight
  else
  begin
    if ShowSpeedButton then
    begin
      if not NewStyleControls then
        FButton.SetBounds (Width - GetSystemMetrics(SM_CXVSCROLL), 0, GetSystemMetrics(SM_CXVSCROLL), Height)
      else
{$IFDEF Win32}
        fButton.SetBounds (ClientWidth - GetSystemMetrics(SM_CXVSCROLL) {- 2},
                            0, GetSystemMetrics(SM_CXVSCROLL), ClientHeight);
{$ELSE}
        FButton.SetBounds (Width - GetSystemMetrics(SM_CXVSCROLL) - 2,
                            2, GetSystemMetrics(SM_CXVSCROLL), ClientHeight - 4);
{$ENDIF}
    end;
    SetEditRect;
  end;
end;

procedure TCBXBase.Paint;
{This will only be called if UserDraw is true, in which case it must be defined}
begin
  {abstract}
end;

procedure TCBXBase.DrawNonEditFocusRect(var Message: TWMPaint);
{ called by WMPaint, this needs to be able to handle any special painting
  of the inside of the edit when editable is false }
begin
  {abstract}
end;
  
procedure TCBXBase.WMPaint(var Message: TWMPaint);
var
  PS: TPaintStruct;
  DC: HDC;
begin
  if not(fGlyph.empty) and assigned(fButton) then
    fButton.Glyph := fGlyph;

  if Not fUserDraw then
  begin
    inherited;
    if (not(editable) and Focused)
{$IFDEF Win32}
   or (csPaintCopy in ControlState)
{$ENDIF} then
      DrawNonEditFocusRect(Message)
  end
  else
  begin
    DC := Message.DC;
    If DC = 0 then
      DC := BeginPaint(Handle, PS);

    fCanvas.Handle := DC;
    try
      Paint;
    finally
      fCanvas.Handle := 0;
      if Message.DC = 0 then EndPaint(Handle, PS);
    end;
  end;
end;

procedure TCBXBase.CMExit(var Message: TCMExit);
{Do stuff when the CB loses focus}
{Decendents should do things like update the place where the data is stored like
an in memory record or a table The dommend out is an example for say an in mem record}
begin
  inherited;
end;

procedure TCBXBase.CMEnter(var Message: TCMGotFocus);
{Do stuff when the CB gains focus}
begin
  FOriginalStr := Text;
  if AutoSelect and not(csLButtonDown in ControlState) then SelectAll;
  inherited;
end;

procedure TCBXBase.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if assigned(FButton) then
    FButton.Enabled := Enabled;
end;

{-------------------------------------------------------------------------}
Constructor TDDLinkBase.Create(aOwner: TCBXBase);
begin
  inherited Create;
  fOwner := aOwner;
  fDropDownObj := nil;

  fListParentFont := True;
  FListFont := TFont.Create;
  FListFont.OnChange := DropDownFontChanged;

  FListFont.assign(fOwner.Font);

  Visible := False;
end;

Destructor TDDLinkBase.Destroy;
begin
  FListFont.Free;
  inherited destroy;
end;

 procedure TDDLinkBase.DropDown;
{Shows whatever is the dropdown}
   {At a minimum you must create or show the dropdown window here}
begin
   {Abstract}
end;

procedure TDDLinkBase.CloseUp;
{Closes whatever is the dropdown}
   {At a minimum you must destroy or hide the dropdown window here}
begin
   {Abstract}
end;

procedure TDDLinkBase.Setbounds(ALeft, ATop, AWidth, AHeight: Integer);
{ Use Setbounds to pass the suggested location and size of the dropdown window
  to the dropdown window }
begin
   {Abstract}
end;

function TDDLinkBase.GetVisible : Boolean;
{returns true if fDropDownObj is visible}
begin
   {Abstract}
   result := assigned(fDropDownObj) and fDropDownObj.visible;
//   result := False;
end;

procedure TDDLinkBase.SetVisible(value : Boolean);
{makes fDropDownObj to visible or not.}
begin
   {Abstract}
end;

procedure TDDLinkBase.DropDownFontChanged(Sender: TObject);
begin
  FListParentFont := False;
end;

procedure TDDLinkBase.SetListFont(const Value: TFont);
begin
  FListFont.Assign(Value);
end;

procedure TDDLinkBase.SetListParentFont(Value: Boolean);
begin
  if fListParentFont <> value then
  begin
    if Value then
      FListFont.Assign(Owner.Font);
    FListParentFont := Value;
  end;
end;


{-- ALL the real button stuff starts here ---------------------- }
{$IFNDEF Win32}
{ TBitPool }
const
  BitsPerInt = SizeOf(Integer) * 8;
type
  TBitEnum = 0..BitsPerInt - 1;
  TBitSet = set of TBitEnum;
  TBitPool = class
  private
    FSize: Integer;
    FBits: Pointer;
    procedure SetSize(Value: Integer);
    procedure SetBit(Index: Integer; Value: Boolean);
    function GetBit(Index: Integer): Boolean;
  public
    destructor Destroy; override;
    function OpenBit: Integer;
    property Bits[Index: Integer]: Boolean read GetBit write SetBit; default;
    property Size: Integer read FSize write SetSize;
  end;
{$ENDIF}

Type
  TGlyphList = class(TImageList)
  private
{$IFDEF Win32}
    Used: TBits;
{$ELSE}
    Used: TBitPool;
{$ENDIF}
    FCount: Integer;
    function AllocateIndex: Integer;
  public
    constructor Create(AWidth, AHeight: Integer);
    {$IFDEF VER130}
       reintroduce;
    {$ENDIF}
    {$IFDEF VER140}
       reintroduce;
    {$ENDIF}
    destructor Destroy; override;
    function Add(Image, Mask: TBitmap): Integer;
{    function AddMasked(Image: TBitmap; MaskColor: TColor): Integer; }
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
    FIndexs: array[TCBButtonState] of Integer;
    FTransparentColor: TColor;
    FNumGlyphs: TNumBtnGlyphs;
    FOnChange: TNotifyEvent;
    procedure GlyphChanged(Sender: TObject);
    procedure SetGlyph(Value: TBitmap);
    procedure SetNumGlyphs(Value: TNumBtnGlyphs);
    procedure Invalidate;
    function CreateButtonGlyph(State: TCBButtonState): Integer;
    procedure DrawButtonGlyph(Canvas: TCanvas; X, Y: Integer;
      State: TCBButtonState);
    procedure CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
      Margin: Integer; var GlyphPos: TPoint);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Draw(Canvas: TCanvas; const Client: TRect;
      Margin: Integer; State: TCBButtonState);
    property Glyph: TBitmap read FOriginal write SetGlyph;
    property NumGlyphs: TNumBtnGlyphs read FNumGlyphs write SetNumGlyphs;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{$IFNDEF Win32}   {TBitPool is replaced in D2+ with TBits}
type
  PBitArray = ^TBitArray;
  TBitArray = array[0..4096] of TBitSet;

destructor TBitPool.Destroy;
begin
  SetSize(0);
  inherited Destroy;
end;

procedure TBitPool.SetSize(Value: Integer);
var
  NewMem: Pointer;
  NewMemSize: Integer;
  OldMemSize: Integer;

  function Min(X, Y: Integer): Integer;
  begin
    Result := X;
    if X > Y then Result := Y;
  end;

begin
  if Value <> Size then
  begin
    NewMemSize := ((Value + BitsPerInt - 1) div BitsPerInt) * SizeOf(Integer);
    OldMemSize := ((Size + BitsPerInt - 1) div BitsPerInt) * SizeOf(Integer);
    if NewMemSize <> OldMemSize then
    begin
      NewMem := nil;
      if NewMemSize <> 0 then
      begin
        GetMem(NewMem, NewMemSize);
        FillChar(NewMem^, NewMemSize, 0);
      end
      else NewMem := nil;
      if OldMemSize <> 0 then
      begin
        if NewMem <> nil then
          Move(FBits^, NewMem^, Min(OldMemSize, NewMemSize));
        FreeMem(FBits, OldMemSize);
      end;
      FBits := NewMem;
    end;
    FSize := Value;
  end;
end;

procedure TBitPool.SetBit(Index: Integer; Value: Boolean);
begin
  if Value then
    Include(PBitArray(FBits)^[Index div BitsPerInt], Index mod BitsPerInt)
  else
    Exclude(PBitArray(FBits)^[Index div BitsPerInt], Index mod BitsPerInt);
end;

function TBitPool.GetBit(Index: Integer): Boolean;
begin
  Result := Index mod BitsPerInt in PBitArray(FBits)^[Index div BitsPerInt];
end;

function TBitPool.OpenBit: Integer;
var
  I: Integer;
  B: TBitSet;
  J: TBitEnum;
  E: Integer;
begin
  E := (Size + BitsPerInt - 1) div BitsPerInt - 1;
  for I := 0 to E do
    if PBitArray(FBits)^[I] <> [0..BitsPerInt - 1] then
    begin
      B := PBitArray(FBits)^[I];
      for J := Low(J) to High(J) do
      begin
        if not (J in B) then
        begin
          Result := I * BitsPerInt + J;
          if Result >= Size then Result := -1;
          Exit;
        end;
      end;
    end;
  Result := -1;
end;
{$ENDIF}

{ TGlyphList ----------------------------------------- }

constructor TGlyphList.Create(AWidth, AHeight: Integer);
begin
{$IFDEF Win32}
  inherited CreateSize(AWidth, AHeight);
  Used := TBits.Create;
{$ELSE}
  inherited Create(AWidth, AHeight);
  Used := TBitPool.Create;
{$ENDIF}
end;

destructor TGlyphList.Destroy;
begin
  Used.Free;
  inherited Destroy;
end;

function TGlyphList.AllocateIndex: Integer;
begin
  Result := Used.OpenBit;
{$IFDEF Win32}
  if Result >= Used.Size then
{$ELSE}
  if Result = -1 then
{$ENDIF}
  begin
    Result := inherited Add(nil, nil);
    Used.Size := Result + 1;
  end;
  Used[Result] := True;
end;

function TGlyphList.Add(Image, Mask: TBitmap): Integer;
begin
  Result := AllocateIndex;
  Replace(Result, Image, Mask);
  Inc(FCount);
end;

(*
function TGlyphList.AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
begin
  Result := AllocateIndex;
  ReplaceMasked(Result, Image, MaskColor);
  Inc(FCount);
end;
*)

procedure TGlyphList.Delete(Index: Integer);
begin
  if Used[Index] then
  begin
    Dec(FCount);
    Used[Index] := False;
  end;
end;

{ TGlyphCache ----------------------------------------------- }

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
  Result := TGlyphList.Create(AWidth, AHeight);
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
  GlyphCache: TGlyphCache;

{ TButtonGlyph }

constructor TButtonGlyph.Create;
var
  I: TCBButtonState;
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
  I: TCBButtonState;
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
      if Glyphs > 3 then Glyphs := 1;
      SetNumGlyphs(Glyphs);
    end;
  end;
end;

procedure TButtonGlyph.SetNumGlyphs(Value: TNumBtnGlyphs);
begin
  if (Value <> FNumGlyphs) and (Value > 0) then
  begin
    Invalidate;
    FNumGlyphs := Value;
  end;
end;

function TButtonGlyph.CreateButtonGlyph(State: TCBButtonState): Integer;
const
{New}  ROP_DSPDxax = $00E20746;
var
  TmpImage, MonoBmp: TBitmap;
  IWidth, IHeight: Integer;
  IRect, ORect: TRect;
  I: TCBButtonState;
{New}  DestDC: HDC;
begin
  if (State = bsDown) and (NumGlyphs < 3) then State := bsUp;
  Result := FIndexs[State];
  if Result <> -1 then Exit;
{New}  if (FOriginal.Width or FOriginal.Height) = 0 then Exit;
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
    I := State;
    if Ord(I) >= NumGlyphs then I := bsUp;
    ORect := Rect(Ord(I) * IWidth, 0, (Ord(I) + 1) * IWidth, IHeight);
    case State of
      bsUp, bsDown:
        begin
          TmpImage.Canvas.BrushCopy(IRect, FOriginal, ORect, FTransparentColor);
          FIndexs[State] := FGlyphList.Add(TmpImage, nil);
        end;
      bsDisabled:
        begin
          MonoBmp := TBitmap.Create;
          try
            if NumGlyphs > 1 then
            with TmpImage.Canvas do
            begin
              {$IFNDEF Win32}
              BrushCopy(IRect, FOriginal, ORect, FTransparentColor);
              FIndexs[State] := FGlyphList.Add(TmpImage, nil);
              {$ELSE}
               { Change white & gray to clBtnHighlight and clBtnShadow }
              CopyRect(IRect, FOriginal.Canvas, ORect);
              MonoBmp.Width := IWidth;
              MonoBmp.Height := IHeight;
              MonoBmp.Monochrome := True;

              { Convert white to clBtnHighlight }
              FOriginal.Canvas.Brush.Color := clWhite;
              MonoBmp.Canvas.CopyRect(IRect, FOriginal.Canvas, ORect);
              Brush.Color := clBtnHighlight;
              DestDC := Handle;
              SetBkColor(DestDC, clWhite);
              BitBlt(DestDC, 0, 0, IWidth, IHeight,
                     MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);

              { Convert gray to clBtnShadow }
              FOriginal.Canvas.Brush.Color := clGray;
              MonoBmp.Canvas.CopyRect(IRect, FOriginal.Canvas, ORect);
              Brush.Color := clBtnShadow;
              DestDC := Handle;
              SetBkColor(DestDC, clWhite);
              BitBlt(DestDC, 0, 0, IWidth, IHeight,
                     MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);

              { Convert transparent color to clBtnFace }
              FOriginal.Canvas.Brush.Color := ColorToRGB(FTransparentColor);
              MonoBmp.Canvas.CopyRect(IRect, FOriginal.Canvas, ORect);
              Brush.Color := clBtnFace;
              DestDC := Handle;
              SetBkColor(DestDC, clWhite);
              BitBlt(DestDC, 0, 0, IWidth, IHeight,
                     MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
              {$ENDIF}
            end
            else
            begin
              { Create a disabled version }
              with MonoBmp do
              begin
                Assign(FOriginal);
                Canvas.Brush.Color := clBlack;
                Width := IWidth;
                if Monochrome then
                begin
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
                SetBkColor(Handle, clWhite);
                BitBlt(Handle, 1, 1, IWidth, IHeight,
                  MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
                Brush.Color := clBtnShadow;
                SetBkColor(Handle, clWhite);
                BitBlt(Handle, 0, 0, IWidth, IHeight,
                  MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
              end;
            end;
            FIndexs[State] := FGlyphList.Add(TmpImage, nil);
          finally
            MonoBmp.Free;
          end;
       end;
    end; {Case}
  finally
    TmpImage.Free;
  end;
  Result := FIndexs[State];
  FOriginal.Dormant;
end;

procedure TButtonGlyph.DrawButtonGlyph(Canvas: TCanvas; X, Y: Integer;
  State: TCBButtonState);
var
  Index: Integer;
begin
  if FOriginal = nil then Exit;
  if (FOriginal.Width = 0) or (FOriginal.Height = 0) then Exit;
  Index := CreateButtonGlyph(State);
{$IFDEF Win32}
  ImageList_DrawEx(FGlyphList.Handle, Index, Canvas.Handle, X, Y, 0, 0,
    ColorToRGB(clBtnFace), clNone, ILD_Normal);
{$ELSE}
  FGlyphList.Draw(Canvas, X, Y, Index);
{$ENDIF}
end;

procedure TButtonGlyph.CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
   Margin: Integer;  var GlyphPos: TPoint);
var
  ClientSize, GlyphSize : TPoint;

begin
  { calculate the size }
  ClientSize := Point(Client.Right - Client.Left, Client.Bottom -
    Client.Top);

  if FOriginal <> nil then
    GlyphSize := Point(FOriginal.Width div FNumGlyphs, FOriginal.Height) else
    GlyphSize := Point(0, 0);

  { Center the Glyph vertically}
  GlyphPos.Y := (ClientSize.Y - GlyphSize.Y + 1) div 2;

  { adjust Margin }
  if Margin = -1 then
  Margin := (ClientSize.X - GlyphSize.X + 1) div 2;

  GlyphPos.X := Margin;
  { fixup the result variables }
  Inc(GlyphPos.X, Client.Left);
  Inc(GlyphPos.Y, Client.Top);
end;

procedure TButtonGlyph.Draw(Canvas: TCanvas; const Client: TRect;
  Margin: Integer;  State: TCBButtonState);
var
  GlyphPos: TPoint;
begin
  CalcButtonLayout(Canvas, Client, Margin, GlyphPos);
  DrawButtonGlyph(Canvas, GlyphPos.X, GlyphPos.Y, State);
end;


{------------ TCBButton ----------------------------------------}

constructor TCBButton.Create(AOwner: TComponent; aCanvas : TCanvas);
begin
  inherited Create(AOwner);
  fCanvas := aCanvas;
  ControlStyle := [csCaptureMouse, csOpaque, csDoubleClicks];

  {$IFDEF Win32}
  ControlStyle := ControlStyle + [csReplicatable]; {so can copy into CtrlGrid}
  {$ENDIF}

  FGlyph := TButtonGlyph.Create;
  TButtonGlyph(FGlyph).OnChange := GlyphChanged;
  FMargin := -1;
  fButtonCursor := crArrow;
  cursor := crArrow;
  SetBounds (0, 0, GetSystemMetrics(SM_CXVSCROLL), 17);
  TButtonGlyph(FGlyph).Glyph.Handle := LoadBitmap(0, PChar(32738));  {AKA bitmap OBM_COMBO}
end;

destructor TCBButton.Destroy;
begin
  TButtonGlyph(FGlyph).Free;
  fCanvas := nil;
  inherited Destroy;
end;

procedure TCBButton.WMPaint(var Message: TWMPaint);
begin
  if Message.DC <> 0 then
  begin
    Canvas.Handle := Message.DC;
    try
      Paint;
    finally
      Canvas.Handle := 0;
    end;
  end;
end;

procedure TCBButton.Paint;
var
  PaintRect: TRect;
begin
  if not Enabled and not (csDesigning in ComponentState) then
  begin
    FState := bsDisabled;
    FDragging := False;
  end
  else if FState = bsDisabled then FState := bsUp;

  PaintRect := DrawButtonFace(Canvas, Rect(0, 0, Width, Height), 1, bsAutoDetect,
    False, FState in [bsDown], False);

  TButtonGlyph(FGlyph).Draw(Canvas, PaintRect, FMargin, FState);
end;

{new} procedure TCBButton.Loaded;
var
  State: TCBButtonState;
begin
  inherited Loaded;
  if Enabled then
    State := bsUp
  else
    State := bsDisabled;
  TButtonGlyph(FGlyph).CreateButtonGlyph(State);
end;

procedure TCBButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  with TCBXBase(Parent{.Parent}) do
  begin
    if not assigned(FDDLink) or not(fDDLink.Visible) then
    begin
      if (Handle <> GetFocus) and CanFocus then
      begin
        SetFocus;
        if GetFocus <> Handle then Exit;
      end;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and Enabled then
  begin
    FState := bsDown;
    Repaint;
    FDragging := True;
  end;
  with TCBXBase(Parent{.Parent}) do
  begin
    if assigned(fDDLink) then
    begin
      if fDDLink.Visible then
        CloseUp
      else
      begin
        DropDown;
      end;
    end;
  end;
end;

procedure TCBButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewState: TCBButtonState;
begin
  inherited MouseMove(Shift, X, Y);
  if Cursor <> fButtonCursor then
    Cursor := fButtonCursor;
  if FDragging then
  begin
    NewState := bsUp;
    if (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight) then
      NewState := bsDown;
    if NewState <> FState then
    begin
      FState := NewState;
      Repaint;
    end;
  end;
  {Handle the case of drag to an open dropdown window}
  if (ssLeft in Shift) and (GetCapture = Parent.Handle) and
      assigned(TCBXBase(Parent{.Parent}).fDDLink) then
    MouseDragToDropDownWin (Self, TCBXBase(Parent{.Parent}).fDDLink, X, Y);
end;

procedure TCBButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  DoClick: Boolean;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FDragging then
  begin
    FDragging := False;
    DoClick := (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight);
    FState := bsUp;
    Repaint;
    if DoClick then Click;
  end;
end;

procedure TCBButton.Click;
begin
  inherited Click;
end;



procedure TCBButton.SetButtonCursor(Value: TCursor);
begin
  if FButtonCursor <> Value then
    FButtonCursor := Value;
end;

function TCBButton.GetPalette: HPALETTE;
begin
  Result := Glyph.Palette;
end;

function TCBButton.GetGlyph: TBitmap;
begin
  Result := TButtonGlyph(FGlyph).Glyph;
end;

procedure TCBButton.SetGlyph(Value: TBitmap);
begin
  TButtonGlyph(FGlyph).Glyph := Value;
  Invalidate;
end;

function TCBButton.GetNumGlyphs: TNumBtnGlyphs;
begin
  Result := TButtonGlyph(FGlyph).NumGlyphs;
end;

procedure TCBButton.SetNumGlyphs(Value: TNumBtnGlyphs);
begin
  if Value < 0 then Value := 1
  else if Value > 3 then Value := 3;
  if Value <> TButtonGlyph(FGlyph).NumGlyphs then
  begin
    TButtonGlyph(FGlyph).NumGlyphs := Value;
    Invalidate;
  end;
end;

procedure TCBButton.GlyphChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TCBButton.SetMargin(Value: Integer);
begin
  if (Value <> FMargin) and (Value >= -1) then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;

procedure TCBButton.CMEnabledChanged(var Message: TMessage);
{New} const
{New}   NewState: array[Boolean] of TCBButtonState = (bsDisabled, bsUp);
begin
{New}  TButtonGlyph(FGlyph).CreateButtonGlyph(NewState[Enabled]);
  Invalidate;
end;

procedure TCBButton.CMSysColorChange(var Message: TMessage);
begin
  with TButtonGlyph(FGlyph) do
  begin
    Invalidate;
    CreateButtonGlyph(FState);
  end;
end;


end.






