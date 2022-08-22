// Author - Vyacheslav Plotnikov
// Component based on TsComboEdit + ARC

// Modified on 02.10.2014 by Eugene Gorbunov, e-mail: gorbunov@nanoweb.info
// Added published property: TacButtonItem.Enabled - allows to make left/right buttons enabled or disabled.
// Added published property: TacButtonItem.DisabledGlyphKind - works exactly in the same way as in TsSpeedButton.DisabledGlyphKind
// Added published property: TacButtonItem.DisabledKind      - works exactly in the same way as in TsSpeedButton.DisabledKind


unit aceComboEditEx;
{$i sDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, acntUtils, ComCtrls, ToolWin, ExtCtrls, sCustomComboEdit, sSpeedButton,
  {$IFDEF DELPHI7UP} Types, {$ENDIF}
  sGlyphUtils, sGraphUtils, sConst, sCommonData, sSkinProps,
  sAlphaGraph, sVCLUtils, sMessages, sStyleSimply, ImgList;

type
  TacComboEditEx = class;

  TacPickListExItem  = class(TCollectionItem)
  private
    FStoreValue: String;
    FDisplayText: String;
    FIdent: Byte;
    procedure SetDisplayText(const Value: String);
    procedure SetStoreValue(const Value: String);
    procedure SetIdent(const Value: Byte);
  protected
    function GetDisplayName: string; override;
  public
  published
    property DisplayText: String read FDisplayText write SetDisplayText;
    property StoreValue: String read FStoreValue write SetStoreValue;
    property Ident: Byte read FIdent write SetIdent;
  end;

  TacPickListExItemClass = class of TacPickListExItem;
  TacPickListExFindMode = (fmStoreValue, fmDisplayText);

  TacPickListEx = class(TCollection)
  private
    FOwner: TPersistent;
    FCashPickListFindMode: TacPickListExFindMode;
    FCashIndex: Integer;
    FCashFindText: String;
    FCash: Boolean;
    function GetItems(Index: Integer): TacPickListExItem;
    procedure SetItems(Index: Integer; const Value: TacPickListExItem);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor CreateEx(Owner: TPersistent; ColumnClass: TacPickListExItemClass);
    function Find(FindText: String; FindMode: TacPickListExFindMode): Integer;
    property Items[Index: Integer]: TacPickListExItem read GetItems write SetItems; default;
  end;

  PacPickListExItemData = ^TacPickListExItemData;
  TacPickListExItemData = record
    DataClass: TacPickListExItem;
  end;

  TOnCreatePickListData = procedure(Sender: TObject; PickList: TacPickListEx; var Result: Boolean) of object;


  TacEditButtonEx = class(TsSpeedButton)
  private
    FGlyphMode: TsGlyphMode;
    FClickKey: TShortCut;
  public
    FOwner: TacComboEditEx;
    procedure BeginInitGlyph;
    procedure EndInitGlyph;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function IsActive: boolean;
    procedure PaintTo(DC: hdc; R: TPoint);
    function PrepareCache: boolean; override;
    procedure Paint; override;
    function GlyphWidth(Common: boolean = False): integer; override;
    function GlyphHeight: integer; override;
    property ClickKey: TShortCut read FClickKey write FClickKey;
    property GlyphMode: TsGlyphMode read FGlyphMode write FGlyphMode;
  end;


  TacEditButtonItem = class(TCollectionItem)
  private
    FButton: TacEditButtonEx;
    procedure SetName(const Value: String);
    function GetName: String;
    function GetBlend: integer;
    function GetGrayed: boolean;
    function GetHint: string;
    function GetImageIndex: integer;
    function GetImageIndexHot: integer;
    function GetImageIndexPressed: integer;
    procedure SetBlend(const Value: integer);
    procedure SetGrayed(const Value: boolean);
    procedure SetHint(const Value: string);
    procedure SetImageIndex(const Value: integer);
    procedure SetImageIndexHot(const Value: integer);
    procedure SetImageIndexPressed(const Value: integer);
    procedure SetShowHint(const Value: boolean);
    function GetShowHint: boolean;
  protected
    function GetDisplayName: string; override;
    function GetVisible: Boolean; virtual;
    procedure SetVisible(const Value: Boolean); virtual;
    // Added by Acetylator on 02.10.2014 --------------------------------------------
    function GetEnabled: Boolean; virtual;
    procedure SetEnabled(const Value: Boolean); virtual;
    function GetDisabledGlyphKind: TsDisabledGlyphKind; virtual;
    procedure SetDisabledGlyphKind(const Value: TsDisabledGlyphKind); virtual;
    function GetDisabledKind: TsDisabledKind; virtual;
    procedure SetDisabledKind(const Value: TsDisabledKind); virtual;
    // ------------------------------------------------------------------------------
    procedure SetOnClick(const Value: TNotifyEvent); virtual;
    function GetOnClick: TNotifyEvent; virtual;
    function GetOnMouseDown: TMouseEvent; virtual;
    function GetOnMouseUp: TMouseEvent; virtual;
    procedure SetOnMouseDown(const Value: TMouseEvent); virtual;
    procedure SetOnMouseUp(const Value: TMouseEvent); virtual;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    function GetGlyphMode: TsGlyphMode;
    procedure SetGlyphMode(const Value: TsGlyphMode);
    property Button: TacEditButtonEx read FButton write FButton;
  published
    property Blend: integer read GetBlend write SetBlend;
    property Grayed: boolean read GetGrayed write SetGrayed;
    property Hint: string read GetHint write SetHint;
    property ShowHint: boolean read GetShowHint write SetShowHint;
    property ImageIndex: integer read GetImageIndex write SetImageIndex;
    property ImageIndexHot: integer read GetImageIndexHot write SetImageIndexHot;
    property ImageIndexPressed: integer read GetImageIndexPressed write SetImageIndexPressed;
    property Visible: Boolean read GetVisible write SetVisible;
    // Added by Acetylator on 02.10.2014 --------------------------------------------
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property DisabledGlyphKind: TsDisabledGlyphKind read GetDisabledGlyphKind write SetDisabledGlyphKind;
    property DisabledKind: TsDisabledKind read GetDisabledKind write SetDisabledKind;
    // ------------------------------------------------------------------------------
    property Name: String read GetName write SetName;
    property OnClick: TNotifyEvent read GetOnClick write SetOnClick;
    property OnMouseDown: TMouseEvent read GetOnMouseDown write SetOnMouseDown;
    property OnMouseUp: TMouseEvent read GetOnMouseUp write SetOnMouseUp;
  end;


  TacEditButtonItems = class(TCollection)
  private
    FsArcCollectionComboEdit: TacComboEditEx;
    FAlign: TAlign;
    function GetItem(Index: Integer): TacEditButtonItem;
    procedure SetItem(Index: Integer; const Value: TacEditButtonItem);
    function GetCountVisible: Integer;
    procedure SetAlign(const Value: TAlign);
    function GetItemByName(Name: String): TacEditButtonItem;
    procedure SetItemByName(Name: String; const Value: TacEditButtonItem);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(sArcCollectionComboEdit: TacComboEditEx); virtual;
    procedure PaintButtons(DC: HDC);
    procedure Paint;
    procedure Invalidate;
    function Add: TacEditButtonItem;
    function IsActive: boolean;
    property Items[Index: Integer]: TacEditButtonItem read GetItem write SetItem; default;
    property ItemByName[Name: String]: TacEditButtonItem read GetItemByName write SetItemByName;
    property CountVisible: Integer read GetCountVisible;
    property Align: TAlign read FAlign write SetAlign;
  end;


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TacComboEditEx = class(TsCustomComboEdit)
  private
    FTagString: String;
    FPopupText: String;
    FItemIndex: Integer;
    FPickList: TacPickListEx;
    FOnCreatePickListData: TOnCreatePickListData;
    FOnSelectItem: TNotifyEvent;

    FMinKeyCount: Integer;
    FItemValue: String;
    FValues: TStringList;
    FImages: TCustomImageList;
    FRightButtons: TacEditButtonItems;
    FLeftButtons: TacEditButtonItems;
    FTextComments: String;

    procedure SetItemIndex(const Value: Integer);
    procedure SetItemValue(const aValue: String);
    procedure SetMinKeyCount(const Value: Integer);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetLeftButtons(const Value: TacEditButtonItems);
    procedure SetRightButtons(const Value: TacEditButtonItems);
    function GetLeftButtonWidth: Integer;
    function GetRightButtonWidth: Integer;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFocuseChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure SetTextComments(const Value: String);
  protected
    procedure PopupWindowClose; override;
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoResultClick(Sender: TObject);
    procedure PaintText; override;
    procedure StdPaintHandler(DC: hdc);
    procedure StdPaintText(DC: hdc);
    procedure PaintBorder(DC: hdc); override;
    procedure OurPaintHandler(DC: hdc);   override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    procedure SetEditRect;
    constructor Create(AOwner:TComponent); override;
    procedure CreateWnd; override;
    destructor Destroy; override;
    procedure Invalidate; override;
    property ItemIndex: Integer Read FItemIndex Write SetItemIndex;
    property ItemValue: String read FItemValue write SetItemValue;
    procedure Loaded; override;
    procedure SelectAll; override;
    function SetItemValueByDisplayText(const aDisplayText: String): Integer;
    procedure WndProc(var Message: TMessage); override;
  published
    property RightButtons: TacEditButtonItems read FRightButtons write SetRightButtons;
    property Images: TCustomImageList read FImages write SetImages;
    property LeftButtons: TacEditButtonItems read FLeftButtons write SetLeftButtons;
    property MinKeyCount: Integer read FMinKeyCount write SetMinKeyCount;
    property PickList: TacPickListEx read FPickList write FPickList;
    property TagString: String read FTagString write FTagString;
    property TextComments: String read FTextComments write SetTextComments;

    property OnCreatePickListData: TOnCreatePickListData read FOnCreatePickListData write FOnCreatePickListData;
    property OnSelectItem: TNotifyEvent read FOnSelectItem write FOnSelectItem;

    property Alignment;
    property CharCase;
    property ClickKey;
    property DirectInput;
    property EditMask;
    property Enabled;
    property Font;
    property HideSelection;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnButtonClick;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnContextPopup;
    property OnEndDock;
    property OnStartDock;

    property AutoSelect;
    property DragCursor;
    property HelpContext;
    property PasswordChar;
    property Hint;

    property OnEnter;
    property OnExit;
  end;


implementation

uses sMaskEdit, Mask;


function BtnOffset(ComboEdit: TsCustomComboEdit): integer;
begin
  Result := integer(ComboEdit.BorderStyle <> bsNone) * (2 * integer(ComboEdit.Ctl3d));
end;


function TacPickListExItem.GetDisplayName: string;
begin
  Result := FDisplayText;
  if Result = '' then
    Result := inherited GetDisplayName;
end;


procedure TacPickListExItem.SetDisplayText(const Value: String);
begin
  FDisplayText := Value;
end;


procedure TacPickListExItem.SetIdent(const Value: Byte);
begin
  FIdent := Value;
end;


procedure TacPickListExItem.SetStoreValue(const Value: String);
begin
  FStoreValue := Value;
end;


constructor TacPickListEx.CreateEx(Owner: TPersistent; ColumnClass: TacPickListExItemClass);
begin
  inherited Create(ColumnClass);
  FOwner := Owner;
end;


function TacPickListEx.Find(FindText: String; FindMode: TacPickListExFindMode): Integer;
var
  i: integer;
begin
  Result := -1;
  if FCash and (FindText = FCashFindText) and (FCashPickListFindMode = FindMode) then
    Result := FCashIndex
  else for i := 0 to Count-1 do
    if ((FindMode = fmStoreValue) and (Items[i].StoreValue = FindText)) or
         ((FindMode = fmDisplayText) and (Items[i].DisplayText = FindText)) then begin
      Result := i;
      FCashPickListFindMode := FindMode;
      FCashIndex := i;
      FCashFindText := FindText;
      FCash := True;
      Break;
    end;
end;


function TacPickListEx.GetItems(Index: Integer): TacPickListExItem;
begin
  Result := TacPickListExItem(inherited Items[Index]);
end;


function TacPickListEx.GetOwner: TPersistent;
begin
  Result := FOwner;
end;


procedure TacPickListEx.SetItems(Index: Integer; const Value: TacPickListExItem);
begin
  Items[Index].Assign(Value);
end;


procedure TacComboEditEx.Change;
begin
  if not DroppedDown then begin
    FPopupText := '';
    inherited Change;
  end
  else begin
    FPopupText := Text;
    PopupWindowShow;
  end;
end;


constructor TacComboEditEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTextComments := '';
  FRightButtons := TacEditButtonItems.Create(Self);
  FRightButtons.Align := alRight;
  FLeftButtons := TacEditButtonItems.Create(Self);
  FLeftButtons.Align := alLeft;

  FPickList := TacPickListEx.CreateEx(Self,TacPickListExItem);
  FItemIndex := -1;
  FMinKeyCount := 1;
  FItemValue := '';
  FValues := TStringList.Create;
  SkinData.COC := COC_TsComboEdit;
  FDefBmpID := iBTN_ELLIPSIS;
end;


destructor TacComboEditEx.Destroy;
begin
  if Assigned(FValues) then FreeAndNil(FValues);
  if Assigned(FPopupwindow) then FreeAndNil(FPopupWindow);
  if Assigned(FPickList) then FreeAndNil(FPickList);
  if Assigned(FRightButtons) then FreeAndNil(FRightButtons);
  if Assigned(FLeftButtons) then FreeAndNil(FLeftButtons);
  inherited;
end;


procedure TacComboEditEx.DoResultClick(Sender: TObject);
begin
  if HandleAllocated then
    SetFocus;
end;


procedure TacComboEditEx.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key in [VK_PRIOR, VK_NEXT, VK_UP,  VK_DOWN, VK_ADD, VK_SUBTRACT, VK_RETURN]) and DroppedDown then
    Key := 0
  else
    if (key = VK_ESCAPE) and DroppedDown then
      PopupWindowClose;

  inherited;
  SetEditRect;
end;


procedure TacComboEditEx.KeyPress(var Key: Char);
begin
  inherited;
  if CharInSet(Key, [#32..#255]) then begin
    if (Length(Trim(Text)) < MinKeyCount-1) and not DroppedDown then
      Exit;

    if not DroppedDown then
      PopupWindowShow;
  end;
end;


procedure TacComboEditEx.PopupWindowClose;
begin
  inherited;
end;


procedure TacComboEditEx.SetItemIndex(const Value: Integer);
begin
  FItemIndex := Value;
  if FItemIndex = -1 then begin
    Text := '';
    FItemValue := '';
  end
end;


procedure TacComboEditEx.SetMinKeyCount(const Value: Integer);
begin
  FMinKeyCount := Value;
end;


procedure TacComboEditEx.SetItemValue(const aValue: String);
var
  i: integer;
  aCreateResult: Boolean;
begin
  Text := '';
  FItemValue := '';
  FItemIndex := -1;

  if aValue <> '' then begin
    if Assigned(FOnCreatePickListData) then
      FOnCreatePickListData(Self, FPickList,aCreateResult);

    i := PickList.Find(aValue,fmStoreValue);
    if i <> -1 then begin
      Text := PickList.Items[i].DisplayText;
      FItemValue := aValue;
      FItemIndex := i;
    end;
  end;
end;


function TacComboEditEx.SetItemValueByDisplayText(const aDisplayText: String): Integer;
var
  i: integer;
  aCreateResult: Boolean;
begin
  Result := -1;
  if aDisplayText <> '' then begin
    if Assigned(FOnCreatePickListData) then
      FOnCreatePickListData(Self, FPickList,aCreateResult);

    i := PickList.Find(aDisplayText,fmDisplayText);
    if i <> -1 then begin
      Text := PickList.Items[i].DisplayText;
      FItemValue := PickList.Items[i].StoreValue;
      FItemIndex := i;
      Result := i;
    end;
  end;
end;


procedure TacComboEditEx.SetImages(const Value: TCustomImageList);

  procedure SetButtonImages(Buttons: TacEditButtonItems);
  var
    n: Integer;
  begin
    for n := 0 to Buttons.Count - 1 do
      Buttons[n].Button.GlyphMode.Images := Value;
  end;

begin
  if FImages <> Value then begin
    FImages := Value;
    SetButtonImages(FRightButtons);
    SetButtonImages(FLeftButtons);
    Invalidate;
  end;
end;


procedure TacComboEditEx.SetEditRect;
var
  Loc: TRect;
begin
  if Parent = nil then
    Exit;
{$WARNINGS OFF}
  SendMessage(Handle, EM_GETRECT, 0, LPARAM(@Loc));
  Loc.Bottom := ClientHeight;
  Loc.Right := ClientWidth - GetRightButtonWidth - 2;
  Loc.Top := 0;
  if (BorderStyle <> bsNone) then
    Loc.Left := GetLeftButtonWidth + 2 * integer(not Ctl3d)
  else
    Loc.Left := GetLeftButtonWidth + 2;

  SendMessage(Handle, EM_SETRECTNP, 0, LPARAM(@Loc));
  SendMessage(Handle, EM_GETRECT, 0, LPARAM(@Loc));  {debug}
{$WARNINGS ON}
end;


procedure TacComboEditEx.SetLeftButtons(const Value: TacEditButtonItems);
begin
  FLeftButtons.Assign(Value);
end;


procedure TacComboEditEx.SetRightButtons(const Value: TacEditButtonItems);
begin
  FRightButtons.Assign(Value);
end;


procedure TacComboEditEx.CreateWnd;
begin
  inherited;
  SetEditRect;
end;


procedure TacComboEditEx.Invalidate;
begin
  inherited;
  FLeftButtons.Invalidate;
  FRightButtons.Invalidate;
end;


procedure TacComboEditEx.Loaded;
begin
  inherited;
  SetEditRect;
end;


procedure TacComboEditEx.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  SetEditRect;
end;

function TacComboEditEx.GetLeftButtonWidth: Integer;
begin
  if Images <> nil then
    Result := FLeftButtons.CountVisible * Images.Width
  else
    Result := 0;
end;


function TacComboEditEx.GetRightButtonWidth: Integer;
begin
  if Images <> nil then
    Result := FRightButtons.CountVisible * Images.Width
  else
    Result := 0;
end;


procedure TacComboEditEx.PaintText;
var
  R: TRect;
  aText: acString;
  bw: integer;
  OldColor: TColor;
begin
  SkinData.FCacheBMP.Canvas.Font.Assign(Font);
  bw := BorderWidth;
  if not Focused and (EditText = '') and (TextComments <> '') then begin
    aText := TextComments;
    OldColor := SkinData.FCacheBMP.Canvas.Font.Color;
    SkinData.FCacheBMP.Canvas.Font.Color := clGray;
    SkinData.CustomFont := True;
  end
  else begin
    aText := EditText;
    OldColor := Font.Color;
  end;
  if SkinData.Skinned then begin
    R := Rect(bw - 1, bw - 1, Width - bw + 1, Height - bw + 1);
    SkinData.FCacheBMP.Canvas.Brush.Color := Color;
    SkinData.FCacheBMP.Canvas.FillRect(R);
  end;
  R := Rect(bw + GetLeftButtonWidth, bw, Width - bw - GetRightButtonWidth - GlyphWidth, Height - bw);
  if PasswordChar = #0 then
{$WARNINGS OFF}
    acWriteTextEx(SkinData.FCacheBMP.Canvas, PacChar(aText), Enabled or SkinData.Skinned, R, DT_TOP or GetStringFlags(Self, Alignment) or DT_NOPREFIX, SkinData, ControlIsActive(SkinData))
{$WARNINGS ON}
  else begin
    acFillString(aText, Length(aText), acChar(PasswordChar));
{$WARNINGS OFF}
    acWriteTextEx(SkinData.FCacheBMP.Canvas, PacChar(aText), Enabled or SkinData.Skinned, R, DT_TOP or GetStringFlags(Self, Alignment) or DT_NOPREFIX,
              SkinData, ControlIsActive(SkinData));
{$WARNINGS ON}
  end;
  SkinData.FCacheBMP.Canvas.Font.Color := OldColor;
  if SkinData.Skinned then begin
    FLeftButtons.PaintButtons(SkinData.FCacheBMP.Canvas.Handle);
    FRightButtons.PaintButtons(SkinData.FCacheBMP.Canvas.Handle);
  end
end;


procedure TacComboEditEx.OurPaintHandler(DC: hdc);
var
  NewDC, SavedDC: hdc;
  PS: TPaintStruct;
begin
  if not InAnimationProcess then
    BeginPaint(Handle, PS);

  if DC = 0 then
    NewDC := GetWindowDC(Handle)
  else
    NewDC := DC;

  SavedDC := SaveDC(NewDC);
  try
    if SkinData.Skinned then begin
      SkinData.Updating := SkinData.Updating;
      if not SkinData.Updating then begin
        SkinData.BGChanged := SkinData.BGChanged or SkinData.HalfVisible or GetBoolMsg(Parent, AC_GETHALFVISIBLE);
        SkinData.HalfVisible := not RectInRect(Parent.ClientRect, BoundsRect);
        if SkinData.BGChanged then
          PrepareCache;

        UpdateCorners(SkinData, 0);
        BitBlt(NewDC, 0, 0, Width, Height, SkinData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);

        FRightButtons.PaintButtons(NewDC);
        FLeftButtons.PaintButtons(NewDC);
{$IFDEF DYNAMICCACHE}
        if Assigned(SkinData.FCacheBmp) then
          FreeAndNil(SkinData.FCacheBmp);
{$ENDIF}
      end;
    end
    else begin
      PrepareCache;
      BitBlt(NewDC, 3, 3, Width - 6, Height - 6, SkinData.FCacheBmp.Canvas.Handle, 3, 3, SRCCOPY);

      FRightButtons.Paint;
      FLeftButtons.Paint;
    end;
  finally
    RestoreDC(NewDC, SavedDC);
    if DC = 0 then
      ReleaseDC(Handle, NewDC);

    if not InAnimationProcess then
      EndPaint(Handle, PS);
  end;
end;


procedure TacComboEditEx.WndProc(var Message: TMessage);
var
  DC: hdc;
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  if Message.MSG = SM_ALPHACMD then
    case Message.WParamHi of
      AC_GETBG : begin
        InitBGInfo(SkinData, PacBGInfo(Message.LParam), 0);
        PacBGInfo(Message.LParam)^.Offset.X := BorderWidth;
        PacBGInfo(Message.LParam)^.Offset.Y := PacBGInfo(Message.LParam)^.Offset.X;
        Exit;
      end;
    end
  else
    case Message.Msg of
      WM_ERASEBKGND:
        if SkinData.Skinned and IsWindowVisible(Handle) then begin
          SkinData.Updating := SkinData.Updating;
          if SkinData.Updating then
            Exit;
        end;

      WM_PRINT:
        if SkinData.Skinned then begin
          SkinData.Updating := False;
          DC := TWMPaint(Message).DC;
          if SkinData.BGChanged then
            PrepareCache;

          UpdateCorners(SkinData, 0);

          OurPaintHandler(DC);
          BitBltBorder(DC, 0, 0, SkinData.FCacheBmp.Width, SkinData.FCacheBmp.Height, SkinData.FCacheBmp.Canvas.Handle, 0, 0, BorderWidth);

          FRightButtons.PaintButtons(DC);
          FLeftButtons.PaintButtons(DC);
          Exit;
        end;

      WM_PAINT:
        if not SkinData.Skinned and not Focused then begin
          StdPaintHandler(TWMPaint(Message).DC);
          Exit;
        end;
    end;

  inherited;
  if Message.MSG = SM_ALPHACMD then
    case Message.WParamHi of
      AC_REMOVESKIN, AC_SETNEWSKIN, AC_REFRESH:
        if LongWord(Message.LParam) = LongWord(SkinData.SkinManager) then begin
          AlphaBroadcast(Self, Message);
          SetEditRect;
        end;
        
      AC_PREPARING: begin
        Message.Result := 0;
        Exit;
      end;
    end
  else
    case Message.Msg of
      WM_SETFOCUS: begin
        if AutoSelect then
          SelectAll;
        Invalidate;
      end;

      WM_SIZE, CM_FONTCHANGED:
        SetEditRect;

      CM_EXIT:
        Repaint;
    end;
end;


procedure TacComboEditEx.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Images) then
    Images := nil;
end;


procedure TacComboEditEx.PaintBorder(DC: hdc);
const
  BordWidth = 2;
var
  NewDC, SavedDC: HDC;
begin
  if not Assigned(Parent) or not Visible or not Parent.Visible or (csCreating in ControlState) or SkinData.Updating then
    Exit;

  if DC = 0 then
    NewDC := GetWindowDC(Handle)
  else
    NewDC := DC;

  SavedDC := SaveDC(NewDC);
  try
    if SkinData.BGChanged and ControlIsActive(SkinData) then begin
      PrepareCache;
      Invalidate;
    end;
    UpdateCorners(SkinData, 0);
    BitBltBorder(NewDC, 0, 0, Width, Height, SkinData.FCacheBmp.Canvas.Handle, 0, 0, BordWidth);

    if ControlIsActive(SkinData) then begin
      FRightButtons.PaintButtons(NewDC);
      FLeftButtons.PaintButtons(NewDC);
    end;
{$IFDEF DYNAMICCACHE}
    if Assigned(SkinData.FCacheBmp) then
      FreeAndNil(SkinData.FCacheBmp);
{$ENDIF}
  finally
    RestoreDC(NewDC, SavedDC);
    if DC = 0 then
      ReleaseDC(Handle, NewDC);
  end;
end;


procedure TacComboEditEx.CMColorChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;


procedure TacComboEditEx.CreateParams(var Params: TCreateParams);
const
  Alignments: array[TAlignment] of Longword = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_MULTILINE {or Alignments[FAlignment]} or WS_CLIPCHILDREN;
end;


procedure TacComboEditEx.CMFocuseChanged(var Message: TCMFocusChanged);
begin
  inherited;
  FRightButtons.Invalidate;
end;


procedure TacComboEditEx.SelectAll;
begin
  FRightButtons.Invalidate;
  SetEditRect;
  if (Text <> '') then
    SendMessage(Handle, EM_SETSEL, 0, -1);
end;


procedure TacComboEditEx.SetTextComments(const Value: String);
begin
  FTextComments := Value;
  Invalidate;
end;


procedure TacComboEditEx.StdPaintHandler(DC: hdc);
var
  NewDC, SavedDC: hdc;
  PS: TPaintStruct;
  i, bw: integer;
begin
  BeginPaint(Handle, PS);
  if DC = 0 then
    NewDC := GetWindowDC(Handle)
  else
    NewDC := DC;

  SavedDC := SaveDC(NewDC);
  try
    if ControlCount <> 0 then begin
      bw := BorderWidth - 1;
      for i := 0 to ControlCount - 1 do
        ExcludeClipRect(NewDC, Controls[i].Left + bw, Controls[i].Top + bw, Controls[i].BoundsRect.Right + bw, Controls[i].BoundsRect.Bottom + bw);
    end;
    if BorderStyle <> bsNone then
      i := 1 + integer(Ctl3d)
    else
      i := 0;

    InterSectClipRect(NewDC, i, i, Width - i * 2, Height - i * 2);
    FillDC(NewDC, Rect(0, 0, Width - integer(ShowButton) * GlyphWidth, Height), ColorToRGB(Color));
    StdPaintText(NewDC);;
    RestoreDC(NewDC, SavedDC);
    if ControlCount > 0 then
      sVCLUtils.PaintControls(NewDC, Self, False, Point(i , i));
  finally
    if DC = 0 then
      ReleaseDC(Handle, NewDC);

    EndPaint(Handle, PS);
  end;
end;


procedure TacComboEditEx.StdPaintText;
var
  R: TRect;
  bw: integer;
  aText: acString;
begin
  SelectObject(DC, Font.Handle);
  bw := BorderWidth;
  aText := EditText;
  R := Rect(GetLeftButtonWidth + bw, bw, Width - bw - GetRightButtonWidth - integer(ShowButton) * GlyphWidth, Height - bw);
  if Text <> '' then begin
    if PasswordChar <> #0 then
      acFillString(aText, Length(aText), acChar(PasswordChar));
      
    acDrawText(DC, aText, R, DT_TOP or GetStringFlags(Self, Alignment) or DT_NOPREFIX);
  end
{$IFDEF D2009}
  else
    if (TextHint <> '') then begin
      SetTextColor(DC, BlendColors(Font.Color, Color, 166));
      acDrawText(DC, TextHint, R, DT_TOP or GetStringFlags(Self, Alignment) and not DT_VCENTER);
    end;
{$ENDIF}
end;


procedure TacEditButtonItem.Assign(Source: TPersistent);
begin
  if Source is TacEditButtonItem then begin
    FButton.Assign(TacEditButtonItem(Source).Button);
    GetGlyphMode.ImageIndex := TacEditButtonItem(Source).GetGlyphMode.ImageIndex;
    GetGlyphMode.ImageIndexHot := TacEditButtonItem(Source).GetGlyphMode.ImageIndexHot;
    GetGlyphMode.ImageIndexPressed := TacEditButtonItem(Source).GetGlyphMode.ImageIndexPressed;
    GetGlyphMode.Blend := TacEditButtonItem(Source).GetGlyphMode.Blend;
    GetGlyphMode.Grayed := TacEditButtonItem(Source).GetGlyphMode.Grayed;
    GetGlyphMode.Hint := TacEditButtonItem(Source).GetGlyphMode.Hint;
    Visible  := TacEditButtonItem(Source).Visible;
    Name  := TacEditButtonItem(Source).Name;
    OnClick  := TacEditButtonItem(Source).OnClick;
    OnMouseDown  := TacEditButtonItem(Source).OnMouseDown;
    OnMouseUp  := TacEditButtonItem(Source).OnMouseUp;
  end
  else
    inherited Assign(Source);
end;


procedure TacEditButtonItem.AssignTo(Dest: TPersistent);
begin
  if Dest is TacEditButtonItem then
    Dest.Assign(Self)
  else
    inherited Assign(Dest);
end;

constructor TacEditButtonItem.Create(Collection: TCollection);
var
  Control: TacComboEditEx;
begin
  inherited;
  with TacEditButtonItems(Collection) do begin
    FButton := TacEditButtonEx.Create(FsArcCollectionComboEdit);
    FButton.Parent := TWinControl(FsArcCollectionComboEdit);
    Control := TacComboEditEx(TacEditButtonItems(Collection).GetOwner);
    FButton.GlyphMode.Images := Control.Images;
    if Control.Images <> nil then
      FButton.Width := Control.Images.Width
    else
      FButton.Width := 0;

    FButton.Left := 1000; // Read just alignment
    FButton.Align := Align;
  end;
end;


destructor TacEditButtonItem.Destroy;
begin
  if Assigned(FButton) and (FButton <> nil) then
    FButton.Free;

  inherited;
end;


function TacEditButtonItem.GetBlend: integer;
begin
  Result := GetGlyphMode.Blend;
end;


function TacEditButtonItem.GetDisplayName: string;
begin
  if FButton.Name <> '' then
    Result := FButton.Name
  else
    Result := 'sEditButtonItem';
end;


function TacEditButtonItem.GetGlyphMode: TsGlyphMode;
begin
  Result := FButton.FGlyphMode;
end;


function TacEditButtonItem.GetGrayed: boolean;
begin
  Result := GetGlyphMode.Grayed;
end;


function TacEditButtonItem.GetHint: string;
begin
  Result := FButton.Hint;
end;


function TacEditButtonItem.GetImageIndex: integer;
begin
  Result := GetGlyphMode.ImageIndex;
end;


function TacEditButtonItem.GetImageIndexHot: integer;
begin
  Result := GetGlyphMode.ImageIndexHot;
end;


function TacEditButtonItem.GetImageIndexPressed: integer;
begin
  Result := GetGlyphMode.ImageIndexPressed;
end;


function TacEditButtonItem.GetName: String;
begin
  Result := FButton.Name;
end;


function TacEditButtonItem.GetOnClick: TNotifyEvent;
begin
  Result := FButton.OnClick;
end;


function TacEditButtonItem.GetOnMouseDown: TMouseEvent;
begin
  Result := FButton.OnMouseDown;
end;


function TacEditButtonItem.GetOnMouseUp: TMouseEvent;
begin
  Result := FButton.OnMouseUp;
end;


function TacEditButtonItem.GetShowHint: boolean;
begin
  Result := FButton.ShowHint;
end;


function TacEditButtonItem.GetVisible: Boolean;
begin
  Result := FButton.Visible;
end;

// ------------------------------------------------------------------------------
// Added by Acetylator on 02.10.2014
// ------------------------------------------------------------------------------

procedure TacEditButtonItem.SetEnabled(const Value: Boolean);
begin
  if FButton.Enabled <> Value then begin
    FButton.Enabled := Value;
    Changed(True);
  end;
end;


function TacEditButtonItem.GetEnabled: Boolean;
begin
  Result := FButton.Enabled;
end;


function TacEditButtonItem.GetDisabledGlyphKind: TsDisabledGlyphKind;
begin
  Result := FButton.DisabledGlyphKind;
end;


procedure TacEditButtonItem.SetDisabledGlyphKind(const Value: TsDisabledGlyphKind);
begin
  if FButton.DisabledGlyphKind <> Value then begin
    FButton.DisabledGlyphKind := Value;
    Changed(True);
  end;
end;


function TacEditButtonItem.GetDisabledKind: TsDisabledKind;
begin
  Result := FButton.DisabledKind;
end;

procedure TacEditButtonItem.SetDisabledKind(const Value: TsDisabledKind);
begin
  if FButton.DisabledKind <> Value then begin
    FButton.DisabledKind := Value;
    Changed(True);
  end;
end;

// ------------------------------------------------------------------------------

procedure TacEditButtonItem.SetVisible(const Value: Boolean);
begin
  if FButton.Visible <> Value then begin
    FButton.Visible := Value;
    Changed(True);
  end;
end;


procedure TacEditButtonItem.SetBlend(const Value: integer);
begin
  GetGlyphMode.Blend := Value;
end;


procedure TacEditButtonItem.SetGlyphMode(const Value: TsGlyphMode);
begin
  FButton.FGlyphMode := Value;
end;


procedure TacEditButtonItem.SetGrayed(const Value: boolean);
begin
  GetGlyphMode.Grayed := Value;
end;


procedure TacEditButtonItem.SetHint(const Value: string);
begin
  FButton.Hint := Value;
end;


procedure TacEditButtonItem.SetImageIndex(const Value: integer);
begin
  GetGlyphMode.ImageIndex := Value;
end;


procedure TacEditButtonItem.SetImageIndexHot(const Value: integer);
begin
  GetGlyphMode.ImageIndexHot := Value;
end;


procedure TacEditButtonItem.SetImageIndexPressed(const Value: integer);
begin
  GetGlyphMode.ImageIndexPressed := Value;
end;


procedure TacEditButtonItem.SetName(const Value: String);
begin
  FButton.Name := Value;
  DisplayName := Value;
end;


procedure TacEditButtonItem.SetOnClick(const Value: TNotifyEvent);
begin
  FButton.OnClick := Value;
end;


procedure TacEditButtonItem.SetOnMouseDown(const Value: TMouseEvent);
begin
  FButton.OnMouseDown := Value
end;


procedure TacEditButtonItem.SetOnMouseUp(const Value: TMouseEvent);
begin
  FButton.OnMouseUp := Value
end;


procedure TacEditButtonItem.SetShowHint(const Value: boolean);
begin
  FButton.ShowHint := Value;
end;


function TacEditButtonItems.Add: TacEditButtonItem;
begin
  Result := TacEditButtonItem(inherited Add);
end;


constructor TacEditButtonItems.Create(sArcCollectionComboEdit: TacComboEditEx);
begin
  inherited Create(TacEditButtonItem);
  FsArcCollectionComboEdit := sArcCollectionComboEdit;
end;


function TacEditButtonItems.GetCountVisible: Integer;
var
  n: Integer;
begin
  Result := 0;
  for n := 0 to Count - 1 do
    with Items[n].FButton do
      if Visible then
        Inc(Result);
end;


function TacEditButtonItems.GetItem(Index: Integer): TacEditButtonItem;
begin
  Result := TacEditButtonItem(inherited GetItem(Index))
end;


function TacEditButtonItems.GetItemByName(Name: String): TacEditButtonItem;
var
  n: Integer;
begin
  Result := nil;
  for n := 0 to Count - 1 do
    if Items[n].Name = Name then begin
      Result := Items[n];
      Break;
    end;
end;


function TacEditButtonItems.GetOwner: TPersistent;
begin
  Result := FsArcCollectionComboEdit;
end;


procedure TacEditButtonItems.Invalidate;
var
  n: Integer;
begin
  for n := 0 to Count - 1 do
    with Items[n].FButton do
      if Items[n].FButton <> nil then begin
        Width := FGlyphMode.Width;
        if Visible then
          if not RestrictDrawing
            then SkinData.BGChanged := True;
        Invalidate;
      end;
end;


function TacEditButtonItems.IsActive: boolean;
var
  n: Integer;
begin
  Result := False;
  for n  := 0 to Count - 1 do begin
    Result := Items[n].FButton.IsActive;
    if Result then
      Break;
  end;
end;


procedure TacEditButtonItems.Paint;
var
  n: Integer;
begin
  for n := 0 to Count - 1 do
    with Items[n].FButton do
      if Visible then
        Paint;
end;


procedure TacEditButtonItems.PaintButtons(DC: HDC);
var
  n: Integer;
begin
  for n := 0 to Count - 1 do
    with Items[n].FButton do
      if Visible then
        PaintTo(DC, Point(Left + 2, Top + 2));
end;


procedure TacEditButtonItems.SetAlign(const Value: TAlign);
begin
  FAlign := Value;
end;


procedure TacEditButtonItems.SetItem(Index: Integer; const Value: TacEditButtonItem);
begin
  inherited SetItem(Index, Value);
end;


procedure TacEditButtonItems.SetItemByName(Name: String; const Value: TacEditButtonItem);
var
  n: Integer;
  found: boolean;
begin
  found := False;
  for n := 0 to Count - 1 do
    if Items[n].Name = Name then begin
      Items[n] := Value;
      Break;
      found := True;
    end;

  if not found then
    raise Exception.Create('item with name: ' + Name + ' not found');
end;


procedure TacEditButtonItems.Update(Item: TCollectionItem);
var
  n: Integer;
begin
  inherited Update(Item);
  for n := 0 to Count - 1 do
    if Items[n].FButton <> nil then
      Items[n].FButton.Left := 1000;

  FsArcCollectionComboEdit.Invalidate;
end;


procedure TacEditButtonEx.Assign(Source: TPersistent);
begin
  if Source is TacEditButtonEx then begin
    FOwner := TacEditButtonEx(Source).FOwner;
    FGlyphMode.Images := TacEditButtonEx(Source).GlyphMode.Images;
    FGlyphMode.ImageIndex := TacEditButtonEx(Source).GlyphMode.ImageIndex;
    FGlyphMode.ImageIndexHot := TacEditButtonEx(Source).GlyphMode.ImageIndexHot;
    FGlyphMode.ImageIndexPressed := TacEditButtonEx(Source).GlyphMode.ImageIndexPressed;
    FGlyphMode.Blend := TacEditButtonEx(Source).GlyphMode.Blend;
    FGlyphMode.Grayed := TacEditButtonEx(Source).GlyphMode.Grayed;
    FGlyphMode.Hint := TacEditButtonEx(Source).GlyphMode.Hint;
    FClickKey := TacEditButtonEx(Source).ClickKey;
    OnClick := TacEditButtonEx(Source).OnClick;
  end
  else
    inherited Assign(Source);
end;


procedure TacEditButtonEx.BeginInitGlyph;
begin
  if (csLoading in ComponentState) or (csCreating in ControlState) then
    Exit;

  SkinData.CtrlSkinState := SkinData.CtrlSkinState or ACS_LOCKED;
  if Assigned(FOwner.Images) and Assigned(GlyphMode.Images) and (GlyphMode.ImageIndex > -1) and (GlyphMode.ImageIndex < GlyphMode.Images.Count) then begin
    Blend := GlyphMode.Blend;
    Grayed := GlyphMode.Grayed;
    Images := GlyphMode.Images;
    case CurrentState of
      0: ImageIndex := GlyphMode.ImageIndex;
      1: ImageIndex := GlyphMode.ImageIndexHot;
      2: ImageIndex := GlyphMode.ImageIndexPressed;
    end;
    if (ImageIndex < 0) or (ImageIndex > GlyphMode.Images.Count - 1) then
      ImageIndex := GlyphMode.ImageIndex;
  end;
  SkinData.CtrlSkinState := SkinData.CtrlSkinState and not ACS_LOCKED;
end;


constructor TacEditButtonEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOwner := TacComboEditEx(AOwner);
  SkinData.SkinSection := s_SPEEDBUTTON_SMALL;
  FGlyphMode := TsGlyphMode.Create(TacComboEditEx(AOwner));
  DisabledGlyphKind := [dgBlended];
{$IFDEF CHECKXP}
  Flat := True;
{$ENDIF}
  Cursor := crArrow;
  Align := alRight; // Button is aligned by right side
  Width := 22;
  ShowCaption := False;
  AnimatEvents := [aeMouseEnter, aeMouseLeave];
  Parent :=  TacComboEditEx(AOwner);
end;


destructor TacEditButtonEx.Destroy;
begin
  FGlyphMode.Free;
  inherited;
end;


procedure TacEditButtonEx.EndInitGlyph;
begin
  if (csLoading in ComponentState) or (csCreating in ControlState) then
    Exit;

  SkinData.CtrlSkinState := SkinData.CtrlSkinState or ACS_LOCKED;
  Images := nil;
  SkinData.CtrlSkinState := SkinData.CtrlSkinState and not ACS_LOCKED;
end;


function TacEditButtonEx.GlyphHeight: integer;
begin
  Result := GlyphMode.Height;
end;


function TacEditButtonEx.GlyphWidth(Common: boolean = False): integer;
begin
  Result := GlyphMode.Width;
end;


function TacEditButtonEx.IsActive: boolean;
begin
  Result := SkinData.FMouseAbove or ControlIsActive(SkinData);
end;


procedure TacEditButtonEx.Paint;
begin
  if not FOwner.SkinData.Skinned then begin
    BeginInitGlyph;
    StdPaint(False);
    EndInitGlyph;
  end
  else
    if not (InAnimationProcess and (Canvas.Handle <> SkinData.PrintDC)) then
      inherited; // Animation under Vista fix
end;


procedure TacEditButtonEx.PaintTo(DC: hdc; R: TPoint);
begin
  if not FOwner.SkinData.Skinned then
    Paint
  else begin
    PrepareCache;
    BitBlt(DC, R.x, R.y, SkinData.FCacheBmp.Width, SkinData.FCacheBmp.Height, SkinData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
  end;
end;


function TacEditButtonEx.PrepareCache: boolean;
var
  CI: TCacheInfo;
begin
  BeginInitGlyph;
  InitCacheBmp(SkinData);
  BitBlt(SkinData.FCacheBmp.Canvas.Handle, 0, 0, Width, Height, FOwner.SkinData.FCacheBmp.Canvas.Handle, Left + BtnOffset(FOwner), Top + BtnOffset(FOwner), SRCCOPY);
  DrawGlyph;
  SkinData.BGChanged := False;
  EndInitGlyph;
  if not Enabled or FOwner.ReadOnly then begin
    CI := MakeCacheInfo(FOwner.SkinData.FCacheBmp, 2, 2);
    BmpDisabledKind(SkinData.FCacheBmp, DisabledKind, Parent, CI, Point(Left, Top));
  end;
  Result := True;
end;  

end.
