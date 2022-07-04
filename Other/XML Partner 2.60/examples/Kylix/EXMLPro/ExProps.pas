
(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower XMLPartner
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* XMLPartner: ExProps.PAS 2.57                          *}
{*********************************************************}
{* XMLPartner: Properties component for XMLEditor        *}
{*********************************************************}

{$I XpDefine.inc}

unit ExProps;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
  Messages,
{$ENDIF}
{$IFDEF LINUX}
  Types,
  ExUtil,
  Qt,
{$ENDIF}
{$IFDEF UsingCLX}
  QMask,
  QControls,
  QGraphics,
  QStdctrls,
  QComctrls,
  QExtctrls,
  QForms,
  QDialogs,
{$ELSE}
  Mask,
  Controls,
  Graphics,
  Stdctrls,
  Comctrls,
  Extctrls,
  Forms,
  Dialogs,
{$ENDIF}
  SysUtils,
  Classes;


const
{$IFDEF MSWINDOWS}
  WM_XPSTORERESULT = WM_USER + 101;
  WM_XPMOVEPREV = WM_USER + 102;
  WM_XPMOVENEXT = WM_USER + 103;
  WM_XPCANCELCONTROLS = WM_USER + 104;
{$ENDIF}
{$IFDEF LINUX}
  cXM_USER = integer(QEventType_ClxUser) + $3E9;
  cXM_XPSTORERESULT = cXM_USER;
  cXM_XPMOVEPREV = cXM_USER + 1;
  cXM_XPMOVENEXT = cXM_USER + 2;
  cXM_XPCANCELCONTROLS = cXM_USER + 3;

  cEventType_XPSTORERESULT = QEventType(cXM_XPSTORERESULT);
  cEventType_XPMOVEPREV = QEventType(cXM_XPMOVEPREV);
  cEventType_XPMOVENEXT = QEventType(cXM_XPMOVENEXT);
  cEventType_XPCANCELCONTROLS = QEventType(cXM_XPCANCELCONTROLS);
{$ENDIF}

type
  EXpEditType = (etNone, etEdit, etEditFunction, etFunction, etDropEdit, etDropList);

  TXpPropertyDataEvent = procedure(oOwner: TObject; wIndex: Integer;
                               var sData: String) of object;

  TXpQueryEditEvent = procedure(oOwner: TObject; wIndex: Integer;
                            var oEditType: EXpEditType;
                            var sFunctionName: String) of object;

  TXpValueChangeEvent = procedure(oOwner: TObject; wIndex: Integer;
                                  sValue: String) of object;

  TXpFunctionEvent = procedure(oOwner: TObject; wIndex: Integer;
                               sFunction: String; var sValue: String;
                           var bSendValue: Boolean) of object;

  TXpPropertyEdit = class(TCustomMaskEdit)
  protected

    { Protected declarations }
{$IFDEF MSWINDOWS}
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
{$ENDIF}
{$IFDEF LINUX}
//    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE; NeedTo CLX
{$ENDIF}
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;

  public
    { Public declarations }
    constructor Create(oOwner: TComponent); override;
  end;

  TXpPropertiesPane = class(TCustomControl)
  private
    { Private declarations }
    FBkgndColor: TColor;
    FRowCount: Integer;
    FRowHeight: Integer;
    FRowTop: Integer;
    FShowLines: Boolean;
    FSelected: Integer;
    FIndent: Integer;
    FEditable: Boolean;
    FEditType: EXpEditType;
    FFunction: String;
    FFont: TFont;
    FSelectedFont: TFont;
    FEdit: TXpPropertyEdit;
    FFunctionBtn: TButton;
    FOnPropertyData: TXpPropertyDataEvent;
    FOnQueryEdit: TXpQueryEditEvent;
    FOnSelectedChange: TNotifyEvent;
    FOnValueChange: TXpValueChangeEvent;
    FOnRowTopChanged: TNotifyEvent;
    FOnFunction: TXpFunctionEvent;

    procedure SetRowCount(const Value: Integer);
    procedure SetRowHeight(const Value: Integer);
    function GetVisibleRowCount: Integer;
    procedure SetRowTop(const Value: Integer);
    procedure SetShowLines(const Value: Boolean);
    procedure SetSelected(const Value: Integer);
    procedure SetEditable(const Value: Boolean);
    procedure SetFont(const Value: TFont);
    procedure SetSelectedFont(const Value: TFont);

    function ControlsVisible: Boolean;
    procedure ClearControls;
    procedure SetControls;
    procedure StoreResult;

  protected
    { Protected declarations }
    procedure DoFunctionClick(oOwner: TObject);
{$IFDEF MSWINDOWS}
    procedure WMEraseBkgnd(var oMsg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMLButtonDown(var oMsg: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMRButtonDown(var oMsg: TWMRButtonDown); message WM_RBUTTONDOWN;
    procedure WMMovePrevious(var oMsg: TMessage); message WM_XPMOVEPREV;
    procedure WMMoveNext(var oMsg: TMessage); message WM_XPMOVENEXT;
    procedure WMStoreResult(var oMsg: TMessage); message WM_XPSTORERESULT;
    procedure WMCancelControls(var oMsg: TMessage); message WM_XPCANCELCONTROLS;
{$ENDIF}
{$IFDEF LINUX}
    procedure XMEraseBkgnd(var oMsg: TExMessage);
//    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE; NeedTo CLX
    procedure XMLButtonDown(var oMsg: TExMouseMessage);
    procedure XMRButtonDown(var oMsg: TExMouseMessage);
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; override;
    procedure XMMovePrevious(var oMsg: TExMessage);
    procedure XMMoveNext(var oMsg: TExMessage);
    procedure XMStoreResult(var oMsg: TExMessage);
    procedure XMCancelControls(var oMsg: TExMessage);
{$ENDIF}
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;

  public
    { Public declarations }
    constructor Create(oOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    property BackgroundColor: TColor read FBkgndColor write FBkgndColor;
    property Editable: Boolean read FEditable write SetEditable;
    property Font: TFont read FFont write SetFont;
    property RowCount: Integer read FRowCount write SetRowCount;
    property RowHeight: Integer read FRowHeight write SetRowHeight;
    property RowTop: Integer read FRowTop write SetRowTop;
    property Selected: Integer read FSelected write SetSelected;
    property SelectedFont: TFont read FSelectedFont write SetSelectedFont;
    property ShowLines: Boolean read FShowLines write SetShowLines;
    property VisibleRowCount: Integer read GetVisibleRowCount;

    property OnFunction: TXpFunctionEvent read FOnFunction write FOnFunction;
    property OnPropertyData: TXpPropertyDataEvent read FOnPropertyData write FOnPropertyData;
    property OnQueryEdit: TXpQueryEditEvent read FOnQueryEdit write FOnQueryEdit;
    property OnRowTopChanged: TNotifyEvent read FOnRowTopChanged write FOnRowTopChanged;
    property OnSelectedChange: TNotifyEvent read FOnSelectedChange write FOnSelectedChange;
    property OnValueChange: TXpValueChangeEvent read FOnValueChange write FOnValueChange;
  end;

  TXpPropertiesWindow = class(TCustomControl)
  private
    { Private declarations }
{    FGeneralPanel: TPanel;}
    FHeader: THeaderControl;
    FNamePane: TXpPropertiesPane;
    FSplitter: TSplitter;
    FValuePane: TXpPropertiesPane;
    FScroller: TScrollBar;
    FShowHeader: Boolean;
    FNameHeader: String;
    FValueHeader: String;

    function GetRowCount: Integer;
    function GetRowHeight: Integer;
    procedure SetRowCount(const Value: Integer);
    procedure SetRowHeight(const Value: Integer);
    function GetRowTop: Integer;
    procedure SetRowTop(const Value: Integer);
    function GetShowLines: Boolean;
    procedure SetShowLines(const Value: Boolean);
    procedure SetNameHeader(const Value: String);
    procedure SetShowHeader(const Value: Boolean);
    procedure SetValueHeader(const Value: String);
    function GetSelected: Integer;
    procedure SetSelected(const Value: Integer);
    function GetOnPropertyName: TXpPropertyDataEvent;
    function GetOnPropertyValue: TXpPropertyDataEvent;
    procedure SetOnPropertyName(const Value: TXpPropertyDataEvent);
    procedure SetOnPropertyValue(const Value: TXpPropertyDataEvent);
    function GetNameFont: TFont;
    function GetNameFontSelected: TFont;
    function GetValueFont: TFont;
    function GetValueFontSelected: TFont;
    procedure SetNameFont(const Value: TFont);
    procedure SetNameFontSelected(const Value: TFont);
    procedure SetValueFont(const Value: TFont);
    procedure SetValueFontSelected(const Value: TFont);
    function GetOnNameQueryEdit: TXpQueryEditEvent;
    function GetOnValueQueryEdit: TXpQueryEditEvent;
    procedure SetOnNameQueryEdit(const Value: TXpQueryEditEvent);
    procedure SetOnValueQueryEdit(const Value: TXpQueryEditEvent);
    function GetOnValueChange: TXpValueChangeEvent;
    procedure SetOnValueChange(const Value: TXpValueChangeEvent);
    procedure SetNameWidth(const Value: Integer);
    function GetNameWidth: Integer;
    function GetOnNameFunction: TXpFunctionEvent;
    function GetOnValueFunction: TXpFunctionEvent;
    procedure SetOnNameFunction(const Value: TXpFunctionEvent);
    procedure SetOnValueFunction(const Value: TXpFunctionEvent);

  protected
    { Protected declarations }
{$IFNDEF UsingCLX}
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
{$ENDIF}
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    {$IFDEF DCC4OrLater}
    procedure Resize; override;
    {$ENDIF}
    {$IFNDEF UsingCLX}
    procedure DoHeaderResize(oHeaderControl: THeaderControl; oSection: THeaderSection);
    {$ELSE}
    procedure DoHeaderResize(oHeaderControl: TCustomHeaderControl; oSection: TCustomHeaderSection);
    {$ENDIF}
    procedure DoSplitterMoved(oOwner: TObject);
    procedure DoScroll(oSender: TObject; oScrollCode: TScrollCode; var oScrollPos: Integer);
    procedure DoSelectedName(oOwner: TObject);
    procedure DoSelectedValue(oOwner: TObject);
    procedure DoRowTopChanged(oOwner: TObject);

  public
    { Public declarations }
    constructor Create(oOwner: TComponent); override;
    destructor Destroy; override;
{$IFNDEF UsingCLX}
    procedure CreateParams(var Params: TCreateParams); override;
{$ENDIF}

    procedure Paint; override;

  published
    { Published declarations }
    property Align;
    property PopupMenu;
    property NameFont: TFont read GetNameFont write SetNameFont;
    property NameFontSelected: TFont read GetNameFontSelected write SetNameFontSelected;
    property NameHeader: String read FNameHeader write SetNameHeader;
    property NameWidth: Integer read GetNameWidth write SetNameWidth;
    property ValueFont: TFont read GetValueFont write SetValueFont;
    property ValueFontSelected: TFont read GetValueFontSelected write SetValueFontSelected;
    property ValueHeader: String read FValueHeader write SetValueHeader;
    property ShowHeader: Boolean read FShowHeader write SetShowHeader;
    property ShowLines: Boolean read GetShowLines write SetShowLines;
    property RowCount: Integer read GetRowCount write SetRowCount;
    property RowHeight: Integer read GetRowHeight write SetRowHeight;
    property RowTop: Integer read GetRowTop write SetRowTop;
    property Selected: Integer read GetSelected write SetSelected;

    property OnClick;
    property OnDblClick;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnNameFunction: TXpFunctionEvent read GetOnNameFunction write SetOnNameFunction;
    property OnNameQueryEdit: TXpQueryEditEvent read GetOnNameQueryEdit write SetOnNameQueryEdit;
    property OnPropertyName: TXpPropertyDataEvent read GetOnPropertyName write SetOnPropertyName;
    property OnPropertyValue: TXpPropertyDataEvent read GetOnPropertyValue write SetOnPropertyValue;
    property OnValueChange: TXpValueChangeEvent read GetOnValueChange write SetOnValueChange;
    property OnValueFunction: TXpFunctionEvent read GetOnValueFunction write SetOnValueFunction;
    property OnValueQueryEdit: TXpQueryEditEvent read GetOnValueQueryEdit write SetOnValueQueryEdit;
  end;

implementation

{ TXpPropertyEdit }

constructor TXpPropertyEdit.Create(oOwner: TComponent);
begin
  inherited Create(oOwner);
  BorderStyle := bsNone;
end;

procedure TXpPropertyEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
{$IFNDEF UsingCLX}
  case Key of
    VK_RETURN: begin
      PostMessage(Parent.Handle, WM_XPSTORERESULT, 0, 0);
      Key := 0;
    end;
    VK_UP: begin
      PostMessage(Parent.Handle, WM_XPMOVEPREV, 0, 0);
      Key := 0;
    end;
    VK_DOWN: begin
      PostMessage(Parent.Handle, WM_XPMOVENEXT, 0, 0);
      Key := 0;
    end;
    VK_ESCAPE: begin
      PostMessage(Parent.Handle, WM_XPCANCELCONTROLS, 0, 0);
      Key := 0;
    end;
  end;
{$ENDIF}
{$IFDEF LINUX}
  case Key of
    Key_return: begin
      ExPostMessage(Parent.Handle, cXM_XPSTORERESULT, 0, 0);
      Key := 0;
    end;
    Key_Up: begin
      ExPostMessage(Parent.Handle, cXM_XPMOVEPREV, 0, 0);
      Key := 0;
    end;
    Key_Down: begin
      ExPostMessage(Parent.Handle, cXM_XPMOVENEXT, 0, 0);
      Key := 0;
    end;
    Key_Escape: begin
      ExPostMessage(Parent.Handle, cXM_XPCANCELCONTROLS, 0, 0);
      Key := 0;
    end;
  end;
{$ENDIF}
  inherited KeyDown(Key, Shift);
end;

procedure TXpPropertyEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (Key = #13) or (Key = #27) then
    Key := #0;
end;
{$IFDEF MSWINDOWS} // NeedTo CLX
procedure TXpPropertyEdit.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTALLKEYS or DLGC_DEFPUSHBUTTON;
end;
{$ENDIF}
{ TXpPropertiesPane }

procedure TXpPropertiesPane.ClearControls;
begin
  FEditType := etNone;
  FEdit.Visible := false;
  FFunctionBtn.Visible := false;
  FFunction := '';
  SetFocus;
end;

constructor TXpPropertiesPane.Create(oOwner: TComponent);
begin
  inherited Create(oOwner);
  FBkgndColor := clBtnFace;
  FIndent := 5;
  FRowCount := 2;
  FRowHeight := 15;
  FRowTop := 0;
  FShowLines := true;
  FSelected := -1;
  FFont := TFont.Create;
  FSelectedFont := TFont.Create;
  FEdit := TXpPropertyEdit.Create(self);
  FEdit.Parent := self;
  FEdit.Visible := false;
  FFunctionBtn := TButton.Create(self);
  FFunctionBtn.Parent := self;
  FFunctionBtn.Visible := false;
  FFunctionBtn.Caption := '...';
  FFunctionBtn.Width := FRowHeight - 2;
  FFunctionBtn.Height := FFunctionBtn.Width;
  FFunctionBtn.TabStop := false;
  FFunctionBtn.OnClick := DoFunctionClick;
end;

destructor TXpPropertiesPane.Destroy;
begin
  FFont.Free;
  FSelectedFont.Free;
  inherited Destroy;
end;

function TXpPropertiesPane.GetVisibleRowCount: Integer;
begin
  Result := -1;
  if FRowHeight > 0 then
    Result := Height div FRowHeight;
end;

procedure TXpPropertiesPane.KeyDown(var Key: Word; Shift: TShiftState);
begin
  TXpPropertiesWindow(Parent).KeyDown(Key, Shift);
end;

procedure TXpPropertiesPane.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (Key = #13) or (Key = #27) then
    Key := #0;
end;

procedure TXpPropertiesPane.Paint;
var
  sDisplayData: String;
  y, wCnt, wHeight: Integer;
begin
  y := 0;
  wCnt := FRowTop;
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Color := clBlack;
{$IFNDEF UsingCLX}
  SetBkMode(Canvas.Handle, TRANSPARENT);  // NeedTo CLX
{$ENDIF}

  while (y < Height) and (wCnt < FRowCount) do begin
    sDisplayData := '';
    if Assigned(FOnPropertyData) then
      FOnPropertyData(Parent, wCnt, sDisplayData);
    if wCnt = FSelected then
      Canvas.Font.Assign(FSelectedFont)
    else
      Canvas.Font.Assign(FFont);
    wHeight := Canvas.TextHeight(sDisplayData);
    Canvas.TextOut(FIndent, y + ((FRowHeight - wHeight) shr 1), sDisplayData);
    Inc(y, FRowHeight);
    Inc(wCnt);
  end;
end;

procedure TXpPropertiesPane.SetControls;
var
  sTmp: String;
begin
  if (FSelected >= 0) and FEditable and Assigned(FOnQueryEdit) then begin
    FOnQueryEdit(Parent, FSelected, FEditType, FFunction);
    case FEditType of
      etEdit: begin
        FEdit.Left := 1;
        FEdit.Top := (FSelected - FRowTop) * FRowHeight + 1;
        FEdit.Width := Width - 2;
        sTmp := '';
        if Assigned(FOnPropertyData) then
          FOnPropertyData(Parent, FSelected, sTmp);
        FEdit.Text := sTmp;
        FEdit.Visible := true;
        FEdit.Height := FRowHeight - 2;
        FEdit.SetFocus;
      end;
      etEditFunction: begin
        FFunctionBtn.Visible := true;
        FFunctionBtn.Width := FRowHeight - 2;
        FFunctionBtn.Height := FFunctionBtn.Width;
        FFunctionBtn.Left := Width - FFunctionBtn.Width;
        FFunctionBtn.Top := (FSelected - FRowTop) * FRowHeight + 1;
        FEdit.Left := 1;
        FEdit.Top := (FSelected - FRowTop) * FRowHeight + 1;
        FEdit.Width := Width - FFunctionBtn.Width - 2;
        sTmp := '';
        if Assigned(FOnPropertyData) then
          FOnPropertyData(Parent, FSelected, sTmp);
        FEdit.Text := sTmp;
        FEdit.Visible := true;
        FEdit.Height := FRowHeight - 2;
        FEdit.SetFocus;
      end;
      etFunction: begin
        FFunctionBtn.Visible := true;
        FFunctionBtn.Width := FRowHeight - 2;
        FFunctionBtn.Height := FFunctionBtn.Width;
        FFunctionBtn.Left := Width - FFunctionBtn.Width;
        FFunctionBtn.Top := (FSelected - FRowTop) * FRowHeight + 1;
      end;
    end;
  end
  else
    SetFocus;
end;

procedure TXpPropertiesPane.SetEditable(const Value: Boolean);
begin
  FEditable := Value;
  Invalidate;
end;

procedure TXpPropertiesPane.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TXpPropertiesPane.SetRowCount(const Value: Integer);
begin
  FRowCount := Value;
  FSelected := -1;
  if ControlsVisible then
    ClearControls;
  Invalidate;
end;

procedure TXpPropertiesPane.SetRowHeight(const Value: Integer);
begin
  if FRowHeight <= 0 then
    FRowHeight := 10;
  FRowHeight := Value;
  Invalidate;
end;

procedure TXpPropertiesPane.SetRowTop(const Value: Integer);
begin
  FRowTop := Value;
  if (FSelected <> -1) and ControlsVisible then
    ClearControls;
  if Assigned(FOnRowTopChanged) then
    FOnRowTopChanged(Parent);
  Invalidate;
end;

procedure TXpPropertiesPane.SetSelected(const Value: Integer);
begin
  FSelected := Value;
  if ControlsVisible then
    ClearControls;
  Invalidate;
end;

procedure TXpPropertiesPane.SetSelectedFont(const Value: TFont);
begin
  FSelectedFont.Assign(Value);
end;

procedure TXpPropertiesPane.SetShowLines(const Value: Boolean);
begin
  FShowLines := Value;
  Invalidate;
end;

procedure TXpPropertiesPane.StoreResult;
begin
  if Assigned(FOnValueChange) then begin
    if FEdit.Visible then
      FOnValueChange(Parent, FSelected, FEdit.Text);
  end;
end;
{$IFDEF MSWINDOWS}
procedure TXpPropertiesPane.WMEraseBkgnd(var oMsg: TWMEraseBkgnd);
{$ENDIF}
{$IFDEF LINUX}
procedure TXpPropertiesPane.XMEraseBkgnd(var oMsg: TExMessage);
{$ENDIF}
var
  y, wCnt: Integer;
  oRect: TRect;
begin
  if Height = 0 then begin
    Invalidate;
    exit;
  end;

  y := 0;
  wCnt := FRowTop;
  Canvas.Pen.Color := clGray;
  Canvas.Pen.Width := 1;
  Canvas.Pen.Style := psDot;
  while (y < Height) and (wCnt < FRowCount) do begin
    { Draw background block }
    oRect := ClientRect;
    oRect.Top := y;
    oRect.Bottom := y + FRowHeight;
    Canvas.Pen.Style := psSolid;
{    if FEditable and (wCnt = FSelected) then
    begin
      Canvas.Pen.Color := clWhite;
      Canvas.Brush.Color := clWhite;
      FillRect(oMsg.DC, oRect, Canvas.Brush.Handle);
    end
    else}
    begin
      Canvas.Pen.Color := FBkgndColor;
      Canvas.Brush.Color := FBkgndColor;
{$IFNDEF UsingCLX}
      FillRect(oMsg.DC, oRect, Canvas.Brush.Handle);
{$ELSE}
      Canvas.FillRect(oRect);
{$ENDIF}
    end;
    if FShowLines then begin
      Canvas.Pen.Color := clGray;
      Canvas.Pen.Style := psDot;
      Canvas.MoveTo(0, y + FRowHeight - 1);
      Canvas.LineTo(Parent.Width, y + FRowHeight - 1);
    end;
    if wCnt = FSelected then begin
      Canvas.Pen.Style := psSolid;
      Canvas.Pen.Color := clWhite;
      Canvas.MoveTo(0, y + FRowHeight - 1);
      Canvas.LineTo(Parent.Width, y + FRowHeight - 1);
      Canvas.Pen.Color := clBlack;
      Canvas.MoveTo(0, y + FRowHeight - 1);
      Canvas.LineTo(0, y);
      Canvas.LineTo(Parent.Width, y);
      Canvas.Pen.Color := clGray;
      Canvas.MoveTo(0, y - 1);
      Canvas.LineTo(Parent.Width, y - 1);
    end;
    Inc(y, FRowHeight);
    Inc(wCnt);
  end;
  if y < Height then begin
    oRect := ClientRect;
    oRect.Top := y;
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Color := FBkgndColor;
    Canvas.Brush.Color := FBkgndColor;
{$IFNDEF UsingCLX}
    FillRect(oMsg.DC, oRect, Canvas.Brush.Handle);
{$ELSE}
    Canvas.FillRect(oRect);
{$ENDIF}
  end;

  oMsg.Result := 1;
end;
{$IFDEF MSWINDOWS}  // NeedTo CLX
procedure TXpPropertiesPane.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := DLGC_WANTALLKEYS or DLGC_WANTARROWS;
end;
{$ENDIF}
{$IFDEF MSWINDOWS}
procedure TXpPropertiesPane.WMLButtonDown(var oMsg: TWMLButtonDown);
{$ENDIF}
{$IFDEF LINUX}
procedure TXpPropertiesPane.XMLButtonDown(var oMsg: TExMouseMessage);
{$ENDIF}
begin
  if ControlsVisible then begin
    StoreResult;
    ClearControls;
  end;

  if FRowCount = 0 then
    FSelected := -1
  else
  begin
    FSelected := RowTop + (oMsg.YPos div FRowHeight);
    if FSelected >= FRowCount then
      FSelected := -1;
  end;
  Invalidate;
  if Assigned(FOnSelectedChange) then
    FOnSelectedChange(Parent);


  SetControls;
  inherited;
end;
{$IFDEF MSWINDOWS}
procedure TXpPropertiesPane.WMRButtonDown(var oMsg: TWMRButtonDown);
{$ENDIF}
{$IFDEF LINUX}
procedure TXpPropertiesPane.XMRButtonDown(var oMsg: TExMouseMessage);
{$ENDIF}
begin
  if ControlsVisible then
    ClearControls;

  if FRowCount = 0 then
    FSelected := -1
  else
  begin
    FSelected := RowTop + (oMsg.YPos div FRowHeight);
    if FSelected > FRowCount then
      FSelected := -1;
  end;
  Invalidate;
  if Assigned(FOnSelectedChange) then
    FOnSelectedChange(Parent);

  inherited;
end;

{$IFDEF LINUX}
function TXpPropertiesPane.EventFilter(Sender: QObjectH; Event: QEventH): Boolean;
  procedure GetMousePos(var M:TExMouseMessage);
  var
    T:TPoint;
  begin
    T := ScreenToClient(Mouse.CursorPos);
    M.XPos := T.X;
    M.YPos := T.Y;
  end;

  procedure GetEventMessage(var pT: pExMessage; var T:TExMessage );
  begin
    pT := QCustomEvent_data(QCustomEventH(Event));
    T := pT^;
  end;
var
  MP:TExMouseMessage;
  pM: pExMessage;
  M:TExMessage;
begin
  result := false;
  case QEvent_type(Event) of
    QEventType_MouseButtonPress: begin
      GetMousePos(MP);
      if QMouseEvent_button(QMouseEventH(Event)) = ButtonState_RightButton then begin
        XMRButtonDown(MP);
        Result := inherited EventFilter(Sender, Event);
      end else if QMouseEvent_button(QMouseEventH(Event)) = ButtonState_LeftButton then
        XMLButtonDown(MP)
      else
        Result := inherited EventFilter(Sender, Event);
    end;
    QEventType_Paint : begin
      FillChar(M,SizeOf(M),$00);
      XMEraseBkgnd(M);
      Result := inherited EventFilter(Sender, Event);
    end;
    cEventType_XPSTORERESULT:begin
      GetEventMessage(pM, M);
      XMStoreResult(M);
    end;
    cEventType_XPMOVEPREV:begin
      GetEventMessage(pM, M);
      XMMovePrevious(M);
    end;
    cEventType_XPMOVENEXT:begin
      GetEventMessage(pM, M);
      XMMoveNext(M);
    end;
    cEventType_XPCANCELCONTROLS:begin
      GetEventMessage(pM, M);
      XMCancelControls(M);
    end;
  else
    Result := inherited EventFilter(Sender, Event);
  end;
end;
{$ENDIF}
{$IFDEF MSWINDOWS}
procedure TXpPropertiesPane.WMMoveNext(var oMsg: TMessage);
{$ENDIF}
{$IFDEF LINUX}
procedure TXpPropertiesPane.XMMoveNext(var oMsg: TExMessage);
{$ENDIF}
begin
  if FSelected < FRowCount - 1 then begin
    if ControlsVisible then
      StoreResult;

    Inc(FSelected);
    if FSelected >= FRowTop + VisibleRowCount then begin
      FRowTop := FSelected - VisibleRowCount + 1;
      if Assigned(FOnRowTopChanged) then
        FOnRowTopChanged(Parent);
    end;
    Invalidate;
    if Assigned(FOnSelectedChange) then
      FOnSelectedChange(Parent);

    if ControlsVisible then begin
      ClearControls;
      SetControls;
    end;
  end;
end;

{$IFDEF MSWINDOWS}
procedure TXpPropertiesPane.WMMovePrevious(var oMsg: TMessage);
{$ENDIF}
{$IFDEF LINUX}
procedure TXpPropertiesPane.XMMovePrevious(var oMsg: TExMessage);
{$ENDIF}
begin
  if FSelected > 0 then begin
    if ControlsVisible then
      StoreResult;

    Dec(FSelected);
    if FSelected < FRowTop then begin
      FRowTop := FSelected;
      if Assigned(FOnRowTopChanged) then
        FOnRowTopChanged(Parent);
    end;
    Invalidate;
    if Assigned(FOnSelectedChange) then
      FOnSelectedChange(Parent);

    if ControlsVisible then begin
      ClearControls;
      SetControls;
    end;
  end;
end;

{$IFDEF MSWINDOWS}
procedure TXpPropertiesPane.WMCancelControls(var oMsg: TMessage);
{$ENDIF}
{$IFDEF LINUX}
procedure TXpPropertiesPane.XMCancelControls(var oMsg: TExMessage);
{$ENDIF}
begin
  if ControlsVisible then
    ClearControls;
end;

{$IFDEF MSWINDOWS}
procedure TXpPropertiesPane.WMStoreResult(var oMsg: TMessage);
{$ENDIF}
{$IFDEF LINUX}
procedure TXpPropertiesPane.XMStoreResult(var oMsg: TExMessage);
{$ENDIF}
begin
  if ControlsVisible then begin
    StoreResult;
    ClearControls;
  end
  else
    SetControls;
end;

function TXpPropertiesPane.ControlsVisible: Boolean;
begin
  Result := FEdit.Visible or FFunctionBtn.Visible;
end;

procedure TXpPropertiesPane.DoFunctionClick(oOwner: TObject);
var
  sResult: String;
  bSendResult: Boolean;
begin
  bSendResult := false;
  sResult := '';
  if Assigned(FOnFunction) then begin
    FOnFunction(Parent, FSelected, FFunction, sResult, bSendResult);
    if bSendResult and Assigned(FOnValueChange) then
      FOnValueChange(Parent, FSelected, sResult);
    SetControls;
    Invalidate;
  end;
end;


{ TXpPropertiesWindow }
{$IFNDEF UsingCLX}  // NeedTo CLX
procedure TXpPropertiesWindow.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  inherited;
  if (Char(Msg.CharCode) = #27) or (Char(Msg.CharCode) = #13) then
    Msg.Result := 1;
end;
{$ENDIF}

constructor TXpPropertiesWindow.Create(oOwner: TComponent);
var
  oSect: THeaderSection;
begin
  inherited Create(oOwner);
  FHeader := THeaderControl.Create(self);
  FHeader.Parent := self;
  FHeader.Visible := true;
  FHeader.Align := alTop;
  FHeader.OnSectionResize := DoHeaderResize;
  oSect := FHeader.Sections.Add;
  oSect.Width := 102;
  oSect := FHeader.Sections.Add;
  {$IFDEF DCC4OrLater}
  oSect.AutoSize := True;
{$IFNDEF UsingCLX} // NeedTo CLX
  FHeader.Style := hsFlat;
{$ENDIF}
  {$ENDIF}
  FNamePane := TXpPropertiesPane.Create(self);
  FNamePane.Parent := self;
  FNamePane.Visible := true;
  FNamePane.Width := 100;
  FNamePane.Align := alNone;
  FNamePane.OnSelectedChange := DoSelectedName;
  FSplitter := TSplitter.Create(self);
  FSplitter.Parent := self;
  FSplitter.Visible := true;
  FSplitter.Beveled := true;
  FSplitter.Align := alNone;
  FSplitter.MinSize := 20;
  FSplitter.Width := 2;
  FSplitter.OnMoved := DoSplitterMoved;
  FValuePane := TXpPropertiesPane.Create(self);
  FValuePane.Parent := self;
  FValuePane.Visible := true;
  FValuePane.Align := alNone;
  FValuePane.Editable := true;
  FValuePane.OnSelectedChange := DoSelectedValue;
  FValuePane.OnRowTopChanged := DoRowTopChanged;
  FScroller := TScrollBar.Create(self);
  FScroller.Parent := self;
  FScroller.Kind := sbVertical;
  FScroller.Align := alRight;
  FScroller.Visible := false;
  FScroller.OnScroll := DoScroll;
end;
{$IFNDEF UsingCLX}
procedure TXpPropertiesWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do begin
    Style := Style or WS_TABSTOP;
    WindowClass.style := CS_DBLCLKS;
    Style := Style and not WS_BORDER;
    ExStyle := ExStyle or WS_EX_CLIENTEDGE;
  end;
end;
{$ENDIF}
destructor TXpPropertiesWindow.Destroy;
begin
  inherited Destroy;
end;

{$IFNDEF UsingCLX}
procedure TXpPropertiesWindow.DoHeaderResize(oHeaderControl: THeaderControl; oSection: THeaderSection);
{$ELSE}
procedure TXpPropertiesWindow.DoHeaderResize(oHeaderControl: TCustomHeaderControl; oSection: TCustomHeaderSection);
{$ENDIF}
begin
  FNamePane.Width := FHeader.Sections.Items[0].Width - 2;
  FHeader.Sections.Items[1].Width := FHeader.Width - FHeader.Sections.Items[0].Width;
end;

procedure TXpPropertiesWindow.DoRowTopChanged(oOwner: TObject);
begin
{$IFDEF LINUX}
  if assigned(FValuePane)then begin
{$ENDIF}
    FNamePane.RowTop := FValuePane.RowTop;
    FScroller.Position := FValuePane.RowTop;
{$IFDEF LINUX}
  end;
{$ENDIF}
end;

procedure TXpPropertiesWindow.DoScroll(oSender: TObject; oScrollCode: TScrollCode; var oScrollPos: Integer);
begin
  case oScrollCode of
    scLineUp, scLineDown, scPageUp, scPageDown, scPosition,
    scTop, scBottom:
      RowTop := oScrollPos;
  end;
end;

procedure TXpPropertiesWindow.DoSelectedName(oOwner: TObject);
begin
{$IFDEF LINUX}
  if assigned(FValuePane)then
{$ENDIF}
    FValuePane.Selected := FNamePane.Selected;
  if assigned(OnClick) then
    OnClick(Self);
end;

procedure TXpPropertiesWindow.DoSelectedValue(oOwner: TObject);
begin
{$IFDEF LINUX}
  if assigned(FValuePane)then
{$ENDIF}
    FNamePane.Selected := FValuePane.Selected;
  if assigned(OnClick) then
    OnClick(Self);
end;

procedure TXpPropertiesWindow.DoSplitterMoved(oOwner: TObject);
begin
  if FHeader.Visible then begin
    FHeader.Sections.Items[0].Width := FNamePane.Width + 2;
    FHeader.Sections.Items[1].Width := FHeader.Width - FHeader.Sections.Items[0].Width;
  end;
end;

function TXpPropertiesWindow.GetNameFont: TFont;
begin
  Result := FNamePane.Font;
end;

function TXpPropertiesWindow.GetNameFontSelected: TFont;
begin
  Result := FNamePane.SelectedFont;
end;

function TXpPropertiesWindow.GetNameWidth: Integer;
begin
  Result := FNamePane.Width;
end;

function TXpPropertiesWindow.GetOnNameFunction: TXpFunctionEvent;
begin
  Result := FNamePane.OnFunction;
end;

function TXpPropertiesWindow.GetOnNameQueryEdit: TXpQueryEditEvent;
begin
  Result := FNamePane.OnQueryEdit;
end;

function TXpPropertiesWindow.GetOnPropertyName: TXpPropertyDataEvent;
begin
  Result := FNamePane.OnPropertyData;
end;

function TXpPropertiesWindow.GetOnPropertyValue: TXpPropertyDataEvent;
begin
{$IFDEF LINUX}
  if assigned(FValuePane)then
{$ENDIF}
    Result := FValuePane.OnPropertyData;
end;

function TXpPropertiesWindow.GetOnValueChange: TXpValueChangeEvent;
begin
{$IFDEF LINUX}
  if assigned(FValuePane)then
{$ENDIF}
    Result := FValuePane.OnValueChange;
end;

function TXpPropertiesWindow.GetOnValueFunction: TXpFunctionEvent;
begin
{$IFDEF LINUX}
  if assigned(FValuePane)then
{$ENDIF}
    Result := FValuePane.OnFunction;
end;

function TXpPropertiesWindow.GetOnValueQueryEdit: TXpQueryEditEvent;
begin
{$IFDEF LINUX}
  if assigned(FValuePane)then
{$ENDIF}
    Result := FValuePane.OnQueryEdit;
end;

function TXpPropertiesWindow.GetRowCount: Integer;
begin
{$IFDEF LINUX}
  Result := 0;
  if assigned(FValuePane)then
{$ENDIF}
    Result := FValuePane.RowCount;
end;

function TXpPropertiesWindow.GetRowHeight: Integer;
begin
{$IFDEF LINUX}
  Result := 0;
  if assigned(FValuePane)then
{$ENDIF}
    Result := FValuePane.RowHeight;
end;

function TXpPropertiesWindow.GetRowTop: Integer;
begin
{$IFDEF LINUX}
  Result := 0;
  if assigned(FValuePane)then
{$ENDIF}
    Result := FValuePane.RowTop;
end;

function TXpPropertiesWindow.GetSelected: Integer;
begin
{$IFDEF LINUX}
  Result := 0;
  if assigned(FValuePane)then
{$ENDIF}
    Result := FValuePane.Selected;
end;

function TXpPropertiesWindow.GetShowLines: Boolean;
begin
{$IFDEF LINUX}
  Result := false;
  if assigned(FValuePane)then
{$ENDIF}
    Result := FValuePane.ShowLines;
end;

function TXpPropertiesWindow.GetValueFont: TFont;
begin
{$IFDEF LINUX}
  Result := tfont(0);
  if assigned(FValuePane)then
{$ENDIF}
    Result := FValuePane.Font;
end;

function TXpPropertiesWindow.GetValueFontSelected: TFont;
begin
{$IFDEF LINUX}
  Result := tfont(0);
  if assigned(FValuePane)then
{$ENDIF}
    Result := FValuePane.SelectedFont;
end;

procedure TXpPropertiesWindow.KeyDown(var Key: Word; Shift: TShiftState);
begin
{$IFNDEF UsingCLX}
  case Key of
    VK_RETURN: begin
      PostMessage(FValuePane.Handle, WM_XPSTORERESULT, 0, 0);
      Key := 0;
    end;
    VK_UP: begin
      PostMessage(FValuePane.Handle, WM_XPMOVEPREV, 0, 0);
      Key := 0;
    end;
    VK_DOWN: begin
      PostMessage(FValuePane.Handle, WM_XPMOVENEXT, 0, 0);
      Key := 0;
    end;
    VK_ESCAPE: begin
      PostMessage(FValuePane.Handle, WM_XPCANCELCONTROLS, 0, 0);
      Key := 0;
    end;
  end;
{$ENDIF}
{$IFDEF LINUX}
  case Key of
    Key_return: begin
      ExPostMessage(FValuePane.Handle, cXM_XPSTORERESULT, 0, 0);
      Key := 0;
    end;
    Key_Up: begin
      ExPostMessage(FValuePane.Handle, cXM_XPMOVEPREV, 0, 0);
      Key := 0;
    end;
    Key_Down: begin
      ExPostMessage(FValuePane.Handle, cXM_XPMOVENEXT, 0, 0);
      Key := 0;
    end;
    Key_Escape: begin
      ExPostMessage(FValuePane.Handle, cXM_XPCANCELCONTROLS, 0, 0);
      Key := 0;
    end;
  end;
{$ENDIF}
  inherited KeyDown(Key, Shift);
end;

procedure TXpPropertiesWindow.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if (Key = #13) or (Key = #27) then
    Key := #0;
end;

procedure TXpPropertiesWindow.Paint;
begin
  inherited Paint;
  if FSplitter.Align = alNone then
    FSplitter.Align := alLeft;
  if FNamePane.Align = alNone then
    FNamePane.Align := alLeft;
{$IFDEF LINUX}
  if assigned(FValuePane)then
{$ENDIF}
    if FValuePane.Align = alNone then
      FValuePane.Align := alClient;
  {$IFDEF DCC4OrLater}
  Resize;
  {$ENDIF}
end;

{$IFDEF DCC4OrLater}
procedure TXpPropertiesWindow.Resize;
begin
  inherited Resize;
{$IFDEF LINUX}
  if assigned(FValuePane)then
{$ENDIF}
    if RowCount > FValuePane.VisibleRowCount then begin
      FScroller.Visible := true;
      FScroller.Max := RowCount - FValuePane.VisibleRowCount;
      FScroller.Position := RowTop;
    end
    else
      FScroller.Visible := false;
end;
{$ENDIF}

procedure TXpPropertiesWindow.SetNameFont(const Value: TFont);
begin
  FNamePane.Font := Value;
end;

procedure TXpPropertiesWindow.SetNameFontSelected(const Value: TFont);
begin
  FNamePane.SelectedFont := Value;
end;

procedure TXpPropertiesWindow.SetNameHeader(const Value: String);
begin
  FNameHeader := Value;
  FHeader.Sections.Items[0].Text := FNameHeader;
end;

procedure TXpPropertiesWindow.SetNameWidth(const Value: Integer);
begin
  FNamePane.Width := Value;
  if FHeader.Visible then begin
    FHeader.Sections.Items[0].Width := FNamePane.Width + 2;
    FHeader.Sections.Items[1].Width := FHeader.Width - FHeader.Sections.Items[0].Width;
  end;
end;

procedure TXpPropertiesWindow.SetOnNameFunction(const Value: TXpFunctionEvent);
begin
  FNamePane.OnFunction := Value;
end;

procedure TXpPropertiesWindow.SetOnNameQueryEdit(const Value: TXpQueryEditEvent);
begin
  FNamePane.OnQueryEdit := Value;
end;

procedure TXpPropertiesWindow.SetOnPropertyName(const Value: TXpPropertyDataEvent);
begin
  FNamePane.OnPropertyData := Value;
end;

procedure TXpPropertiesWindow.SetOnPropertyValue(const Value: TXpPropertyDataEvent);
begin
{$IFDEF LINUX}
  if assigned(FValuePane)then
{$ENDIF}
    FValuePane.OnPropertyData := Value;
end;

procedure TXpPropertiesWindow.SetOnValueChange(const Value: TXpValueChangeEvent);
begin
{$IFDEF LINUX}
  if assigned(FValuePane)then
{$ENDIF}
    FValuePane.OnValueChange := Value;
end;

procedure TXpPropertiesWindow.SetOnValueFunction(const Value: TXpFunctionEvent);
begin
{$IFDEF LINUX}
  if assigned(FValuePane)then
{$ENDIF}
    FValuePane.OnFunction := Value;
end;

procedure TXpPropertiesWindow.SetOnValueQueryEdit(const Value: TXpQueryEditEvent);
begin
{$IFDEF LINUX}
  if assigned(FValuePane)then
{$ENDIF}
    FValuePane.OnQueryEdit := Value;
end;

procedure TXpPropertiesWindow.SetRowCount(const Value: Integer);
begin
  FNamePane.RowCount := Value;
{$IFDEF LINUX}
  if assigned(FValuePane)then
{$ENDIF}
    FValuePane.RowCount := Value;
  {$IFDEF DCC4OrLater}
  Resize;
  {$ENDIF}
end;

procedure TXpPropertiesWindow.SetRowHeight(const Value: Integer);
begin
  FNamePane.RowHeight := Value;
{$IFDEF LINUX}
  if assigned(FValuePane)then
{$ENDIF}
    FValuePane.RowHeight := Value;
end;

procedure TXpPropertiesWindow.SetRowTop(const Value: Integer);
begin
{$IFDEF LINUX}
  if assigned(FValuePane)then
{$ENDIF}
    FValuePane.RowTop := Value;
  FScroller.Position := Value;
end;

procedure TXpPropertiesWindow.SetSelected(const Value: Integer);
begin
  FNamePane.Selected := Value;
{$IFDEF LINUX}
  if assigned(FValuePane)then
{$ENDIF}
    FValuePane.Selected := Value;
end;

procedure TXpPropertiesWindow.SetShowHeader(const Value: Boolean);
begin
  FShowHeader := Value;
  FHeader.Visible := FShowHeader;
  DoSplitterMoved(self);
end;

procedure TXpPropertiesWindow.SetShowLines(const Value: Boolean);
begin
  FNamePane.ShowLines := Value;
{$IFDEF LINUX}
  if assigned(FValuePane)then
{$ENDIF}
    FValuePane.ShowLines := Value;
end;

procedure TXpPropertiesWindow.SetValueFont(const Value: TFont);
begin
{$IFDEF LINUX}
  if assigned(FValuePane)then
{$ENDIF}
    FValuePane.Font := Value;
end;

procedure TXpPropertiesWindow.SetValueFontSelected(const Value: TFont);
begin
{$IFDEF LINUX}
  if assigned(FValuePane)then
{$ENDIF}
    FValuePane.SelectedFont := Value;
end;

procedure TXpPropertiesWindow.SetValueHeader(const Value: String);
begin
  FValueHeader := Value;
  FHeader.Sections.Items[1].Text := FValueHeader;
end;
{$IFNDEF UsingCLX} // NeedTo CLX
procedure TXpPropertiesWindow.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := DLGC_WANTARROWS;
end;
{$ENDIF}
end.
