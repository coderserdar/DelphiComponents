unit TB2ExtItems;

{
  Toolbar2000
  Copyright (C) 1998-2008 by Jordan Russell
  All rights reserved.

  The contents of this file are subject to the "Toolbar2000 License"; you may
  not use or distribute this file except in compliance with the
  "Toolbar2000 License". A copy of the "Toolbar2000 License" may be found in
  TB2k-LICENSE.txt or at:
    http://www.jrsoftware.org/files/tb2k/TB2k-LICENSE.txt

  Alternatively, the contents of this file may be used under the terms of the
  GNU General Public License (the "GPL"), in which case the provisions of the
  GPL are applicable instead of those in the "Toolbar2000 License". A copy of
  the GPL may be found in GPL-LICENSE.txt or at:
    http://www.jrsoftware.org/files/tb2k/GPL-LICENSE.txt
  If you wish to allow use of your version of this file only under the terms of
  the GPL and not to allow others to use your version of this file under the
  "Toolbar2000 License", indicate your decision by deleting the provisions
  above and replace them with the notice and other provisions required by the
  GPL. If you do not delete the provisions above, a recipient may use your
  version of this file under either the "Toolbar2000 License" or the GPL.

  $jrsoftware: tb2k/Source/TB2ExtItems.pas,v 1.68 2008/04/10 21:51:12 jr Exp $
}

interface

{$I TB2Ver.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CommCtrl, Menus, ActnList,
  TB2Item;

type
  TTBEditItemOption = (tboUseEditWhenVertical);
  TTBEditItemOptions = set of TTBEditItemOption;

const
  EditItemDefaultEditOptions = [];
  EditItemDefaultEditWidth = 64;

type
  TTBEditItem = class;
  TTBEditItemViewer = class;

  TTBAcceptTextEvent = procedure(Sender: TObject; var NewText: String;
    var Accept: Boolean) of object;
  TTBBeginEditEvent = procedure(Sender: TTBEditItem; Viewer: TTBEditItemViewer;
    EditControl: TEdit) of object;

  TTBEditAction = class(TAction)
  private
    FEditOptions: TTBEditItemOptions;
    FEditCaption: String;
    FEditWidth: Integer;
    FOnAcceptText: TTBAcceptTextEvent;
    FText: String;
    procedure SetEditCaption(Value: String);
    procedure SetEditOptions(Value: TTBEditItemOptions);
    procedure SetEditWidth(Value: Integer);
    procedure SetOnAcceptText(Value: TTBAcceptTextEvent);
    procedure SetText(Value: String);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property EditCaption: String read FEditCaption write SetEditCaption;
    property EditOptions: TTBEditItemOptions read FEditOptions write SetEditOptions default EditItemDefaultEditOptions;
    property EditWidth: Integer read FEditWidth write SetEditWidth default EditItemDefaultEditWidth;
    property Text: String read FText write SetText;

    property OnAcceptText: TTBAcceptTextEvent read FOnAcceptText write SetOnAcceptText;
  end;

  TTBEditItemActionLink = class(TTBCustomItemActionLink)
  protected
    procedure AssignClient(AClient: TObject); override;
    function IsEditCaptionLinked: Boolean; virtual;
    function IsEditOptionsLinked: Boolean; virtual;
    function IsEditWidthLinked: Boolean; virtual;
    function IsOnAcceptTextLinked: Boolean; virtual;
    function IsTextLinked: Boolean; virtual;
    procedure SetEditCaption(const Value: String); virtual;
    procedure SetEditOptions(Value: TTBEditItemOptions); virtual;
    procedure SetEditWidth(const Value: Integer); virtual;
    procedure SetOnAcceptText(Value: TTBAcceptTextEvent); virtual;
    procedure SetText(const Value: String); virtual;
  end;

  TTBEditItem = class(TTBCustomItem)
  private
    FCharCase: TEditCharCase;
    FEditCaption: String;
    FEditOptions: TTBEditItemOptions;
    FEditWidth: Integer;
    FMaxLength: Integer;
    FOnAcceptText: TTBAcceptTextEvent;
    FOnBeginEdit: TTBBeginEditEvent;
    FText: String;
    function IsEditCaptionStored: Boolean;
    function IsEditOptionsStored: Boolean;
    function IsEditWidthStored: Boolean;
    function IsTextStored: Boolean;
    procedure SetCharCase(Value: TEditCharCase);
    procedure SetEditCaption(Value: String);
    procedure SetEditOptions(Value: TTBEditItemOptions);
    procedure SetEditWidth(Value: Integer);
    procedure SetMaxLength(Value: Integer);
    procedure SetText(Value: String);
  protected
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure DoBeginEdit(Viewer: TTBEditItemViewer); virtual;
    function GetActionLinkClass: TTBCustomItemActionLinkClass; override;
    function GetItemViewerClass(AView: TTBView): TTBItemViewerClass; override;
    function NeedToRecreateViewer(AViewer: TTBItemViewer): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Clear;
    procedure Click; override;
  published
    property Action;
    property AutoCheck;
    property Caption;
    property CharCase: TEditCharCase read FCharCase write SetCharCase default ecNormal;
    property Checked;
    property DisplayMode;
    property EditCaption: String read FEditCaption write SetEditCaption stored IsEditCaptionStored;
    property EditOptions: TTBEditItemOptions read FEditOptions write SetEditOptions stored IsEditOptionsStored;
    property EditWidth: Integer read FEditWidth write SetEditWidth stored IsEditWidthStored;
    property MaxLength: Integer read FMaxLength write SetMaxLength default 0;
    property Enabled;
    property GroupIndex;
    property HelpContext;
    property Hint;
    property ImageIndex;
    property InheritOptions;
    property MaskOptions;
    property Options;
    property RadioItem;
    property ShortCut;
    property Text: String read FText write SetText stored IsTextStored;
    property Visible;

    property OnAcceptText: TTBAcceptTextEvent read FOnAcceptText write FOnAcceptText;
    property OnBeginEdit: TTBBeginEditEvent read FOnBeginEdit write FOnBeginEdit;
    property OnClick;
    property OnSelect;
  end;

  TTBEditItemViewer = class(TTBItemViewer)
  private
    FEditControl: TEdit;
    FEditControlStatus: set of (ecsContinueLoop, ecsAccept, ecsClose);
    function EditLoop(const CapHandle: HWND): Boolean;
    procedure EditWndProc(var Message: TMessage);
    procedure MouseBeginEdit;
  protected
    procedure CalcSize(const Canvas: TCanvas; var AWidth, AHeight: Integer);
      override;
    function CaptionShown: Boolean; override;
    function DoExecute: Boolean; override;
    function GetAccRole: Integer; override;
    function GetAccValue(var Value: WideString): Boolean; override;
    function GetCaptionText: String; override;
    procedure GetCursor(const Pt: TPoint; var ACursor: HCURSOR); override;
    procedure GetEditRect(var R: TRect); virtual;
    procedure MouseDown(Shift: TShiftState; X, Y: Integer;
      var MouseDownOnMenu: Boolean); override;
    procedure MouseUp(X, Y: Integer; MouseWasDownOnMenu: Boolean); override;
    procedure Paint(const Canvas: TCanvas; const ClientAreaRect: TRect;
      IsSelected, IsPushed, UseDisabledShadow: Boolean); override;
    function UsesSameWidth: Boolean; override;
  public
    property EditControl: TEdit read FEditControl;
  end;

  { TTBVisibilityToggleItem }

  TTBVisibilityToggleItem = class(TTBCustomItem)
  private
    FControl: TControl;
    procedure SetControl(Value: TControl);
    procedure UpdateProps;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    procedure Click; override;
    procedure InitiateAction; override;
  published
    property Caption;
    property Control: TControl read FControl write SetControl;
    property DisplayMode;
    property Enabled;
    property HelpContext;
    property Hint;
    property ImageIndex;
    property Images;
    property InheritOptions;
    property MaskOptions;
    property Options;
    property ShortCut;
    property Visible;

    property OnClick;
    property OnSelect;
  end;
  

implementation

uses
  TB2Common, TB2Consts;

const
  EditMenuTextMargin = 3;
  EditMenuMidWidth = 4;

type
  TControlAccess = class(TControl);
  TEditAccess = {$IFNDEF CLR} class(TEdit) {$ELSE} IControl {$ENDIF};


{ TTBEditAction }

constructor TTBEditAction.Create(AOwner: TComponent);
begin
  inherited;
  FEditOptions := EditItemDefaultEditOptions;
  FEditWidth := EditItemDefaultEditWidth;
  DisableIfNoHandler := False;
end;

procedure TTBEditAction.SetEditCaption(Value: String);
var
  I: Integer;
begin
  if FEditCaption <> Value then begin
    for I := 0 to FClients.Count - 1 do
      if TBasicActionLink(FClients[I]) is TTBEditItemActionLink then
        TTBEditItemActionLink(FClients[I]).SetEditCaption(Value);
    FEditCaption := Value;
    Change;
  end;
end;

procedure TTBEditAction.SetEditOptions(Value: TTBEditItemOptions);
var
  I: Integer;
begin
  if FEditOptions <> Value then begin
    for I := 0 to FClients.Count - 1 do
      if TBasicActionLink(FClients[I]) is TTBEditItemActionLink then
        TTBEditItemActionLink(FClients[I]).SetEditOptions(Value);
    FEditOptions := Value;
    Change;
  end;
end;

procedure TTBEditAction.SetEditWidth(Value: Integer);
var
  I: Integer;
begin
  if FEditWidth <> Value then begin
    for I := 0 to FClients.Count - 1 do
      if TBasicActionLink(FClients[I]) is TTBEditItemActionLink then
        TTBEditItemActionLink(FClients[I]).SetEditWidth(Value);
    FEditWidth := Value;
    Change;
  end;
end;

procedure TTBEditAction.SetOnAcceptText(Value: TTBAcceptTextEvent);
var
  I: Integer;
begin
  {$IFNDEF CLR}
  if not MethodsEqual(TMethod(FOnAcceptText), TMethod(Value)) then begin
  {$ELSE}
  if @FOnAcceptText <> @Value then begin
  {$ENDIF}
    for I := 0 to FClients.Count - 1 do
      if TBasicActionLink(FClients[I]) is TTBEditItemActionLink then
        TTBEditItemActionLink(FClients[I]).SetOnAcceptText(Value);
    FOnAcceptText := Value;
    Change;
  end;
end;

procedure TTBEditAction.SetText(Value: String);
var
  I: Integer;
begin
  if FText <> Value then begin
    for I := 0 to FClients.Count - 1 do
      if TBasicActionLink(FClients[I]) is TTBEditItemActionLink then
        TTBEditItemActionLink(FClients[I]).SetText(Value);
    FText := Value;
    Change;
  end;
end;


{ TTBEditItemActionLink }

procedure TTBEditItemActionLink.AssignClient(AClient: TObject);
begin
  FClient := AClient as TTBEditItem;
end;

function TTBEditItemActionLink.IsEditCaptionLinked: Boolean;
begin
  if Action is TTBEditAction then
    Result := TTBEditItem(FClient).EditCaption = TTBEditAction(Action).EditCaption
  else
    Result := False;
end;

function TTBEditItemActionLink.IsEditOptionsLinked: Boolean;
begin
  if Action is TTBEditAction then
    Result := TTBEditItem(FClient).EditOptions = TTBEditAction(Action).EditOptions
  else
    Result := False;
end;

function TTBEditItemActionLink.IsEditWidthLinked: Boolean;
begin
  if Action is TTBEditAction then
    Result := TTBEditItem(FClient).EditWidth = TTBEditAction(Action).EditWidth
  else
    Result := False;
end;

function TTBEditItemActionLink.IsOnAcceptTextLinked: Boolean;
begin
  if Action is TTBEditAction then
    {$IFNDEF CLR}
    Result := MethodsEqual(TMethod(TTBEditItem(FClient).OnAcceptText),
      TMethod(TTBEditAction(Action).OnAcceptText))
    {$ELSE}
    Result := @TTBEditItem(FClient).OnAcceptText = @TTBEditAction(Action).OnAcceptText
    {$ENDIF}
  else
    Result := False;
end;

function TTBEditItemActionLink.IsTextLinked: Boolean;
begin
  if Action is TTBEditAction then
    Result := TTBEditItem(FClient).Text = TTBEditAction(Action).Text
  else
    Result := False;
end;

procedure TTBEditItemActionLink.SetEditCaption(const Value: String);
begin
  if IsEditCaptionLinked then TTBEditItem(FClient).EditCaption := Value;
end;

procedure TTBEditItemActionLink.SetEditOptions(Value: TTBEditItemOptions);
begin
  if IsEditOptionsLinked then TTBEditItem(FClient).EditOptions := Value;
end;

procedure TTBEditItemActionLink.SetEditWidth(const Value: Integer);
begin
  if IsEditWidthLinked then TTBEditItem(FClient).EditWidth := Value;
end;

procedure TTBEditItemActionLink.SetOnAcceptText(Value: TTBAcceptTextEvent);
begin
  if IsOnAcceptTextLinked then TTBEditItem(FClient).OnAcceptText := Value;
end;

procedure TTBEditItemActionLink.SetText(const Value: String);
begin
  if IsTextLinked then TTBEditItem(FClient).Text := Value;
end;


{ TTBEditItem }

constructor TTBEditItem.Create(AOwner: TComponent);
begin
  inherited;
  FEditOptions := EditItemDefaultEditOptions;
  FEditWidth := EditItemDefaultEditWidth;
end;

procedure TTBEditItem.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited;
  if Action is TTBEditAction then
    with TTBEditAction(Sender) do
    begin
      if not CheckDefaults or (Self.EditCaption = '') then
        Self.EditCaption := EditCaption;
      if not CheckDefaults or (Self.EditOptions = []) then
        Self.EditOptions := EditOptions;
      if not CheckDefaults or (Self.Text = '') then
        Self.Text := Text;
      if not CheckDefaults or not Assigned(Self.OnAcceptText) then
        Self.OnAcceptText := OnAcceptText;
    end;
end;

function TTBEditItem.GetActionLinkClass: TTBCustomItemActionLinkClass;
begin
  Result := TTBEditItemActionLink;
end;

function TTBEditItem.GetItemViewerClass(AView: TTBView): TTBItemViewerClass;
begin
  if not(tboUseEditWhenVertical in EditOptions) and
     (AView.Orientation = tbvoVertical) then
    Result := inherited GetItemViewerClass(AView)
  else
    Result := TTBEditItemViewer;
end;

function TTBEditItem.NeedToRecreateViewer(AViewer: TTBItemViewer): Boolean;
begin
  Result := GetItemViewerClass(AViewer.View) <> AViewer.ClassType;
end;

procedure TTBEditItem.Clear;
begin
  Text := '';
end;

procedure TTBEditItem.Click;
begin
  inherited;
end;

procedure TTBEditItem.DoBeginEdit(Viewer: TTBEditItemViewer);
begin
  if Assigned(FOnBeginEdit) then
    FOnBeginEdit(Self, Viewer, Viewer.EditControl);
end;

function TTBEditItem.IsEditOptionsStored: Boolean;
begin
  Result := (EditOptions <> EditItemDefaultEditOptions) and
    ((ActionLink = nil) or not(ActionLink is TTBEditItemActionLink) or
     not TTBEditItemActionLink(ActionLink).IsEditOptionsLinked);
end;

function TTBEditItem.IsEditCaptionStored: Boolean;
begin
  Result := (ActionLink = nil) or not(ActionLink is TTBEditItemActionLink) or
    not TTBEditItemActionLink(ActionLink).IsEditCaptionLinked;
end;

function TTBEditItem.IsEditWidthStored: Boolean;
begin
  Result := (EditWidth <> EditItemDefaultEditWidth) and
    ((ActionLink = nil) or not(ActionLink is TTBEditItemActionLink) or
     not TTBEditItemActionLink(ActionLink).IsEditWidthLinked);
end;

function TTBEditItem.IsTextStored: Boolean;
begin
  Result := (ActionLink = nil) or not(ActionLink is TTBEditItemActionLink) or
    not TTBEditItemActionLink(ActionLink).IsTextLinked;
end;

procedure TTBEditItem.SetCharCase(Value: TEditCharCase);
begin
  if FCharCase <> Value then begin
    FCharCase := Value;
    Text := Text;  { update case }
  end;
end;

procedure TTBEditItem.SetEditOptions(Value: TTBEditItemOptions);
begin
  if FEditOptions <> Value then begin
    FEditOptions := Value;
    Change(True);
  end;
end;

procedure TTBEditItem.SetEditCaption(Value: String);
begin
  if FEditCaption <> Value then begin
    FEditCaption := Value;
    Change(True);
  end;
end;

procedure TTBEditItem.SetEditWidth(Value: Integer);
begin
  if FEditWidth <> Value then begin
    FEditWidth := Value;
    Change(True);
  end;
end;

procedure TTBEditItem.SetMaxLength(Value: Integer);
begin
  if FMaxLength <> Value then begin
    FMaxLength := Value;
    Change(False);
  end;
end;

procedure TTBEditItem.SetText(Value: String);
begin
  case FCharCase of
    ecUpperCase: Value := {$IFNDEF CLR} AnsiUpperCase {$ELSE} UpperCase {$ENDIF} (Value);
    ecLowerCase: Value := {$IFNDEF CLR} AnsiLowerCase {$ELSE} LowerCase {$ENDIF} (Value);
  end;
  if FText <> Value then begin
    FText := Value;
    Change(False);
  end;
end;


{ TTBEditItemViewer }

procedure TTBEditItemViewer.EditWndProc(var Message: TMessage);
var
  Item: TTBEditItem;

  procedure AcceptText;
  var
    S: String;
    Accept: Boolean;
  begin
    S := FEditControl.Text;
    Accept := True;
    if Assigned(Item.FOnAcceptText) then
      Item.FOnAcceptText(Self, S, Accept);
    if Accept then
      Item.Text := S;
  end;

begin
  Item := TTBEditItem(Self.Item);
  if Message.Msg = WM_CHAR then
    case Word(Message.WParam) of
      VK_TAB: begin
          FEditControlStatus := [ecsAccept];
          AcceptText;
          Exit;
        end;
      VK_RETURN: begin
          FEditControlStatus := [ecsAccept, ecsClose];
          AcceptText;
          Exit;
        end;
      VK_ESCAPE: begin
          FEditControlStatus := [];
          Exit;
        end;
    end;
  TEditAccess(FEditControl).WndProc(Message);
  if Message.Msg = WM_KILLFOCUS then begin
    { Someone has stolen the focus from us, so 'cancel mode'. (We have to
      handle WM_KILLFOCUS in addition to the upstream WM_CANCELMODE handling
      since we don't always hold the mouse capture.) }
    View.CancelMode;
    FEditControlStatus := [ecsClose];
  end;
end;

procedure TTBEditItemViewer.GetEditRect(var R: TRect);
var
  Item: TTBEditItem;
  DC: HDC;
begin
  Item := TTBEditItem(Self.Item);
  DC := GetDC(0);
  try
    SelectObject(DC, View.GetFont.Handle);
    R := BoundsRect;
    if not View.IsToolbar and (Item.EditCaption <> '') then begin
      Inc(R.Left, GetTextWidth(DC, Item.EditCaption, True) +
        EditMenuMidWidth + EditMenuTextMargin * 2);
    end;
  finally
    ReleaseDC(0, DC);
  end;
end;

procedure TTBEditItemViewer.CalcSize(const Canvas: TCanvas;
  var AWidth, AHeight: Integer);
var
  Item: TTBEditItem;
  DC: HDC;
begin
  Item := TTBEditItem(Self.Item);
  DC := Canvas.Handle;
  AWidth := Item.FEditWidth;
  AHeight := GetTextHeight(DC) + (EditMenuTextMargin * 2) + 1;
  if not IsToolbarStyle and (Item.EditCaption <> '') then begin
    Inc(AWidth, GetTextWidth(DC, Item.EditCaption, True) + EditMenuMidWidth +
      EditMenuTextMargin * 2);
  end;
  { Review: Should the height include external leading on fonts that use it,
    such as the default menu font on Windows Me Trad. Chinese? Office 2000
    seems to insist on using Tahoma on Chinese Windows, so I'm not sure how it
    handles external leading on edit items. }
end;

function TTBEditItemViewer.CaptionShown: Boolean;
begin
  Result := not IsToolbarStyle and inherited CaptionShown;
end;

function TTBEditItemViewer.GetCaptionText: String;
begin
  Result := TTBEditItem(Item).EditCaption;
end;

procedure TTBEditItemViewer.Paint(const Canvas: TCanvas;
  const ClientAreaRect: TRect; IsSelected, IsPushed, UseDisabledShadow: Boolean);
const
  FillColors: array[Boolean] of TColor = (clBtnFace, clWindow);
  TextColors: array[Boolean] of TColor = (clGrayText, clWindowText);
var
  Item: TTBEditItem;
  S: String;
  R: TRect;
  W: Integer;
begin
  Item := TTBEditItem(Self.Item);
  R := ClientAreaRect;

  { Caption }
  if not IsToolbarStyle and (Item.EditCaption <> '') then begin
    S := Item.EditCaption;
    W := GetTextWidth(Canvas.Handle, S, True) + EditMenuTextMargin * 2;
    R.Right := R.Left + W;
    if IsSelected then
      Canvas.FillRect(R);
    Inc(R.Left, EditMenuTextMargin);
    DrawItemCaption(Canvas, R, S, UseDisabledShadow, DT_SINGLELINE or
      DT_LEFT or DT_VCENTER);
    R := ClientAreaRect;
    Inc(R.Left, W + EditMenuMidWidth);
  end;

  { Border }
  if IsSelected and Item.Enabled then
    DrawEdge(Canvas.Handle, R, BDR_SUNKENOUTER, BF_RECT);
  InflateRect(R, -1, -1);
  Canvas.Brush.Color := FillColors[not Item.Enabled];
  Canvas.FrameRect(R);
  InflateRect(R, -1, -1);

  { Fill }
  Canvas.Brush.Color := FillColors[Item.Enabled];
  Canvas.FillRect(R);
  InflateRect(R, -1, -1);

  { Text }
  if Item.Text <> '' then begin
    S := Item.Text;
    Canvas.Brush.Style := bsClear;  { speed optimization }
    Canvas.Font.Color := TextColors[Item.Enabled];
    DrawTextStr(Canvas.Handle, S, R, DT_SINGLELINE or DT_NOPREFIX);
  end;
end;

procedure TTBEditItemViewer.GetCursor(const Pt: TPoint; var ACursor: HCURSOR);
var
  R: TRect;
begin
  if not Item.Enabled then
    Exit;
  GetEditRect(R);
  OffsetRect(R, -BoundsRect.Left, -BoundsRect.Top);
  InflateRect(R, -2, -2);
  if PtInRect(R, Pt) then
    ACursor := LoadCursor(0, IDC_IBEAM);
end;

function TTBEditItemViewer.EditLoop(const CapHandle: HWND): Boolean;

  procedure ControlMessageLoop;

    function PointInWindow(const Wnd: HWND; const P: TPoint): Boolean;
    var
      W: HWND;
    begin
      Result := False;
      W := WindowFromPoint(P);
      if W = 0 then Exit;
      if W = Wnd then
        Result := True
      else
        if IsChild(Wnd, W) then
          Result := True;
    end;

    function ContinueLoop: Boolean;
    begin
      Result := (ecsContinueLoop in FEditControlStatus) and
        not View.IsModalEnding and FEditControl.Focused and Item.Enabled;
      { Note: View.IsModalEnding is checked since TTBView.CancelMode doesn't
        destroy popup windows; it merely hides them and calls EndModal. So if
        IsModalEnding returns True we can infer that CancelMode was likely
        called. }
    end;

  var
    Msg: TMsg;
    IsKeypadDigit: Boolean;
    ScanCode: Byte;
    V: Integer;
  begin
    try
      while ContinueLoop do begin
        { Examine the next message before popping it out of the queue }
        if not PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE) then begin
          WaitMessage;
          Continue;
        end;
        case Msg.message of
          WM_SYSKEYDOWN: begin
              { Exit immediately if Alt+[key] or F10 are pressed, but not
                Alt+Shift, Alt+`, or Alt+[keypad digit] }
              if not(Word(Msg.wParam) in [VK_MENU, VK_SHIFT, VK_HANJA]) then begin
                IsKeypadDigit := False;
                { This detect digits regardless of whether Num Lock is on: }
                ScanCode := Byte(Msg.lParam shr 16);
                if ScanCode <> 0 then
                  for V := VK_NUMPAD0 to VK_NUMPAD9 do
                    if MapVirtualKey(V, 0) = ScanCode then begin
                      IsKeypadDigit := True;
                      Break;
                    end;
                if not IsKeypadDigit then begin
                  FEditControlStatus := [ecsClose];
                  Exit;
                end;
              end;
            end;
          WM_SYSKEYUP: begin
              { Exit when Alt is released by itself }
              if Word(Msg.wParam) = VK_MENU then begin
                FEditControlStatus := [ecsClose];
                Exit;
              end;
            end;
          WM_LBUTTONDOWN, WM_LBUTTONDBLCLK,
          WM_RBUTTONDOWN, WM_RBUTTONDBLCLK,
          WM_MBUTTONDOWN, WM_MBUTTONDBLCLK,
          WM_NCLBUTTONDOWN, WM_NCLBUTTONDBLCLK,
          WM_NCRBUTTONDOWN, WM_NCRBUTTONDBLCLK,
          WM_NCMBUTTONDOWN, WM_NCMBUTTONDBLCLK: begin
              { If a mouse click outside the edit control is in the queue,
                exit and let the upstream message loop deal with it }
              if Msg.hwnd <> FEditControl.Handle then
                Exit;
            end;
          WM_MOUSEMOVE, WM_NCMOUSEMOVE: begin
              if GetCapture = CapHandle then begin
                if PointInWindow(FEditControl.Handle, Msg.pt) then
                  ReleaseCapture;
              end
              else if GetCapture = 0 then begin
                if not PointInWindow(FEditControl.Handle, Msg.pt) then
                  SetCapture(CapHandle);
              end;
              if GetCapture = CapHandle then
                SetCursor(LoadCursor(0, IDC_ARROW));
            end;
        end;
        { Now pop the message out of the queue }
        if not PeekMessage(Msg, 0, Msg.message, Msg.message, PM_REMOVE or PM_NOYIELD) then
          Continue;
        if ((Msg.message >= WM_MOUSEFIRST) and (Msg.message <= WM_MOUSELAST)) and
           (Msg.hwnd = CapHandle) then
          { discard, so that the selection doesn't get changed }
        else begin
          TranslateMessage(Msg);
          DispatchMessage(Msg);
        end;
      end;
    finally
      { Make sure there are no outstanding WM_*CHAR messages }
      RemoveMessages(WM_CHAR, WM_DEADCHAR);
      RemoveMessages(WM_SYSCHAR, WM_SYSDEADCHAR);
    end;
  end;

  procedure RestoreEditControlWndProc;
  {$IFNDEF CLR}
  var
    OrigWndProc: TWndMethod;
  begin
    { NOTE: We can't assign WndProc to WindowProc directly because on Delphi 4
      and 5, the compiler generates incorrect code, causing an AV at run-time }
    OrigWndProc := TEditAccess(FEditControl).WndProc;
    FEditControl.WindowProc := OrigWndProc;
  end;
  {$ELSE}
  begin
    IControl(FEditControl).RestoreWndProc;
  end;
  {$ENDIF}

var
  Item: TTBEditItem;
  R: TRect;
  ActiveWnd, FocusWnd: HWND;
begin
  Item := TTBEditItem(Self.Item);
  GetEditRect(R);
  if IsRectEmpty(R) then begin
    Result := False;
    Exit;
  end;

  ActiveWnd := GetActiveWindow;
  FocusWnd := GetFocus;

  { Create the edit control }
  InflateRect(R, -3, -3);
  //View.FreeNotification(Self);
  FEditControl := TEdit.Create(nil);
  try
    FEditControl.Visible := False;
    FEditControl.BorderStyle := bsNone;
    FEditControl.AutoSize := False;
    FEditControl.Font.Assign(View.GetFont);
    FEditControl.Text := Item.Text;
    FEditControl.CharCase := Item.FCharCase;
    FEditControl.MaxLength := Item.FMaxLength;
    FEditControl.BoundsRect := R;
    FEditControl.WindowProc := EditWndProc;
    FEditControl.ParentWindow := View.Window.Handle;
    FEditControl.SelectAll;
    Item.DoBeginEdit(Self);
    FEditControl.Visible := True;
    FEditControl.SetFocus;
    if GetActiveWindow <> ActiveWnd then
      { don't gray out title bar of old active window }
      SendMessage(ActiveWnd, WM_NCACTIVATE, 1, 0)
    else
      ActiveWnd := 0;

    FEditControlStatus := [ecsContinueLoop];
    ControlMessageLoop;
  finally
    { Restore the original window procedure before destroying the control so
      it doesn't see a WM_KILLFOCUS message }
    RestoreEditControlWndProc;
    FreeAndNil(FEditControl);
  end;

  { ensure the area underneath the edit control is repainted immediately }
  View.Window.Update;
  { If app is still active, set focus to previous control and restore capture
    to CapHandle if another control hasn't taken it }
  if GetActiveWindow <> 0 then begin
    SetFocus(FocusWnd);
    if GetCapture = 0 then
      SetCapture(CapHandle);
  end;
  if ActiveWnd <> 0 then
    SendMessage(ActiveWnd, WM_NCACTIVATE, Ord(GetActiveWindow = ActiveWnd), 0);
  { The SetFocus call above can change the Z order of windows. If the parent
    window is a popup window, reassert its topmostness. }
  if View.Window is TTBPopupWindow then
    SetWindowPos(View.Window.Handle, HWND_TOPMOST, 0, 0, 0, 0,
      SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE);
  { Send an MSAA "focus" event now that we're returning to the regular modal loop }
  View.NotifyFocusEvent;

  Result := ecsClose in FEditControlStatus;
  if not Result and (GetCapture = CapHandle) then begin
    if ecsAccept in FEditControlStatus then
      { if we are accepting but not closing, Tab must have been pressed }
      View.Selected := View.NextSelectable(View.Selected,
        GetKeyState(VK_SHIFT) >= 0);
  end;
end;

function TTBEditItemViewer.DoExecute: Boolean;
begin
  { Close any delay-close popup menus before entering the edit loop }
  View.CancelChildPopups;
  Result := False;
  if EditLoop(View.GetCaptureWnd) then begin
    View.EndModal;
    if ecsAccept in FEditControlStatus then
      Result := True;
  end;
end;

procedure TTBEditItemViewer.MouseBeginEdit;
begin
  if Item.Enabled then
    Execute(True)
  else begin
    if (View.ParentView = nil) and not View.IsPopup then
      View.EndModal;
  end;
end;

procedure TTBEditItemViewer.MouseDown(Shift: TShiftState; X, Y: Integer;
  var MouseDownOnMenu: Boolean);
begin
  if IsPtInButtonPart(X, Y) then  { for TBX... }
    MouseBeginEdit
  else
    inherited;
end;

procedure TTBEditItemViewer.MouseUp(X, Y: Integer; MouseWasDownOnMenu: Boolean);
begin
  if IsPtInButtonPart(X, Y) then  { for TBX... }
    MouseBeginEdit
  else
    inherited;
end;

function TTBEditItemViewer.UsesSameWidth: Boolean;
begin
  Result := False;
end;

function TTBEditItemViewer.GetAccRole: Integer;
const
  ROLE_SYSTEM_TEXT = $2a;  { from OleAcc.h }
begin
  Result := ROLE_SYSTEM_TEXT;
end;

function TTBEditItemViewer.GetAccValue(var Value: WideString): Boolean;
begin
  Value := TTBEditItem(Item).Text;
  Result := True;
end;


{ TTBToolbarVisibilityItem }

procedure TTBVisibilityToggleItem.Click;
begin
  if Assigned(FControl) then
    FControl.Visible := not FControl.Visible;
  inherited;
end;

procedure TTBVisibilityToggleItem.InitiateAction;
begin
  UpdateProps;
end;

procedure TTBVisibilityToggleItem.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FControl) then
    Control := nil;
end;

procedure TTBVisibilityToggleItem.SetControl(Value: TControl);
begin
  if FControl <> Value then begin
    FControl := Value;
    if Assigned(Value) then begin
      Value.FreeNotification(Self);
      if (Caption = '') and not(csLoading in ComponentState) then
        {$IFNDEF CLR}
        Caption := TControlAccess(Value).Caption;
        {$ELSE}
        Caption := Value.GetText;
        {$ENDIF}
    end;
    UpdateProps;
  end;
end;

procedure TTBVisibilityToggleItem.UpdateProps;
begin
  if (ComponentState * [csDesigning, csLoading, csDestroying] = []) then
    Checked := Assigned(FControl) and FControl.Visible;
end;

end.
