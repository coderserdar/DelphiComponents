unit DsToolBar;
//------------------------------------------------------------------------------
interface
//------------------------------------------------------------------------------
uses
  Windows, Classes, Controls, Messages, ImgList, Menus, DsToolBarStyler, Graphics,
  StdCtrls;
//------------------------------------------------------------------------------
const
  bmDropDown = 'BTN_ARROWDOWN';
//------------------------------------------------------------------------------
type
//------------------------------------------------------------------------------
  TSubjectChange = (cngAttach, cngDetach, cngVisible);
  TTextOrientation = (toHorizontal, toVertical);

  TObserver = class
  public
    procedure Update(aSubject: TObject; aChange: TSubjectChange); virtual; abstract;
  end;


  TTextToImagePosition = (tipBeforeImage, tipAfterImage, tipAboveImage, tipBelowImage, tipOverlayImage);

  TDSToolButton = class;

  TDSToolButtonActionLink = class(TControlActionLink)
  private
    fClient: TDSToolButton;
  protected
    procedure AssignClient(aClient: TObject); override;
    function IsCheckedLinked: Boolean; override;
    function IsImageIndexLinked: Boolean; override;
    procedure SetChecked(Value: Boolean); override;
    procedure SetImageIndex(Value: Integer); override;
  public
    function Execute(AComponent: TComponent = nil): Boolean; override;
  end;
  TDSToolButtonActionLinkClass = class of TDSToolButtonActionLink;
//------------------------------------------------------------------------------
  TDSToolButtonGroup = class(TComponent)
  private
    fButtons: TList;
    fAllowAllUp: Boolean;
  protected
    procedure InternalAppendButton(aButton: TDSToolButton);
    procedure InternalRemoveButton(aButton: TDSToolButton);
    procedure CanCheck(aButton: TDSToolButton; var aChecked: Boolean);
    procedure ButtonCheck(aButton: TDSToolButton; aChecked: Boolean);
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure AppendButton(aButton: TDSToolButton);
    procedure RemoveButton(aButton: TDSToolButton);
  published
    property AllowAllUp: Boolean read fAllowAllUp write fAllowAllUp;
  end;//TDSToolButtonGroup
//------------------------------------------------------------------------------
  TPopupSide = (popLeft, popTop, popRight, popBottom);
  
  TDSToolButton = class(TGraphicControl)
  private
    fFlat: Boolean;
    fDown: Boolean;
    fHot: Boolean;
    fCheck: Boolean;
    fOldDown: Boolean;
    fImages: TCustomImageList;
    fImageIndex: TImageIndex;
    fDropDownMenu: TPopupMenu;
    fMenuItem: TMenuItem;
    fDropped: Boolean;
    fDropButton: Boolean;
    fShowText: Boolean;
    fShowImage: Boolean;
    fTextToImage: TTextToImagePosition;
    fImageRect: TRect;
    fTextRect: TRect;
    vDropDown: TPopupMenu;
    fTextOrientation: TTextOrientation;
    fStyler: TDsToolBarStyler;
    fPopupSide: TPopupSide;
    fGroup: TDsToolButtonGroup;
    fOnCheck: TNotifyEvent;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMHitTest(var Message: TCMHitTest); message CM_HITTEST;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure setShowText(const Value: Boolean);
    procedure setShowImage(const Value: Boolean);
    procedure setTextToImage(const Value: TTextToImagePosition);
    procedure setTextOrientation(const Value: TTextOrientation);
    procedure TempMenuToTMenuItem(vTempMenu: TPopupMenu);
    function CreatePopupFromMenuItem: TPopupMenu;
    procedure setGroup(const Value: TDsToolButtonGroup);
  protected
    procedure OrderTextAndImage;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure AssignTo(Dest: TPersistent); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure setMenuItem(const Value: TMenuItem);
    procedure setDropDownMenu(const Value: TPopupMenu);
    procedure setDropButton(const Value: Boolean);
    procedure setImageIndex(const Value: TImageIndex);
    procedure setImages(const Value: TCustomImageList);
    procedure setHot(const Value: Boolean);
    procedure setCheck(const Value: Boolean);
    procedure setDown(const Value: Boolean);
    procedure setFlat(const Value: Boolean);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure Notification(aComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    function NeededWidth: Integer;
    function NeededHeight: Integer;
    procedure Click; override;
  published
    property Action;
    property Enabled;
    property Flat: Boolean read fFlat write setFlat;
    property Down: Boolean read fDown write setDown;
    property Hot: Boolean read fHot write setHot;
    property Check: Boolean read fCheck write setCheck;
    property Group: TDsToolButtonGroup read fGroup write setGroup;
    property Images: TCustomImageList read fImages write setImages;
    property ImageIndex: TImageIndex read fImageIndex write setImageIndex default -1;
    property MenuItem: TMenuItem read fMenuItem write setMenuItem;
    property DropDownMenu: TPopupMenu read fDropDownMenu write setDropDownMenu;
    property DropButton: Boolean read fDropButton write setDropButton;
    property Caption;
    property ShowHint;
    property ShowImage: Boolean read fShowImage write setShowImage;
    property ShowText: Boolean read fShowText write setShowText;
    property TextToImage: TTextToImagePosition read fTextToImage write setTextToImage;
    property TextOrientation: TTextOrientation read fTextOrientation write setTextOrientation;
    property PopupSide: TPopupSide read fPopupSide write fPopupSide;
    property Color;
    property Font;
    property OnClick;
    property OnCheck: TNotifyEvent read fOnCheck write fOnCheck;
  end;//TDSToolButton
//------------------------------------------------------------------------------
  TDSToolBar = class(TCustomControl)
  private
    fVertical: Boolean;
    fButtons: TList;
    fInOrderButtons: Boolean;
    fMenu: TMainMenu;
    fInMenuLoop: Boolean;
    fMenuResult: Boolean;
    fCaptureChangeCancels: Boolean;
    fObservers: TList;
    fStyler: TDsToolBarStyler;
    fColorTo: TColor;
    fInsertControl: TControl;
    fImages: TImageList;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMControlListChange(var Message: TCMControlListChange); message CM_CONTROLLISTCHANGE;
    procedure CMControlChange(var Message: TCMControlChange); message CM_CONTROLCHANGE;
    procedure CMHitTest(var Message: TCMHitTest); message CM_HITTEST;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure setMenu(const Value: TMainMenu);
    procedure WMSysCommand(var Message: TWMSysCommand); message WM_SYSCOMMAND;
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    function FindButtonFromAccel(Accel: Word): TDSToolButton;
  protected
    procedure setVertical(const Value: Boolean);
    procedure setColorTo(const Value: TColor);
    procedure setImages(const Value: TImageList);
    procedure Notification(aComponent: TComponent; Operation: TOperation); override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    function MostRight: Integer;
    function MostBottom: Integer;
    procedure Paint; override;
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure ChangeButtonPlace(aControl: TControl);
    procedure WndProc(var Message: TMessage); override;
    procedure Loaded; override;
    //
    procedure InitMenu(Button: TDSToolButton); dynamic;
    procedure CancelMenu; dynamic;
    //
    procedure Notify(aChange: TSubjectChange); virtual;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure ArrangeButtons();
    function TrackMenu(aButton: TDSToolButton): Boolean; dynamic;
    procedure ClickButton(aButton: TDSToolButton); dynamic;
    procedure Attach(aObserver: TObserver);
    procedure Detach(aObserver: TObserver);
    function MinWidth: Integer;
    function MaxWidth: Integer;
    function MinHeigth: Integer;
  published
    property Align;
    property AutoSize;
    property Caption;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Font;
    property Vertical: Boolean read fVertical write setVertical default False;
    property Menu: TMainMenu read fMenu write setMenu;
    property Visible;
    property Color;
    property ColorTo: TColor read fColorTo write setColorTo;
    property Images: TImageList read fImages write setImages;
  end;//TDSToolBar
//------------------------------------------------------------------------------
procedure Register;
//------------------------------------------------------------------------------
implementation
//------------------------------------------------------------------------------
uses
  SysUtils, Math, Types, ActnList, Forms, CommCtrl, Dialogs;
//------------------------------------------------------------------------------
{$R DsToolBar.res}
//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('DS Menus', [TDSToolBar, TDSToolButton, TDsToolButtonGroup]);
end;
//------------------------------------------------------------------------------
{ TDSToolButtonActionLink }
//------------------------------------------------------------------------------
procedure TDSToolButtonActionLink.AssignClient(aClient: TObject);
begin
  inherited AssignClient(aClient);
  fClient := TDSToolButton(aClient); 
end;//AssignClient
//------------------------------------------------------------------------------
function TDSToolButtonActionLink.Execute(AComponent: TComponent): Boolean;
begin
  Result := inherited Execute(AComponent);
end;

function TDSToolButtonActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked and
            (fClient.Down = (Action as TCustomAction).Checked);
end;
//------------------------------------------------------------------------------
function TDSToolButtonActionLink.IsImageIndexLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked and
           (fClient.ImageIndex = (Action as TCustomAction).ImageIndex);
end;
//------------------------------------------------------------------------------
procedure TDSToolButtonActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then
    fClient.Down := Value;
end;
//------------------------------------------------------------------------------
procedure TDSToolButtonActionLink.SetImageIndex(Value: Integer);
begin
  if IsImageIndexLinked then
    fClient.ImageIndex := Value;
end;
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
{ TDSToolBar }
//------------------------------------------------------------------------------
constructor TDSToolBar.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  ControlStyle := [csAcceptsControls,
                   csCaptureMouse,
                   csClickEvents,
                   csDoubleClicks,
                   csOpaque
//                   csParentBackground
                   ];
  fStyler := TDsToolBarStyler.Create;
  fInsertControl := nil;
  Font.Name := 'Tahoma';
  fInOrderButtons := False;
  Width := 129;
  Height := 23;
  fVertical := False;
  fButtons := TList.Create;
  DragKind := dkDock;
//  DragMode := dmAutomatic;
  fObservers := TList.Create;
end;//Create
//------------------------------------------------------------------------------
destructor TDSToolBar.Destroy;
begin
  FreeAndNil(fObservers);
  FreeAndNil(fButtons);
  FreeAndNil(fStyler);
  inherited Destroy;
end;//Destroy
//------------------------------------------------------------------------------
procedure TDSToolBar.Loaded;
var
  i: Integer;

  function FindPos: Integer;
  var
    j: Integer;
  begin
    Result := -1;
    for j := 0 to fButtons.Count - 1 do
    begin
      if TControl(fButtons[j]).Top > Controls[i].Top then
      begin
        Result := j;
        Break;
      end
      else
      if TControl(fButtons[j]).Top = Controls[i].Top then
        if TControl(fButtons[j]).Left > Controls[i].Left then
        begin
          Result := j;
          Break;
        end;
    end;

    if Result = -1 then
      Result := fButtons.Count;
  end;
  
begin
  inherited;
  for i := 0 to ControlCount - 1 do
  begin
    fButtons.Insert(FindPos, Controls[i]);
  end;
  ArrangeButtons();
end;//Loaded
//------------------------------------------------------------------------------
procedure TDSToolBar.Paint;
begin
  if fInOrderButtons then
    Exit;
  if Vertical then
    fStyler.DrawGradientH(Canvas, Color, ColorTo, ClientRect)
  else
    fStyler.DrawGradientV(Canvas, Color, ColorTo, ClientRect);
end;//Paint
//------------------------------------------------------------------------------
procedure TDSToolBar.setVertical(const Value: Boolean);
var
  i: Integer;
  vTextOri: TTextOrientation;
  vPopupSide: TPopupSide;
begin
  fVertical := Value;
  if Value then
  begin
    vTextOri := toVertical;
    vPopupSide := popRight;
  end
  else
  begin
    vTextOri := toHorizontal;
    vPopupSide := popBottom;
  end;

  fInOrderButtons := True;
  try
  for i := 0 to fButtons.Count - 1 do
    if TControl(fButtons[i]) is TDSToolButton then
    begin
      TDSToolButton(fButtons[i]).TextOrientation := vTextOri;
      TDSToolButton(fButtons[i]).PopupSide := vPopupSide;
    end;
  finally
    fInOrderButtons := False;
  end;
  ArrangeButtons();
  Refresh;
end;
//------------------------------------------------------------------------------
procedure TDSToolBar.ChangeButtonPlace(aControl: TControl);
var
  i, vNewIndex, vOldIndex: Integer;
  vButton: TControl;
  vNewPos: Integer;

  function getButtonPos(aButton: TControl): Integer;
  begin
    if Vertical then
      Result := aButton.Top
    else
      Result := aButton.Left;
  end;

  function getButtonWidth(aButton: TControl): Integer;
  begin
    if Vertical then
      Result := aButton.Top + aButton.Height
    else
      Result := aButton.Left + aButton.Width;
  end;



begin
  if csLoading in ComponentState then
    Exit;

  vNewPos := getButtonPos(aControl);
  if vNewPos < 0 then
    vNewIndex := -1
  else
  begin
    vNewIndex :=  -1;
    for i := 0 to fButtons.Count - 1 do
    if TControl(fButtons[i]) <> aControl then
    begin
      vButton := TControl(fButtons[i]);
      if (vNewPos > getButtonPos(vButton)) and (vNewPos < getButtonWidth(vButton)) then
      begin
        vNewIndex := i;
        Break;
      end;
    end;

    if vNewIndex = -1 then
      Exit;
  end;

  vOldIndex := fButtons.IndexOf(aControl);
  if vOldIndex > vNewIndex then
    vNewIndex := vNewIndex + 1;

  if vOldIndex <> vNewIndex then
    fButtons.Move(vOldIndex, vNewIndex)
end;//ChangeButtonPlace
//------------------------------------------------------------------------------
procedure TDSToolBar.AlignControls(AControl: TControl; var Rect: TRect);
var
  i: Integer;
begin
  if not fInOrderButtons then
  begin
    if (fInsertControl = AControl) and (AControl <> nil) then
    begin
      if AControl is TDSToolButton then
        if Vertical then
          TDSToolButton(AControl).TextOrientation := toVertical;
      ArrangeButtons();
    end
    else
    begin
      if (aControl <> nil) and (fInsertControl = nil) then
      begin
        ChangeButtonPlace(aControl);
        if Vertical then
        begin
          for i := 0 to fButtons.Count - 1 do
            if (TControl(fButtons[i]) is TDSToolButton) then
              TDSToolButton(fButtons[i]).Width := AControl.Width;
        end
        else
        begin
          for i := 0 to fButtons.Count - 1 do
            if (TControl(fButtons[i]) is TDSToolButton) then
              TDSToolButton(fButtons[i]).Height := AControl.Height;
        end;
      end;
      ArrangeButtons();
    end;
    Refresh;
  end;
  if Showing then
    AdjustSize;
end;//AlignControls
//------------------------------------------------------------------------------
procedure TDSToolBar.ArrangeButtons();
var
  vPrev, vButton: TControl;
  vButtonHeight, vLeftBorder, vTopBorder: Integer;
  vBtnWidth: Integer;
  vNewPos: Integer;

  function getButtonWidth(aControl: TControl): Integer;
  begin
    if Vertical then
    begin
      if (aControl is TDSToolButton) then
        Result := TDSToolButton(aControl).NeededHeight
      else
        Result := aControl.Height;
    end
    else
    begin
      if (aControl is TDSToolButton) then
        Result := TDSToolButton(aControl).NeededWidth
      else
        Result := aControl.Width;
    end;
    if (Result = 0) then
      Result := 23;
  end;

  function getButtonsHeight: Integer;
  var
    i: Integer;
  begin
    Result := 0;
    if Vertical then
    begin
      for i := 0 to fButtons.Count - 1 do
        if TControl(fButtons[i]) is TDSToolButton then
          if TDSToolButton(fButtons[i]).NeededWidth > Result then
            Result := TDSToolButton(fButtons[i]).NeededWidth;
      if Result = 0 then
      begin
        for i := 0 to fButtons.Count - 1 do
            if TControl(fButtons[i]).Width > Result then
              Result := TControl(fButtons[i]).Width;
      end;
    end
    else
    begin
      // Find the highest needed height of a Tool button
      for i := 0 to fButtons.Count - 1 do
        if TControl(fButtons[i]) is TDSToolButton then
          if TDSToolButton(fButtons[i]).NeededHeight > Result then
            Result := TDSToolButton(fButtons[i]).NeededHeight;
      if Result = 0 then
      begin
        for i := 0 to fButtons.Count - 1 do
            if TControl(fButtons[i]).Height > Result then
              Result := TControl(fButtons[i]).Height;
      end;
    end;
    if vButtonHeight = 0 then
      vButtonHeight := 23;
  end;

  function getNextPos(aControl: TControl): Integer;
  begin
    if Vertical then
      Result := aControl.Top + aControl.Height
    else
      Result := aControl.Left + aControl.Width;
  end;

  procedure SetButtonBounds(aLeft, aTop, aWidth, aHeight: Integer);
  begin
    if Vertical then
      vButton.SetBounds(aTop, aLeft, aHeight, aWidth)
    else
      vButton.SetBounds(aLeft, aTop, aWidth, aHeight);
  end;

var
  i: Integer;

begin
  if fButtons.Count = 0 then
    Exit;
  if fInOrderButtons then
    Exit;
  DisableAlign;
  try
    fInOrderButtons := True;
    vLeftBorder   := 0;
    vTopBorder    := 0;
    vButton := TControl(fButtons[0]);
    vButtonHeight := getButtonsHeight;
    vBtnWidth := getButtonWidth(vButton);
    SetButtonBounds(vLeftBorder, vTopBorder, vBtnWidth, vButtonHeight);
    vPrev := vButton;
    for i := 1 to fButtons.Count - 1 do
    begin
      vButton := TControl(fButtons[i]);
      vBtnWidth := getButtonWidth(vButton);
      vNewPos := getNextPos(vPrev);
      SetButtonBounds(vNewPos, 0, vBtnWidth, vButtonHeight);
      vPrev := vButton;
    end;
  finally
    EnableAlign;
    fInOrderButtons := False;
  end;
end;//OrderButtons
//------------------------------------------------------------------------------
// Вика се при промяна на позицията, размера на контролата
function TDSToolBar.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
var
  vWidth, vHeight: Integer;
begin
  { Ситуации
   - Свива се контролата или се разширява - не се извършва нареждане на контролите вътре,
     NewWidth, NewHeight се различават от текущите
   - Мести се позицията на контролата - не се извършва нареждане на контролите вътре,
     NewWidth, NewHeight не се различават от текущите

  }
  if fInOrderButtons then
  begin
//    Result := False;
//    Exit;
  end;

  if NewWidth <> Width then
  begin
    ArrangeButtons();
    vWidth := MostRight;
    if vWidth > 0 then
      NewWidth := vWidth;
    vHeight := MostBottom;
    if vHeight > 0 then
      NewHeight := vHeight;
  end
  else
  if NewHeight <> Height then
  begin
    ArrangeButtons();
    vWidth := MostRight;
    if vWidth > 0 then
      NewWidth := vWidth;
    vHeight := MostBottom;
    if vHeight > 0 then
      NewHeight := vHeight;
  end
  else
  begin
    vWidth := MostRight;
    if vWidth > 0 then
      NewWidth := vWidth;
    vHeight := MostBottom;
    if vHeight > 0 then
      NewHeight := vHeight;
  end;

  Result := True;
end;//CanAutoSize
//------------------------------------------------------------------------------
function TDSToolBar.MostRight: Integer;
var
  i: Integer;
  vButton: TControl;
begin
  Result := 0;
  for i := 0 to fButtons.Count - 1 do
  begin
    vButton := TControl(fButtons[i]);
    if vButton.Left + vButton.Width > Result then
      Result := vButton.Left + vButton.Width;
  end;
end;//MostRight
//------------------------------------------------------------------------------
function TDSToolBar.MostBottom: Integer;
var
  i: Integer;
  vButton: TControl;
begin
  Result := 0;
  for i := 0 to fButtons.Count - 1 do
  begin
    vButton := TControl(fButtons[i]);
    if vButton.Top + vButton.Height > Result then
      Result := vButton.Top + vButton.Height;
  end;
end;//MostBottom
//------------------------------------------------------------------------------
procedure TDSToolBar.CMVisibleChanged(var Message: TMessage); 
begin
  inherited;
  Notify(cngVisible);
end;//
//------------------------------------------------------------------------------
procedure TDSToolBar.CMControlListChange(var Message: TCMControlListChange);
begin
  inherited;
  if csLoading in ComponentState then
    Exit;
  if csDestroying in ComponentState then
    Exit;
  if Message.Inserting then
  begin
    fButtons.Add(Message.Control);
    fInsertControl := Message.Control;
  end
  else
  begin
    fButtons.Remove(Message.Control);
  end;
end;
//------------------------------------------------------------------------------
procedure TDSToolBar.CMControlChange(var Message: TCMControlChange);
begin
  if csLoading in ComponentState then
    Exit;
  if csDestroying in ComponentState then
    Exit;

  if Message.Inserting then
    fInsertControl := nil;
end;
//------------------------------------------------------------------------------
procedure TDSToolBar.CMHitTest(var Message: TCMHitTest);
begin
  Message.Result := -1;
end;//CMHitTest
//------------------------------------------------------------------------------
procedure TDSToolBar.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 0;
end;
//------------------------------------------------------------------------------
procedure TDSToolBar.setMenu(const Value: TMainMenu);
var
  i: Integer;
  vButton: TDSToolButton;
begin
  if Value = fMenu then
    Exit;

  if csAcceptsControls in ControlStyle then
  begin
    ControlStyle := ControlStyle - [csAcceptsControls] + [csMenuEvents];
    RecreateWnd;
  end;           

  while ControlCount > 0 do
  begin
    Controls[0].Free;
  end;

  fMenu := Value;

  if fMenu = nil then
    Exit;

  fMenu.FreeNotification(Self);

  fInOrderButtons := True;

  for I := 0 to fMenu.Items.Count - 1 do
  begin
    vButton := TDsToolButton.Create(Self);
    try
      vButton.Parent := Self;
      vButton.ShowImage := False;
      vButton.MenuItem := FMenu.Items[I];
    except
      Free;
      raise;
    end;
  end;
  fInOrderButtons := False;
  AutoSize := True;
//  if csDesigning in ComponentState then
//    GetParentForm(Self).Designer.Modified;
  AdjustSize;
  ArrangeButtons();
  Refresh;
end;//setMenu
//------------------------------------------------------------------------------
procedure TDSToolBar.WMSysCommand(var Message: TWMSysCommand);
var
  vButton: TDSToolButton;
begin
  if not fInMenuLoop and Enabled and Showing and (Menu <> nil) then
  begin
    if ((Message.CmdType and $FFF0) = SC_KEYMENU) and (Message.Key <> VK_SPACE) and
       (Message.Key <> Word('-')) and (GetCapture = 0) then
    begin
      if Message.Key = 0 then
        vButton := nil
      else
        vButton := FindButtonFromAccel(Message.Key);

      if (vButton <> nil) or (Message.Key = 0) then
      begin
        TrackMenu(vButton);
        Message.Result := 1;
      end;
    end;
  end;
end;//WMSysCommand
//------------------------------------------------------------------------------
procedure TDSToolBar.WMGetDlgCode(var Message: TMessage);
begin
  if fInMenuLoop then
    Message.Result := DLGC_WANTARROWS;
end;//WMGetDlgCode
//------------------------------------------------------------------------------
procedure TDSToolBar.WMKeyDown(var Message: TWMKeyDown);
var
  i, vIdx: Integer;
  vButton: TDSToolButton;
begin
  if fInMenuLoop then
  begin
    case Message.CharCode of
      VK_RETURN, VK_DOWN:
        begin
          vButton := nil;
          for i := 0 to fButtons.Count-1 do
            if TDSToolButton(fButtons[i]).Hot then
            begin
              vButton := TDSToolButton(fButtons[i]);
              Break;
            end;
          if vButton <> nil then
            ClickButton(vButton);

          { Prevent default processing }
          if Message.CharCode = VK_DOWN then
            Exit;
        end;
      VK_LEFT:
        begin
          vIdx := -1;
          for i := 0 to fButtons.Count-1 do
            if TDSToolButton(fButtons[i]).Hot then
            begin
              TDSToolButton(fButtons[i]).Hot := False;
              vIdx := i;
              Break;
            end;

          if vIdx = 0 then
            vIdx := fButtons.Count - 1
          else
            vIdx := vIdx - 1;

          TDSToolButton(fButtons[vIdx]).Hot := True;
          Exit;// Prevent default processing
        end;
      VK_RIGHT:
        begin
          vIdx := -1;
          for i := 0 to fButtons.Count-1 do
            if TDSToolButton(fButtons[i]).Hot then
            begin
              TDSToolButton(fButtons[i]).Hot := False;
              vIdx := i;
              Break;
            end;

          if vIdx = fButtons.Count - 1 then
            vIdx := 0
          else
            vIdx := vIdx + 1;

          TDSToolButton(fButtons[vIdx]).Hot := True;  

          Exit;// Prevent default processing
        end;
      VK_ESCAPE: CancelMenu;
    end;
  end;
  inherited;
end;
//------------------------------------------------------------------------------
function TDSToolBar.FindButtonFromAccel(Accel: Word): TDSToolButton;
var
  i: Integer;
begin
  for i := 0 to fButtons.Count - 1 do
    if TControl(fButtons[I]) is TDSToolButton then
    begin
      Result := TDSToolButton(fButtons[I]);
      if Result.Visible and Result.Enabled and IsAccel(Accel, Result.Caption) then
        Exit;
    end;
  Result := nil;
end;//FindButtonFromAccel
//------------------------------------------------------------------------------
function TDSToolBar.TrackMenu(aButton: TDSToolButton): Boolean;
begin
  { Already in menu loop - click button to drop-down menu }
  if fInMenuLoop then
  begin
    if aButton <> nil then
    begin
      ClickButton(aButton);
      Result := True;
    end
    else
      Result := False;
    Exit;
  end;

  InitMenu(aButton);
  try
    fInMenuLoop := True;
    repeat
      Application.HandleMessage;
      if Application.Terminated then
        fInMenuLoop := False;
    until not fInMenuLoop;

  finally
    CancelMenu;
  end;
  Result := fMenuResult;
end;
//------------------------------------------------------------------------------
procedure TDSToolBar.ClickButton(aButton: TDSToolButton);
var
  P: TPoint;
begin
  FCaptureChangeCancels := False;
  P := aButton.ClientToScreen(Point(1, 1));
  PostMessage(Handle, WM_LBUTTONDOWN, MK_LBUTTON, Longint(PointToSmallPoint(ScreenToClient(P))));
end;
//------------------------------------------------------------------------------
var
  ToolMenuHook: HHOOK;
  InitDone: Boolean = False;
  MenuToolBar, MenuToolBar2: TDSToolBar;
  vMenuButton: TDSToolButton;
  LastMenuItem: TMenuItem;
  LastMousePos: TPoint;
  StillModal: Boolean;

function ToolMenuGetMsgHook(Code: Integer; WParam: Longint; var Msg: TMsg): Longint; stdcall;
const
  RightArrowKey: array[Boolean] of Word = (VK_LEFT, VK_RIGHT);
  LeftArrowKey: array[Boolean] of Word = (VK_RIGHT, VK_LEFT);
var
  P: TPoint;
  Target: TControl;
  Item: Integer;
  FindKind: TFindItemKind;
  ParentMenu: TMenu;

  function FindButton(aForward: Boolean): TDSToolButton;
  var
    vIdx: Integer;
  begin
    vIdx := MenuToolBar.fButtons.IndexOf(vMenuButton);
    if aForward then
    begin
      if vIdx < MenuToolBar.fButtons.Count - 1 then
        vIdx := vIdx + 1
      else
        vIdx := 0;
    end
    else
    begin
      if vIdx > 0 then
        vIdx := vIdx - 1
      else
        vIdx := MenuToolBar.fButtons.Count - 1;
    end;
    Result := TDSToolButton(MenuToolBar.fButtons[vIdx]);
  end;

begin
  if LastMenuItem <> nil then
  begin
    ParentMenu := LastMenuItem.GetParentMenu;
    if ParentMenu <> nil then
    begin
      if ParentMenu.IsRightToLeft then
        if Msg.WParam = VK_LEFT then
          Msg.WParam := VK_RIGHT
        else if Msg.WParam = VK_RIGHT then
          Msg.WParam := VK_LEFT;
    end;
  end;
  Result := CallNextHookEx(ToolMenuHook, Code, WParam, Longint(@Msg));
  if Result <> 0 then Exit;
  if (Code = MSGF_MENU) then
  begin
    Target := nil;
    if not InitDone then
    begin
      InitDone := True;
      PostMessage(Msg.Hwnd, WM_KEYDOWN, VK_DOWN, 0);
    end;
    case Msg.Message of
      WM_MENUSELECT:
        begin
          if (HiWord(Msg.WParam) = $FFFF) and (Msg.LParam = 0) then
          begin
            if not StillModal then
              MenuToolBar.CancelMenu;
            Exit;
          end
          else
            StillModal := False;
          FindKind := fkCommand;
          if HiWord(Msg.WParam) and MF_POPUP <> 0 then FindKind := fkHandle;
            if FindKind = fkHandle then
              Item := GetSubMenu(Msg.LParam, LoWord(Msg.WParam))
            else
              Item := LoWord(Msg.WParam);
            LastMenuItem := vMenuButton.vDropDown.FindItem(Item, FindKind);
        end;
      WM_SYSKEYDOWN:
        if Msg.WParam = VK_MENU then
        begin
          MenuToolBar.CancelMenu;
          Exit;
        end;
      WM_KEYDOWN:
        if Msg.WParam = VK_RETURN then
          MenuToolBar.FMenuResult := True
        else if Msg.WParam = VK_ESCAPE then
          StillModal := True
        else if LastMenuItem <> nil then
        begin
          if (Msg.WParam = VK_RIGHT) and (LastMenuItem.Count = 0) then
            Target := FindButton(True)
          else if (Msg.WParam = VK_LEFT) and (LastMenuItem.GetParentComponent is TPopupMenu) then
            Target := FindButton(False)
          else
            Target := nil;
          if Target <> nil then
            P := Target.ClientToScreen(Point(0,0));
        end;
      WM_MOUSEMOVE:
        begin
          P := Msg.pt;
          if (P.X <> LastMousePos.X) or (P.Y <> LastMousePos.Y) then
          begin
            Target := FindDragTarget(P, False);
            LastMousePos := P;
          end;
        end;
    end;
    if (Target <> nil) and (Target is TDSToolButton) then
    begin
      with TDSToolButton(Target) do
        if (TDSToolButton(Target) <> vMenuButton) {and Grouped} and (Parent <> nil) and
          Parent.HandleAllocated then
        begin
          StillModal := True;
          MenuToolBar.FCaptureChangeCancels := False;
          MenuToolBar.ClickButton(TDSToolButton(Target));
//          MenuToolBar.ClickButton(TDSToolButton(Target));
        end;
    end;
  end;
end;

procedure InitToolMenuHooks;
begin
  StillModal := False;
  GetCursorPos(LastMousePos);
  if ToolMenuHook = 0 then
    ToolMenuHook := SetWindowsHookEx(WH_MSGFILTER, @ToolMenuGetMsgHook, 0, GetCurrentThreadID);
end;

procedure ReleaseToolMenuHooks;
begin
  if ToolMenuHook <> 0 then UnhookWindowsHookEx(ToolMenuHook);
  ToolMenuHook := 0;
  LastMenuItem := nil;
  MenuToolBar := nil;
  vMenuButton := nil;
  InitDone := False;
end;

var
  vToolMenuKeyHook: HHOOK;

procedure ReleaseToolMenuKeyHooks; forward;

function ToolMenuKeyMsgHook(Code: Integer; WParam: Longint; var Msg: TMsg): Longint; stdcall;
begin
  if (Code = HC_ACTION) then
  begin
    if Msg.Message = CM_DEACTIVATE then
      MenuToolBar2.CancelMenu
    else
    if Msg.message = WM_COMMAND then
      ReleaseToolMenuKeyHooks
    else
    if (ToolMenuHook = 0) and
       ((Msg.Message = WM_CHAR) or (Msg.Message = WM_KEYDOWN) or (Msg.Message = WM_KEYUP) or
        (Msg.Message = WM_SYSKEYDOWN) or (Msg.Message = WM_SYSKEYUP)) then
      Msg.hwnd := MenuToolBar2.Handle;
  end;
  Result := CallNextHookEx(vToolMenuKeyHook, Code, WParam, Longint(@Msg))
end;

procedure InitToolMenuKeyHooks;
begin
  if vToolMenuKeyHook = 0 then
    vToolMenuKeyHook := SetWindowsHookEx(WH_GETMESSAGE, @ToolMenuKeyMsgHook, 0, GetCurrentThreadID);
end;

procedure ReleaseToolMenuKeyHooks;
begin
  if vToolMenuKeyHook <> 0 then
    UnhookWindowsHookEx(vToolMenuKeyHook);
  vToolMenuKeyHook := 0;
  MenuToolBar2 := nil;
end;

procedure TDSToolBar.InitMenu(Button: TDSToolButton);
begin
  Perform(TB_SETANCHORHIGHLIGHT, 1, 0);
  MenuToolBar2 := Self;
  MouseCapture := True;
  InitToolMenuKeyHooks;
  if Button <> nil then
  begin
    Button.Hot := True;
    ClickButton(Button);
  end
  else
    TDSToolButton(fButtons[0]).Hot := True;

  if Button = nil then
    FCaptureChangeCancels := True;
end;
//------------------------------------------------------------------------------
procedure TDSToolBar.CancelMenu;
var
  i: Integer;
begin
  if fInMenuLoop then
  begin
    ReleaseToolMenuKeyHooks;
    MouseCapture := False;
    Perform(TB_SETANCHORHIGHLIGHT, 0, 0);
  end;
  FInMenuLoop := False;
  FCaptureChangeCancels := False;
  for i := 0 to fButtons.Count - 1 do
    if TControl(fButtons[i]) is TDSToolButton then
      TDSToolButton(fButtons[i]).Hot := False;
end;
//------------------------------------------------------------------------------
procedure TDSToolBar.WndProc(var Message: TMessage);
var
  vControl: TControl;
  vCapControl: TControl;

  function IsToolButtonMouseMsg(var Message: TWMMouse): Boolean;
  begin
    if GetCapture = Handle then
    begin
      vCapControl := GetCaptureControl;
      if (vCapControl <> nil) and (vCapControl.Parent <> Self) then
        vCapControl := nil;
    end
    else
      vCapControl := nil;
    vControl := ControlAtPos(SmallPointToPoint(Message.Pos), True);
    Result := (vControl <> nil) and (vControl is TDSToolButton) and not vControl.Dragging;
  end;
var
  vTempDragMode: TDragMode;
begin
  if not (csDesigning in ComponentState) then
  begin
    case Message.Msg of
      WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
      if IsToolButtonMouseMsg(TWMMouse(Message)) then
        begin
          if fInMenuLoop and MouseCapture then
            TDSToolButton(vControl).MouseCapture := True;
          vTempDragMode := DragMode;
          DragMode := dmManual;
          inherited WndProc(Message);
          DragMode := vTempDragMode;
          Exit;
        end;
    end;
  end;
  inherited WndProc(Message);
end;//WndProc
//------------------------------------------------------------------------------
procedure TDSToolBar.Notify(aChange: TSubjectChange);
var
  i: Integer;
begin
  for i := 0 to fObservers.Count - 1 do
    TObserver(fObservers[i]).Update(Self, cngVisible);
end;
//------------------------------------------------------------------------------
procedure TDSToolBar.Attach(aObserver: TObserver);
begin
  if fObservers.IndexOf(aObserver) < 0 then
  begin
    fObservers.Add(aObserver);
    Notify(cngAttach);
  end;
end;//Attach
//------------------------------------------------------------------------------
procedure TDSToolBar.Detach(aObserver: TObserver);
begin
  if fObservers <> nil then
    if aObserver <> nil then
    begin
      fObservers.Remove(aObserver);
      Notify(cngDetach);
    end;
end;//Detach
//------------------------------------------------------------------------------
function TDSToolBar.MaxWidth: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to fButtons.Count - 1 do
    if TControl(fButtons[i]) is TDSToolButton then
    begin
      Result := Result + TDSToolButton(fButtons[i]).NeededWidth;
    end
    else
    begin
      Result := Result + TControl(fButtons[i]).Width;
    end;
    
end;
//------------------------------------------------------------------------------
function TDSToolBar.MinWidth: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to fButtons.Count - 1 do
    if TControl(fButtons[i]) is TDSToolButton then
    begin
      if TDSToolButton(fButtons[i]).NeededWidth > Result then
        Result := TDSToolButton(fButtons[i]).NeededWidth;
    end
    else
    begin
      if TControl(fButtons[i]).Width > Result then
        Result := TControl(fButtons[i]).Width;
    end;
end;
//------------------------------------------------------------------------------
function TDSToolBar.MinHeigth: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to fButtons.Count - 1 do
    if TControl(fButtons[i]) is TDSToolButton then
    begin
      if TDSToolButton(fButtons[i]).NeededHeight > Result then
        Result := TDSToolButton(fButtons[i]).NeededHeight;
    end
    else
    begin
      if TControl(fButtons[i]).Height > Result then
        Result := TControl(fButtons[i]).Height;
    end;
end;
//------------------------------------------------------------------------------
procedure TDSToolBar.Notification(aComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(aComponent, Operation);
  if Operation = opRemove then
  begin
    if aComponent = fMenu then
      Menu := nil;
  end;
end;
//------------------------------------------------------------------------------
procedure TDSToolBar.setColorTo(const Value: TColor);
begin
  fColorTo := Value;
  Refresh;
end;
//------------------------------------------------------------------------------
procedure TDSToolBar.setImages(const Value: TImageList);
var
  vOldImages: TImageList;
  i: Integer;
begin
  vOldImages := fImages;
  fImages := Value;

  for i := 0 to fButtons.Count - 1 do
    if (TControl(fButtons[i]) is TDSToolButton) then
      if TDSToolButton(fButtons[i]).Images = vOldImages then
        TDSToolButton(fButtons[i]).Images := Value;
end;
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
{ TDSToolButton }
//------------------------------------------------------------------------------
constructor TDSToolButton.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fStyler := TDSToolBarStyler.Create;
  fStyler.Corner := 1;
  fTextOrientation := toHorizontal;
  fDown := False;
  fFlat := True;
  fCheck := False;
  fDropped := False;
  fShowImage := True;
  fShowText := False;
  TextToImage := tipAfterImage;
  vDropDown := nil;
  PopupSide := popBottom;
  fGroup := nil;
end;
//------------------------------------------------------------------------------
destructor TDSToolButton.Destroy;
begin
  FreeAndNil(fStyler);
  inherited Destroy;
end;
//------------------------------------------------------------------------------
function TDSToolButton.CreatePopupFromMenuItem: TPopupMenu;
var
  i: Integer;
  vParentMenu: TMenu;
  Menu: TMenu;
  Item: TMenuItem;
begin
  MenuItem.Click;
//  ClearTempMenu;
  Result := TPopupMenu.Create(Self);
  vParentMenu := MenuItem.GetParentMenu;
  if vParentMenu <> nil then
    Result.BiDiMode := vParentMenu.BiDiMode;
  Result.HelpContext := MenuItem.HelpContext;
  Result.TrackButton := tbLeftButton;
  Menu := MenuItem.GetParentMenu;
  if Menu <> nil then
    Result.Images := Menu.Images;
//  FButtonMenu := Button.MenuItem;
  for I := MenuItem.Count - 1 downto 0 do
  begin
    Item := MenuItem.Items[I];
    MenuItem.Delete(I);
    Result.Items.Insert(0, Item);
  end;
end;//CreatePopupFromMenuItem
//------------------------------------------------------------------------------
procedure TDSToolButton.TempMenuToTMenuItem(vTempMenu: TPopupMenu);
var
  I: Integer;
  Item: TMenuItem;
begin
  if {(fButtonMenu <> nil) and (FMenuButton <> nil) and
    (FMenuButton.MenuItem <> nil) and }(vTempMenu <> nil) then
  begin
    for I := vTempMenu.Items.Count - 1 downto 0 do
    begin
      Item := vTempMenu.Items[I];
      vTempMenu.Items.Delete(I);
      MenuItem.Insert(0, Item);
    end;
    vMenuButton := nil;
  end;
end;//TempMenuToTMenuItem
//------------------------------------------------------------------------------
procedure TDSToolButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Msg: TMsg;
  vPt: TPoint;
  vPopupPos: TPoint;
begin
  if not (Button = mbLeft) then
    Exit;

  if Check then
    fOldDown := Down;

  Down := True;

  inherited;

  if MenuItem <> nil then
  begin
    if not fDropped then
    begin
      MenuToolBar := TDSToolBar(Self.Parent);

      TempMenuToTMenuItem(vDropDown);
      vDropDown.Free;

      vMenuButton := Self;
      vDropDown := CreatePopupFromMenuItem;
      vDropDown.PopupComponent := Self;
      fDropped := True;
      InitToolMenuHooks;
                                                             
      case PopupSide of
        popLeft:  vPopupPos := Point(0, 0);
        popTop:   vPopupPos := Point(0, - GetSystemMetrics(SM_CYMENU) * vDropDown.Items.Count);
        popRight: vPopupPos := Point(Width, 0);
      else
        vPopupPos := Point(0, Height);
      end;
      
      with ClientToScreen(vPopupPos) do
        vDropDown.Popup(X, Y);

      ReleaseToolMenuHooks;

      Down := False;
      Msg.message := 0;
      if PeekMessage(Msg, Parent.Handle, WM_LBUTTONDOWN, WM_LBUTTONDOWN, PM_REMOVE) then
      begin
        vPt := ScreenToClient(Msg.pt);
        if not PtInRect(ClientRect, vPt) then
        begin
          PostMessage(Parent.Handle, Msg.message, Msg.wParam, Msg.lParam);
          Hot := False;
        end;
      end
      else
        Hot := False;
      Refresh;
    end;
    fDropped := False;
  end
  else
  if DropDownMenu <> nil then
    if (not DropButton) or (DropButton and (X >= Width - 12)) then
    begin
      if not fDropped then
      begin
        DropDownMenu.PopupComponent := Self;
        fDropped := True;
        with ClientToScreen(Point(0, Height)) do
          DropDownMenu.Popup(X, Y);
        Down := False;
        Msg.message := 0;
        if PeekMessage(Msg, Parent.Handle, WM_LBUTTONDOWN, WM_LBUTTONDOWN, PM_REMOVE) then
        begin
          vPt := ScreenToClient(Msg.pt);
          if not PtInRect(ClientRect, vPt) then
          begin
            PostMessage(Parent.Handle, Msg.message, Msg.wParam, Msg.lParam);
            Hot := False;
          end;
        end
        else
          Hot := False;
        Refresh;
      end;
      fDropped := False;
    end;
end;
//------------------------------------------------------------------------------
procedure TDSToolButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if MouseCapture then
  begin
    if PtInRect(ClientRect, Point(X, Y)) then
    begin
      if not Down then
        Down := True;
    end
    else
      if Down then
        if Check then
        begin
          if not fOldDown then
            Down := False;
        end
        else
          Down := False;
  end
  else
  begin
    if PtInRect(ClientRect, Point(X, Y)) then
      Hot := True
  end;
end;
//------------------------------------------------------------------------------
procedure TDSToolButton.WMLButtonUp(var Message: TWMLButtonUp);
begin
  if PtInRect(ClientRect, Point(Message.XPos, Message.YPos)) then
  begin
    if Check then
    begin
      if Down <> not fOldDown then
        Down := not fOldDown
    end
    else
    if Down then
      Down := False;
  end
  else
    if Check then
      Down := fOldDown
    else
    if Down then
      Down := False;

  if Check and (Down <> fOldDown) then
    if Assigned(fOnCheck) then
      fOnCheck(Self);
      
  inherited;
end;
//------------------------------------------------------------------------------
procedure TDSToolButton.CMMouseLeave(var Message: TMessage);
begin
  if Hot then
    Hot := False;
end;
//------------------------------------------------------------------------------
procedure TDSToolButton.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 0;
end;
//------------------------------------------------------------------------------
procedure TDSToolButton.Notification(aComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if aComponent = Images then
      Images := nil
    else
    if aComponent = MenuItem then
      MenuItem := nil
    else
    if aComponent = DropDownMenu then
      DropDownMenu := nil;
  end;
end;//Notification
//------------------------------------------------------------------------------
procedure TDSToolButton.Paint;
var
  vRect: TRect;
  Bmp: TBitmap;
  vTextWidth, vTextHeight: Integer;

  function CreateVerticalFont: HFONT;
  var
    LogFont: TLogFont;
  begin
    LogFont.lfHeight := Font.Height;
    LogFont.lfWidth := 0; { have font mapper choose }
    LogFont.lfEscapement := -900; { only straight fonts }
    LogFont.lfOrientation := 30; { no rotation }
    if fsBold in Font.Style then
      LogFont.lfWeight := FW_BOLD
    else
      LogFont.lfWeight := FW_NORMAL;
    LogFont.lfItalic := Byte(fsItalic in Font.Style);
    LogFont.lfUnderline := Byte(fsUnderline in Font.Style);
    LogFont.lfStrikeOut := Byte(fsStrikeOut in Font.Style);
    LogFont.lfCharSet := Byte(Font.Charset);
    if AnsiCompareText(Font.Name, 'Default') = 0 then  // do not localize
      StrPCopy(LogFont.lfFaceName, DefFontData.Name)
    else
      StrPCopy(LogFont.lfFaceName, Font.Name);
    LogFont.lfQuality := DEFAULT_QUALITY;
    { Everything else as default }
    LogFont.lfOutPrecision := OUT_DEFAULT_PRECIS;
    LogFont.lfClipPrecision := CLIP_DEFAULT_PRECIS;
    case Font.Pitch of
      fpVariable: LogFont.lfPitchAndFamily := VARIABLE_PITCH;
      fpFixed: LogFont.lfPitchAndFamily := FIXED_PITCH;
    else
      LogFont.lfPitchAndFamily := DEFAULT_PITCH;
    end;
    Result := CreateFontIndirect(LogFont);
  end;

var
  vFontHandle: HFONT;
begin
  if fStyler = nil then
    Exit;
  vRect := ClientRect;
  Canvas.Brush.Color := Color;
  if Down then
    fStyler.DrawEdge(Canvas, vRect, False)
  else
  if (not Flat) or Hot or (csDesigning in ComponentState) then
    fStyler.DrawEdge(Canvas, vRect, True);


  if DropButton then
  begin
    vRect.Left := Width - 12;
    if Down then
      fStyler.DrawEdge(Canvas, vRect, False)
    else
    if (not Flat) or Hot or (csDesigning in ComponentState) then
      fStyler.DrawEdge(Canvas, vRect, True);

    Bmp := TBitmap.Create;
    try
      Bmp.LoadFromResourceName(HInstance, bmDropDown);
      Bmp.Transparent := True;
      if Down then
        Canvas.Draw(Width - 8, (Height - 5) div 2 + 1, Bmp)
      else
        Canvas.Draw(Width - 9, (Height - 5) div 2, Bmp);
    finally
      Bmp.Free;
    end;
  end;
  
  if ShowImage and (Images <> nil) then
    if (ImageIndex < Images.Count) and (ImageIndex >= 0) then
      if Down then
        Images.Draw(Canvas, fImageRect.Left + 1, fImageRect.Top + 1, ImageIndex, Enabled)
      else
        Images.Draw(Canvas, fImageRect.Left, fImageRect.Top, ImageIndex, Enabled);
        
  if ShowText then
  begin
    vRect := fTextRect;
    Canvas.Brush.Style := bsClear;

    vFontHandle := 0;
    if TextOrientation = toVertical then
      vFontHandle := SelectObject(Canvas.Handle, CreateVerticalFont);

    if TextOrientation = toVertical then
    begin
      vTextHeight := vRect.Top + (vRect.Bottom - vRect.Top - Canvas.TextWidth(Caption)) div 2;
      vTextWidth := vRect.Left + (vRect.Right - vRect.Left - Canvas.TextHeight(Caption)) div 2 + Canvas.TextHeight(Caption);
    end
    else
    begin
      vTextWidth := vRect.Left + ((vRect.Right - vRect.Left - Canvas.TextWidth(Caption)) div 2);
      vTextHeight := vRect.Top + ((vRect.Bottom - vRect.Top - Canvas.TextHeight(Caption)) div 2);
    end;
    if Down then
    begin
      vTextWidth := vTextWidth + 1;
      vTextHeight := vTextHeight + 1;
    end;
    TextOut(Canvas.Handle, vTextWidth, vTextHeight, PAnsiChar(Caption), Length(Caption));
    if TextOrientation = toVertical then
    begin
      vFontHandle := SelectObject(Canvas.Handle, vFontHandle);
      DeleteObject(vFontHandle);
    end;
  end;

end;//Paint
//------------------------------------------------------------------------------
procedure TDSToolButton.setDown(const Value: Boolean);
var
  vValue: Boolean;
begin
  vValue := Value;
  if fGroup <> nil then
    fGroup.CanCheck(Self, vValue);
  fDown := vValue;
  Refresh;
  if Group <> nil then
    fGroup.ButtonCheck(Self, fDown);
end;
//------------------------------------------------------------------------------
procedure TDSToolButton.setFlat(const Value: Boolean);
begin
  fFlat := Value;
  Refresh;
end;
//------------------------------------------------------------------------------
procedure TDSToolButton.setHot(const Value: Boolean);
begin
  fHot := Value;
  Refresh;
end;
//------------------------------------------------------------------------------
procedure TDSToolButton.setCheck(const Value: Boolean);
begin
  fCheck := Value;
  if not Check then
  begin
    Down := False;
    Refresh;
  end;
end;
//------------------------------------------------------------------------------
procedure TDSToolButton.setImages(const Value: TCustomImageList);
begin
  fImages := Value;
  if fImages <> nil then
    fImages.FreeNotification(Self);
  OrderTextAndImage;
end;
//------------------------------------------------------------------------------
procedure TDSToolButton.setImageIndex(const Value: TImageIndex);
begin
  fImageIndex := Value;
  Refresh;
end;
//------------------------------------------------------------------------------
procedure TDSToolButton.setMenuItem(const Value: TMenuItem);
begin
  if Value <> fMenuItem then
  begin
    fMenuItem := Value;
    if Value <> nil then
    begin
      Value.FreeNotification(Self);
      Caption := Value.Caption;
      ShowText := True;
      Hint := Value.Hint;
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure TDSToolButton.setDropDownMenu(const Value: TPopupMenu);
begin
  if Value <> fDropdownMenu then
  begin
    fDropdownMenu := Value;
    if Value <> nil then
      Value.FreeNotification(Self);
  end;
end;//setDropDownMenu
//------------------------------------------------------------------------------
procedure TDSToolButton.setDropButton(const Value: Boolean);
begin
  fDropButton := Value;
  if fDropButton then
    Width := Width + 11;
  Refresh;
end;//setDropButton
//------------------------------------------------------------------------------
procedure TDSToolButton.setShowText(const Value: Boolean);
begin
  fShowText := Value;
  Width := NeededWidth;
  Refresh;
end;//setShowText
//------------------------------------------------------------------------------
procedure TDSToolButton.setShowImage(const Value: Boolean);
begin
  fShowImage := Value;
  Refresh;
end;
//------------------------------------------------------------------------------
procedure TDSToolButton.setTextToImage(const Value: TTextToImagePosition);
begin
  fTextToImage := Value;
  OrderTextAndImage;
end;
//------------------------------------------------------------------------------
procedure TDSToolButton.OrderTextAndImage;
begin
  if ShowText and (ShowImage and (Images <> nil)) then
  begin
    case TextToImage of
    tipAfterImage:
      begin
        fImageRect.Left := 3;
        fImageRect.Top := (Height - Images.Height) div 2;
        fImageRect.Right := fImageRect.Left + Images.Width;
        fImageRect.Bottom := fImageRect.Top + Images.Height;

        fTextRect.Left := fImageRect.Right + 1;
        fTextRect.Top  := 1;
        fTextRect.Right := Width - 1;
        fTextRect.Bottom := Height - 1;
      end;
    tipBeforeImage:
      begin
        fImageRect.Left := Width - Images.Width - 6;
        fImageRect.Top := (Height - Images.Height) div 2;
        fImageRect.Right := fImageRect.Left + Images.Width;
        fImageRect.Bottom := fImageRect.Top + Images.Height -1;
        
        fTextRect.Left := 1;
        fTextRect.Top  := 1;
        fTextRect.Right := fImageRect.Left - 1;
        fTextRect.Bottom := Height - 1;
      end;
    tipAboveImage:
      begin
        fImageRect.Left := (Width - Images.Width) div 2;
        fImageRect.Top := Height - Images.Height - 3;
        fImageRect.Right := fImageRect.Left + Images.Width;
        fImageRect.Bottom := fImageRect.Top + Images.Height -1;

        fTextRect.Left := 1;
        fTextRect.Top  := 1;
        fTextRect.Right := Width - 1;
        fTextRect.Bottom := fImageRect.Top - 1;
      end;
    tipBelowImage:
      begin
        fImageRect.Left := (Width - Images.Width) div 2;
        fImageRect.Top := 2;
        fImageRect.Right := fImageRect.Left + Images.Width;
        fImageRect.Bottom := fImageRect.Top + Images.Height -1;

        fTextRect.Left := 1;
        fTextRect.Top  := fImageRect.Bottom + 1;
        fTextRect.Right := Width - 1;
        fTextRect.Bottom := Height - 1;
      end;
    tipOverlayImage:
      begin
        fImageRect.Left := (Width - Images.Width) div 2;
        fImageRect.Top := (Height - Images.Height) div 2;
        fImageRect.Right := fImageRect.Left + Images.Width;
        fImageRect.Bottom := fImageRect.Top + Images.Height -1;

        fTextRect.Left := 1;
        fTextRect.Top  := 1;
        fTextRect.Right := Width - 1;
        fTextRect.Bottom := Height - 1;
      end;
    end;
  end
  else
  if ShowText then
  begin
    fTextRect.Left := 1;
    fTextRect.Top := 1;
    fTextRect.Right := Width - 1;
    fTextRect.Bottom := Height - 1;
  end
  else
  if ShowImage and (Images <> nil) then
  begin
    fImageRect.Left := (Width - Images.Width) div 2;
    fImageRect.Top := (Height - Images.Height) div 2;
    fImageRect.Right := fImageRect.Left + Images.Width;
    fImageRect.Bottom := fImageRect.Top + Images.Height -1;
  end;
  RequestAlign;
  Refresh;
end;//OrderTextAndImage
//------------------------------------------------------------------------------
procedure TDSToolButton.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Canvas.Font := Font;
  OrderTextAndImage;
end;
//------------------------------------------------------------------------------
procedure TDSToolButton.CMHitTest(var Message: TCMHitTest);
begin
  Message.Result := 1;
end;//CMHitTest
//------------------------------------------------------------------------------
procedure TDSToolButton.CMTextChanged(var Message: TMessage);
begin
  inherited;
  OrderTextAndImage;
end;
//------------------------------------------------------------------------------
function TDSToolButton.NeededWidth: Integer;
var
  vTextWidth: Integer;
  vTextOffset: Integer;
begin
  if ShowText then
  begin
    if TextOrientation = toHorizontal then
    begin
      vTextWidth := Canvas.TextWidth(Caption);
      vTextOffset := 14;
    end
    else
    begin
      vTextWidth := Canvas.TextHeight(Caption);
      vTextOffset := 6;
    end;
      
    if ShowImage and (Images <> nil) then
    begin
      if TextToImage in [tipAboveImage, tipBelowImage, tipOverlayImage] then
        Result := Max(vTextWidth + vTextOffset, Images.Width + 7)
      else
        Result := vTextWidth + Images.Width + 10 + 7
    end
    else
      Result := vTextWidth + vTextOffset;
  end
  else
    if ShowImage and (Images <> nil) then
      Result := Images.Width + 7
    else
      Result := Width;

  if DropButton then
    Result := Result + 12;
end;//NeededWidth
//------------------------------------------------------------------------------
function TDSToolButton.NeededHeight: Integer;
var
  vTextHeight: Integer;
  vTextOffset: Integer;
begin
  if ShowText then
  begin
    if TextOrientation = toHorizontal then
    begin
      vTextHeight := Canvas.TextHeight(Caption);
      vTextOffset := 6
    end
    else
    begin
      vTextHeight := Canvas.TextWidth(Caption);
      vTextOffset :=  14;
    end;
      
    if ShowImage and (Images <> nil) then
    begin
      if TextToImage in [tipAfterImage, tipBeforeImage, tipOverlayImage] then
        Result := Max(vTextHeight + vTextOffset, Images.Height + 6)
      else
        Result := vTextHeight + Images.Height + 7;
    end
    else
      Result := vTextHeight + vTextOffset;
  end
  else
    if ShowImage and (Images <> nil) then
      Result := Images.Height + 6
    else
      Result := Height;
end;//NeededHeight
//------------------------------------------------------------------------------
procedure TDSToolButton.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  OrderTextAndImage;
end;
//------------------------------------------------------------------------------
procedure TDSToolButton.Click;
begin
  inherited Click;
end;
//------------------------------------------------------------------------------
procedure TDSToolButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
  begin
    if not CheckDefaults then
      Down := TCustomAction(Sender).Checked;
    if not CheckDefaults or (Self.ImageIndex = -1) or (Images = nil) then
    begin
      if not ((Images <> nil) and (TCustomAction(Sender).ActionList.Images = nil)) then
        Images := TCustomAction(Sender).ActionList.Images;
      ImageIndex := TCustomAction(Sender).ImageIndex;
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure TDSToolButton.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
  if Dest is TCustomAction then
  begin
    TCustomAction(Dest).ImageIndex := ImageIndex;
  end;
end;
//------------------------------------------------------------------------------
function TDSToolButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TDSToolButtonActionLink;
end;
//------------------------------------------------------------------------------

procedure TDSToolButton.setTextOrientation(const Value: TTextOrientation);
begin
  fTextOrientation := Value;
  OrderTextAndImage;
end;
//------------------------------------------------------------------------------
procedure TDSToolButton.setGroup(const Value: TDsToolButtonGroup);
begin
  if Value <> fGroup then
  begin
    if fGroup <> nil then
      fGroup.InternalRemoveButton(Self);
    fGroup := Value;
    if fGroup <> nil then
      fGroup.InternalAppendButton(Self);
  end;
end;//setGroup
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// TDsToolButtonGroup
//------------------------------------------------------------------------------
constructor TDsToolButtonGroup.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fButtons := TList.Create;
  AllowAllUp := False;
end;//Create
//------------------------------------------------------------------------------
destructor TDsToolButtonGroup.Destroy;
begin
  FreeAndNil(fButtons);
  inherited Destroy;
end;//Destroy
//------------------------------------------------------------------------------
procedure TDsToolButtonGroup.AppendButton(aButton: TDSToolButton);
begin
  aButton.Group := Self;
end;//AppendButton
//------------------------------------------------------------------------------
procedure TDsToolButtonGroup.RemoveButton(aButton: TDSToolButton);
begin
  aButton.Group := Self;
end;//RemoveButton
//------------------------------------------------------------------------------
procedure TDsToolButtonGroup.InternalAppendButton(aButton: TDSToolButton);
begin
  fButtons.Add(aButton);
end;//InternalAppendButton
//------------------------------------------------------------------------------
procedure TDsToolButtonGroup.InternalRemoveButton(aButton: TDSToolButton);
begin
  fButtons.Remove(aButton);
end;//InternalRemoveButton
//------------------------------------------------------------------------------
procedure TDsToolButtonGroup.ButtonCheck(aButton: TDSToolButton; aChecked: Boolean);
var
  i: Integer;
begin
  if aButton.Check then
  begin
    for i := 0 to fButtons.Count - 1 do
      if TDSToolButton(fButtons[i]) <> aButton then
        if TDSToolButton(fButtons[i]).Down then
          TDSToolButton(fButtons[i]).Down := False;
  end;
end;//ButtonCheck
//------------------------------------------------------------------------------
procedure TDsToolButtonGroup.CanCheck(aButton: TDSToolButton; var aChecked: Boolean);
var
  i: Integer;
  vOtherDown: Boolean;
begin
  if aButton.Down and not AllowAllUp and not aChecked then
  begin
    vOtherDown := False;
    for i := 0 to fButtons.Count - 1 do
      if TDSToolButton(fButtons[i]) <> aButton then
        if TDSToolButton(fButtons[i]).Down then
        begin
          vOtherDown := True;
          Break;
        end;
     if not vOtherDown then
       aChecked := True;
  end;
end;//CanCheck
//------------------------------------------------------------------------------
end.

