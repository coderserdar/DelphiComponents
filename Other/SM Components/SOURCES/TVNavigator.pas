{ Copyright (C) 1998-2003, written by Shkolnik Mike, Scalabium
  E-Mail:  mshkolnik@scalabium.com
           mshkolnik@yahoo.com
  WEB: http://www.scalabium.com
       http://www.geocities.com/mshkolnik
  tel: 380-/44/-552-10-29

  TSMTVNavigator component is "dbnavigator" for TreeView controls
  with the next features:
  - first record's button
  - previous record's button
  - next record's button
  - last record's button
  - new record's button
  - edit box for navigation by record number
}

unit TVNavigator;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, comctrls, Buttons;

type
  TSMANavButton = class;

  TSMANavGlyph = (ngEnabled, ngDisabled);
  TSMANavigateBtn = (sbaFirst, sbaPrior, sbaRecNo, sbaRecordCount,
                     sbaNext, sbaLast, sbaNew, sbaInsert, sbaDelete);
  TSMAButtonSet = set of TSMANavigateBtn;
  TSMANavButtonStyle = set of (nsAllowTimer, nsFocusRect);

  ENavBefore = procedure (Sender: TObject; var Button: TSMANavigateBtn; var CanClick: Boolean) of object;
  ENavClick = procedure (Sender: TObject; Button: TSMANavigateBtn) of object;

  TEditRecord = procedure (Sender: TObject; IsCopy: Boolean) of object;

  TTVNavigator = class(TCustomPanel)
  private
    { Private declarations }
    FVisibleButtons: TSMAButtonSet;

    ButtonWidth: Integer;
    MinBtnSize: TPoint;

    FOnNavClick: ENavClick;
    FBeforeAction: ENavBefore;
    FocusedButton: TSMANavigateBtn;
    FConfirmDelete: Boolean;
    FFlat: Boolean;

    FTreeView: TTreeView;
    oldTVChanged: TTVChangedEvent;

    procedure NavOnChange(Sender: TObject; Node: TTreeNode);
    function GetTreeView: TTreeView;
    procedure SetTreeView(Value: TTreeView);

    procedure ClickHandler(Sender: TObject);
    procedure EditKeyHandler(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure SetVisible(Value: TSMAButtonSet);
    procedure InitButtons;
    function GetButtonControl(i: TSMANavigateBtn): TControl;

    procedure BtnMouseDown (Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetFlat(Value: Boolean);

    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  protected
    { Protected declarations }
    procedure DataChanged;
    procedure EditingChanged;
    procedure Loaded; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BtnClick(Index: TSMANavigateBtn);
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
  published
    { Published declarations }
    property ConfirmDelete: Boolean read FConfirmDelete write FConfirmDelete default True;
    property TreeView: TTreeView read GetTreeView write SetTreeView;
    property Flat: Boolean read FFlat write SetFlat default False;

    property VisibleButtons: TSMAButtonSet read FVisibleButtons write SetVisible
      default [sbaFirst, sbaPrior, sbaRecNo, sbaRecordCount,
               sbaNext, sbaLast, sbaNew, sbaInsert, sbaDelete];

    property BeforeAction: ENavBefore read FBeforeAction write FBeforeAction;
    property OnClick: ENavClick read FOnNavClick write FOnNavClick;

    property Align;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Ctl3D;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnResize;
    property OnStartDrag;
  end;

{ TSMANavButton }
  TSMANavButton = class(TSpeedButton)
  private
    FNavStyle: TSMANavButtonStyle;
    FRepeatTimer: TTimer;
    procedure TimerExpired(Sender: TObject);
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  public
    destructor Destroy; override;
    property NavStyle: TSMANavButtonStyle read FNavStyle write FNavStyle;
  end;

const
  InitRepeatPause = 400;  { pause before repeat timer (ms) }
  RepeatPause     = 100;  { pause before hint window displays (ms)}
  SpaceSize       =   5;  { size of space between special buttons }

procedure Register;

implementation
{$R TVNAVIGATOR.RES}

uses StdCtrls;

const
  sNewItem = 'New Item';
  sNewSubItem = 'New Sub-Item';
  sDeleteNodeQuestion = 'Do you really want to delete a selected node?';

procedure Register;
begin
  RegisterComponents('SMComponents', [TTVNavigator]);
end;

{ TTVNavigator }
constructor TTVNavigator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] + [csOpaque];
  if not NewStyleControls then
    ControlStyle := ControlStyle + [csFramed];

  FVisibleButtons := [sbaFirst, sbaPrior, sbaRecNo, sbaRecordCount,
               sbaNext, sbaLast, sbaNew, sbaInsert, sbaDelete];

  InitButtons;

  BevelOuter := bvNone;
  BevelInner := bvNone;
  Width := 241;
  Height := 25;
  ButtonWidth := 0;
  FocusedButton := sbaFirst;
  FConfirmDelete := True;
  FullRepaint := False;
end;

destructor TTVNavigator.Destroy;
begin
  inherited Destroy;
end;

procedure TTVNavigator.SetVisible(Value: TSMAButtonSet);
begin
  if FVisibleButtons <> Value then
  begin
    FVisibleButtons := Value;
    InitButtons;
    Invalidate;
  end;
end;

procedure TTVNavigator.InitButtons;
var
  X: Integer;

  procedure CreateBtn(idx: TSMANavigateBtn; img: string; IsTimer: Boolean);
  var
    Btn: TSMANavButton;
  begin
    Btn := TSMANavButton.Create(Self);
    Btn.Flat := Flat;
    Btn.Tag := Ord(idx);
    Btn.Visible := True;
    Btn.Enabled := True;
    Btn.SetBounds(X, 0, MinBtnSize.X, MinBtnSize.Y);
    Btn.Glyph.LoadFromResourceName(HInstance, img);
    Btn.NumGlyphs := 1;
    Btn.OnClick := ClickHandler;
    Btn.OnMouseDown := BtnMouseDown;
    Btn.Parent := Self;

    if IsTimer then
      Btn.NavStyle := Btn.NavStyle + [nsAllowTimer];

    X := X + MinBtnSize.X;
  end;

  procedure CreateEdit(idx: TSMANavigateBtn);
  var
    edit: TEdit;
  begin
    edit := TEdit.Create(Self);
    edit.Visible := True;
    edit.Tag := Ord(idx);
    edit.Enabled := (idx = sbaRecNo);
    if edit.Enabled then
    begin
      edit.OnExit := ClickHandler;
      edit.OnKeyDown := EditKeyHandler;
    end
    else
      edit.Color := clBtnFace;
    edit.SetBounds(X, 0, 3*MinBtnSize.X, MinBtnSize.Y);
    edit.Parent := Self;
    edit.Text := '1';
    edit.OnClick := ClickHandler;
    edit.OnMouseDown := BtnMouseDown;

    X := X + 3*MinBtnSize.X;
  end;

  procedure CreateLabel;
  var
    lbl: TLabel;
  begin
    lbl := TLabel.Create(Self);
    lbl.AutoSize := False;
    lbl.Visible := True;
    lbl.Enabled := True;
//    lbl.Tag := Ord(sbaRecordCount);
    lbl.SetBounds(X, 0, MinBtnSize.X, MinBtnSize.Y);
    lbl.Parent := Self;
    lbl.Caption := ' of';
    lbl.Alignment := taCenter;
    lbl.Layout := tlCenter;

    X := X + MinBtnSize.X;
  end;

begin
  for x := ComponentCount-1 downto 0 do
    Components[x].Free;

  MinBtnSize := Point(20, 21{18});
  X := 0;

  if (sbaFirst in FVisibleButtons) then
    CreateBtn(sbaFirst, 'SMT_FIRST', False);
  if (sbaPrior in FVisibleButtons) then
    CreateBtn(sbaPrior, 'SMT_PRIOR', True);

  if (sbaRecNo in FVisibleButtons) then
    CreateEdit(sbaRecNo);
  if (sbaRecordCount in FVisibleButtons) then
  begin
    CreateLabel;
    CreateEdit(sbaRecordCount);
  end;

  if (sbaNext in FVisibleButtons) then
    CreateBtn(sbaNext, 'SMT_NEXT', True);
  if (sbaLast in FVisibleButtons) then
    CreateBtn(sbaLast, 'SMT_LAST', False);
  if (sbaNew in FVisibleButtons) then
    CreateBtn(sbaNew, 'SMT_NEW', False);
  if (sbaInsert in FVisibleButtons) then
    CreateBtn(sbaInsert, 'SMT_INSERT', False);
  if (sbaDelete in FVisibleButtons) then
    CreateBtn(sbaDelete, 'SMT_DELETE', False)
end;

procedure TTVNavigator.SetFlat(Value: Boolean);
var
  i: Integer;
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    for i := 0 to ComponentCount-1 do
      if Components[i] is TSpeedButton then
        TSpeedButton(Components[i]).Flat := Value;
  end;
end;

procedure TTVNavigator.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
end;

procedure TTVNavigator.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and
     (AComponent = TreeView) then
    TreeView := nil;
end;

procedure TTVNavigator.ClickHandler(Sender: TObject);
begin
  BtnClick(TSMANavigateBtn(TSMANavButton(Sender).Tag));
end;

procedure TTVNavigator.EditKeyHandler(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    BtnClick(sbaRecNo)
end;

function TTVNavigator.GetButtonControl(i: TSMANavigateBtn): TControl;
var
  j: Integer;
begin
  Result := nil;
  for j := 0 to ControlCount-1 do
    if TControl(Controls[j]).Tag = Ord(i) then
    begin
      Result := TControl(Controls[j]);
      break
    end;
end;

procedure TTVNavigator.BtnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  OldFocus: TSMANavigateBtn;
  cntrl: TControl;
begin
  OldFocus := FocusedButton;
  FocusedButton := TSMANavigateBtn(TSMANavButton(Sender).Tag);
  if TabStop and (GetFocus <> Handle) and CanFocus then
  begin
    SetFocus;
    if (GetFocus <> Handle) then
      Exit;
  end
  else
    if TabStop and (GetFocus = Handle) and (OldFocus <> FocusedButton) then
    begin
      cntrl := GetButtonControl(OldFocus);
      if Assigned(cntrl) then
        cntrl.Invalidate;
      cntrl := GetButtonControl(FocusedButton);
      if Assigned(cntrl) then
        cntrl.Invalidate;
    end;
end;

procedure TTVNavigator.BtnClick(Index: TSMANavigateBtn);
var
  ProcessClick: Boolean;
  i: Integer;
  cntrl: TControl;
begin
  if (TreeView <> nil) then
  begin
    ProcessClick := True;
    if not (csDesigning in ComponentState) and Assigned(FBeforeAction) then
      FBeforeAction(Self, Index, ProcessClick);

    try
      if ProcessClick then
      begin
        case Index of
          sbaPrior: if Assigned(TreeView.Selected) then
                      TreeView.Selected.GetPrev.Selected := True
                    else
                      TreeView.Items.GetFirstNode.Selected := True;
          sbaNext: if Assigned(TreeView.Selected) then
                     TreeView.Selected.GetNext.Selected := True
                   else
                     TreeView.Items.GetFirstNode.Selected := True;
          sbaFirst: TreeView.Items.GetFirstNode.Selected := True;
          sbaLast: TreeView.Items[TreeView.Items.Count-1].Selected := True;
          sbaNew: if Assigned(TreeView.Selected) then
                    TreeView.Items.AddChild(TreeView.Selected, sNewItem);
          sbaInsert: if Assigned(TreeView.Selected) then
                       TreeView.Items.Add(TreeView.Selected, sNewSubItem);
          sbaDelete: if Assigned(TreeView.Selected) and
                        ((not FConfirmDelete) or
                         (MessageDlg(SDeleteNodeQuestion, mtConfirmation, mbOKCancel, 0) <> idCancel)) then
                       TreeView.Selected.Delete;
          sbaRecNo: begin
                      try
                        cntrl := GetButtonControl(sbaRecNo);
                        if Assigned(cntrl) then
                           with TEdit(cntrl) do
                           begin
                             i := StrToInt(Text);
                             if (i > 0) and (i < TreeView.Items.Count) then
                               TreeView.Items[i].Selected := True
                             else
                               if Assigned(TreeView.Selected) then
                                 Text := IntToStr(TreeView.Selected.AbsoluteIndex+1)
                               else
                                 Text := '1'
                           end
                      except
                      end;
                    end;
        end;
        if not (csDesigning in ComponentState) and Assigned(FOnNavClick) then
          FOnNavClick(Self, Index);
      end;
    except
    end;
  end;
end;

procedure TTVNavigator.WMSetFocus(var Message: TWMSetFocus);
var
  cntrl: TControl;
begin
  cntrl := GetButtonControl(FocusedButton);
  if Assigned(cntrl) then
    cntrl.Invalidate;
end;

procedure TTVNavigator.WMKillFocus(var Message: TWMKillFocus);
var
  cntrl: TControl;
begin
  cntrl := GetButtonControl(FocusedButton);
  if Assigned(cntrl) then
    cntrl.Invalidate;
end;

procedure TTVNavigator.KeyDown(var Key: Word; Shift: TShiftState);
var
  NewFocus: TSMANavigateBtn;
  OldFocus: TSMANavigateBtn;
  cntrl: TControl;
begin
  OldFocus := FocusedButton;
  case Key of
    VK_RIGHT:
      begin
        NewFocus := FocusedButton;
        repeat
          if Ord(NewFocus) < ComponentCount-1 then
            NewFocus := Succ(NewFocus);
        until (Ord(NewFocus) = ComponentCount-1);
        if NewFocus <> FocusedButton then
        begin
          FocusedButton := NewFocus;

          cntrl := GetButtonControl(OldFocus);
          if Assigned(cntrl) then
            cntrl.Invalidate;
          cntrl := GetButtonControl(FocusedButton);
          if Assigned(cntrl) then
            cntrl.Invalidate;
        end;
      end;
    VK_LEFT:
      begin
        NewFocus := FocusedButton;
        repeat
          if Ord(NewFocus) > 0 then
            NewFocus := Pred(NewFocus);
        until (Ord(NewFocus) = 0);
        if NewFocus <> FocusedButton then
        begin
          FocusedButton := NewFocus;

          cntrl := GetButtonControl(OldFocus);
          if Assigned(cntrl) then
            cntrl.Invalidate;
          cntrl := GetButtonControl(FocusedButton);
          if Assigned(cntrl) then
            cntrl.Invalidate;
        end;
      end;
    VK_SPACE:
      begin
        if (Components[Ord(FocusedButton)] is TSMANavButton) then
          TSMANavButton(Components[Ord(FocusedButton)]).Click;
      end;
  end;
end;

procedure TTVNavigator.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;

procedure TTVNavigator.DataChanged;
var
  TVEnabled, UpEnable, DnEnable, DataEnable: Boolean;
  cntrl: TControl;
begin
  TVEnabled := Enabled and Assigned(FTreeView) and (FTreeView.Items.Count > 0) and Assigned(FTreeView.Selected);
  UpEnable := TVEnabled and (FTreeView.Selected.AbsoluteIndex > 0);
  DnEnable := TVEnabled and (FTreeView.Selected.AbsoluteIndex < FTreeView.Items.Count);
  DataEnable := TVEnabled and (not FTreeView.ReadOnly);

  cntrl := GetButtonControl(sbaFirst);
  if Assigned(cntrl) then
    cntrl.Enabled := UpEnable;
  cntrl := GetButtonControl(sbaPrior);
  if Assigned(cntrl) then
    cntrl.Enabled := UpEnable;
  cntrl := GetButtonControl(sbaRecNo);
  if Assigned(cntrl) then
    with TEdit(cntrl) do
    begin
      Enabled := DataEnable;
      if Assigned(FTreeView) and Assigned(FTreeView.Selected) then
        Text := IntToStr(FTreeView.Selected.AbsoluteIndex+1)
      else
        Text := '1';
    end;
  cntrl := GetButtonControl(sbaRecordCount);
  if Assigned(cntrl) and
     Assigned(FTreeView) then
    (cntrl as TEdit).Text := IntToStr(FTreeView.Items.Count);
  cntrl := GetButtonControl(sbaNext);
  if Assigned(cntrl) then
    cntrl.Enabled := DnEnable;
  cntrl := GetButtonControl(sbaLast);
  if Assigned(cntrl) then
    cntrl.Enabled := DnEnable;
  cntrl := GetButtonControl(sbaDelete);
  if Assigned(cntrl) then
    cntrl.Enabled := DataEnable;
end;

procedure TTVNavigator.EditingChanged;
var
  CanModify: Boolean;
  cntrl: TControl;
begin
  CanModify := Enabled and Assigned(FTreeView) and Assigned(FTreeView.Selected) and (not FTreeView.ReadOnly);

  cntrl := GetButtonControl(sbaNew);
  if Assigned(cntrl) then
    cntrl.Enabled := CanModify;

  cntrl := GetButtonControl(sbaInsert);
  if Assigned(cntrl) then
    cntrl.Enabled := CanModify;
end;

procedure TTVNavigator.CMEnabledChanged(var Message: TMessage);
begin
  inherited;

//  if not (csLoading in ComponentState) then
//    ActiveChanged;
end;

procedure TTVNavigator.SetTreeView(Value: TTreeView);
begin
  if Assigned(FTreeView) then
    FTreeView.OnChange := oldTVChanged;

  FTreeView := Value;

//  if not (csLoading in ComponentState) then
//    ActiveChanged;
  if Value <> nil then
  begin
    oldTVChanged := Value.OnChange;
    Value.OnChange := NavOnChange;
    Value.FreeNotification(Self);
  end
  else
    oldTVChanged := nil;
end;

function TTVNavigator.GetTreeView: TTreeView;
begin
  Result := FTreeView;
end;

procedure TTVNavigator.NavOnChange(Sender: TObject; Node: TTreeNode);
var
  i: Integer;
begin
  if Assigned(oldTVChanged) then
    oldTVChanged(Sender, Node);

  if (not Enabled) or (Assigned(FTreeView) and (FTreeView.Items.Count = 0)) then
    for i := 0 to ControlCount-1 do
      TControl(Controls[i]).Enabled := False
  else
  begin
    DataChanged;
    EditingChanged;
  end;
end;

procedure TTVNavigator.Loaded;
begin
  inherited Loaded;

//  ActiveChanged;
end;

{TSMANavButton}
destructor TSMANavButton.Destroy;
begin
  if FRepeatTimer <> nil then
    FRepeatTimer.Free;
  inherited Destroy;
end;

procedure TSMANavButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown (Button, Shift, X, Y);

  if nsAllowTimer in FNavStyle then
  begin
    if FRepeatTimer = nil then
      FRepeatTimer := TTimer.Create(Self);

    FRepeatTimer.OnTimer := TimerExpired;
    FRepeatTimer.Interval := InitRepeatPause;
    FRepeatTimer.Enabled  := True;
  end;
end;

procedure TSMANavButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
                                  X, Y: Integer);
begin
  inherited MouseUp (Button, Shift, X, Y);
  if FRepeatTimer <> nil then
    FRepeatTimer.Enabled  := False;
end;

procedure TSMANavButton.TimerExpired(Sender: TObject);
begin
  FRepeatTimer.Interval := RepeatPause;
  if (FState = bsDown) and MouseCapture then
  begin
    try
      Click;
    except
      FRepeatTimer.Enabled := False;
      raise;
    end;
  end;
end;

procedure TSMANavButton.Paint;
var
  R: TRect;
begin
  inherited Paint;
  if (GetFocus = Parent.Handle) and
     (TSMANavigateBtn(Tag) = TTVNavigator(Parent).FocusedButton) then
  begin
    R := Bounds(0, 0, Width, Height);
    InflateRect(R, -3, -3);
    if FState = bsDown then
      OffsetRect(R, 1, 1);
    DrawFocusRect(Canvas.Handle, R);
  end;
end;

end.
