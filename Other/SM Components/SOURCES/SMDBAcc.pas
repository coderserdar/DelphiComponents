{ Copyright (C) 1998-2003, written by Shkolnik Mike, Scalabium
  E-Mail:  mshkolnik@scalabium.com
           mshkolnik@yahoo.com
  WEB: http://www.scalabium.com
       http://www.geocities.com/mshkolnik
  tel: 380-/44/-552-10-29

  TSMDBAccessNavigator component is dbnavigator a-la navigator
  in MS Access with the next features:
  - first record's button
  - previous record's button
  - next record's button
  - last record's button
  - new record's button
  - edit box for navigation by record number
}

unit SMDBAcc;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, 
  ExtCtrls, DB, Buttons;

type
  TSMANavButton = class;
  TSMANavDataLink = class;

  TSMANavGlyph = (ngEnabled, ngDisabled);
  TSMANavigateBtn = (sbaFirst, sbaPrior, sbaRecNo, sbaRecordCount,
                     sbaNext, sbaLast, sbaNew, sbaInsert, sbaDelete);
  TSMAButtonSet = set of TSMANavigateBtn;
  TSMANavButtonStyle = set of (nsAllowTimer, nsFocusRect);

  ENavBefore = procedure (Sender: TObject; var Button: TSMANavigateBtn; var CanClick: Boolean) of object;
  ENavClick = procedure (Sender: TObject; Button: TSMANavigateBtn) of object;

  TEditRecord = procedure (Sender: TObject; IsCopy: Boolean) of object;

  TSMDBAccessNavigator = class(TCustomPanel)
  private
    { Private declarations }
    FVisibleButtons: TSMAButtonSet;

    FDataLink: TSMANavDataLink;
    ButtonWidth: Integer;
    MinBtnSize: TPoint;

    FOnNavClick: ENavClick;
    FBeforeAction: ENavBefore;
    FocusedButton: TSMANavigateBtn;
    FConfirmDelete: Boolean;
    FFlat: Boolean;

    procedure ClickHandler(Sender: TObject);
    procedure EditKeyHandler(Sender: TObject; var Key: Word; Shift: TShiftState);

    function GetDataSource: TDataSource;
    procedure SetDataSource(Value: TDataSource);

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
    procedure ActiveChanged;
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
    property DataSource: TDataSource read GetDataSource write SetDataSource;
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

{ TSMNavDataLink }

  TSMANavDataLink = class(TDataLink)
  private
    FNavigator: TSMDBAccessNavigator;
  protected
    procedure EditingChanged; override;
    procedure DataSetChanged; override;
    procedure ActiveChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
  public
    constructor Create(ANav: TSMDBAccessNavigator);
    destructor Destroy; override;
  end;

const
  InitRepeatPause = 400;  { pause before repeat timer (ms) }
  RepeatPause     = 100;  { pause before hint window displays (ms)}
  SpaceSize       =   5;  { size of space between special buttons }

procedure Register;

implementation
{$R SMDBACC}

uses Dialogs, SysUtils, StdCtrls, DBTables, DBConsts, SmCnst;

procedure Register;
begin
  RegisterComponents('SMComponents', [TSMDBAccessNavigator]);
end;

{ TSMDBAccessNavigator }
constructor TSMDBAccessNavigator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] + [csOpaque];
  if not NewStyleControls then
    ControlStyle := ControlStyle + [csFramed];
  FDataLink := TSMANavDataLink.Create(Self);

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

destructor TSMDBAccessNavigator.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;

  inherited Destroy;
end;

procedure TSMDBAccessNavigator.SetVisible(Value: TSMAButtonSet);
begin
  if FVisibleButtons <> Value then
  begin
    FVisibleButtons := Value;
    InitButtons;
    Invalidate;
  end;
end;

procedure TSMDBAccessNavigator.InitButtons;
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
    lbl.Caption := SRecOf;
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
    CreateBtn(sbaFirst, 'SMA_FIRST', False);
  if (sbaPrior in FVisibleButtons) then
    CreateBtn(sbaPrior, 'SMA_PRIOR', True);

  if (sbaRecNo in FVisibleButtons) then
    CreateEdit(sbaRecNo);
  if (sbaRecordCount in FVisibleButtons) then
  begin
    CreateLabel;
    CreateEdit(sbaRecordCount);
  end;

  if (sbaNext in FVisibleButtons) then
    CreateBtn(sbaNext, 'SMA_NEXT', True);
  if (sbaLast in FVisibleButtons) then
    CreateBtn(sbaLast, 'SMA_LAST', False);
  if (sbaNew in FVisibleButtons) then
    CreateBtn(sbaNew, 'SMA_NEW', False);
  if (sbaInsert in FVisibleButtons) then
    CreateBtn(sbaInsert, 'SMA_INSERT', False);
  if (sbaDelete in FVisibleButtons) then
    CreateBtn(sbaDelete, 'SMA_DELETE', False)
end;

procedure TSMDBAccessNavigator.SetFlat(Value: Boolean);
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

procedure TSMDBAccessNavigator.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
end;

procedure TSMDBAccessNavigator.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (FDataLink <> nil) and
     (AComponent = DataSource) then
    DataSource := nil;
end;

procedure TSMDBAccessNavigator.ClickHandler(Sender: TObject);
begin
  BtnClick(TSMANavigateBtn(TSMANavButton(Sender).Tag));
end;

procedure TSMDBAccessNavigator.EditKeyHandler(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    BtnClick(sbaRecNo)
end;

function TSMDBAccessNavigator.GetButtonControl(i: TSMANavigateBtn): TControl;
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

procedure TSMDBAccessNavigator.BtnMouseDown(Sender: TObject; Button: TMouseButton;
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

procedure TSMDBAccessNavigator.BtnClick(Index: TSMANavigateBtn);
var
  ProcessClick: Boolean;
  i: Integer;
  cntrl: TControl;
begin
  if (DataSource <> nil) and (DataSource.State <> dsInactive) then
  begin
    ProcessClick := True;
    if not (csDesigning in ComponentState) and Assigned(FBeforeAction) then
      FBeforeAction(Self, Index, ProcessClick);

    try
      if ProcessClick then
      begin
        case Index of
          sbaPrior: DataSource.DataSet.Prior;
          sbaNext: DataSource.DataSet.Next;
          sbaFirst: begin
                      DataSource.DataSet.First;
                    end;
          sbaLast: begin
                     DataSource.DataSet.Last;
                   end;
          sbaNew: DataSource.DataSet.Append;
          sbaInsert: DataSource.DataSet.Insert;
          sbaDelete: if not FConfirmDelete or
                        (MessageDlg(SDeleteRecordQuestion, mtConfirmation, mbOKCancel, 0) <> idCancel) then
                       DataSource.DataSet.Delete;
          sbaRecNo: begin
                      try
                        cntrl := GetButtonControl(sbaRecNo);
                        if Assigned(cntrl) then
                           with TEdit(cntrl) do
                           begin
                             i := StrToInt(Text);
                             if (i > 0) and (i < DataSource.DataSet.RecNo) then
                               DataSource.DataSet.RecNo := i
                             else
                               Text := IntToStr(DataSource.DataSet.RecNo)
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

procedure TSMDBAccessNavigator.WMSetFocus(var Message: TWMSetFocus);
var
  cntrl: TControl;
begin
  cntrl := GetButtonControl(FocusedButton);
  if Assigned(cntrl) then
    cntrl.Invalidate;
end;

procedure TSMDBAccessNavigator.WMKillFocus(var Message: TWMKillFocus);
var
  cntrl: TControl;
begin
  cntrl := GetButtonControl(FocusedButton);
  if Assigned(cntrl) then
    cntrl.Invalidate;
end;

procedure TSMDBAccessNavigator.KeyDown(var Key: Word; Shift: TShiftState);
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

procedure TSMDBAccessNavigator.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;

procedure TSMDBAccessNavigator.DataChanged;
var
  UpEnable, DnEnable, DataEnable: Boolean;
  cntrl: TControl;
begin
  UpEnable := Enabled and FDataLink.Active and not FDataLink.DataSet.BOF;
  DnEnable := Enabled and FDataLink.Active and not FDataLink.DataSet.EOF;
  DataEnable := Enabled and FDataLink.Active and
                FDataLink.DataSet.CanModify and
                not (FDataLink.DataSet.BOF and FDataLink.DataSet.EOF);

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
      Text := IntToStr(DataSource.Dataset.RecNo);
    end;
  cntrl := GetButtonControl(sbaRecordCount);
  if Assigned(cntrl) then
    (cntrl as TEdit).Text := IntToStr(FDataLink.Dataset.RecordCount);
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

procedure TSMDBAccessNavigator.EditingChanged;
var
  CanModify: Boolean;
  cntrl: TControl;
begin
  CanModify := Enabled and FDataLink.Active and FDataLink.DataSet.CanModify;

  cntrl := GetButtonControl(sbaNew);
  if Assigned(cntrl) then
    cntrl.Enabled := CanModify;

  cntrl := GetButtonControl(sbaInsert);
  if Assigned(cntrl) then
    cntrl.Enabled := CanModify;
end;

procedure TSMDBAccessNavigator.ActiveChanged;
var
  i: Integer;
begin
  if not (Enabled and FDataLink.Active) then
    for i := 0 to ControlCount-1 do
      TControl(Controls[i]).Enabled := False
  else
  begin
    DataChanged;
    EditingChanged;
  end;
end;

procedure TSMDBAccessNavigator.CMEnabledChanged(var Message: TMessage);
begin
  inherited;

  if not (csLoading in ComponentState) then
    ActiveChanged;
end;

procedure TSMDBAccessNavigator.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if not (csLoading in ComponentState) then
    ActiveChanged;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

function TSMDBAccessNavigator.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TSMDBAccessNavigator.Loaded;
begin
  inherited Loaded;

  ActiveChanged;
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
     (TSMANavigateBtn(Tag) = TSMDBAccessNavigator(Parent).FocusedButton) then
  begin
    R := Bounds(0, 0, Width, Height);
    InflateRect(R, -3, -3);
    if FState = bsDown then
      OffsetRect(R, 1, 1);
    DrawFocusRect(Canvas.Handle, R);
  end;
end;

{TSMANavDataLink}
constructor TSMANavDataLink.Create(ANav: TSMDBAccessNavigator);
begin
  inherited Create;

  FNavigator := ANav;
end;

destructor TSMANavDataLink.Destroy;
begin
  FNavigator := nil;

  inherited Destroy;
end;

procedure TSMANavDataLink.EditingChanged;
begin
  inherited;

  if FNavigator <> nil then
    FNavigator.EditingChanged;
end;

procedure TSMANavDataLink.DataSetChanged;
begin
  inherited;

  if FNavigator <> nil then
    FNavigator.DataChanged;
end;

procedure TSMANavDataLink.ActiveChanged;
begin
  inherited;

  if FNavigator <> nil then
    FNavigator.ActiveChanged;
end;

procedure TSMANavDataLink.DataSetScrolled(Distance: Integer);
var
  cntrl: TControl;
begin
  inherited;

  if (FNavigator <> nil) then
  begin
    cntrl := FNavigator.GetButtonControl(sbaRecNo);
    if Assigned(cntrl) and (cntrl is TEdit) then
      with TEdit(cntrl) do
        if Dataset <> nil then
          Text := IntToStr(Dataset.RecNo)
        else
          Text := ''
  end;
end;

end.
