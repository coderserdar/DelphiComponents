unit DBNavPlus;

{*************************************************************}
{*                 Freeware dbExpress Plus                   *}
{* Copyright (c) Business Software Systems, Inc. 1995 - 2002 *}
{*                   All rights reserved.                    *}
{*************************************************************}

{$G+,N+,P+,S-,R-,H+,X+}

{$R DBNAVPLUS}

interface

uses
  Windows, SysUtils, Messages, Classes, Controls, Forms, Graphics,
  Menus, StdCtrls, ExtCtrls, Mask, Buttons, ComCtrls, DB, DBClient;

const
  InitRepeatPause = 400; // pause before repeat timer (ms)
  RepeatPause     = 100; // pause before hint window displays (ms)
  SpaceSize       = 5;   // size of space between special buttons

type
  TDBNavPlusButton = class;
  TDBNavPlusDataLink = class;

  TDBNavPlusGlyph = (ngEnabled, ngDisabled);
  TDBNavPlusBtn = (nbInsert, nbSave, nbApplyUpdates, nbDelete, nbCancel,
          nbRefresh, nbEdit, nbFirst, nbPrior, nbNext, nbLast);
  TDBNavPlusBtnSet = set of TDBNavPlusBtn;
  TDBNavPlusButtonStyle = set of (nsAllowTimer, nsFocusRect);

  EDBNavPlusClick = procedure (Sender: TObject; Button: TDBNavPlusBtn) of object;

 { TDBNavPlus }

  TDBNavPlus = class (TCustomPanel)
  private
    FDataLink: TDBNavPlusDataLink;
    FVisibleButtons: TDBNavPlusBtnSet;
    FHints: TStrings;
    FDefHints: TStrings;
    ButtonWidth: Integer;
    MinBtnSize: TPoint;
    FOnNavClick: EDBNavPlusClick;
    FBeforeAction: EDBNavPlusClick;
    FocusedButton: TDBNavPlusBtn;
    FMaxErrors: Integer;
    FConfirmDelete: Boolean;
    FFlat: Boolean;
    FInsertClosedDataSet: Boolean;
    procedure BtnMouseDown (Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ClickHandler(Sender: TObject);
    function GetDataSource: TDataSource;
    function GetHints: TStrings;
    procedure HintsChanged(Sender: TObject);
    procedure InitButtons;
    procedure InitHints;
    procedure SetDataSource(Value: TDataSource);
    procedure SetFlat(Value: Boolean);
    procedure SetHints(Value: TStrings);
    procedure SetSize(var W: Integer; var H: Integer);
    procedure SetVisible(Value: TDBNavPlusBtnSet);
    procedure WMSize(var Message: TWMSize);  message WM_SIZE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure WMWindowPosChanging(var Message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
  protected
    Buttons: array[TDBNavPlusBtn] of TDBNavPlusButton;
    procedure DataChanged;
    procedure EditingChanged;
    procedure ActiveChanged;
    procedure Loaded; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure CalcMinSize(var W, H: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure BtnClick(Index: TDBNavPlusBtn);
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property VisibleButtons: TDBNavPlusBtnSet read FVisibleButtons write SetVisible;
    property Align;
    property Anchors;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Flat: Boolean read FFlat write SetFlat default False;
    property Ctl3D;
    property Hints: TStrings read GetHints write SetHints;
    property InsertClosedDataSet: Boolean read FInsertClosedDataSet write FInsertClosedDataSet;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property ConfirmDelete: Boolean read FConfirmDelete write FConfirmDelete default True;
    property MaxErrors: Integer read FMaxErrors write FMaxErrors;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property BeforeAction: EDBNavPlusClick read FBeforeAction write FBeforeAction;
    property OnClick: EDBNavPlusClick read FOnNavClick write FOnNavClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

{ TDBNavPlusButton }

  TDBNavPlusButton = class(TSpeedButton)
  private
    FIndex: TDBNavPlusBtn;
    FNavStyle: TDBNavPlusButtonStyle;
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
    property NavStyle: TDBNavPlusButtonStyle read FNavStyle write FNavStyle;
    property Index : TDBNavPlusBtn read FIndex write FIndex;
  end;

{ TDBNavPlusDataLink }

  TDBNavPlusDataLink = class(TDataLink)
  private
    FNavigator: TDBNavPlus;
  protected
    procedure EditingChanged; override;
    procedure DataSetChanged; override;
    procedure ActiveChanged; override;
  public
    constructor Create(ANav: TDBNavPlus);
    destructor Destroy; override;
  end;

implementation

uses  DBNavPlusConsts, Dialogs, Math;

{ TDBNavPlus }

var
  BtnTypeName: array[TDBNavPlusBtn] of PChar = ('INSERT', 'SAVE', 'APPLYUPDATES',
     'DELETE', 'CANCEL', 'REFRESH', 'EDIT', 'FIRST', 'PRIOR', 'NEXT', 'LAST');
  BtnHintId: array[TDBNavPlusBtn] of Pointer = (@SDBNavPlusInsertRecord,
     @SDBNavPlusSaveEdit, @SDBNavPlusApplyUpdates,  @SDBNavPlusDeleteRecord,
     @SDBNavPlusCancelEdit, @SDBNavPlusRefreshRecord, @SDBNavPlusEditRecord,
     @SDBNavPlusFirstRecord, @SDBNavPlusPriorRecord, @SDBNavPlusNextRecord,
     @SDBNavPlusLastRecord);

constructor TDBNavPlus.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] + [csOpaque];
  if not NewStyleControls then ControlStyle := ControlStyle + [csFramed];
  FDataLink := TDBNavPlusDataLink.Create(Self);
  FVisibleButtons := [nbInsert, nbSave, nbApplyUpdates, nbDelete, nbCancel,
           nbRefresh, nbEdit, nbFirst, nbPrior, nbNext, nbLast];
  FHints := TStringList.Create;
  TStringList(FHints).OnChange := HintsChanged;
  InitButtons;
  InitHints;
  BevelOuter := bvNone;
  BevelInner := bvNone;
  Width := 311;
  Height := 25;
  ButtonWidth := 0;
  FocusedButton := nbFirst;
  FConfirmDelete := False;
  FullRepaint := False;
  FMaxErrors := 0;
end;

destructor TDBNavPlus.Destroy;
begin
  FDefHints.Free;
  FDataLink.Free;
  FHints.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TDBNavPlus.InitButtons;
var
  i: TDBNavPlusBtn;
  Btn: TDBNavPlusButton;
  x: Integer;
  ResName: string;
  BtnType: Word;
begin
  MinBtnSize := Point(25, 25);
  x := 0;
  BtnType := 0;
  for i := Low(Buttons) to High(Buttons) do begin
    Inc(BtnType);
    Btn := TDBNavPlusButton.Create(Self);
    Btn.Flat := Flat;
    Btn.Index := i;
    Btn.Visible := i in FVisibleButtons;
    Btn.Enabled := True;
    Btn.SetBounds (x, 0, MinBtnSize.X, MinBtnSize.Y);
    FmtStr(ResName, 'BSS_%s', [BtnTypeName[i]]);
    Btn.Glyph.LoadFromResourceName(HInstance, ResName);
    Btn.NumGlyphs := 2;
    Btn.Enabled := False;
    Btn.Enabled := True;
    Btn.OnClick := ClickHandler;
    Btn.OnMouseDown := BtnMouseDown;
    Btn.Parent := Self;
    Buttons[i] := Btn;
    if (BtnType = 4) or (BtnType = 6) or (BtnType = 8) then X := X + 7;
    x := x + MinBtnSize.X;
  end;
  Buttons[nbPrior].NavStyle := Buttons[nbPrior].NavStyle + [nsAllowTimer];
  Buttons[nbNext].NavStyle  := Buttons[nbNext].NavStyle + [nsAllowTimer];
end;

procedure TDBNavPlus.InitHints;
var
  i: Integer;
  j: TDBNavPlusBtn;
begin
  if not Assigned(FDefhints) then begin
    FDefhints := TStringList.Create;
    for j := Low(Buttons) to High(Buttons) do begin
      FDefHints.Add(LoadResString(BtnHintId[j]));
    end;
  end;
  for j := Low(Buttons) to High(Buttons) do begin
    Buttons[j].Hint := LoadResString(BtnHintId[j]);
  end;
  j := Low(Buttons);
  for i := 0 to (FHints.Count - 1) do begin
    if FHints.Strings[i] <> '' then Buttons[j].Hint := FHints.Strings[i];
    if j = High(Buttons) then Exit;
    Inc(j);
  end;
end;

procedure TDBNavPlus.HintsChanged(Sender: TObject);
begin
  InitHints;
end;

procedure TDBNavPlus.SetFlat(Value: Boolean);
var
  i: TDBNavPlusBtn;
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    for i := Low(Buttons) to High(Buttons) do
      Buttons[i].Flat := Value;
  end;
end;

procedure TDBNavPlus.SetHints(Value: TStrings);
begin
  if Value.Text = FDefHints.Text then begin
    FHints.Clear;
    end
  else begin
    FHints.Assign(Value);
  end;
end;

function TDBNavPlus.GetHints: TStrings;
begin
  if (csDesigning in ComponentState) and not (csWriting in ComponentState) and
     not (csReading in ComponentState) and (FHints.Count = 0) then begin
    Result := FDefHints;
    end
  else begin
    Result := FHints;
  end;
end;

procedure TDBNavPlus.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
  // Not implemented.
end;

procedure TDBNavPlus.CalcMinSize(var W, H: Integer);
var
  ButtonCount, TotalSpacerSize: Integer;
  I: TDBNavPlusBtn;
begin
  if (csLoading in ComponentState) then Exit;
  if Buttons[nbFirst] = nil then Exit;

  ButtonCount := 0;
  TotalSpacerSize := 0;

  for I := Low(Buttons) to High(Buttons) do
    if Buttons[I].Visible then
      Inc(ButtonCount);
  if ButtonCount = 0 then Inc(ButtonCount);

  if ((Buttons[nbInsert].Visible = True) or (Buttons[nbSave].Visible = True) or
     (Buttons[nbApplyUpdates].Visible = True)) and ((Buttons[nbDelete].Visible = True) or
     (Buttons[nbCancel].Visible = True)) then TotalSpacerSize := TotalSpacerSize + 7;
  if ((Buttons[nbInsert].Visible = True) or (Buttons[nbSave].Visible = True) or
     (Buttons[nbApplyUpdates].Visible = True) or (Buttons[nbDelete].Visible = True) or
     (Buttons[nbCancel].Visible = True)) and ((Buttons[nbRefresh].Visible = True) or
     (Buttons[nbEdit].Visible = True)) then TotalSpacerSize := TotalSpacerSize + 7;
  if ((Buttons[nbInsert].Visible = True) or (Buttons[nbSave].Visible = True) or
     (Buttons[nbApplyUpdates].Visible = True) or (Buttons[nbDelete].Visible = True) or
     (Buttons[nbCancel].Visible = True) or (Buttons[nbRefresh].Visible = True) or
     (Buttons[nbEdit].Visible = True)) and ((Buttons[nbFirst].Visible = True) or
     (Buttons[nbPrior].Visible = True) or (Buttons[nbNext].Visible = True) or
     (Buttons[nbLast].Visible = True)) then TotalSpacerSize := TotalSpacerSize + 7;

  W := Max(W, (ButtonCount * MinBtnSize.X) + ButtonCount + TotalSpacerSize);
  H := Max(H, MinBtnSize.Y);

  if Align = alNone then
     W := ((W - ButtonCount - TotalSpacerSize) div ButtonCount) * ButtonCount +
     ButtonCount + TotalSpacerSize;
end;

procedure TDBNavPlus.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TDBNavPlus.SetVisible(Value: TDBNavPlusBtnSet);
var
  i: TDBNavPlusBtn;
  w, h: Integer;
begin
  w := Width;
  h := Height;
  FVisibleButtons := Value;
  for i := Low(Buttons) to High(Buttons) do
    Buttons[i].Visible := i in FVisibleButtons;
  SetSize(W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds (Left, Top, W, H);
  Invalidate;
end;

procedure TDBNavPlus.SetSize (var W: Integer; var H: Integer); // Bss
var
  ButtonCount, TotalSpacerSize, MinNavWidth, NavTestWidth: Integer;
  i: TDBNavPlusBtn;
  X: Integer;
begin
  if (csLoading in ComponentState) then Exit;
  if Buttons[nbInsert] = nil then Exit;
  ButtonCount := 0;
  TotalSpacerSize := 0;
  for i := Low(Buttons) to High(Buttons) do begin
    if Buttons[i].Visible then begin
      Inc(ButtonCount);
    end;
  end;
  if ((Buttons[nbInsert].Visible = True) or (Buttons[nbSave].Visible = True) or
     (Buttons[nbApplyUpdates].Visible = True)) and ((Buttons[nbDelete].Visible = True) or
     (Buttons[nbCancel].Visible = True)) then TotalSpacerSize := TotalSpacerSize + 7;
  if ((Buttons[nbInsert].Visible = True) or (Buttons[nbSave].Visible = True) or
     (Buttons[nbApplyUpdates].Visible = True) or (Buttons[nbDelete].Visible = True) or
     (Buttons[nbCancel].Visible = True)) and ((Buttons[nbRefresh].Visible = True) or
     (Buttons[nbEdit].Visible = True)) then TotalSpacerSize := TotalSpacerSize + 7;
  if ((Buttons[nbInsert].Visible = True) or (Buttons[nbSave].Visible = True) or
     (Buttons[nbApplyUpdates].Visible = True) or (Buttons[nbDelete].Visible = True) or
     (Buttons[nbCancel].Visible = True) or (Buttons[nbRefresh].Visible = True) or
     (Buttons[nbEdit].Visible = True)) and ((Buttons[nbFirst].Visible = True) or
     (Buttons[nbPrior].Visible = True) or (Buttons[nbNext].Visible = True) or
     (Buttons[nbLast].Visible = True)) then TotalSpacerSize := TotalSpacerSize + 7;

  if ButtonCount = 0 then Inc(ButtonCount);         //lastbutton=index of last visible btn

  MinNavWidth := ButtonCount * (MinBtnSize.X + 1) + TotalSpacerSize;
  if W < MinNavWidth then W := MinNavWidth;
  if H < MinBtnSize.Y then H := MinBtnSize.Y;

  ButtonWidth := (W - ButtonCount - TotalSpacerSize) div ButtonCount; //adjusting width of buttons
  NavTestWidth := (ButtonWidth * ButtonCount) + ButtonCount + TotalSpacerSize;  //was 1 - temp=width of all buttons
  if Align = alNone then W := NavTestWidth;

  X := 0;
  for i := Low(Buttons) to High(Buttons) do begin

    //Set gap for buttons
    if (i = nbDelete) and ((Buttons[nbInsert].Visible = True) or
       (Buttons[nbSave].Visible = True) or (Buttons[nbApplyUpdates].Visible = True)) and
       ((Buttons[nbDelete].Visible = True) or (Buttons[nbCancel].Visible = True)) then X := X + 7;
    if (i = nbRefresh) and ((Buttons[nbInsert].Visible = True) or
       (Buttons[nbSave].Visible = True) or (Buttons[nbApplyUpdates].Visible = True) or
       (Buttons[nbDelete].Visible = True) or (Buttons[nbCancel].Visible = True)) and
       ((Buttons[nbRefresh].Visible = True) or (Buttons[nbEdit].Visible = True)) then X := X + 7;
    if (i = nbFirst) and ((Buttons[nbInsert].Visible = True) or (Buttons[nbSave].Visible = True) or
       (Buttons[nbApplyUpdates].Visible = True) or (Buttons[nbDelete].Visible = True) or
       (Buttons[nbCancel].Visible = True) or (Buttons[nbRefresh].Visible = True) or
       (Buttons[nbEdit].Visible = True)) and ((Buttons[nbFirst].Visible = True) or
       (Buttons[nbPrior].Visible = True) or (Buttons[nbNext].Visible = True) or
       (Buttons[nbLast].Visible = True)) then X := X + 7;

    if Buttons[i].Visible then begin
      Buttons[i].SetBounds (X, 0, ButtonWidth, Height);
      Inc (X, ButtonWidth + 1);
      end
    else begin
      Buttons[i].SetBounds (Width + ButtonWidth, 0, ButtonWidth, Height);
    end;
  end;
end;

procedure TDBNavPlus.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  W, H: Integer;
begin
  W := AWidth;
  H := AHeight;
  if not HandleAllocated then SetSize(W, H);
  inherited SetBounds (ALeft, ATop, W, H);
end;

procedure TDBNavPlus.WMSize(var Message: TWMSize);
var
  W, H: Integer;
begin
  inherited;
  { check for minimum size }
  W := Width;
  H := Height;
  SetSize (W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds(Left, Top, W, H);
  Message.Result := 0;
end;

procedure TDBNavPlus.ClickHandler(Sender: TObject);
begin
  BtnClick(TDBNavPlusButton(Sender).Index);
end;

procedure TDBNavPlus.BtnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  OldFocus: TDBNavPlusBtn;
begin
  OldFocus := FocusedButton;
  FocusedButton := TDBNavPlusButton (Sender).Index;
  if TabStop and (GetFocus <> Handle) and CanFocus then begin
    SetFocus;
    if (GetFocus <> Handle) then Exit;
    end
  else begin
    if TabStop and (GetFocus = Handle) and
       (OldFocus <> FocusedButton) then begin
      Buttons[OldFocus].Invalidate;
      Buttons[FocusedButton].Invalidate;
    end;
  end;
end;

procedure TDBNavPlus.BtnClick(Index: TDBNavPlusBtn);
var
  aMasterDataSet: TDataSet;
begin
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then begin
    if not(csDesigning in ComponentState) and Assigned(FBeforeAction) then
       FBeforeAction(Self, Index);

    aMasterDataSet := DataSource.DataSet;

    if DataSource.DataSet.ClassType = TClientDataSet then begin
      while TClientDataSet(aMasterDataSet).DataSetField <> nil do begin
        aMasterDataSet := TClientDataSet(aMasterDataSet).DataSetField.DataSet;
      end;
    end;

    case Index of
      nbInsert:
        begin
          if (DataSource.DataSet is TClientDataSet) and
             (FInsertClosedDataSet = True) and
             (DataSource.DataSet.Active = False) then
            DataSource.DataSet.Active := True;
          if DataSource.State <> dsInactive then DataSource.DataSet.Insert;
        end;
      nbSave:
        begin
          if DataSource.State <> dsInactive then DataSource.DataSet.Post;
        end;
      nbApplyUpdates:
        begin
          if DataSource.DataSet.ClassType = TClientDataSet then begin
             //if TClientDataSet(aMasterDataSet).ChangeCount > 0 then
             //   TClientDataSet(aMasterDataSet).ApplyUpdates(FMaxErrors);
             if TClientDataSet(DataSource.DataSet).ChangeCount > 0 then
                TClientDataSet(DataSource.DataSet).ApplyUpdates(FMaxErrors);
          end;

          Buttons[nbApplyUpdates].Enabled := False;
        end;
      nbDelete:
        begin
          if DataSource.State <> dsInactive then begin
            if (FConfirmDelete = False) or
               (MessageDlg(SDBNavPlusDeleteRecordQuestion, mtConfirmation,
               mbOKCancel, 0) <> idCancel) then begin
              DataSource.DataSet.Delete;
              Buttons[nbApplyUpdates].Enabled := True;
            end;
          end;
        end;
      nbCancel: begin if DataSource.State <> dsInactive then DataSource.DataSet.Cancel; end;
      nbRefresh: begin if DataSource.State <> dsInactive then aMasterDataSet.Refresh; end;
      nbEdit: begin if DataSource.State <> dsInactive then DataSource.DataSet.Edit; end;
      nbPrior: begin if DataSource.State <> dsInactive then DataSource.DataSet.Prior; end;
      nbNext: begin if DataSource.State <> dsInactive then DataSource.DataSet.Next; end;
      nbFirst: begin if DataSource.State <> dsInactive then DataSource.DataSet.First; end;
      nbLast: begin if DataSource.State <> dsInactive then DataSource.DataSet.Last; end;
    end;
  end;
  if not (csDesigning in ComponentState) and Assigned(FOnNavClick) then
    FOnNavClick(Self, Index);
end;

procedure TDBNavPlus.WMSetFocus(var Message: TWMSetFocus);
begin
  Buttons[FocusedButton].Invalidate;
end;

procedure TDBNavPlus.WMKillFocus(var Message: TWMKillFocus);
begin
  Buttons[FocusedButton].Invalidate;
end;

procedure TDBNavPlus.KeyDown(var Key: Word; Shift: TShiftState);
var
  NewFocus: TDBNavPlusBtn;
  OldFocus: TDBNavPlusBtn;
begin
  OldFocus := FocusedButton;
  case Key of
    VK_RIGHT:
      begin
        NewFocus := FocusedButton;
        repeat
          if NewFocus < High(Buttons) then
            NewFocus := Succ(NewFocus);
        until (NewFocus = High(Buttons)) or (Buttons[NewFocus].Visible);
        if NewFocus <> FocusedButton then
        begin
          FocusedButton := NewFocus;
          Buttons[OldFocus].Invalidate;
          Buttons[FocusedButton].Invalidate;
        end;
      end;
    VK_LEFT:
      begin
        NewFocus := FocusedButton;
        repeat
          if NewFocus > Low(Buttons) then
            NewFocus := Pred(NewFocus);
        until (NewFocus = Low(Buttons)) or (Buttons[NewFocus].Visible);
        if NewFocus <> FocusedButton then
        begin
          FocusedButton := NewFocus;
          Buttons[OldFocus].Invalidate;
          Buttons[FocusedButton].Invalidate;
        end;
      end;
    VK_SPACE:
      begin
        if Buttons[FocusedButton].Enabled then
          Buttons[FocusedButton].Click;
      end;
  end;
end;

procedure TDBNavPlus.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;

procedure TDBNavPlus.DataChanged;
var
  UpEnable, DnEnable: Boolean;
begin
  UpEnable := Enabled and FDataLink.Active and not FDataLink.DataSet.BOF;
  DnEnable := Enabled and FDataLink.Active and not FDataLink.DataSet.EOF;
  Buttons[nbFirst].Enabled := UpEnable;
  Buttons[nbPrior].Enabled := UpEnable;
  Buttons[nbNext].Enabled := DnEnable;
  Buttons[nbLast].Enabled := DnEnable;
  Buttons[nbDelete].Enabled := Enabled and FDataLink.Active and
    FDataLink.DataSet.CanModify and
    not (FDataLink.DataSet.BOF and FDataLink.DataSet.EOF);
end;

procedure TDBNavPlus.EditingChanged;
var
  CanModify: Boolean;
begin
  CanModify := Enabled and FDataLink.Active and FDataLink.DataSet.CanModify;
  Buttons[nbInsert].Enabled := True;
  Buttons[nbSave].Enabled := CanModify and FDataLink.Editing;

  if (DataSource <> nil) and (DataSource.DataSet <> nil) and
     ((DataSource.DataSet.ClassType = TClientDataSet) {or
     (DataSource.DataSet.ClassType = TSQLClientDataSet)}) then begin
    Buttons[nbApplyUpdates].Enabled :=
       TClientDataSet(DataSource.DataSet).ChangeCount > 0;
    end
  else begin
    Buttons[nbApplyUpdates].Enabled := False;
  end;
  
  Buttons[nbCancel].Enabled := CanModify and FDataLink.Editing;
  Buttons[nbRefresh].Enabled := CanModify;
  Buttons[nbEdit].Enabled := CanModify and not FDataLink.Editing;
end;

procedure TDBNavPlus.ActiveChanged;
var
  i: TDBNavPlusBtn;
begin
  if not (Enabled and FDataLink.Active) then begin
    for i := Low(Buttons) to High(Buttons) do begin
      Buttons[i].Enabled := False
    end;
    if (FInsertClosedDataSet = True) and (DataSource <> nil) and (DataSource.DataSet <> nil) then
       Buttons[nbInsert].Enabled := True;
    end   
  else begin
    DataChanged;
    EditingChanged;
  end;
end;

procedure TDBNavPlus.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if not (csLoading in ComponentState) then
    ActiveChanged;
end;

procedure TDBNavPlus.WMWindowPosChanging(var Message: TWMWindowPosChanging);
begin
  inherited;
  if (SWP_NOSIZE and Message.WindowPos.Flags) = 0 then
    CalcMinSize(Message.WindowPos.cx, Message.WindowPos.cy);
end;

procedure TDBNavPlus.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if not (csLoading in ComponentState) then ActiveChanged;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TDBNavPlus.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDBNavPlus.Loaded;
var
  W, H: Integer;
begin
  inherited Loaded;
  W := Width;
  H := Height;
  SetSize (W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds (Left, Top, W, H);
  InitHints;
  ActiveChanged;
end;

{TDBNavPlusButton}

destructor TDBNavPlusButton.Destroy;
begin
  if FRepeatTimer <> nil then
    FRepeatTimer.Free;
  inherited Destroy;
end;

procedure TDBNavPlusButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
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

procedure TDBNavPlusButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
                                  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FRepeatTimer <> nil then
     FRepeatTimer.Enabled  := False;
end;

procedure TDBNavPlusButton.TimerExpired(Sender: TObject);
begin
  FRepeatTimer.Interval := RepeatPause;
  if (FState = bsDown) and MouseCapture then begin
    try
      Click;
    except
      FRepeatTimer.Enabled := False;
      raise;
    end;
  end;
end;

procedure TDBNavPlusButton.Paint;
var
  R: TRect;
begin
  inherited Paint;
  if (GetFocus = Parent.Handle) and
     (FIndex = TDBNavPlus (Parent).FocusedButton) then begin
    R := Bounds(0, 0, Width, Height);
    InflateRect(R, -3, -3);
    if FState = bsDown then OffsetRect(R, 1, 1);
    DrawFocusRect(Canvas.Handle, R);
  end;
end;

{ TDBNavPlusDataLink }

constructor TDBNavPlusDataLink.Create(ANav: TDBNavPlus);
begin
  inherited Create;
  FNavigator := ANav;
  VisualControl := True;
end;

destructor TDBNavPlusDataLink.Destroy;
begin
  FNavigator := nil;
  inherited Destroy;
end;

procedure TDBNavPlusDataLink.EditingChanged;
begin
  if FNavigator <> nil then FNavigator.EditingChanged;
end;

procedure TDBNavPlusDataLink.DataSetChanged;
begin
  if FNavigator <> nil then FNavigator.DataChanged;
end;

procedure TDBNavPlusDataLink.ActiveChanged;
begin
  if FNavigator <> nil then FNavigator.ActiveChanged;
end;

end.
