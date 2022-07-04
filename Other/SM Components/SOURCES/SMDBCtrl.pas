{ Copyright (C) 1998-2008, written by Shkolnik Mike, Scalabium Software
  E-Mail:  mshkolnik@scalabium.com
           mshkolnik@yahoo.com
  WEB: http://www.scalabium.com

  TSMDBNavigator component is the extended TDBNavigator
  Additionally to standard features this component allows:
   - new buttons for Filter/Find/Print/Export/Import (+standard First/Nex/.../Refresh)
   - new Choose button for record select and new Clear button to clear the selection
   - custom Hints (russian, for example)
   - you may specify the button captions (additionally to glyphs)
   - enable/disable the button captions (ShowCaption property) or glyphs (ShowGlyph property)
   - glyph layout (Layout property)
   - possibility to cancel an action in BeforeAction event
   - possibility to change the clicked button in BeforeAction event
}

unit SMDBCtrl;

interface

{$IFDEF VER100}
  {$DEFINE SMForDelphi3}
{$ENDIF}

{$IFDEF VER110}
  {$DEFINE SMForDelphi3}
{$ENDIF}

{$IFDEF VER120}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
{$ENDIF}

{$IFDEF VER125}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
{$ENDIF}

{$IFDEF VER130}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
{$ENDIF}

{$IFDEF VER140}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
{$ENDIF}

{$IFDEF VER150}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
{$ENDIF}

{$IFDEF VER170}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
{$ENDIF}

{$IFDEF VER180}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
  {$DEFINE SMForDelphi2006}
  {$IFDEF BCB}
    {$DEFINE SMForBCB2006}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER185}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
  {$DEFINE SMForDelphi2006}
  {$IFDEF BCB}
    {$DEFINE SMForBCB2006}
    {$DEFINE SMForBCB2007}
  {$ENDIF}
  {$DEFINE SMForDelphi2007}
{$ENDIF}

{$IFDEF VER190}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
  {$DEFINE SMForDelphi2006}
  {$IFDEF BCB}
    {$DEFINE SMForBCB2006}
    {$DEFINE SMForBCB2007}
  {$ENDIF}
  {$DEFINE SMForDelphi2007}
  {$DEFINE SMForRADStudio2007}
{$ENDIF}

{$IFDEF VER200}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
  {$DEFINE SMForDelphi2006}
  {$IFDEF BCB}
    {$DEFINE SMForBCB2006}
    {$DEFINE SMForBCB2007}
    {$DEFINE SMForBCB2009}
  {$ENDIF}
  {$DEFINE SMForDelphi2007}
  {$DEFINE SMForRADStudio2007}
  {$DEFINE SMForDelphi2009}
{$ENDIF}

uses
  Windows, Messages, Classes, Controls, ExtCtrls, DB, Buttons, Graphics,
  SMCnst
  {$IFDEF SMForDelphi6} , Variants{$ENDIF};

type
  TSMNavButton = class;
  TSMNavDataLink = class;

  TSMNavGlyph = (ngEnabled, ngDisabled);
  TSMNavigateBtn = (sbFirst, sbPrior, sbNext, sbLast,
                    sbInsert, sbCopy, sbDelete, sbEdit,
                    sbFilter, sbFind, sbPrint, sbExport, sbImport,
                    sbPost, sbCancel, sbRefresh,
                    sbChoice, sbClear);
  TSMButtonSet = set of TSMNavigateBtn;
  TSMNavButtonStyle = set of (nsAllowTimer, nsFocusRect);

  ENavBefore = procedure (Sender: TObject; var Button: TSMNavigateBtn; var CanClick: Boolean) of object;
  ENavClick = procedure (Sender: TObject; Button: TSMNavigateBtn) of object;
  
  TEditRecord = procedure (Sender: TObject; IsCopy: Boolean) of object;

  TSMDBNavigator = class(TCustomPanel)
  private
    { Private declarations }
    FDataLink: TSMNavDataLink;
    FVisibleButtons: TSMButtonSet;
    FHints: TStrings;
    FCaptions: TStrings;
    FShowCaption: Boolean;
    FShowGlyph: Boolean;
    FLayout: TButtonLayout;
    ButtonWidth: Integer;
    MinBtnSize: TPoint;
    FOnNavClick: ENavClick;
    FBeforeAction: ENavBefore;
    FocusedButton: TSMNavigateBtn;
    FConfirmDelete: Boolean;
    FFlat: Boolean;
    FIncludeGap: Boolean;

    FOnAppendRecord: TNotifyEvent;
    FOnEditRecord: TEditRecord;
    FOnDeleteRecord: TNotifyEvent;
    FOnPostData: TNotifyEvent;
    FOnCancelData: TNotifyEvent;
    FOnRefreshData: TNotifyEvent;

    FOnFilterData: TNotifyEvent;
    FOnFindData: TNotifyEvent;
    FOnPrintData: TNotifyEvent;
    FOnExportData: TNotifyEvent;
    FOnImportData: TNotifyEvent;

    procedure ClickHandler(Sender: TObject);
    function GetDataSource: TDataSource;
    procedure SetDataSource(Value: TDataSource);
    procedure InitButtons;
    procedure InitHints;
    procedure InitCaptions;
    procedure BtnMouseDown (Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetVisible(Value: TSMButtonSet);
    procedure SetIncludeGap(Value: Boolean);
    procedure AdjustSizeNav(var W: Integer; var H: Integer);
    procedure HintsChanged(Sender: TObject);
    procedure CaptionsChanged(Sender: TObject);
    procedure SetHints(Value: TStrings);
    procedure SetCaptions(Value: TStrings);

    procedure SetFlat(Value: Boolean);
    procedure SetLayout(Value: TButtonLayout);
    procedure SetShowCaption(Value: Boolean);
    procedure SetShowGlyph(Value: Boolean);
    procedure RefreshData;
    procedure CopyRecord;
    procedure WMSize(var Message: TWMSize);  message WM_SIZE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
  protected
    { Protected declarations }
    Buttons: array[TSMNavigateBtn] of TSMNavButton;
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
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure BtnClick(Index: TSMNavigateBtn);
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
  published
    { Published declarations }
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property VisibleButtons: TSMButtonSet read FVisibleButtons write SetVisible
      default [sbFirst, sbPrior, sbNext, sbLast, sbInsert, sbDelete,
        sbEdit, sbPost, sbCancel, sbRefresh];
    property Align;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Flat: Boolean read FFlat write SetFlat default False;
    property Ctl3D;
    property Captions: TStrings read FCaptions write SetCaptions;
    property Hints: TStrings read FHints write SetHints;
    property IncludeGap: Boolean read FIncludeGap write SetIncludeGap default True;
    property Layout: TButtonLayout read FLayout write SetLayout;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption;
    property ShowGlyph: Boolean read FShowGlyph write SetShowGlyph;
//    property ParentColor;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property ConfirmDelete: Boolean read FConfirmDelete write FConfirmDelete default True;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property BeforeAction: ENavBefore read FBeforeAction write FBeforeAction;
    property OnClick: ENavClick read FOnNavClick write FOnNavClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnResize;
    property OnStartDrag;

    property OnAppendRecord: TNotifyEvent read FOnAppendRecord write FOnAppendRecord;
    property OnEditRecord: TEditRecord read FOnEditRecord write FOnEditRecord;
    property OnDeleteRecord: TNotifyEvent read FOnDeleteRecord write FOnDeleteRecord;
    property OnPostData: TNotifyEvent read FOnPostData write FOnPostData;
    property OnCancelData: TNotifyEvent read FOnCancelData write FOnCancelData;
    property OnRefreshData: TNotifyEvent read FOnRefreshData write FOnRefreshData;

    property OnFilterData: TNotifyEvent read FOnFilterData write FOnFilterData;
    property OnFindData: TNotifyEvent read FOnFindData write FOnFindData;
    property OnPrintData: TNotifyEvent read FOnPrintData write FOnPrintData;
    property OnExportData: TNotifyEvent read FOnExportData write FOnExportData;
    property OnImportData: TNotifyEvent read FOnImportData write FOnImportData;
  end;

{ TSMNavButton }

  TSMNavButton = class(TSpeedButton)
  private
    FIndex: TSMNavigateBtn;
    FNavStyle: TSMNavButtonStyle;
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
    property NavStyle: TSMNavButtonStyle read FNavStyle write FNavStyle;
    property Index: TSMNavigateBtn read FIndex write FIndex;
  end;

{ TSMNavDataLink }

  TSMNavDataLink = class(TDataLink)
  private
    FNavigator: TSMDBNavigator;
  protected
    procedure EditingChanged; override;
    procedure DataSetChanged; override;
    procedure ActiveChanged; override;
  public
    constructor Create(ANav: TSMDBNavigator);
    destructor Destroy; override;
  end;

const
  InitRepeatPause = 400;  { pause before repeat timer (ms) }
  RepeatPause     = 100;  { pause before hint window displays (ms)}
  SpaceSize       =  5;   { size of space between special buttons }

procedure Register;

implementation
{$R SMDBCTRL}

uses Dialogs, SysUtils, DBTables;

procedure Register;
begin
  RegisterComponents('SMComponents', [TSMDBNavigator]);
end;

{ TSMDBNavigator }
var
  BtnTypeName: array[TSMNavigateBtn] of PChar = ('FIRST', 'PRIOR', 'NEXT', 'LAST',
   'INSERT', 'COPY', 'DELETE', 'EDIT',
   'FILTER', 'FIND', 'PRINT', 'EXPORT', 'IMPORT',
   'POST', 'CANCEL', 'REFRESH',
   'CHOICE', 'CLEAR');
  BtnCaption: array[TSMNavigateBtn] of PChar =
    (SFirstName, SPriorName, SNextName, SLastName,
    SInsertName, SCopyName, SDeleteName, SEditName,
    SFilterName, SFindName, SPrintName, SExportName, SImportName,
    SPostName, SCancelName, SRefreshName,
    SChoiceName, SClearName);
  BtnHintId: array[TSMNavigateBtn] of PChar = (SFirstRecord, SPriorRecord, SNextRecord, SLastRecord,
    SInsertRecord, SCopyRecord, SDeleteRecord, SEditRecord,
    SFilterRecord, SFindRecord, SPrintRecord, SExportRecord, SImportRecord,
    SPostEdit, SCancelEdit, SRefreshRecord,
    SChoice, SClear);

constructor TSMDBNavigator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] + [csOpaque];
  if not NewStyleControls then ControlStyle := ControlStyle + [csFramed];
  FDataLink := TSMNavDataLink.Create(Self);

  FLayout := blGlyphLeft;
  ParentColor := True;
  FIncludeGap := True;

  FVisibleButtons := [sbFirst, sbPrior, sbNext, sbLast,
                      sbInsert, sbDelete, sbEdit,
                      sbPost, sbCancel, sbRefresh];
  FHints := TStringList.Create;
  TStringList(FHints).OnChange := HintsChanged;

  FCaptions := TStringList.Create;
  TStringList(FCaptions).OnChange := CaptionsChanged;

  FShowGlyph := True;

  InitButtons;
  InitCaptions;

  BevelOuter := bvNone;
  BevelInner := bvNone;
  Width := 241;
  Height := 25;
  ButtonWidth := 0;
  FocusedButton := sbFirst;
  FConfirmDelete := True;
  FullRepaint := False;
end;

destructor TSMDBNavigator.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;

  FHints.Free;
  FCaptions.Free;

  inherited Destroy;
end;

procedure TSMDBNavigator.InitButtons;
var
  i: TSMNavigateBtn;
  Btn: TSMNavButton;
  X: Integer;
  ResName: string;
begin
  for x := ComponentCount-1 downto 0 do
    Components[x].Free;

  MinBtnSize := Point(20, 18);
  X := 0;
  for i := Low(Buttons) to High(Buttons) do
  begin
    Btn := TSMNavButton.Create(Self);
    Btn.Flat := Flat;
    Btn.Index := I;
    Btn.Visible := I in FVisibleButtons;
    Btn.Enabled := True;
    Btn.SetBounds (X, 0, MinBtnSize.X, MinBtnSize.Y);
    FmtStr(ResName, 'smn_%s', [BtnTypeName[I]]);
    if FShowGlyph then
    begin
      Btn.Glyph.LoadFromResourceName(HInstance, ResName);
      Btn.NumGlyphs := 2;
    end;
    Btn.Layout := FLayout;
    Btn.Enabled := False;
    Btn.Enabled := True;
    Btn.OnClick := ClickHandler;
    Btn.OnMouseDown := BtnMouseDown;
    Btn.Parent := Self;
    Buttons[I] := Btn;
    X := X + MinBtnSize.X;
  end;
  InitHints;
  Buttons[sbPrior].NavStyle := Buttons[sbPrior].NavStyle + [nsAllowTimer];
  Buttons[sbNext].NavStyle  := Buttons[sbNext].NavStyle + [nsAllowTimer];
end;

procedure TSMDBNavigator.InitHints;
var
  I: Integer;
  J: TSMNavigateBtn;
begin
  for J := Low(Buttons) to High(Buttons) do
    Buttons[J].Hint := BtnHintId[J];
  J := Low(Buttons);
  for I := 0 to (FHints.Count - 1) do
  begin
    if FHints.Strings[I] <> '' then
      Buttons[J].Hint := FHints.Strings[I];
    if J = High(Buttons) then Exit;
    Inc(J);
  end;
end;

procedure TSMDBNavigator.InitCaptions;
var i: Integer;
    j: TSMNavigateBtn;
begin
  if FShowCaption then
  begin
    for j := Low(Buttons) to High(Buttons) do
      Buttons[j].Caption := BtnCaption[j];

    j := Low(Buttons);
    for i := 0 to (FCaptions.Count - 1) do
    begin
      if FCaptions.Strings[i] <> '' then
        Buttons[j].Caption := FCaptions.Strings[i];
      if j = High(Buttons) then Exit;
      Inc(j);
    end;
  end
  else
  begin
    for j := Low(Buttons) to High(Buttons) do
      Buttons[j].Caption := '';
  end;
end;

procedure TSMDBNavigator.HintsChanged(Sender: TObject);
begin
  InitHints;
end;

procedure TSMDBNavigator.CaptionsChanged(Sender: TObject);
begin
  InitCaptions;
end;

procedure TSMDBNavigator.SetFlat(Value: Boolean);
var
  I: TSMNavigateBtn;
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    for I := Low(Buttons) to High(Buttons) do
      Buttons[I].Flat := Value;
  end;
end;

procedure TSMDBNavigator.SetHints(Value: TStrings);
begin
  FHints.Assign(Value);
end;

procedure TSMDBNavigator.SetCaptions(Value: TStrings);
begin
  FCaptions.Assign(Value);
end;

procedure TSMDBNavigator.SetLayout(Value: TButtonLayout);
var I: TSMNavigateBtn;
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    for I := Low(Buttons) to High(Buttons) do
      Buttons[I].Layout := Value;
  end;
end;

procedure TSMDBNavigator.SetShowCaption(Value: Boolean);
begin
  if FShowCaption <> Value then
  begin
    FShowCaption := Value;
    InitCaptions;
  end;
end;

procedure TSMDBNavigator.SetShowGlyph(Value: Boolean);
var
  i: TSMNavigateBtn;
  ResName: string;
begin
  if FShowGlyph <> Value then
  begin
    FShowGlyph := Value;

    for i := Low(Buttons) to High(Buttons) do
      if FShowGlyph then
      begin
        FmtStr(ResName, 'smn_%s', [BtnTypeName[I]]);
        Buttons[i].Glyph.LoadFromResourceName(HInstance, ResName);
        Buttons[i].NumGlyphs := 2;
      end
      else
        Buttons[i].Glyph := nil;
  end;
end;

procedure TSMDBNavigator.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
end;

procedure TSMDBNavigator.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;

procedure TSMDBNavigator.SetIncludeGap(Value: Boolean);
begin
  if (FIncludeGap <> Value) then
  begin
    FIncludeGap := Value;

    SetVisible(FVisibleButtons);
  end;
end;

procedure TSMDBNavigator.SetVisible(Value: TSMButtonSet);
var
  I: TSMNavigateBtn;
  W, H: Integer;
begin
  W := Width;
  H := Height;
  FVisibleButtons := Value;
  for I := Low(Buttons) to High(Buttons) do
    Buttons[I].Visible := I in FVisibleButtons;
  AdjustSizeNav(W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds(Left, Top, W, H);
  Invalidate;
end;

procedure TSMDBNavigator.AdjustSizeNav(var W: Integer; var H: Integer);
var
  Count: Integer;
  MinW: Integer;
  I: TSMNavigateBtn;
  Space, Temp, Remain: Integer;
  X: Integer;
  boolShift: Boolean;
begin
  if (csLoading in ComponentState) then Exit;
  if Buttons[sbFirst] = nil then Exit;

  Count := 0;
  for I := Low(Buttons) to High(Buttons) do
    if Buttons[I].Visible then
      Inc(Count);
  if (Count = 0) then Inc(Count);

  boolShift := True;
  if IncludeGap and
     ((sbChoice in VisibleButtons) or
      (sbClear in VisibleButtons)) then
    Inc(Count); // for shift before btnChoice

  MinW := Count * MinBtnSize.X;
  if (W < MinW) then
    W := MinW;
  if (H < MinBtnSize.Y) then
    H := MinBtnSize.Y;

  ButtonWidth := W div Count;
  Temp := Count * ButtonWidth;
  if (Align = alNone) then
    W := Temp;

  X := 0;
  Remain := W - Temp;
  Temp := Count div 2;
  for I := Low(Buttons) to High(Buttons) do
  begin
    if Buttons[I].Visible then
    begin
      Space := 0;
      if Remain <> 0 then
      begin
        Dec(Temp, Remain);
        if Temp < 0 then
        begin
          Inc(Temp, Count);
          Space := 1;
        end;
      end;
      if IncludeGap and ((i = sbChoice) or (i = sbClear)) and boolShift then
      begin
        Inc(X, ButtonWidth + Space);
        boolShift := False;
      end;

      Buttons[I].SetBounds(X, 0, ButtonWidth + Space, Height);
      Inc(X, ButtonWidth + Space);
    end
    else
      Buttons[I].SetBounds(Width + 1, 0, ButtonWidth, Height);
  end;
end;

procedure TSMDBNavigator.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var W, H: Integer;
begin
  W := AWidth;
  H := AHeight;
  if not HandleAllocated then AdjustSizeNav(W, H);
  inherited SetBounds(ALeft, ATop, W, H);
end;

procedure TSMDBNavigator.WMSize(var Message: TWMSize);
var
  W, H: Integer;
begin
  inherited;
  { check for minimum size }
  W := Width;
  H := Height;
  AdjustSizeNav(W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds(Left, Top, W, H);
  Message.Result := 0;
end;

procedure TSMDBNavigator.ClickHandler(Sender: TObject);
begin
  BtnClick(TSMNavButton(Sender).Index);
end;

procedure TSMDBNavigator.BtnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  OldFocus: TSMNavigateBtn;
begin
  OldFocus := FocusedButton;
  FocusedButton := TSMNavButton (Sender).Index;
  if TabStop and (GetFocus <> Handle) and CanFocus then
  begin
    SetFocus;
    if (GetFocus <> Handle) then
      Exit;
  end
  else if TabStop and (GetFocus = Handle) and (OldFocus <> FocusedButton) then
  begin
    Buttons[OldFocus].Invalidate;
    Buttons[FocusedButton].Invalidate;
  end;
end;

procedure TSMDBNavigator.BtnClick(Index: TSMNavigateBtn);
var ProcessClick: Boolean;
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
          sbPrior: DataSource.DataSet.Prior;
          sbNext: DataSource.DataSet.Next;
          sbFirst: DataSource.DataSet.First;
          sbLast: DataSource.DataSet.Last;
          sbInsert: if Assigned(FOnAppendRecord) then
                      FOnAppendRecord(Self)
                    else
                      DataSource.DataSet.Insert;
          sbCopy: begin
                    CopyRecord;
                    if Assigned(FOnEditRecord) then
                      FOnEditRecord(Self, True);
                  end;
          sbEdit: if Assigned(FOnEditRecord) then
                    FOnEditRecord(Self, False)
                  else
                    DataSource.DataSet.Edit;
          sbCancel: if Assigned(FOnCancelData) then
                      FOnCancelData(Self)
                    else
                      DataSource.DataSet.Cancel;
          sbPost: if Assigned(FOnPostData) then
                    FOnPostData(Self)
                  else
                    DataSource.DataSet.Post;
          sbRefresh: if Assigned(FOnRefreshData) then
                       FOnRefreshData(Self)
                     else
                       RefreshData; // DataSource.DataSet.Refresh;
          sbDelete: if Assigned(FOnDeleteRecord) then
                      FOnDeleteRecord(Self)
                    else
                      if not FConfirmDelete or
                         (MessageDlg(SDeleteRecordQuestion, mtConfirmation, mbOKCancel, 0) <> idCancel) then
                        DataSource.DataSet.Delete;

          sbFilter: if Assigned(FOnFilterData) then
                      FOnFilterData(Self);
          sbFind: if Assigned(FOnFindData) then
                    FOnFindData(Self);
          sbPrint: if Assigned(FOnPrintData) then
                     FOnPrintData(Self);
          sbExport: if Assigned(FOnExportData) then
                      FOnExportData(Self);
          sbImport: if Assigned(FOnImportData) then
                      FOnImportData(Self);
        end;
        if not (csDesigning in ComponentState) and Assigned(FOnNavClick) then
          FOnNavClick(Self, Index);
      end;
    except
    end;
  end;
end;

procedure TSMDBNavigator.RefreshData;
var bookPosition: TBookMark;
    boolContinue: Boolean;
begin
  boolContinue := True;

  {если нужно, сохраняем введенные данные}
  if Assigned(FDataLink.DataSet) then
  begin
     with FDataLink.DataSet do
     begin
       if (State in [dsInsert, dsEdit]) and CanModify then Post;
       if (FDataLink.DataSet is TBDEDataSet) then
         with (FDataLink.DataSet as TBDEDataSet) do
         begin
           if CachedUpdates and UpdatesPending then
             try
               case MessageDlg(strSaveChanges, mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
                 mrYes: ApplyUpdates;
                 mrNo: CancelUpdates;
                 else
                   boolContinue := False;
               end;
             except
               MessageDlg(strErrSaveChanges, mtError, [mbOk], 0);
               boolContinue := False;
             end;
           end;

       if boolContinue then
       begin
         {запоминаем текущую позицию}
         bookPosition := GetBookmark;

         {закрываем и открываем выборку}
         Active := False;
         Active := True;

         {восстанавливаем сохраненную текущую позицию}
         try
           GotoBookmark(bookPosition);
         except
           First;
         end;
         FreeBookmark(bookPosition);
       end;
     end;
  end;
end;

procedure TSMDBNavigator.CopyRecord;
var varCopyData: Variant;
    i: Integer;
begin
  with DataSource.DataSet do
  begin
    varCopyData := VarArrayCreate([0, FieldCount-1], varVariant);
    for i := 0 to FieldCount-1 do
      varCopyData[i] := Fields[i].Value;

    Insert;

    for i := 0 to FieldCount-1 do
      Fields[i].Value := varCopyData[i];
  end;
end;

procedure TSMDBNavigator.WMSetFocus(var Message: TWMSetFocus);
begin
  Buttons[FocusedButton].Invalidate;
end;

procedure TSMDBNavigator.WMKillFocus(var Message: TWMKillFocus);
begin
  Buttons[FocusedButton].Invalidate;
end;

procedure TSMDBNavigator.KeyDown(var Key: Word; Shift: TShiftState);
var
  NewFocus: TSMNavigateBtn;
  OldFocus: TSMNavigateBtn;
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

procedure TSMDBNavigator.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;

procedure TSMDBNavigator.DataChanged;
var
  UpEnable, DnEnable, DataEnable: Boolean;
begin
  UpEnable := Enabled and FDataLink.Active and not FDataLink.DataSet.BOF;
  DnEnable := Enabled and FDataLink.Active and not FDataLink.DataSet.EOF;
  DataEnable := Enabled and FDataLink.Active and
                FDataLink.DataSet.CanModify and
                not (FDataLink.DataSet.BOF and FDataLink.DataSet.EOF);
  Buttons[sbFirst].Enabled := UpEnable;
  Buttons[sbPrior].Enabled := UpEnable;
  Buttons[sbNext].Enabled := DnEnable;
  Buttons[sbLast].Enabled := DnEnable;
  Buttons[sbCopy].Enabled := DataEnable;
  Buttons[sbDelete].Enabled := DataEnable;
  Buttons[sbFilter].Enabled := Enabled and FDataLink.Active;
  Buttons[sbFind].Enabled := Enabled and FDataLink.Active;
  Buttons[sbPrint].Enabled := Enabled and FDataLink.Active;
  Buttons[sbExport].Enabled := Enabled and FDataLink.Active;
  Buttons[sbImport].Enabled := Enabled and FDataLink.Active;
  Buttons[sbChoice].Enabled := UpEnable or DnEnable;
  Buttons[sbClear].Enabled := True;
end;

procedure TSMDBNavigator.EditingChanged;
var CanModify: Boolean;
begin
  CanModify := Enabled and FDataLink.Active and FDataLink.DataSet.CanModify;
  Buttons[sbInsert].Enabled := CanModify;
  Buttons[sbCopy].Enabled := CanModify and not FDataLink.Editing;
  Buttons[sbEdit].Enabled := CanModify and not FDataLink.Editing;
  Buttons[sbPost].Enabled := CanModify and FDataLink.Editing;
  Buttons[sbCancel].Enabled := CanModify and FDataLink.Editing;
  Buttons[sbRefresh].Enabled := Enabled and FDataLink.Active {CanModify};

  Buttons[sbFilter].Enabled := Enabled and FDataLink.Active and not FDataLink.Editing;
  Buttons[sbFind].Enabled := Enabled and FDataLink.Active and not FDataLink.Editing;
  Buttons[sbPrint].Enabled := Enabled and FDataLink.Active and not FDataLink.Editing;
  Buttons[sbExport].Enabled := Enabled and FDataLink.Active and not FDataLink.Editing;
  Buttons[sbImport].Enabled := Enabled and FDataLink.Active and not FDataLink.Editing;
  Buttons[sbClear].Enabled := Enabled and FDataLink.Active and not FDataLink.Editing;
  Buttons[sbChoice].Enabled := Buttons[sbClear].Enabled and
                               not (FDataLink.DataSet.BOF and FDataLink.DataSet.EOF);
end;

procedure TSMDBNavigator.ActiveChanged;
var i: TSMNavigateBtn;
begin
  if not (Enabled and FDataLink.Active) then
    for i := Low(Buttons) to High(Buttons) do
      Buttons[i].Enabled := False
  else
  begin
    DataChanged;
    EditingChanged;
  end;
end;

procedure TSMDBNavigator.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if not (csLoading in ComponentState) then
    ActiveChanged;
end;

procedure TSMDBNavigator.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if not (csLoading in ComponentState) then
    ActiveChanged;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TSMDBNavigator.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TSMDBNavigator.Loaded;
var
  W, H: Integer;
begin
  inherited Loaded;
  W := Width;
  H := Height;
  AdjustSizeNav(W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds (Left, Top, W, H);
  InitHints;
  ActiveChanged;
end;

{TSMNavButton}

destructor TSMNavButton.Destroy;
begin
  if FRepeatTimer <> nil then
    FRepeatTimer.Free;
  inherited Destroy;
end;

procedure TSMNavButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
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

procedure TSMNavButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
                                  X, Y: Integer);
begin
  inherited MouseUp (Button, Shift, X, Y);
  if FRepeatTimer <> nil then
    FRepeatTimer.Enabled  := False;
end;

procedure TSMNavButton.TimerExpired(Sender: TObject);
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

procedure TSMNavButton.Paint;
var
  R: TRect;
begin
  inherited Paint;
  if (GetFocus = Parent.Handle) and
     (FIndex = TSMDBNavigator(Parent).FocusedButton) then
  begin
    R := Bounds(0, 0, Width, Height);
    InflateRect(R, -3, -3);
    if FState = bsDown then
      OffsetRect(R, 1, 1);
    DrawFocusRect(Canvas.Handle, R);
  end;
end;

{ TSMNavDataLink }

constructor TSMNavDataLink.Create(ANav: TSMDBNavigator);
begin
  inherited Create;
  FNavigator := ANav;
end;

destructor TSMNavDataLink.Destroy;
begin
  FNavigator := nil;
  inherited Destroy;
end;

procedure TSMNavDataLink.EditingChanged;
begin
  if FNavigator <> nil then FNavigator.EditingChanged;
end;

procedure TSMNavDataLink.DataSetChanged;
begin
  if FNavigator <> nil then FNavigator.DataChanged;
end;

procedure TSMNavDataLink.ActiveChanged;
begin
  if FNavigator <> nil then FNavigator.ActiveChanged;
end;

end.
