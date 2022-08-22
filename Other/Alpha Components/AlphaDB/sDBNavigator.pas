unit sDBNavigator;
{$I sDefs.inc}
{$R SDBRES.RES}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, dbctrls, db, dbconsts, math, ImgList,
  {$IFNDEF DELPHI5} Types, {$ENDIF}
  sConst, sPanel, sDefaults, sSpeedButton;


type
  TsNavButton = class;
  TsNavDataLink = class;
{$IFDEF D2006}
  TButtonSet = set of TNavigateBtn;
{$ENDIF}

{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsDBNavigator = class(TsPanel)
  private
    FHints,
    FDefHints: TStrings;

    FOnNavClick,
    FBeforeAction: ENavClick;

    FNumGlyphs,
    FFirstImageIndex: integer;

    MinBtnSize: TPoint;
    FDataLink: TsNavDataLink;
    FocusedButton: TNavigateBtn;
    FImages: TCustomImageList;
    FVisibleButtons: TButtonSet;
    FDisabledGlyphKind: TsDisabledGlyphKind;
    FUseColoredGlyphs,
    FConfirmDelete: Boolean;
    function ButtonWidth: integer;
    function ButtonsCount: integer;
    procedure BtnMouseDown (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ClickHandler(Sender: TObject);
    function GetDataSource: TDataSource;
    function GetHints: TStrings;
    procedure HintsChanged(Sender: TObject);
    procedure SetDataSource(Value: TDataSource);
    procedure SetHints(Value: TStrings);
    procedure SetSize(var W: Integer; var H: Integer);
    procedure SetVisible(Value: TButtonSet);
    procedure WMSize             (var Message: TWMSize);              message WM_SIZE;
    procedure WMSetFocus         (var Message: TWMSetFocus);          message WM_SETFOCUS;
    procedure WMKillFocus        (var Message: TWMKillFocus);         message WM_KILLFOCUS;
    procedure WMGetDlgCode       (var Message: TWMGetDlgCode);        message WM_GETDLGCODE;
    procedure CMEnabledChanged   (var Message: TMessage);             message CM_ENABLEDCHANGED;
    procedure WMWindowPosChanging(var Message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
    procedure SetFirstImageIndex(const Value: integer);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetNumGlyphs(const Value: integer);
    procedure SetDisabledGlyphKind(const Value: TsDisabledGlyphKind);
    procedure SetUseColoredGlyphs(const Value: boolean);
  protected
    procedure DataChanged;
    procedure EditingChanged;
    procedure ActiveChanged;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CalcMinSize(var W, H: Integer);
    function MarginWidth: integer;
    property NumGlyphs: integer read FNumGlyphs Write SetNumGlyphs default 1;
    procedure InitHints;
  public
    Buttons: array [TNavigateBtn] of TsNavButton;
    constructor Create(AOwner: TComponent); override;
    procedure InitButtons;
    procedure UpdateGlyphs;
    procedure CreateWnd; override;
    destructor Destroy; override;
    procedure WndProc (var Message: TMessage); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure BtnClick(Index: TNavigateBtn); virtual;
    procedure Loaded; override;
  published
    property Images: TCustomImageList read FImages write SetImages;
    property FirstImageIndex: integer read FFirstImageIndex write SetFirstImageIndex default 0;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DisabledGlyphKind: TsDisabledGlyphKind read FDisabledGlyphKind write SetDisabledGlyphKind default [];//dgBlended, dgGrayed];
    property VisibleButtons: TButtonSet read FVisibleButtons write SetVisible default [nbFirst, nbPrior, nbNext,
                                  nbLast, nbInsert, nbDelete, nbEdit, nbPost, nbCancel, nbRefresh];
    property Hints: TStrings read GetHints write SetHints;
    property ConfirmDelete: Boolean read FConfirmDelete write FConfirmDelete default True;
    property BeforeAction: ENavClick read FBeforeAction write FBeforeAction;
    property OnClick: ENavClick read FOnNavClick write FOnNavClick;
    property UseColoredGlyphs: boolean read FUseColoredGlyphs write SetUseColoredGlyphs default False;
    property Width default 240;
    property Height default 25;
  end;


  TsNavButton = class(TsSpeedButton)
  private
    FIndex: TNavigateBtn;
    FRepeatTimer: TTimer;
    FNavStyle: TNavButtonStyle;
    procedure TimerExpired(Sender: TObject);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp  (Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DrawGlyph; override;
  public
    procedure Paint; override;
    destructor Destroy; override;
    constructor Create (AOwner: TComponent); override;
    property Index: TNavigateBtn read FIndex write FIndex;
    property NavStyle: TNavButtonStyle read FNavStyle write FNavStyle;
  end;


  TsNavDataLink = class(TDataLink)
  private
    FNavigator: TsDBNavigator;
  protected
    procedure EditingChanged; override;
    procedure DataSetChanged; override;
    procedure ActiveChanged; override;
  public
    constructor Create(ANav: TsDBNavigator);
    destructor Destroy; override;
  end;


implementation

uses
  Buttons,
  {$IFDEF DELPHI_XE3} Themes, {$ENDIF}
  acntUtils, sMessages, sSkinProps, sDialogs, sGraphUtils, sCommonData, sAlphaGraph, sGlyphUtils, acDBBtns;


resourcestring
  SFirstRecord          = 'First record';
  SPriorRecord          = 'Prior record';
  SNextRecord           = 'Next record';
  SLastRecord           = 'Last record';
  SInsertRecord         = 'Insert record';
  SDeleteRecord         = 'Delete record';
  SEditRecord           = 'Edit record';
  SPostEdit             = 'Post edit';
  SCancelEdit           = 'Cancel edit';
  SRefreshRecord        = 'Refresh data';
  SDeleteRecordQuestion = 'Delete record?';
{$IFDEF DELPHI_XE2}
  SApplyUpdEdit         = 'Apply changes for all changed records';
  SCancelUpdEdit        = 'Cancels edits for all edited records';
{$ENDIF}

var
  BtnTypeNames: array [TNavigateBtn] of PChar = ('DBN_FIRST', 'DBN_PRIOR', 'DBN_NEXT',
    'DBN_LAST', 'DBN_INSERT', 'DBN_DELETE', 'DBN_EDIT', 'DBN_POST', 'DBN_CANCEL', 'DBN_REFRESH'{$IFDEF DELPHI_XE2}, 'DBN_POST', 'DBN_CANCEL'{$ENDIF});

  BtnHintId: array [TNavigateBtn] of Pointer = (@SFirstRecord, @SPriorRecord,
    @SNextRecord, @SLastRecord, @SInsertRecord, @SDeleteRecord, @SEditRecord,
    @SPostEdit, @SCancelEdit, @SRefreshRecord {$IFDEF DELPHI_XE2}, @SApplyUpdEdit, @SCancelUpdEdit{$ENDIF});


procedure TsDBNavigator.ActiveChanged;
var
  I: TNavigateBtn;
begin
  if Buttons[Low(Buttons)] <> nil then
    if not (Enabled and FDataLink.Active) then begin
      for I := Low(Buttons) to High(Buttons) do
        if Buttons[I] <> nil then
          Buttons[I].Enabled := False
    end
    else begin
      DataChanged;
      EditingChanged;
    end;
end;


procedure TsDBNavigator.BtnClick(Index: TNavigateBtn);
begin
  if (DataSource <> nil) and (DataSource.State <> dsInactive) then begin
    if not (csDesigning in ComponentState) and Assigned(FBeforeAction) then
      FBeforeAction(Self, Index);

    with DataSource.DataSet do
      case Index of
        nbPrior:   Prior;
        nbNext:    Next;
        nbFirst:   First;
        nbLast:    Last;
        nbInsert:  Insert;
        nbEdit:    Edit;
        nbCancel:  Cancel;
        nbPost:    Post;
        nbRefresh: Refresh;
        nbDelete:
          if not FConfirmDelete or (sMessageDlg(SDeleteRecordQuestion, mtConfirmation, mbOKCancel, 0) <> idCancel) then
            Delete;
      end;
  end;
  if not (csDesigning in ComponentState) and Assigned(FOnNavClick) then
    FOnNavClick(Self, Index);
end;


procedure TsDBNavigator.BtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  OldFocus: TNavigateBtn;
begin
  OldFocus := FocusedButton;
  FocusedButton := TsNavButton(Sender).Index;
  if TabStop and (GetFocus <> Handle) and CanFocus then begin
    SetFocus;
    if GetFocus <> Handle then
      Exit;
  end
  else
    if TabStop and (GetFocus = Handle) and (OldFocus <> FocusedButton) then begin
      Buttons[OldFocus].Invalidate;
      Buttons[FocusedButton].Invalidate;
    end;
end;


function TsDBNavigator.ButtonsCount: integer;
var
  i: TNavigateBtn;
begin
  Result := 0;
  for I := Low(Buttons) to High(Buttons) do
    if (Buttons[I] <> nil) and Buttons[I].Visible then
      Inc(Result);

  if Result = 0 then
    Inc(Result);
end;


function TsDBNavigator.ButtonWidth: Integer;
begin
  ButtonWidth := (Width - 2 * MarginWidth) div ButtonsCount;
end;


procedure TsDBNavigator.CalcMinSize(var W, H: Integer);
begin
  if not (csLoading in ComponentState) and (Buttons[nbFirst] <> nil) then begin
    W := Max(W, ButtonsCount * MinBtnSize.X + 2 * MarginWidth);
    H := Max(H, MinBtnSize.Y + 2 * MarginWidth);
  end;
end;


procedure TsDBNavigator.ClickHandler(Sender: TObject);
begin
  BtnClick(TsNavButton(Sender).Index);
end;


procedure TsDBNavigator.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if not (csLoading in ComponentState) then
    ActiveChanged;
end;


constructor TsDBNavigator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SkinData.COC := COC_TsToolBar;
  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] + [csOpaque];
  FDataLink := TsNavDataLink.Create(Self);
  FDisabledGlyphKind := [];//dgBlended, dgGrayed];
  FVisibleButtons := [nbFirst, nbPrior, nbNext, nbLast, nbInsert, nbDelete, nbEdit, nbPost, nbCancel, nbRefresh];
  FHints := TStringList.Create;
  TStringList(FHints).OnChange := HintsChanged;
  Width := 240;
  Height := 25;
  FocusedButton := nbFirst;
  FConfirmDelete := True;
  FullRepaint := False;
  FNumGlyphs := 1;
  FUseColoredGlyphs := False;
  FFirstImageIndex := 0;
end;


procedure TsDBNavigator.CreateWnd;
var
  I: TNavigateBtn;
  W, H: Integer;
begin
  inherited;
  InitButtons;
  InitHints;
  for I := Low(Buttons) to High(Buttons) do
    if Buttons[I] <> nil then begin
      if Assigned(Images) and (Images <> acCharImages) then
        Buttons[I].ImageIndex := FFirstImageIndex + ord(I);

      Buttons[I].SkinData.SkinManager := SkinData.SkinManager;
    end;
    
  W := Width;
  H := Height;
  SetSize(W, H);
  ActiveChanged;
end;


procedure TsDBNavigator.DataChanged;
var
  UpEnable, DnEnable: Boolean;
begin
  if not (csLoading in ComponentState) then begin
    UpEnable := Enabled and FDataLink.Active and not FDataLink.DataSet.BOF;
    DnEnable := Enabled and FDataLink.Active and not FDataLink.DataSet.EOF;
    if Buttons[nbFirst] <> nil then begin
      Buttons[nbFirst].Enabled  := UpEnable;
      Buttons[nbPrior].Enabled  := UpEnable;
      Buttons[nbNext].Enabled   := DnEnable;
      Buttons[nbLast].Enabled   := DnEnable;
      Buttons[nbDelete].Enabled := Enabled and FDataLink.Active and FDataLink.DataSet.CanModify and
                                 not (FDataLink.DataSet.BOF and FDataLink.DataSet.EOF);
    end;
  end;
end;


destructor TsDBNavigator.Destroy;
begin
  FDefHints.Free;
  FDataLink.Free;
  FHints.Free;
  FDataLink := nil;
  inherited Destroy;
end;


procedure TsDBNavigator.EditingChanged;
var
  CanModify: Boolean;
begin
  if not (csLoading in ComponentState) then begin
    CanModify := Enabled and FDataLink.Active and FDataLink.DataSet.CanModify;
    if Buttons[nbInsert] <> nil then begin
      Buttons[nbInsert].Enabled  := CanModify;
      Buttons[nbEdit].Enabled    := CanModify and not FDataLink.Editing;
      Buttons[nbPost].Enabled    := CanModify and FDataLink.Editing;
      Buttons[nbCancel].Enabled  := CanModify and FDataLink.Editing;
      Buttons[nbRefresh].Enabled := CanModify;
    end;
  end;
end;


function TsDBNavigator.GetDataSource: TDataSource;
begin
  if Assigned(FDataLink) and Assigned(FDataLink.DataSource) then
    Result := FDataLink.DataSource
  else
    Result := nil;
end;


function TsDBNavigator.GetHints: TStrings;
begin
  if (csDesigning in ComponentState) and ([csWriting, csReading] * ComponentState = []) and (FHints.Count = 0) then
    Result := FDefHints
  else
    Result := FHints;
end;


procedure TsDBNavigator.HintsChanged(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
    InitHints;
end;


procedure TsDBNavigator.InitButtons;
var
  I: TNavigateBtn;
  Btn: TsNavButton;
  X: Integer;
begin
  MinBtnSize := Point(6, 6);
  X := MarginWidth;
  for I := Low(Buttons) to High(Buttons) do begin
    Btn := TsNavButton.Create(Self);
    Btn.Flat := True;
    Btn.Index := I;
    Btn.Visible := I in FVisibleButtons;
    Btn.Enabled := True;
    Btn.DisabledGlyphKind := DisabledGlyphKind;
    Btn.SetBounds(X, X, MinBtnSize.X, Height - 2 * X);
    Btn.OnClick := ClickHandler;
    Btn.OnMouseDown := BtnMouseDown;
    Btn.Images := Images;
    Btn.Parent := Self;
    Buttons[I] := Btn;
    X := X + MinBtnSize.X;
  end;
  UpdateGlyphs;
  Buttons[nbPrior].NavStyle := Buttons[nbPrior].NavStyle + [dbctrls.nsAllowTimer];
  Buttons[nbNext].NavStyle  := Buttons[nbNext] .NavStyle + [dbctrls.nsAllowTimer];
end;


procedure TsDBNavigator.InitHints;
var
  i: Integer;
  j: TNavigateBtn;
begin
  if not Assigned(FDefHints) then begin
    FDefHints := TStringList.Create;
    for j := Low(Buttons) to High(Buttons) do
      if Buttons[j] <> nil then
        FDefHints.Add(LoadResString(BtnHintId[j]));
  end;
  for j := Low(Buttons) to High(Buttons) do
    Buttons[j].Hint := FDefHints[Ord(j)];

  j := Low(Buttons);
  for i := 0 to (FHints.Count - 1) do
    if IsValidIndex(i, FHints.Count) then begin
      if FHints.Strings[i] <> '' then
        if Buttons[j] <> nil then
          Buttons[j].Hint := FHints.Strings[i];

      if j = High(Buttons) then
        Exit;

      Inc(j);
    end;
end;


procedure TsDBNavigator.KeyDown(var Key: Word; Shift: TShiftState);
var
  NewFocus, OldFocus: TNavigateBtn;
begin
  OldFocus := FocusedButton;
  case Key of
    VK_RIGHT: begin
      NewFocus := FocusedButton;
      repeat
        if NewFocus < High(Buttons) then
          NewFocus := Succ(NewFocus);
      until
        (NewFocus = High(Buttons)) or Buttons[NewFocus].Visible;
        
      if NewFocus <> FocusedButton then begin
        FocusedButton := NewFocus;
        Buttons[OldFocus].Invalidate;
        Buttons[FocusedButton].Invalidate;
      end;
    end;

    VK_LEFT: begin
      NewFocus := FocusedButton;
      repeat
        if NewFocus > Low(Buttons) then
          NewFocus := Pred(NewFocus);
      until
        (NewFocus = Low(Buttons)) or Buttons[NewFocus].Visible;

      if NewFocus <> FocusedButton then begin
        FocusedButton := NewFocus;
        Buttons[OldFocus].Invalidate;
        Buttons[FocusedButton].Invalidate;
      end;
    end;

    VK_SPACE:
      if Buttons[FocusedButton].Enabled then
        Buttons[FocusedButton].Click;
  end;
end;


procedure TsDBNavigator.Loaded;
var
  W, H: Integer;
begin
  inherited Loaded;
  W := Width;
  H := Height;
  SetSize(W, H);
  if (Images = nil) and (Images <> acCharImages) then
    Images := acCharImages;
end;


function TsDBNavigator.MarginWidth: integer;
begin
  Result := integer(BorderStyle = bsSingle) * (1 + 3 * integer(Ctl3d)) + BorderWidth +
            (integer(BevelOuter <> bvNone) + integer(BevelInner <> bvNone)) * BevelWidth;
end;


procedure TsDBNavigator.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if (FDataLink <> nil) and (AComponent = DataSource) then
      DataSource := nil
    else
      if AComponent = Images then
        Images := nil;
end;


procedure TsDBNavigator.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  W, H: Integer;
begin
  W := AWidth;
  H := AHeight;
  if not HandleAllocated then
    SetSize(W, H);

  inherited SetBounds(ALeft, ATop, aWidth, aHeight);
end;


procedure TsDBNavigator.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if not (csLoading in ComponentState) then
    ActiveChanged;

  if Value <> nil then
    Value.FreeNotification(Self);
end;


procedure TsDBNavigator.SetDisabledGlyphKind(const Value: TsDisabledGlyphKind);
var
  I: TNavigateBtn;
begin
  if FDisabledGlyphKind <> Value then begin
    FDisabledGlyphKind := Value;
    if Visible or (csDesigning in ComponentState) then
      for I := Low(Buttons) to High(Buttons) do
        if Buttons[I] <> nil then
          Buttons[I].DisabledGlyphKind := DisabledGlyphKind;
  end;
end;

procedure TsDBNavigator.SetFirstImageIndex(const Value: integer);
var
  I: TNavigateBtn;
begin
  if FFirstImageIndex <> Value then begin
    FFirstImageIndex := Value;
    if Buttons[Low(Buttons)] <> nil then
      for I := Low(Buttons) to High(Buttons) do
        if Buttons[I] <> nil then
          Buttons[I].ImageIndex := Value + ord(I);
  end;
end;


procedure TsDBNavigator.SetHints(Value: TStrings);
begin
  if value <> nil then 
    if Value.Text = FDefHints.Text then
      FHints.Clear
    else
      FHints.Assign(Value);
end;


procedure TsDBNavigator.SetImages(const Value: TCustomImageList);
begin
  if FImages <> Value then begin
    FImages := Value;
    if not (csLoading in ComponentState) then begin
      UpdateGlyphs;
      SkinData.Invalidate;
    end;
  end;
end;


procedure TsDBNavigator.SetNumGlyphs(const Value: integer);
var
  I: TNavigateBtn;
begin
  if FNumGlyphs <> Value then begin
    FNumGlyphs := Value;
    for I := Low(Buttons) to High(Buttons) do
      if Buttons[I] <> nil then
        Buttons[I].NumGlyphs := Value;
  end;
end;


procedure TsDBNavigator.SetSize(var W, H: Integer);
var
  I: TNavigateBtn;
  Space, Temp, Remain, X: Integer;
begin
  if not (csLoading in ComponentState) and (Parent <> nil) and (Buttons[nbFirst] <> nil) then begin
    Temp := ButtonsCount * ButtonWidth;
    if Align = alNone then
      W := Temp + 2 * MarginWidth;

    X := MarginWidth;
    Remain := Width - 2 * MarginWidth - Temp;
    Temp := ButtonsCount div 2;
    for I := Low(Buttons) to High(Buttons) do
      if Buttons[I] <> nil then
        if Buttons[I].Visible then begin
          Space := 0;
          if Remain <> 0 then begin
            Dec(Temp, Remain);
            if Temp < 0 then begin
              Inc(Temp, ButtonsCount);
              Space := 1;
            end;
          end;
          Buttons[I].SetBounds(X, MarginWidth, ButtonWidth + Space, Height - 2 * MarginWidth - 1);
          Inc(X, ButtonWidth + Space);
        end
        else
          Buttons[I].SetBounds(Width + 1, MarginWidth, ButtonWidth, Height - 2 * MarginWidth - 1);
  end;
end;


procedure TsDBNavigator.SetUseColoredGlyphs(const Value: boolean);
begin
  if FUseColoredGlyphs <> Value then begin
    FUseColoredGlyphs := Value;
    if not (csLoading in ComponentState) then begin
      UpdateGlyphs;
      SkinData.Invalidate;
    end;
  end;
end;


procedure TsDBNavigator.SetVisible(Value: TButtonSet);
var
  I: TNavigateBtn;
  W, H: Integer;
begin
  W := Width;
  H := Height;
  FVisibleButtons := Value;
  if not (csLoading in ComponentState) then
    for I := Low(Buttons) to High(Buttons) do
      if Buttons[I] <> nil then
        Buttons[I].Visible := I in FVisibleButtons;

  SetSize(W, H);
  Invalidate;
end;


procedure LoadGlyph(Btn: TsNavButton; aBtn: TNavigateBtn);
var
  s: string;
  ResBitmap: TBitmap;
begin
  if (TsDBNavigator(Btn.Parent).Images = nil) or (TsDBNavigator(Btn.Parent).Images = acCharImages) then begin
    ResBitmap := TBitmap.Create;

    s := BtnTypeNames[aBtn];
    s := 's' + s;
    ResBitmap.LoadFromResourceName(hInstance, s);
    Btn.Glyph.PixelFormat := pf32bit;
    Btn.Glyph.Width := ResBitmap.Width;
    Btn.Glyph.Height := ResBitmap.Height;
    BitBlt(Btn.Glyph.Canvas.Handle, 0, 0, Btn.Glyph.Width, Btn.Glyph.Height, ResBitmap.Canvas.Handle, 0, 0, SRCCOPY);
    ChangeBitmapPixels(Btn.Glyph, MakeAlphaPixel, 0, Btn.Glyph.Canvas.Pixels[0, Btn.Glyph.Height - 1]);
    ResBitmap.Free;
    Btn.SkinData.BGChanged := True;
  end;
end;


procedure TsDBNavigator.UpdateGlyphs;
var
  I: TNavigateBtn;
begin
  if Buttons[Low(Buttons)] <> nil then
    for I := Low(Buttons) to High(Buttons) do
      if sConst.ac_OldGlyphsMode then
        LoadGlyph(Buttons[I], I)
      else begin
        Buttons[I].Glyph.Assign(nil);
        if (Images = acCharImages) or (Images = nil) then begin
          Buttons[I].Images := acCharImages;
          Buttons[I].ImageIndex := DBGlyphIndex(I);
        end
        else begin
          Buttons[I].Images := FImages;
          Buttons[I].ImageIndex := FFirstImageIndex + ord(I);
        end;
      end;
end;


procedure TsDBNavigator.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;


procedure TsDBNavigator.WMKillFocus(var Message: TWMKillFocus);
begin
  Buttons[FocusedButton].Invalidate;
end;


procedure TsDBNavigator.WMSetFocus(var Message: TWMSetFocus);
begin
  Buttons[FocusedButton].Invalidate;
end;


procedure TsDBNavigator.WMSize(var Message: TWMSize);
var
  W, H: Integer;
begin
  inherited;
  W := Width;
  H := Height;
  SetSize(W, H);
end;


procedure TsDBNavigator.WMWindowPosChanging(var Message: TWMWindowPosChanging);
begin
  inherited;
  if SWP_NOSIZE and Message.WindowPos.Flags = 0 then
    CalcMinSize(Message.WindowPos.cx, Message.WindowPos.cy);
end;


procedure TsDBNavigator.WndProc(var Message: TMessage);
var
  i: TNavigateBtn;
begin
  inherited;
  if Message.Msg = SM_ALPHACMD then
    case Message.WParamHi of
      AC_SETNEWSKIN, AC_REFRESH, AC_REMOVESKIN:
        for i := Low(Buttons) to High(Buttons) do
          if Buttons[I] <> nil then
            Buttons[i].Perform(Message.Msg, Message.WParam, Message.LParam);
    end;
end;


type
  TAccessCommonData = class(TsCommonData);


constructor TsNavButton.Create(AOwner: TComponent);
begin
  inherited;
  ShowCaption := False;
  SkinData.SkinManager := TAccessCommonData(TsPanel(AOwner).SkinData).FSkinManager;
  SkinData.SkinSection := s_TOOLBUTTON;
  Self.DisabledGlyphKind := Self.DisabledGlyphKind + [dgBlended];
  Self.DisabledKind := Self.DisabledKind + [dkBlended];
  Glyph.Assign(nil);
end;


destructor TsNavButton.Destroy;
begin
  if FRepeatTimer <> nil then
    FRepeatTimer.Free;

  inherited Destroy;
end;


procedure TsNavButton.DrawGlyph;
var
  R: TRect;
  C: TColor;
  S0, S: PRGBAArray_S;
  D0, D: PRGBAArray_D;
  w, DeltaD, DeltaS, x, y: integer;
begin
  if TsDBNavigator(Parent).FUseColoredGlyphs then
    GlyphColorTone := DBGlyphColor(SkinData, Index, CurrentState)
  else
    GlyphColorTone := clNone;

  if not Glyph.Empty then begin
    R.Left := (Width - Glyph.Width) div 2;
    R.Top := (Height - Glyph.Height) div 2;
    R.Right := R.Left + Glyph.Width;
    R.Bottom := R.Top + Glyph.Height;
    Glyph.Handle; // Generate a handle
    w := Glyph.Width - 1;
    if TsDBNavigator(Parent).FUseColoredGlyphs and not ((Grayed or SkinData.SkinManager.Effects.DiscoloredGlyphs) and (CurrentState = 0)) then
      C := SwapRedBlue(GlyphColorTone)
    else
      C := clBlack;

    if (R.Bottom < Height) and (R.Right < Width) then
      if InitLine(SkinData.FCacheBmp, Pointer(D0), DeltaD) and InitLine(Glyph, Pointer(S0), DeltaS) then
        for y := 0 to Glyph.Height - 1 do begin
          S := Pointer(PAnsiChar(S0) + DeltaS * Y);
          D := Pointer(PAnsiChar(D0) + DeltaD * (Y + R.Top));
          for x := 0 to w do
            if S[X].SA = MaxByte then
              D[X + R.Left].DC := C;
        end;
  end
  else
    inherited;
end;


procedure TsNavButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown (Button, Shift, X, Y);
  if dbctrls.nsAllowTimer in FNavStyle then begin
    if FRepeatTimer = nil then
      FRepeatTimer := TTimer.Create(Self);

    FRepeatTimer.OnTimer := TimerExpired;
    FRepeatTimer.Interval := InitRepeatPause;
    FRepeatTimer.Enabled  := True;
  end;
end;


procedure TsNavButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp (Button, Shift, X, Y);
  if FRepeatTimer <> nil then
    FRepeatTimer.Enabled  := False;
end;


procedure TsNavButton.Paint;
begin
  if [csDestroying, csLoading] * ComponentState = [] then begin
    if (GetFocus = Parent.Handle) and (FIndex = TsDBNavigator (Parent).FocusedButton) then
      SkinData.FFocused := True;

    inherited Paint;
    SkinData.FFocused := False;
  end;
end;


procedure TsNavButton.TimerExpired(Sender: TObject);
begin
  FRepeatTimer.Interval := RepeatPause;
  if (FState = bsDown) and MouseCapture then
    try
      Click;
    except
      FRepeatTimer.Enabled := False;
      raise;
    end;
end;


procedure TsNavDataLink.ActiveChanged;
begin
  if FNavigator <> nil then
    FNavigator.ActiveChanged;
end;


constructor TsNavDataLink.Create(ANav: TsDBNavigator);
begin
  inherited Create;
  FNavigator := ANav;
  VisualControl := True;
end;


procedure TsNavDataLink.DataSetChanged;
begin
  if FNavigator <> nil then
    FNavigator.DataChanged;
end;


destructor TsNavDataLink.Destroy;
begin
  FNavigator := nil;
  inherited Destroy;
end;


procedure TsNavDataLink.EditingChanged;
begin
  if FNavigator <> nil then
    FNavigator.EditingChanged;
end;

end.










