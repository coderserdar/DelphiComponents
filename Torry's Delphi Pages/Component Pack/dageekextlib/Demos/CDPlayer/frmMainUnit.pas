{*******************************************************}
{                                                       }
{       CD Player                                       }
{       Extension Library example of                    }
{       TELTrayIcon, TELInstanceChecker                 }
{       ELPackStrings, ELUnpackStrings                  }
{       See Readme.txt for comments                     }
{                                                       }
{       (c) 2000 - 2001, Balabuyev Yevgeny              }
{       E-mail: stalcer@rambler.ru                      }
{                                                       }
{*******************************************************}

{ See Initialization section of this unit to find example of
  TELInstanceChecker usage }

unit frmMainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, MPlayer, ExtCtrls, ComCtrls, MMSystem, Buttons, AppEvnts,
  ImgList, Menus, ELControls, Registry, ELUtils;


  { TDisplay }

type
  TDisplay = class
  private
    FLabel: TLabel;
    FTimer: TTimer;
    FText: string;
    FIsTempText: Boolean;
    function GetHint: string;
    procedure SetHint(const Value: string);
    procedure SetText(const Value: string);
    function GetTempText: string;
  protected
    procedure FTimerTimer(Sender: TObject);
  public
    constructor Create(ALabel: TLabel);
    destructor Destroy; override;
    procedure SetTempText(S: string);
    procedure CancelTempText;
    property IsTempText: Boolean read FIsTempText;
    property TempText: string read GetTempText;
    property Text: string read FText write SetText;
    property Hint: string read GetHint write SetHint;
  end;

const
  DISPLAY_TEMP_TEXT_INTERVAL = 1000;


  { TPositioner }

type
  TMouseEventType = (meMove, meClick, meExit);
  TOnMouse = procedure(MouseEventType: TMouseEventType;
    Pos: Integer) of object;

  TPositioner = class
  private
    FProgressBar: TProgressBar;
    FPosition: Integer;
    FOnMouse: TOnMouse;
    OldWinProc: TWndMethod;
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
    procedure SetPosition(const Value: Integer);
  protected
    procedure FProgressBarWinProc(var Message: TMessage);
    procedure FProgressBarMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure FProgressBarMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetMinMax;
    procedure UpdatePosition;
    procedure Mouse(MouseEventType: TMouseEventType; Pos: Integer);
    function GetProgressBarPosition(X, Y: Integer): Integer;
  public
    constructor Create(AProgressBar: TProgressBar);
    destructor Destroy; override;
    procedure Update;
    property Position: Integer read FPosition;
    property Enabled: Boolean read GetEnabled;
    property OnMouse: TOnMouse read FOnMouse write FOnMouse;
  end;


  { TfrmMain }

type
  TCDPlayerMode = (pmNotOpened, pmNoCD, pmStoped, pmPlaying);

  TfrmMain = class(TForm)
    ProgressBar1: TProgressBar;
    Timer1: TTimer;
    Panel1: TPanel;
    Label2: TLabel;
    Panel2: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    Bevel2: TBevel;
    Panel3: TPanel;
    SpeedButton7: TSpeedButton;
    ELTrayIcon1: TELTrayIcon;
    ApplicationEvents1: TApplicationEvents;
    ImageList1: TImageList;
    Image1: TImage;
    PopupMenu1: TPopupMenu;
    N1: TMenuItem;
    ImageList2: TImageList;
    Registerfiletypes1: TMenuItem;
    Close1: TMenuItem;
    Bevel1: TBevel;
    Label1: TLabel;
    procedure Timer1Timer(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
    procedure ELTrayIcon1Click(Sender: TObject);
    procedure ApplicationEvents1Deactivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure N1Click(Sender: TObject);
    procedure Registerfiletypes1Click(Sender: TObject);
    procedure Close1Click(Sender: TObject);
  private
    { Private declarations }
    Display: TDisplay;
    Positioner: TPositioner;
    FMode: TCDPlayerMode;
    FPowerOffMode: Boolean;
    FixedPopupMenuItemCount: Integer;
    procedure SetMode(const Value: TCDPlayerMode);
    procedure SetPowerOffMode(const Value: Boolean);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure UpdateIcon;
    procedure UpdateUI;
    procedure PlayNotify(Code: Integer);
    procedure PositionerMouse(MouseEventType: TMouseEventType;
      Pos: Integer);
    procedure PopupMenuTrackItemsClick(Sender: TObject);
    procedure InstanceCheckerResieveData(Sender: TObject; const AData: string);
  public
    { Public declarations }
    procedure DoCommand;
    property Mode: TCDPlayerMode read FMode;
    property PowerOffMode: Boolean read FPowerOffMode write SetPowerOffMode;
  end;

var
  ParamStrings: TStringList;
  InstanceChecker: TELInstanceChecker;

const
  CD_TIMER_INTERVAL = 300;
  NO_CD_TIMER_INTERVAL = 1000;
  DISPLAY_NO_CD = '- -';
  DISPLAY_HINT_PREFIX = 'Track: ';
  POPUPMENU_TRACK_PREFIX = 'Track';
  NOTCDDAFILE = 'File ''%S'' is not a CD audio file';
  CANNOTOPENFILE = 'Can not open file ''%S''';

var
  frmMain: TfrmMain;

function CreateDisplayString(Track, Min, Sec: Integer): string;
function CreateDisplayHint(Min, Sec: Integer): string;
function TMSF_To_CurTrackMS(Value: Integer): Integer;
function MSF_To_CurTrackMS(Value: Integer): Integer;
function CurTrackMS_To_TMSF(Track: Byte; Value: Integer): Integer;
function TMSFToTMS(TMSFValue: Integer): Integer;

procedure FillParamStrings(IgnoredPrefixChars: TSysCharSet; Strings: TStrings);

implementation

uses
  CD_Routines, dlgExitWindowsUnit;

{$R *.DFM}

{$IFNDEF VER140}
function GetModuleName(Module: HMODULE): string;
var
  ModName: array[0..MAX_PATH] of Char;
begin
  SetString(Result, ModName, Windows.GetModuleFileName(Module, ModName, SizeOf(ModName)));
end;
{$ENDIF}


procedure FillParamStrings(IgnoredPrefixChars: TSysCharSet; Strings: TStrings);

  function _IgnorePrefix(S: string): string;
  var
    I: Integer;
  begin
    I := 1;
    while (I <= Length(S)) and (S[I] in IgnoredPrefixChars) do Inc(I);
    Result := Copy(S, I, MAXINT);
  end;

var
  I: Integer;
  
begin
  Strings.Clear;
  for I := 1 to ParamCount do
    Strings.Add(_IgnorePrefix(ParamStr(I)));
end;

function CreateDisplayString(Track, Min, Sec: Integer): string;
var
  S1, S2: string;
begin
  if Min < 10 then S1 := '0' else S1 := '';
  if Sec < 10 then S2 := '0' else S2 := '';
  Result :=
    '[' + IntToStr(Track) + ']' + ' ' +
    S1 + IntToStr(Min) + ':' +
    S2 + IntToStr(Sec);
end;

function CreateDisplayHint(Min, Sec: Integer): string;
var
  S1, S2: string;
begin
  if Min < 10 then S1 := '0' else S1 := '';
  if Sec < 10 then S2 := '0' else S2 := '';
  Result :=
    DISPLAY_HINT_PREFIX +
    S1 + IntToStr(Min) + ':' +
    S2 + IntToStr(Sec);
end;

function TMSF_To_CurTrackMS(Value: Integer): Integer;
begin
  Result := (mci_TMSF_Minute(Value) * 60 + mci_TMSF_Second(Value)) * 1000;
end;

function MSF_To_CurTrackMS(Value: Integer): Integer;
begin
  Result := (mci_MSF_Minute(Value) * 60 + mci_MSF_Second(Value)) * 1000;
end;

function CurTrackMS_To_TMSF(Track: Byte; Value: Integer): Integer;
var
  Mins, Secs: Byte;
begin
  Mins := Value div (60 * 1000);
  Secs := (Value - Mins * 60 * 1000) div 1000;
  Result := mci_Make_TMSF(
    Track,
    Mins,
    Secs,
    0
  );
end;

function TMSFToTMS(TMSFValue: Integer): Integer;
const
  FRAME_MASK: Integer = $00FFFFFF;
begin
  Result := TMSFValue and FRAME_MASK; // Set frame to 0
end;

procedure TfrmMain.PlayNotify(Code: Integer);
var
  NeedExitWindows: Boolean;
begin
  if (Code = MCI_NOTIFY_SUCCESSFUL) and PowerOffMode then
  begin
    with TdlgExitWindows.Create(Application) do
    try
      NeedExitWindows := ShowModal = mrOk;
    finally
      Free;
    end;
    if NeedExitWindows then
    begin
      Windows.ExitWindowsEx(EWX_SHUTDOWN or EWX_POWEROFF, 0);
      Close;
    end;
  end;
  UpdateUI;
end;

procedure TfrmMain.Timer1Timer(Sender: TObject);
begin
  if not CD_Opened then CD_Open;
  if not CD_Opened then Exit;
  UpdateUI;
end;

procedure TfrmMain.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
    Style := WS_POPUP or WS_BORDER or WS_THICKFRAME;
end;

procedure TfrmMain.SpeedButton7Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.ELTrayIcon1Click(Sender: TObject);
var
  P: TPoint;
begin
  GetCursorPos(P);
  if (P.x <= Screen.Width - Width) then Left := P.x else Left := P.x - Width;
  if (P.y <= Screen.Height - Height) then Top := P.y else Top := P.y - Height;
  Show;
end;

procedure TfrmMain.ApplicationEvents1Deactivate(Sender: TObject);
begin
  Hide;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  SetForegroundWindow(Self.Handle);
  UpdateUI;
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then Hide;
end;

procedure TfrmMain.UpdateUI;
var
  OldPos, TrackLength, Track: Integer;
  OldPositionerEnabled: Boolean;
begin
  if not CD_Opened then Exit;

  OldPositionerEnabled := Positioner.Enabled;
  OldPos := Positioner.Position;
  Positioner.Update;

  if Positioner.Enabled then
  begin
    if (not OldPositionerEnabled) and (CD_Mode <> MCI_MODE_PLAY) then
      CD_ChangePosition(CD_TrackStartPos(1));

    if TMSFToTMS(OldPos) <> TMSFToTMS(Positioner.Position) then
      Display.Text := CreateDisplayString(
        mci_TMSF_Track(Positioner.Position),
        mci_TMSF_Minute(Positioner.Position),
        mci_TMSF_Second(Positioner.Position)
      );
    Track := mci_TMSF_Track(Positioner.Position);
    if mci_TMSF_Track(OldPos) <> Track then
    begin
     TrackLength := CD_TrackLength(Track);
     Display.Hint := CreateDisplayHint(
       mci_MSF_Minute(TrackLength),
       mci_MSF_Second(TrackLength)
     );
    end;
    if CD_Mode = MCI_MODE_PLAY then SetMode(pmPlaying)
      else SetMode(pmStoped);
    Timer1.Interval := CD_TIMER_INTERVAL;
  end
  else
    if Mode <> pmNoCD then
    begin
      Display.Text := DISPLAY_NO_CD;
      Display.Hint := '';                    
      SetMode(pmNoCD);
      Timer1.Interval := NO_CD_TIMER_INTERVAL;
    end;
end;

procedure TfrmMain.SetMode(const Value: TCDPlayerMode);

  procedure _SetButtonsEnabled(Mode: TCDPlayerMode);
  begin
    SpeedButton1.Enabled := not (Mode in [pmNotOpened, pmNoCD]);
    SpeedButton2.Enabled := not (Mode in [pmNotOpened, pmNoCD, pmPlaying]);
    SpeedButton3.Enabled := not (Mode in [pmNotOpened, pmNoCD]);
    SpeedButton4.Enabled := not (Mode in [pmNotOpened, pmNoCD]);
    SpeedButton5.Enabled := not (Mode in [pmNotOpened, pmPlaying]);
    SpeedButton6.Enabled := not (Mode in [pmNotOpened, pmPlaying]);
  end;

begin
  if FMode = Value then Exit;
  FMode := Value;
  _SetButtonsEnabled(Value);
  UpdateIcon;
end;

procedure TfrmMain.SetPowerOffMode(const Value: Boolean);
begin
  FPowerOffMode := Value;
  UpdateIcon;
end;

procedure TfrmMain.UpdateIcon;
var
  Index: Integer;
begin
  case Mode of
    pmNoCD: Index := 1;
    pmStoped: Index := 2;
    pmPlaying: if not PowerOffMode then Index := 3 else Index := 4;
  else
    Index := 0; // pmNotOpened
  end;

  if Index <> 0 then
    { Change tray icon (this always stop any animations) }
    ImageList1.GetIcon(Index, ELTrayIcon1.Icon)
  else
    { Start animation if cd device is used by other program }
    ELTrayIcon1.Animations[0].Start;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Display := TDisplay.Create(Label2);
  Positioner := TPositioner.Create(ProgressBar1);
  Positioner.OnMouse := PositionerMouse;
  FixedPopupMenuItemCount := PopupMenu1.Items.Count;
  InstanceChecker.OnResieveData := InstanceCheckerResieveData;
  Timer1Timer(Timer1);
  DoCommand;
end;

procedure TfrmMain.SpeedButton1Click(Sender: TObject);
begin
  CD_PrevTrack;
  UpdateUI;
end;

procedure TfrmMain.SpeedButton2Click(Sender: TObject);
begin
  CD_Play(PlayNotify);
  UpdateUI;
end;

procedure TfrmMain.SpeedButton3Click(Sender: TObject);
begin
  if CD_Mode = MCI_MODE_PLAY then
  begin
    CD_Stop;
    PowerOffMode := False;
  end
  else
    CD_ChangePosition(CD_TrackStartPos(1));
  UpdateUI;
end;

procedure TfrmMain.SpeedButton4Click(Sender: TObject);
begin
  CD_NextTrack;
  UpdateUI;
end;

procedure TfrmMain.SpeedButton5Click(Sender: TObject);
begin
  CD_CloseDoor;
  UpdateUI;
end;

procedure TfrmMain.SpeedButton6Click(Sender: TObject);
begin
  CD_OpenDoor;
  UpdateUI;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  Display.Free;
  Positioner.Free;

  if CD_Opened and (CD_Mode = MCI_MODE_PLAY) then CD_Stop;
  if CD_Opened then CD_Close;
end;

{ TDisplay }

procedure TDisplay.CancelTempText;
begin
  FIsTempText := False;
  FTimer.Enabled := False;
  FLabel.Caption := Text;
end;

constructor TDisplay.Create(ALabel: TLabel);
begin
  FLabel := ALabel;
  FLabel.ShowHint := True;
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.Interval := DISPLAY_TEMP_TEXT_INTERVAL;
  FTimer.OnTimer := FTimerTimer;

  FLabel.Caption := Text;
end;

destructor TDisplay.Destroy;
begin
  FTimer.Free;
  inherited;
end;

procedure TDisplay.FTimerTimer(Sender: TObject);
begin
  CancelTempText;
end;

function TDisplay.GetHint: string;
begin
  Result := FLabel.Hint;
end;

function TDisplay.GetTempText: string;
begin
  if IsTempText then Result := FLabel.Caption else Result := '';
end;

procedure TDisplay.SetHint(const Value: string);
begin
  FLabel.Hint := Value;
end;

procedure TDisplay.SetTempText(S: string);
begin
  FLabel.Caption := S;
  FIsTempText := True;
  FTimer.Enabled := False;
  FTimer.Enabled := True;
end;

procedure TDisplay.SetText(const Value: string);
begin
  FText := Value;
  if not IsTempText then FLabel.Caption := Value;
end;

{ TPositioner }

constructor TPositioner.Create(AProgressBar: TProgressBar);
begin
  FProgressBar := AProgressBar;
  FProgressBar.OnMouseMove := FProgressBarMouseMove;
  FProgressBar.OnMouseUp := FProgressBarMouseUp;
  OldWinProc := FProgressBar.WindowProc;
  FProgressBar.WindowProc := FProgressBarWinProc;
  FProgressBar.Enabled := False;
end;

destructor TPositioner.Destroy;
begin
  FProgressBar.WindowProc := OldWinProc;
  inherited;
end;

procedure TPositioner.FProgressBarMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  NewPos: Integer;
begin
  NewPos := GetProgressBarPosition(X, Y);
  if NewPos <> -1 then Mouse(meMove, NewPos);
end;

procedure TPositioner.FProgressBarMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  NewPos: Integer;
begin
  if Button = mbLeft then
  begin
    NewPos := GetProgressBarPosition(X, Y);
    if NewPos <> -1 then Mouse(meClick, NewPos);
  end;
end;

procedure TPositioner.FProgressBarWinProc(var Message: TMessage);
begin
  if Message.Msg = CM_MOUSELEAVE then Mouse(meExit, 0);
  OldWinProc(Message);
end;

function TPositioner.GetEnabled: Boolean;
begin
  Result := FProgressBar.Enabled;
end;

function TPositioner.GetProgressBarPosition(X, Y: Integer): Integer;
var
  NewPos: Integer;
begin
  if (X > 0) and (X < FProgressBar.Width) and (Y > 0) and
    (Y < FProgressBar.Height) and (FProgressBar.Enabled) then
  begin
    NewPos := Round(X / FProgressBar.Width *
      (FProgressBar.Max - FProgressBar.Min) + FProgressBar.Min);
    Result := CurTrackMS_To_TMSF(mci_TMSF_Track(Position),
      NewPos);
  end else Result := -1;
end;

procedure TPositioner.Mouse(MouseEventType: TMouseEventType; Pos: Integer);
begin
  if Assigned(OnMouse) then OnMouse(MouseEventType, Pos);
end;

procedure TPositioner.SetEnabled(const Value: Boolean);
begin
  if GetEnabled = Value then Exit;
  FProgressBar.Enabled := Value;
  if Value then
  begin
    SetMinMax;
    UpdatePosition;
  end
  else
    with FProgressBar do
    begin
      Position := Min;
      Max := Min;
    end;
end;

procedure TPositioner.SetMinMax;
begin
  if not Enabled then Exit;
  FProgressBar.Min := TMSF_To_CurTrackMS(
    CD_TrackStartPos(mci_TMSF_Track(Position))
  );
  FProgressBar.Max := FProgressBar.Min + MSF_To_CurTrackMS(
    CD_TrackLength(mci_TMSF_Track(Position))
  );
end;

procedure TPositioner.SetPosition(const Value: Integer);
var
  OldPos: Integer;
begin
  OldPos := FPosition;
  FPosition := Value;
  if TMSFToTMS(OldPos) = TMSFToTMS(FPosition) then Exit;
  if mci_TMSF_Track(OldPos) <> mci_TMSF_Track(FPosition) then
    SetMinMax;
  UpdatePosition;
end;

procedure TPositioner.Update;
begin
  if CD_Opened and CD_IsAudioMediaPresent then
  begin
    SetPosition(CD_Position);
    SetEnabled(True);
  end
  else SetEnabled(False);
end;

procedure TPositioner.UpdatePosition;
begin
  if not Enabled then Exit;
  FProgressBar.Position := TMSF_To_CurTrackMS(Position);
end;

procedure TfrmMain.PositionerMouse(MouseEventType: TMouseEventType;
  Pos: Integer);
begin
  case MouseEventType of
    meMove:
    begin
      Display.SetTempText('-' + CreateDisplayString(
        mci_TMSF_Track(Pos),
        mci_TMSF_Minute(Pos),
        mci_TMSF_Second(Pos)
      ) + '-');
    end;
    meClick:
    begin
      CD_ChangePosition(Pos);
      UpdateUI;
    end;
    meExit: Display.CancelTempText;
  end;
end;

procedure TfrmMain.DoCommand;

  procedure _ClickEnabledSpdBtn(SpeedButton: TSpeedButton);
  begin
    if SpeedButton.Enabled and Assigned(SpeedButton.OnClick) then
      SpeedButton.OnClick(SpeedButton);
  end;

var
  CDCode: Integer;
  TN: Byte;
  S: string;

begin
  if (ParamStrings.Count = 0) or not CD_Opened then Exit;
  if SameText(ParamStrings[0], 'play') then
  begin
    if ParamStrings.Count >= 2 then
    begin
      case CD_DecodeCDDAFile(ParamStrings[1], CDCode, TN) of
        CD_DECODECDDAFILE_CANNOTOPEN: S := Format(CANNOTOPENFILE, [ParamStrings[1]]);
        CD_DECODECDDAFILE_WRONGFORMAT: S := Format(NOTCDDAFILE, [ParamStrings[1]]);
      else
        S := '';
      end;
      if S <> '' then
      begin
        MessageDlg(S, mtError, [mbOk], 0);
        Exit;
      end;
      if {(CDCode = CD_GetCode) and }(TN <= CD_TrackCount) then
        CD_ChangePosition(CD_TrackStartPos(TN));
    end;
    _ClickEnabledSpdBtn(SpeedButton2);
  end
  else
    if SameText(ParamStrings[0], 'stop') then
      _ClickEnabledSpdBtn(SpeedButton3)
    else
      if SameText(ParamStrings[0], 'prevtrack') then
        _ClickEnabledSpdBtn(SpeedButton1)
      else
        if SameText(ParamStrings[0], 'nexttrack') then
          _ClickEnabledSpdBtn(SpeedButton4)
        else
          if SameText(ParamStrings[0], 'closedoor') then
            _ClickEnabledSpdBtn(SpeedButton5)
          else
            if SameText(ParamStrings[0], 'opendoor') then
              _ClickEnabledSpdBtn(SpeedButton6);
end;

procedure TfrmMain.PopupMenu1Popup(Sender: TObject);
var
  I, TC, Track: Integer;
  NewItem: TMenuItem;
begin
  N1.Enabled := (Mode = pmPlaying);
  N1.Checked := PowerOffMode;
  with PopupMenu1 do
  begin
    while Items.Count > FixedPopupMenuItemCount do
      Items[Items.Count - 1].Free;
    TC := CD_TrackCount;
    Track := mci_TMSF_Track(CD_Position);
    if TC > 0 then
    begin
      NewItem := TMenuItem.Create(Self);
      NewItem.Caption := '-';
      Items.Add(NewItem);
      for I := 1 to TC do
      begin
        NewItem := TMenuItem.Create(Self);
        NewItem.RadioItem := True;
        NewItem.Caption := POPUPMENU_TRACK_PREFIX + InttoStr(I);
        NewItem.Tag := I;
        NewItem.OnClick := PopupMenuTrackItemsClick;
        if I = Track then NewItem.Checked := True;
        Items.Add(NewItem);
      end;
    end;
  end;
end;

procedure TfrmMain.N1Click(Sender: TObject);
begin
  PowerOffMode := not PowerOffMode;
  UpdateUI;
end;

procedure TfrmMain.PopupMenuTrackItemsClick(Sender: TObject);
begin
  Update;
  CD_ChangePosition(
    mci_Make_TMSF(TMenuItem(Sender).Tag, 0, 0, 0)
  );
  UpdateUI;
end;

procedure TfrmMain.Registerfiletypes1Click(Sender: TObject);
var
  LR: TRegIniFile;
  LS: string;
begin
  LR := TRegIniFile.Create;
  try
    LS := GetModuleName(HInstance);

    LR.RootKey := HKEY_CLASSES_ROOT;
    LR.CreateKey('AudioCD');
    LR.CreateKey('AudioCD\shell');
    LR.WriteString('AudioCD\shell', '', 'play');
    LR.CreateKey('AudioCD\shell\play');
    LR.WriteString('AudioCD\shell\play', '', '&Play');
    LR.CreateKey('AudioCD\shell\play\command');
    LR.WriteString('AudioCD\shell\play\command', '', '"' + LS + '" play');

    LR.CreateKey('.cda');
    LR.WriteString('.cda', '', 'cdafile');

    LR.CreateKey('cdafile');
    LR.WriteString('cdafile', '', 'CD track');
    LR.CreateKey('cdafile\shell');
    LR.WriteString('cdafile\shell', '', 'play');
    LR.CreateKey('cdafile\shell\play');
    LR.WriteString('cdafile\shell\play', '', '&Play');
    LR.CreateKey('cdafile\shell\play\command');
    LR.WriteString('cdafile\shell\play\command', '', '"' + LS + '" play "%1"');
  finally
    LR.Free;
  end;
end;

procedure TfrmMain.Close1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.InstanceCheckerResieveData(Sender: TObject;
  const AData: string);
begin
  ELUnpackStrings(AData, ParamStrings);
  DoCommand;
end;

initialization
  ParamStrings := TStringList.Create;
  InstanceChecker := TELInstanceChecker.Create('{329CADC3-9D49-11D5-BB7C-EB9EBCAFE165}');
  FillParamStrings(['-', '/', '\'], ParamStrings);

  { Try to register this instance of CD Player }
  if not InstanceChecker.RegisterInstance then
  begin
    { If instance was not registered, then other instance of CD Player
      is runing in system, so post new param strings to it and
      close this instance. }
    InstanceChecker.PostData(ELPackStrings(ParamStrings));
    Halt;
  end;

finalization
  ParamStrings.Free;
  InstanceChecker.Free;

end.
