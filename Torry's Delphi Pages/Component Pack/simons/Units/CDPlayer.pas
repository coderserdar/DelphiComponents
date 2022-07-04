unit CDPlayer;

{ TCDPlayer (C)opyright 2004 Version 0.20
  Autor : Simon Reinhardt
  eMail : reinhardt@picsoft.de
  Internet : http://www.picsoft.de

  Die Komponente TCDPlayer ist eine Ableitung von TMediaPlayer,
  die jedoch auf die Wiedergabe von Audio-CDs spezialisiert ist.
  Dafür bietet sie einige zusätzliche Ereignise und Eigenschaften.

  Die Komponente ist Public Domain, das Urheberrecht liegt aber
  beim Autor. }

interface

uses Classes, Messages, Graphics, MPlayer, ExtCtrls;

type
  TCDTime = record
    Min,
    Sec : word;
  end;

  TPlaybackMode = (pmCD, pmTrack, pmSection, pmPlaylist, pmIntroScan);
  TPlayerState  = (psClosed, psNoCD, psStopped, psPaused, psPlaying, psSearching);
  TTrackOffset  = array [1..99] of longint;
  TTrackTime    = array [1..99] of TCDTime;

  TPlayList = class(TPersistent)
  private
    FItems    : TList;
    FMaxValue,
    FMinValue : integer;

  protected
    function Get(Index: Integer): integer;
    function GetCount: Integer;
    procedure Put(Index: Integer; const Value: integer);

  public
    function Add(const Value: integer): Integer;
    procedure Append(const Value: integer);
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    function IndexOf(const Value: integer): Integer;
    procedure Insert(Index: Integer; const Value: integer);
    procedure LoadFromFile(const FileName: string);
    procedure Move(CurIndex, NewIndex: Integer);
    procedure SaveToFile(const FileName: string);

    property Count: Integer read GetCount;
    property Items[Index: Integer]: integer read Get write Put; default;
    property MaxValue: Integer read FMaxValue write FMaxValue;
    property MinValue: Integer read FMinValue write FMinValue;
  end;

  TCDPlayer = class(TMediaPlayer)
  private
    FActive,
    FAudioCDOnly,
    FCDExtra        : boolean;
    FBitmap         : TBitmap;
    FCueIn,
    FCueOut,
    FOldTrackPos,
    FCDPos,
    FCDRemain,
    FCDTime,
    FTrackPos,
    FTrackRemain    : TCDTime;
    FCounter,
    FCountTo,
    FIntroscanTime,
    FOldTrackNum    : word;
    FLoop           : boolean;
    FPlaybackMode   : TPlaybackMode;
    FPlayList       : TList;
    FPlayListPos    : integer;
    FState          : TPlayerState;
    FSkipInterval,
    FCheckInterval,
    FTimerInterval  : word;
    FTimer          : TTimer;
    FTrackEndSens   : boolean;
    FTrackNum       : byte;
    FTrackOffset    : TTrackOffset;
    FTracks         : byte;
    FTrackSecOffset : array [1..99] of word;
    FTrackTime      : TTrackTime;

    FOnCDLoad,
    FOnCDEject,
    FOnClose,
    FOnEndPos,
    FOnTrackChange,
    FOnOpen,
    FOnPause,
    FOnPlay,
    FOnPlaylistPosChange,
    FOnStop,
    FOnTimer,
    FOnTrackEnd      : TNotifyEvent;

    procedure CDEject;
    procedure CDLoad;
//    procedure CloseOnEject;
    procedure GetPlaybackPositon;
    procedure InitTracks;
    function IsAudioCDInDrive:boolean;
    function IsTrackEnd:boolean;
    procedure SetActive(const Value:boolean);
    procedure SetCheckInterval(const Value:word);
    procedure SetPlaybackMode(const Value:TPlaybackMode);
    procedure SetPlayListPos(const Value:integer);
    procedure SetTimerInterval(const Value:word);
    procedure SetTrackNum(const Value:byte);
    procedure TrackChange;
    procedure TrackEnd;
    procedure WMEnable(var Message: TWMEnable); message WM_Enable;
    procedure WMLButtonDown(var Message: TWMLButtonDown);
      message WM_LButtonDown;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk);
      message WM_LButtonDblClk;
    procedure WMMouseMove(var Message: TWMMouseMove);
      message WM_MouseMove;
    procedure WMLButtonUp(var Message: TWMLButtonUp);
      message WM_LButtonUp;

  protected
    procedure ActivatePlayer;
    function DecodeCDPosition(const Position:integer;const TrackNum:word):TCDTime;
    function DecodeTrackCount(const ALength:integer):byte;
    function DecodeTrackPosition(const Position:integer):TCDTime;
    procedure DoNotify; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure Paint; override;
    procedure TimerEvent(Sender: TObject);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Close;
    function GetTrackFromCDTime(const ATime:TCDTime):byte;
    function GetTrackFromPosition(const APos:integer):byte;
    procedure GoToCDTime(const NewTime:TCDTime;const ResumePlayback:boolean);
    procedure GoToTrack(const NewTrack:byte;const ResumePlayback:boolean);
    procedure Eject;
    procedure Load;
    procedure Next;
    procedure Open;
    procedure Pause;
    procedure Play;
    procedure Previous;
    procedure ResetPlaylist(const NewPos:integer);
    procedure Shuffle;
    procedure SkipBack;
    procedure SkipFwd;
    procedure SortPlaylist(const Ascending:boolean);
    procedure Stop;

    property CDExtra: boolean read FCDExtra;
    property CDPlaybackPos: TCDTime read FCDPos;
    property CDRemainTime: TCDTime read FCDRemain;
    property CDTime: TCDTime read FCDTime;
    property CueIn: TCDTime read FCueIn write FCueIn;
    property CueOut: TCDTime read FCueOut write FCueOut;
    property PlayList: TList read FPlayList write FPlayList;
    property PlayListPos: integer read FPlayListPos write SetPlayListPos;
    property TrackNum: byte read FTrackNum write SetTrackNum;
    property TrackOffset: TTrackOffset read FTrackOffset;
    property TrackPlaybackPos: TCDTime read FTrackPos;
    property TrackRemainTime: TCDTime read FTrackRemain;
    property TrackTime: TTrackTime read FTrackTime;
    property State: TPlayerState read FState;

  published
    property Active: boolean read FActive write SetActive;
    property AudioCDOnly: boolean read FAudioCDOnly write FAudioCDOnly;
    property CheckInterval: word read FCheckInterval write SetCheckInterval;
    property IntroscanTime: word read FIntroscanTime write FIntroscanTime;
    property Loop: boolean read FLoop write FLoop;
    property PlaybackMode: TPlaybackMode read FPlaybackMode write SetPlaybackMode;
    property SkipInterval: word read FSkipInterval write FSkipInterval;
    property TimerInterval: word read FTimerInterval write SetTimerInterval;

    property OnCDEject: TNotifyEvent read FOnCDEject write FOnCDEject;
    property OnCDLoad: TNotifyEvent read FOnCDLoad write FOnCDLoad;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnEndPos: TNotifyEvent read FOnEndPos write FOnEndPos;
    property OnTrackChange: TNotifyEvent read FOnTrackChange write FOnTrackChange;
    property OnOpen: TNotifyEvent read FOnOpen write FOnOpen;
    property OnPause: TNotifyEvent read FOnPause write FOnPause;
    property OnPlay: TNotifyEvent read FOnPlay write FOnPlay;
    property OnPlaylistPosChange: TNotifyEvent read FOnPlaylistPosChange write FOnPlaylistPosChange;
    property OnStop: TNotifyEvent read FOnStop write FOnStop;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
    property OnTrackEnd: TNotifyEvent read FOnTrackEnd write FOnTrackEnd;

  end;

procedure Register;

procedure AddCDTime(var Time1:TCDTime;const Time2:TCDTime);
function CDTimeToSeconds(const ATime:TCDTime):word;
function CDTimeToStr(const ATime:TCDTime;const ShowHours:boolean):string;
function CompareCDTime(const Time1,Time2:TCDTime):integer;
function DecodeLength(const Length:integer):TCDTime;
function SecondsToCDTime(const ASecs:word):TCDTime;
function StrToCDTime(const AText:string):TCDTime;
function SubtractCDTime(const Time1,Time2:TCDTime):TCDTime;

implementation

{$R *.dcr}

uses Windows, Forms, MMSystem, SysUtils, SRUtils;

var
  SortAscending : boolean;

procedure AddCDTime(var Time1:TCDTime;const Time2:TCDTime);
begin
  Time1.Min:=Time1.Min+Time2.Min;
  Time1.Sec:=Time1.Sec+Time2.Sec;
  if Time1.Sec>=60 then begin
    Time1.Min:=Time1.Min+Time1.Sec div 60;
    Time1.Sec:=Time1.Sec mod 60;
  end;
end; // AddCDTime

function CDTimeToSeconds(const ATime:TCDTime):word;
begin
  Result:=(ATime.Min*60)+ATime.Sec;
end; // TimeToSeconds

function CDTimeToStr(const ATime:TCDTime;const ShowHours:boolean):string;
var Hour,
    Min  : word;
begin
  if ShowHours then begin
    Hour:=ATime.Min div 60;
    Min:=ATime.Min mod 60;
    Result:=IntToStrFixed(Hour, 2)+':'+IntToStrFixed(Min, 2)+':'+IntToStrFixed(ATime.Sec, 2);
  end
  else
    Result:=IntToStrFixed(ATime.Min, 2)+':'+IntToStrFixed(ATime.Sec, 2);
end; // CDTimeToStr

function CompareCDTime(const Time1,Time2:TCDTime):integer;
begin
  if Time1.Min<Time2.Min then
    Result:=-1
  else begin
    if Time1.Min>Time2.Min then
      Result:=1
    else begin
      if Time1.Sec<Time2.Sec then
        Result:=-1
      else begin
        if Time1.Sec>Time2.Sec then
          Result:=1
        else
          Result:=0;
      end;
    end;
  end;
end; // CompareCDTime

function DecodeLength(const Length:integer):TCDTime;
begin
  Result.Min:=Length mod $100;
  Result.Sec:=(Length shr 8) mod $100;
end; // DecodeLength

function ListSortProc(Item1, Item2:Pointer):integer;
begin
  if integer(Item1)<integer(Item2) then
    Result:=-1
  else begin
    if integer(Item1)>integer(Item2) then
      Result:=1
    else
      Result:=0;
  end;
  if not SortAscending then
    Result:=-Result;
end; // ListSortProc

function SecondsToCDTime(const ASecs:word):TCDTime;
begin
  Result.Sec:=ASecs mod 60;
  Result.Min:=ASecs div 60;
end; // SecondsToCDTime

function StrToCDTime(const AText:string):TCDTime;
var P : integer;
begin
  P:=Pos(':', AText);
  if P>0 then begin
    Result.Min:=StrToIntDef(copy(AText, 1, P-1), 0);
    Result.Sec:=StrToIntDef(copy(AText, P+1, length(AText)-P), 0);
  end;
end; // StrToCDTime

function SubtractCDTime(const Time1,Time2:TCDTime):TCDTime;
var Temp : integer;
begin
  Result.Min:=Time1.Min-Time2.Min;
  Temp:=Time1.Sec-Time2.Sec;
  if (Temp<0) and (Result.Min>0) then
    Result.Min:=Result.Min-1;
  if abs(Temp)<=60 then begin
    if Temp<0 then
      Result.Sec:=60+Temp
    else
      Result.Sec:=Temp;
  end;
end; // SubtractCDTime

{ Klasse TPlaylist }

function TPlaylist.Add(const Value: integer): Integer;
begin
  if (Value>=MinValue) and (Value<=MaxValue) then begin
    Result := GetCount;
    FItems.Insert(Result, Pointer(Value));
  end
  else
    Result:=-1;
end;

procedure TPlaylist.Append(const Value: integer);
begin
  FItems.Add(Pointer(Value));
end;

procedure TPlaylist.Clear;
begin
  FItems.Clear;
end;

procedure TPlaylist.Delete(Index: Integer);
begin
  FItems.Delete(Index);
end;

procedure TPlaylist.Exchange(Index1,Index2: integer);
var TempValue : integer;
begin
  TempValue := Items[Index1];
  Items[Index1] := Items[Index2];
  Items[Index2] := TempValue;
end;

function TPlaylist.Get(Index: Integer): integer;
begin
  Result := integer(FItems[Index]);
end;

function TPlaylist.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TPlaylist.IndexOf(const Value: integer): Integer;
begin
  for Result:=0 to GetCount-1 do
    if Items[Result] = Value then
      Exit;
  Result := -1;
end;

procedure TPlaylist.Insert(Index: integer;const Value: integer);
begin
  if (Value>=MinValue) and (Value<=MaxValue) then
    FItems.Insert(Index, Pointer(Value));
end;

procedure TPlaylist.LoadFromFile(const FileName: string);
var Stream : TStream;
    Value  : integer;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Clear;
    while Stream.Position<(Stream.Size-SizeOf(Value)) do begin
      Stream.ReadBuffer(Value, SizeOf(Value));
      Add(Value);
    end;
  finally
    Stream.Free;
  end;
end;

procedure TPlaylist.Move(CurIndex, NewIndex: Integer);
var TempValue : integer;
begin
  if CurIndex <> NewIndex then begin
    TempValue := Items[CurIndex];
    Delete(CurIndex);
    Insert(NewIndex, TempValue);
  end;
end;

procedure TPlaylist.Put(Index: Integer; const Value: integer);
begin
  FItems[Index] := Pointer(Value);
end;

procedure TPlaylist.SaveToFile(const FileName: string);
var Stream  : TStream;
    Value,i : integer;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    for i:=0 to Count-1 do begin
      Value:=Items[i];
      Stream.WriteBuffer(Value, SizeOf(Value));
    end;
  finally
    Stream.Free;
  end;
end;

{ Komponente TCDPlayer }

constructor TCDPlayer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Randomize;
  FPlayList:=TList.Create;
  FTimer:=TTimer.Create(self);
  FBitmap:=Graphics.TBitmap.Create;
  FBitmap.Height:=16;
  FBitmap.Width:=16;
  FBitmap.LoadFromResourceName(HInstance, 'CDIMAGE');

  { defaults }
  FActive:=false;
  FCounter:=0;
  FCheckInterval:=1000;
  FTimerInterval:=100;
  FCountTo:=FCheckInterval div FTimerInterval;
  FTimer.Enabled:=false;
  FTimer.Interval:=FTimerInterval;
  FTimer.OnTimer:=TimerEvent;

  AutoEnable:=false;
  AutoRewind:=true;
  AutoOpen:=false;
  ColoredButtons:=[];
  DeviceType:=dtCDAudio;
  EnabledButtons:=[];
  Height:=30;
  TimeFormat:=tfTMSF;
  Visible:=false;
  VisibleButtons:=[];
  Width:=30;

  FAudioCDOnly:=true;
  FIntroscanTime:=10;
  FLoop:=false;
  FPlaybackMode:=pmCD;
  FState:=psClosed;
  FSkipInterval:=10;
  FTrackEndSens:=true;
  InitTracks;
end; // Create

destructor TCDPlayer.Destroy;
begin
  FBitmap.Free;
  FTimer.Free;
  FPlayList.Free;

  inherited Destroy;
end; // Destroy

procedure TCDPlayer.ActivatePlayer;
begin
  if FState=psClosed then begin
    FState:=psNoCD;
    if IsAudioCDInDrive then
      CDLoad;
  end;
//  FTimer.Enabled:=true;
end; // ActivatePlayer

procedure TCDPlayer.CDEject;
begin
  if FState>=psStopped then
    FState:=psNoCD;
  InitTracks;
  if assigned(FOnCDEject) then
    FOnCDEject(Self);
  Close;
//  CloseOnEject;
end; // CDEject

procedure TCDPlayer.CDLoad;
var OldFormat : TMPTimeFormats;
    i         : byte;
begin
  if (Mode=mpNotReady) or (Mode>mpOpen) then
    Open;
  if (Mode>mpNotReady) and (Mode<=mpOpen) then begin
    if FState<psStopped then
      FState:=psStopped;
    // Gesamtzeit der CD ermitteln
    FCDTime:=DecodeLength(Length);
    FTracks:=DecodeTrackCount(Length);
    // Track-Offsets in Sekunden ermitteln
    OldFormat:=TimeFormat;
    try
      TimeFormat:=tfMilliSeconds;
      for i:=1 to 99 do begin
        if i<=FTracks then
          FTrackOffset[i]:=TrackPosition[i]
        else
          FTrackOffset[i]:=0;
      end;
      for i:=1 to 99 do
        FTrackSecOffset[i]:=FTrackOffset[i] div 1000;
    finally
      TimeFormat:=OldFormat;
    end;
    // Trackzeiten ermitteln
    if TrackLength[1]=Length then
      FCDExtra:=true
    else
      FCDExtra:=false;
    for i:=1 to 99 do begin
      if i<=FTracks then begin
        if FCDExtra then begin
          // Trackzeiten für CD-Extra berechnen
          if i<FTracks then
            FTrackTime[i]:=SubtractCDTime(DecodeLength(TrackLength[i]), DecodeLength(TrackLength[i+1]))
          else
            FTrackTime[i]:=DecodeLength(TrackLength[i]);
        end
        else
          // "Normale" Trackzeiten auslesen
          FTrackTime[i]:=DecodeLength(TrackLength[i]);
      end
      else
        FTrackTime[i]:=SecondsToCDTime(0);
    end;
    // PlayList initialisieren
    ResetPlaylist(-1);
    FPlayListPos:=0;
    // Wiedergabeposition ermitteln
    GetPlaybackPositon;
    if assigned(FOnCDLoad) then
      FOnCDLoad(Self);
  end;
end; // CDLoad

procedure TCDPlayer.Close;
begin
  if FState>=psStopped then
    FState:=psClosed;
  InitTracks;
//  FTimer.Enabled:=false;
  if Assigned(FOnClose) then
    FOnClose(Self);

  inherited;
end; // Close

{procedure TCDPlayer.CloseOnEject;
begin
  if FState>=psStopped then
    FState:=psClosed;
  InitTracks;
  if Assigned(FOnClose) then
    FOnClose(Self);
end; // CloseOnEject}

function TCDPlayer.DecodeCDPosition(const Position:integer;const TrackNum:word):TCDTime;
var Offset : TCDTime;
begin
  Offset:=SecondsToCDTime(FTrackSecOffset[TrackNum]-FTrackSecOffset[1]);
  Result.Sec:=Offset.Sec+((Position shr 16) mod $100);
  Result.Min:=Offset.Min+((Position shr 8) mod $100);
end; // DecodeCDPosition

function TCDPlayer.DecodeTrackCount(const ALength:integer):byte;
type
  TTMSFRec = record
    Tracks,
    Minutes,
    Seconds,
    Frames: byte;

  end;begin
  Result:=TTMSFRec(ALength).Tracks
end; // DecodeTrackCount

function TCDPlayer.DecodeTrackPosition(const Position:integer):TCDTime;
begin
  Result.Sec:=(Position shr 16) mod $100;
  Result.Min:=(Position shr 8) mod $100;
end; // DecodeTrackPosition

procedure TCDPlayer.DoNotify;
begin
  case Mode of
    mpStopped :
      if (FState<>psPaused) and assigned(FOnStop) then
        FOnStop(Self);
    mpPlaying :
      if assigned(FOnPlay) then
        FOnPlay(Self);
    mpPaused:
      if assigned(FOnPause) then
        FOnPause(Self);
  end;

  inherited;
  Notify:=true;
end; // DoNotify

{procedure TCDPlayer.Eject;
begin
  inherited;

  CDEject;
end;}

procedure TCDPlayer.Eject;
var Command,
    DriveName : string;
//    ErrCode : integer;
//    ErrStr  : array [0..255] of char;
begin
  if State>=psStopped then
    Inherited Eject
  else begin
    DriveName:=FileName[1]+':';
    Command := 'open '+DriveName+' type cdaudio alias cddrive';
    MCISendString(PChar(Command), nil, 0, 0);
    MCISendString('set cddrive door open wait', nil, 0, 0);
    MCISendString('close cddrive', nil, 0, 0);
  {  if ErrCode <> 0 then begin
      MCIGetErrorString(ErrCode, ErrStr, 255);
      StatusBar.Panels[1].Text:=ErrStr;
    end;}
  end;
  CDEject;
end; // Load

procedure TCDPlayer.GetPlaybackPositon;
var OldTrack : word;
begin
  if FState>=psStopped then begin
    OldTrack:=FTrackNum;
    FTrackNum:=Position mod $100;

    FOldTrackPos:=FTrackPos;
    FTrackPos:=DecodeTrackPosition(Position);
    FTrackRemain:=SubtractCDTime(FTrackTime[FTrackNum], FTrackPos);

    FCDPos:=DecodeCDPosition(Position, FTrackNum);
    FCDRemain:=SubtractCDTime(FCDTime, FCDPos);

    if OldTrack<>FTrackNum then begin
      FOldTrackNum:=OldTrack;
      TrackChange;
    end;
  end
  else
    InitTracks;
end; // GetPlaybackPositon

function TCDPlayer.GetTrackFromCDTime(const ATime:TCDTime):byte;
var APos : integer;
begin
  APos:=CDTimeToSeconds(ATime);
  Result:=GetTrackFromPosition(APos);
end; // GetTrackNumFromCDTime

function TCDPlayer.GetTrackFromPosition(const APos:integer):byte;
var i : byte;
begin
  Result:=0;
  if APos>=FTrackSecOffset[FTracks] then
    Result:=Tracks
  else begin
    for i:=1 to FTracks-1 do begin
      if (APos>=FTrackSecOffset[i]) and (APos<FTrackSecOffset[i+1]) then begin
        Result:=i;
        Exit;
      end;
    end;
  end;
end; // GetTrackFromPosition

procedure TCDPlayer.GoToCDTime(const NewTime:TCDTime;const ResumePlayback:boolean);
var OldState  : TPlayerState;
    OldFormat : TMPTimeFormats;
    OldTrack,
    NewPos    : integer;
begin
  NewPos:=(CDTimeToSeconds(NewTime)+FTrackSecOffset[1])*1000;
  if (State>=psStopped) and (NewPos>=0) then begin
    OldState:=FState;
    if State>psStopped then
      Stop;
    OldFormat:=TimeFormat;
    try
      TimeFormat:=tfMilliSeconds;
      if NewPos<Length then
        Position:=NewPos;
    finally
      TimeFormat:=OldFormat;
    end;
    OldTrack:=FTrackNum;
    FTrackNum:=Position mod $100;
    if OldTrack<>FTrackNum then begin
      FOldTrackNum:=OldTrack;
      TrackChange;
    end;
    GetPlaybackPositon;
    if ResumePlayback and (OldState=psPlaying) then
      Play;
  end;
end; // GoToCDTime

procedure TCDPlayer.GoToTrack(const NewTrack:byte;const ResumePlayback:boolean);
var OldState : TPlayerState;
    OldAuto  : boolean;
begin
  if (State>=psStopped) and (NewTrack>0) and (NewTrack<=FTracks) then begin
    OldState:=FState;
    if State>psStopped then begin
      OldAuto:=AutoRewind;
      AutoRewind:=false;
      Stop;
      AutoRewind:=OldAuto;
    end;
    Position:=TrackPosition[NewTrack];
    GetPlaybackPositon;
    if ResumePlayback and (OldState=psPlaying) then
      Play;
  end;
end; // GoToTrack

procedure TCDPlayer.InitTracks;
var i : byte;
begin
  FTrackNum:=0;
  FTrackPos:=SecondsToCDTime(0);
  FTrackRemain:=FTrackPos;

  FCDPos:=SecondsToCDTime(0);
  FCDRemain:=FCDPos;
  for i:=1 to 99 do begin
    FTrackTime[i]:=SecondsToCDTime(0);
    FTrackOffset[i]:=0;
  end;
end; // InitTracks

function TCDPlayer.IsAudioCDInDrive:boolean;
var SR        : TSearchRec;
    MaxLength,
    FSFlags   : DWORD;
    DriveRoot,
    VName     : string;
begin
  Result:=false;
  if FileName<>'' then begin
    DriveRoot:=FileName[1]+':\';
    if FAudioCDOnly then begin
      SetLength(VName, 64);
      if GetVolumeInformation(PChar(DriveRoot),
                              PChar(VName), System.length(VName),
                              nil, MaxLength, FSFlags, nil, 0) then
        Result:=lStrCmp(PChar(VName), 'Audio CD')=0
      else begin
        if SysUtils.FindFirst(DriveRoot+'*.cda', faAnyFile, SR)=0 then begin
          Result:=true;
          FindClose(SR);
        end;
      end;
    end
    else begin
      if SysUtils.FindFirst(DriveRoot+'*', faAnyFile, SR)=0 then begin
        Result:=true;
        FindClose(SR);
      end
      else
        Result:=false;
    end;
  end;
end; // IsAudioCDInDrive

function TCDPlayer.IsTrackEnd:boolean;
begin
  if CompareCDTime(FTrackPos, FTrackTime[FTrackNum])>0 then
    Result:=false
  else
    Result:=(CompareCDTime(FTrackPos, FTrackTime[FTrackNum])=0) or
            (CompareCDTime(FTrackPos, FOldTrackPos)<0);
end; // IsTrackEnd

procedure TCDPlayer.KeyDown(var Key: Word; Shift: TShiftState);
begin
  // Keine Tastaturereignisse durchreichen
end;

{procedure TCDPlayer.Load;
var SetParm : TMCI_Set_Parms;
    FFlags  : longint;
begin
  FFlags := 0;
  if Wait then
    FFlags := mci_Wait;
  if Notify then
    FFlags := FFlags or mci_Notify;
  FFlags := FFlags or mci_Set_Door_Closed;
  SetParm.dwCallback := Handle;
  mciSendCommand(DeviceID, mci_Set, FFlags, Longint(@SetParm));
end; // Load}

procedure TCDPlayer.Load;
var Command,
    DriveName : string;
//    ErrCode : integer;
//    ErrStr  : array [0..255] of char;
begin
  DriveName:=FileName[1]+':';
  Command := 'open '+DriveName+' type cdaudio alias cddrive';
  MCISendString(PChar(Command), nil, 0, 0);
  MCISendString('set cddrive door closed wait', nil, 0, 0);
  MCISendString('close cddrive', nil, 0, 0);
{  if ErrCode <> 0 then begin
    MCIGetErrorString(ErrCode, ErrStr, 255);
    StatusBar.Panels[1].Text:=ErrStr;
  end;}
end; // Load

procedure TCDPlayer.Loaded;
begin
  inherited Loaded;

  if (not (csDesigning in ComponentState)) and (Mode>mpNotReady) and (Mode<=mpOpen) then begin
    ActivatePlayer;
    if Assigned(FOnOpen) then
      FOnOpen(Self);
  end;
end; // Loaded

procedure TCDPlayer.Next;
begin
  if PlaybackMode=pmPlaylist then begin
    if FPlaylistPos<(FPlaylist.Count-1) then
      SetPlaylistPos(FPlaylistPos+1);
  end
  else
    inherited;
end; // Next

procedure TCDPlayer.Open;
begin
  inherited;

  ActivatePlayer;
  if Assigned(FOnOpen) then
    FOnOpen(Self);
end; // Open

procedure TCDPlayer.Paint;
var CRect : TRect;
begin
  if (csDesigning in ComponentState) then begin
    // Im Designmodus Symbol zeichnen
    CRect:=ClientRect;
    with Canvas do begin
      Brush.Color:=clSilver;
      FillRect(CRect);
      DrawEdge(Handle, CRect, Edge_Raised, BF_Rect);
      Draw(6, 6, FBitmap);
{      Font.Color:=clBtnText;
      Font.Size:=10;
      Font.Style:=[fsBold];
      DrawText(Handle, 'CD', 2, CRect, DT_SingleLine or DT_Center or DT_VCenter);}
    end;
  end;
  // Sonst gar nix.
end; // Paint

procedure TCDPlayer.Pause;
begin
  if Mode=mpPlaying then
    FState:=psPaused;

  inherited;

  if Assigned(FOnPause) then
    FOnPause(Self);
end; // Pause

procedure TCDPlayer.Play;
begin
  if (State<>psPlaying) and (PlaybackMode=pmSection) then
    GotoCDTime(CueIn, true);

  inherited;
end; // Play

procedure TCDPlayer.Previous;
begin
  if PlaybackMode=pmPlaylist then begin
    if FPlaylistPos>0 then
      SetPlaylistPos(FPlaylistPos-1);
  end
  else
    inherited;
end; // Previous

procedure TCDPlayer.ResetPlaylist(const NewPos:integer);
var i : byte;
begin
  FPlayList.Clear;
  for i:=1 to FTracks do
    FPlayList.Add(Pointer(i));
  if (NewPos>=0) and (NewPos<FPlayList.Count) then
    SetPlayListPos(NewPos);
end; // ResetPlaylist

procedure TCDPlayer.SetActive(const Value:boolean);
begin
  if FActive<>Value then begin
    FActive:=Value;
    Enabled:=FActive;
    FTimer.Enabled:=FActive;
  end;
end; // SetActive

procedure TCDPlayer.SetCheckInterval(const Value:word);
begin
  if FCheckInterval<>Value then begin
    FCheckInterval:=Value;
    if FTimerInterval>FCheckInterval then
      FCountTo:=1
    else
      FCountTo:=FCheckInterval div FTimerInterval;
  end;
end; // SetCheckInterval

procedure TCDPlayer.SetPlaybackMode(const Value:TPlaybackMode);
var APos : integer;
begin
  if FPlaybackMode<>Value then begin
    FPlaybackMode:=Value;
    if FPlaybackMode=pmPlaylist then begin
      APos:=FPlayList.IndexOf(Pointer(TrackNum));
      if (APos>=0) and (APos<FPlayList.Count) then
        SetPlayListPos(APos)
      else
        if (FPlaylistPos>=0) and (FPlaylistPos<FPlayList.Count) then
          GoToTrack(integer(FPlayList[FPlaylistPos]), true);
    end;
  end;
end; // SetPlaybackMode

procedure TCDPlayer.SetPlayListPos(const Value:integer);
var NewTrack : byte;
begin
  if (Value>=0) and (Value<FPlayList.Count) then begin
    FPlayListPos:=Value;
    if (FPlaybackMode=pmPlaylist) then begin
      NewTrack:=integer(FPlayList[FPlaylistPos]);
      if NewTrack<>TrackNum then
        GoToTrack(NewTrack, true);
      if assigned(FOnPlaylistPosChange) then
        FOnPlaylistPosChange(Self);
    end;
  end;
end; // SetPlayListPos

procedure TCDPlayer.SetTimerInterval(const Value:word);
begin
  if FTimerInterval<>Value then begin
    FTimerInterval:=Value;
    if FTimerInterval>FCheckInterval then
      FCountTo:=1
    else
      FCountTo:=FCheckInterval div FTimerInterval;
    FTimer.Interval:=FTimerInterval;
  end;
end; // SetCheckInterval

procedure TCDPlayer.SetTrackNum(const Value:byte);
begin
  if (FTrackNum<>Value) and (Value>0) and (Value<=FTracks) then begin
    FTrackNum:=Value;
    GotoTrack(FTrackNum, State=psPlaying);
  end;
end; // SetTrackNum

procedure TCDPlayer.Shuffle;
var OldCount,
    TrackNr,
    i,Max    : integer;
begin
  FPlayList.Clear;
  Max:=FTracks;
  for i:=1 to Max do begin
    OldCount:=FPlayList.Count;
    repeat
      TrackNr:=Random(Max)+1;
      if FPlayList.IndexOf(Pointer(TrackNr))<0 then
        FPlayList.Add(Pointer(TrackNr));
    until FPlayList.Count>OldCount;
  end;
  SetPlayListPos(0);
end; // Shuffle

procedure TCDPlayer.SkipBack;
var NewTime : TCDTime;
    OldPos  : integer;
begin
  OldPos:=CDTimeToSeconds(CDPlaybackPos);
  if OldPos>=FSkipInterval then
    NewTime:=SecondsToCDTime(OldPos-FSkipInterval)
  else
    NewTime:=SecondsToCDTime(0);
  GotoCDTime(NewTime, true);
end; // SkipBack

procedure TCDPlayer.SkipFwd;
var NewTime : TCDTime;
    OldPos  : integer;
begin
  OldPos:=CDTimeToSeconds(CDPlaybackPos);
  NewTime:=SecondsToCDTime(OldPos+FSkipInterval);
  if CompareCDTime(NewTime, CDTime)<=0 then
    GotoCDTime(NewTime, true);
end; // SkipFwd

procedure TCDPlayer.SortPlaylist(const Ascending:boolean);
begin
  SortAscending:=Ascending;
  Playlist.Sort(ListSortProc);
end; // SortPlaylist

procedure TCDPlayer.Stop;
begin
  inherited;

  if FState<>psStopped then
    FState:=psStopped;
  if Assigned(FOnStop) then
      FOnStop(Self);
  if AutoRewind then
    GotoTrack(TrackNum, false);
end; // Stop

procedure TCDPlayer.TimerEvent(Sender: TObject);
begin
  inc(FCounter);
  if FCounter>=FCountTo then begin
    FCounter:=0;
    // Player.State aktualisieren
    if (FState>=psStopped) and not IsAudioCDInDrive then
      CDEject;
    if (FState>psNoCD) and ((Mode=mpOpen) or (Mode=mpNotReady)) then
      CDEject;
    if (FState<=psNoCD) and IsAudioCDInDrive then
      CDLoad;
    if (FState>psNoCD) and ((Tracks=0) or (Tracks>99)) then
      FTracks:=DecodeTrackCount(Length);
    if (FState>psNoCD) and (FTracks>0) then begin
      // Wiedergabeposition ermitteln
      GetPlaybackPositon;
      // Player.State aktualisieren
      if (FState>=psPlaying) and (Mode=mpStopped) then begin
        FState:=psStopped;
        if assigned(FOnStop) then
          FOnStop(Self);
      end;
      if (FState<>psPaused) and (Mode=mpPaused) then begin
        FState:=psPaused;
        if assigned(FOnPause) then
          FOnPause(Self);
      end;
      if (FState<>psPlaying) and (Mode=mpPlaying) then begin
        FState:=psPlaying;
        if assigned(FOnPlay) then
          FOnPlay(Self);
      end;
      if (FState<>psSearching) and (Mode=mpSeeking) then
        FState:=psSearching;
      if FState=psPlaying then begin
        if IsTrackEnd then
          TrackEnd;
        if (FPlaybackMode=pmIntroScan) and (CDTimeToSeconds(FTrackPos)>FIntroscanTime)
         and (FOldTrackNum<FTrackNum) then begin
          // Intro-Scan
          if FTrackNum<FTracks then
            GoToTrack(FTrackNum+1, true)
          else
            if FLoop then
              GoToTrack(1, true);
        end;
        if (FPlaybackMode=pmSection) and (CompareCDTime(FCDPos, FCueOut)>=0) then begin
          // Cueschleife
          if assigned(FOnEndPos) then
            FOnEndPos(self);
          GoToCDTime(FCueIn, FLoop);
        end;
      end;
    end;
  end;
  if Assigned(FOnTimer) then
    FOnTimer(Self);
end; // TimerEvent

procedure TCDPlayer.TrackChange;
begin
  if assigned(FOnTrackChange) then
    FOnTrackChange(Self);
end; // TrackChange

procedure TCDPlayer.TrackEnd;
begin
  if Assigned(FOnTrackEnd) then
    FOnTrackEnd(Self);
  if FState=psPlaying then begin
    if FLoop and (FPlaybackMode=pmCD) and (FTrackNum=FTracks) then begin
      // CD-Loop
      GoToTrack(1, true);
      if not (State=psPlaying) then
        Play;
    end;
    if FPlaybackMode=pmTrack then begin
      if FLoop then begin
        // Track-Loop
        GoToTrack(FTrackNum, true);
        if not (State=psPlaying) then
          Play;
      end
      else
        // Autopause
        GoToTrack(FTrackNum, false);
    end;
    if (FPlaybackMode=pmPlayList) and (FPlaylist.Count>0) then begin
      // Playlist-Zeiger erhöhen
      if FPlaylistPos<FPlaylist.Count then
        SetPlayListPos(FPlaylistPos+1)
      else
        if Loop then
          SetPlayListPos(0);
    end;
  end;
end;

procedure TCDPlayer.WMEnable(var Message: TWMEnable);
begin
//  Active:=Enabled;
  if not Enabled then
    FTimer.Enabled:=false
  else
    FTimer.Enabled:=true;
end; // WMEnable

procedure TCDPlayer.WMLButtonDown(var Message: TWMLButtonDown);
begin
  // Keine Mausereignisse durchreichen
end;

procedure TCDPlayer.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  // Keine Mausereignisse durchreichen
end;

procedure TCDPlayer.WMMouseMove(var Message: TWMMouseMove);
begin
  // Keine Mausereignisse durchreichen
end;

procedure TCDPlayer.WMLButtonUp(var Message: TWMLButtonUp);
begin
  // Keine Mausereignisse durchreichen
end;

procedure Register;
begin
  RegisterComponents('Simon', [TCDPlayer]);
end; // Register

end.
