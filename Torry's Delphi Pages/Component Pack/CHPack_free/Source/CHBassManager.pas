unit CHBassManager;

{ ##############################################################################
  TCHBassManager

  Version   :   1.1.0
  Delphi    :   5, 6, 7
  Author    :   Christian Hämmerle
  eMail     :   chaemmerle@Blue-Xplosion.de
  Internet  :   http://www.Blue-Xplosion.de (German/English)

  Licenc    :   Freeware

  History:
  1.0.0 - 19.11.2002    - First Release
  1.0.1 - 29.11.2002    - BUG: GetMusicLength if *.MOD, *.s3m, *.xm, ...
  1.1.0 - 07.12.2002    - ADD: Bass 1.6 and 1.7 supported
                        - CHANGE: remove cmSample Mode because never used
                        - ADD: SetMODOption, SetMODRamping, SetMODEmulation,
                               SetMODSurround, SetMODReaction, GetMusicMode,
                               GetSongLengthExt, GetPositionExt
                        - BUG: correct GetPosition if MOD music
                        - BUG: MOD music don´t overload STREAM music
                        - CHANGE: GetStatus use BASS function
                        - CHANGE: a few little other changes

  Function description can find in "CHBassManager.txt"

  ############################################################################ }

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, Controls,

  BASS;

type
  TBassVolume = 0..100;
  TBassFrequency = 100..100000;
  TBassPan = -100..100;
  TBassChannel = 0..128;
  TChannelMode = (cmNone, cmStream, cmMod);
  TBassMode = (bmNone, bmPlaying, bmStopped, bmPaused, bmError, bmStalled);


  TCHBassManager = class(TComponent)
  private
    FNormalRamp: Boolean;
    FStopMoving: Boolean;
    FSensitiveRamp: Boolean;
    FEmulateProTracker: Boolean;
    FEmulateFastTracker: Boolean;
    FSurround: Boolean;
    FSurround2 : Boolean;
    FStopBack : Boolean;
    FLoop: Boolean;
    FUse_3D_Effect : Boolean;
    FUse_FX_Effect : Boolean;

    FVolume: TBassVolume;
    FPan: TBassPan;
    FLength : DWORD;
    FPattern : Integer;
    FForm : TCustomForm;
    FFormHwnd : THandle;
    FChannelMode : TChannelMode;
    FArrayMusic : array[0..128] of HMUSIC;
    FArrayStream : array[0..128] of HSTREAM;
    FArrayChannelMode : array[0..128] of TChannelMode;
    FArrayChannelStatus : array[0..128] of TBassMode;
    FChannel: Cardinal;
    FMono: Boolean;
    FModScale : Word;

    procedure Error(msg: string);
    function CheckFormat(sFileName :string) : Boolean;
  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Load(Channel : Cardinal; sFileName : string) : Boolean;
    function Play(Channel : Cardinal) : Boolean;
    function Pause(Channel : Cardinal) : Boolean;
    function Resume(Channel : Cardinal) : Boolean;
    function Stop(Channel : Cardinal) : Boolean;
    function FreeStream(Channel : Cardinal) : Boolean;
    function FreeMod(Channel : Cardinal) : Boolean;
    function GetSongLength(Channel : Cardinal) : DWORD;
    function GetSongLengthExt(Channel : Cardinal; Pattern : Boolean) : DWORD;
    function GetLevelLeft(Channel : Cardinal) : DWORD;
    function GetLevelRight(Channel : Cardinal) : DWORD;
    function GetPosition(Channel : Cardinal) : DWORD;
    function GetPositionExt(Channel : Cardinal; Pattern : Boolean) : DWORD;
    function GetVolume(Channel : Cardinal) : DWORD;
    function GetPan(Channel : Cardinal) : Integer;
    function GetFrequency(Channel : Cardinal) : DWORD;
    function GetStatus(Channel : Cardinal) : TBassMode;
    function GetMusicMode(Channel : Cardinal) : TChannelMode;
    function SetPosition(Channel : Cardinal; Value: DWORD) : Boolean;
    function SetVolume(Channel : Cardinal; Value: TBassVolume) : Boolean;
    function SetPan(Channel : Cardinal; Value: TBassPan) : Boolean;
    function SetFrequency(Channel : Cardinal; Value: TBassFrequency) : Boolean;
    procedure SetMODOption(Loop, Mono : Boolean);
    procedure SetMODRamping(NormalRamp, SensitiveRamp : Boolean);
    procedure SetMODEmulation(FastTracker2, ProTracker1 : Boolean);
    procedure SetMODSurround(Surround, Surround2 : Boolean);
    procedure SetMODReaction(PosReset, StopBack : Boolean);
    procedure SetMODEffect(Use_3D_Effect, Use_FX_Effect : Boolean);
    function FreeAll : Boolean;
    function StopAll : Boolean;
    function PlayAll : Boolean;
    function PauseAll : Boolean;
    function ResumeAll : Boolean;
  end;

implementation


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHBassManager.Error(msg: string);
var
  s: string;
begin
  s := msg + #13#10 + '(error code: ' + IntToStr(BASS_ErrorGetCode) + ')';
  MessageBox(Application.handle, PChar(s), 'Error', MB_ICONERROR or MB_OK);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHBassManager.Create(AOwner: TComponent);
var
  nBassVersion : Integer;
  nCheckVer : Integer;
  bSupported : Boolean;
begin
  inherited Create(AOwner);

  FLoop := False;
  FMono := False;

  FChannel := 0;
  FVolume := 100;
  FPan := 0;
  FModScale := 1;
  FArrayChannelMode[FChannel] := cmNone;

  FForm := GetParentForm(TControl(AOwner));
  FFormHwnd := FForm.Handle;

  // get BASS-Version
  nBassVersion := BASS_GetVersion();
  nCheckVer := MakeLong(2,0);

  // check BASS-Version
  if (nBassVersion = nCheckVer) then
    bSupported := True
  else
    bSupported := False;

  if not bSupported then
  begin
    Application.MessageBox('Only BASS.DLL Version 2.0 or higher supported!', 'Error',
      MB_OK+MB_DEFBUTTON1+MB_APPLMODAL);
    Application.Terminate;
  end;

  // BASS-Output initialisieren
  if not BASS_Init(1, 44100, 0, FFormHwnd, nil) then
  begin
    BASS_Init(0, 44100, 0, FFormHwnd, nil);
  end;

  BASS_Start;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHBassManager.Destroy;
begin
  BASS_Free;
  inherited Destroy;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHBassManager.CheckFormat(sFileName: string): Boolean;
var
  sExt : String;
  bRet : Boolean;
begin
  try
    sExt := AnsiLowerCase(ExtractFileExt(sFileName));

    if (sExt = '.wav') or
       (sExt = '.mp1') or
       (sExt = '.mp2') or
       (sExt = '.mp3') or
       (sExt = '.ogg') then
    begin
      bRet := True;
      FChannelMode := cmStream;
    end
    else if
      (sExt = '.mo3') or
      (sExt = '.mod') or
      (sExt = '.mtm') or
      (sExt = '.umx') or
      (sExt = '.xm') or
      (sExt = '.s3m') or
      (sExt = '.it') then
    begin
      bRet := True;
      FChannelMode := cmMod;
    end
    else
    begin
      bRet := False;
      FChannelMode := cmNone;
    end;
  except
    bRet := False;
    FChannelMode := cmNone;
  end;
  Result := bRet;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHBassManager.Load(Channel : Cardinal; sFileName: string): Boolean;
var
  bRet : Boolean;
  StreamFlag, ModFlag : DWORD;
begin
  bRet := True;
  StreamFlag := 0;
  ModFlag := BASS_MUSIC_CALCLEN;
  try
    // check supported format and set "FChannelMode"
    if CheckFormat(sFileName) then
    begin
      // free selected channel
      FArrayChannelMode[Channel] := cmNone;
      BASS_StreamFree(FArrayStream[Channel]);
      BASS_MusicFree(FArrayMusic[Channel]);

      // set flags (STREAM)
      if FMono then
        StreamFlag := BASS_SAMPLE_MONO;

      // set flags (MOD)
      if FUse_3D_Effect then
        ModFlag := ModFlag + BASS_MUSIC_3D;
      if FUse_FX_Effect then
        ModFlag := ModFlag + BASS_MUSIC_FX;

      // load file
      case FChannelMode of
        cmStream : begin
                    FArrayChannelMode[Channel] := cmStream;
                    FArrayStream[Channel] := BASS_StreamCreateFile(FALSE, PChar(sFileName), 0, 0, StreamFlag);
                   end;
        cmMod    : begin
                    FArrayChannelMode[Channel] := cmMod;
                    FArrayMusic[Channel] := BASS_MusicLoad(FALSE, PChar(sFileName), 0, 0, ModFlag, 0);
                  end;
      end;

      // can load file?
      if FArrayChannelMode[Channel] = cmNone then
      begin
        Error('Can''t load the file: ' + sFileName);
        bRet := False;
      end;
    end;
  except
    bRet := False;
  end;
  Result := bRet;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHBassManager.Play(Channel : Cardinal): Boolean;
var
  bRet : Boolean;
  StreamFlag, ModFlag : integer;
begin
  bRet := True;
  StreamFlag := 0;
  ModFlag := 0;
  try
    // if channel loaded
    if FArrayChannelMode[Channel] <> cmNone then
    begin
      // set flags STREAM-Music
      if FLoop then
        StreamFlag := BASS_SAMPLE_LOOP;

      // set flags MOD-Music
      if FNormalRamp then
        ModFlag := BASS_MUSIC_RAMP;
      if FSensitiveRamp then
        ModFlag := ModFlag + BASS_MUSIC_RAMPS;
      if FEmulateFastTracker then
        ModFlag := ModFlag + BASS_MUSIC_FT2MOD;
      if FEmulateProTracker then
        ModFlag := ModFlag + BASS_MUSIC_PT1MOD;
      if FStopMoving then
        ModFlag := ModFlag + BASS_MUSIC_POSRESET;
      if FSurround then
        ModFlag := ModFlag + BASS_MUSIC_SURROUND;
      if FSurround2 then
        ModFlag := ModFlag + BASS_MUSIC_SURROUND2;
      if FLoop then
        ModFlag := ModFlag + BASS_MUSIC_LOOP;
      if FStopBack then
        ModFlag := ModFlag + BASS_MUSIC_STOPBACK;

      // play channel
      if FArrayChannelMode[Channel] = cmStream then
        BASS_StreamPlay(FArrayStream[Channel], False, StreamFlag)
      else if FArrayChannelMode[Channel] = cmMod then
        BASS_MusicPlayEx(FArrayMusic[Channel], 0, ModFlag, True);
    end
    else
    begin
      bRet := False;
    end;
  except
    bRet := False;
  end;
  Result := bRet;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHBassManager.Pause(Channel : Cardinal): Boolean;
var
  bRet : Boolean;
begin
  bRet := True;
  try
    BASS_ChannelPause(FArrayStream[Channel]);
    BASS_ChannelPause(FArrayMusic[Channel]);
  except
    bRet := False;
  end;
  Result := bRet;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHBassManager.Resume(Channel : Cardinal): Boolean;
var
  bRet : Boolean;
begin
  bRet := True;
  try
    BASS_ChannelResume(FArrayStream[Channel]);
    BASS_ChannelResume(FArrayMusic[Channel]);
  except
    bRet := False;
  end;
  Result := bRet;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHBassManager.Stop(Channel : Cardinal): Boolean;
var
  bRet : Boolean;
begin
  bRet := True;
  try
    BASS_ChannelStop(FArrayStream[Channel]);
    BASS_ChannelStop(FArrayMusic[Channel]);
  except
    bRet := False;
  end;
  Result := bRet;
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHBassManager.FreeAll: Boolean;
var
  I : Integer;
  bRet : Boolean;
begin
  bRet := True;
  try
    // free mod
    for I := 0 to 128 do
      BASS_MusicFree(FArrayMusic[I]);

    // free stream
    for I := 0 to 128 do
      BASS_StreamFree(FArrayStream[I]);
  except
    bRet := False;
  end;
  Result := bRet;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHBassManager.FreeMod(Channel: Cardinal): Boolean;
var
  bRet : Boolean;
begin
  bRet := True;
  try
    // free mod
    BASS_MusicFree(FArrayMusic[Channel]);
  except
    bRet := False;
  end;
  Result := bRet;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHBassManager.FreeStream(Channel: Cardinal): Boolean;
var
  bRet : Boolean;
begin
  bRet := True;
  try
    // free stream
    BASS_StreamFree(FArrayStream[Channel]);
  except
    bRet := False;
  end;
  Result := bRet;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHBassManager.SetPosition(Channel : Cardinal; Value: DWORD) : Boolean;
var
  nNewPos : Int64;
  bRet : Boolean;
begin
  bRet := True;
  try
    case FArrayChannelMode[Channel] of
      cmStream : begin
                   nNewPos := BASS_ChannelSeconds2Bytes(FArrayStream[Channel], Value / 1000);
                   BASS_ChannelSetPosition(FArrayStream[Channel], nNewPos);
                 end;
      cmMod    : begin
                   BASS_ChannelSetPosition(FArrayMusic[Channel], Round(Value));
                 end;
    end;
  except
    bRet := False;
  end;
  Result := bRet;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHBassManager.GetPosition(Channel : Cardinal): DWORD;
var
  nCurrentPos, nCurrentResult : Int64;
  nCurrentSec : FLOAT;
begin
  Result := 0;

  case FArrayChannelMode[Channel] of
    cmStream : begin
                 nCurrentPos := BASS_ChannelGetPosition(FArrayStream[Channel]);
                 nCurrentSec := BASS_ChannelBytes2Seconds(FArrayStream[Channel], nCurrentPos);
                 Result := Round(nCurrentSec * 1000);
               end;
    cmMod    : begin
                 BASS_MusicSetPositionScaler(FArrayMusic[Channel], FModScale);
                 nCurrentPos := BASS_ChannelGetPosition(FArrayMusic[Channel]);
                 nCurrentResult := LOWORD(nCurrentPos);

                 if FArrayChannelStatus[Channel] = bmStopped then
                   Result := nCurrentResult
                 else
                  Result := nCurrentResult +1;
               end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHBassManager.GetPositionExt(Channel: Cardinal; Pattern: Boolean): DWORD;
var
  nCurrentPos, nCurrentResult : Int64;
  nCurrentSec : FLOAT;
begin
  Result := 0;

  case FArrayChannelMode[Channel] of
    cmStream : begin
                 nCurrentPos := BASS_ChannelGetPosition(FArrayStream[Channel]);
                 nCurrentSec := BASS_ChannelBytes2Seconds(FArrayStream[Channel], nCurrentPos);
                 Result := Round(nCurrentSec * 1000);
               end;
    cmMod    : begin
                 BASS_MusicSetPositionScaler(FArrayMusic[Channel], FModScale);
                 nCurrentPos := BASS_ChannelGetPosition(FArrayMusic[Channel]);
                 if Pattern then
                   nCurrentResult := LOWORD(nCurrentPos)
                 else
                   nCurrentResult := HIWORD(nCurrentPos);

                 if FArrayChannelStatus[Channel] = bmStopped then
                   Result := nCurrentResult
                 else
                  Result := nCurrentResult +1;
               end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHBassManager.GetSongLength(Channel : Cardinal): DWORD;
var
  nSongLength : Int64;
  nSongSec : FLOAT;
begin
  nSongSec := 0;
  case FArrayChannelMode[Channel] of
    cmStream : begin
                 nSongLength := BASS_StreamGetLength(FArrayStream[Channel]);
                 nSongSec := BASS_ChannelBytes2Seconds(FArrayStream[Channel], nSongLength);
               end;
    cmMod    : begin
                 nSongLength := BASS_MusicGetLength(FArrayMusic[Channel], True);
                 nSongSec := BASS_ChannelBytes2Seconds(FArrayMusic[Channel], nSongLength);
               end;
  end;

  FLength := Round(nSongSec * 1000);
  Result := FLength;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHBassManager.GetSongLengthExt(Channel: Cardinal; Pattern: Boolean): DWORD;
var
  nSongPattern : FLOAT;
begin
  FLength := 0;
  FPattern := 0;
  Result := 0;
  if Pattern then
  begin
    if FArrayChannelMode[Channel] = cmMod then
    begin
      nSongPattern := BASS_MusicGetLength(FArrayMusic[Channel], False);
      FPattern := Round(nSongPattern);
      Result := FPattern;
    end;
  end
  else
  begin
    FLength := GetSongLength(Channel);
    Result := FLength;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHBassManager.SetPan(Channel : Cardinal; Value: TBassPan) : Boolean;
var
  bRet : Boolean;
begin
  bRet := True;
  try
    BASS_ChannelSetAttributes(FArrayMusic[Channel], -1, -1, Value);
    BASS_ChannelSetAttributes(FArrayStream[Channel], -1, -1, Value);
  except
    bRet := False;
  end;
  Result := bRet;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHBassManager.SetVolume(Channel : Cardinal; Value: TBassVolume) : Boolean;
var
  bRet : Boolean;
begin
  bRet := True;
  try
    BASS_ChannelSetAttributes(FArrayMusic[Channel], -1, Value, -101);
    BASS_ChannelSetAttributes(FArrayStream[Channel], -1, Value, -101);
  except
    bRet := False;
  end;
  Result := bRet;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHBassManager.SetFrequency(Channel : Cardinal; Value: TBassFrequency) : Boolean;
var
  bRet : Boolean;
begin
  bRet := True;
  try
    BASS_ChannelSetAttributes(FArrayStream[Channel], Value, -1, -101);
    BASS_ChannelSetAttributes(FArrayMusic[Channel], Value, -1, -101);
  except
    bRet := False;
  end;
  Result := bRet;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHBassManager.GetFrequency(Channel : Cardinal): DWORD;
var
  nFreq, nVol: DWORD;
  nPan : Integer;
begin
  case FArrayChannelMode[Channel] of
    cmStream : BASS_ChannelGetAttributes(FArrayStream[Channel], nFreq, nVol, nPan);
    cmMod    : BASS_ChannelGetAttributes(FArrayMusic[Channel], nFreq, nVol, nPan);
  end;
  Result := nFreq;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHBassManager.GetPan(Channel : Cardinal): Integer;
var
  nFreq, nVol: DWORD;
  nPan : Integer;
begin
  case FArrayChannelMode[Channel] of
    cmStream : BASS_ChannelGetAttributes(FArrayStream[Channel], nFreq, nVol, nPan);
    cmMod    : BASS_ChannelGetAttributes(FArrayMusic[Channel], nFreq, nVol, nPan);
  end;
  Result := nPan;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHBassManager.GetVolume(Channel : Cardinal): DWORD;
var
  nFreq, nVol: DWORD;
  nPan : Integer;
begin
  case FArrayChannelMode[Channel] of
    cmStream : BASS_ChannelGetAttributes(FArrayStream[Channel], nFreq, nVol, nPan);
    cmMod    : BASS_ChannelGetAttributes(FArrayMusic[Channel], nFreq, nVol, nPan);
  end;
  Result := nVol;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHBassManager.GetLevelLeft(Channel: Cardinal): DWORD;
var
  nLevel : DWORD;
begin
  nLevel := BASS_ChannelGetLevel(Channel);
  Result := LOWORD(nLevel);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHBassManager.GetLevelRight(Channel: Cardinal): DWORD;
var
  nLevel : DWORD;
begin
  nLevel := BASS_ChannelGetLevel(Channel);
  Result := HIWORD(nLevel)
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHBassManager.GetStatus(Channel: Cardinal): TBassMode;
var
  nStatus : DWORD;
begin
  nStatus := BASS_ChannelIsActive(FArrayMusic[Channel]);
  if nStatus = BASS_ACTIVE_PLAYING then
    Result := bmPlaying
  else if nStatus = BASS_ACTIVE_PAUSED then
    Result := bmPaused
  else if nStatus = BASS_ACTIVE_STOPPED then
    Result := bmStopped
  else if nStatus = BASS_ACTIVE_STALLED then
    Result := bmStalled
  else
    Result := bmNone;

  FArrayChannelStatus[Channel] := Result;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHBassManager.PauseAll: Boolean;
var
  I : Integer;
  bRet : Boolean;
begin
  bRet := True;
  try
    for I := 0 to 128 do
    begin
      Pause(I);
    end;
  except
    bRet := False;
  end;
  Result := bRet;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHBassManager.PlayAll: Boolean;
var
  I : Integer;
  bRet : Boolean;
begin
  bRet := True;
  try
    for I := 0 to 128 do
    begin
      Play(I);
    end;
  except
    bRet := False;
  end;
  Result := bRet;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHBassManager.ResumeAll: Boolean;
var
  I : Integer;
  bRet : Boolean;
begin
  bRet := True;
  try
    for I := 0 to 128 do
    begin
      Resume(I);
    end;
  except
    bRet := False;
  end;
  Result := bRet;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHBassManager.StopAll: Boolean;
var
  I : Integer;
  bRet : Boolean;
begin
  bRet := True;
  try
    for I := 0 to 128 do
    begin
      Stop(I);
    end;
  except
    bRet := False;
  end;
  Result := bRet;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHBassManager.GetMusicMode(Channel: Cardinal): TChannelMode;
begin
  Result := FArrayChannelMode[Channel];
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHBassManager.SetMODEmulation(FastTracker2, ProTracker1: Boolean);
begin
  FEmulateFastTracker := FastTracker2;
  FEmulateProTracker := ProTracker1;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHBassManager.SetMODOption(Loop, Mono: Boolean);
begin
  FLoop := Loop;
  FMono := Mono;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHBassManager.SetMODRamping(NormalRamp, SensitiveRamp: Boolean);
begin
  FSensitiveRamp := SensitiveRamp;
  FNormalRamp := NormalRamp;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHBassManager.SetMODReaction(PosReset, StopBack: Boolean);
begin
  FStopMoving := PosReset;
  FStopBack := StopBack;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHBassManager.SetMODSurround(Surround, Surround2: Boolean);
begin
  FSurround := Surround;
  FSurround2 := Surround2;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHBassManager.SetMODEffect(Use_3D_Effect, Use_FX_Effect: Boolean);
begin
  FUse_3D_Effect := Use_3D_Effect;
  FUse_FX_Effect := Use_FX_Effect;
end;

end.
