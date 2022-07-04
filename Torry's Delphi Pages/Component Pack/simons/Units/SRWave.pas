unit SRWave;

{ TSRWavePlayer - Komponente (C)opyright 2000   Version 1.02
  Autor : Simon Reinhardt
  eMail : reinhardt@picsoft.de
  Internet : http://www.picsoft.de

  Die Komponente TSRWavePlayer kapselt die Methoden PlaySound (32Bit)
  bzw. sndPlaySound (16Bit) der Windows-API zur Wiedergabe von Wave-Sounds.

  Diese Komponente ist Public Domain, das Urheberrecht liegt aber beim Autor.
  Fragen und Verbesserungsvorschläge sind immer willkommen. }

interface

{$I SRDefine.inc}

uses
  {$IFDEF SR_Win32} Windows, {$ELSE} WinTypes, WinProcs, SysUtils, {$ENDIF}
  Classes, Graphics, Controls, Forms, MMSystem;

type
  TWaveLocation = (wlFile, wlResource, wlRAM);

  TSRWavePlayer = class(TComponent)
  private
    FAsync,
    FLoop         : boolean;
    FWaveName     : string;
    FWavePointer  : pointer;
    FWaveLocation : TWaveLocation;

    FBeforePlay,
    FAfterPlay    : TNotifyEvent;

    procedure SetAfterPlay(Value: TNotifyEvent);
    procedure SetAsync(Value: boolean);
    procedure SetBeforePlay(Value: TNotifyEvent);
    procedure SetLoop(Value: boolean);

  public
    property WavePointer: pointer read fWavePointer write fWavePointer;
    function Play: boolean;
    procedure Stop;

  published
    property Async: boolean read FAsync write SetAsync;
    property Loop: boolean read FLoop write SetLoop;
    {$IFDEF SR_Delphi2_Up}
    property WaveLocation: TWaveLocation read FWaveLocation write fWaveLocation default wlFile;
    {$ENDIF}
    property WaveName: string read FWaveName write FWaveName;
    property BeforePlay: TNotifyEvent read FBeforePlay write SetBeforePlay;
    property AfterPlay: TNotifyEvent read FAfterPlay write SetAfterPlay;

  end;

procedure Register;

implementation

{$IFDEF SR_Delphi2_Up}
{$R *.D32}
{$ELSE}
{$R *.D16}
{$ENDIF}

procedure TSRWavePlayer.SetAfterPlay(Value: TNotifyEvent);
begin
  FAfterPlay:=Value;
end;

procedure TSRWavePlayer.SetAsync(Value: boolean);
begin
  FAsync:=Value;
  if not FAsync then
    FLoop:=false;
end;

procedure TSRWavePlayer.SetBeforePlay(Value: TNotifyEvent);
begin
  FBeforePlay:=Value;
end;

procedure TSRWavePlayer.SetLoop(Value: boolean);
begin
  if (FLoop<>Value) and FAsync then
    FLoop:=Value;
end;

function TSRWavePlayer.Play;
{$IFDEF SR_Delphi2_Up}
var Flags     : DWORD;
{$ELSE}
var Flags     : WORD;
    PWaveName : PChar;
{$ENDIF}
begin
  if Assigned(FBeforePlay) then
    FBeforePlay(Self);
  {$IFDEF SR_Delphi2_Up}
    case FWaveLocation of
      wlFile     : Flags:=SND_FILENAME;
      wlResource : Flags:=SND_RESOURCE;
      else         Flags:=SND_MEMORY;
    end;
  {$ELSE}
  Flags := 0;
  {$ENDIF}
  if FLoop then
    Flags:=Flags or SND_LOOP;
  if FAsync then
    Flags:=Flags or SND_ASYNC
  else
    Flags:=Flags or SND_SYNC;
  {$IFDEF SR_Delphi2_Up}
  if FWaveLocation = wlRAM then
    Result:=PlaySound(FWavePointer, 0, Flags)
  else
    Result:=PlaySound(PChar(FWaveName), HInstance, Flags);
  {$ELSE}
  PWaveName:=StrAlloc(255);
  StrPCopy(PWaveName, FWaveName);
  Result:=sndPlaySound(PWaveName, Flags);
  StrDispose(PWaveName);
  {$ENDIF}
  if Assigned(FAfterPlay) then
    FAfterPlay(Self);
end;

procedure TSRWavePlayer.Stop;
{$IFDEF SR_Delphi2_Up}
var Flags : DWORD;
{$ELSE}
var Flags : WORD;
{$ENDIF}
begin
  {$IFDEF SR_Delphi2_Up}
  case FWaveLocation of
    wlFile     : Flags:=SND_FILENAME;
    wlResource : Flags:=SND_RESOURCE;
    else         Flags:=SND_MEMORY;
  end;
  PlaySound(nil, 0, Flags);
  {$ELSE}
  sndPlaySound(nil, 0);
  {$ENDIF}
end;

procedure Register;
begin
  RegisterComponents('Simon', [TSRWavePlayer]);
end;

end.
