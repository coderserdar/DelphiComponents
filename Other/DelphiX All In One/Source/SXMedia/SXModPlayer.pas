{
 SXMedia  Components - Beta 1
 --------------------------------
 Copyright 1999 Dean Ellis
 http://www.sillex.freeserve.co.uk

 This unit is part of the SXMedia Component Set. This code is
 supplied as is with no guarantees and must be used at your own
 risk.

 No modifications to this code must be made without the express
 permission of the author. Please report any problems to
 support@sillex.freeserve.co.uk

 You may use these components to create any freeware/shareware
 applications that you wish. If the components are to be used in
 a commercail product then credit for developement of these components
 should be given.

 Credits :

 Developer : Dean Ellis
 Testers   : Dominique Louis
             Ivan Blecic
             Naoki Haga

Version History
--------------------------------------------------------------------------------
25/01/2000 Modified Destroy and Stop methods to check IDSBuffer before
           calling IBuffer.Stop. Stops the "Buffer not Created exception"
           being thrown.
06/02/2000 Added OnStop and OnStart events
           Added Finalize method. This does not have to be called but if you
           manually finalize and initialize the DXSound Component you
           need to call this method to clear the buffers and the Threads.
22/02/2000 Modified SetLoop code to correct functionality.
           Modifed Stop method to make sure the OnStop event is only called
           if it wsa playing and that the Value of Playing would be False
           when the event is fired.
15/05/2000 Made use of the MppSdkLibLoaded variable to make sure the component
           does nothing if the mppsdk.dll is not found.
           Removed the raising of an exception in the constructor as it seemed to
           be causing more problems than it was solving.
--------------------------------------------------------------------------------
}
unit SXModPlayer;

{$INCLUDE DelphiXcfg.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DXSounds, MMSystem, MpSndSys,
  {$IFDEF StandardDX}
  DirectDraw, DirectSound,
    {$IFDEF DX7}
      {$IFDEF D3DRM}
  Direct3DRM,
      {$ENDIF}
  Direct3D;
    {$ENDIF}
    {$IFDEF DX9}
  Direct3D9, Direct3D, D3DX9, {Direct3D8,} DX7toDX8;
    {$ENDIF}
  {$ELSE}
  DirectX;
  {$ENDIF}

type

   TModOption = (NoResampling, BassExpansion, Surround, Reverb,
                  HighQuality, GainControl, NoiseReduction);

   TModOptions = Set of TModOption;

   TSXModPlayer = class(TComponent)
     private
       { Private declarations }
       FFilename: TFilename;
       FDXSound : TDXSound;
       FSoundBuffer : TDirectSoundBuffer;
       FSoundNotify : IDirectSoundNotify;
       FBufferDesc : TDSBufferDesc;
       FWaveFormat : TWaveFormatEx;
       FLoop : Boolean;
       FPlaying : Boolean;
       FOptions : TModOptions;
       FEvents: TList;
       FNotify: TList;
       FOnStop: TNotifyEvent;
       FOnStart: TNotifyEvent;
       function GetPosition : Integer;
       function GetSilenceData:Integer;
     protected
       { Protected declarations }
       procedure InitSoundEvents;
       procedure ThreadCallback;
       procedure SetFilename(const Value : TFilename);
       procedure SetLoop(const Value : Boolean);
       procedure SetOptions(const Value : TModOptions);
       procedure Notification(AComponent: TComponent; Operation: TOperation); override;
       procedure DoStop;
       procedure DoStart;

       function CreateEventList : TList;
       function CreateNotifyList : TList;
       function CreateSoundBuffer : TDirectSoundBuffer;
       function CreateSoundNotify : IDirectSoundNotify;
       procedure ClearSoundNotify;
       procedure CreateEventThread;
       procedure ClearSoundBuffer;

       procedure StartThread;
       procedure StopThread;

       property BufferDesc : TDSBufferDesc read FBufferDesc write FBufferDesc;
       property WaveFormat : TWaveFormatEx read FWaveFormat write FWaveFormat;
       property SoundBuffer : TDirectSoundBuffer read FSoundBuffer write FSoundBuffer;
       property SoundNotify : IDirectSoundNotify read FSoundNotify write FSoundNotify;
       property Events : TList read FEvents write FEvents;
       property Notify : TList read FNotify write FNotify;
     public
       { Public declarations }
       constructor Create( AOwner : TComponent); override;
       destructor Destroy; override;
       procedure Initialize(Stream:TMemoryStream);
       procedure Finalize;
       procedure Play(Loop : Boolean);
       procedure Stop;
       procedure Reset;

       property Position : Integer read GetPosition;
       property Playing : Boolean read FPlaying;
     published
       { Published declarations }
       property Filename : TFilename read FFilename write SetFilename;
       property DXSound : TDXSound read FDXSound write FDXSound;
       property Looping : Boolean read FLoop write SetLoop default False;
       property Options : TModOptions read FOptions write SetOptions;
       property OnStart : TNotifyEvent read FOnStart write FOnStart;
       property OnStop : TNotifyEvent read FOnStop write FOnStop;
  end;

implementation

// If you are having problems compiling the package edit this file.
// Comment out this include Statement. You should only need to do this
// if you are NOT using the latest version of DelphiX (991024)
// Comment this Define out if you are using DelphiX992404 or earlier
{$DEFINE VERSION991024}

const
    EVENTCOUNT:Integer = 2;

type
    {Music Event Callback thread}
    TSXEventThread = class(TThread)
       private
          FEventCallback:TThreadMethod;
       public
          procedure Execute; override;
          property EventCallback : TThreadMethod read FEventCallback write FEventCallback;

    end;


var

   EventThread : TSXEventThread;
   CurrentEvent : Integer;

{ TSXEventThread }

procedure TSXEventThread.Execute;
begin
  if not Assigned(EventCallback) then Terminate;
  while not Terminated do
  begin
     EventCallback;
  end;
end;

{ TSXModPlayer }

constructor TSXModPlayer.Create(AOwner : TComponent);
begin
   inherited Create(AOwner);
   if MppSdkLibLoaded then
     ModMixer.SetMixerOptions(0);
   Events := CreateEventList;
   Notify := CreateNotifyList;
end;
destructor TSXModPlayer.Destroy;
begin
   Finalize;
   {}
   Notify.Free;
   Events.Free;
   inherited Destroy;
end;
procedure TSXModPlayer.Initialize(Stream:TMemoryStream);
var Data:Pointer;
    Size:Integer;
    FreeStream:Boolean;
begin
  try
     if MppSdkLibLoaded then
     begin
       if SoundBuffer = nil then InitSoundEvents;
       FreeStream := False;
       if Stream = nil then
       begin
          Stream := TMemoryStream.Create;
          Stream.LoadFromFile(Filename);
          FreeStream := True;
       end;
       Data := Stream.Memory;
       Size := Stream.Size;
       ModMixer.FreeSong;
       ModMixer.LoadSong(Data,Size);
       if FreeStream then Stream.Free;
     end;
  except
  end;
end;
procedure TSXModPlayer.Finalize;
begin
   Stop;
   if Assigned(EventThread) then
   begin
      EventThread.Terminate;
      EventThread.Free;
      EventThread := nil;
   end;
   ClearSoundNotify;
   if Assigned(SoundBuffer) then
   begin
      try
         if Playing and (SoundBuffer.IDSBuffer <> nil) then
            SoundBuffer.IBuffer.Stop;
      finally
         SoundBuffer := nil;
      end;
   end;
end;

procedure TSXModPlayer.InitSoundEvents;
// DelphiX Version 991024 Edit Version.inc to change declarations
{$IFDEF VERSION991024}
  var SizeWritten: Cardinal;
{$ELSE}
// DelphiX Version 992404 and earlier
  var  SizeWritten : Integer;
{$ENDIF}
begin
   if MppSdkLibLoaded and Assigned(DXSound) then
   begin
      DXSound.Primary.IBuffer.GetFormat(@FWaveFormat,Sizeof(WaveFormat),@SizeWritten);
      ModMixer.SetWaveFormat(WaveFormat.nSamplesPerSec,WaveFormat.nChannels,WaveFormat.wBitsPerSample);
      SoundBuffer := CreateSoundBuffer;
      SoundNotify := CreateSoundNotify;
      if (SoundBuffer <> nil) and (SoundNotify <> nil) then
         CreateEventThread;
   end;
end;
procedure TSXModPlayer.ThreadCallback;
var Msg : TMsg;

   procedure ReadData(Event:Integer);
   var W1:pointer;
       // DelphiX Version 991024 Edit Version.inc to change declarations
       {$IFDEF VERSION991024}
       S1, S2 : Cardinal;
       {$ELSE}
       // DelphiX Version 992404 and earlier
       S1,S2:Integer;
       {$ENDIF}
       NumWrite:Longint;
       Pos:Integer;
       Written1:Integer;
   begin
     if Event = 0 then
        Pos := TDSBPositionNotify(Notify[EVENTCOUNT -1]^).dwOffset
     else
        Pos := TDSBPositionNotify(Notify[Event-1]^).dwOffset;
     NumWrite := TDSBPositionNotify(Notify[Event]^).dwOffset - Pos;
     if (NumWrite < 0) then
        inc(NumWrite,BufferDesc.dwBufferBytes);
     if SoundBuffer.IBuffer.Lock(Pos,NumWrite,w1,{$IFDEF UNICODE}@{$ENDIF}s1,Pointer(nil^),{$IFDEF UNICODE}@{$ENDIF}s2,0) = 0 then
     begin
        Written1 := ModMixer.Render(W1,S1);
        SoundBuffer.IBuffer.Unlock(w1,Written1, nil,0);
        if (Written1 = 0) then
           Stop;
     end;
   end;

begin
    CurrentEvent := MsgWaitForMultipleObjects(EVENTCOUNT,Events.List{$IFNDEF UNICODE}^{$ENDIF}[0], False, INFINITE, QS_ALLINPUT);
    dec(CurrentEvent,WAIT_OBJECT_0);
    if CurrentEvent >= EVENTCOUNT then
    begin
      while (PeekMessage(Msg, 0, 0,0, PM_REMOVE)) do
      begin
        if Msg.Message = WM_QUIT then
           Stop
        else
        begin
          TranslateMessage(Msg);
          DispatchMessage(Msg);
        end;
      end;
    end
    else
    begin
       ReadData(CurrentEvent);
    end;
end;
function TSXModPlayer.CreateEventList : TList;
begin
  Result := TList.Create;
  Result.Capacity := EVENTCOUNT;
end;
function TSXModPlayer.CreateNotifyList : TList;
begin
  Result := TList.Create;
  Result.Capacity := EVENTCOUNT;
end;
function TSXModPlayer.CreateSoundBuffer : TDirectSoundBuffer;
{$IFDEF UNICODE}
const DSBCAPS_CTRLDEFAULT = DSBCAPS_CTRLPAN or DSBCAPS_CTRLVOLUME or DSBCAPS_CTRLFREQUENCY;
{$ENDIF}
begin
   Result := TDirectSoundBuffer.Create(DXSound.DSound);
   ZeroMemory(@BufferDesc,Sizeof(TDSBufferDesc));
   FBufferDesc.dwSize := Sizeof(TDSBufferDesc);
   FBufferDesc.dwFlags := DSBCAPS_CTRLDEFAULT or DSBCAPS_STATIC or
                         DSBCAPS_GETCURRENTPOSITION2 or DSBCAPS_CTRLPOSITIONNOTIFY;
   FBufferDesc.dwBufferBytes := WaveFormat.nAvgBytesPerSec * 2;
   FBufferDesc.lpwfxFormat := @WaveFormat;
   Result.CreateBuffer(BufferDesc);
end;
function TSXModPlayer.CreateSoundNotify : IDirectSoundNotify;
type TNotifyArray = Array[0..99] of TDSBPositionNotify;
var PDSNotify : PDSBPositionNotify;
    index : Integer; Offset: Integer;
    PNotify: ^TNotifyArray;
begin
   Result := nil;
   if SoundBuffer.IBuffer.QueryInterface(IID_IDirectSoundNotify,Result) = 0 then
   begin
      {setup notifications here}
      Offset := 0;
      GetMem(PNotify,EVENTCOUNT * Sizeof(TDSBPositionNotify));
      for Index := 1 to EVENTCOUNT do
      begin
         New(PDSNotify);
         PDSNotify^.dwOffset := OffSet;
         PDSNotify^.hEventNotify := CreateEvent(nil,False,False,nil);
         Notify.Add(PDSNotify);
         Events.Add(Pointer(PDSNotify^.hEventNotify));
         PNotify[Index-1] := PDSNotify^;
         inc(Offset,WaveFormat.nAvgBytesPerSec div EVENTCOUNT);
      end;

      if Result.SetNotificationPositions(EVENTCOUNT,{$IFDEF UNICODE}@{$ENDIF}PNotify[0]) <> 0 then
         ShowMessage('Notification Falied');
      FreeMem(PNotify,EVENTCOUNT * Sizeof(TDSBPositionNotify));
   end;
end;
procedure TSXModPlayer.ClearSoundNotify;
var PDSNotify : PDSBPositionNotify;
    Index : Integer;
begin
   for Index := Notify.Count -1 downto 0 do
   begin
      PDSNotify := Notify[Index];
      Notify.Delete(Index);
      Events.Delete(Index);
      CloseHandle(PDSNotify^.hEventNotify);
   end;
   SoundNotify := nil;
end;
procedure TSXModPlayer.CreateEventThread;
begin
  if not Assigned(EventThread) then
  begin
     EventThread := TSXEventThread.Create(True);
     EventThread.Priority := tpNormal;
     EventThread.EventCallback := ThreadCallback;
  end;
end;

procedure TSXModPlayer.ClearSoundBuffer;
var w1,w2:pointer;
    // DelphiX Version 991024 Edit Version.inc to change declarations
    {$IFDEF VERSION991024}
    S1, S2 : Cardinal;
    {$ELSE}
    // DelphiX Version 992404 and earlier
    S1,S2:Integer;
    {$ENDIF}
    Data:Word;
begin
  Data := GetSilenceData;
  if SoundBuffer.IBuffer.Lock(0,0,w1,{$IFDEF UNICODE}@{$ENDIF}s1,w2,{$IFDEF UNICODE}@{$ENDIF}s2,DSBLOCK_ENTIREBUFFER) = 0 then
  begin
     FillMemory(W1,S1,Data);
     if W2 <> nil then
        FillMemory(W2,S2,Data);
     SoundBuffer.IBuffer.Unlock(W1,S1,W2,S2);
  end;
end;
procedure TSXModPlayer.StartThread;
begin
  if Assigned(EventThread) then EventThread.Resume;
end;
procedure TSXModPlayer.StopThread;
begin
  if Assigned(EventThread) then EventThread.Suspend;
end;
{}
procedure TSXModPlayer.Play(Loop : Boolean);
begin
   if MppSdkLibLoaded then
   begin
     if Assigned(SoundBuffer) and Assigned(EventThread) then
     begin
        SetLoop(Loop);
        ClearSoundBuffer;
        StartThread;
        FPlaying := True;
        DoStart;
        SoundBuffer.IBuffer.Play(0,0,DSBPLAY_LOOPING);
     end;
   end;
end;
procedure TSXModPlayer.Stop;
begin
   if MppSdkLibLoaded then
   begin
     if Assigned(SoundBuffer) and Assigned(EventThread) then
     begin
        try
          if Playing and (SoundBuffer.IDSBuffer <> nil) then
          begin
            FPlaying := False;
            SoundBuffer.IBuffer.Stop;
            DoStop;
          end;
        finally
          StopThread;
        end;
     end;
   end;
end;
procedure TSXModPlayer.Reset;
begin
   if MppSdkLibLoaded then
     ModMixer.SetCurrentOrder(0);
end;
{Property Accessors}
function TSXModPlayer.GetPosition : Integer;
begin
  Result := 0;
end;
procedure TSXModPlayer.SetFilename(const Value : TFilename);
begin
  if FFilename <> Value then
  begin
     FFilename := Value;
  end;
end;
procedure TSXModPlayer.SetLoop( const Value : Boolean);
var Flags : DWord;
begin
   if FLoop <> Value then
   begin
      FLoop := Value;
      if MppSdkLibLoaded then
      begin
        Flags := ModMixer.GetMixerOptions;
        case Value of
           True:Flags := Flags or MPPMIX_LOOP;
           False:Flags := Flags and (not MPPMIX_LOOP);
        end;
        ModMixer.SetMixerOptions(Flags);
      end;
   end;
end;
procedure TSXModPlayer.SetOptions( const Value : TModOptions );
const OptionArray: array[Boolean,TModOption] of Integer = (
    (0,0,0,0,0,0,0),
   (MPPMIX_NORESAMPLING, MPPMIX_BASSEXPANSION,  MPPMIX_SURROUND,
   MPPMIX_REVERB, MPPMIX_HIGHQUALITY, MPPMIX_GAINCONTROL,
   MPPMIX_NOISEREDUCTION)
   );
var Flags : DWord;
begin
   if FOptions <> Value then
   begin
      FOptions := Value;
      if MppSdkLibLoaded then
      begin
        Flags := 0;
        Flags := Flags or OptionArray[NoResampling in Value,NoResampling];
        Flags := Flags or OptionArray[BassExpansion in Value,BassExpansion];
        Flags := Flags or OptionArray[Surround in Value,Surround];
        Flags := Flags or OptionArray[Reverb in Value,Reverb];
        Flags := Flags or OptionArray[HighQuality in Value,HighQuality];
        Flags := Flags or OptionArray[GainControl in Value,GainControl];
        Flags := Flags or OptionArray[NoiseReduction in Value,NoiseReduction];
        ModMixer.SetMixerOptions(Flags);
        SetLoop(Looping);
      end;
   end;
end;
procedure TSXModPlayer.Notification(AComponent: TComponent; Operation: TOperation);
begin
   inherited Notification( AComponent, Operation);

   if (Operation = opRemove) and (AComponent = DXSound) then
      DXSound := nil;
end;
procedure TSXModPlayer.DoStop;
begin
  if Assigned(FOnStop) then
    FOnStop(Self);
end;
procedure TSXModPlayer.DoStart;
begin
  if Assigned(FOnStart) then
    FOnStart(Self);
end;
function TSXModPlayer.GetSilenceData:integer;
const SilenceData:array[1..2] of integer = ($80,$0);
begin
  Result := SilenceData[WaveFormat.wBitsPerSample div 8];
end;

end.
