{------------------------------------------------------------------------------}
{                                                                              }
{  WaveIO - Abstract definition of wave audio input/output                     }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

{$I DELPHIAREA.INC}

unit scWaveIO;

interface

uses
  Windows, Messages, Classes, SysUtils, MMSystem, scWaveUtils;

type

  // The base abstract class for wave audio player and recorder components
  TWaveAudioIO = class(TComponent)
  private
    fAsync: Boolean;
    fDeviceID: DWORD;
    fLastError: MMRESULT;
    fBufferLength: WORD;
    fBufferCount: WORD;
    fOpening: Boolean;
    fClosing: Boolean;
    fCallback: DWORD;
    fCallbackType: DWORD;
    fOnActivate: TWaveAudioEvent;
    fOnDeactivate: TWaveAudioEvent;
    fOnPause: TWaveAudioEvent;
    fOnResume: TWaveAudioEvent;
    fOnError: TWaveAudioEvent;
    fOnLevel: TWaveAudioLevelEvent;
    fOnFilter: TWaveAudioFilterEvent;
    Buffers: TList;
    ThreadEvent: THandle;
    ThreadHandle: THandle;
    CS: TRTLCriticalSection;
    procedure SetAsync(Value: Boolean);
    procedure SetDeviceID(Value: DWORD);
    procedure SetBufferLength(Value: WORD);
    procedure SetBufferCount(Value: WORD);
    function GetActiveBufferCount: WORD;
    function GetLastErrorText: String;
    function GetPreferredBufferSize: DWORD;
  protected
    // Helper Procedures
    procedure Lock;
    procedure Unlock;
    procedure CreateCallback;
    procedure DestroyCallback;
    procedure PostWaveMessage(WaveMsg: DWORD; pWaveHeader: PWaveHdr);
    function ProcessWaveMessage(Msg: DWORD; pWaveHeader: PWaveHdr): Boolean;
    procedure CallbackWindowProc(var Message: TMessage);
    function Success(mmResult: MMRESULT): Boolean;
    function mmTimeToMS(const mmTime: TMMTime): DWORD;
    function ReallocateBuffer(var pWaveHeader: PWaveHdr;
      BufferSize: DWORD; Buffer: Pointer): Boolean;
  protected
    // Callback Notifications
    procedure DoWaveInDeviceOpen; virtual;
    procedure DoWaveInDeviceClose; virtual;
    procedure DoWaveInDeviceData(pWaveHeader: PWaveHdr); virtual;
    procedure DoWaveOutDeviceOpen; virtual;
    procedure DoWaveOutDeviceClose; virtual;
    procedure DoWaveOutDeviceDone(pWaveHeader: PWaveHdr); virtual;
  protected
    WaveFormat: TWaveFormatEx; // only for some internal calculations
    function GetNumDevs: DWORD; virtual; abstract;
    function GetActive: Boolean; virtual;
    procedure SetActive(Value: Boolean); virtual;
    function GetPaused: Boolean; virtual; abstract;
    procedure SetPaused(Value: Boolean); virtual;
    function GetDeviceName: String; virtual; abstract;
    function GetDeviceFormats: TWaveDeviceFormats; virtual; abstract;
    function GetPosition: DWORD; virtual; abstract;
    function GetErrorText(ErrorCode: MMRESULT): String; virtual; abstract;
    procedure GetWaveFormat(var pWaveFormat: PWaveFormatEx;
      var FreeIt: Boolean); virtual; abstract;
    function ValidateDeviceID(ADeviceID: DWORD): MMRESULT; virtual; abstract;
    function InternalOpen: Boolean; virtual; abstract;
    function InternalClose: Boolean; virtual; abstract;
    function InternalPause: Boolean; virtual; abstract;
    function InternalResume: Boolean; virtual; abstract;
    function HandleAllocated: Boolean; virtual; abstract;
    procedure DefineBuffers; virtual; abstract;
    procedure ResetBuffers; virtual;
    procedure DoDeviceOpen; virtual;
    procedure DoDeviceClose; virtual;
    procedure DoActivate; virtual;
    procedure DoDeactivate; virtual;
    procedure DoPause; virtual;
    procedure DoResume; virtual;
    procedure DoError; virtual;
    procedure DoLevel(const Buffer: Pointer; BufferSize: DWORD); virtual;
    procedure DoFilter(const Buffer: Pointer; BufferSize: DWORD); virtual;
  protected
    property Callback: DWORD read fCallback;
    property CallbackType: DWORD read fCallbackType;
    property Opening: Boolean read fOpening write fOpening;
    property Closing: Boolean read fClosing write fClosing;
    property PreferredBufferSize: DWORD read GetPreferredBufferSize;
    property NumDevs: DWORD read GetNumDevs;
    property DeviceName: String read GetDeviceName;
    property DeviceFormats: TWaveDeviceFormats read GetDeviceFormats;
    property Position: DWORD read GetPosition; // Milliseconds
    property LastError: MMRESULT read fLastError;
    property LastErrorText: String read GetLastErrorText;
    property ActiveBufferCount: WORD read GetActiveBufferCount;
    property DeviceID: DWORD read fDeviceID write SetDeviceID default WAVE_MAPPER;
    property BufferLength: WORD read fBufferLength write SetBufferLength default 1000; // Milliseconds
    property BufferCount: WORD read fBufferCount write SetBufferCount default 2;
    property Async: Boolean read fAsync write SetAsync default False;
    property Paused: Boolean read GetPaused write SetPaused default False;
    property Active: Boolean read GetActive write SetActive default False; // As published, should be the last one
    property OnActivate: TWaveAudioEvent read fOnActivate write fOnActivate;
    property OnDeactivate: TWaveAudioEvent read fOnDeactivate write fOnDeactivate;
    property OnPause: TWaveAudioEvent read fOnPause write fOnPause;
    property OnResume: TWaveAudioEvent read fOnResume write fOnResume;
    property OnError: TWaveAudioEvent read fOnError write fOnError;
    property OnLevel: TWaveAudioLevelEvent read fOnLevel write fOnLevel;
    property OnFilter: TWaveAudioFilterEvent read fOnFilter write fOnFilter;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure WaitForStart;
    procedure WaitForStop;
    function Query(const pWaveFormat: PWaveFormatEx): Boolean; virtual; abstract;
    function QueryPCM(PCMFormat: TPCMFormat): Boolean; virtual;
  end;

implementation

{$IFNDEF COMPILER6_UP}
uses Forms;
{$ENDIF}

{ TWaveThread }

type
  TWaveThread = class(TThread)
  private
    WaveAudioIO: TWaveAudioIO;
  protected
    procedure Execute; override;
  public
    constructor Create(Owner: TWaveAudioIO);
    destructor Destroy; override;
  end;

constructor TWaveThread.Create(Owner: TWaveAudioIO);
begin
  inherited Create(True);
  WaveAudioIO := Owner;
  WaveAudioIO.ThreadHandle := Handle;
  WaveAudioIO.ThreadEvent := CreateEvent(nil, True, False, nil);
  FreeOnTerminate := True;
  Priority := tpHigher;
  Resume;
end;

destructor TWaveThread.Destroy;
begin
  CloseHandle(WaveAudioIO.ThreadEvent);
  WaveAudioIO.ThreadEvent := 0;
  inherited Destroy;
end;

procedure TWaveThread.Execute;
var
  MSG: TMSG;
begin
  PeekMessage(MSG, 0, 0, 0, PM_NOREMOVE);
  SetEvent(WaveAudioIO.ThreadEvent);
  while GetMessage(MSG, 0, 0, 0) do
    WaveAudioIO.ProcessWaveMessage(MSG.Message, PWaveHdr(MSG.lParam));
  ResetEvent(WaveAudioIO.ThreadEvent);
  WaveAudioIO.ThreadHandle := 0;
end;

{ TWaveAudioIO }

constructor TWaveAudioIO.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InitializeCriticalSection(CS);
  fDeviceID := WAVE_MAPPER;
  fBufferLength := 1000;
  fBufferCount := 2;
  Buffers := TList.Create;
end;

destructor TWaveAudioIO.Destroy;
begin
  Active := False;
  WaitForStop;
  Buffers.Free;
  DeleteCriticalSection(CS);
  inherited Destroy;
end;

procedure TWaveAudioIO.SetAsync(Value: Boolean);
begin
  if not Active and (Async <> Value) then
    fAsync := Value;
end;

procedure TWaveAudioIO.SetDeviceID(Value: DWORD);
begin
  if (DeviceID <> Value) then
  begin
    if Active then
      raise EWaveAudioInvalidOperation.Create('DeviceID cannot be changed while device is open')
    else if Success(ValidateDeviceID(Value)) then
      fDeviceID := Value;
  end;
end;

function TWaveAudioIO.GetActive: Boolean;
begin
  Result := (Callback <> 0);
end;

procedure TWaveAudioIO.SetActive(Value: Boolean);
begin
  if Active <> Value then
  begin
    if Value then
      InternalOpen
    else
      InternalClose;
  end;
end;

procedure TWaveAudioIO.SetPaused(Value: Boolean);
begin
  if Paused <> Value then
  begin
    if Value then
      InternalPause
    else
      InternalResume;
  end;
end;

procedure TWaveAudioIO.SetBufferLength(Value: WORD);
begin
  if Value < 10 then
    fBufferLength := 10
  else
    fBufferLength := Value;
end;

procedure TWaveAudioIO.SetBufferCount(Value: WORD);
begin
  fBufferCount := Value;
end;

function TWaveAudioIO.GetActiveBufferCount: WORD;
begin
  Result := Buffers.Count;
end;

function TWaveAudioIO.GetPreferredBufferSize: DWORD;
begin
  Result := CalcWaveBufferSize(@WaveFormat, BufferLength);
end;

function TWaveAudioIO.GetLastErrorText: String;
begin
  Result := GetErrorText(fLastError);
end;

procedure TWaveAudioIO.DoDeviceOpen;
begin
  Opening := False;
  DefineBuffers;
  DoActivate;
end;

procedure TWaveAudioIO.DoDeviceClose;
begin
  DoLevel(nil, 0);
  Closing := False;
  DestroyCallback;
  ResetBuffers;
  DoDeactivate;
end;

procedure TWaveAudioIO.DoActivate;
begin
  if Assigned(fOnActivate) then
    fOnActivate(Self);
end;

procedure TWaveAudioIO.DoDeactivate;
begin
  if Assigned(fOnDeactivate) and not (csDestroying in ComponentState) then
    fOnDeactivate(Self);
end;

procedure TWaveAudioIO.DoPause;
begin
  if Assigned(fOnPause) then
    fOnPause(Self);
end;

procedure TWaveAudioIO.DoResume;
begin
  if Assigned(fOnResume) then
    fOnResume(Self);
end;

procedure TWaveAudioIO.DoError;
begin
  if Assigned(fOnError) then
    fOnError(Self)
  else
    raise EWaveAudioSysError.Create(LastErrorText);
end;

procedure TWaveAudioIO.DoLevel(const Buffer: Pointer; BufferSize: DWORD);
begin
  if Assigned(fOnLevel) and not (csDestroying in ComponentState) and
    (WaveFormat.wFormatTag = WAVE_FORMAT_PCM) then
  begin
    fOnLevel(Self, GetWaveAudioPeakLevel(Buffer, BufferSize, @WaveFormat));
  end;
end;

procedure TWaveAudioIO.DoFilter(const Buffer: Pointer; BufferSize: DWORD);
begin
  if Assigned(fOnFilter) and not (csDestroying in ComponentState) then
    fOnFilter(Self, Buffer, BufferSize);
end;

procedure TWaveAudioIO.Lock;
begin
  EnterCriticalSection(CS);
end;

procedure TWaveAudioIO.Unlock;
begin
  LeaveCriticalSection(CS);
end;

procedure TWaveAudioIO.CreateCallback;
begin
  if Async then
  begin
    fCallbackType := CALLBACK_THREAD;
    fCallback := TWaveThread.Create(Self).ThreadID;
  end
  else
  begin
    fCallbackType := CALLBACK_WINDOW;
    fCallback := AllocateHWnd(CallbackWindowProc);
    WaitForSyncObject(ThreadEvent, INFINITE);
  end;
end;

procedure TWaveAudioIO.DestroyCallback;
begin
  if Callback <> 0 then
  begin
    if ThreadHandle <> 0 then
      while not PostThreadMessage(Callback, WM_QUIT, 0, 0) do
        Sleep(0)
    else
      DeallocateHWnd(Callback);
    fCallback := 0;
  end;
end;

procedure TWaveAudioIO.PostWaveMessage(WaveMsg: DWORD; pWaveHeader: PWaveHdr);
begin
  if Callback <> 0 then
  begin
    if ThreadHandle <> 0 then
      while not PostThreadMessage(Callback, WaveMsg, 0, Integer(pWaveHeader)) do
        Sleep(0)
    else
      PostMessage(Callback, WaveMsg, 0, Integer(pWaveHeader));
  end;
end;

function TWaveAudioIO.ProcessWaveMessage(Msg: DWORD; pWaveHeader: PWaveHdr): Boolean;
begin
  Result := True;
  try
    case Msg of
      MM_WIM_OPEN:
      begin
        EnterCriticalSection(CS);
        try
          DoWaveInDeviceOpen;
        finally
          LeaveCriticalSection(CS);
        end;
      end;
      MM_WIM_DATA:
      begin
        EnterCriticalSection(CS);
        try
          DoWaveInDeviceData(pWaveHeader);
        finally
          LeaveCriticalSection(CS);
        end;
      end;
      MM_WIM_Close:
      begin
        EnterCriticalSection(CS);
        try
          DoWaveInDeviceClose;
        finally
          LeaveCriticalSection(CS);
        end;
      end;
      MM_WOM_OPEN:
      begin
        EnterCriticalSection(CS);
        try
          DoWaveOutDeviceOpen;
        finally
          LeaveCriticalSection(CS);
        end;
      end;
      MM_WOM_DONE:
      begin
        EnterCriticalSection(CS);
        try
          DoWaveOutDeviceDone(pWaveHeader);
        finally
          LeaveCriticalSection(CS);
        end;
      end;
      MM_WOM_CLOSE:
      begin
        EnterCriticalSection(CS);
        try
          DoWaveOutDeviceClose;
        finally
          LeaveCriticalSection(CS);
        end;
      end;
    else
      Result := False;
    end;
  except
    ShowException(ExceptObject, ExceptAddr);
  end;
end;

procedure TWaveAudioIO.CallbackWindowProc(var Message: TMessage);
begin
  if not ProcessWaveMessage(Message.Msg, PWaveHdr(Message.LParam)) then
    with Message do Result := DefWindowProc(Callback, Msg, WParam, LParam);
end;

function TWaveAudioIO.Success(mmResult: MMRESULT): Boolean;
begin
  Result := True;
  fLastError := mmResult;
  if mmResult <> MMSYSERR_NOERROR then
  begin
    Result := False;
    DoError;
  end;
end;

function TWaveAudioIO.mmTimeToMS(const mmTime: TMMTime): DWORD;
begin
  case mmTime.wType of
    TIME_MS:
      Result := mmTime.ms;
    TIME_BYTES:
      if WaveFormat.nAvgBytesPerSec <> 0 then
        Result := MulDiv(1000, mmTime.cb, WaveFormat.nAvgBytesPerSec)
      else
        Result := 0;
    TIME_SAMPLES:
      if WaveFormat.nSamplesPerSec <> 0 then
        Result := MulDiv(1000, mmTime.sample, WaveFormat.nSamplesPerSec)
      else
        Result := 0;
    TIME_SMPTE:
      Result := 1000 * ((mmTime.hour * 3600) + (mmTime.min * 60) + mmTime.sec);
  else
    Result := 0;
  end;
end;

function TWaveAudioIO.ReallocateBuffer(var pWaveHeader: PWaveHdr;
  BufferSize: DWORD; Buffer: Pointer): Boolean;
var
  InternalBuffer: Boolean;
begin
  Result := True;
  if BufferSize = 0 then
  begin
    if Assigned(pWaveHeader) then
    begin
      Buffers.Remove(pWaveHeader);
      if pWaveHeader.dwUser = DWORD(Self) then
        ReallocMem(pWaveHeader^.lpData, 0);
      ReallocMem(pWaveHeader, 0);
    end;
  end
  else
  begin
    InternalBuffer := not Assigned(Buffer);
    if not Assigned(pWaveHeader) then
    begin
      try
        ReallocMem(pWaveHeader, SizeOf(TWaveHdr));
        FillChar(pWaveHeader^, SizeOf(TWaveHdr), 0);
        Buffers.Add(pWaveHeader);
      except
        Result := False;
        pWaveHeader := nil;
        Success(MMSYSERR_NOMEM); // Raises an OnError event
      end;
    end;
    if Assigned(pWaveHeader) then
    begin
      if pWaveHeader^.dwUser <> DWORD(Self) then
      begin
        pWaveHeader^.lpData := nil;
        pWaveHeader^.dwBufferLength := 0;
      end;
      if InternalBuffer then
      begin
        Buffer := pWaveHeader^.lpData;
        if pWaveHeader^.dwBufferLength <> BufferSize then
        begin
          try
            ReallocMem(Buffer, BufferSize);
          except
            Result := False;
            ReallocateBuffer(pWaveHeader, 0, nil);
            Success(MMSYSERR_NOMEM); // Raises an OnError event
          end;
        end;
      end
      else if pWaveHeader^.dwUser = DWORD(Self) then
        ReallocMem(pWaveHeader^.lpData, 0);
      if Result then
      begin
        FillChar(pWaveHeader^, SizeOf(TWaveHdr), 0);
        if InternalBuffer then
          pWaveHeader.dwUser := DWORD(Self);
        pWaveHeader^.lpData := Buffer;
        pWaveHeader^.dwBufferLength := BufferSize;
      end;
    end;
  end;
end;

procedure TWaveAudioIO.ResetBuffers;
var
  I: Integer;
  pWaveHeader: PWaveHdr;
begin
  for I := Buffers.Count - 1 downto 0 do
  begin
    pWaveHeader := Buffers[I];
    Buffers.Delete(I);
    ReallocateBuffer(pWaveHeader, 0, nil);
  end;
end;

procedure TWaveAudioIO.WaitForStart;
var
  MSG: TMSG;
begin
  if Callback <> 0 then
  begin
    if ThreadHandle = 0 then
    begin
      while Opening and (Callback <> 0) do
        if PeekMessage(MSG, Callback, 0, 0, PM_REMOVE) then
        begin
          TranslateMessage(MSG);
          DispatchMessage(MSG);
          if MSG.message = WM_QUIT then Exit;
        end;
    end
    else
    begin
      while Opening do
        WaitForSyncObject(ThreadEvent, INFINITE);
    end;
  end;
end;

procedure TWaveAudioIO.WaitForStop;
var
  MSG: TMSG;
begin
  if Callback <> 0 then
  begin
    if ThreadHandle = 0 then
    begin
      while Callback <> 0 do
        if PeekMessage(MSG, Callback, 0, 0, PM_REMOVE) then
        begin
          TranslateMessage(MSG);
          DispatchMessage(MSG);
          if MSG.message = WM_QUIT then Exit;
        end;
    end
    else
    begin
      while Closing do
        WaitForSyncObject(ThreadHandle, INFINITE);
    end;
  end;
end;

function TWaveAudioIO.QueryPCM(PCMFormat: TPCMFormat): Boolean;
var
  WaveFormat: TWaveFormatEx;
begin
  SetPCMAudioFormatS(@WaveFormat, PCMFormat);
  Result := Query(@WaveFormat);
end;

procedure TWaveAudioIO.DoWaveInDeviceOpen;
begin
  DoDeviceOpen;
end;

procedure TWaveAudioIO.DoWaveInDeviceClose;
begin
  DoDeviceClose;
end;

procedure TWaveAudioIO.DoWaveInDeviceData(pWaveHeader: PWaveHdr);
begin
  // The WaveIn class override this
end;

procedure TWaveAudioIO.DoWaveOutDeviceOpen;
begin
  DoDeviceOpen;
end;

procedure TWaveAudioIO.DoWaveOutDeviceClose;
begin
  DoDeviceClose;
end;

procedure TWaveAudioIO.DoWaveOutDeviceDone(pWaveHeader: PWaveHdr);
begin
  // The WaveOut class override this
end;

end.
