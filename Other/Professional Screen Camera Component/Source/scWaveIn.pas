{------------------------------------------------------------------------------}
{                                                                              }
{  WaveIn - Abstract definition of wave audio input                            }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

{$I DELPHIAREA.INC}

unit scWaveIn;

interface

uses
  Windows, Messages, Classes, MMSystem, scWaveUtils, scWaveIO;

type

  // The base abstract class for wave audio recorder components
  TWaveAudioIn = class(TWaveAudioIO)
  private
    fHandle: HWAVEIN;
    fPaused: Boolean;
    fPCMFormat: TPCMFormat;
    fOnFormat: TWaveAudioGetFormatEvent;
    procedure SetPCMFormat(Value: TPCMFormat);
  protected
    procedure DoWaveInDeviceOpen; override;
    procedure DoWaveInDeviceClose; override;
    procedure DoWaveInDeviceData(pWaveHeader: PWaveHdr); override;
    function GetNumDevs: DWORD; override;
    function GetPaused: Boolean; override;
    function GetDeviceName: String; override;
    function GetDeviceFormats: TWaveDeviceFormats; override;
    function GetPosition: DWORD; override;
    function GetErrorText(ErrorCode: MMRESULT): String; override;
    function ValidateDeviceID(ADeviceID: DWORD): MMRESULT; override;
    procedure DefineBuffers; override;
    function InternalOpen: Boolean; override;
    function InternalClose: Boolean; override;
    function InternalPause: Boolean; override;
    function InternalResume: Boolean; override;
    function HandleAllocated: Boolean; override;
    function AddWaveHeader(const pWaveHeader: PWaveHdr): Boolean; virtual;
    function AddBuffer(const Buffer: Pointer; BufferSize: DWORD;
      FreeIt: Boolean): Boolean; virtual;
    procedure GetWaveFormat(var pWaveFormat: PWaveFormatEx;
      var FreeIt: Boolean); override;
    procedure WaveDataReady(const Buffer: Pointer; BufferSize: DWORD;
      var FreeIt: Boolean); virtual;
  protected
    property OnFormat: TWaveAudioGetFormatEvent read fOnFormat write fOnFormat;
    property PCMFormat: TPCMFormat read fPCMFormat write SetPCMFormat default Mono16Bit8000Hz;
  public
    constructor Create(AOwner: TComponent); override;
    function Query(const pWaveFormat: PWaveFormatEx): Boolean; override;
    property Handle: HWAVEIN read fHandle;
  end;

implementation

uses
  SysUtils;

{ TWaveAudioIn }

constructor TWaveAudioIn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fPCMFormat := Mono16bit8000Hz;
end;

function TWaveAudioIn.GetPaused: Boolean;
begin
  Result := fPaused;
end;

function TWaveAudioIn.GetNumDevs: DWORD;
begin
  Result := WaveInGetNumDevs;
end;

function TWaveAudioIn.GetDeviceName: String;
var
  DevCaps: TWaveInCaps;
begin
  if WaveInGetDevCaps(DeviceID, @DevCaps, SizeOf(DevCaps)) = MMSYSERR_NOERROR then
    Result := StrPas(DevCaps.szPname)
  else
    Result := '';
end;

function TWaveAudioIn.GetDeviceFormats: TWaveDeviceFormats;
var
  DevCaps: TWaveInCaps;
begin
  Result := [];
  if WaveInGetDevCaps(DeviceID, @DevCaps, SizeOf(DevCaps)) = MMSYSERR_NOERROR then
  begin
    Include(Result, Mono8bit8000Hz);
    Include(Result, Stereo8bit8000Hz);
    Include(Result, Mono16bit8000Hz);
    Include(Result, Stereo16bit8000Hz);
    if LongBool(DevCaps.dwFormats and WAVE_FORMAT_1M08) then
      Include(Result, Mono8bit11025Hz);
    if LongBool(DevCaps.dwFormats and WAVE_FORMAT_1S08) then
      Include(Result, Stereo8bit11025Hz);
    if LongBool(DevCaps.dwFormats and WAVE_FORMAT_1M16) then
      Include(Result, Mono16bit11025Hz);
    if LongBool(DevCaps.dwFormats and WAVE_FORMAT_1S16) then
      Include(Result, Stereo16bit11025Hz);
    if LongBool(DevCaps.dwFormats and WAVE_FORMAT_2M08) then
      Include(Result, Mono8bit22050Hz);
    if LongBool(DevCaps.dwFormats and WAVE_FORMAT_2S08) then
      Include(Result, Stereo8bit22050Hz);
    if LongBool(DevCaps.dwFormats and WAVE_FORMAT_2M16) then
      Include(Result, Mono16bit22050Hz);
    if LongBool(DevCaps.dwFormats and WAVE_FORMAT_2S16) then
      Include(Result, Stereo16bit22050Hz);
    if LongBool(DevCaps.dwFormats and WAVE_FORMAT_4M08) then
      Include(Result, Mono8bit44100Hz);
    if LongBool(DevCaps.dwFormats and WAVE_FORMAT_4S08) then
      Include(Result, Stereo8bit44100Hz);
    if LongBool(DevCaps.dwFormats and WAVE_FORMAT_4M16) then
      Include(Result, Mono16bit44100Hz);
    if LongBool(DevCaps.dwFormats and WAVE_FORMAT_4S16) then
      Include(Result, Stereo16bit44100Hz);
  end;
end;

function TWaveAudioIn.GetPosition: DWORD;
var
  mmTime: TMMTime;
begin
  Result := 0;
  mmTime.wType := TIME_MS;
  if WaveInGetPosition(Handle, @mmTime, SizeOf(mmTime)) = MMSYSERR_NOERROR then
    Result := mmTimeToMS(mmTime);
end;

function TWaveAudioIn.GetErrorText(ErrorCode: MMRESULT): String;
var
  ErrorText: array[0..255] of Char;
begin
  if WaveInGetErrorText(ErrorCode, ErrorText, SizeOf(ErrorText)) = MMSYSERR_NOERROR then
    Result := StrPas(ErrorText)
  else
    Result := '';
end;

procedure TWaveAudioIn.SetPCMFormat(Value: TPCMFormat);
begin
  if PCMFormat <> Value then
  begin
    if HandleAllocated then
      raise EWaveAudioInvalidOperation.Create('Audio format cannot be changed while device is open')
    else
      fPCMFormat := Value;
  end;
end;

function TWaveAudioIn.ValidateDeviceID(ADeviceID: DWORD): MMRESULT;
var
  DevCaps: TWaveInCaps;
begin
  Result := WaveInGetDevCaps(ADeviceID, @DevCaps, SizeOf(DevCaps));
end;

function TWaveAudioIn.InternalOpen: Boolean;
var
  pWaveFormat: PWaveFormatEx;
  FreeWaveFormat: Boolean;
begin
  Result := False;
  if not Opening then
  begin
    if Closing then
      WaitForStop;
    if not Active then
    begin
      Lock;
      Opening := True;
      try
        FreeWaveFormat := True;
        GetWaveFormat(pWaveFormat, FreeWaveFormat);
        try
          if Success(WaveInOpen(nil, DeviceID, pWaveFormat, 0, 0, WAVE_FORMAT_QUERY)) then
          begin
            Move(pWaveFormat^, WaveFormat, SizeOf(WaveFormat) - SizeOf(WaveFormat.cbSize));
            CreateCallback;
            try
              if Success(WaveInOpen(@fHandle, DeviceID, pWaveFormat, Callback, 0, CallbackType)) then
                Result := True
              else
                DestroyCallback;
            except
              DestroyCallback;
            end;
          end;
        finally
          if FreeWaveFormat then
            FreeMem(pWaveFormat);
        end;
      finally
        Opening := False;
        Unlock;
      end;
    end
    else
      raise EWaveAudioInvalidOperation.Create('Device is aleardy open');
  end;
end;

function TWaveAudioIn.InternalClose: Boolean;
begin
  Result := False;
  if not Closing then
  begin
    if Opening then
      WaitForStart;
    if Active then
    begin
      Lock;
      try
        Closing := True;
        try
          if Success(WaveInReset(Handle)) then
            if ActiveBufferCount = 0 then
              Result := Success(WaveInClose(Handle))
            else
              Result := True
          else
            Closing := False;
        except
          Closing := False;
        end;
      finally
        Unlock;
      end;
    end
    else
      raise EWaveAudioInvalidOperation.Create('Device is aleardy close');
  end;
end;

function TWaveAudioIn.InternalPause: Boolean;
begin
  Result := False;
  if not Paused then
  begin
    Lock;
    try
      if not HandleAllocated or Success(WaveInStop(Handle)) then
      begin
        fPaused := True;
        DoPause;
        Result := True;
      end;
    finally
      Unlock;
    end;
  end;
end;

function TWaveAudioIn.InternalResume: Boolean;
begin
  Result := False;
  if Paused then
  begin
    Lock;
    try
      if not HandleAllocated or Success(WaveInStart(Handle)) then
      begin
        fPaused := False;
        DoResume;
        Result := True;
      end;
    finally
      Unlock;
    end;
  end;
end;

function TWaveAudioIn.HandleAllocated: Boolean;
begin
  Result := (Handle <> 0);
end;

function TWaveAudioIn.AddWaveHeader(const pWaveHeader: PWaveHdr): Boolean;
var
  AlreadyPrepared: Boolean;
begin
  Result := False;
  AlreadyPrepared := LongBool(pWaveHeader^.dwFlags and WHDR_PREPARED);
  if AlreadyPrepared or
     Success(waveInPrepareHeader(Handle, pWaveHeader, SizeOf(TWaveHdr)))
  then
    try
      if Success(waveInAddBuffer(Handle, pWaveHeader, SizeOf(TWaveHdr))) then
        Result := True
      else if not AlreadyPrepared then
        waveInUnprepareHeader(Handle, pWaveHeader, SizeOf(TWaveHdr));
    except
      if not AlreadyPrepared then
        waveInUnprepareHeader(Handle, pWaveHeader, SizeOf(TWaveHdr));
      raise;
    end;
end;

function TWaveAudioIn.AddBuffer(const Buffer: Pointer; BufferSize: DWORD;
  FreeIt: Boolean): Boolean;
var
  pWaveHeader: PWaveHdr;
begin
  Result := False;
  pWaveHeader := nil;
  if ReallocateBuffer(pWaveHeader, BufferSize, Buffer) then
  begin
    if FreeIt then
      pWaveHeader^.dwUser := DWORD(Self);
    try
      if AddWaveHeader(pWaveHeader) then
        Result := True
      else
        ReallocateBuffer(pWaveHeader, 0, nil);
    except
      ReallocateBuffer(pWaveHeader, 0, nil);
    end;
  end;
end;

procedure TWaveAudioIn.GetWaveFormat(var pWaveFormat: PWaveFormatEx;
  var FreeIt: Boolean);
begin
  if PCMFormat <> nonePCM then
  begin
    FreeIt := True;
    GetMem(pWaveFormat, SizeOf(TWaveFormatEx));
    SetPCMAudioFormatS(pWaveFormat, PCMFormat)
  end
  else if Assigned(fOnFormat) then
    fOnFormat(Self, pWaveFormat, FreeIt);
end;

procedure TWaveAudioIn.WaveDataReady(const Buffer: Pointer; BufferSize: DWORD;
  var FreeIt: Boolean);
begin
  // Nothing to do
end;

function TWaveAudioIn.Query(const pWaveFormat: PWaveFormatEx): Boolean;
begin
  Result := (WaveInOpen(nil, DeviceID, pWaveFormat, 0, 0,
    WAVE_FORMAT_QUERY) = MMSYSERR_NOERROR);
end;

procedure TWaveAudioIn.DefineBuffers;
var
  Count: Integer;
  pWaveHeader: PWaveHdr;
begin
  pWaveHeader := nil;
  try
    Count := ActiveBufferCount;
    while (Count < BufferCount) and HandleAllocated and not Closing and
           ReallocateBuffer(pWaveHeader, PreferredBufferSize, nil) do
    begin
      if not AddWaveHeader(pWaveHeader) then
      begin
        ReallocateBuffer(pWaveHeader, 0, nil);
        Break;
      end
      else
      begin
        pWaveHeader := nil;
        Inc(Count);
      end;
    end;
  except
    ReallocateBuffer(pWaveHeader, 0, nil);
  end;
end;

procedure TWaveAudioIn.DoWaveInDeviceOpen;
begin
  inherited DoWaveInDeviceOpen;
  fPaused := Paused or not Success(WaveInStart(Handle));
end;

procedure TWaveAudioIn.DoWaveInDeviceClose;
begin
  fHandle := 0;
  inherited DoWaveInDeviceClose;
end;

procedure TWaveAudioIn.DoWaveInDeviceData(pWaveHeader: PWaveHdr);
var
  FreeBuffer: Boolean;
begin
  try
    try
      if Assigned(pWaveHeader) and
         Success(waveInUnprepareHeader(Handle, pWaveHeader, SizeOf(TWaveHdr))) and
         (pWaveHeader^.dwBytesRecorded > 0) then
      begin
        FreeBuffer := True;
        DoLevel(pWaveHeader^.lpData, pWaveHeader^.dwBytesRecorded);
        DoFilter(pWaveHeader^.lpData, pWaveHeader^.dwBytesRecorded);
        WaveDataReady(pWaveHeader^.lpData, pWaveHeader^.dwBytesRecorded, FreeBuffer);
        if not FreeBuffer then pWaveHeader^.dwUser := 0;
      end;
      if Closing or (ActiveBufferCount > BufferCount) or
         not ReallocateBuffer(pWaveHeader, PreferredBufferSize, nil) or
         not AddWaveHeader(pWaveHeader)
      then
        ReallocateBuffer(pWaveHeader, 0, nil);
    except
      if Assigned(pWaveHeader) then
      begin
        if LongBool(pWaveHeader^.dwFlags and WHDR_PREPARED) then
          waveInUnprepareHeader(Handle, pWaveHeader, SizeOf(TWaveHdr));
        ReallocateBuffer(pWaveHeader, 0, nil);
      end;
      raise;
    end;
  finally
    if Closing and (ActiveBufferCount = 0) then
      Success(WaveInClose(Handle));
  end;
end;

end.
