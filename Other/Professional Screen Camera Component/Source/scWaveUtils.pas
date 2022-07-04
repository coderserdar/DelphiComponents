{------------------------------------------------------------------------------}
{                                                                              }
{  WaveUtils - Utility functions and data types                                }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

{$I DELPHIAREA.INC}

unit scWaveUtils;

interface

uses
  Windows, Messages, Classes, SysUtils, MMSystem;

type

  // Milliseconds to string format specifiers
  TMS2StrFormat = (
    msHMSh, // Hour:Minute:Second.Hunderdth
    msHMS,  // Hour:Minute:Second
    msMSh,  // Minute:Second.Hunderdth
    msMS,   // Minute:Second
    msSh,   // Second.Hunderdth
    msS,    // Second
    msAh,   // Best format with hunderdth of second
    msA);   // Best format without hunderdth of second

  // Standard PCM Audio Format
  TPCMChannel = (cMono, cStereo);
  TPCMSamplesPerSec = (ss8000Hz, ss11025Hz, ss22050Hz, ss44100Hz, ss48000Hz);
  TPCMBitsPerSample = (bs8Bit, bs16Bit);
  TPCMFormat = (nonePCM, Mono8Bit8000Hz, Stereo8bit8000Hz, Mono16bit8000Hz,
    Stereo16bit8000Hz, Mono8bit11025Hz, Stereo8bit11025Hz, Mono16bit11025Hz,
    Stereo16bit11025Hz, Mono8bit22050Hz, Stereo8bit22050Hz, Mono16bit22050Hz,
    Stereo16bit22050Hz, Mono8bit44100Hz, Stereo8bit44100Hz, Mono16bit44100Hz,
    Stereo16bit44100Hz, Mono8bit48000Hz, Stereo8bit48000Hz, Mono16bit48000Hz,
    Stereo16bit48000Hz);

  // Wave Device Supported PCM Formats
  TWaveDeviceFormats = set of TPCMFormat;

  // Wave Out Device Supported Features
  TWaveOutDeviceSupport = (dsVolume, dsStereoVolume, dsPitch, dsPlaybackRate,
    dsPosition, dsAsynchronize, dsDirectSound);
  TWaveOutDeviceSupports = set of TWaveOutDeviceSupport;

  // Wave Out Options
  TWaveOutOption = (woSetVolume, woSetPitch, woSetPlaybackRate);
  TWaveOutOptions = set of TWaveOutOption;

  {$IFNDEF COMPILER4_UP}
  // Ownership flag of a stream
  TStreamOwnership = (soReference, soOwned);
  {$ENDIF}

  // State of wave stream adapter
  TWaveStreamState = (wssReady, wssReading, wssWriting, wssWritingEx);

  // Raw wave audio record
  PRawWave = ^TRawWave;
  TRawWave = record
    pData: Pointer;
    dwSize: DWORD;
  end;

  // Wave Audio Exceptions
  EWaveAudioError = class(Exception);
  EWaveAudioSysError = class(EWaveAudioError);
  EWaveAudioInvalidOperation = class(EWaveAudioError);

  // Wave Audio Events
  TWaveAudioEvent = procedure(Sender: TObject) of object;
  TWaveAudioGetFormatEvent = procedure(Sender: TObject;
    var pWaveFormat: PWaveFormatEx; var FreeIt: Boolean) of object;
  TWaveAudioGetDataEvent = function(Sender: TObject; const Buffer: Pointer;
    BufferSize: DWORD; var NumLoops: DWORD): DWORD of object;
  TWaveAudioGetDataPtrEvent = function(Sender: TObject; var Buffer: Pointer;
    var NumLoops: DWORD; var FreeIt: Boolean): DWORD of object;
  TWaveAudioDataReadyEvent = procedure(Sender: TObject; const Buffer: Pointer;
    BufferSize: DWORD; var FreeIt: Boolean) of object;
  TWaveAudioLevelEvent = procedure(Sender: TObject; Level: Integer) of object;
  TWaveAudioFilterEvent = procedure(Sender: TObject; const Buffer: Pointer;
    BufferSize: DWORD) of object;

// Retrieves format, size, and offset of the wave audio for an open mmIO
// handle. On success when the the function returns true, it is the caller
// responsibility to free the memory allocated for the Wave Format structure.
function GetWaveAudioInfo(mmIO: HMMIO; var pWaveFormat: PWaveFormatEx;
  var DataSize, DataOffset: DWORD): Boolean;

// Initializes a new wave RIFF format in an open mmIO handle. The previous
// content of mmIO will be lost.
function CreateWaveAudio(mmIO: HMMIO; const pWaveFormat: PWaveFormatEx;
  var ckRIFF, ckData: TMMCKInfo): Boolean;

// Updates the chunks and closes an mmIO handle.
procedure CloseWaveAudio(mmIO: HMMIO; var ckRIFF, ckData: TMMCKInfo);

// Retrieves format, size, and offset of the wave audio for a stream. On
// success when the the function returns true, it is the caller responsibility
// to free the memory allocated for the Wave Format structure.
function GetStreamWaveAudioInfo(Stream: TStream; var pWaveFormat: PWaveFormatEx;
  var DataSize, DataOffset: DWORD): Boolean;

// Initializes wave RIFF format in a stream and returns the mmIO handle.
// After calling this function, the previous content of the stream will be lost.
function CreateStreamWaveAudio(Stream: TStream; const pWaveFormat: PWaveFormatEx;
  var ckRIFF, ckData: TMMCKInfo): HMMIO;

// Opens wave RIFF format in a stream for read and write operations and returns
// the mmIO handle.
function OpenStreamWaveAudio(Stream: TStream): HMMIO;

// Claculates the wave buffer size for the specified duration.
function CalcWaveBufferSize(const pWaveFormat: PWaveFormatEx; Duration: DWORD): DWORD;

// Returns the string representation of an audio format tag.
function GetAudioFormat(FormatTag: Word): String;

// Returns the wave's length in milliseconds.
// Returns the string representation of a wave audio format.
function GetWaveAudioFormat(const pWaveFormat: PWaveFormatEx): String;

// Returns the wave's length in milliseconds.
function GetWaveAudioLength(const pWaveFormat: PWaveFormatEx; DataSize: DWORD): DWORD;

// Returns the wave's bit rate in kbps (kilo bits per second).
function GetWaveAudioBitRate(const pWaveFormat: PWaveFormatEx): DWORD;

// Returns the wave data volume peak level in percent (PCM format only).
function GetWaveAudioPeakLevel(const Data: Pointer; DataSize: DWORD;
  const pWaveFormat: PWaveFormatEx): Integer;

// Inverts the wave data (PCM format only).
function InvertWaveAudio(const Data: Pointer; DataSize: DWORD;
  const pWaveFormat: PWaveFormatEx): Boolean;

// Fills the wave data with silence
function SilenceWaveAudio(const Data: Pointer; DataSize: DWORD;
  const pWaveFormat: PWaveFormatEx): Boolean;

// Increases/Decreases the wave data volume by the specified percentage (PCM format only).
function ChangeWaveAudioVolume(const Data: Pointer; DataSize: DWORD;
  const pWaveFormat: PWaveFormatEx; Percent: Integer): Boolean;

// Mixes several waves with the same format (PCM format only).
function MixWaveAudio(const RawWaves: PRawWave; Count: Integer;
  const pWaveFormat: PWaveFormatEx; Buffer: Pointer; BufferSize: DWORD): Boolean;

// Converts the wave data to the specified format (PCM format only). The caller is
// responsible to release the memory allocated for the converted wave data.
function ConvertWaveFormat(const srcFormat: PWaveFormatEx; srcData: Pointer; srcDataSize: DWORD;
  const dstFormat: PWaveFormatEx; var dstData: Pointer; var dstDataSize: DWORD): Boolean;

// Initializes a standard PCM wave format header. The size of memory referenced
// by the pWaveFormat parameter must not be less than the size of TWaveFormatEx
// record.
procedure SetPCMAudioFormat(const pWaveFormat: PWaveFormatEx; Channels: TPCMChannel;
  SamplesPerSec: TPCMSamplesPerSec; BitsPerSample: TPCMBitsPerSample);

// Initializes a standard PCM wave format header (shorcut form). The size of
// memory referenced by the pWaveFormat parameter must not be less than the
// size of TWaveFormatEx record.
procedure SetPCMAudioFormatS(const pWaveFormat: PWaveFormatEx; PCMFormat: TPCMFormat);

// Returns the standard PCM format specifier of a wave format.
function GetPCMAudioFormat(const pWaveFormat: PWaveFormatEx): TPCMFormat;

// Returns the byte offset (zero base) of the wave audio specified by the pWaveFormat
// parameter at the position specified by the Position parameter in milliseconds.
function GetWaveDataPositionOffset(const pWaveFormat: PWaveFormatEx; Position: DWORD): DWORD;

// Converts milliseconds to string
function MS2Str(Milliseconds: DWORD; Fmt: TMS2StrFormat): String;

// Waits for the scnchronize object while lets the caller thread processes
// its messages.
function WaitForSyncObject(SyncObject: THandle; Timeout: DWORD): DWORD;

// User defined mmIOProc to handle Delphi streams by Windows' mmIO functions
function mmioStreamProc(lpmmIOInfo: PMMIOInfo; uMsg, lParam1, lParam2: DWORD): LRESULT; stdcall;

implementation

uses
  scWaveACM;

{ Global Procedures }

// To open a stream using mmIO API functions, use the following code sample:
//
//    FillChar(mmioInfo, SizeOf(mmioInfo), 0);
//    mmioInfo.pIOProc := @mmioStreamProc;
//    mmioInfo.adwInfo[0] := DWORD(your_stream_instance);
//    mmIO := mmioOpen(nil, @mmioInfo, dwOpenFlags);
//
// The flags specified by the dwOpenFlags parameter of mmioOpen function can
// be only one of MMIO_READ, MMIO_WRITE, and MMIO_READWRITE flags. If you use
// another flags, simply they will be ignored by this user defined function.

function mmIOStreamProc(lpmmIOInfo: PMMIOInfo; uMsg, lParam1, lParam2: DWORD): LRESULT; stdcall;
var
  Stream: TStream;
begin
  if Assigned(lpmmIOInfo) and (lpmmIOInfo^.adwInfo[0] <> 0) then
  begin
    Stream := TStream(lpmmIOInfo^.adwInfo[0]);
    case uMsg of
      MMIOM_OPEN:
      begin
        if TObject(lpmmIOInfo^.adwInfo[0]) is TStream then
        begin
          Stream.Seek(0, SEEK_SET);
          lpmmIOInfo^.lDiskOffset := 0;
          Result := MMSYSERR_NOERROR;
        end
        else
          Result := -1;
      end;
      MMIOM_CLOSE:
        Result := MMSYSERR_NOERROR;
      MMIOM_SEEK:
        try
          if lParam2 = SEEK_CUR then
            Stream.Seek(lpmmIOInfo^.lDiskOffset, SEEK_SET);
          Result := Stream.Seek(lParam1, lParam2);
          lpmmIOInfo^.lDiskOffset := Result;
        except
          Result := -1;
        end;
      MMIOM_READ:
        try
          Stream.Seek(lpmmIOInfo^.lDiskOffset, SEEK_SET);
          Result := Stream.Read(Pointer(lParam1)^, lParam2);
          lpmmIOInfo^.lDiskOffset := Stream.Seek(0, SEEK_CUR);
        except
          Result := -1;
        end;
      MMIOM_WRITE,
      MMIOM_WRITEFLUSH:
        try
          Stream.Seek(lpmmIOInfo^.lDiskOffset, SEEK_SET);
          Result := Stream.Write(Pointer(lParam1)^, lParam2);
          lpmmIOInfo^.lDiskOffset := Stream.Seek(0, SEEK_CUR);
        except
          Result := -1;
        end
    else
      Result := MMSYSERR_NOERROR;
    end;
  end
  else
    Result := -1;
end;

// Retrieves format, size, and offset of the wave audio for an open mmIO
// handle. On success when the the function returns true, it is the caller
// responsibility to free the memory allocated for the Wave Format structure.
function GetWaveAudioInfo(mmIO: HMMIO; var pWaveFormat: PWaveFormatEx;
  var DataSize, DataOffset: DWORD): Boolean;

  function GetWaveFormat(const ckRIFF: TMMCKInfo): Boolean;
  var
    ckFormat: TMMCKInfo;
  begin
    Result := False;
    ckFormat.ckid := mmioStringToFOURCC('fmt', 0);
    if (mmioDescend(mmIO, @ckFormat, @ckRIFF, MMIO_FINDCHUNK) = MMSYSERR_NOERROR) and
       (ckFormat.cksize >= SizeOf(TWaveFormat)) then
    begin
      if ckFormat.cksize < SizeOf(TWaveFormatEx) then
      begin
        GetMem(pWaveFormat, SizeOf(TWaveFormatEx));
        FillChar(pWaveFormat^, SizeOf(TWaveFormatEx), 0);
      end
      else
        GetMem(pWaveFormat, ckFormat.cksize);
      Result := (mmioRead(mmIO, PAnsiChar(pWaveFormat), ckFormat.cksize) = Integer(ckFormat.cksize));
    end;
  end;

  function GetWaveData(const ckRIFF: TMMCKInfo): Boolean;
  var
    ckData: TMMCKInfo;
  begin
    Result := False;
    ckData.ckid := mmioStringToFOURCC('data', 0);
    if (mmioDescend(mmIO, @ckData, @ckRIFF, MMIO_FINDCHUNK) = MMSYSERR_NOERROR) then
    begin
      DataSize := ckData.cksize;
      DataOffset := ckData.dwDataOffset;
      Result := True;
    end;
  end;

var
  ckRIFF: TMMCKInfo;
  OrgPos: Integer;
begin
  Result := False;
  OrgPos := mmioSeek(mmIO, 0, SEEK_CUR);
  try
    mmioSeek(mmIO, 0, SEEK_SET);
    ckRIFF.fccType := mmioStringToFOURCC('WAVE', 0);
    if (mmioDescend(mmIO, @ckRIFF, nil, MMIO_FINDRIFF) = MMSYSERR_NOERROR) then
    begin
      pWaveFormat := nil;
      if GetWaveFormat(ckRIFF) and GetWaveData(ckRIFF) then
        Result := True
      else if Assigned(pWaveFormat) then
        ReallocMem(pWaveFormat, 0);
    end
  finally
    mmioSeek(mmIO, OrgPos, SEEK_SET);
  end;
end;

// Initializes a new wave RIFF format in an open mmIO handle. The previous
// content of mmIO will be lost.
function CreateWaveAudio(mmIO: HMMIO; const pWaveFormat: PWaveFormatEx;
  var ckRIFF, ckData: TMMCKInfo): Boolean;
var
  ckFormat: TMMCKInfo;
  FormatSize: Integer;
begin
  Result := False;
  FormatSize := SizeOf(TWaveFormatEx) + pWaveFormat^.cbSize;
  mmIOSeek(mmIO, 0, SEEK_SET);
  FillChar(ckRIFF, SizeOf(TMMCKInfo), 0);
  ckRIFF.fccType := mmioStringToFOURCC('WAVE', 0);
  if mmioCreateChunk(mmIO, @ckRIFF, MMIO_CREATERIFF) = MMSYSERR_NOERROR then
  begin
    FillChar(ckFormat, SizeOf(TMMCKInfo), 0);
    ckFormat.ckid := mmioStringToFOURCC('fmt', 0);
    if (mmioCreateChunk(mmIO, @ckFormat, 0) = MMSYSERR_NOERROR) and
       (mmioWrite(mmIO, PAnsiChar(pWaveFormat), FormatSize) = FormatSize) and
       (mmioAscend(mmIO, @ckFormat, 0) = MMSYSERR_NOERROR) then
    begin
      FillChar(ckData, SizeOf(TMMCKInfo), 0);
      ckData.ckid := mmioStringToFOURCC('data', 0);
      Result := (mmioCreateChunk(mmIO, @ckData, 0) = MMSYSERR_NOERROR);
    end;
  end;
end;

// Updates the chunks and closes an mmIO handle.
procedure CloseWaveAudio(mmIO: HMMIO; var ckRIFF, ckData: TMMCKInfo);
begin
  mmioAscend(mmIO, @ckData, 0);
  mmioAscend(mmIO, @ckRIFF, 0);
  mmioClose(mmIO, 0);
end;

// Retrieves format, size, and offset of the wave audio for a stream. On
// success when the the function returns true, it is the caller responsibility
// to free the memory allocated for the Wave Format structure.
function GetStreamWaveAudioInfo(Stream: TStream; var pWaveFormat: PWaveFormatEx;
  var DataSize, DataOffset: DWORD): Boolean;
var
  mmIO: HMMIO;
begin
  Result := False;
  if Stream.Size <> 0 then
  begin
    mmIO := OpenStreamWaveAudio(Stream);
    if mmIO <> 0 then
      try
        Result := GetWaveAudioInfo(mmIO, pWaveFormat, DataSize, DataOffset);
      finally
        mmioClose(mmIO, MMIO_FHOPEN);
      end;
  end;
end;

// Initializes wave RIFF format in a stream and returns the mmIO handle.
// After calling this function, the previous content of the stream will be lost.
function CreateStreamWaveAudio(Stream: TStream; const pWaveFormat: PWaveFormatEx;
 var ckRIFF, ckData: TMMCKInfo): HMMIO;
begin
  Result := OpenStreamWaveAudio(Stream);
  if Result <> 0 then
  begin
    Stream.Size := 0;
    if not CreateWaveAudio(Result, pWaveFormat, ckRIFF, ckData) then
    begin
      mmioClose(Result, MMIO_FHOPEN);
      Result := 0;
    end;
  end;
end;

// Opens wave RIFF format in a stream for read and write operations and returns
// the mmIO handle.
function OpenStreamWaveAudio(Stream: TStream): HMMIO;
var
  mmIOInfo: TMMIOINFO;
begin
  FillChar(mmIOInfo, SizeOf(mmIOInfo), 0);
  mmIOInfo.pIOProc := @mmIOStreamProc;
  mmIOInfo.adwInfo[0] := DWORD(Stream);
  Result := mmioOpen(nil, @mmIOInfo, MMIO_READ or MMIO_WRITE);
end;

// Claculates the wave buffer size for the specified duration.
function CalcWaveBufferSize(const pWaveFormat: PWaveFormatEx; Duration: DWORD): DWORD;
var
  Alignment: DWORD;
begin
  Result := MulDiv(Duration, pWaveFormat^.nAvgBytesPerSec, 1000);
  if pWaveFormat^.nBlockAlign > 1 then
  begin
    Alignment := Result mod pWaveFormat^.nBlockAlign;
    if Alignment <> 0 then
      Inc(Result, pWaveFormat^.nBlockAlign - Alignment);
  end;
end;

// Returns the string representation of an audio format tag.
function GetAudioFormat(FormatTag: Word): String;
begin
  case FormatTag of
    WAVE_FORMAT_PCM: Result := 'PCM';
    $0002: Result := 'Microsoft ADPCM';
    $0003: Result := 'PCM IEEE';
    $0004: Result := 'VSELP';
    $0005: Result := 'CSVD';
    $0006: Result := 'A-Law';
    $0007: Result := 'u-Law';
    $0008: Result := 'DTS';
    $0011: Result := 'IMA ADPCM';
    $0031: Result := 'GSM 6.10';
    $0043: Result := 'Microsoft G.723.1';
    $0050: Result := 'MPEG';
    $0055: Result := 'MPEG Layer-3';
  else
    Result := '';
  end;
end;

// Returns the string representation of a wave audio format.
function GetWaveAudioFormat(const pWaveFormat: PWaveFormatEx): String;
const
  Channels: array[1..2] of String = ('Mono', 'Stereo');
var
  Fmt: String;
begin
  with pWaveFormat^ do
  begin
    if wBitsPerSample <> 0 then
      if nChannels in [1..2] then
        Result := Format('%.3f kHz, %d Bit, %s', [nSamplesPerSec / 1000,
          wBitsPerSample, Channels[nChannels]])
      else
        Result := Format('%.3f kHz, %d Bit, %d Ch', [nSamplesPerSec / 1000,
          wBitsPerSample, nChannels])
    else
      if nChannels in [1..2] then
        Result := Format('%.3f kHz, %s', [nSamplesPerSec / 1000,
          Channels[nChannels]])
      else
        Result := Format('%.3f kHz, %d Ch', [nSamplesPerSec / 1000,
          nChannels]);
    Fmt := GetAudioFormat(wFormatTag);
    if Fmt <> '' then
      Result := Fmt + ': ' + Result;
  end;
end;

// Returns the wave's length in milliseconds.
function GetWaveAudioLength(const pWaveFormat: PWaveFormatEx; DataSize: DWORD): DWORD;
begin
  with pWaveFormat^ do
    if nAvgBytesPerSec <> 0 then
      Result := MulDiv(1000, DataSize, nAvgBytesPerSec)
    else
      Result := 0;
end;

// Returns the wave's bit rate in kbps (kilo bits per second).
function GetWaveAudioBitRate(const pWaveFormat: PWaveFormatEx): DWORD;
begin
  with pWaveFormat^ do
    Result := MulDiv(nSamplesPerSec, nChannels * wBitsPerSample, 1000)
end;

// Returns the wave data peak level in percent (PCM format only).
function GetWaveAudioPeakLevel(const Data: Pointer; DataSize: DWORD;
  const pWaveFormat: PWaveFormatEx): Integer;

  function GetAudioPeakLevel8Bit: Integer;
  var
    pSample: PByte;
    Max: Byte;
  begin
    Max := 0;
    pSample := Data;
    while DataSize > 0 do
    begin
      if pSample^ > Max then
        Max := pSample^;
      Inc(pSample);
      Dec(DataSize);
    end;
    if ByteBool(Max and $80) then
      Max := Max and $7F
    else
      Max := 0;
    Result := (100 * Max) div $7F;
  end;

  function GetAudioPeakLevel16Bit: Integer;
  var
    pSample: PSmallInt;
    Max: SmallInt;
  begin
    Max := 0;
    pSample := Data;
    while DataSize > 0 do
    begin
      if pSample^ > Max then
        Max := pSample^;
      Inc(pSample);
      Dec(DataSize, 2);
    end;
    Result := (100 * Max) div $7FFF;
  end;

begin
  Result := -1;
  if pWaveFormat^.wFormatTag = WAVE_FORMAT_PCM then
    case pWaveFormat^.wBitsPerSample of
      8: Result := GetAudioPeakLevel8Bit;
      16: Result := GetAudioPeakLevel16Bit;
    end;
end;

// Inverts the wave data (PCM format only).
function InvertWaveAudio(const Data: Pointer; DataSize: DWORD;
  const pWaveFormat: PWaveFormatEx): Boolean;

  function Invert8Bit: Boolean;
  var
    pStart, pEnd: PByte;
  begin
    pStart := Data;
    pEnd := PByte(DWORD(pStart) + DataSize - SizeOf(Byte));
    while DWORD(pStart) < DWORD(pEnd) do
    begin
      pStart^ := pStart^ xor pEnd^;
      pEnd^ := pStart^ xor pEnd^;
      pStart^ := pStart^ xor pEnd^;
      Inc(pStart);
      Dec(pEnd);
    end;
    Result := True;
  end;

  function Invert16Bit: Boolean;
  var
    pStart, pEnd: PSmallInt;
  begin
    pStart := Data;
    pEnd := PSmallInt(DWORD(pStart) + DataSize - SizeOf(SmallInt));
    while DWORD(pStart) < DWORD(pEnd) do
    begin
      pStart^ := pStart^ xor pEnd^;
      pEnd^ := pStart^ xor pEnd^;
      pStart^ := pStart^ xor pEnd^;
      Inc(pStart);
      Dec(pEnd);
    end;
    Result := True;
  end;

begin
  Result := False;
  if pWaveFormat^.wFormatTag = WAVE_FORMAT_PCM then
    case pWaveFormat^.wBitsPerSample of
      8: Result := Invert8Bit;
      16: Result := Invert16Bit;
    end;
end;

// Fills the wave data with silence
function SilenceWaveAudio(const Data: Pointer; DataSize: DWORD;
  const pWaveFormat: PWaveFormatEx): Boolean;
begin
  Result := False;
  if pWaveFormat^.wFormatTag = WAVE_FORMAT_PCM then
  begin
    Result := True;
    case pWaveFormat^.wBitsPerSample of
      8: FillChar(Data^, DataSize, $7F);
      16: FillChar(Data^, DataSize, 0);
    else
      Result := False;
    end;
  end;
end;

// Increases/Decreases the wave data volume by the specified percentage (PCM format only).
function ChangeWaveAudioVolume(const Data: Pointer; DataSize: DWORD;
  const pWaveFormat: PWaveFormatEx; Percent: Integer): Boolean;

  function ChangeVolume8Bit: Boolean;
  var
    pSample: PByte;
    Value: Integer;
  begin
    pSample := Data;
    while DataSize > 0 do
    begin
      Value := pSample^ + (pSample^ * Percent) div 100;
      if Value > High(Byte) then
        Value := High(Byte)
      else if Value < 0 then
        Value := 0;
      pSample^ := Value;
      Inc(pSample);
      Dec(DataSize, SizeOf(Byte));
    end;
    Result := True;
  end;

  function ChangeVolume16Bit: Boolean;
  var
    pSample: PSmallInt;
    Value: Integer;
  begin
    pSample := Data;
    while DataSize > 0 do
    begin
      Value := pSample^ + (pSample^ * Percent) div 100;
      if Value > High(SmallInt) then
        Value := High(SmallInt)
      else if Value < -High(SmallInt) then
        Value := -High(SmallInt);
      pSample^ := Value;
      Inc(pSample);
      Dec(DataSize, SizeOf(SmallInt));
    end;
    Result := True;
  end;

begin
  Result := False;
  if pWaveFormat^.wFormatTag = WAVE_FORMAT_PCM then
    case pWaveFormat^.wBitsPerSample of
      8: Result := ChangeVolume8Bit;
      16: Result := ChangeVolume16Bit;
    end;
end;

// Mixes several waves with the same format (PCM format only).
function MixWaveAudio(const RawWaves: PRawWave; Count: Integer;
  const pWaveFormat: PWaveFormatEx; Buffer: Pointer; BufferSize: DWORD): Boolean;

  function Mix8Bit: Boolean;
  type
    TPByteArray = array[0..1024] of PByte;
    PPByteArray = ^TPByteArray;
  var
    RawWave: PRawWave;
    CurSamples: PPByteArray;
    LastSamples: PPByteArray;
    CurSample: PByte;
    LastSample: PByte;
    I, Sum: Integer;
  begin
    GetMem(CurSamples, SizeOf(PByte) * Count);
    try
      GetMem(LastSamples, SizeOf(PByte) * Count);
      try
        RawWave := RawWaves;
        for I := 0 to Count - 1 do
        begin
          CurSamples[I] := RawWave^.pData;
          LastSamples[I] := PByte(DWORD(RawWave^.pData) + RawWave^.dwSize);
          Inc(RawWave);
        end;
        CurSample := Buffer;
        LastSample := PByte(DWORD(Buffer) + BufferSize);
        while CurSample <> LastSample do
        begin
          Sum := 0;
          for I := 0 to Count - 1 do
            if CurSamples[I] <> LastSamples[I] then
            begin
              Inc(Sum, CurSamples[I]^);
              Inc(CurSamples[I]);
            end;
          CurSample^ := Sum div Count;
          Inc(CurSample);
        end;
      finally
        FreeMem(LastSamples);
      end;
    finally
      FreeMem(CurSamples);
    end;
    Result := True;
  end;

  function Mix16Bit: Boolean;
  type
    TPSmallIntArray = array[0..1024] of PSmallInt;
    PPSmallIntArray = ^TPSmallIntArray;
  var
    RawWave: PRawWave;
    CurSamples: PPSmallIntArray;
    LastSamples: PPSmallIntArray;
    CurSample: PSmallInt;
    LastSample: PSmallInt;
    I, Sum: Integer;
  begin
    GetMem(CurSamples, SizeOf(PSmallInt) * Count);
    try
      GetMem(LastSamples, SizeOf(PSmallInt) * Count);
      try
        RawWave := RawWaves;
        for I := 0 to Count - 1 do
        begin
          CurSamples[I] := RawWave^.pData;
          LastSamples[I] := PSmallInt(DWORD(RawWave^.pData) + RawWave^.dwSize);
          Inc(RawWave);
        end;
        CurSample := Buffer;
        LastSample := PSmallInt(DWORD(Buffer) + BufferSize);
        while CurSample <> LastSample do
        begin
          Sum := 0;
          for I := 0 to Count - 1 do
            if CurSamples[I] <> LastSamples[I] then
            begin
              Inc(Sum, CurSamples[I]^);
              Inc(CurSamples[I]);
            end;
          CurSample^ := Sum div Count;
          Inc(CurSample);
        end;
      finally
        FreeMem(LastSamples);
      end;
    finally
      FreeMem(CurSamples);
    end;
    Result := True;
  end;

begin
  Result := False;
  if pWaveFormat^.wFormatTag = WAVE_FORMAT_PCM then
    case pWaveFormat^.wBitsPerSample of
      8: Result := Mix8Bit;
      16: Result := Mix16Bit;
    end;
end;

// Converts the wave data to the specified format. The caller is responsible to
// release the memory allocated for the converted wave data buffer.
function ConvertWaveFormat(const srcFormat: PWaveFormatEx; srcData: Pointer; srcDataSize: DWORD;
  const dstFormat: PWaveFormatEx; var dstData: Pointer; var dstDataSize: DWORD): Boolean;
var
  hStream: HACMSTREAM;
  Header: TACMSTREAMHEADER;
begin
  Result := False;
  if acmStreamOpen(hStream, 0, srcFormat, dstFormat, nil, 0, 0, ACM_STREAMOPENF_NONREALTIME) = 0 then
  begin
    try
      if acmStreamSize(hStream, srcDataSize, dstDataSize, ACM_STREAMSIZEF_SOURCE) = 0 then
      begin
        dstData := nil;
        FillChar(Header, SizeOf(Header), 0);
        ReallocMem(dstData, dstDataSize);
        try
          Header.cbStruct := SizeOf(Header);
          Header.pbSrc := srcData;
          Header.cbSrcLength := srcDataSize;
          Header.pbDst := dstData;
          Header.cbDstLength := dstDataSize;
          if acmStreamPrepareHeader(hStream, Header, 0) = 0 then
            try
              Result := (acmStreamConvert(hStream, Header, ACM_STREAMCONVERTF_START or ACM_STREAMCONVERTF_END) = 0);
            finally
              acmStreamUnprepareHeader(hStream, Header, 0);
            end;
        finally
          ReallocMem(dstData, Header.cbDstLengthUsed);
          dstDataSize := Header.cbDstLengthUsed;
        end;
      end;
    finally
      acmStreamClose(hStream, 0);
    end;
  end;
end;

// Initializes a standard PCM wave format header. The size of memory referenced
// by the pWaveFormat parameter must not be less than the size of TWaveFormatEx
// record.
procedure SetPCMAudioFormat(const pWaveFormat: PWaveFormatEx;
  Channels: TPCMChannel; SamplesPerSec: TPCMSamplesPerSec;
  BitsPerSample: TPCMBitsPerSample);
begin
  with pWaveFormat^ do
  begin
    wFormatTag := WAVE_FORMAT_PCM;
    case Channels of
      cMono: nChannels := 1;
      cStereo: nChannels := 2;
    end;
    case SamplesPerSec of
      ss8000Hz: nSamplesPerSec := 8000;
      ss11025Hz: nSamplesPerSec := 11025;
      ss22050Hz: nSamplesPerSec := 22050;
      ss44100Hz: nSamplesPerSec := 44100;
      ss48000Hz: nSamplesPerSec := 48000;
    end;
    case BitsPerSample of
      bs8Bit: wBitsPerSample := 8;
      bs16Bit: wBitsPerSample := 16;
    end;
    nBlockAlign := MulDiv(nChannels, wBitsPerSample, 8);
    nAvgBytesPerSec := nSamplesPerSec * nBlockAlign;
    cbSize := 0;
  end;
end;

// Initializes a standard PCM wave format header (shorcut form). The size of
// memory referenced by the pWaveFormat parameter must not be less than the
// size of TWaveFormatEx record.
procedure SetPCMAudioFormatS(const pWaveFormat: PWaveFormatEx; PCMFormat: TPCMFormat);
begin
  case PCMFormat of
    Mono8Bit8000Hz:
      SetPCMAudioFormat(pWaveFormat, cMono, ss8000Hz, bs8Bit);
    Mono8Bit11025Hz:
      SetPCMAudioFormat(pWaveFormat, cMono, ss11025Hz, bs8Bit);
    Mono8Bit22050Hz:
      SetPCMAudioFormat(pWaveFormat, cMono, ss22050Hz, bs8Bit);
    Mono8Bit44100Hz:
      SetPCMAudioFormat(pWaveFormat, cMono, ss44100Hz, bs8Bit);
    Mono8Bit48000Hz:
      SetPCMAudioFormat(pWaveFormat, cMono, ss48000Hz, bs8Bit);
    Mono16Bit8000Hz:
      SetPCMAudioFormat(pWaveFormat, cMono, ss8000Hz, bs16Bit);
    Mono16Bit11025Hz:
      SetPCMAudioFormat(pWaveFormat, cMono, ss11025Hz, bs16Bit);
    Mono16Bit22050Hz:
      SetPCMAudioFormat(pWaveFormat, cMono, ss22050Hz, bs16Bit);
    Mono16Bit44100Hz:
      SetPCMAudioFormat(pWaveFormat, cMono, ss44100Hz, bs16Bit);
    Mono16Bit48000Hz:
      SetPCMAudioFormat(pWaveFormat, cMono, ss48000Hz, bs16Bit);
    Stereo8bit8000Hz:
      SetPCMAudioFormat(pWaveFormat, cStereo, ss8000Hz, bs8Bit);
    Stereo8bit11025Hz:
      SetPCMAudioFormat(pWaveFormat, cStereo, ss11025Hz, bs8Bit);
    Stereo8bit22050Hz:
      SetPCMAudioFormat(pWaveFormat, cStereo, ss22050Hz, bs8Bit);
    Stereo8bit44100Hz:
      SetPCMAudioFormat(pWaveFormat, cStereo, ss44100Hz, bs8Bit);
    Stereo8bit48000Hz:
      SetPCMAudioFormat(pWaveFormat, cStereo, ss48000Hz, bs8Bit);
    Stereo16bit8000Hz:
      SetPCMAudioFormat(pWaveFormat, cStereo, ss8000Hz, bs16Bit);
    Stereo16bit11025Hz:
      SetPCMAudioFormat(pWaveFormat, cStereo, ss11025Hz, bs16Bit);
    Stereo16bit22050Hz:
      SetPCMAudioFormat(pWaveFormat, cStereo, ss22050Hz, bs16Bit);
    Stereo16bit44100Hz:
      SetPCMAudioFormat(pWaveFormat, cStereo, ss44100Hz, bs16Bit);
    Stereo16bit48000Hz:
      SetPCMAudioFormat(pWaveFormat, cStereo, ss48000Hz, bs16Bit);
  end;
end;

// Returns the standard PCM format specifier of a wave format.
function GetPCMAudioFormat(const pWaveFormat: PWaveFormatEx): TPCMFormat;
begin
  Result := nonePCM;
  with pWaveFormat^ do
    if wFormatTag = WAVE_FORMAT_PCM then
    begin
      if (nChannels = 1) and (nSamplesPerSec = 8000) and (wBitsPerSample = 8) then
        Result := Mono8Bit8000Hz
      else if (nChannels = 2) and (nSamplesPerSec = 8000) and (wBitsPerSample = 8) then
        Result := Stereo8Bit8000Hz
      else if (nChannels = 1) and (nSamplesPerSec = 8000) and (wBitsPerSample = 16) then
        Result := Mono16bit8000Hz
      else if (nChannels = 2) and (nSamplesPerSec = 8000) and (wBitsPerSample = 16) then
        Result := Stereo16Bit8000Hz
      else if (nChannels = 1) and (nSamplesPerSec = 11025) and (wBitsPerSample = 8) then
        Result := Mono8Bit11025Hz
      else if (nChannels = 2) and (nSamplesPerSec = 11025) and (wBitsPerSample = 8) then
        Result := Stereo8Bit11025Hz
      else if (nChannels = 1) and (nSamplesPerSec = 11025) and (wBitsPerSample = 16) then
        Result := Mono16bit11025Hz
      else if (nChannels = 2) and (nSamplesPerSec = 11025) and (wBitsPerSample = 16) then
        Result := Stereo16Bit11025Hz
      else if (nChannels = 1) and (nSamplesPerSec = 22050) and (wBitsPerSample = 8) then
        Result := Mono8Bit22050Hz
      else if (nChannels = 2) and (nSamplesPerSec = 22050) and (wBitsPerSample = 8) then
        Result := Stereo8Bit22050Hz
      else if (nChannels = 1) and (nSamplesPerSec = 22050) and (wBitsPerSample = 16) then
        Result := Mono16bit22050Hz
      else if (nChannels = 2) and (nSamplesPerSec = 22050) and (wBitsPerSample = 16) then
        Result := Stereo16Bit22050Hz
      else if (nChannels = 1) and (nSamplesPerSec = 44100) and (wBitsPerSample = 8) then
        Result := Mono8Bit44100Hz
      else if (nChannels = 2) and (nSamplesPerSec = 44100) and (wBitsPerSample = 8) then
        Result := Stereo8Bit44100Hz
      else if (nChannels = 1) and (nSamplesPerSec = 44100) and (wBitsPerSample = 16) then
        Result := Mono16bit44100Hz
      else if (nChannels = 2) and (nSamplesPerSec = 44100) and (wBitsPerSample = 16) then
        Result := Stereo16Bit44100Hz
      else if (nChannels = 1) and (nSamplesPerSec = 48000) and (wBitsPerSample = 8) then
        Result := Mono8Bit48000Hz
      else if (nChannels = 2) and (nSamplesPerSec = 48000) and (wBitsPerSample = 8) then
        Result := Stereo8Bit48000Hz
      else if (nChannels = 1) and (nSamplesPerSec = 48000) and (wBitsPerSample = 16) then
        Result := Mono16bit48000Hz
      else if (nChannels = 2) and (nSamplesPerSec = 48000) and (wBitsPerSample = 16) then
        Result := Stereo16Bit48000Hz
    end;
end;

// Returns the byte offset (zero base) of the wave audio specified by the pWaveFormat 
// parameter at the position specified by the Position parameter in milliseconds.
function GetWaveDataPositionOffset(const pWaveFormat: PWaveFormatEx; Position: DWORD): DWORD;
begin
  with pWaveFormat^ do
  begin
    Result := MulDiv(nAvgBytesPerSec, Position, 1000);
    if nBlockAlign > 1 then
      Result := nBlockAlign * (Result div nBlockAlign);
  end;
end;

// Converts milliseconds to string
function MS2Str(Milliseconds: DWORD; Fmt: TMS2StrFormat): String;
var
  HSecs, Secs, Mins, Hours: DWORD;
begin
  HSecs := Milliseconds div 10;
  Secs := HSecs div 100;
  Mins := Secs div 60;
  Hours := Mins div 60;
  if Fmt in [msAh, msA] then
  begin
    if Hours <> 0 then
      if Fmt = msAh then Fmt := msHMSh  else Fmt := msHMS
    else if Mins <> 0 then
      if Fmt = msAh then Fmt := msMSh else Fmt := msMS
    else
      if Fmt = msAh then Fmt := msSh else Fmt := msS
  end;
  case Fmt of
    msHMSh:
      Result := Format('%u%s%2.2u%s%2.2u%s%2.2u',
        [Hours, TimeSeparator, Mins mod 60, TimeSeparator, Secs mod 60, DecimalSeparator, HSecs mod 100]);
    msHMS:
      Result := Format('%u%s%2.2u%s%2.2u',
        [Hours, TimeSeparator, Mins mod 60, TimeSeparator, Secs mod 60]);
    msMSh:
      Result := Format('%u%s%2.2u%s%2.2u',
        [Mins, TimeSeparator, Secs mod 60, DecimalSeparator, HSecs mod 100]);
    msMS:
      Result := Format('%u%s%2.2u',
        [Mins, TimeSeparator, Secs mod 60]);
    msSh:
      Result := Format('%u%s%2.2u',
        [Secs, DecimalSeparator, HSecs mod 100]);
    msS:
      Result := Format('%u', [Secs]);
  else
    Result := IntToStr(Milliseconds);
  end;
end;

// Waits for the scnchronize object while lets the caller thread processes
// its messages.
function WaitForSyncObject(SyncObject: THandle; Timeout: DWORD): DWORD;
const
  EVENTMASK = QS_ALLEVENTS and not QS_INPUT;
var
  Msg: TMsg;
  StartTime: DWORD;
  EllapsedTime: DWORD;
  Handle: THandle;
begin
  Handle := SyncObject;
  if (SyncObject = GetCurrentThread) or (SyncObject = GetCurrentProcess) then
    DuplicateHandle(GetCurrentProcess, SyncObject, GetCurrentProcess, @Handle, SYNCHRONIZE, False, 0);
  try
    repeat
      StartTime := GetTickCount;
      Result := MsgWaitForMultipleObjects(1, Handle, False, Timeout, EVENTMASK);
      if Result = WAIT_OBJECT_0 + 1 then
      begin
        while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) do
        begin
          if ((Msg.message < WM_KEYFIRST) or (Msg.message > WM_KEYLAST)) and
             ((Msg.message < WM_MOUSEFIRST) or (Msg.message > WM_MOUSELAST)) then
          begin
            TranslateMessage(Msg);
            DispatchMessage(Msg);
            if Msg.message = WM_QUIT then Exit;
          end;
        end;
        if Timeout <> INFINITE then
        begin
          EllapsedTime := GetTickCount - StartTime;
          if EllapsedTime < Timeout then
            Dec(Timeout, EllapsedTime)
          else
            Timeout := 0;
        end;
      end;
    until Result <> WAIT_OBJECT_0 + 1;
  finally
    if SyncObject <> Handle then
      CloseHandle(Handle);
  end;
end;

end.
