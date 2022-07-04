{ *************************************************************************** }
{                                                                             }
{ Audio Tools Library (Freeware)                                              }
{ Class TWAVfile - for extracting information from WAV file header            }
{                                                                             }
{ Copyright (c) 2001 by Jurgen Faul                                           }
{ E-mail: jfaul@gmx.de                                                        }
{ http://jfaul.de/atl                                                         }
{                                                                             }
{ Version 1.1 (9 October 2001)                                                }
{   - Fixed bug with WAV header detection                                     }
{                                                                             }
{ Version 1.0 (31 July 2001)                                                  }
{   - Info: channel mode, sample rate, bits per sample, file size, duration   }
{                                                                             }
{ *************************************************************************** }

//  - Modified for Delphi 2009  (08 May 2009)

unit WAVfile;

interface

uses
  Classes, SysUtils;

const
  { Used with ChannelModeID property }
  WAV_CM_MONO = 1;                                      { Index for mono mode }
  WAV_CM_STEREO = 2;                                  { Index for stereo mode }

  { Channel mode names }
  WAV_MODE: array [0..2] of string = ('Unknown', 'Mono', 'Stereo');

type
  { Class TWAVfile }
  TWAVfile = class(TObject)
    private
      { Private declarations }
      FValid: Boolean;
      FChannelModeID: Byte;
      FSampleRate: Word;
      FBitsPerSample: Byte;
      FFileSize: Cardinal;
      procedure FResetData;
      function FGetChannelMode: string;
      function FGetDuration: Double;
    public
      { Public declarations }
      constructor Create;                                     { Create object }
      function ReadFromFile(const FileName: string): Boolean;   { Load header }
      property Valid: Boolean read FValid;             { True if header valid }
      property ChannelModeID: Byte read FChannelModeID;   { Channel mode code }
      property ChannelMode: string read FGetChannelMode;  { Channel mode name }
      property SampleRate: Word read FSampleRate;          { Sample rate (hz) }
      property BitsPerSample: Byte read FBitsPerSample;     { Bits per sample }
      property FileSize: Cardinal read FFileSize;         { File size (bytes) }
      property Duration: Double read FGetDuration;       { Duration (seconds) }
  end;

implementation

type
  { Real structure of WAV file header }
  WAVRecord = record
    { RIFF file header }
    RIFFHeader: array [1..4] of AnsiChar;        { Must be "RIFF" }
    FileSize: Integer;                           { Must be "RealFileSize - 8" }
    WAVEHeader: array [1..4] of AnsiChar;        { Must be "WAVE" }
    { Format information }
    FormatHeader: array [1..4] of AnsiChar;      { Must be "fmt " }
    FormatSize: Integer;                         { Must be 16 (decimal) }
    FormatCode: Word;                            { Must be 1 }
    ChannelNumber: Word;                         { Number of channels }
    SampleRate: Integer;                         { Sample rate (hz) }
    BytesPerSecond: Integer;                     { Bytes per second }
    BytesPerSample: Word;                        { Bytes per sample }
    BitsPerSample: Word;                         { Bits per sample }
    { Data area }
    DataHeader: array [1..4] of AnsiChar;        { Must be "data" }
    DataSize: Integer;                           { Data size }
  end;

{ ********************* Auxiliary functions & procedures ******************** }

function ReadWAV(const FileName: string; var WAVData: WAVRecord): Boolean;
var
  SourceFile: file;
  Transferred: Integer;
begin
  try
    Result := true;
    { Set read-access and open file }
    AssignFile(SourceFile, FileName);
    FileMode := 0;
    Reset(SourceFile, 1);
    { Read header }
    BlockRead(SourceFile, WAVData, 44, Transferred);
    CloseFile(SourceFile);
    { if transfer is not complete }
    if Transferred < 44 then Result := false;
  except
    { Error }
    Result := false;
  end;
end;

{ --------------------------------------------------------------------------- }

function HeaderIsValid(const WAVData: WAVRecord): Boolean;
begin
  Result := true;   // Assume

  { Validation }
  if WAVData.RIFFHeader <> 'RIFF' then
  begin
    Result := false;
    exit;
  end;
  if WAVData.WAVEHeader <> 'WAVE' then
  begin
    Result := false;
    exit;
  end;
  if WAVData.FormatHeader <> 'fmt ' then
  begin
    Result := false;
    exit;
  end;
  if (WAVData.ChannelNumber <> WAV_CM_MONO) and
    (WAVData.ChannelNumber <> WAV_CM_STEREO) then Result := false;
end;

{ ********************** Private functions & procedures ********************* }

procedure TWAVfile.FResetData;
begin
  FValid := false;
  FChannelModeID := 0;
  FSampleRate := 0;
  FBitsPerSample := 0;
  FFileSize := 0;
end;

{ --------------------------------------------------------------------------- }

function TWAVfile.FGetChannelMode: string;
begin
  Result := WAV_MODE[FChannelModeID];
end;

{ --------------------------------------------------------------------------- }

function TWAVfile.FGetDuration: Double;
begin
  if FValid then
    Result := (FFileSize - 44) * 8 /
      FSampleRate / FBitsPerSample / FChannelModeID
  else
    Result := 0;
end;

{ ********************** Public functions & procedures ********************** }

constructor TWAVfile.Create;
begin
  inherited;
  FResetData;
end;

{ --------------------------------------------------------------------------- }

function TWAVfile.ReadFromFile(const FileName: string): Boolean;
var
  WAVData: WAVRecord;
begin
  { Reset and load header data from file to variable }
  FResetData;
  Result := ReadWAV(FileName, WAVData);
  { Process data if loaded and header valid }
  if (Result) and (HeaderIsValid(WAVData)) then
  begin
    FValid := true;
    { Fill properties with header data }
    FChannelModeID := WAVData.ChannelNumber;
    FSampleRate := WAVData.SampleRate;
    FBitsPerSample := WAVData.BitsPerSample;
    FFileSize := WAVData.FileSize + 8;
  end;
end;

end.
