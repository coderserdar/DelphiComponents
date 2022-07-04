{------------------------------------------------------------------------------}
{                                                                              }
{  WaveStorage - Wave storage components                                       }
{  by Kambiz R. Khojasteh                                                      }
{                                                                              }
{  kambiz@delphiarea.com                                                       }
{  http://www.delphiarea.com                                                   }
{                                                                              }
{------------------------------------------------------------------------------}

{$I DELPHIAREA.INC}

unit scWaveStorage;

interface

uses
  Windows, Messages, Classes, MMSystem, scWaveUtils, scWaveACM;

type

  // Encapsulates a wave audio as a stream and provides easy access to its
  // informational fields.
  TWaveStreamAdapter = class(TPersistent)
  private
    fStream: TStream;
    fOwnership: TStreamOwnership;
    fModified: Boolean;
    fValid: Boolean;
    fDataSize: DWORD;
    fDataOffset: DWORD;
    fWaveFormat: PWaveFormatEx;
    fOnChanging: TNotifyEvent;
    fOnChange: TNotifyEvent;
    fState: TWaveStreamState;
    function GetValid: Boolean;
    function GetEmpty: Boolean;
    function GetDataSize: DWORD;
    function GetDataOffset: DWORD;
    function GetLength: DWORD;
    function GetBitRate: DWORD;
    function GetPeakLevel: Integer;
    function GetPCMFormat: TPCMFormat;
    function GetWaveFormat: PWaveFormatEx;
    function GetAudioFormat: String;
    function GetPosition: Integer;
    procedure SetPosition(Value: Integer);
  protected
    ckRIFF, ckData: TMMCKInfo;
    mmIO: HMMIO;
    function UpdateWaveInfo: Boolean; virtual;
    function LockData(var pData: Pointer; ForceCopy: Boolean): Boolean; virtual;
    function UnlockData(pData: Pointer; WriteData: Boolean): Boolean; virtual;
    function MSecToByte(MSec: DWORD): DWORD; virtual;
    procedure DoChanging; virtual;
    procedure DoChange; virtual;
    property Modified: Boolean read fModified;
  public
    constructor Create(AStream: TStream; AOwnership: TStreamOwnership
      {$IFDEF COMPILER4_UP} = soReference {$ENDIF}); virtual;
    destructor Destroy; override;
    function Equals(Wave: TWaveStreamAdapter): Boolean;
      {$IFDEF COMPILER4_UP} reintroduce; {$ENDIF} virtual;
    function SameFormat(WaveStream: TWaveStreamAdapter): Boolean; virtual;
    function SameWaveFormat(pWaveFormat: PWaveFormatEx): Boolean; virtual;
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromStream(AStream: TStream); virtual;
    procedure SaveToStream(AStream: TStream); virtual;
    procedure LoadFromFile(const AFileName: String); virtual;
    procedure SaveToFile(const AFileName: String); virtual;
    procedure Clear; virtual;
    procedure Refresh; virtual;
    procedure Crop; virtual;
    function Invert: Boolean; virtual;
    function ChangeVolume(Percent: Integer): Boolean; virtual;
    function ConvertTo(const pTargetWaveFormat: PWaveFormatEx): Boolean; virtual;
    function ConvertToPCM(TargetFormat: TPCMFormat): Boolean;
    function Copy(Source: TWaveStreamAdapter;
      Pos: DWORD {$IFDEF COMPILER4_UP} = 0 {$ENDIF};
      Len: DWORD {$IFDEF COMPILER4_UP} = $FFFFFFFF {$ENDIF}): Boolean; virtual;
    function Sub(Pos: DWORD; Len: DWORD): Boolean; virtual;
    function Delete(Pos: DWORD; Len: DWORD): Boolean; virtual;
    function Insert(Pos: DWORD; WaveStream: TWaveStreamAdapter): Boolean; virtual;
    function InsertSilence(Pos: DWORD; Len: DWORD): Boolean; virtual;
    function TrimStart(Len: DWORD): Boolean; virtual;
    function TrimEnd(Len: DWORD): Boolean; virtual;
    function Concat(const Sources: array of TWaveStreamAdapter;
      Gap: DWORD {$IFDEF COMPILER4_UP} = 0 {$ENDIF}): Boolean; virtual;
    function Mix(const Sources: array of TWaveStreamAdapter): Boolean; virtual;
    function BeginRewrite(pWaveFormat: PWaveFormatEx): Boolean; virtual;
    function BeginRewritePCM(Format: TPCMFormat): Boolean;
    function EndRewrite: Boolean; virtual;
    function Write(const Buffer; Count: Integer): Integer; virtual;
    function BeginRead: Boolean; virtual;
    function EndRead: Boolean; virtual;
    function Read(var Buffer; Count: Integer): Integer; virtual;
    property Stream: TStream read fStream;
    property Ownership: TStreamOwnership read fOwnership write fOwnership;
    property State: TWaveStreamState read fState;
    property Valid: Boolean read GetValid;
    property Empty: Boolean read GetEmpty;
    property DataSize: DWORD read GetDataSize;
    property DataOffset: DWORD read GetDataOffset;
    property PCMFormat: TPCMFormat read GetPCMFormat;
    property WaveFormat: PWaveFormatEx read GetWaveFormat;
    property AudioFormat: String read GetAudioFormat;
    property Length: DWORD read GetLength;              // in milliseconds
    property BitRate: DWORD read GetBitRate;            // in kbps
    property PeakLevel: Integer read GetPeakLevel;      // in percent
    property Position: Integer read GetPosition write SetPosition;
    property OnChanging: TNotifyEvent read fOnChanging write fOnChanging;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  end;

  // Converts audio format of wave streams
  TWaveStreamConverter = class(TWaveStreamAdapter)
  private
    fBufferFormat: PWaveFormatEx;
    ACMStream: HACMSTREAM;
    ACMHeader: TACMSTREAMHEADER;
    SrcBuffer: Pointer;
    SrcBufferSize: DWORD;
    DstBuffer: Pointer;
    DstBufferSize: DWORD;
    BufferOffset: DWORD;
  protected
    procedure Reset;
  public
    destructor Destroy; override;
    procedure SetBufferFormat(const pWaveFormat: PWaveFormatEx); virtual;
    procedure SetBufferFormatPCM(Format: TPCMFormat);
    function CanRewrite(pWaveFormat: PWaveFormatEx): Boolean; virtual;
    function CanRewritePCM(Format: TPCMFormat): Boolean;
    function BeginRewrite(pWaveFormat: PWaveFormatEx): Boolean; override;
    function EndRewrite: Boolean; override;
    function Write(const Buffer; Count: Integer): Integer; override;
    function CanRead: Boolean; virtual;
    function BeginRead: Boolean; override;
    function EndRead: Boolean; override;
    function Read(var Buffer; Count: Integer): Integer; override;
    property BufferFormat: PWaveFormatEx read fBufferFormat;
  end;

  // Creates a memory stream as a wave audio
  TWave = class(TWaveStreamAdapter)
  public
    constructor Create;
      {$IFDEF COMPILER4_UP} reintroduce; {$ENDIF}
  end;

  // Creates a file stream as a wave audio
  TWaveFile = class(TWaveStreamAdapter)
  public
    constructor Create(const FileName: String; Mode: Word);
      {$IFDEF COMPILER4_UP} reintroduce; {$ENDIF}
  end;

  // Creates a memory stream as a wave audio for conversion purpose
  TWaveConverter = class(TWaveStreamConverter)
  public
    constructor Create;
      {$IFDEF COMPILER4_UP} reintroduce; {$ENDIF}
  end;

  // Creates a file stream as a wave audio for conversion purpose
  TWaveFileConverter = class(TWaveStreamConverter)
  public
    constructor Create(const FileName: String; Mode: Word);
      {$IFDEF COMPILER4_UP} reintroduce; {$ENDIF}
  end;

  // Base class for wave storage classes
  TCustomWaveStorage = class(TComponent)
  protected
    function GetWaveStream(Index: Integer): TStream; virtual; abstract;
  public
    function Equals(Another: TCustomWaveStorage): Boolean;
      {$IFDEF COMPILER4_UP} reintroduce; {$ENDIF} virtual; abstract;
    property WaveStream[Index: Integer]: TStream read GetWaveStream;
  end;

  // Stores one wave audio
  TWaveStorage = class(TCustomWaveStorage)
  private
    fWave: TWave;
    procedure SetWave(Value: TWave);
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    function GetWaveStream(Index: Integer): TStream; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function Equals(Another: TCustomWaveStorage): Boolean; override;
  published
    property Wave: TWave read fWave write SetWave;
  end;

  // Stores a collection of wave audios

  TWaveItem = class;
  TWaveItems = class;

  TWaveItemClass = class of TWaveItem;

  // TWave Item
  TWaveItem = class(TCollectionItem)
  private
    fName: String;
    fWave: TWave;
    fTag: Integer;
    procedure SetWave(Value: TWave);
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    function GetDisplayName: String; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Wave: TWave read fWave write SetWave;
    property Name: String read fName write fName;
    property Tag: Integer read fTag write fTag default 0;
  end;

  // TWaveItems
  TWaveItems = class(TCollection)
  private
    fOwner: TPersistent;
    function GetItem(Index: Integer): TWaveItem;
    procedure SetItem(Index: Integer; Value: TWaveItem);
  protected
    function GetOwner: TPersistent; override;
  public
    {$IFNDEF COMPILER4_UP}
    constructor Create(AOwner: TPersistent; ItemClass: TWaveItemClass); virtual;
    {$ELSE}
    constructor Create(AOwner: TPersistent; ItemClass: TWaveItemClass); reintroduce; virtual;
    {$ENDIF}
    function Add: TWaveItem;
    {$IFDEF COMPILER4_UP}
    function Insert(Index: Integer): TWaveItem;
    {$ENDIF}
    property Items[Index: Integer]: TWaveItem read GetItem write SetItem; default;
  end;

  // TWaveCollection
  TWaveCollection = class(TCustomWaveStorage)
  private
    fWaves: TWaveItems;
    procedure SetWaves(const Value: TWaveItems);
  protected
    function GetWaveStream(Index: Integer): TStream; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Equals(Another: TCustomWaveStorage): Boolean; override;
    function ExportWaveNames(const List: TStrings): Integer; virtual;
    function IndexOfName(const AName: String): Integer; virtual;
  published
    property Waves: TWaveItems read fWaves write SetWaves;
  end;

implementation

uses
  SysUtils, DateUtils;

{ TWaveStreamAdapter }

constructor TWaveStreamAdapter.Create(AStream: TStream; AOwnership: TStreamOwnership);
begin
  inherited Create;
  fModified := True;
  fStream := AStream;
  fOwnership := AOwnership;
end;

destructor TWaveStreamAdapter.Destroy;
begin
  case fState of
    wssReading: EndRead;
    wssWriting: EndRewrite;
  end;
  if Assigned(fWaveFormat) then
    FreeMem(fWaveFormat);
  if fOwnership = soOwned then
    fStream.Free;
  inherited Destroy;
end;

function TWaveStreamAdapter.BeginRewritePCM(Format: TPCMFormat): Boolean;
var
  WaveFormatEx: TWaveFormatEx;
begin
  SetPCMAudioFormatS(@WaveFormatEx, Format);
  Result := BeginRewrite(@WaveFormatEx);
end;

function TWaveStreamAdapter.BeginRewrite(pWaveFormat: PWaveFormatEx): Boolean;
begin
  Result := False;
  if fState = wssReady then
  begin
    mmIO := CreateStreamWaveAudio(Stream, pWaveFormat, ckRIFF, ckData);
    if mmIO <> 0 then
    begin
      fState := wssWriting;
      fDataOffset := mmioSeek(mmIO, 0, SEEK_CUR);
      DoChanging;
      Result := True;
    end;
  end;
end;

function TWaveStreamAdapter.EndRewrite: Boolean;
begin
  Result := False;
  if fState = wssWriting then
  begin
    mmioAscend(mmIO, @ckData, 0);
    mmioAscend(mmIO, @ckRIFF, 0);
    mmioClose(mmIO, 0);
    mmIO := 0;
    fState := wssReady;
    UpdateWaveInfo;
    Stream.Size := fDataOffset + fDataSize;
    Result := True;
  end;
end;

function TWaveStreamAdapter.Write(const Buffer; Count: Integer): Longint;
begin
  if fState = wssWriting then
    Result := mmioWrite(mmIO, @Buffer, Count)
  else
    Result := -1;
end;

function TWaveStreamAdapter.BeginRead: Boolean;
begin
  Result := False;
  if Valid and (fState = wssReady) then
  begin
    mmIO := OpenStreamWaveAudio(Stream);
    if mmIO <> 0 then
    begin
      mmioSeek(mmIO, fDataOffset, SEEK_SET);
      fState := wssReading;
      Result := True;
    end;
  end;
end;

function TWaveStreamAdapter.EndRead: Boolean;
begin
  Result := False;
  if fState = wssReading then
  begin
    mmioClose(mmIO, 0);
    mmIO := 0;
    fState := wssReady;
    Result := True;
  end;
end;

function TWaveStreamAdapter.Read(var Buffer; Count: Integer): Integer;
begin
  if fState = wssReading then
    Result := mmioRead(mmIO, @Buffer, Count)
  else
    Result := -1;
end;

procedure TWaveStreamAdapter.DoChanging;
begin
  if not fModified then
  begin
    fValid := False;
    fModified := True;
    if Assigned(fOnChanging) then
      fOnChanging(Self);
  end;
end;

procedure TWaveStreamAdapter.DoChange;
begin
  if Assigned(fOnChange) then
    fOnChange(Self);
end;

function TWaveStreamAdapter.MSecToByte(MSec: DWORD): DWORD;
begin
  Result := GetWaveDataPositionOffset(fWaveFormat, MSec);
end;

function TWaveStreamAdapter.LockData(var pData: Pointer; ForceCopy: Boolean): Boolean;
begin
  Result := False;
  pData := nil;
  if UpdateWaveInfo and (fDataSize <> 0) then
  begin
    Result := True;
    if not ForceCopy and (Stream is TCustomMemoryStream) then
    begin
      pData := TCustomMemoryStream(Stream).Memory;
      Inc(PByte(pData), fDataOffset);
    end
    else
    begin
      ReallocMem(pData, fDataSize);
      Stream.Seek(fDataOffset, soFromBeginning);
      if DWORD(Stream.Read(pData^, fDataSize)) <> fDataSize then
      begin
        ReallocMem(pData, 0);
        Result := False;
      end;
    end;
  end;
end;

function TWaveStreamAdapter.UnlockData(pData: Pointer; WriteData: Boolean): Boolean;
begin
  Result := False;
  if Assigned(pData) then
  begin
    Result := True;
    if not (Stream is TCustomMemoryStream) or
      ((DWORD(TCustomMemoryStream(Stream).Memory) + fDataOffset) <> DWORD(pData)) then
    begin
      if WriteData then
      begin
        Stream.Seek(fDataOffset, soFromBeginning);
        if DWORD(Stream.Write(pData^, fDataSize)) <> fDataSize then
          Result := False;
      end;
      ReallocMem(pData, 0);
    end;
  end;
end;

function TWaveStreamAdapter.UpdateWaveInfo: Boolean;
begin
  if fModified and (fState <> wssWriting) then
  begin
    fModified := False;
    if Assigned(fWaveFormat) then
    begin
      FreeMem(fWaveFormat);
      fWaveFormat := nil;
    end;
    fValid := GetStreamWaveAudioInfo(Stream, fWaveFormat, fDataSize, fDataOffset);
    DoChange;
  end;
  Result := fValid;
end;

function TWaveStreamAdapter.GetPosition: Integer;
begin
  if fState <> wssReady then
    Result := mmioSeek(mmIO, 0, SEEK_CUR) - Integer(fDataOffset)
  else
    Result := -1;
end;

procedure TWaveStreamAdapter.SetPosition(Value: Integer);
begin
  if fState <> wssReady then
    mmioSeek(mmIO, Integer(fDataOffset) + Value, SEEK_SET);
end;

function TWaveStreamAdapter.GetAudioFormat: String;
begin
  if UpdateWaveInfo then
    Result := GetWaveAudioFormat(fWaveFormat)
  else
    Result := '';
end;

function TWaveStreamAdapter.GetBitRate: DWORD;
begin
  if UpdateWaveInfo then
    Result := GetWaveAudioBitRate(fWaveFormat)
  else
    Result := 0;
end;

function TWaveStreamAdapter.GetPeakLevel: Integer;
var
  Data: Pointer;
begin
  Result := -1;
  if LockData(Data, False) then
    try
      Result := GetWaveAudioPeakLevel(Data, fDataSize, fWaveFormat)
    finally
      UnlockData(Data, False);
    end;
end;

function TWaveStreamAdapter.GetLength: DWORD;
begin
  if UpdateWaveInfo then
    Result := GetWaveAudioLength(fWaveFormat, fDataSize)
  else
    Result := 0;
end;

function TWaveStreamAdapter.GetDataSize: DWORD;
begin
  if UpdateWaveInfo then
    Result := fDataSize
  else
    Result := 0;
end;

function TWaveStreamAdapter.GetDataOffset: DWORD;
begin
  if UpdateWaveInfo then
    Result := fDataOffset
  else
    Result := 0;
end;

function TWaveStreamAdapter.GetValid: Boolean;
begin
  Result := UpdateWaveInfo;
end;

function TWaveStreamAdapter.GetEmpty: Boolean;
begin
  Result := (Stream.Size = 0);
end;

function TWaveStreamAdapter.GetPCMFormat: TPCMFormat;
begin
  if UpdateWaveInfo then
    Result := GetPCMAudioFormat(fWaveFormat)
  else
    Result := nonePCM;
end;

function TWaveStreamAdapter.GetWaveFormat: PWaveFormatEx;
begin
  if UpdateWaveInfo then
    Result := fWaveFormat
  else
    Result := nil;
end;

procedure TWaveStreamAdapter.Assign(Source: TPersistent);
begin
  if Source is TWaveStreamAdapter then
  begin
    TWaveStreamAdapter(Source).Stream.Position := 0;
    LoadFromStream(TWaveStreamAdapter(Source).Stream)
  end
  else
    inherited Assign(Source);
end;

procedure TWaveStreamAdapter.LoadFromStream(AStream: TStream);
begin
  DoChanging;
  fStream.Position := 0;
  fStream.CopyFrom(AStream, AStream.Size);
  fStream.Size := AStream.Size;
  fState := wssReady;
  UpdateWaveInfo;
end;

procedure TWaveStreamAdapter.SaveToStream(AStream: TStream);
begin
  fStream.Position := 0;
  AStream.CopyFrom(fStream, fStream.Size);
end;

procedure TWaveStreamAdapter.LoadFromFile(const AFileName: String);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TWaveStreamAdapter.SaveToFile(const AFileName: String);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(AFileName, fmCreate or fmShareExclusive);
  try
    SaveToStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TWaveStreamAdapter.Clear;
begin
  DoChanging;
  fStream.Size := 0;
  fState := wssReady;
  DoChange;
end;

procedure TWaveStreamAdapter.Refresh;
begin
  fModified := True;
end;

function TWaveStreamAdapter.Equals(Wave: TWaveStreamAdapter): Boolean;
var
  SelfData, WaveData: Pointer;
begin
  if SameFormat(Wave) and (fDataSize = Wave.fDataSize) then
  begin
    LockData(SelfData, False);
    try
      Wave.LockData(WaveData, False);
      try
        Result := CompareMem(SelfData, WaveData, fDataSize);
      finally
        Wave.UnlockData(WaveData, False);
      end;
    finally
      UnlockData(SelfData, False);
    end;
  end
  else
    Result := False;
end;

function TWaveStreamAdapter.SameFormat(WaveStream: TWaveStreamAdapter): Boolean;
begin
  Result := SameWaveFormat(WaveStream.WaveFormat);
end;

function TWaveStreamAdapter.SameWaveFormat(pWaveFormat: PWaveFormatEx): Boolean;
begin
  Result := Valid and Assigned(pWaveFormat) and
    (fWaveFormat^.cbSize = pWaveFormat^.cbSize) and
     CompareMem(fWaveFormat, pWaveFormat, SizeOf(TWaveFormatEx) + fWaveFormat^.cbSize)
end;

procedure TWaveStreamAdapter.Crop;
begin
  if fState <> wssWriting then
    Stream.Size := DataOffset + DataSize;
end;

function TWaveStreamAdapter.Invert: Boolean;
var
  Data: Pointer;
begin
  Result := False;
  if LockData(Data, False) then
    try
      Result := InvertWaveAudio(Data, fDataSize, fWaveFormat);
    finally
      UnlockData(Data, Result);
    end;
end;

function TWaveStreamAdapter.ChangeVolume(Percent: Integer): Boolean;
var
  Data: Pointer;
begin
  Result := False;
  if LockData(Data, False) then
    try
      Result := ChangeWaveAudioVolume(Data, fDataSize, fWaveFormat, Percent);
    finally
      UnlockData(Data, Result);
    end;
end;

function TWaveStreamAdapter.ConvertTo(const pTargetWaveFormat: PWaveFormatEx): Boolean;
var
  Data: Pointer;
  NewData: Pointer;
  NewDataSize: DWORD;
  HelperData: Pointer;
  HelperDataSize: DWORD;
  Succeeded: Boolean;
  HelperWaveFormat: TWaveFormatEx;
begin
  Result := False;
  if SameWaveFormat(pTargetWaveFormat) then
    Result := True
  else if LockData(Data, False) then
  begin
    try
      Succeeded := ConvertWaveFormat(fWaveFormat, Data, fDataSize,
        pTargetWaveFormat, NewData, NewDataSize);
      if not Succeeded then
      begin
        // try to convert using an intermediate format
        FillChar(HelperWaveFormat, SizeOf(HelperWaveFormat), 0);
        with HelperWaveFormat do
        begin
          wFormatTag := WAVE_FORMAT_PCM;
          nChannels := fWaveFormat^.nChannels;
          nSamplesPerSec := fWaveFormat^.nSamplesPerSec;
        end;
        if acmFormatSuggest(0, fWaveFormat, @HelperWaveFormat, SizeOf(HelperWaveFormat),
           ACM_FORMATSUGGESTF_WFORMATTAG or ACM_FORMATSUGGESTF_NCHANNELS or
           ACM_FORMATSUGGESTF_NSAMPLESPERSEC) = MMSYSERR_NOERROR then
        begin
          if ConvertWaveFormat(fWaveFormat, Data, fDataSize, @HelperWaveFormat,
             HelperData, HelperDataSize) then
          begin
            Succeeded := ConvertWaveFormat(@HelperWaveFormat, HelperData,
              HelperDataSize, pTargetWaveFormat, NewData, NewDataSize);
            ReallocMem(HelperData, 0);
          end;
        end;
      end;
    finally
      UnlockData(Data, False);
    end;
    if Succeeded then
      try
        if BeginRewrite(pTargetWaveFormat) then
          try
            Result := (DWORD(Write(NewData^, NewDataSize)) = NewDataSize);
          finally
            EndRewrite;
          end;
      finally
        ReallocMem(NewData, 0);
      end;
  end;
end;

function TWaveStreamAdapter.ConvertToPCM(TargetFormat: TPCMFormat): Boolean;
var
  NewWaveFormat: TWaveFormatEx;
begin
  Result := False;
  if TargetFormat <> nonePCM then
  begin
    SetPCMAudioFormatS(@NewWaveFormat, TargetFormat);
    Result := ConvertTo(@NewWaveFormat);
  end;
end;

function TWaveStreamAdapter.Copy(Source: TWaveStreamAdapter;
  Pos, Len: DWORD): Boolean;
var
  Data: Pointer;
  ByteStart, ByteEnd: DWORD;
begin
  Result := False;
  if Source.Valid and (Len > 0) and (Pos < Source.Length) then
  begin
    ByteStart := Source.MSecToByte(Pos);
    ByteEnd := ByteStart + Source.MSecToByte(Len);
    if ByteEnd > Source.fDataSize then
      ByteEnd := Source.fDataSize;
    if Source.LockData(Data, False) then
      try
        if BeginRewrite(Source.fWaveFormat) then
          try
            Result := Write(Pointer(DWORD(Data) + ByteStart)^, ByteEnd - ByteStart) > 0;
          finally
            EndRewrite;
          end;
      finally
        Source.UnlockData(Data, False);
      end;
  end;
end;

function TWaveStreamAdapter.Sub(Pos: DWORD; Len: DWORD): Boolean;
var
  Data: Pointer;
  ByteStart, ByteEnd: DWORD;
begin
  Result := False;
  if Valid and (Len > 0) and (Pos < Length) then
  begin
    ByteStart := MSecToByte(Pos);
    ByteEnd := ByteStart + MSecToByte(Len);
    if ByteEnd > fDataSize then
      ByteEnd := fDataSize;
    if LockData(Data, True) then
      try
        if BeginRewrite(fWaveFormat) then
          try
            Result := Write(Pointer(DWORD(Data) + ByteStart)^, ByteEnd - ByteStart) > 0;
          finally
            EndRewrite;
          end;
      finally
        UnlockData(Data, False);
      end;
  end;
end;

function TWaveStreamAdapter.Delete(Pos, Len: DWORD): Boolean;
var
  Data: Pointer;
  ByteStart, ByteEnd: DWORD;
begin
  Result := False;
  if Valid and (Len > 0) and (Pos < Length) then
  begin
    ByteStart := MSecToByte(Pos);
    ByteEnd := ByteStart + MSecToByte(Len);
    if ByteEnd > fDataSize then
      ByteEnd := fDataSize;
    if LockData(Data, True) then
      try
        Move(Pointer(DWORD(Data) + ByteEnd)^,
             Pointer(DWORD(Data) + ByteStart)^,
             fDataSize - ByteEnd);
        if BeginRewrite(fWaveFormat) then
          try
            Result := Write(Data^, fDataSize - (ByteEnd - ByteStart)) > 0;
          finally
            EndRewrite;
          end;
      finally
        UnlockData(Data, False);
      end;
  end;
end;

function TWaveStreamAdapter.Insert(Pos: DWORD; WaveStream: TWaveStreamAdapter): Boolean;
var
  SelfData, WaveData: Pointer;
  BytePos: DWORD;
begin
  Result := False;
  if SameFormat(WaveStream) then
  begin
    BytePos := MSecToByte(Pos);
    if BytePos > fDataSize then
      BytePos := fDataSize;
    if LockData(SelfData, True) then
      try
        if WaveStream.LockData(WaveData, False) then
        begin
          try
            ReallocMem(SelfData, fDataSize + WaveStream.fDataSize);
            Move(Pointer(DWORD(SelfData) + BytePos)^,
                 Pointer(DWORD(SelfData) + BytePos + WaveStream.fDataSize)^,
                 fDataSize - BytePos);
            Move(WaveData^, Pointer(DWORD(SelfData) + BytePos)^, WaveStream.fDataSize);
          finally
            WaveStream.UnlockData(WaveData, False);
          end;
          if BeginRewrite(fWaveFormat) then
            try
              Result := (Write(SelfData^, fDataSize + WaveStream.fDataSize) > 0);
            finally
              EndRewrite;
            end;
        end;
      finally
        UnlockData(SelfData, False);
      end;
  end;
end;

function TWaveStreamAdapter.InsertSilence(Pos, Len: DWORD): Boolean;
var
  BytePos, SilenceLen: DWORD;
  Data: Pointer;
begin
  Result := False;
  if Valid and (Len > 0) then
  begin
    BytePos := MSecToByte(Pos);
    if BytePos > fDataSize then
      BytePos := fDataSize;
    SilenceLen := MSecToByte(Len);
    if LockData(Data, True) then
      try
        ReallocMem(Data, fDataSize + SilenceLen);
        Move(Pointer(DWORD(Data) + BytePos)^,
             Pointer(DWORD(Data) + BytePos + SilenceLen)^,
             fDataSize - BytePos);
        Result := SilenceWaveAudio(Pointer(DWORD(Data) + BytePos), SilenceLen, fWaveFormat);
        if Result and BeginRewrite(fWaveFormat) then
          try
            Result := (Write(Data^, fDataSize + SilenceLen) > 0);
          finally
            EndRewrite;
          end;
      finally
        UnlockData(Data, False);
      end;
  end;
end;

function TWaveStreamAdapter.TrimStart(Len: DWORD): Boolean;
begin
  Result := Delete(0, Len);
end;

function TWaveStreamAdapter.TrimEnd(Len: DWORD): Boolean;
begin
  if Len > Length then Len := Length;
  Result := Delete(Length - Len, Len);
end;

function TWaveStreamAdapter.Concat(const Sources: array of TWaveStreamAdapter;
  Gap: DWORD): Boolean;
var
  TargetFormat: PWaveFormatEx;
  Data, SilentBuffer: Pointer;
  SilentBufferSize: DWORD;
  I: Integer;
begin
  Result := False;
  if System.Length(Sources) <> 0 then
  begin
    TargetFormat := Sources[Low(Sources)].WaveFormat;
    SilentBufferSize := 0;
    SilentBuffer := nil;
    try
      if (Gap <> 0) and (System.Length(Sources) > 1) then
      begin
        SilentBufferSize := GetWaveDataPositionOffset(TargetFormat, Gap);
        ReallocMem(SilentBuffer, SilentBufferSize);
        SilenceWaveAudio(SilentBuffer, SilentBufferSize, TargetFormat);
      end;
      BeginRewrite(TargetFormat);
      try
        for I := Low(Sources) to High(Sources) do
        begin
          if Assigned(SilentBuffer) and (I <> Low(Sources)) then
            Write(SilentBuffer^, SilentBufferSize);
          if Sources[I].LockData(Data, False) then
            try
              Write(Data^, Sources[I].DataSize);
            finally
              Sources[I].UnlockData(Data, False);
            end
          else
            Exit;
        end;
      finally
        EndRewrite;
      end;
      Result := True;
    finally
      ReallocMem(SilentBuffer, 0);
    end;
  end;
end;

function TWaveStreamAdapter.Mix(const Sources: array of TWaveStreamAdapter): Boolean;
var
  TargetFormat: PWaveFormatEx;
  Buffer: Pointer;
  BufferSize: DWORD;
  RawWaves: PRawWave;
  RawWave: PRawWave;
  I: Integer;
begin
  Result := False;
  if System.Length(Sources) <> 0 then
  begin
    BufferSize := 0;
    TargetFormat := Sources[Low(Sources)].WaveFormat;
    GetMem(RawWaves, SizeOf(TRawWave) * System.Length(Sources));
    try
      RawWave := RawWaves;
      for I := Low(Sources) to High(Sources) do
      begin
        if not Sources[I].SameWaveFormat(TargetFormat) then
          Exit
        else if Sources[I].DataSize > BufferSize then
          BufferSize := Sources[I].DataSize;
        RawWave^.dwSize := Sources[I].DataSize;
        Sources[I].LockData(RawWave^.pData, False);
        Inc(RawWave);
      end;
      GetMem(Buffer, BufferSize);
      try
        if MixWaveAudio(RawWaves, System.Length(Sources), TargetFormat, Buffer, BufferSize) then
        begin
          BeginRewrite(TargetFormat);
          try
            Write(Buffer^, BufferSize);
          finally
            EndRewrite;
          end;
          Result := True;
        end;
      finally
        FreeMem(Buffer);
      end;
    finally
      RawWave := RawWaves;
      for I := Low(Sources) to High(Sources) do
      begin
        Sources[I].UnlockData(RawWave^.pData, False);
        Inc(RawWave);
      end;
      FreeMem(RawWaves);
    end;
  end;
end;

{ TWaveStreamConverter }

destructor TWaveStreamConverter.Destroy;
begin
  ReallocMem(fBufferFormat, 0);
  ReallocMem(SrcBuffer, 0);
  ReallocMem(DstBuffer, 0);
  inherited Destroy;
end;

procedure TWaveStreamConverter.Reset;
begin
  ReallocMem(SrcBuffer, 0);
  SrcBufferSize := 0;
  ReallocMem(DstBuffer, 0);
  DstBufferSize := 0;
  BufferOffset := 0;
  ACMStream := 0;
end;

procedure TWaveStreamConverter.SetBufferFormat(const pWaveFormat: PWaveFormatEx);
begin
  if Assigned(pWaveFormat) then
  begin
    ReallocMem(fBufferFormat, SizeOf(TWaveFormatEx) + pWaveFormat.cbSize);
    CopyMemory(fBufferFormat, pWaveFormat, SizeOf(TWaveFormatEx) + pWaveFormat.cbSize);
  end
  else
    ReallocMem(fBufferFormat, 0);
end;

procedure TWaveStreamConverter.SetBufferFormatPCM(Format: TPCMFormat);
var
  WaveFormat: TWaveFormatEx;
begin
  SetPCMAudioFormatS(@WaveFormat, Format);
  SetBufferFormat(@WaveFormat);
end;

function TWaveStreamConverter.CanRewrite(pWaveFormat: PWaveFormatEx): Boolean;
var
  Dummy: HACMSTREAM;
begin
  Result := not Assigned(fBufferFormat) or (acmStreamOpen(Dummy, 0,
     fBufferFormat, pWaveFormat, nil, 0, 0, ACM_STREAMOPENF_QUERY) = 0);
end;

function TWaveStreamConverter.CanRewritePCM(Format: TPCMFormat): Boolean;
var
  WaveFormatEx: TWaveFormatEx;
begin
  SetPCMAudioFormatS(@WaveFormatEx, Format);
  Result := CanRewrite(@WaveFormatEx);
end;

function TWaveStreamConverter.BeginRewrite(pWaveFormat: PWaveFormatEx): Boolean;
begin
  Result := False;
  if not Assigned(fBufferFormat) or (acmStreamOpen(ACMStream, 0,
     fBufferFormat, pWaveFormat, nil, 0, 0, ACM_STREAMOPENF_NONREALTIME) = 0) then
  begin
    if not inherited BeginRewrite(pWaveFormat) then
    begin
      acmStreamClose(ACMStream, 0);
      ACMStream := 0;
    end
    else
      Result := True;
  end;
end;

function TWaveStreamConverter.EndRewrite: Boolean;
var
  NeededBytes: DWORD;
begin
  if ACMStream <> 0 then
  begin
    if BufferOffset <> 0 then
    begin
      acmStreamSize(ACMStream, BufferOffset, NeededBytes, ACM_STREAMSIZEF_SOURCE);
      if NeededBytes > DstBufferSize then
      begin
        DstBufferSize := NeededBytes;
        ReallocMem(DstBuffer, DstBufferSize);
      end;
      FillChar(ACMHeader, SizeOf(TACMSTREAMHEADER), 0);
      ACMHeader.cbStruct := SizeOf(TACMSTREAMHEADER);
      ACMHeader.pbSrc := SrcBuffer;
      ACMHeader.cbSrcLength := BufferOffset;
      ACMHeader.pbDst := DstBuffer;
      ACMHeader.cbDstLength := NeededBytes;
      acmStreamPrepareHeader(ACMStream, ACMHeader, 0);
      acmStreamConvert(ACMStream, ACMHeader, ACM_STREAMCONVERTF_END);
      acmStreamUnprepareHeader(ACMStream, ACMHeader, 0);
      inherited Write(ACMHeader.pbDst^, ACMHeader.cbDstLengthUsed);
    end;
    acmStreamClose(ACMStream, 0);
    Reset;
  end;
  Result := inherited EndRewrite;
end;

function TWaveStreamConverter.Write(const Buffer; Count: Integer): Integer;
var
  NeededBytes: DWORD;
  Flags: Integer;
begin
  if ACMStream <> 0 then
  begin
    Result := -1;
    Flags := ACM_STREAMCONVERTF_BLOCKALIGN;
    if Position = 0 then
      Flags := Flags or ACM_STREAMCONVERTF_START;
    if BufferOffset > 0 then
    begin
      if BufferOffset + DWORD(Count) > SrcBufferSize then
      begin
        SrcBufferSize := BufferOffset + DWORD(Count);
        ReallocMem(SrcBuffer, SrcBufferSize);
      end;
      CopyMemory(Pointer(DWORD(SrcBuffer) + BufferOffset), @Buffer, Count);
    end;
    acmStreamSize(ACMStream, DWORD(Count) + BufferOffset, NeededBytes, ACM_STREAMSIZEF_SOURCE);
    if NeededBytes > DstBufferSize then
    begin
      DstBufferSize := NeededBytes;
      ReallocMem(DstBuffer, DstBufferSize);
    end;
    FillChar(ACMHeader, SizeOf(TACMSTREAMHEADER), 0);
    ACMHeader.cbStruct := SizeOf(TACMSTREAMHEADER);
    if BufferOffset <> 0 then
      ACMHeader.pbSrc := SrcBuffer
    else
      ACMHeader.pbSrc := @Buffer;
    ACMHeader.cbSrcLength := BufferOffset + DWORD(Count);
    ACMHeader.pbDst := DstBuffer;
    ACMHeader.cbDstLength := NeededBytes;
    acmStreamPrepareHeader(ACMStream, ACMHeader, 0);
    acmStreamConvert(ACMStream, ACMHeader, Flags);
    acmStreamUnprepareHeader(ACMStream, ACMHeader, 0);
    if inherited Write(ACMHeader.pbDst^, ACMHeader.cbDstLengthUsed) >= 0 then
      Result := Count;
    BufferOffset := 0;
    if ACMHeader.cbSrcLength <> ACMHeader.cbSrcLengthUsed then
    begin
      BufferOffset := ACMHeader.cbSrcLength - ACMHeader.cbSrcLengthUsed;
      if BufferOffset > SrcBufferSize then
      begin
        SrcBufferSize := BufferOffset;
        ReallocMem(SrcBuffer, SrcBufferSize);
      end;
      CopyMemory(SrcBuffer, Pointer(DWORD(ACMHeader.pbSrc) +
        ACMHeader.cbSrcLengthUsed), BufferOffset);
    end;
  end
  else
    Result := inherited Write(Buffer, Count);
end;

function TWaveStreamConverter.CanRead: Boolean;
var
  Dummy: HACMSTREAM;
begin
  Result := Valid and (not Assigned(fBufferFormat) or
    (acmStreamOpen(Dummy, 0, fWaveFormat, fBufferFormat, nil, 0, 0,
     ACM_STREAMOPENF_QUERY) = 0));
end;

function TWaveStreamConverter.BeginRead: Boolean;
begin
  Result := False;
  if Valid and (not Assigned(fBufferFormat) or
    (acmStreamOpen(ACMStream, 0, fWaveFormat, fBufferFormat, nil, 0, 0,
     ACM_STREAMOPENF_NONREALTIME) = 0)) then
  begin
    if not inherited BeginRead then
    begin
      acmStreamClose(ACMStream, 0);
      ACMStream := 0;
    end
    else
      Result := True;
  end;
end;

function TWaveStreamConverter.EndRead: Boolean;
begin
  if ACMStream <> 0 then
  begin
    acmStreamClose(ACMStream, 0);
    Reset;
  end;
  Result := inherited EndRead;
end;

function TWaveStreamConverter.Read(var Buffer; Count: Integer): Integer;
var
  NeededBytes: DWORD;
  ReadBytes: Integer;
  Flags: Integer;
begin
  if ACMStream <> 0 then
  begin
    Result := -1;
    Flags := 0;
    if Position = 0 then
      Flags := Flags or ACM_STREAMCONVERTF_START;
    acmStreamSize(ACMStream, Count, NeededBytes, ACM_STREAMSIZEF_DESTINATION);
    if NeededBytes > SrcBufferSize then
    begin
      SrcBufferSize := NeededBytes;
      ReallocMem(SrcBuffer, SrcBufferSize);
    end;
    ReadBytes := inherited Read(Pointer(DWORD(SrcBuffer) + BufferOffset)^, NeededBytes - BufferOffset);
    if ReadBytes >= 0 then
    begin
      if DWORD(ReadBytes) + BufferOffset > 0 then
      begin
        if DWORD(ReadBytes) < NeededBytes - BufferOffset then
          Flags := Flags or ACM_STREAMCONVERTF_END
        else
          Flags := Flags or ACM_STREAMCONVERTF_BLOCKALIGN;
        FillChar(ACMHeader, SizeOf(TACMSTREAMHEADER), 0);
        ACMHeader.cbStruct := SizeOf(TACMSTREAMHEADER);
        ACMHeader.pbSrc := SrcBuffer;
        ACMHeader.cbSrcLength := BufferOffset + DWORD(ReadBytes);
        ACMHeader.pbDst := @Buffer;
        ACMHeader.cbDstLength := Count;
        acmStreamPrepareHeader(ACMStream, ACMHeader, 0);
        acmStreamConvert(ACMStream, ACMHeader, Flags);
        acmStreamUnprepareHeader(ACMStream, ACMHeader, 0);
        BufferOffset := 0;
        if ACMHeader.cbSrcLength <> ACMHeader.cbSrcLengthUsed then
        begin
          BufferOffset := ACMHeader.cbSrcLength - ACMHeader.cbSrcLengthUsed;
          CopyMemory(SrcBuffer, Pointer(DWORD(SrcBuffer) +
            ACMHeader.cbSrcLengthUsed), BufferOffset);
        end;
        Result := ACMHeader.cbDstLengthUsed;
      end
      else
        Result := 0;
    end;
  end
  else
    Result := inherited Read(Buffer, Count);
end;

{ TWave }

constructor TWave.Create;
begin
  inherited Create(TMemoryStream.Create, soOwned);
end;

{ TWaveFile }

constructor TWaveFile.Create(const FileName: String; Mode: Word);
begin
  inherited Create(TFileStream.Create(FileName, Mode), soOwned);
end;

{ TWaveConverter }

constructor TWaveConverter.Create;
begin
  inherited Create(TMemoryStream.Create, soOwned);
end;

{ TWaveFileConverter }

constructor TWaveFileConverter.Create(const FileName: String; Mode: Word);
begin
  inherited Create(TFileStream.Create(FileName, Mode), soOwned);
end;

{ TWaveStorage }

constructor TWaveStorage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fWave := TWave.Create;
end;

destructor TWaveStorage.Destroy;
begin
  fWave.Free;
  inherited Destroy;
end;

procedure TWaveStorage.SetWave(Value: TWave);
begin
  if Wave <> Value then
  begin
    if Assigned(Value) then
    begin
      Value.Stream.Position := 0;
      Wave.LoadFromStream(Value.Stream);
    end
    else
      Wave.Clear;
  end;
end;

procedure TWaveStorage.ReadData(Stream: TStream);
begin
  Wave.LoadFromStream(Stream);
end;

procedure TWaveStorage.WriteData(Stream: TStream);
begin
  Wave.SaveToStream(Stream);
end;

procedure TWaveStorage.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Data', ReadData, WriteData, not Wave.Empty);
end;

procedure TWaveStorage.Assign(Source: TPersistent);
begin
  if Source is TWaveStorage then
    Wave := TWaveStorage(Source).Wave
  else if Source is TWaveItem then
    Wave := TWaveItem(Source).Wave
  else
    inherited Assign(Source);
end;

function TWaveStorage.GetWaveStream(Index: Integer): TStream;
begin
  Result := Wave.Stream;
end;

function TWaveStorage.Equals(Another: TCustomWaveStorage): Boolean;
begin
  if Another is TWaveStorage then
    Result := Wave.Equals(TWaveStorage(Another).Wave)
  else
    Result := False;
end;

{ TWaveItem }

constructor TWaveItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  fWave := TWave.Create;
end;

destructor TWaveItem.Destroy;
begin
  fWave.Free;
  inherited Destroy;
end;

procedure TWaveItem.SetWave(Value: TWave);
begin
  if Wave <> Value then
  begin
    if Assigned(Value) then
    begin
      Value.Stream.Position := 0;
      Wave.LoadFromStream(Value.Stream);
    end
    else
      Wave.Clear;
  end;
end;

procedure TWaveItem.ReadData(Stream: TStream);
begin
  Wave.LoadFromStream(Stream);
end;

procedure TWaveItem.WriteData(Stream: TStream);
begin
  Wave.SaveToStream(Stream);
end;

procedure TWaveItem.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Data', ReadData, WriteData, not Wave.Empty);
end;

procedure TWaveItem.Assign(Source: TPersistent);
begin
  if Source is TWaveItem then
  begin
    Wave := TWaveItem(Source).Wave;
    Name := TWaveItem(Source).Name;
    Tag := TWaveItem(Source).Tag;
  end
  else if Source is TWaveStorage then
    Wave := TWaveStorage(Source).Wave
  else
    inherited Assign(Source);
end;

function TWaveItem.GetDisplayName: String;
var
  WaveInfo: String;
begin
  if (Wave <> nil) and (Wave.Stream.Size <> 0) then
  begin
    if Wave.Valid then
      WaveInfo := Wave.AudioFormat + ', ' +
                  IntToStr(Wave.BitRate) + ' kbps, ' +
                  MS2Str(Wave.Length, msAh) + ' sec.'
    else
      WaveInfo := 'Invalid Content';
  end
  else
    WaveInfo := 'Empty';
  Result := Name + ' (' + WaveInfo + ')';
end;

{ TWaveItems }

constructor TWaveItems.Create(AOwner: TPersistent; ItemClass: TWaveItemClass);
begin
  inherited Create(ItemClass);
  fOwner := AOwner;
end;

function TWaveItems.GetOwner: TPersistent;
begin
  Result := fOwner;
end;

function TWaveItems.Add: TWaveItem;
begin
  Result := TWaveItem(inherited Add);
end;

{$IFDEF COMPILER4_UP}
function TWaveItems.Insert(Index: Integer): TWaveItem;
begin
  Result := TWaveItem(inherited Insert(Index));
end;
{$ENDIF}

function TWaveItems.GetItem(Index: Integer): TWaveItem;
begin
  Result := TWaveItem(inherited Items[Index]);
end;

procedure TWaveItems.SetItem(Index: Integer; Value: TWaveItem);
begin
  inherited Items[Index] := Value;
end;

{ TWaveCollection }

constructor TWaveCollection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fWaves := TWaveItems.Create(Self, TWaveItem);
end;

destructor TWaveCollection.Destroy;
begin
  fWaves.Free;
  inherited Destroy;
end;

procedure TWaveCollection.SetWaves(const Value: TWaveItems);
begin
  if Waves <> Value then
    Waves.Assign(Value);
end;

function TWaveCollection.GetWaveStream(Index: Integer): TStream;
begin
  Result := Waves[Index].Wave.Stream;
end;

function TWaveCollection.Equals(Another: TCustomWaveStorage): Boolean;
var
  I: Integer;
begin
  if (Another is TWaveCollection) and
     (Waves.Count = TWaveCollection(Another).Waves.Count) then
  begin
    Result := True;
    for I := 0 to Waves.Count - 1 do
      if not Waves[I].Wave.Equals(TWaveCollection(Another).Waves[I].Wave) then
      begin
        Result := False;
        Break;
      end;
  end
  else
    Result := False;
end;

function TWaveCollection.ExportWaveNames(const List: TStrings): Integer;
var
  Index: Integer;
begin
  for Index := 0 to Waves.Count - 1 do
    List.Add(Waves[Index].Name);
  Result := Waves.Count;
end;

function TWaveCollection.IndexOfName(const AName: String): Integer;
var
  Index: Integer;
begin
  Result := -1;
  for Index := 0 to Waves.Count - 1 do
    if CompareText(Waves[Index].Name, AName) = 0 then
    begin
      Result := Index;
      Break;
    end;
end;

end.
