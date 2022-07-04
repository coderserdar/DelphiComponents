unit AudTools;

{ TODO : BWF-Header auslesen und schreiben }

{ Komponente TAudioTools  Version 2.07
  (C)opyright 2005 Simon Reinhardt <reinhardt@picsoft.de>

  Für Pegelmanipulationen in RIFF-Audiodateien (*.wav) }

// {$Define BigEndian}

{$I SRDefine.inc}

interface

uses Classes;

type
  TString4 = string[4];

const
  MaxChunks  = 9;
  RIFFChunks : array [0..MaxChunks] of TString4 =
   ('RIFF', 'fmt ', 'data', 'bext', 'mext', 'fact', 'qlty', 'levl', 'link', 'axml');

type
  T24BitSample        = array [0..2] of byte;
  TByteBuffer         = array [0..5] of byte;
  TCheckOption        = (coNone, coLength);
  TErrorCode          = (errNone, errNoOperation, errClip, errFileExists, errRead, errWrite);
  TFileBuffer         = array [0..5999] of byte;
  TOperation          = (opChangeBalance, opChangeLevel, opCopyAndAnalyzeFile, opCopyFile,
                         opGetMaxSampleValue, opNormalize, opSetLevel);
  TOperationDoneEvent = procedure (Sender: TObject; Operation: TOperation; ErrorCode:TErrorCode) of object;
  TProgressEvent      = procedure (Sender: TObject; Progress: byte) of object;
  TSampleBuffer       = array [0..2] of smallint;

  TAudioTools = class(TComponent)
  private
    // Protected Variables
    FBackupFileName : string;
    FBitsPerSample,
    FBufferSize,
    FBytesPerSample : byte;
    FChunks         : TStringList;
    FCompression    : word;
    FSize,
    FDataOffset,
    FDataSize       : Int64;
    FErrorCode      : TErrorCode;
    FFileName       : string;
    FLength         : double;
    FNumChannels    : byte;
    FNumSamples,
    FSampleRate     : integer;
    FTargetName     : string;
    FValidFormat    : boolean;

    FOnOperationDone : TOperationDoneEvent;
    FOnProgress      : TProgressEvent;

  protected
    // Protected Methods
    function ApplyNewLevelToChannel(const AChannel:byte;const Ratio:double):boolean;
    {$IFDEF SR_Delphi4_Up}
    function ApplyNewLevelToFile(const Ratio:double):boolean; overload;
    function ApplyNewLevelToFile(const Ratio:double;const FadeInTime,FadeOutTime:word):boolean; overload;
    {$ELSE}
    function ApplyNewLevelToFile(const Ratio:double):boolean;
    {$ENDIF}
    function ChunkExists(const AChunk:TString4):boolean;
    function EncodeLittleEndian(const AValues:TSampleBuffer):TByteBuffer;
    {$IFDEF SR_Delphi5_Up}
    function Get24BitValue(const Value:T24BitSample;const Signed:boolean = true): integer; virtual;
    {$ELSE}
    function Get24BitValue(const Value:T24BitSample;const Signed:boolean): integer; virtual;
    {$ENDIF}
    function GetCompressionName:string;
    function GetMaxExistingSampleValue(var MaxPossibleValue:integer):integer;
    function GetSampleValue(Buf:TByteBuffer;const ChannelNum:byte): integer;
    function ReadWaveFileHeader:boolean;
    procedure SetFileName(const NewName:string); virtual;

  public
    // Public Methods
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function ChangeBalance(const Offset:double):boolean; virtual;
    function ChangeLevel(const Offset:double):boolean; virtual;
    function CheckHeader(const CheckOption:TCheckOption):boolean; virtual;
    function CopyFile(const FailIfExists:boolean):boolean; virtual;
    function CopyAndAnalyzeFile(var MaximumLevel:double;
                                const FailIfExists:boolean):boolean; virtual;
    function GetErrorMessage:string; virtual;
    function GetMaximumLevel:double; virtual;
    function GetMaxPossibleSampleValue:integer; virtual;
    function Normalize:double; virtual;
    {$IFDEF SR_Delphi4_Up}
    function SetLevel(const Level:double):double; overload; virtual;
    function SetLevel(const Level:double;const FadeInTime,FadeOutTime:word):double; overload; virtual;
    {$ELSE}
    function SetLevel(const Level:double):double; virtual;
    {$ENDIF}

    // Public Properties
    property BackupFileName: string read FBackupFileName;
    property BitsPerSample: byte read FBitsPerSample;
    property BytesPerSample: byte read FBytesPerSample;
    property Chunks: TStringList read FChunks;
    property CompressionName: string read GetCompressionName;
    property DataSize: Int64 read FDataSize;
    property ErrorCode: TErrorCode read FErrorCode;
    property Length: double read FLength;
    property NumChannels: byte read FNumChannels;
    property NumSamples: integer read FNumSamples;
    property SampleRate: integer read FSampleRate;
    property ValidFormat: boolean read FValidFormat;

  published
    // Published Properties
    property FileName: string read FFileName write SetFileName;
    property TargetName: string read FTargetName write FTargetName;
    property OnOperationDone: TOperationDoneEvent read FOnOperationDone write FOnOperationDone;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;

  end;

procedure Register;

implementation

{$IFDEF SR_Delphi2}
{$R *.dcr}
{$ENDIF}

uses SysUtils, MMSystem, Math{$IFNDEF SR_Delphi5_Up}, SRUtils{$ENDIF};

const
  RIFF_SIGNATURE = 'RIFF';
  WAVE_SIGNATURE = 'WAVE';
  DATA_SIGNATURE = 'DATA';
  FMT_SIGNATURE  = 'FMT ';
  BackupExt      = '.bak';
  FHeaderSize    = 44;     // Minimale Länge des Wave-Headers

type
  TSignatureType = array [1..4] of char;

  TRIFFHeader = packed record
    Signature : TSignatureType;
    Size      : cardinal;
    Format    : TSignatureType;
  end;

  TFMTHeader = packed record
    Signature      : TSignatureType;
    Size           : cardinal;
    Compression,
    NumChannels    : word;
    SampleRate,
    BytesPerSec    : cardinal;
    BytesPerSample,
    BitsPerSample  : word;
  end;

function CopyBufferFrom(const Source:TFileBuffer;const APos,ACount:integer):TByteBuffer;
var i : integer;
begin
  for i:=0 to ACount-1 do
    Result[i]:=Source[APos+i];
end; // CopyBufferFrom

procedure CopyBufferTo(const Buf:TByteBuffer;var Target:TFileBuffer;
                       const APos,ACount:integer);
var i : integer;
begin
  for i:=0 to ACount-1 do
    Target[APos+i]:=Buf[i];
end; // CopyBufferTo

function IsRIFFChunk(const AText:TString4):boolean;
var i :integer;
begin
  Result:=false;
  i:=0;
  while (i<=MaxChunks) and not Result do begin
    if LowerCase(AText)=LowerCase(RIFFChunks[i]) then
      Result:=true
    else
      inc(i);
  end;
end; // IsRIFFChunk

function LevelToRatio(const ALevel:double):double;
begin
  // Umrechnung eines Pegelwertes in einen Quotienten
  Result:=Power(10, ALevel/20);
end; // LevelToRatio

function RatioToLevel(const ARatio:double):double;
begin
  // Umrechnung eines Quotienten in einen Pegelwert
  Result:=Log10(ARatio)*20;
end; // RatioToLevel

{ Komponente TAudioTools }

constructor TAudioTools.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FChunks:=TStringList.Create;
end;

destructor TAudioTools.Destroy;
begin
  if assigned(FChunks) then
    FChunks.Free;

  inherited Destroy;
end; {Destroy}

function TAudioTools.ApplyNewLevelToChannel(const AChannel:byte;const Ratio:double):boolean;
var ChannelNum,
    SampleNum,
    AllSampleNum,
    NewValue,
    MaxValue,
    BufSize,
    ACount,
    BytesRead      : integer;
    AProgress,
    OldProgress    : byte;
    Clip           : boolean;
    SrcName,
    TgtName        : string;
    OldBuffer,
    NewBuffer      : TFileBuffer;
    ASamples       : TSampleBuffer;
    OldFile,
    NewFile        : TFileStream;
    Buf            : TByteBuffer;
begin
  // Nur den Pegel des Kanals "ChannelNum" um den Faktor "Ratio" ändern
  Result:=false;
  FErrorCode:=errNoOperation;
  MaxValue:=GetMaxPossibleSampleValue;
  if (MaxValue<>0) and (Ratio<>0) then begin
    // Zieldatei ermitteln
    if FTargetName<>'' then begin
      // Ziel ist "FTargetFile":
      SrcName:=FFileName;
      TgtName:=FTargetName;
    end
    else begin
      // Ziel ist Originaldatei:
      // Originaldatei als Backupfile umbenennen
      FBackupFileName:=ChangeFileExt(FFileName, BackupExt);
      if FileExists(FBackupFileName) then
        if not DeleteFile(FBackupFileName) then
          FErrorCode:=errWrite;
      if not RenameFile(FFileName, FBackupFileName) then
        FErrorCode:=errWrite;
      SrcName:=FBackupFileName;
      TgtName:=FFileName;
    end;
    if FErrorCode=errNoOperation then begin
      OldFile:=TFileStream.Create(SrcName, fmOpenRead or fmShareDenyWrite);
      try
        NewFile:=TFileStream.Create(TgtName, fmCreate or fmShareDenyWrite);
        try
          // Header kopieren
          NewFile.CopyFrom(OldFile, FDataOffset);
          Clip:=false;
          AProgress:=0;
          AllSampleNum:=0;
          BufSize:=SizeOf(OldBuffer);
          repeat
            // Puffer einlesen
            BytesRead:=OldFile.Read(OldBuffer, BufSize);
            ACount:=BytesRead div FBytesPerSample;
            // Einzelne Samples aus dem Puffer lesen
            for SampleNum:=0 to ACount-1 do begin
              // Alten Samplewert auslesen
              Buf:=CopyBufferFrom(OldBuffer, SampleNum*FBytesPerSample, FBytesPerSample);
              // SampleBuffer initialisieren
              for ChannelNum:=0 to 2 do
                ASamples[ChannelNum]:=0;
              for ChannelNum:=0 to FNumChannels-1 do begin
                // Alle Kanäle auslesen
                ASamples[ChannelNum]:=GetSampleValue(Buf, ChannelNum);
                if ChannelNum=AChannel then begin
                  // Neuen Samplewert berechnen
                  NewValue:=round(ASamples[ChannelNum]*Ratio);
                  if NewValue<=MaxValue then
                    ASamples[ChannelNum]:=NewValue
                  else begin
                    // Neuer Samplewert ist zu groß!
                    ASamples[ChannelNum]:=MaxValue;
                    Clip:=true;
                  end;
                end;
              end;
              // Umwandeln in "Little Endian"-Buffer
              Buf:=EncodeLittleEndian(ASamples);
              CopyBufferTo(Buf, NewBuffer, SampleNum*FBytesPerSample, FBytesPerSample);
            end;
            // Umgerechneten Puffer in die neue Datei schreiben
            NewFile.Write(NewBuffer, BytesRead);
            // Fortschritt der Operation berechnen
            AllSampleNum:=AllSampleNum+ACount;
            if assigned(FOnProgress) then begin
              OldProgress:=AProgress;
              AProgress:=round((100/FNumSamples)*AllSampleNum);
              if OldProgress<>AProgress then
                FOnProgress(Self, AProgress);
            end;
          until BytesRead<BufSize;
          if Clip then
            FErrorCode:=errClip
          else
            FErrorCode:=errNone;
        except
          FErrorCode:=errWrite;
        end;
        Result:=FErrorCode=errNone;
        FreeAndNil(NewFile);
      finally
        FreeAndNil(OldFile);
      end;
    end;
  end;
end; // ApplyNewLevelToChannel

function TAudioTools.ApplyNewLevelToFile(const Ratio:double):boolean;
var ChannelNum,
    SampleNum,
    AllSampleNum,
    NewValue,
    MaxValue,
    BufSize,
    ACount,
    BytesRead      : integer;
    AProgress,
    OldProgress    : byte;
    Clip           : boolean;
    SrcName,
    TgtName        : string;
    OldBuffer,
    NewBuffer      : TFileBuffer;
    ASamples       : TSampleBuffer;
    OldFile,
    NewFile        : TFileStream;
    Buf            : TByteBuffer;
begin
  // Den Pegel der Datei um den Faktor "Ratio" ändern
  Result:=false;
  FErrorCode:=errNoOperation;
  MaxValue:=GetMaxPossibleSampleValue;
  if (MaxValue<>0) and (Ratio<>0) then begin
    // Zieldatei ermitteln
    if FTargetName<>'' then begin
      // Ziel ist "FTargetFile":
      SrcName:=FFileName;
      TgtName:=FTargetName;
    end
    else begin
      // Ziel ist Originaldatei:
      // Originaldatei als Backupfile umbenennen
      FBackupFileName:=ChangeFileExt(FFileName, BackupExt);
      if FileExists(FBackupFileName) then
        if not DeleteFile(FBackupFileName) then
          FErrorCode:=errWrite;
      if not RenameFile(FFileName, FBackupFileName) then
        FErrorCode:=errWrite;
      SrcName:=FBackupFileName;
      TgtName:=FFileName;
    end;
    if FErrorCode=errNoOperation then begin
      OldFile:=TFileStream.Create(SrcName, fmOpenRead or fmShareDenyWrite);
      try
        NewFile:=TFileStream.Create(TgtName, fmCreate or fmShareDenyWrite);
        try
          // Header kopieren
          NewFile.CopyFrom(OldFile, FDataOffset);
          Clip:=false;
          AProgress:=0;
          AllSampleNum:=0;
          BufSize:=SizeOf(OldBuffer);
          repeat
            // Puffer einlesen
            BytesRead:=OldFile.Read(OldBuffer, BufSize);
            ACount:=BytesRead div FBytesPerSample;
            // Einzelne Samples aus dem Puffer lesen
            for SampleNum:=0 to ACount-1 do begin
              // Alten Samplewert auslesen
              Buf:=CopyBufferFrom(OldBuffer, SampleNum*FBytesPerSample, FBytesPerSample);
              // SampleBuffer initialisieren
              for ChannelNum:=0 to 2 do
                ASamples[ChannelNum]:=0;
              for ChannelNum:=0 to FNumChannels-1 do begin
                // Alle Kanäle auslesen
                ASamples[ChannelNum]:=GetSampleValue(Buf, ChannelNum);
                // Neuen Samplewert berechnen
                NewValue:=round(ASamples[ChannelNum]*Ratio);
                if NewValue<=MaxValue then
                  ASamples[ChannelNum]:=NewValue
                else begin
                  // Neuer Samplewert ist zu groß!
                  ASamples[ChannelNum]:=MaxValue;
                  Clip:=true;
                end;
              end;
              // Umwandeln in "Little Endian"-Buffer
              Buf:=EncodeLittleEndian(ASamples);
              CopyBufferTo(Buf, NewBuffer, SampleNum*FBytesPerSample, FBytesPerSample);
            end;
            // Umgerechneten Puffer in die neue Datei schreiben
            NewFile.Write(NewBuffer, BytesRead);
            // Fortschritt der Operation berechnen
            AllSampleNum:=AllSampleNum+ACount;
            if assigned(FOnProgress) then begin
              OldProgress:=AProgress;
              AProgress:=round((100/FNumSamples)*AllSampleNum);
              if OldProgress<>AProgress then
                FOnProgress(Self, AProgress);
            end;
          until BytesRead<BufSize;
          if Clip then
            FErrorCode:=errClip
          else
            FErrorCode:=errNone;
        except
          FErrorCode:=errWrite;
        end;
        Result:=FErrorCode=errNone;
        FreeAndNil(NewFile);
      finally
        FreeAndNil(OldFile);
      end;
    end;
  end;
end; // ApplyNewLevelToFile

{$IFDEF SR_Delphi4_Up}
function TAudioTools.ApplyNewLevelToFile(const Ratio:double;const FadeInTime,FadeOutTime:word):boolean;
var ChannelNum,
    SampleNum,
    AllSampleNum,
    CurrSampleNum,
    FirstFLSampleNum,
    LastFLSampleNum,
    NewValue,
    MaxValue,
    BufSize,
    ACount,
    BytesRead       : integer;
    AProgress,
    OldProgress     : byte;
    Clip            : boolean;
    Factor          : double;
    SrcName,
    TgtName         : string;
    OldBuffer,
    NewBuffer       : TFileBuffer;
    ASamples        : TSampleBuffer;
    OldFile,
    NewFile         : TFileStream;
    Buf             : TByteBuffer;
begin
  // Den Pegel der Datei um den Faktor "Ratio" ändern
  Result:=false;
  FErrorCode:=errNoOperation;
  MaxValue:=GetMaxPossibleSampleValue;
  if (MaxValue<>0) and (Ratio<>0) then begin
    // Zieldatei ermitteln
    if FTargetName<>'' then begin
      // Ziel ist "FTargetFile":
      SrcName:=FFileName;
      TgtName:=FTargetName;
    end
    else begin
      // Ziel ist Originaldatei:
      // Originaldatei als Backupfile umbenennen
      FBackupFileName:=ChangeFileExt(FFileName, BackupExt);
      if FileExists(FBackupFileName) then
        if not DeleteFile(FBackupFileName) then
          FErrorCode:=errWrite;
      if not RenameFile(FFileName, FBackupFileName) then
        FErrorCode:=errWrite;
      SrcName:=FBackupFileName;
      TgtName:=FFileName;
    end;
    if FErrorCode=errNoOperation then begin
      OldFile:=TFileStream.Create(SrcName, fmOpenRead or fmShareDenyWrite);
      try
        NewFile:=TFileStream.Create(TgtName, fmCreate or fmShareDenyWrite);
        try
          // Sample-Nummern für FadeIn-Ende und FadeOut-Beginn berechnen
          FirstFLSampleNum:=FSampleRate*FadeInTime div 1000;
          LastFLSampleNum:=FNumSamples-(FSampleRate*FadeOutTime div 1000);
          // Header kopieren
          NewFile.CopyFrom(OldFile, FDataOffset);
          Clip:=false;
          AProgress:=0;
          AllSampleNum:=0;
          BufSize:=SizeOf(OldBuffer);
          repeat
            // Puffer einlesen
            BytesRead:=OldFile.Read(OldBuffer, BufSize);
            ACount:=BytesRead div FBytesPerSample;
            // Einzelne Samples aus dem Puffer lesen
            for SampleNum:=0 to ACount-1 do begin
              CurrSampleNum:=AllSampleNum+SampleNum;
              // Alten Samplewert auslesen
              Buf:=CopyBufferFrom(OldBuffer, SampleNum*FBytesPerSample, FBytesPerSample);
              // SampleBuffer initialisieren
              for ChannelNum:=0 to 2 do
                ASamples[ChannelNum]:=0;
              for ChannelNum:=0 to FNumChannels-1 do begin
                // Alle Kanäle auslesen
                ASamples[ChannelNum]:=GetSampleValue(Buf, ChannelNum);
                // Neuen Samplewert berechnen
                if CurrSampleNum<FirstFLSampleNum then begin
                  // FadeIn: Samplewert für aktuelle Fade-Position berechnen
                  if CurrSampleNum=0 then
                    Factor:=0
                  else
                    Factor:=CurrSampleNum/FirstFLSampleNum;
                  NewValue:=round(ASamples[ChannelNum]*Factor*Ratio);
                end
                else begin
                  if CurrSampleNum>LastFLSampleNum then begin
                    // FadeOut: Samplewert für aktuelle Fade-Position berechnen
                    if CurrSampleNum=0 then
                      Factor:=0
                    else
                      Factor:=(FNumSamples-CurrSampleNum)/(FNumSamples-LastFLSampleNum);
                    NewValue:=round(ASamples[ChannelNum]*Factor*Ratio);
                  end
                  else
                    // Zwischen den Fades
                    NewValue:=round(ASamples[ChannelNum]*Ratio);
                end;
                if NewValue<=MaxValue then
                  ASamples[ChannelNum]:=NewValue
                else begin
                  // Neuer Samplewert ist zu groß!
                  ASamples[ChannelNum]:=MaxValue;
                  Clip:=true;
                end;
              end;
              // Umwandeln in "Little Endian"-Buffer
              Buf:=EncodeLittleEndian(ASamples);
              CopyBufferTo(Buf, NewBuffer, SampleNum*FBytesPerSample, FBytesPerSample);
            end;
            // Umgerechneten Puffer in die neue Datei schreiben
            NewFile.Write(NewBuffer, BytesRead);
            // Fortschritt der Operation berechnen
            AllSampleNum:=AllSampleNum+ACount;
            if assigned(FOnProgress) then begin
              OldProgress:=AProgress;
              AProgress:=round((100/FNumSamples)*AllSampleNum);
              if OldProgress<>AProgress then
                FOnProgress(Self, AProgress);
            end;
          until BytesRead<BufSize;
          if Clip then
            FErrorCode:=errClip
          else
            FErrorCode:=errNone;
        except
          FErrorCode:=errWrite;
        end;
        Result:=FErrorCode=errNone;
        FreeAndNil(NewFile);
      finally
        FreeAndNil(OldFile);
      end;
    end;
  end;
end; // ApplyNewLevelToFile
{$ENDIF}

function TAudioTools.ChangeBalance(const Offset:double):boolean;
var Ratio : double;
    OpRes : boolean;
begin
  // Balance einer Stereo-Datei um "Offset" (in dB) ändern
  // Offset < 0: Nach Links
  // Offset > 0: Nach Rechts
  Result:=false;
  // Pegeländerungen funktionieren nur mit Mono- und Stereofiles
  // mit 8- oder 16-Bit-Auflösung!
  if (FNumChannels=2) and (FBytesPerSample<=4) then begin
    // Verstärkungsfaktor aus Offset berechnen
    Ratio:=LevelToRatio(-abs(Offset));
    if Ratio<>0 then begin
      // Neuen Pegel berechnen
      if Offset<0 then
        // Balance nach Links: Pegel für Kanal 0 (Rechts) um "Ratio" absenken
        OpRes:=ApplyNewLevelToChannel(0, Ratio)
      else
        // Balance nach Rechts: Pegel für Kanal 1 (Links) um "Ratio" absenken
        OpRes:=ApplyNewLevelToChannel(1, Ratio);
      if OpRes then
        Result:=true;
      if assigned(FOnOperationDone) then
        FOnOperationDone(Self, opChangeBalance, FErrorCode);
    end;
  end;
end; // ChangeLevel

function TAudioTools.ChangeLevel(const Offset:double):boolean;
var Ratio : double;
begin
  // Pegel der Datei um "Offset" (in dB) ändern
  Result:=false;
  // Pegeländerungen funktionieren nur mit Mono- und Stereofiles
  // mit 8- oder 16-Bit-Auflösung!
  if FBytesPerSample<=4 then begin
    // Verstärkungsfaktor aus Offset berechnen
    Ratio:=LevelToRatio(Offset);
    if Ratio<>0 then begin
      // Neuen Pegel berechnen
      if ApplyNewLevelToFile(Ratio) then
        Result:=true;
      if assigned(FOnOperationDone) then
        FOnOperationDone(Self, opChangeLevel, FErrorCode);
    end;
  end;
end; // ChangeLevel

function TAudioTools.CheckHeader(const CheckOption:TCheckOption):boolean;
var ACount  : integer;
    ASize   : Cardinal;
    ALength : Double;
begin
  Result:=true;
  if CheckOption=coLength then begin
    // Stimmt die aus "FDataSize" berechnete Länge mit der tatsächlichen Länge überein?
    ASize:=FSize-FDataOffset;
    ACount:=integer(ASize) div FBytesPerSample;
    ALength:=ACount/FSampleRate;
    if ALength<>FLength then
      Result:=false;
  end;
end; // CheckHeader

function TAudioTools.CopyFile(const FailIfExists:boolean):boolean;
var NewFile,
    OldFile     : TFileStream;
    FileBuffer  : TFileBuffer;
    AProgress,
    OldProgress : byte;
    BufSize,
    BytesRead,
    FFileSize   : integer;
begin
  // Die Datei "FileName" nach "TargetFileName" kopieren
  FErrorCode:=errNoOperation;
  if (FTargetName<>'') and (FTargetName<>FFileName) then begin
    OldFile:=TFileStream.Create(FFileName, fmOpenRead or fmShareDenyWrite);
    try
      if FileExists(FTargetName) then begin
        // Datei "TargetFileName" existiert bereits
        if FailIfExists then
          // Fehler
          FErrorCode:=errFileExists
        else
          // Existierende Datei löschen
          if not DeleteFile(FTargetName) then
            FErrorCode:=errWrite;
      end;
      if FErrorCode=errNoOperation then begin
        FFileSize:=OldFile.Size;
        NewFile:=TFileStream.Create(FTargetName, fmCreate or fmShareDenyWrite);
        try
          // Header kopieren
          NewFile.CopyFrom(OldFile, FDataOffset);
          AProgress:=0;
          BufSize:=SizeOf(FileBuffer);
          repeat
            // Puffer einlesen
            BytesRead:=OldFile.Read(FileBuffer, BufSize);
            // Puffer in die neue Datei schreiben
            NewFile.Write(FileBuffer, BytesRead);
            // Fortschritt der Operation berechnen
            if assigned(FOnProgress) then begin
              OldProgress:=AProgress;
              AProgress:=round((100/FFileSize)*OldFile.Position);
              if OldProgress<>AProgress then
                FOnProgress(Self, AProgress);
            end;
          until BytesRead<BufSize;
          FErrorCode:=errNone;
        except
          FErrorCode:=errWrite;
        end;
        FreeAndNil(NewFile);
        if assigned(FOnOperationDone) then
          FOnOperationDone(Self, opCopyFile, FErrorCode);
      end;
    finally
      FreeAndNil(OldFile);
    end;
  end;
  Result:=FErrorCode=errNone;
end; // CopyFile

function TAudioTools.CopyAndAnalyzeFile(var MaximumLevel:double;
                                        const FailIfExists:boolean):boolean;
var NewFile,
    OldFile          : TFileStream;
    Buf              : TByteBuffer;
    FileBuffer       : TFileBuffer;
    AProgress,
    OldProgress      : byte;
    AllSampleNum,
    SampleNum,
    ChannelNum,
    ASample,
    BufSize,
    BytesRead,
    ACount,
    MaxPossibleValue,
    MaxExistingValue : integer;
    Ratio            : double;
begin
  // Die Datei "FileName" nach "TargetFileName" kopieren und dabei den maximalen Pegel ermitteln
  FErrorCode:=errNoOperation;
  if (FTargetName<>'') and (FTargetName<>FFileName) then begin
    OldFile:=TFileStream.Create(FFileName, fmOpenRead or fmShareDenyWrite);
    try
      if FileExists(FTargetName) then begin
        // Datei "TargetFileName" existiert bereits
        if FailIfExists then
          // Fehler
          FErrorCode:=errFileExists
        else
          // Existierende Datei löschen
          if not DeleteFile(FTargetName) then
            FErrorCode:=errWrite;
      end;
      if FErrorCode=errNoOperation then begin
        NewFile:=TFileStream.Create(FTargetName, fmCreate or fmShareDenyWrite);
        try
          // Header kopieren
          NewFile.CopyFrom(OldFile, FDataOffset);
          AProgress:=0;
          AllSampleNum:=0;
          MaxExistingValue:=0;
          BufSize:=FBytesPerSample*1000;
          repeat
            // Puffer einlesen
            BytesRead:=OldFile.Read(FileBuffer, BufSize);
            // Anzahl Samples im Puffer
            ACount:=BytesRead div FBytesPerSample;
            // Einzelne Samples aus dem Puffer lesen
            for SampleNum:=0 to ACount-1 do begin
              // Samplewert auslesen
              Buf:=CopyBufferFrom(FileBuffer, SampleNum*FBytesPerSample, FBytesPerSample);
              for ChannelNum:=0 to FNumChannels-1 do begin
                // Alle Kanäle auslesen und Maximalpegel merken
                ASample:=GetSampleValue(Buf, ChannelNum);
                if abs(ASample)>MaxExistingValue then
                  MaxExistingValue:=abs(ASample);
              end;
            end;
            // Puffer in die neue Datei schreiben
            NewFile.Write(FileBuffer, BytesRead);
            // Fortschritt der Operation berechnen
            AllSampleNum:=AllSampleNum+ACount;
            if assigned(FOnProgress) then begin
              OldProgress:=AProgress;
              AProgress:=round((100/FNumSamples)*AllSampleNum);
              if OldProgress<>AProgress then
                FOnProgress(Self, AProgress);
            end;
          until BytesRead<BufSize;
          // Höchsten tatsächlichen Samplewert suchen
          MaxPossibleValue:=GetMaxPossibleSampleValue;
          // Verhältnis des höchsten Samplewertes zum höchstmöglichen Samplewert berechnen
          Ratio:=MaxExistingValue/MaxPossibleValue;
          // Umrechnung in Pegelwert
          MaximumLevel:=RatioToLevel(Ratio);
          FErrorCode:=errNone;
        except
          FErrorCode:=errWrite;
        end;
        FreeAndNil(NewFile);
        if assigned(FOnOperationDone) then
          FOnOperationDone(Self, opCopyAndAnalyzeFile, FErrorCode);
      end;
    finally
      FreeAndNil(OldFile);
    end;
  end;
  Result:=FErrorCode=errNone;
end; // CopyAndAnalyzeFile

function TAudioTools.ChunkExists(const AChunk:TString4):boolean;
var i : integer;
    LChunk : TString4;
begin
  Result:=false;
  LChunk:=LowerCase(AChunk);
  i:=0;
  while (i<FChunks.Count) and not Result do begin
    if LowerCase(FChunks[i])=LChunk then
      Result:=true;
    inc(i);
  end;
end; // ChunkExists

function TAudioTools.EncodeLittleEndian(const AValues:TSampleBuffer):TByteBuffer;
var i,j,
    LoByte,
    HiByte : byte;
begin
  for i:=0 to 5 do
    Result[i]:=0;
  j:=0;
  for i:=0 to 2 do begin
    LoByte:=AValues[i] mod $100;
    Result[j]:=LoByte;
    inc(j);
    HiByte:=AValues[i] shr 8;
    Result[j]:=HiByte;
    inc(j);
  end;
end; // EncodeLittleEndian

function TAudioTools.Get24BitValue(const Value: T24BitSample;const Signed: boolean): integer;
begin
  Result := 0;
  if Signed then
    if (Value[2] and $80) <> 0 then
      Result := integer($FF000000);
  Move(Value, Result, 3);
end; // Get24BitValue

function TAudioTools.GetCompressionName:string;
begin
  case FCompression of
    1  : Result:='PCM/Uncompressed';
    2  : Result:='Microsoft ADPCM';
    6  : Result:='ITU G.711 a-law';
    7  : Result:='ITU G.711 µ-law';
    17 : Result:='IMA ADPCM';
    20 : Result:='ITU G.723 ADPCM (Yamaha)';
    49 : Result:='GSM 6.10';
    64 : Result:='ITU G.721 ADPCM';
    80 : Result:='MPEG';
    else Result:='Unknown';
  end;
end; // GetCompressionName

function TAudioTools.GetErrorMessage:string;
begin
  case FErrorCode of
    errNone        : Result:='';
    errNoOperation : Result:='Nichts zu tun.';
    errClip        : Result:='Die Pegeländerung führte zu Clipping (abgeschnittene Samplewerte).';
    errRead        : Result:='Aus der Datei konnte nicht gelesen werden.';
    errWrite       : Result:='In die Datei kann nicht geschreiben werden.';
  end;
end; // GetErrorMessage

function TAudioTools.GetMaxExistingSampleValue(var MaxPossibleValue:integer):integer;
var WaveFile         : TFileStream;
    Buf              : TByteBuffer;
    FileBuffer       : TFileBuffer;
    AProgress,
    OldProgress      : byte;
    AllSampleNum,
    SampleNum,
    ChannelNum,
    ASample,
    BufSize,
    BytesRead,
    ACount,
    MaxExistingValue : integer;
begin
  // Den höchsten Samplewert in der Datei suchen
  Result:=-1;
  FErrorCode:=errNoOperation;
  // Höchstmöglichen Samplewert ermitteln
  MaxPossibleValue:=GetMaxPossibleSampleValue;
  WaveFile:=TFileStream.Create(FFilename, fmOpenRead or fmShareDenyWrite);
  try
    // Beginn der Sampledaten suchen
    WaveFile.Seek(FDataOffset, 0);
    AProgress:=0;
    AllSampleNum:=0;
    MaxExistingValue:=0;
    BufSize:=FBytesPerSample*1000;
    repeat
      // Puffer einlesen
      BytesRead:=WaveFile.Read(FileBuffer, BufSize);
      // Anzahl Samples im Puffer
      ACount:=BytesRead div FBytesPerSample;
      SampleNum:=0;
      // Einzelne Samples aus dem Puffer lesen
      while (SampleNum<ACount) and (MaxExistingValue<MaxPossibleValue) do begin
        // Samplewert auslesen
        Buf:=CopyBufferFrom(FileBuffer, SampleNum*FBytesPerSample, FBytesPerSample);
        for ChannelNum:=0 to FNumChannels-1 do begin
          // Alle Kanäle auslesen
          ASample:=GetSampleValue(Buf, ChannelNum);
          if abs(ASample)>MaxExistingValue then
            MaxExistingValue:=abs(ASample);
        end;
        inc(SampleNum);
      end;
      // Fortschritt der Operation berechnen
      AllSampleNum:=AllSampleNum+ACount;
      if assigned(FOnProgress) then begin
        OldProgress:=AProgress;
        AProgress:=round((100/FNumSamples)*AllSampleNum);
        if OldProgress<>AProgress then
          FOnProgress(Self, AProgress);
      end;
    until (BytesRead<BufSize);
    Result:=MaxExistingValue;
    FErrorCode:=errNone;
  except
    FErrorCode:=errRead;
  end;
  FreeAndNil(WaveFile);
  if assigned(FOnOperationDone) then
    FOnOperationDone(Self, opGetMaxSampleValue, FErrorCode);
end; // GetMaxExistingSampleValue

function TAudioTools.GetMaxPossibleSampleValue:integer;
var i      : byte;
    AValue : T24BitSample;
begin
  // Höchstmöglichen Samplewert ermitteln
  Result:=0;
  case FBitsPerSample of
    8  : Result:=High(Byte);
    16 : Result:=High(Word) div 2;
    24 : begin
      for i:=0 to High(T24BitSample) do
        AValue[i]:=$FF;
      Result:=Get24BitValue(AValue, false) div 2;
    end;
  end;
end; // GetMaxPossibleSampleValue

function TAudioTools.GetMaximumLevel:double;
var MaxPossibleValue,
    MaxExistingValue : integer;
    Ratio            : double;
begin
  Result:=-1;
  // Höchsten tatsächlichen Samplewert suchen
  MaxExistingValue:=GetMaxExistingSampleValue(MaxPossibleValue);
  // Verhältnis des höchsten Samplewertes zum höchstmöglichen Samplewert berechnen
  Ratio:=MaxExistingValue/MaxPossibleValue;
  if Ratio<>0 then
    // Umrechnung in Pegelwert
    Result:=RatioToLevel(Ratio);
end; // GetMaximumLevel

function TAudioTools.GetSampleValue(Buf:TByteBuffer;const ChannelNum:byte): integer;
var Offset : byte;
begin
  {$IfDef BigEndian}
  // Umwandlung eines "Big Endian"-Buffers in integer
  Result:=0;
  Offset:=ChannelNum*FBufferSize;
  case FBufferSize of
    1 : Result := Buf[0+Offset];
    2 : Result := SmallInt(Buf[1+Offset]+(Buf[0+Offset] shl 8){-$8000});
    3 : Result := Buf[2+Offset]+(Buf[1+Offset] shl 8)+(Buf[0+Offset] shl 16)-$800000;
  end;
  {$Else}
  // Umwandlung eines "Little Endian"-Buffers in integer
  Result:=0;
  Offset:=ChannelNum*FBufferSize;
  case FBufferSize of
    1 : Result := Buf[0+Offset];
    2 : Result := SmallInt(Buf[0+Offset]+(Buf[1+Offset] shl 8){-$8000});
    3 : Result := Buf[0+Offset]+(Buf[1+Offset] shl 8)+(Buf[2+Offset] shl 16)-$800000;
  end;
  {$EndIf}
end; // GetSampleValue

function TAudioTools.Normalize:double;
var MaxPossibleValue,
    MaxExistingValue,
    MaxTargetValue   : integer;
    Ratio            : double;
begin
  // Datei normalisieren
  Result:=0;
  // Höchsten tatsächlichen Samplewert suchen
  MaxExistingValue:=GetMaxExistingSampleValue(MaxPossibleValue);
  if (FErrorCode=errNone) and (MaxExistingValue<>0) then begin
    // Höchsten Samplewert für die Normalisierung ermitteln
    MaxTargetValue:=round(MaxPossibleValue-(MaxPossibleValue/100));
    // Verhältnis des höchsten Samplewertes zum höchstmöglichen Samplewert berechnen
    Ratio:=MaxTargetValue/MaxExistingValue;
    if Ratio<>0 then begin
      if ApplyNewLevelToFile(Ratio) then
        // Umrechnung in Pegelwert
        Result:=RatioToLevel(Ratio);
      if assigned(FOnOperationDone) then
        FOnOperationDone(Self, opNormalize, FErrorCode);
    end;
  end;
end; // Normalize

function TAudioTools.ReadWaveFileHeader:boolean;
var WaveFile   : TFileStream;
    RIFFChunk  : TRIFFHeader;
    FmtChunk   : TFMTHeader;
    Signature  : TSignatureType;
    OldPos     : Cardinal;
    IsFmtSign,
    IsDataSign : boolean;
begin
  Result:=false;
  try
    WaveFile:=TFileStream.Create(FFilename, fmOpenRead + fmShareDenyWrite);
    FSize:=WaveFile.Size;
    // "RIFF Chunk" auslesen
    WaveFile.Read(RIFFChunk, SizeOf(RIFFChunk));
    if (AnsiCompareText(RIFFChunk.Signature, RIFF_SIGNATURE) = 0) and
      (AnsiCompareText(RIFFChunk.Format, WAVE_SIGNATURE) = 0) then begin
      FChunks.Add('RIFF');
      // Korrektes Format: "FMT "- und "DATA" Signatur suchen
      Signature:='    ';
      FDataSize:=0;
      FDataOffset:=FHeaderSize;
      FNumSamples:=0;
      FLength:=0;
      IsDataSign:=false;
      while (WaveFile.Position<FSize) and not IsDataSign do begin
        WaveFile.Read(Signature, SizeOf(Signature));
        { TODO : Signature nur zu "FChunks" hinzufügen, wenn ausschl. Buchstaben enthalten sind }
        if IsRIFFChunk(Signature) and not ChunkExists(Signature) then
          FChunks.Add(Signature);
        IsDataSign:=(AnsiCompareText(Signature, DATA_SIGNATURE)=0);
        IsFmtSign:=(AnsiCompareText(Signature, FMT_SIGNATURE)=0);
        if not IsFmtSign and (AnsiCompareText(copy(Signature, 3, 2), 'fm')=0) then begin
          // "FMT "-Signatur mit einem Offset von 2 Bytes auslesen
          OldPos:=WaveFile.Position;
          WaveFile.Position:=OldPos-2;
          WaveFile.Read(Signature, SizeOf(Signature));
          IsFmtSign:=(AnsiCompareText(Signature, FMT_SIGNATURE)=0);
          if not IsFmtSign then
            WaveFile.Position:=OldPos;
        end;
        if IsFmtSign then begin
          // "FMT "-Signatur gefunden
          FChunks.Add('FMT ');
          OldPos:=WaveFile.Position;
          WaveFile.Position:=OldPos-SizeOf(Signature);
          // Audioformat auslesen
          WaveFile.Read(FMTChunk, SizeOf(FMTChunk));
          with FMTChunk do begin
            FBitsPerSample:=BitsPerSample;
            FBytesPerSample:=BytesPerSample;
            FCompression:=Compression;
            FNumChannels:=NumChannels;
            FSampleRate:=SampleRate;
          end;
          FBufferSize:=FBitsPerSample div 8;
          if (FBitsPerSample mod 8)>0 then
            inc(FBufferSize);
          WaveFile.Position:=OldPos;
        end;
        if not IsDataSign and (AnsiCompareText(copy(Signature, 3, 2), 'da')=0) then begin
          // "DATA"-Signatur mit einem Offset von 2 Bytes auslesen
          OldPos:=WaveFile.Position;
          WaveFile.Position:=OldPos-2;
          WaveFile.Read(Signature, SizeOf(Signature));
          IsDataSign:=(AnsiCompareText(Signature, DATA_SIGNATURE)=0);
          if not IsDataSign then
            WaveFile.Position:=OldPos;
        end;
        if IsDataSign then begin
          // "DATA"-Signatur gefunden
          FChunks.Add('DATA');
          // Größe des Datenblocks auslesen
          WaveFile.Read(FDataSize, SizeOf(FDataSize));
          // Beginn des Datenblocks speichern
          FDataOffset:=WaveFile.Position;
          // Anzahl der Samples und Länge in sec ermitteln
          if FBytesPerSample>0 then
            FNumSamples:=FDataSize div FBytesPerSample
          else
            FNumSamples:=0;
          if FMTChunk.BytesPerSec<>0 then
            FLength:=FDataSize/FMTChunk.BytesPerSec;
          Result:=FCompression=1;
        end;
      end;
    end;
  finally
    FreeAndNil(WaveFile);
  end;
end; // ReadWaveFileHeader

procedure TAudioTools.SetFileName(const NewName: string);
begin
  if (NewName <> FFileName) and FileExists(NewName) then begin
    FFileName:=NewName;
    FChunks.Clear;
    FValidFormat:=ReadWaveFileHeader;
    FBackupFileName:='';
  end;
end; // SetFileName

function TAudioTools.SetLevel(const Level:double):double;
var MaxPossibleValue,
    MaxExistingValue,
    MaxTargetValue   : integer;
    Ratio            : double;
begin
  // Datei auf den vorgegebenen Pegel "Level" (in dBFS) umrechnen
  Result:=0;
  // Höchsten tatsächlichen Samplewert suchen
  MaxExistingValue:=GetMaxExistingSampleValue(MaxPossibleValue);
  if (FErrorCode=errNone) and (MaxExistingValue<>0) then begin
    // Höchsten Samplewert aus Level und höchstmöglichen Samplewert berechnen
    if Level<=0 then begin
      Ratio:=LevelToRatio(Level);
      MaxTargetValue:=round(MaxPossibleValue*Ratio);
      // Verstärkungsfaktor aus höchstem Samplewert und höchstmöglichen Samplewert berechnen
      Ratio:=MaxTargetValue/MaxExistingValue;
    end
    else
      Ratio:=1;
    if Ratio<>0 then begin
      if ApplyNewLevelToFile(Ratio) then
        // Umrechnung in Pegelwert
        Result:=RatioToLevel(Ratio);
      if assigned(FOnOperationDone) then
        FOnOperationDone(Self, opSetLevel, FErrorCode);
    end;
  end;
end; // SetLevel

{$IFDEF SR_Delphi4_Up}
function TAudioTools.SetLevel(const Level:double;const FadeInTime,FadeOutTime:word):double;
var MaxPossibleValue,
    MaxExistingValue,
    MaxTargetValue   : integer;
    Ratio            : double;
begin
  // Datei auf den vorgegebenen Pegel "Level" (in dBFS) umrechnen
  Result:=0;
  // Höchsten tatsächlichen Samplewert suchen
  MaxExistingValue:=GetMaxExistingSampleValue(MaxPossibleValue);
  if (FErrorCode=errNone) and (MaxExistingValue<>0) then begin
    // Höchsten Samplewert aus Level und höchstmöglichen Samplewert berechnen
    if Level<=0 then begin
      Ratio:=LevelToRatio(Level);
      MaxTargetValue:=round(MaxPossibleValue*Ratio);
      // Verstärkungsfaktor aus höchstem Samplewert und höchstmöglichen Samplewert berechnen
      Ratio:=MaxTargetValue/MaxExistingValue;
    end
    else
      Ratio:=1;
    if Ratio<>0 then begin
      if ApplyNewLevelToFile(Ratio, FadeInTime, FadeOutTime) then
        // Umrechnung in Pegelwert
        Result:=RatioToLevel(Ratio);
      if assigned(FOnOperationDone) then
        FOnOperationDone(Self, opSetLevel, FErrorCode);
    end;
  end;
end; // SetLevel
{$ENDIF}

procedure Register;
begin
  RegisterComponents('Simon', [TAudioTools]);
end;

end.
