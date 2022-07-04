unit DragDropFile;
// -----------------------------------------------------------------------------
// Project:         Drag and Drop Component Suite.
// Module:          DragDropFile
// Description:     Implements Dragging and Dropping of files and folders.
// Version:         5.0
// Date:            04-FEB-2009
// Target:          Win32, Delphi 5-2009
// Authors:         Anders Melander, anders@melander.dk, http://melander.dk
// Copyright        © 1997-1999 Angus Johnson & Anders Melander
//                  © 2000-2009 Anders Melander
// -----------------------------------------------------------------------------

interface

uses
  DragDrop,
  DropTarget,
  DropSource,
  DragDropFormats,
  DragDropText,
  ShlObj,
  ActiveX,
  Windows,
  Classes;

{$include DragDrop.inc}

// TODO : The following DD_WIDESTRINGLIST define is only for debug use. It should be removed before deploy.
{_$define DD_WIDESTRINGLIST}
{$ifdef DD_WIDESTRINGLIST}
type
  TWString = record
    WString: UnicodeString;
  end;

  TWideStringsHelper = class helper for TStrings
  private
    function GetWideText: UnicodeString;
  protected
    function GetWide(Index: Integer): UnicodeString;
    procedure PutWide(Index: Integer; const S: UnicodeString);
    function IsWide: boolean;
  published
  public
    function Add(const S: UnicodeString): Integer; overload;
    function AddWide(const S: UnicodeString): Integer;
    function AddObject(const S: UnicodeString; AObject: TObject): Integer; overload;
    function AddObjectWide(const S: UnicodeString; AObject: TObject): Integer;
    function IndexOf(const S: UnicodeString): Integer; overload;
    function IndexOfWide(const S: UnicodeString): Integer;
    procedure Insert(Index: Integer; const S: UnicodeString); overload;
    procedure InsertWide(Index: Integer; const S: UnicodeString);
    property WideStrings[Index: Integer]: UnicodeString read GetWide write PutWide;
    property WideText: UnicodeString read GetWideText;
    property Wide: boolean read IsWide;
  end;

  TWideStrings = class(TStrings)
  private
  protected
    function GetWide(Index: Integer): UnicodeString; virtual; abstract;
    procedure PutWide(Index: Integer; const S: UnicodeString); virtual; abstract;
  public
    function AddObject(const S: UnicodeString; AObject: TObject): Integer; reintroduce; virtual;
    function Add(const S: UnicodeString): Integer; reintroduce; virtual;
    function IndexOf(const S: UnicodeString): Integer; reintroduce; virtual;
  end;

  TWideStringList = class(TWideStrings)
  private
    FWideStringList: TList;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
  protected
    procedure Changed; virtual;
    procedure Changing; virtual;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    function GetWide(Index: Integer): UnicodeString; override;
    procedure PutWide(Index: Integer; const S: UnicodeString); override;
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure AddStrings(Strings: TStrings); override;
    function Add(const S: UnicodeString): Integer; override;
    function AddObject(const S: UnicodeString; AObject: TObject): Integer; override;
    procedure Delete(Index: Integer); override;
    function IndexOf(const S: UnicodeString): Integer; override;
    procedure Insert(Index: Integer; const S: string); overload; override;
    procedure Insert(Index: Integer; const S: UnicodeString); overload;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;
{$else}
type
  TWideStrings = class(TStrings);
  TWideStringList = class(TStringList);
{$endif}


type
////////////////////////////////////////////////////////////////////////////////
//
//		TFileClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
  TFileClipboardFormat = class(TCustomSimpleClipboardFormat)
  private
    FFiles: TStrings;
    FWide: boolean;
  protected
    function ReadData(Value: pointer; Size: integer): boolean; override;
    function WriteData(Value: pointer; Size: integer): boolean; override;
    function GetSize: integer; override;
    property Wide: boolean read FWide;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetClipboardFormat: TClipFormat; override;
    function Assign(Source: TCustomDataFormat): boolean; override;
    function AssignTo(Dest: TCustomDataFormat): boolean; override;
    procedure Clear; override;
    function HasData: boolean; override;
    property Files: TStrings read FFiles;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//		TAnsiFilenameClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
  TAnsiFilenameClipboardFormat = class(TCustomAnsiTextClipboardFormat)
  private
  protected
    class procedure RegisterCompatibleFormats; override;
  public
    function GetClipboardFormat: TClipFormat; override;
    function Assign(Source: TCustomDataFormat): boolean; override;
    function AssignTo(Dest: TCustomDataFormat): boolean; override;
    property Filename: AnsiString read GetString write SetString;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//		TUnicodeFilenameClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
  TUnicodeFilenameClipboardFormat = class(TCustomUnicodeTextClipboardFormat)
  private
  protected
    class procedure RegisterCompatibleFormats; override;
  public
    function GetClipboardFormat: TClipFormat; override;
    function Assign(Source: TCustomDataFormat): boolean; override;
    function AssignTo(Dest: TCustomDataFormat): boolean; override;
    property Filename: UnicodeString read GetText write SetText;
  end;

  TFilenameWClipboardFormat = TUnicodeFilenameClipboardFormat {$ifdef VER17_PLUS}deprecated {$IFDEF VER20_PLUS}'Use TUnicodeFilenameClipboardFormat instead'{$ENDIF}{$endif};

////////////////////////////////////////////////////////////////////////////////
//
//		TFilenameClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
{$ifdef UNICODE}
  TFilenameClipboardFormat = TUnicodeFilenameClipboardFormat;
{$else}
  TFilenameClipboardFormat = TAnsiFilenameClipboardFormat;
{$endif}

////////////////////////////////////////////////////////////////////////////////
//
//		TAnsiFilenameMapClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
  TAnsiFilenameMapClipboardFormat = class(TCustomSimpleClipboardFormat)
  private
    FFileMaps: TStrings;
  protected
    function ReadData(Value: pointer; Size: integer): boolean; override;
    function WriteData(Value: pointer; Size: integer): boolean; override;
    function GetSize: integer; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetClipboardFormat: TClipFormat; override;
    procedure Clear; override;
    function HasData: boolean; override;
    property FileMaps: TStrings read FFileMaps;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//		TUnicodeFilenameMapClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
  TUnicodeFilenameMapClipboardFormat = class(TCustomSimpleClipboardFormat)
  private
    FFileMaps: TStrings;
  protected
    function ReadData(Value: pointer; Size: integer): boolean; override;
    function WriteData(Value: pointer; Size: integer): boolean; override;
    function GetSize: integer; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetClipboardFormat: TClipFormat; override;
    procedure Clear; override;
    function HasData: boolean; override;
    property FileMaps: TStrings read FFileMaps;
  end;

  TFilenameMapWClipboardFormat = TUnicodeFilenameMapClipboardFormat {$ifdef VER17_PLUS}deprecated {$IFDEF VER20_PLUS}'Use TUnicodeFilenameMapClipboardFormat instead'{$ENDIF}{$endif};

//////////////////////////////////////////////////////////////////////////////
//
//		TFilenameMapClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
{$ifdef UNICODE}
  TFilenameMapClipboardFormat = TUnicodeFilenameMapClipboardFormat;
{$else}
  TFilenameMapClipboardFormat = TAnsiFilenameMapClipboardFormat;
{$endif}


////////////////////////////////////////////////////////////////////////////////
//
//		TFileMapDataFormat
//
////////////////////////////////////////////////////////////////////////////////
  TFileMapDataFormat = class(TCustomDataFormat)
  private
    FFileMaps: TStrings;
  protected
    class procedure RegisterCompatibleFormats; override;
  public
    constructor Create(AOwner: TDragDropComponent); override;
    destructor Destroy; override;
    function Assign(Source: TClipboardFormat): boolean; override;
    function AssignTo(Dest: TClipboardFormat): boolean; override;
    procedure Clear; override;
    function HasData: boolean; override;
    function NeedsData: boolean; override;
    property FileMaps: TStrings read FFileMaps;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//		TFileDataFormat
//
////////////////////////////////////////////////////////////////////////////////
  TFileDataFormat = class(TCustomDataFormat)
  private
    FFiles: TStrings;
  protected
    class procedure RegisterCompatibleFormats; override;
  public
    constructor Create(AOwner: TDragDropComponent); override;
    destructor Destroy; override;
    function Assign(Source: TClipboardFormat): boolean; override;
    function AssignTo(Dest: TClipboardFormat): boolean; override;
    procedure Clear; override;
    function HasData: boolean; override;
    function NeedsData: boolean; override;
    property Files: TStrings read FFiles;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//		TFileGroupDescriptorCustomClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
  TFileGroupDescriptorCustomClipboardFormat = class(TCustomSimpleClipboardFormat)
  private
    FBuffer: pointer;
  protected
    function GetFileDescriptor(Index: integer): pointer;
    function GetCount: integer;
    procedure SetCount(const Value: integer);
    function GetBufferSize(Count: integer): integer; virtual; abstract;
    function GetSize: integer; override;
    procedure CopyFrom(Source: pointer);
    function ReadData(Value: pointer; Size: integer): boolean; override;
    function WriteData(Value: pointer; Size: integer): boolean; override;
    property FileGroupDescriptor: pointer read FBuffer;
    property FileDescriptors[Index: integer]: pointer read GetFileDescriptor;
  public
    destructor Destroy; override;
    procedure Clear; override;
    function HasData: boolean; override;
    property Count: integer read GetCount write SetCount;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//		TAnsiFileGroupDescriptorClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
const
  // Missing declaration from shlobj.pas (D6 and earlier)
  FD_PROGRESSUI = $4000; // Show Progress UI w/Drag and Drop

type
  TAnsiFileGroupDescriptorClipboardFormat = class(TFileGroupDescriptorCustomClipboardFormat)
  private
  protected
    function GetFilename(Index: integer): AnsiString;
    procedure SetFilename(Index: integer; const Value: AnsiString);
    function GetFileDescriptor(Index: integer): PFileDescriptorA;
    function GetFileGroupDescriptor: PFileGroupDescriptorA;
    function GetBufferSize(Count: integer): integer; override;
  public
    function GetClipboardFormat: TClipFormat; override;
    procedure CopyFrom(AFileGroupDescriptor: PFileGroupDescriptorA);
    property FileGroupDescriptor: PFileGroupDescriptorA read GetFileGroupDescriptor;
    property FileDescriptors[Index: integer]: PFileDescriptorA read GetFileDescriptor;
    property Filenames[Index: integer]: AnsiString read GetFilename write SetFilename;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//		TUnicodeFileGroupDescriptorClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
  // Warning: TFileGroupDescriptorW has wrong declaration in ShlObj.pas!
  TFileGroupDescriptorW = record
    cItems: UINT;
    fgd: array[0..0] of TFileDescriptorW;
  end;

  PFileGroupDescriptorW = ^TFileGroupDescriptorW;

  TUnicodeFileGroupDescriptorClipboardFormat = class(TFileGroupDescriptorCustomClipboardFormat)
  private
  protected
    function GetFilename(Index: integer): UnicodeString;
    procedure SetFilename(Index: integer; const Value: UnicodeString);
    function GetFileDescriptor(Index: integer): PFileDescriptorW;
    function GetFileGroupDescriptor: PFileGroupDescriptorW;
    function GetBufferSize(Count: integer): integer; override;
  public
    function GetClipboardFormat: TClipFormat; override;
    procedure CopyFrom(AFileGroupDescriptor: PFileGroupDescriptorW);
    property FileGroupDescriptor: PFileGroupDescriptorW read GetFileGroupDescriptor;
    property FileDescriptors[Index: integer]: PFileDescriptorW read GetFileDescriptor;
    property Filenames[Index: integer]: UnicodeString read GetFilename write SetFilename;
  end;

  TFileGroupDescriptorWClipboardFormat = TUnicodeFileGroupDescriptorClipboardFormat {$ifdef VER17_PLUS}deprecated {$IFDEF VER20_PLUS}'Use TUnicodeFileGroupDescriptorClipboardFormat instead'{$ENDIF}{$endif};

////////////////////////////////////////////////////////////////////////////////
//
//		TFileGroupDescriptorClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
{$ifdef UNICODE}
  TFileGroupDescriptorClipboardFormat = TUnicodeFileGroupDescriptorClipboardFormat;
{$else}
  TFileGroupDescriptorClipboardFormat = TAnsiFileGroupDescriptorClipboardFormat;
{$endif}

////////////////////////////////////////////////////////////////////////////////
//
//		TFileContentsClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
  TFileContentsClipboardFormat = class(TCustomAnsiStringClipboardFormat)
  public
    function GetClipboardFormat: TClipFormat; override;
    constructor Create; override;
    property Data;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//		TFileContentsStreamClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
  TFileContentsStreamClipboardFormat = class(TClipboardFormat)
  private
    FStreams: TStreamList;
  protected
    class procedure RegisterCompatibleFormats; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetClipboardFormat: TClipFormat; override;
    function GetData(const DataObject: IDataObject): boolean; override;
    procedure Clear; override;
    function HasData: boolean; override;
    function AssignTo(Dest: TCustomDataFormat): boolean; override;
    property Streams: TStreamList read FStreams;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//		TFileContentsStreamOnDemandClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
  TVirtualFileStreamDataFormat = class;
  TFileContentsStreamOnDemandClipboardFormat = class;

  TOnGetStreamEvent = procedure(Sender: TFileContentsStreamOnDemandClipboardFormat;
    Index: integer; out AStream: IStream) of object;

  TFileContentsStreamOnDemandClipboardFormat = class(TClipboardFormat)
  private
    FOnGetStream: TOnGetStreamEvent;
    FGotData: boolean;
    FDataRequested: boolean;
  protected
    function DoSetData(const AFormatEtcIn: TFormatEtc;
      var AMedium: TStgMedium): boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetClipboardFormat: TClipFormat; override;
    function GetData(const DataObject: IDataObject): boolean; override;
    procedure Clear; override;
    function HasData: boolean; override;
    function AssignTo(Dest: TCustomDataFormat): boolean; override;
    function Assign(Source: TCustomDataFormat): boolean; override;

    function GetStream(Index: integer): IStream;

    property OnGetStream: TOnGetStreamEvent read FOnGetStream write FOnGetStream;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//		TFileContentsStorageClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
  TFileContentsStorageClipboardFormat = class(TClipboardFormat)
  private
    FStorages: TStorageInterfaceList;
  protected
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetClipboardFormat: TClipFormat; override;
    function GetData(const DataObject: IDataObject): boolean; override;
    procedure Clear; override;
    function HasData: boolean; override;
    function AssignTo(Dest: TCustomDataFormat): boolean; override;
    property Storages: TStorageInterfaceList read FStorages;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//		TVirtualFileStreamDataFormat
//
////////////////////////////////////////////////////////////////////////////////
  TVirtualFileStreamDataFormat = class(TCustomDataFormat)
  private
    FFileDescriptors: TMemoryList;
    FFileNames: TStrings;
    FFileContentsClipboardFormat: TFileContentsStreamOnDemandClipboardFormat;
    FAnsiFileGroupDescriptorClipboardFormat: TAnsiFileGroupDescriptorClipboardFormat;
    FUnicodeFileGroupDescriptorClipboardFormat: TUnicodeFileGroupDescriptorClipboardFormat;
    FHasContents: boolean;
  protected
    class procedure RegisterCompatibleFormats; override;
    procedure SetFileNames(const Value: TStrings);
    function GetOnGetStream: TOnGetStreamEvent;
    procedure SetOnGetStream(const Value: TOnGetStreamEvent);
    function GetFileDescriptor(Index: integer): PFileDescriptorW;
  public
    constructor Create(AOwner: TDragDropComponent); override;
    destructor Destroy; override;

    function Assign(Source: TClipboardFormat): boolean; override;
    function AssignTo(Dest: TClipboardFormat): boolean; override;
    procedure Clear; override;
    function HasData: boolean; override;
    function NeedsData: boolean; override;
    property FileDescriptors[Index: integer]: PFileDescriptorW read GetFileDescriptor;
    property FileNames: TStrings read FFileNames write SetFileNames;
    property FileContentsClipboardFormat: TFileContentsStreamOnDemandClipboardFormat
      read FFileContentsClipboardFormat;
    property AnsiFileGroupDescriptorClipboardFormat: TAnsiFileGroupDescriptorClipboardFormat
      read FAnsiFileGroupDescriptorClipboardFormat;
    property UnicodeFileGroupDescriptorClipboardFormat: TUnicodeFileGroupDescriptorClipboardFormat
      read FUnicodeFileGroupDescriptorClipboardFormat;
{$ifdef UNICODE}
    property FileGroupDescriptorClipboardFormat: TFileGroupDescriptorClipboardFormat
      read FUnicodeFileGroupDescriptorClipboardFormat;
{$else}
    property FileGroupDescriptorClipboardFormat: TFileGroupDescriptorClipboardFormat
      read FAnsiFileGroupDescriptorClipboardFormat;
{$endif}
    property OnGetStream: TOnGetStreamEvent read GetOnGetStream write SetOnGetStream;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//		TDropFileTarget
//
////////////////////////////////////////////////////////////////////////////////
  TDropFileTarget = class(TCustomDropMultiTarget)
  private
    FFileFormat: TFileDataFormat;
    FFileMapFormat: TFileMapDataFormat;
  protected
    function GetFiles: TStrings;
    function GetMappedNames: TStrings;
    function GetPreferredDropEffect: LongInt; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Files: TStrings read GetFiles;
    property MappedNames: TStrings read GetMappedNames;
  end;

////////////////////////////////////////////////////////////////////////////////
//
//		TDropFileSource
//
////////////////////////////////////////////////////////////////////////////////
  TDropFileSource = class(TCustomDropMultiSource)
  private
    FFileFormat: TFileDataFormat;
    FFileMapFormat: TFileMapDataFormat;
    function GetFiles: TStrings;
    function GetMappedNames: TStrings;
  protected
    procedure SetFiles(AFiles: TStrings);
    procedure SetMappedNames(ANames: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Files: TStrings read GetFiles write SetFiles;
    // MappedNames is only needed if files need to be renamed during a drag op.
    // E.g. dragging from 'Recycle Bin'.
    property MappedNames: TStrings read GetMappedNames write SetMappedNames;
  end;


////////////////////////////////////////////////////////////////////////////////
//
//		Misc.
//
////////////////////////////////////////////////////////////////////////////////
function ReadFilesFromHGlobal(const HGlob: HGlobal; Files: TStrings): boolean; // V4: renamed
function ReadFilesFromData(Data: pointer; Size: integer; Files: TStrings): boolean;
function ReadFilesFromZeroList(const Data: pointer; Size: integer;
  Wide: boolean; Files: TStrings): boolean;
function WriteFilesToZeroList(Data: pointer; Size: integer;
  Wide: boolean; const Files: TStrings): boolean;


////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//
//			IMPLEMENTATION
//
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
implementation

uses
{$ifdef VER14_PLUS}
  RTLConsts,
{$else}
  Consts,
{$endif}
  DragDropPIDL,
  ComObj,
  SysUtils;

////////////////////////////////////////////////////////////////////////////////
//
//		Utilities
//
////////////////////////////////////////////////////////////////////////////////

function ReadFilesFromHGlobal(const HGlob: HGlobal; Files: TStrings): boolean;
var
  DropFiles: PDropFiles;
begin
  DropFiles := PDropFiles(GlobalLock(HGlob));
  try
    Result := ReadFilesFromData(DropFiles, GlobalSize(HGlob), Files)
  finally
    GlobalUnlock(HGlob);
  end;
end;

function ReadFilesFromData(Data: pointer; Size: integer; Files: TStrings): boolean;
var
  Wide: boolean;
begin
  Files.Clear;
  if (Data <> nil) then
  begin
    Wide := PDropFiles(Data)^.fWide;
    dec(Size, PDropFiles(Data)^.pFiles);
    inc(PByte(Data), PDropFiles(Data)^.pFiles);
    ReadFilesFromZeroList(Data, Size, Wide, Files);
  end;

  Result := (Files.Count > 0);
end;

function ReadFilesFromZeroList(const Data: pointer; Size: integer;
  Wide: boolean; Files: TStrings): boolean;
var
  p: PAnsiChar;
  StringSize: integer;
begin
  Result := False;
  if (Data <> nil) then
  begin
    p := Data;
    while (Size > 0) and (p^ <> #0) do
    begin
      if (Wide) then
      begin
{$ifdef DD_WIDESTRINGLIST}
        Files.AddWide(PWideChar(p));
{$else}
        Files.Add(PWideChar(p));
{$endif}
        StringSize := (Length(PWideChar(p)) + 1) * 2;
      end else
      begin
        Files.Add(p);
        StringSize := Length(p) + 1;
      end;
      inc(p, StringSize);
      dec(Size, StringSize);
      Result := True;
    end;
  end;
end;

function WriteFilesToZeroList(Data: pointer; Size: integer;
  Wide: boolean; const Files: TStrings): boolean;
var
{$ifdef DD_WIDESTRINGLIST}
  j: integer;
  ws: UnicodeString;
  pws: PWideChar;
{$endif}
  i: integer;
  p: PAnsiChar;
  pw: PWideChar;
  StringSize: integer;
  s: string;
begin
  Result := False;
  if (Data <> nil) then
  begin
    p := Data;
    i := 0;
    dec(Size);
    while (Size > 0) and (i < Files.Count) do
    begin
      if (Wide) then
      begin
        pw := PWideChar(p);
{$ifdef DD_WIDESTRINGLIST}
        if (Files.Wide) then
        begin
          ws := Files.WideStrings[i];
          pws := PWideChar(ws);
          j := Size;
          while (j > 0) and (pws^ <> #0) do
          begin
            pw^ := pws^;
            inc(pw);
            inc(pws);
            dec(j, SizeOf(WideChar));
          end;
          pw^ := #0;
          StringSize := (Length(ws)+1)*2;
        end else
{$endif}
        begin
          s := Files[i];
          StringToWideChar(s, pw, Size);
          StringSize := (Length(s)+1)*2;
        end;
      end else
      begin
        s := Files[i];
        StrPLCopy(p, s, Size);
        StringSize := Length(s)+1;
      end;
      inc(p, StringSize);
      dec(Size, StringSize);
      inc(i);
      Result := True;
    end;

    // Final teminating zero.
    if (Size >= 0) then
      PByte(p)^ := 0;
  end;
end;


////////////////////////////////////////////////////////////////////////////////
//
//		TFileClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
constructor TFileClipboardFormat.Create;
begin
  inherited Create;
  FFiles := TWideStringList.Create;
  // Note: Setting dwAspect to DVASPECT_SHORT will request that the data source
  // returns the file names in short (8.3) format.
  // FFormatEtc.dwAspect := DVASPECT_SHORT;
  FWide := (Win32Platform = VER_PLATFORM_WIN32_NT);
end;

destructor TFileClipboardFormat.Destroy;
begin
  FFiles.Free;
  inherited Destroy;
end;

function TFileClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  Result := CF_HDROP;
end;

procedure TFileClipboardFormat.Clear;
begin
  FFiles.Clear;
end;

function TFileClipboardFormat.HasData: boolean;
begin
  Result := (FFiles.Count > 0);
end;

function TFileClipboardFormat.GetSize: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to FFiles.Count-1 do
    inc(Result, Length(FFiles[i])+1);
  if (Wide) then
    // Wide strings
    Inc(Result, Result);
  inc(Result, SizeOf(TDropFiles)+2);
end;

function TFileClipboardFormat.ReadData(Value: pointer;
  Size: integer): boolean;
begin
  Result := (Size > SizeOf(TDropFiles));
  if (not Result) then
    exit;

  Result := ReadFilesFromData(Value, Size, FFiles);
end;

function TFileClipboardFormat.WriteData(Value: pointer;
  Size: integer): boolean;
begin
  Result := (Size > SizeOf(TDropFiles));
  if (not Result) then
    exit;

  FillChar(Value^, Size, 0);
  PDropFiles(Value)^.pfiles := SizeOf(TDropFiles);
  PDropFiles(Value)^.fwide := BOOL(ord(Wide));
  inc(PByte(Value), SizeOf(TDropFiles));
  dec(Size, SizeOf(TDropFiles));

  WriteFilesToZeroList(Value, Size, Wide, FFiles);
end;

function TFileClipboardFormat.Assign(Source: TCustomDataFormat): boolean;
begin
  if (Source is TFileDataFormat) then
  begin
    FFiles.Assign(TFileDataFormat(Source).Files);
    Result := True;
  end else
    Result := inherited Assign(Source);
end;

function TFileClipboardFormat.AssignTo(Dest: TCustomDataFormat): boolean;
begin
  if (Dest is TFileDataFormat) then
  begin
    TFileDataFormat(Dest).Files.Assign(FFiles);
    Result := True;
  end else
    Result := inherited AssignTo(Dest);
end;


////////////////////////////////////////////////////////////////////////////////
//
//		TAnsiFilenameClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
var
  CF_FILENAMEA: TClipFormat = 0;

function TAnsiFilenameClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  if (CF_FILENAMEA = 0) then
    CF_FILENAMEA := RegisterClipboardFormat(CFSTR_FILENAMEA);
  Result := CF_FILENAMEA;
end;

class procedure TAnsiFilenameClipboardFormat.RegisterCompatibleFormats;
begin
  inherited RegisterCompatibleFormats;
  RegisterDataConversion(TFileDataFormat, 3+AnsiBoost);
end;

function TAnsiFilenameClipboardFormat.Assign(Source: TCustomDataFormat): boolean;
begin
  if (Source is TFileDataFormat) then
  begin
    Result := (TFileDataFormat(Source).Files.Count > 0);
    if (Result) then
      Filename := TFileDataFormat(Source).Files[0];
  end else
    Result := inherited Assign(Source);
end;

function TAnsiFilenameClipboardFormat.AssignTo(Dest: TCustomDataFormat): boolean;
begin
  if (Dest is TFileDataFormat) then
  begin
    TFileDataFormat(Dest).Files.Clear;
    TFileDataFormat(Dest).Files.Add(Filename);
    Result := True;
  end else
    Result := inherited AssignTo(Dest);
end;


////////////////////////////////////////////////////////////////////////////////
//
//		TUnicodeFilenameClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
var
  CF_FILENAMEW: TClipFormat = 0;

function TUnicodeFilenameClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  if (CF_FILENAMEW = 0) then
    CF_FILENAMEW := RegisterClipboardFormat(CFSTR_FILENAMEW);
  Result := CF_FILENAMEW;
end;

class procedure TUnicodeFilenameClipboardFormat.RegisterCompatibleFormats;
begin
  inherited RegisterCompatibleFormats;
  RegisterDataConversion(TFileDataFormat, 3+UnicodeBoost);
end;

function TUnicodeFilenameClipboardFormat.Assign(Source: TCustomDataFormat): boolean;
begin
  if (Source is TFileDataFormat) then
  begin
    Result := (TFileDataFormat(Source).Files.Count > 0);
    if (Result) then
      Filename := TFileDataFormat(Source).Files[0];
  end else
    Result := inherited Assign(Source);
end;

function TUnicodeFilenameClipboardFormat.AssignTo(Dest: TCustomDataFormat): boolean;
begin
  if (Dest is TFileDataFormat) then
  begin
    TFileDataFormat(Dest).Files.Add(Filename);
    Result := True;
  end else
    Result := inherited AssignTo(Dest);
end;


////////////////////////////////////////////////////////////////////////////////
//
//		TAnsiFilenameMapClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
var
  CF_FILENAMEMAP: TClipFormat = 0;

constructor TAnsiFilenameMapClipboardFormat.Create;
begin
  inherited Create;
  FFileMaps := TStringList.Create;
end;

destructor TAnsiFilenameMapClipboardFormat.Destroy;
begin
  FFileMaps.Free;
  inherited Destroy;
end;

function TAnsiFilenameMapClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  if (CF_FILENAMEMAP = 0) then
    CF_FILENAMEMAP := RegisterClipboardFormat(CFSTR_FILENAMEMAPA);
  Result := CF_FILENAMEMAP;
end;

procedure TAnsiFilenameMapClipboardFormat.Clear;
begin
  FFileMaps.Clear;
end;

function TAnsiFilenameMapClipboardFormat.HasData: boolean;
begin
  Result := (FFileMaps.Count > 0);
end;

function TAnsiFilenameMapClipboardFormat.GetSize: integer;
var
  i: integer;
begin
  Result := FFileMaps.Count + 1;
  for i := 0 to FFileMaps.Count-1 do
    inc(Result, Length(FFileMaps[i]));
end;

function TAnsiFilenameMapClipboardFormat.ReadData(Value: pointer;
  Size: integer): boolean;
begin
  Result := ReadFilesFromZeroList(Value, Size, False, FFileMaps);
end;

function TAnsiFilenameMapClipboardFormat.WriteData(Value: pointer;
  Size: integer): boolean;
begin
  Result := WriteFilesToZeroList(Value, Size, False, FFileMaps);
end;


////////////////////////////////////////////////////////////////////////////////
//
//		TUnicodeFilenameMapClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
var
  CF_FILENAMEMAPW: TClipFormat = 0;

constructor TUnicodeFilenameMapClipboardFormat.Create;
begin
  inherited Create;
  FFileMaps := TWideStringList.Create;
end;

destructor TUnicodeFilenameMapClipboardFormat.Destroy;
begin
  FFileMaps.Free;
  inherited Destroy;
end;

function TUnicodeFilenameMapClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  if (CF_FILENAMEMAPW = 0) then
    CF_FILENAMEMAPW := RegisterClipboardFormat(CFSTR_FILENAMEMAPW);
  Result := CF_FILENAMEMAPW;
end;

procedure TUnicodeFilenameMapClipboardFormat.Clear;
begin
  FFileMaps.Clear;
end;

function TUnicodeFilenameMapClipboardFormat.HasData: boolean;
begin
  Result := (FFileMaps.Count > 0);
end;

function TUnicodeFilenameMapClipboardFormat.GetSize: integer;
var
  i: integer;
begin
  Result := FFileMaps.Count + 1;
  for i := 0 to FFileMaps.Count-1 do
{$ifdef DD_WIDESTRINGLIST}
    inc(Result, Length(FFileMaps.WideStrings[i]));
{$else}
    inc(Result, Length(FFileMaps[i]));
{$endif}
  inc(Result, Result);
end;

function TUnicodeFilenameMapClipboardFormat.ReadData(Value: pointer;
  Size: integer): boolean;
begin
  Result := ReadFilesFromZeroList(Value, Size, True, FFileMaps);
end;

function TUnicodeFilenameMapClipboardFormat.WriteData(Value: pointer;
  Size: integer): boolean;
begin
  Result := WriteFilesToZeroList(Value, Size, True, FFileMaps);
end;


////////////////////////////////////////////////////////////////////////////////
//
//		TFileGroupDescriptorCustomClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
procedure TFileGroupDescriptorCustomClipboardFormat.Clear;
begin
  if (FBuffer <> nil) then
  begin
    FreeMem(FBuffer);
    FBuffer := nil;
  end;
end;

procedure TFileGroupDescriptorCustomClipboardFormat.CopyFrom(Source: pointer);
var
  Size: integer;
begin
  Clear;
  if (Source <> nil) then
  begin
    Size := GetBufferSize(PFileGroupDescriptor(Source)^.cItems);
    GetMem(FBuffer, Size);
    Move(Source^, FBuffer^, Size);
  end;
end;

destructor TFileGroupDescriptorCustomClipboardFormat.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TFileGroupDescriptorCustomClipboardFormat.GetCount: integer;
begin
  ASSERT(pointer(@(PFileGroupDescriptorA(nil)^.cItems))=pointer(@(PFileGroupDescriptorW(nil)^.cItems)));
  if (FileGroupDescriptor <> nil) then
    Result := PFileGroupDescriptor(FileGroupDescriptor)^.cItems
  else
    Result := 0;
end;

function TFileGroupDescriptorCustomClipboardFormat.GetFileDescriptor(
  Index: integer): pointer;
begin
  ASSERT(pointer(@(PFileGroupDescriptorA(nil)^.cItems))=pointer(@(PFileGroupDescriptorW(nil)^.cItems)));
  if (FileGroupDescriptor <> nil) then
    Result := @(PFileGroupDescriptor(FileGroupDescriptor)^.fgd[Index])
  else
    Result := nil;
end;

function TFileGroupDescriptorCustomClipboardFormat.GetSize: integer;
begin
  if (FileGroupDescriptor <> nil) then
    Result := GetBufferSize(Count)
  else
    Result := 0;
end;

function TFileGroupDescriptorCustomClipboardFormat.HasData: boolean;
begin
  Result := (Count > 0);
end;

function TFileGroupDescriptorCustomClipboardFormat.ReadData(Value: pointer;
  Size: integer): boolean;
begin
  // Note: Some sources (e.g. Outlook) provides a larger buffer than is needed.
  ASSERT(pointer(@(PFileGroupDescriptorA(nil)^.cItems))=pointer(@(PFileGroupDescriptorW(nil)^.cItems)));
  Result := (Value <> nil) and
    (Size >= GetBufferSize(PFileGroupDescriptor(Value)^.cItems));
  if (Result) then
    CopyFrom(Value);
end;

procedure TFileGroupDescriptorCustomClipboardFormat.SetCount(
  const Value: integer);
var
  Size: integer;
begin
  Clear;
  Size := GetBufferSize(Value);
  GetMem(FBuffer, Size);
  FillChar(FBuffer^, Size, 0);
  PFileGroupDescriptor(FBuffer)^.cItems := Value;
end;

function TFileGroupDescriptorCustomClipboardFormat.WriteData(Value: pointer;
  Size: integer): boolean;
var
  BufferSize: integer;
begin
  ASSERT(pointer(@(PFileGroupDescriptorA(nil)^.cItems))=pointer(@(PFileGroupDescriptorW(nil)^.cItems)));
  BufferSize := GetBufferSize(Count);
  Result := (FileGroupDescriptor <> nil) and (Value <> nil) and
    (Size >= BufferSize);
  if (Result) then
    Move(FileGroupDescriptor^, Value^, BufferSize);
end;

////////////////////////////////////////////////////////////////////////////////
//
//		TAnsiFileGroupDescriptorClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
var
  CF_FILEGROUPDESCRIPTOR: TClipFormat = 0;

function TAnsiFileGroupDescriptorClipboardFormat.GetBufferSize(
  Count: integer): integer;
begin
  Result := SizeOf(UINT) + Count * SizeOf(TFileDescriptorA);
end;

function TAnsiFileGroupDescriptorClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  if (CF_FILEGROUPDESCRIPTOR = 0) then
    CF_FILEGROUPDESCRIPTOR := RegisterClipboardFormat(CFSTR_FILEDESCRIPTORA);
  Result := CF_FILEGROUPDESCRIPTOR;
end;

function TAnsiFileGroupDescriptorClipboardFormat.GetFileDescriptor(
  Index: integer): PFileDescriptorA;
begin
  Result := PFileDescriptorA(inherited GetFileDescriptor(Index));
end;

function TAnsiFileGroupDescriptorClipboardFormat.GetFileGroupDescriptor: PFileGroupDescriptorA;
begin
  Result := inherited FileGroupDescriptor;
end;

function TAnsiFileGroupDescriptorClipboardFormat.GetFilename(
  Index: integer): AnsiString;
var
  s: AnsiString;
begin
  if (Index >= Count) then
    raise Exception.CreateFmt('Filename index out of bounds (%d)', [Index]);
  SetLength(s, MAX_PATH);
  StrLCopy(PAnsiChar(s), @FileGroupDescriptor^.fgd[Index].cFileName[0], MAX_PATH);
  Result := PAnsiChar(s);
end;

procedure TAnsiFileGroupDescriptorClipboardFormat.SetFilename(Index: integer;
  const Value: AnsiString);
begin
  if (Index >= Count) then
    raise Exception.CreateFmt('Filename index out of bounds (%d)', [Index]);
  StrPLCopy(@FileGroupDescriptor^.fgd[Index].cFileName[0], Value, MAX_PATH);
end;

procedure TAnsiFileGroupDescriptorClipboardFormat.CopyFrom(AFileGroupDescriptor: PFileGroupDescriptorA);
begin
  inherited CopyFrom(AFileGroupDescriptor);
end;


////////////////////////////////////////////////////////////////////////////////
//
//		TUnicodeFileGroupDescriptorClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
var
  CF_FILEGROUPDESCRIPTORW: TClipFormat = 0;

function TUnicodeFileGroupDescriptorClipboardFormat.GetBufferSize(
  Count: integer): integer;
begin
  Result := SizeOf(UINT) + Count * SizeOf(TFileDescriptorW);
end;

function TUnicodeFileGroupDescriptorClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  if (CF_FILEGROUPDESCRIPTORW = 0) then
    CF_FILEGROUPDESCRIPTORW := RegisterClipboardFormat(CFSTR_FILEDESCRIPTORW);
  Result := CF_FILEGROUPDESCRIPTORW;
end;

function TUnicodeFileGroupDescriptorClipboardFormat.GetFileDescriptor(
  Index: integer): PFileDescriptorW;
begin
  Result := PFileDescriptorW(inherited GetFileDescriptor(Index));
end;

function TUnicodeFileGroupDescriptorClipboardFormat.GetFileGroupDescriptor: PFileGroupDescriptorW;
begin
  Result := inherited FileGroupDescriptor;
end;

function TUnicodeFileGroupDescriptorClipboardFormat.GetFilename(Index: integer): UnicodeString;
var
  s: UnicodeString;
begin
  if (Index >= Count) then
    raise Exception.CreateFmt('Filename index out of bounds (%d)', [Index]);
  SetLength(s, MAX_PATH);
  WStrLCopy(PWideChar(s), @FileGroupDescriptor^.fgd[Index].cFileName[0], MAX_PATH);
  Result := PWideChar(s);
end;

procedure TUnicodeFileGroupDescriptorClipboardFormat.SetFilename(Index: integer;
  const Value: UnicodeString);
begin
  if (Index >= Count) then
    raise Exception.CreateFmt('Filename index out of bounds (%d)', [Index]);
  WStrPLCopy(@FileGroupDescriptor^.fgd[Index].cFileName[0], Value, MAX_PATH);
end;

procedure TUnicodeFileGroupDescriptorClipboardFormat.CopyFrom(AFileGroupDescriptor: PFileGroupDescriptorW);
begin
  inherited CopyFrom(AFileGroupDescriptor);
end;


////////////////////////////////////////////////////////////////////////////////
//
//		TFileContentsClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
var
  CF_FILECONTENTS: TClipFormat = 0;

constructor TFileContentsClipboardFormat.Create;
begin
  // Note: Previous versions trimmed zeroes and added a terminating zero.
  // This just seems wrong as we shouldn't modify the content at this level. If
  // the data need to be trimmed or zero terminated it should be done at a
  // higher level. The purpose of this class is just to deliver the raw data.
  inherited Create;
  FFormatEtc.lindex := 0;
end;

function TFileContentsClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  if (CF_FILECONTENTS = 0) then
    CF_FILECONTENTS := RegisterClipboardFormat(CFSTR_FILECONTENTS);
  Result := CF_FILECONTENTS;
end;


////////////////////////////////////////////////////////////////////////////////
//
//		TFileContentsStreamClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
constructor TFileContentsStreamClipboardFormat.Create;
begin
  CreateFormat(TYMED_ISTREAM or TYMED_ISTORAGE);
  FStreams := TStreamList.Create;
end;

destructor TFileContentsStreamClipboardFormat.Destroy;
begin
  Clear;
  FStreams.Free;
  inherited Destroy;
end;

function TFileContentsStreamClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  if (CF_FILECONTENTS = 0) then
    CF_FILECONTENTS := RegisterClipboardFormat(CFSTR_FILECONTENTS);
  Result := CF_FILECONTENTS;
end;

procedure TFileContentsStreamClipboardFormat.Clear;
begin
  FStreams.Clear;
end;

function TFileContentsStreamClipboardFormat.HasData: boolean;
begin
  Result := (FStreams.Count > 0);
end;

class procedure TFileContentsStreamClipboardFormat.RegisterCompatibleFormats;
begin
  inherited RegisterCompatibleFormats;
  RegisterDataConsumer(TDataStreamDataFormat);
end;

function TFileContentsStreamClipboardFormat.AssignTo(Dest: TCustomDataFormat): boolean;
begin
  Result := True;
  if (Dest is TDataStreamDataFormat) then
  begin
    TDataStreamDataFormat(Dest).Streams.Assign(Streams);
  end else
    Result := inherited AssignTo(Dest);
end;

{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
function TFileContentsStreamClipboardFormat.GetData(const DataObject: IDataObject): boolean;
var
  AFormatEtc: TFormatEtc;
  FGD: TFileGroupDescriptorClipboardFormat;
  Count: integer;
  Medium: TStgMedium;
  Stream: IStream;
  Name: string;
  MemStream: TMemoryStream;
  StatStg: TStatStg;
  Size: longInt;
  Remaining: longInt;
  pChunk: PByte;
begin
  Result := False;

  Clear;
  FGD := TFileGroupDescriptorClipboardFormat.Create;
  try
    // Make copy of original FormatEtc and work with the copy.
    // If we modify the original, we *must* change it back when we are done with
    // it.
    AFormatEtc := FormatEtc;
    if (FGD.GetData(DataObject)) then
    begin
      // Multiple objects, retrieve one at a time
      Count := FGD.FileGroupDescriptor^.cItems;
      AFormatEtc.lindex := 0;
    end else
    begin
      // Single object, retrieve "all" at once
      Count := 0;
      AFormatEtc.lindex := -1;
      Name := '';
    end;
    while (AFormatEtc.lindex < Count) do
    begin
      FillChar(Medium, SizeOf(Medium), 0);
      if (Failed(DataObject.GetData(AFormatEtc, Medium))) then
        break;
      try
        inc(AFormatEtc.lindex);

        if (Medium.tymed = TYMED_ISTORAGE) then
        begin
          Stream := CreateIStreamFromIStorage(IStorage(Medium.stg));
          if (Stream = nil) then
          begin
            Result := False;
            break;
          end;
        end else
        if (Medium.tymed = TYMED_ISTREAM) then
          Stream := IStream(Medium.stm)
        else
          continue;

        Stream.Stat(StatStg, STATFLAG_NONAME);
        MemStream := TMemoryStream.Create;
        try
          Remaining := StatStg.cbSize;
          MemStream.Size := Remaining;
          pChunk := MemStream.Memory;

          // Fix for Outlook attachment paste bug #1.
          // Some versions of Outlook doesn't reset the stream position after we
          // have read data from the stream, so the next time we ask Outlook for
          // the same stream (e.g. by pasting the same attachment twice), we get
          // a stream where the current position is at EOS.
          Stream.Seek(0, STREAM_SEEK_SET, PLargeint(nil)^);

          while (Remaining > 0) do
          begin
            if (Failed(Stream.Read(pChunk, Remaining, @Size))) or
              (Size = 0) then
              break;
            inc(pChunk, Size);
            dec(Remaining, Size);
          end;
          // Fix for Outlook attachment paste bug  #2.
          // We reset the stream position here just to be nice to other
          // applications which might not have work arounds for this problem
          // (e.g. Windows Explorer).
          Stream.Seek(0, STREAM_SEEK_SET, PLargeint(nil)^);

          if (AFormatEtc.lindex > 0) then
            Name := FGD.FileGroupDescriptor^.fgd[AFormatEtc.lindex-1].cFileName;
          Streams.AddNamed(MemStream, Name);
        except
          MemStream.Free;
          raise;
        end;
        Stream := nil;
        Result := True;
      finally
        ReleaseStgMedium(Medium);
      end;
    end;
  finally
    FGD.Free;
  end;
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}


////////////////////////////////////////////////////////////////////////////////
//
//		TFileContentsStreamOnDemandClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
constructor TFileContentsStreamOnDemandClipboardFormat.Create;
begin
  // We also support TYMED_ISTORAGE for drop targets, but since we only support
  // TYMED_ISTREAM for both source and targets, we can't specify TYMED_ISTORAGE
  // here. See GetStream method.
  CreateFormat(TYMED_ISTREAM or TYMED_HGLOBAL);
end;

destructor TFileContentsStreamOnDemandClipboardFormat.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TFileContentsStreamOnDemandClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  if (CF_FILECONTENTS = 0) then
    CF_FILECONTENTS := RegisterClipboardFormat(CFSTR_FILECONTENTS);
  Result := CF_FILECONTENTS;
end;

procedure TFileContentsStreamOnDemandClipboardFormat.Clear;
begin
  FGotData := False;
  FDataRequested := False;
end;

function TFileContentsStreamOnDemandClipboardFormat.HasData: boolean;
begin
  Result := FGotData or FDataRequested;
end;

function TFileContentsStreamOnDemandClipboardFormat.AssignTo(Dest: TCustomDataFormat): boolean;
begin
  if (Dest is TVirtualFileStreamDataFormat) then
  begin
    Result := True;
  end else
    Result := inherited AssignTo(Dest);
end;

function TFileContentsStreamOnDemandClipboardFormat.Assign(
  Source: TCustomDataFormat): boolean;
begin
  if (Source is TVirtualFileStreamDataFormat) then
  begin
    // Acknowledge that we can offer the requested data, but defer the actual
    // data transfer.
    FDataRequested := True;
    Result := True
  end else
    Result := inherited Assign(Source);
end;

function TFileContentsStreamOnDemandClipboardFormat.DoSetData(
  const AFormatEtcIn: TFormatEtc; var AMedium: TStgMedium): boolean;
var
  Stream: IStream;
  Index: integer;
  StatStg: TStatStg;
  Data: pointer;
  ReadSize: longInt;
begin
  Result := False;
  Index := AFormatEtcIn.lindex;
  (*
  ** Warning:
  ** The meaning of the value -1 in FormatEtcIn.lindex is undocumented in this
  ** context (TYMED_ISTREAM), but can occur when pasting to the clipboard.
  ** Apparently the clipboard doesn't use the stream returned from a call with
  ** lindex = -1, but only uses it as a test to see if data is available.
  ** When the clipboard actually needs the data it will specify correct values
  ** for lindex.
  ** In version 4.0 we rejected the call if -1 was specified, but in order to
  ** support clipboard operations we now map -1 to 0.
  *)
  if (Index = -1) then
    Index := 0;

  if (Assigned(FOnGetStream)) and (AFormatEtcIn.tymed and FormatEtc.tymed <> 0) and
    (Index >= 0) then
  begin
    FOnGetStream(Self, Index, Stream);

    if (Stream <> nil) then
    begin
      if (AFormatEtcIn.tymed and TYMED_ISTREAM <> 0) then
      begin
        IStream(AMedium.stm) := Stream;
        AMedium.tymed := TYMED_ISTREAM;
        Result := True;
      end else
      if (AFormatEtcIn.tymed and TYMED_HGLOBAL <> 0) then
      begin
        if (Failed(Stream.Stat(StatStg, STATFLAG_NONAME))) then
          exit;
        AMedium.hGlobal := GlobalAlloc(GMEM_MOVEABLE or GMEM_ZEROINIT, StatStg.cbSize);
        if (AMedium.hGlobal <> 0) then
          try
            try
              Data := GlobalLock(AMedium.hGlobal);
              try
                OleCheck(Stream.Seek(0, STREAM_SEEK_SET, PLargeint(nil)^));
                OleCheck(Stream.Read(Data, StatStg.cbSize, @ReadSize));
                Result := (ReadSize = StatStg.cbSize);
              finally
                GlobalUnlock(AMedium.hGlobal);
              end;
            finally
              if (not Result) then
              begin
                GlobalFree(AMedium.hGlobal);
                AMedium.hGlobal := 0;
              end;
            end;
          except
            // Eat exceptions since they wont work inside drag/drop anyway.
            Result := False;
          end;
      end;
    end;
  end;
end;

function TFileContentsStreamOnDemandClipboardFormat.GetData(const DataObject: IDataObject): boolean;
begin
  // Flag that data has been offered to us, but defer the actual data transfer.
  FGotData := True;
  Result := True;
end;

function TFileContentsStreamOnDemandClipboardFormat.GetStream(Index: integer): IStream;
var
  Medium: TStgMedium;
  AFormatEtc: TFormatEtc;
begin
  Result := nil;
  // Get an IStream interface from the source.
  AFormatEtc := FormatEtc;
  AFormatEtc.tymed := AFormatEtc.tymed or TYMED_ISTORAGE;
  AFormatEtc.lindex := Index;
  if (Succeeded((DataFormat.Owner as TCustomDroptarget).DataObject.GetData(AFormatEtc,
    Medium))) then
    try
      case Medium.tymed of
        TYMED_ISTREAM:
          Result := IStream(Medium.stm);
        TYMED_ISTORAGE:
          Result := CreateIStreamFromIStorage(IStorage(Medium.stg));
        TYMED_HGLOBAL:
          if (Succeeded(CreateStreamOnHGlobal(Medium.hGlobal,
            (Medium.unkForRelease = nil), Result))) then
            // Prevent ReleaseStgMedium from freeing the Medium.hGlobal.
            Medium.hGlobal := 0
          else
            Result := nil;
      end;
    finally
      ReleaseStgMedium(Medium);
    end;
end;


////////////////////////////////////////////////////////////////////////////////
//
//		TFileContentsStorageClipboardFormat
//
////////////////////////////////////////////////////////////////////////////////
constructor TFileContentsStorageClipboardFormat.Create;
begin
  CreateFormat(TYMED_ISTORAGE);
  FStorages := TStorageInterfaceList.Create;
end;

destructor TFileContentsStorageClipboardFormat.Destroy;
begin
  Clear;
  FStorages.Free;
  inherited Destroy;
end;

function TFileContentsStorageClipboardFormat.GetClipboardFormat: TClipFormat;
begin
  if (CF_FILECONTENTS = 0) then
    CF_FILECONTENTS := RegisterClipboardFormat(CFSTR_FILECONTENTS);
  Result := CF_FILECONTENTS;
end;

procedure TFileContentsStorageClipboardFormat.Clear;
begin
  FStorages.Clear;
end;

function TFileContentsStorageClipboardFormat.HasData: boolean;
begin
  Result := (FStorages.Count > 0);
end;

function TFileContentsStorageClipboardFormat.AssignTo(Dest: TCustomDataFormat): boolean;
begin
(*
  Result := True;
  if (Dest is TDataStreamDataFormat) then
  begin
    TDataStreamDataFormat(Dest).Streams.Assign(Streams);
  end else
*)
    Result := inherited AssignTo(Dest);
end;

{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
function TFileContentsStorageClipboardFormat.GetData(const DataObject: IDataObject): boolean;
var
  FGD: TAnsiFileGroupDescriptorClipboardFormat;
  Count: integer;
  Medium: TStgMedium;
  Storage: IStorage;
  Name: string;
  AFormatEtc: TFormatEtc;
begin
  Result := False;

  Clear;
  // The FileContents formats is always accompanied by the FileGroupDescriptor
  // format, so we can get the names from the FileGroupDescriptor format.
  FGD := TAnsiFileGroupDescriptorClipboardFormat.Create;
  try
    // Work on a temporary copy of the FormatEtc structure so we can modify it
    // without side effects (Thanks Tom!).
    AFormatEtc := FormatEtc;
    if (FGD.GetData(DataObject)) then
    begin
      // Multiple objects, retrieve one at a time
      Count := FGD.FileGroupDescriptor^.cItems;
      AFormatEtc.lindex := 0;
    end else
    begin
      // Single object, retrieve "all" at once
      Count := 0;
      AFormatEtc.lindex := -1;
      Name := '';
    end;

    while (AFormatEtc.lindex < Count) do
    begin
      if (Failed(DataObject.GetData(AFormatEtc, Medium))) then
        break;
      try
        inc(AFormatEtc.lindex);
        if (Medium.tymed <> TYMED_ISTORAGE) then
          continue;
        Storage := IStorage(Medium.stg);
        try
          if (AFormatEtc.lindex > 0) then
            Name := FGD.FileGroupDescriptor^.fgd[AFormatEtc.lindex-1].cFileName;
          Storages.AddNamed(Storage, Name);
        finally
          Storage := nil;
        end;
        Result := True;
      finally
        ReleaseStgMedium(Medium);
      end;
    end;
  finally
    FGD.Free;
  end;
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}


////////////////////////////////////////////////////////////////////////////////
//
//		TFileDescriptorToFilenameStrings
//		TFileDescriptorWToFilenameStrings
//
////////////////////////////////////////////////////////////////////////////////
// Used internally to convert between FileDescriptors and filenames on-demand.
////////////////////////////////////////////////////////////////////////////////
// TODO : Need Unicode/WideString support for pre D2009
type
  TFileDescriptorToFilenameStrings = class(TStrings)
  private
    FFileDescriptors: TMemoryList;
    FObjects: TList;
  protected
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    function GetObject(Index: Integer): TObject; override;
  public
    constructor Create(AFileDescriptors: TMemoryList);
    destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
    procedure Assign(Source: TPersistent); override;
  end;

constructor TFileDescriptorToFilenameStrings.Create(AFileDescriptors: TMemoryList);
begin
  inherited Create;
  FFileDescriptors := AFileDescriptors;
  FObjects := TList.Create;
end;

destructor TFileDescriptorToFilenameStrings.Destroy;
begin
  FObjects.Free;
  inherited Destroy;
end;

function TFileDescriptorToFilenameStrings.Get(Index: Integer): string;
begin
  Result := PFileDescriptorW(FFileDescriptors[Index]).cFileName;
end;

function TFileDescriptorToFilenameStrings.GetCount: Integer;
begin
  Result := FFileDescriptors.Count;
end;

procedure TFileDescriptorToFilenameStrings.Assign(Source: TPersistent);
var
  i: integer;
begin
  if Source is TStrings then
  begin
    BeginUpdate;
    try
      FFileDescriptors.Clear;
      for i := 0 to TStrings(Source).Count-1 do
        AddObject(TStrings(Source)[i], TStrings(Source).Objects[i]);
    finally
      EndUpdate;
    end;
  end else
    inherited Assign(Source);
end;

procedure TFileDescriptorToFilenameStrings.Clear;
begin
  FFileDescriptors.Clear;
  FObjects.Clear;
end;

procedure TFileDescriptorToFilenameStrings.Delete(Index: Integer);
begin
  FFileDescriptors.Delete(Index);
  FObjects.Delete(Index);
end;

procedure TFileDescriptorToFilenameStrings.Insert(Index: Integer; const S: string);
var
  FD: PFileDescriptorW;
begin
  if (Index = FFileDescriptors.Count) then
  begin
    GetMem(FD, SizeOf(TFileDescriptorW));
    try
      FillChar(FD^, SizeOf(TFileDescriptorW), 0);
      StringToWideChar(S, FD.cFileName, SizeOf(FD.cFileName));
      FFileDescriptors.Add(FD);
      FObjects.Add(nil);
    except
      FreeMem(FD);
      raise;
    end;
  end;
end;

procedure TFileDescriptorToFilenameStrings.PutObject(Index: Integer;
  AObject: TObject);
begin
  FObjects[Index] := AObject;
end;

function TFileDescriptorToFilenameStrings.GetObject(Index: Integer): TObject;
begin
  Result := FObjects[Index];
end;


////////////////////////////////////////////////////////////////////////////////
//
//		TVirtualFileStreamDataFormat
//
////////////////////////////////////////////////////////////////////////////////
constructor TVirtualFileStreamDataFormat.Create(AOwner: TDragDropComponent);
begin
  inherited Create(AOwner);
  FFileDescriptors := TMemoryList.Create;
  FFileNames := TFileDescriptorToFilenameStrings.Create(FFileDescriptors);

  FFileContentsClipboardFormat := TFileContentsStreamOnDemandClipboardFormat(CompatibleFormats.GetFormat(TFileContentsStreamOnDemandClipboardFormat));
  FAnsiFileGroupDescriptorClipboardFormat := TAnsiFileGroupDescriptorClipboardFormat(CompatibleFormats.GetFormat(TAnsiFileGroupDescriptorClipboardFormat));
  FUnicodeFileGroupDescriptorClipboardFormat := TUnicodeFileGroupDescriptorClipboardFormat(CompatibleFormats.GetFormat(TUnicodeFileGroupDescriptorClipboardFormat));
end;

destructor TVirtualFileStreamDataFormat.Destroy;
begin
  FFileDescriptors.Free;
  FFileNames.Free;
  inherited Destroy;
end;

procedure TVirtualFileStreamDataFormat.SetFileNames(const Value: TStrings);
begin
  FFileNames.Assign(Value);
end;

{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
function TVirtualFileStreamDataFormat.Assign(Source: TClipboardFormat): boolean;
var
  i: integer;
  FDA: PFileDescriptorA;
  FDW: PFileDescriptorW;
begin
  Result := True;

  (*
  ** TFileContentsStreamOnDemandClipboardFormat
  *)
  if (Source is TFileContentsStreamOnDemandClipboardFormat) then
  begin
    FHasContents := TFileContentsStreamOnDemandClipboardFormat(Source).HasData;
  end else
  (*
  ** TAnsiFileGroupDescriptorClipboardFormat
  *)
  if (Source is TAnsiFileGroupDescriptorClipboardFormat) then
  begin
    FFileDescriptors.Clear;
    for i := 0 to TAnsiFileGroupDescriptorClipboardFormat(Source).FileGroupDescriptor^.cItems-1 do
    begin
      GetMem(FDW, SizeOf(TFileDescriptorW));
      try
        // Copy the common A&W part

        // Argh! Delphi can't compute SizeOf a type member at compile time
        // ASSERT(SizeOf(TFileDescriptorA)-SizeOf(TFileDescriptorA.cFileName) = SizeOf(TFileDescriptorW)-SizeOf(TFileDescriptorW.cFileName));
        ASSERT(pointer(@PFileDescriptorA(nil)^.cFileName) = pointer(@PFileDescriptorW(nil)^.cFileName));

        FDA := @TAnsiFileGroupDescriptorClipboardFormat(Source).FileGroupDescriptor^.fgd[i];
        Move(FDA^, FDW^, integer(@PFileDescriptorA(nil)^.cFileName));
        // Convert and copy the Ansi string to Unicode
        MultiByteToWideChar(DefaultSystemCodePage, 0,
          FDA^.cFileName, SizeOf(FDA^.cFileName),
          FDW^.cFileName, SizeOf(FDW^.cFileName));
        FFileDescriptors.Add(FDW);
      except
        FreeMem(FDW);
        raise;
      end;
    end;
  end else
  (*
  ** TUnicodeFileGroupDescriptorClipboardFormat
  *)
  if (Source is TUnicodeFileGroupDescriptorClipboardFormat) then
  begin
    FFileDescriptors.Clear;
    for i := 0 to TUnicodeFileGroupDescriptorClipboardFormat(Source).FileGroupDescriptor^.cItems-1 do
    begin
      GetMem(FDW, SizeOf(TFileDescriptorW));
      try
        Move(TUnicodeFileGroupDescriptorClipboardFormat(Source).FileGroupDescriptor^.fgd[i],
          FDW^, SizeOf(TFileDescriptorW));
        FFileDescriptors.Add(FDW);
      except
        FreeMem(FDW);
        raise;
      end;
    end;
  end else
  (*
  ** None of the above...
  *)
    Result := inherited Assign(Source);
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}

{$IFOPT R+}
  {$DEFINE R_PLUS}
  {$RANGECHECKS OFF}
{$ENDIF}
function TVirtualFileStreamDataFormat.AssignTo(Dest: TClipboardFormat): boolean;
var
  FGDA: PFileGroupDescriptorA;
  FGDW: PFileGroupDescriptorW;
  i: integer;
begin
  (*
  ** TFileContentsStreamOnDemandClipboardFormat
  *)
  if (Dest is TFileContentsStreamOnDemandClipboardFormat) then
  begin
    // Let the clipboard format handle the transfer.
    // No data is actually transferred, but TFileContentsStreamOnDemandClipboardFormat
    // needs to set a flag when data is requested.
    Result := Dest.Assign(Self);
  end else
  (*
  ** TAnsiFileGroupDescriptorClipboardFormat
  *)
  if (Dest is TAnsiFileGroupDescriptorClipboardFormat) then
  begin
    if (FFileDescriptors.Count > 0) then
    begin
      // Copy the common A&W part
      // Argh! Delphi can't compute SizeOf a type member at compile time
      // ASSERT(SizeOf(TFileDescriptorA)-SizeOf(TFileDescriptorA.cFileName) = SizeOf(TFileDescriptorW)-SizeOf(TFileDescriptorW.cFileName));
      ASSERT(pointer(@PFileDescriptorA(nil)^.cFileName) = pointer(@PFileDescriptorW(nil)^.cFileName));

      GetMem(FGDA, SizeOf(UINT) + FFileDescriptors.Count * SizeOf(TFileDescriptorA));
      try
        FGDA.cItems := FFileDescriptors.Count;
        for i := 0 to FFileDescriptors.Count-1 do
        begin
          Move(FFileDescriptors[i]^, FGDA.fgd[i], integer(@PFileDescriptorA(nil)^.cFileName));
          // Convert and copy the Unicode string to Ansi
          WideCharToMultiByte(DefaultSystemCodePage, 0,
            PFileDescriptorW(FFileDescriptors[i])^.cFileName,
              SizeOf(PFileDescriptorW(FFileDescriptors[i])^.cFileName),
            FGDA.fgd[i].cFileName,
              SizeOf(FGDA.fgd[i].cFileName),
              nil, nil);
        end;
        TAnsiFileGroupDescriptorClipboardFormat(Dest).CopyFrom(FGDA);
      finally
        FreeMem(FGDA);
      end;
      Result := True;
    end else
      Result := False;
  end else
  (*
  ** TUnicodeFileGroupDescriptorClipboardFormat
  *)
  if (Dest is TUnicodeFileGroupDescriptorClipboardFormat) then
  begin
    if (FFileDescriptors.Count > 0) then
    begin
      GetMem(FGDW, SizeOf(UINT) + FFileDescriptors.Count * SizeOf(TFileDescriptorW));
      try
        FGDW.cItems := FFileDescriptors.Count;
        for i := 0 to FFileDescriptors.Count-1 do
          Move(FFileDescriptors[i]^, FGDW.fgd[i], SizeOf(TFileDescriptorW));
        TUnicodeFileGroupDescriptorClipboardFormat(Dest).CopyFrom(FGDW);
      finally
        FreeMem(FGDW);
      end;
      Result := True;
    end else
      Result := False;
  end else
  (*
  ** None of the above...
  *)
    Result := inherited AssignTo(Dest);
end;
{$IFDEF R_PLUS}
  {$RANGECHECKS ON}
  {$UNDEF R_PLUS}
{$ENDIF}

procedure TVirtualFileStreamDataFormat.Clear;
begin
  FFileDescriptors.Clear;
  FHasContents := False;
end;

function TVirtualFileStreamDataFormat.HasData: boolean;
begin
  Result := (FFileDescriptors.Count > 0) and
    ((FHasContents) or Assigned(FFileContentsClipboardFormat.OnGetStream));
end;

function TVirtualFileStreamDataFormat.NeedsData: boolean;
begin
  Result := (FFileDescriptors.Count = 0) or (not FHasContents);
end;

class procedure TVirtualFileStreamDataFormat.RegisterCompatibleFormats;
begin
  inherited RegisterCompatibleFormats;

  RegisterDataConversion(TFileContentsStreamOnDemandClipboardFormat);
  RegisterDataConversion(TAnsiFileGroupDescriptorClipboardFormat, 1);
  RegisterDataConversion(TUnicodeFileGroupDescriptorClipboardFormat, 0);
end;

function TVirtualFileStreamDataFormat.GetFileDescriptor(Index: integer): PFileDescriptorW;
begin
  Result := PFileDescriptorW(FFileDescriptors[Index]);
end;

function TVirtualFileStreamDataFormat.GetOnGetStream: TOnGetStreamEvent;
begin
  Result := FFileContentsClipboardFormat.OnGetStream;
end;

procedure TVirtualFileStreamDataFormat.SetOnGetStream(const Value: TOnGetStreamEvent);
begin
  FFileContentsClipboardFormat.OnGetStream := Value;
end;


////////////////////////////////////////////////////////////////////////////////
//
//		TFileMapDataFormat
//
////////////////////////////////////////////////////////////////////////////////
constructor TFileMapDataFormat.Create(AOwner: TDragDropComponent);
begin
  inherited Create(AOwner);
  // TODO : This should be a TWideStringList
  FFileMaps := TStringList.Create;
  TStringList(FFileMaps).OnChanging := DoOnChanging;
end;

destructor TFileMapDataFormat.Destroy;
begin
  FFileMaps.Free;
  inherited Destroy;
end;

function TFileMapDataFormat.Assign(Source: TClipboardFormat): boolean;
begin
  Result := True;

  if (Source is TAnsiFilenameMapClipboardFormat) then
    FFileMaps.Assign(TAnsiFilenameMapClipboardFormat(Source).FileMaps)

  else if (Source is TUnicodeFilenameMapClipboardFormat) then
    FFileMaps.Assign(TUnicodeFilenameMapClipboardFormat(Source).FileMaps)

  else
    Result := inherited Assign(Source);
end;

function TFileMapDataFormat.AssignTo(Dest: TClipboardFormat): boolean;
begin
  Result := True;

  if (Dest is TAnsiFilenameMapClipboardFormat) then
    TAnsiFilenameMapClipboardFormat(Dest).FileMaps.Assign(FFileMaps)

  else if (Dest is TUnicodeFilenameMapClipboardFormat) then
    TUnicodeFilenameMapClipboardFormat(Dest).FileMaps.Assign(FFileMaps)

  else
    Result := inherited AssignTo(Dest);
end;

procedure TFileMapDataFormat.Clear;
begin
  FFileMaps.Clear;
end;

function TFileMapDataFormat.HasData: boolean;
begin
  Result := (FFileMaps.Count > 0);
end;

function TFileMapDataFormat.NeedsData: boolean;
begin
  Result := (FFileMaps.Count = 0);
end;


class procedure TFileMapDataFormat.RegisterCompatibleFormats;
begin
  inherited RegisterCompatibleFormats;

  RegisterDataConversion(TAnsiFilenameMapClipboardFormat, AnsiBoost);
  RegisterDataConversion(TUnicodeFilenameMapClipboardFormat, UnicodeBoost);
end;

////////////////////////////////////////////////////////////////////////////////
//
//		TFileDataFormat
//
////////////////////////////////////////////////////////////////////////////////
constructor TFileDataFormat.Create(AOwner: TDragDropComponent);
begin
  inherited Create(AOwner);
  FFiles := TWideStringList.Create;
  TWideStringList(FFiles).OnChanging := DoOnChanging;
  // FFiles := TStringList.Create;
  // TStringList(FFiles).OnChanging := DoOnChanging;
end;

destructor TFileDataFormat.Destroy;
begin
  FFiles.Free;
  inherited Destroy;
end;

function TFileDataFormat.Assign(Source: TClipboardFormat): boolean;
begin
  Result := True;

  if (Source is TFileClipboardFormat) then
    FFiles.Assign(TFileClipboardFormat(Source).Files)

  else if (Source is TPIDLClipboardFormat) then
    FFiles.Assign(TPIDLClipboardFormat(Source).Filenames)

  else
    Result := inherited Assign(Source);
end;

function TFileDataFormat.AssignTo(Dest: TClipboardFormat): boolean;
begin
  Result := True;

  if (Dest is TFileClipboardFormat) then
    TFileClipboardFormat(Dest).Files.Assign(FFiles)

  else if (Dest is TPIDLClipboardFormat) then
    TPIDLClipboardFormat(Dest).Filenames.Assign(FFiles)

  else
    Result := inherited AssignTo(Dest);
end;

procedure TFileDataFormat.Clear;
begin
  FFiles.Clear;
end;

function TFileDataFormat.HasData: boolean;
begin
  Result := (FFiles.Count > 0);
end;

function TFileDataFormat.NeedsData: boolean;
begin
  Result := (FFiles.Count = 0);
end;


class procedure TFileDataFormat.RegisterCompatibleFormats;
begin
  inherited RegisterCompatibleFormats;

  RegisterDataConversion(TFileClipboardFormat, 0);
  RegisterDataConversion(TPIDLClipboardFormat, 1);
end;

////////////////////////////////////////////////////////////////////////////////
//
//		TDropFileTarget
//
////////////////////////////////////////////////////////////////////////////////
constructor TDropFileTarget.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OptimizedMove := True;

  FFileFormat := TFileDataFormat.Create(Self);
  FFileMapFormat := TFileMapDataFormat.Create(Self);
end;

destructor TDropFileTarget.Destroy;
begin
  FFileFormat.Free;
  FFileMapFormat.Free;
  inherited Destroy;
end;

function TDropFileTarget.GetFiles: TStrings;
begin
  Result := FFileFormat.Files;
end;

function TDropFileTarget.GetMappedNames: TStrings;
begin
  Result := FFileMapFormat.FileMaps;
end;

function TDropFileTarget.GetPreferredDropEffect: LongInt;
begin
  // TODO : Needs explanation of why this is nescessary.
  Result := inherited GetPreferredDropEffect;
  if (Result = DROPEFFECT_NONE) then
    Result := DROPEFFECT_COPY;
end;


////////////////////////////////////////////////////////////////////////////////
//
//		TDropFileSource
//
////////////////////////////////////////////////////////////////////////////////
constructor TDropFileSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FFileFormat := TFileDataFormat.Create(Self);
  FFileMapFormat := TFileMapDataFormat.Create(Self);
end;

destructor TDropFileSource.Destroy;
begin
  FFileFormat.Free;
  FFileMapFormat.Free;
  inherited Destroy;
end;

function TDropFileSource.GetFiles: TStrings;
begin
  Result := FFileFormat.Files;
end;

function TDropFileSource.GetMappedNames: TStrings;
begin
  Result := FFileMapFormat.FileMaps;
end;

procedure TDropFileSource.SetFiles(AFiles: TStrings);
begin
  FFileFormat.Files.Assign(AFiles);
end;

procedure TDropFileSource.SetMappedNames(ANames: TStrings);
begin
  FFileMapFormat.FileMaps.Assign(ANames);
end;


////////////////////////////////////////////////////////////////////////////////
//
//		TWideStrings
//
////////////////////////////////////////////////////////////////////////////////
{$ifdef DD_WIDESTRINGLIST}
function TWideStrings.Add(const S: UnicodeString): Integer;
begin
  Result := inherited Add(S);
end;

function TWideStrings.AddObject(const S: UnicodeString; AObject: TObject): Integer;
begin
  Result := inherited AddObject(S, AObject);
end;
function TWideStrings.IndexOf(const S: UnicodeString): Integer;
begin
  Result := inherited IndexOf(S);
end;

{$endif}

////////////////////////////////////////////////////////////////////////////////
//
//		TWideStringList
//
////////////////////////////////////////////////////////////////////////////////
{$ifdef DD_WIDESTRINGLIST}
function TWideStringList.Add(const S: UnicodeString): Integer;
var
  PWStr: ^TWString;
begin
  Changing;
  New(PWStr);
  PWStr^.WString := S;
  Result := FWideStringList.Add(PWStr);
  Changed;
end;

function TWideStringList.AddObject(const S: UnicodeString; AObject: TObject): Integer;
begin
  Result := Add(S);
  PutObject(Result, AObject);
end;

procedure TWideStringList.AddStrings(Strings: TStrings);
var
  I: Integer;
begin
  if (Strings.Wide) then
  begin
    BeginUpdate;
    try
      for I := 0 to Strings.Count - 1 do
        AddObjectWide(Strings.WideStrings[I], Strings.Objects[I]);
    finally
      EndUpdate;
    end;
  end else
    inherited AddStrings(Strings);
end;

procedure TWideStringList.Changed;
begin
  if (UpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TWideStringList.Changing;
begin
  if (UpdateCount = 0) and Assigned(FOnChanging) then
    FOnChanging(Self);
end;

procedure TWideStringList.Clear;
var
  Index: Integer;
  PWStr: ^TWString;
begin
  Changing;
  for Index := 0 to FWideStringList.Count-1 do
  begin
    PWStr := FWideStringList.Items[Index];
    if PWStr <> nil then
      Dispose(PWStr);
  end;
  FWideStringList.Clear;
  Changed;
end;

constructor TWideStringList.Create;
begin
  inherited Create;
  FWideStringList := TList.Create;
end;

procedure TWideStringList.Delete(Index: Integer);
var
  PWStr: ^TWString;
begin
  Changing;
  if (Index < 0) or (Index >= Count) then
    Error(SListIndexError, Index);
  PWStr := FWideStringList.Items[Index];
  if PWStr <> nil then
    Dispose(PWStr);
  FWideStringList.Delete(Index);
  Changed;
end;

destructor TWideStringList.Destroy;
begin
  Clear;
  FWideStringList.Free;

  inherited Destroy;
end;

function TWideStringList.Get(Index: Integer): string;
begin
  Result := GetWide(Index);
end;

function TWideStringList.GetCount: Integer;
begin
  Result := FWideStringList.Count;
end;

function TWideStringList.GetWide(Index: Integer): UnicodeString;
var
  PWStr: ^TWString;
begin
  Result := '';
  if ( (Index >= 0) and (Index < FWideStringList.Count) ) then
  begin
    PWStr := FWideStringList.Items[Index];
    if PWStr <> nil then
      Result := PWStr^.WString;
  end;
end;

function TWideStringList.IndexOf(const S: UnicodeString): Integer;
var
  Index: Integer;
  PWStr: ^TWString;
begin
  Result := -1;
  for Index := 0 to FWideStringList.Count -1 do
  begin
    PWStr := FWideStringList.Items[Index];
    if PWStr <> nil then
    begin
      if S = PWStr^.WString then
      begin
        Result := Index;
        break;
      end;
    end;
  end;
end;

procedure TWideStringList.Insert(Index: Integer; const S: string);
begin
  Insert(Index, WideString(S));
end;

procedure TWideStringList.Insert(Index: Integer; const S: UnicodeString);
var
  PWStr: ^TWString;
begin
  if((Index < 0) or (Index > FWideStringList.Count)) then
    Error(SListIndexError, Index);
  if Index < FWideStringList.Count then
  begin
    PWStr := FWideStringList.Items[Index];
    if PWStr <> nil then
      PWStr.WString := S;
  end
  else
    Add(S);
end;

procedure TWideStringList.Put(Index: Integer; const S: string);
begin
  Changing;
  inherited Put(Index, S);
  Changed;
end;

procedure TWideStringList.PutObject(Index: Integer; AObject: TObject);
begin
  Changing;
  inherited PutObject(Index, AObject);
  Changed;
end;

procedure TWideStringList.PutWide(Index: Integer; const S: UnicodeString);
begin
  Insert(Index, S);
end;

procedure TWideStringList.SetUpdateState(Updating: Boolean);
begin
  if Updating then Changing else Changed;
end;

function TWideStringsHelper.Add(const S: UnicodeString): Integer;
begin
  if (Wide) then
    Result := AddWide(S)
  else
    Result := inherited Add(S);
end;

function TWideStringsHelper.AddObject(const S: UnicodeString;
  AObject: TObject): Integer;
begin
  if (Wide) then
    Result := AddObjectWide(S, AObject)
  else
    Result := inherited AddObject(S, AObject);
end;

function TWideStringsHelper.AddObjectWide(const S: UnicodeString;
  AObject: TObject): Integer;
begin
  if (Wide) then
    Result := TWideStrings(Self).AddObject(S, AObject)
  else
    Result := inherited AddObject(S, AObject);
end;

function TWideStringsHelper.AddWide(const S: UnicodeString): Integer;
begin
  if (Wide) then
    Result := TWideStrings(Self).Add(S)
  else
    Result := inherited Add(S);
end;

function TWideStringsHelper.GetWide(Index: Integer): UnicodeString;
begin
  if (Wide) then
    Result := TWideStrings(Self).GetWide(Index)
  else
    Result := Get(Index);
end;

function TWideStringsHelper.GetWideText: UnicodeString;
var
  I, L, Size, Count: Integer;
  P: PWideChar;
  S, LB: WideString;
begin
  if (Wide) then
  begin
    Count := GetCount;
    Size := 0;
    LB := LineBreak;
    for I := 0 to Count - 1 do
      Inc(Size, Length(GetWide(I)) + Length(LB));

    SetLength(Result, Size);
    P := Pointer(Result);
    for I := 0 to Count - 1 do
    begin
      S := GetWide(I);
      L := Length(S);
      if L <> 0 then
      begin
        System.Move(Pointer(S)^, P^, L*2);
        Inc(P, L);
      end;
      L := Length(LB);
      if L <> 0 then
      begin
        System.Move(Pointer(LB)^, P^, L*2);
        Inc(P, L);
      end;
    end;
  end else
    Result := GetTextStr;
end;

function TWideStringsHelper.IndexOf(const S: UnicodeString): Integer;
begin
  if (Wide) then
    Result := IndexOfWide(S)
  else
    Result := inherited IndexOf(S);
end;

function TWideStringsHelper.IndexOfWide(const S: UnicodeString): Integer;
begin
  Result := TWideStrings(Self).IndexOf(S);
end;

procedure TWideStringsHelper.Insert(Index: Integer; const S: UnicodeString);
begin
  if (Wide) then
    InsertWide(Index, S)
  else
    inherited Insert(Index, S);
end;

procedure TWideStringsHelper.InsertWide(Index: Integer; const S: UnicodeString);
begin
  TWideStrings(Self).Insert(Index, S);
end;

function TWideStringsHelper.IsWide: boolean;
begin
  Result := (Self is TWideStrings);
end;

procedure TWideStringsHelper.PutWide(Index: Integer; const S: UnicodeString);
begin
  if (Wide) then
    TWideStrings(Self).PutWide(Index, S)
  else
    Put(Index, S);
end;
{$endif}

////////////////////////////////////////////////////////////////////////////////
//
//		Initialization/Finalization
//
////////////////////////////////////////////////////////////////////////////////

initialization
  // Data format registration
  TFileDataFormat.RegisterDataFormat;
  TFileMapDataFormat.RegisterDataFormat;
  TVirtualFileStreamDataFormat.RegisterDataFormat;

  // Clipboard format registration
  TAnsiFilenameClipboardFormat.RegisterFormat;
  TFileClipboardFormat.RegisterFormat;
  TPIDLClipboardFormat.RegisterFormat;
  TAnsiFilenameClipboardFormat.RegisterFormat;
  TUnicodeFilenameClipboardFormat.RegisterFormat;
  TAnsiFilenameMapClipboardFormat.RegisterFormat;
  TUnicodeFilenameMapClipboardFormat.RegisterFormat;
  TAnsiFileGroupDescriptorClipboardFormat.RegisterFormat;
  TUnicodeFileGroupDescriptorClipboardFormat.RegisterFormat;
  TFileContentsClipboardFormat.RegisterFormat;
  TFileContentsStreamClipboardFormat.RegisterFormat;
  TFileContentsStorageClipboardFormat.RegisterFormat;

finalization

end.
