{*******************************************************}
{                                                       }
{       Extension Library                               }
{       Utilites Unit                                   }
{                                                       }
{       (c) 1999 - 2001, Balabuyev Yevgeny              }
{       E-mail: stalcer@rambler.ru                      }
{                                                       }
{*******************************************************}

unit ELUtils;

{$H+}

interface

uses
  Windows, Classes, SysUtils;

  { Strings streaming routines }

function ELPackStrings(AValue: TStrings): string;
procedure ELUnpackStrings(const AValue: string; Result: TStrings);

procedure ELSaveStringToStream(const AStr: string; AStream: TStream);
function ELLoadStringFromStream(AStream: TStream): string;


  { String masks with wincard symbols (*, ?) }

type
  TELMaskInfo = record
    BeginStar: Boolean;
    EndStar: Boolean;
    Strings: array of string;
    StLength: array of Integer;
    Count: Integer;
  end;

function ELPrepareMask(const AMask: string): TELMaskInfo;
function ELMaskCompare(const AStr: string; const AMask: TELMaskInfo): Boolean;


  { Quick sort }

type
  TELQSCompareProc = function(AItemIndex1, AItemIndex2: Integer;
    ACustomPar: Pointer): Integer;
  TELQSExchangeProc = procedure(AItemIndex1, AItemIndex2: Integer;
    ACustomPar: Pointer);

procedure ELQuickSort(AFromIndex, AToIndex: Integer;
  ACompareProc: TELQSCompareProc; AExchangeProc: TELQSExchangeProc;
  ACustomPar: Pointer);


  { Search files }

const
  {   From SysUtils unit:
    faReadOnly  = $00000001;
    faHidden    = $00000002;
    faSysFile   = $00000004;
    faVolumeID  = $00000008;
    faDirectory = $00000010;
    faArchive   = $00000020;
    faAnyFile   = $0000003F;  }
    // Extended flag
    elfaNormal    = $00000040;  // File with no attributes

const
  EL_SF_RECURSION        = $00000001;  // Recursion search
  // Search files :
  EL_SF_USE_ATTRS        = $00000002;  // With Query.Attributes attributes
      // With size:
  EL_SF_USE_SIZE_MIN     = $00000004;  // Not less then Query.SizeMin;
  EL_SF_USE_SIZE_MAX     = $00000008;  // Not greate then Query.SizeMax;
      // With time:
  EL_SF_USE_FROM_TIME    = $00000010;  // Greate then Query.TimeMin
  EL_SF_USE_TO_TIME      = $00000020;  // Less then Query.TimeMax
  // Time type:
  EL_SF_CREATION_TIME    = $00000040;  // Creation time
  EL_SF_LAST_ACCESS_TIME = $00000080;  // Last access time
  EL_SF_LAST_WRITE_TIME  = $000000C0;  // Last write time

type
  TELSearchFilesQuery = record
    Directory: string;
    Mask: string;
    Attributes: Cardinal;
    SizeMin: Cardinal;
    SizeMax: Cardinal;
    FromTime: TFileTime;
    ToTime: TFileTime;
    SearchFlags: Cardinal; // Combination of EL_SF_XXX constants
  end;
  TELSearchFilesProc = procedure(const AData: TWin32FindData;
    const ADir: string; ACustomPar: Pointer);

function ELSearchFiles(const ADir, AMask: string; ARecurs: Boolean;
  AResult: TStrings): Integer; overload;
function ELSearchFiles(const ADir, AMask: string; ARecurs: Boolean;
  AProc: TELSearchFilesProc; ACustomPar: Pointer): Integer; overload;
function ELSearchFiles(const AQuery: TELSearchFilesQuery;
  AProc: TELSearchFilesProc; ACustomPar: Pointer): Integer; overload;

type
  TELThreadFilesSearchOnAddEvent = procedure(Sender: TObject;
    const AData: TWin32FindData; const ADir: string) of object;

  TELThreadFilesSearch = class(TThread)
  private
    FOnAdd: TELThreadFilesSearchOnAddEvent;
    FDataPtr: PWin32FindData;
    FDir: string;
    FQuery: TELSearchFilesQuery;
    procedure DoAdd(const AData: TWin32FindData; const ADir: string);
    procedure SyncProc;
  protected
    procedure Execute; override;
    procedure Added(const AData: TWin32FindData; const ADir: string); virtual;
  public
    constructor Create(CreateSuspended: Boolean; const AQuery: TELSearchFilesQuery);
    property OnAdd: TELThreadFilesSearchOnAddEvent read FOnAdd write FOnAdd;
  end;

implementation

function ELPackStrings(AValue: TStrings): string;
var
  LI, LC, LL: Longint;
begin
  Result := '';
  with TStringStream.Create('') do
  try
    LC := AValue.Count;
    Write(LC, SIzeOf(Longint));
    for LI := 0 to AValue.Count - 1 do
    begin
      LL := Length(AValue[LI]);
      Write(LL, SizeOf(Longint));
      Write(Pointer(AValue[LI])^, LL);
    end;
    Result := DataString;
  finally
    Free;
  end;
end;

procedure ELUnpackStrings(const AValue: string; Result: TStrings);
var
  LI, LC, LL: Longint;
  LS: string;
begin
  Result.Clear;
  with TStringStream.Create(AValue) do
  try
    Read(LC, SizeOf(Longint));
    for LI := 0 to LC - 1 do
    begin
      Read(LL, SizeOf(Longint));
      SetString(LS, nil, LL);
      Read(Pointer(LS)^, LL);
      Result.Add(LS);
    end;
  finally
    Free;
  end;
end;

procedure ELSaveStringToStream(const AStr: string; AStream: TStream);
var
  LC: Longint;
begin
  LC := Length(AStr);
  AStream.Write(LC, SizeOf(Longint));
  AStream.Write(Pointer(AStr)^, LC);
end;

function ELLoadStringFromStream(AStream: TStream): string;
var
  LC: Longint;
begin
  AStream.Read(LC, SizeOf(Longint));
  SetLength(Result, LC);
  AStream.Read(Pointer(Result)^, LC);
end;

procedure ELQuickSort(AFromIndex, AToIndex: Integer;
  ACompareProc: TELQSCompareProc; AExchangeProc: TELQSExchangeProc;
  ACustomPar: Pointer);

  procedure _Sort(AFromIndex, AToIndex: Integer);
  var
    LI, LJ, LCenter: Integer;
  begin
    repeat
      LI := AFromIndex;
      LJ := AToIndex;
      LCenter := (AFromIndex + AToIndex) div 2;
      repeat
        while ACompareProc(LI, LCenter, ACustomPar) < 0 do Inc(LI);
        while ACompareProc(LJ, LCenter, ACustomPar) > 0 do Dec(LJ);
        if LI <= LJ then
        begin
          AExchangeProc(LI, LJ, ACustomPar);
          if LCenter = LI then
            LCenter := LJ
          else if LCenter = LJ then
            LCenter := LI;
          Inc(LI);
          Dec(LJ);
        end;
      until LI > LJ;
      if AFromIndex < LJ then _Sort(AFromIndex, LJ);
      AFromIndex := LI;
    until LI >= AToIndex;
  end;

begin
  _Sort(AFromIndex, AToIndex);
end;

function ELPrepareMask(const AMask: string): TELMaskInfo;
var
  LI: Integer;
  LStar: Boolean;
  LStr: string;
begin
  LStr := '';
  LStar := False;
  for LI := 1 to Length(AMask) do
  begin
    case AMask[LI] of
      '*': LStar := True;
      '?': LStr := LStr + '?';
    else
      if LStar then LStr := LStr + '*';
      LStar := False;
      LStr := LStr + AMask[LI];
    end;
  end;
  if LStar then LStr := LStr + '*';

  Result.BeginStar := (Length(LStr) > 0) and (LStr[1] = '*');
  Result.EndStar := (Length(LStr) > 0) and (LStr[Length(LStr)] = '*');
  if Length(LStr) > 0 then
  begin
    SetLength(Result.Strings, 0);
    LI := 1;
    while LI <= Length(LStr) do
    begin
      if LStr[LI] <> '*' then
      begin
        SetLength(Result.Strings, Length(Result.Strings) + 1);
        Result.Strings[High(Result.Strings)] := '';
        repeat
          Result.Strings[High(Result.Strings)] :=
            Result.Strings[High(Result.Strings)] + LStr[LI];
          Inc(LI);
        until (LI > Length(LStr)) or (LStr[LI] = '*');
      end;
      Inc(LI);
    end;
  end
  else
    SetLength(Result.Strings, 0);
  SetLength(Result.StLength, Length(Result.Strings));
  for LI := 0 to High(Result.Strings) do
    Result.StLength[LI] := Length(Result.Strings[LI]);
  Result.Count := Length(Result.Strings);
end;

function ELMaskCompare(const AStr: string; const AMask: TELMaskInfo): Boolean;

var
  LI: Integer;
  LStr: PChar;

  function _PCompare(AMask: PChar): Boolean;
  label
    LFalse, LTrue, LEnd;
  var
    LOldPSCur: PChar;
  begin
    LOldPSCur := LStr;
    while True do
    begin
      case AMask^ of
        #0:  goto LTrue;
        '?': if LStr^ = #0 then goto LFalse;
      else
        if LStr^ <> AMask^ then goto LFalse;
      end;
      Inc(LStr);
      Inc(AMask);
    end;
LFalse:
    Result := False;
    LStr := LOldPSCur + 1;
    goto LEnd;
LTrue:
    Result := True;
LEnd:
  end;

  function _PFind(AMask: PChar): Boolean;
  label
    LFalse, LTrue, LEnd;
  var
    LOldPSCur: PChar;
  begin
    LOldPSCur := LStr;
    Result := False;
    while True do
    begin
      while True do
      begin
        case AMask^ of
          #0:  goto LTrue;
          '?': if LStr^ = #0 then goto LEnd;
        else
          if LStr^ <> AMask^ then
            if LStr^ = #0 then goto LEnd else goto LFalse;
        end;
        Inc(LStr);
        Inc(AMask);
      end;
LFalse:
      Inc(LOldPSCur);
      LStr := LOldPSCur;
    end;
LTrue:
    Result := True;
LEnd:
  end;

begin
  LStr := PChar(AStr);
  if AMask.Count = 0 then
  begin
    Result := (LStr^ = #0) or AMask.BeginStar;
    Exit;
  end;
  if not AMask.BeginStar then
    if not _PCompare(PChar(AMask.Strings[0])) then
    begin
      Result := False;
      Exit;
    end;
  for LI := Ord(not AMask.BeginStar)
    to AMask.Count - 1 - Ord(not AMask.EndStar) do
    if not _PFind(PChar(AMask.Strings[LI])) then
    begin
      Result := False;
      Exit;
    end;
  if not AMask.EndStar then
  begin
    while LStr^ <> #0 do Inc(LStr);
    Dec(LStr, AMask.StLength[AMask.Count - 1]);
    Result := _PCompare(PChar(AMask.Strings[AMask.Count - 1]));
  end
  else Result := True;
end;


type
  TCompareProc = function(const Data: TWin32FindData;
    const Qwery: TELSearchFilesQuery): Boolean;
  TCompareProcRec = record
    Proc: TCompareProc;
    Flag: Cardinal;
  end;

function _cmpAttrs(const AData: TWin32FindData;
  const AQuery: TELSearchFilesQuery): Boolean;
begin
  Result :=
    ((AData.dwFileAttributes and AQuery.Attributes) > 0) or
    ((AData.dwFileAttributes = 0) and
    ((AQuery.Attributes and elfaNormal) > 0));
end;

function _cmpSizeMin(const AData: TWin32FindData;
  const AQuery: TELSearchFilesQuery): Boolean;
begin
  Result := (AData.nFileSizeLow >= AQuery.SizeMin);
end;

function _cmpSizeMax(const AData: TWin32FindData;
  const AQuery: TELSearchFilesQuery): Boolean;
begin
  Result := (AData.nFileSizeLow <= AQuery.SizeMax);
end;

function _cmpFromTime(const AData: TWin32FindData;
  const AQuery: TELSearchFilesQuery): Boolean;
const
  FlagMask = EL_SF_CREATION_TIME or EL_SF_LAST_ACCESS_TIME or
    EL_SF_LAST_WRITE_TIME;
var
  Flag: Cardinal;
  PTime: PFileTime;
begin
  Flag := AQuery.SearchFlags and FlagMask;
  case Flag of
    EL_SF_CREATION_TIME:    PTime := @(AData.ftCreationTime);
    EL_SF_LAST_ACCESS_TIME: PTime := @(AData.ftLastAccessTime);
  else
  { EL_SF_LAST_WRITE_TIME}  PTime := @(AData.ftLastWriteTime);
  end;
  Result := (CompareFileTime(PTime^, AQuery.FromTime) >= 0);
end;

function _cmpToTime(const AData: TWin32FindData;
  const AQuery: TELSearchFilesQuery): Boolean;
const
  FlagMask = EL_SF_CREATION_TIME or EL_SF_LAST_ACCESS_TIME or
    EL_SF_LAST_WRITE_TIME;
var
  Flag: Cardinal;
  PTime: PFileTime;
begin
  Flag := AQuery.SearchFlags and FlagMask;
  case Flag of
    EL_SF_CREATION_TIME:    PTime := @(AData.ftCreationTime);
    EL_SF_LAST_ACCESS_TIME: PTime := @(AData.ftLastAccessTime);
  else
  { EL_SF_LAST_WRITE_TIME } PTime := @(AData.ftLastWriteTime);
  end;
  Result := (CompareFileTime(PTime^, AQuery.ToTime) <= 0);
end;

const
  ALLCOMPPROCS: array[0..4] of TCompareProcRec = (
    (Proc: _cmpAttrs;      Flag: EL_SF_USE_ATTRS),
    (Proc: _cmpSizeMin;    Flag: EL_SF_USE_SIZE_MIN),
    (Proc: _cmpSizeMax;    Flag: EL_SF_USE_SIZE_MAX),
    (Proc: _cmpFromTime;   Flag: EL_SF_USE_FROM_TIME),
    (Proc: _cmpToTime;     Flag: EL_SF_USE_TO_TIME)
  );

type
  TStopBit = ^Boolean;

function _Files_Search(const AQuery: TELSearchFilesQuery;
  AProc: TELSearchFilesProc; AStopBit: TStopBit; ACustomPar: Pointer): Integer;

var
  LI: Integer;
  LCompProcs: array of TCompareProc;
  LDirs: array of string;
  LMaskInfo: TELMaskInfo;

  function __CheckCompareProcs(const AData: TWin32FindData;
    const AQuery: TELSearchFilesQuery): Boolean;
  var
    LI: Integer;
  begin
    Result := True;
    for LI := 0 to High(LCompProcs) - 1 do
      if not LCompProcs[LI](AData, AQuery) then
      begin
        Result := False;
        Break;
      end;
  end;

  procedure __FindFiles(const ADir, AMask: string);
  var
    LhWnd: THandle;
    LFindData: TWin32FindData;
    LDir: string;
  begin
    if ADir[Length(ADir)] = '\' then LDir := ADir else LDir :=ADir + '\';

    LhWnd := FindFirstFile(PChar(LDir + '*'), LFindData);
    if LhWnd <> INVALID_HANDLE_VALUE then
    begin
      try
        repeat
          if (AStopBit <> nil) and AStopBit^ then Break;
          if (LFindData.cFileName[0] <> '.') then
          begin
            if (LFindData.dwFileAttributes and faDirectory) > 0 then
            begin
              SetLength(LDirs, Length(LDirs) + 1);
              LDirs[High(LDirs)] := LDir + LFindData.cFileName;
            end;
            if __CheckCompareProcs(LFindData, AQuery) and
              ELMaskCompare(UpperCase(LFindData.cFileName), LMaskInfo) then
              AProc(LFindData, Copy(LDir, 1, Length(LDir) - 1), ACustomPar);
          end;
          if (AStopBit <> nil) and AStopBit^ then Break;
        until not FindNextFile(LhWnd, LFindData);
      finally
        Windows.FindClose(LhWnd);
      end;
    end;
  end;

  procedure __RecursSearch(const ADir, AMask: string);
  var
    I, I1, I2: Integer;
  begin
    if (AStopBit <> nil) and AStopBit^ then Exit;
    I1 := High(LDirs) + 1;
    __FindFiles(ADir, AMask);
    I2 := High(LDirs);
    for I := I1 to I2 do __RecursSearch(LDirs[I], AMask);
  end;

begin
  Result := 0;
  { Create compare procedure list }
  for LI := 0 to High(ALLCOMPPROCS) do
    if (AQuery.SearchFlags and ALLCOMPPROCS[LI].Flag) > 0 then
    begin
      SetLength(LCompProcs, Length(LCompProcs) + 1);
      LCompProcs[High(LCompProcs)] := ALLCOMPPROCS[LI].Proc;
    end;
  { Search files }
  LMaskInfo := ELPrepareMask(UpperCase(AQuery.Mask));
  if (AQuery.SearchFlags and EL_SF_RECURSION) > 0 then
    __RecursSearch(AQuery.Directory, AQuery.Mask)
  else
    __FindFiles(AQuery.Directory, AQuery.Mask);
end;

function ELSearchFiles(const AQuery: TELSearchFilesQuery;
  AProc: TELSearchFilesProc; ACustomPar: Pointer): Integer;
begin
  Result := _Files_Search(AQuery, AProc, nil, ACustomPar);
end;

function ELSearchFiles(const ADir, AMask: string; ARecurs: Boolean;
  AProc: TELSearchFilesProc; ACustomPar: Pointer): Integer;
var
  LSearchFilesQuery: TELSearchFilesQuery;
begin
  with LSearchFilesQuery do
  begin
    Directory := ADir;
    Mask := AMask;
    if ARecurs then SearchFlags := EL_SF_RECURSION else SearchFlags := 0;
  end;
  Result := _Files_Search(LSearchFilesQuery, AProc, nil, ACustomPar);
end;

procedure _SearchFiles_AddToStrings(const AData: TWin32FindData;
  const ADir: string; ACustomPar: Pointer);
begin
  TStrings(ACustomPar).Add(ADir + '\' + AData.cFileName);
end;

function ELSearchFiles(const ADir, AMask: string; ARecurs: Boolean;
  AResult: TStrings): Integer; overload;
var
  LSearchFilesQuery: TELSearchFilesQuery;
begin
  if AResult <> nil then
  begin
    AResult.BeginUpdate;
    try
      AResult.Clear;
      with LSearchFilesQuery do
      begin
        Directory := ADir;
        Mask := AMask;
        if ARecurs then SearchFlags := EL_SF_RECURSION else SearchFlags := 0;
      end;
      Result := _Files_Search(LSearchFilesQuery, _SearchFiles_AddToStrings, nil,
        AResult);
    finally
      AResult.EndUpdate;
    end;
  end else Result := -1;
end;

{ TThreadFilesSearch }

constructor TELThreadFilesSearch.Create(CreateSuspended: Boolean;
  const AQuery: TELSearchFilesQuery);
begin
  FQuery := AQuery;
  SetLength(FQuery.Directory, Length(FQuery.Directory));
  SetLength(FQuery.Mask, Length(FQuery.Mask));
  inherited Create(CreateSuspended);
end;

procedure TELThreadFilesSearch.DoAdd(const AData: TWin32FindData;
  const ADir: string);
begin
  FDataPtr := @AData;
  FDir := ADir;
  Synchronize(SyncProc);
end;

procedure TELThreadFilesSearch.Added(const AData: TWin32FindData;
  const ADir: string);
begin
  if Assigned(FOnAdd) then FOnAdd(Self, AData, ADir);
end;

procedure _ThreadFilesSearchProc(const AData: TWin32FindData;
  const ADir: string; ACustomPar: Pointer);
begin
  TELThreadFilesSearch(ACustomPar).DoAdd(AData, ADir);
end;

procedure TELThreadFilesSearch.Execute;
begin
  _Files_Search(FQuery, _ThreadFilesSearchProc, @Terminated, Self);
end;

procedure TELThreadFilesSearch.SyncProc;
begin
  if not Terminated then Added(FDataPtr^, FDir);
  FDataPtr := nil;
  FDir := '';
end;

end.
