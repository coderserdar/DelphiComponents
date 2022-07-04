// TntWare Delphi Unicode Controls
//      http://www.tntware.com/delphicontrols/unicode/
// Copyright (c) 2002-2007, Troy Wolbrink (www.tntware.com)
//
// This unit is a abstracted unit with required elements for Tag managing units,
//  all elements are extracted from the units of "Tnt Delphi UNICODE Controls"
//  package V2.3.0.

// Bug fix of TTntFileStream  (22 Apr 2009)

unit TntCollection;

interface

uses Windows, classes, SysUtils, UniCodeUtils;

{$INCLUDE Delphi_Ver.inc}

type

 // Following 2 types are defined TntWindows.pas
  TPathLengthResultOption = (poAllowDirectoryMode, poZeroSmallBuff, poExactCopy, poExactCopySubPaths);
  TPathLengthResultOptions = set of TPathLengthResultOption;

  TTntFileStream = class(THandleStream)   // defined in TntClasses.pas
  private
    OpenedHandle : integer;
  public
    constructor Create(const FileName: WideString; Mode: Word);
    destructor Destroy; override;
  end;

 // Following functions are defined in TntWideStrUtils.pas
  function WStrScan(const Str: PWideChar; Chr: WideChar): PWideChar;
  function WStrLCopy(Dest, Source: PWideChar; MaxLen: Cardinal): PWideChar;
  function WStrPCopy(Dest: PWideChar; const Source: WideString): PWideChar;

 // Following functions are defined in TntSysUtils.pas
  function WideExpandFileName(const FileName: WideString): WideString;
  function WideFileCreate(const FileName: WideString): Integer;
  function WideFileOpen(const FileName: WideString; Mode: LongWord): Integer;
  function WideFileExists(const Name: WideString): Boolean;
  function WideDeleteFile(const FileName: WideString): Boolean;
  function WideFileAge(const FileName: WideString): Integer; overload;
  function WideFileAge(const FileName: WideString; out FileDateTime: TDateTime): Boolean; overload;
  function WideFileSetAttr(const FileName: WideString; Attr: Integer): Boolean;
  function WideRenameFile(const OldName, NewName: WideString): Boolean;
  function WideExtractFileDrive(const FileName: WideString): WideString;
  {$IFNDEF DELPHI_6_BELOW}
  function WideLibraryErrorMessage(const LibName: WideString; Dll: THandle; ErrorCode: Integer): WideString;
  function WideSysErrorMessage(ErrorCode: Integer): WideString;
  {$ENDIF}

 // Following functions are deined in TntWindows.pas
  function Tnt_GetFullPathNameW(lpFileName: PWideChar; nBufferLength: DWORD;
    lpBuffer: PWideChar; var lpFilePart: PWideChar): DWORD;
  function Tnt_CreateFileW(lpFileName: PWideChar; dwDesiredAccess, dwShareMode: DWORD;
    lpSecurityAttributes: PSecurityAttributes; dwCreationDisposition, dwFlagsAndAttributes: DWORD;
      hTemplateFile: THandle): THandle;
  function Tnt_DeleteFileW(lpFileName: PWideChar): BOOL;
  function Tnt_MoveFileW(lpExistingFileName, lpNewFileName: PWideChar): BOOL;
  function Tnt_FindFirstFileW(lpFileName: PWideChar; var lpFindFileData: TWIN32FindDataW): THandle;
  function Tnt_FindNextFileW(hFindFile: THandle; var lpFindFileData: TWIN32FindDataW): BOOL;
  function Tnt_SetFileAttributesW(lpFileName: PWideChar; dwFileAttributes: DWORD): BOOL;

var
  Win32PlatformIsUnicode: Boolean;

implementation

uses {$IFDEF DELPHI_6_BELOW}consts {$ELSE}RTLConsts {$ENDIF};

{$IFDEF DELPHI_6_BELOW}
const
  PathDelim  = {$IFDEF MSWINDOWS} '\'; {$ELSE} '/'; {$ENDIF}
  DriveDelim = {$IFDEF MSWINDOWS} ':'; {$ELSE} '';  {$ENDIF}
  PathSep    = {$IFDEF MSWINDOWS} ';'; {$ELSE} ':'; {$ENDIF}
{$ENDIF}

procedure _ExactStrCopyW(pDest, pSource: PWideChar; Count: Integer);
var
  i: integer;
begin
  for i := 1 to Count do begin
    pDest^ := pSource^;
    Inc(PSource);
    Inc(pDest);
  end;
end;

procedure _ExactCopySubPaths(pDest, pSource: PWideChar; Count: Integer);
var
  i: integer;
  OriginalSource: PWideChar;
  PNextSlash: PWideChar;
begin
  if Count >= 4 then begin
    OriginalSource := pSource;
    PNextSlash := WStrScan(pSource, '\');
    for i := 1 to Count - 1 do begin
      // determine next path delimiter
      if pSource > pNextSlash then begin
        PNextSlash := WStrScan(pSource, '\');
      end;
      // leave if no more sub paths
      if (PNextSlash = nil)
      or ((pNextSlash - OriginalSource) >= Count) then begin
        exit;
      end;
      // copy char
      pDest^ := pSource^;
      Inc(PSource);
      Inc(pDest);
    end;
  end;
end;

function _HandlePathLengthResult(nBufferLength: DWORD; lpBuffer: PWideChar; const AnsiBuff: AnsiString;
          Options: TPathLengthResultOptions): Integer;
var
  WideBuff: WideString;
begin
  WideBuff := AnsiBuff;
  if nBufferLength > Cardinal(Length(WideBuff)) then begin
    // normal
    Result := Length(WideBuff);
    WStrLCopy(lpBuffer, PWideChar(WideBuff), nBufferLength);
  end else if (poExactCopy in Options) then begin
    // exact
    Result := nBufferLength;
    _ExactStrCopyW(lpBuffer, PWideChar(WideBuff), nBufferLength);
  end else begin
    // other
    if (poAllowDirectoryMode in Options)
    and (nBufferLength = Cardinal(Length(WideBuff))) then begin
      Result := Length(WideBuff) + 1;
      WStrLCopy(lpBuffer, PWideChar(WideBuff), nBufferLength - 1);
    end else begin
      Result := Length(WideBuff) + 1;
      if (nBufferLength > 0) then begin
        if (poZeroSmallBuff in Options) then
          lpBuffer^ := #0
        else if (poExactCopySubPaths in Options) then
          _ExactCopySubPaths(lpBuffer, PWideChar(WideBuff), nBufferLength);
      end;
    end;
  end;
end;

procedure _MakeWideWin32FindData(var WideFindData: TWIN32FindDataW; AnsiFindData: TWIN32FindDataA);
begin
  CopyMemory(@WideFindData, @AnsiFindData,
    Integer(@WideFindData.cFileName) - Integer(@WideFindData));
  WStrPCopy(WideFindData.cFileName, AnsiFindData.cFileName);
  WStrPCopy(WideFindData.cAlternateFileName, AnsiFindData.cAlternateFileName);
end;

function WStrScan(const Str: PWideChar; Chr: WideChar): PWideChar;
begin
  Result := Str;
  while Result^ <> Chr do
  begin
    if Result^ = #0 then
    begin
      Result := nil;
      Exit;
    end;
    Inc(Result);
  end;
end;

function WStrLCopy(Dest, Source: PWideChar; MaxLen: Cardinal): PWideChar;
var
  Count: Cardinal;
begin
  // copies a specified maximum number of characters from Source to Dest
  Result := Dest;
  Count := 0;
  While (Count < MaxLen) and (Source^ <> #0) do begin
    Dest^ := Source^;
    Inc(Source);
    Inc(Dest);
    Inc(Count);
  end;
  Dest^ := #0;
end;

function WStrPCopy(Dest: PWideChar; const Source: WideString): PWideChar;
begin
  Result := WStrLCopy(Dest, PWideChar(Source), Length(Source));
end;

function WideExpandFileName(const FileName: WideString): WideString;
var
  FName: PWideChar;
  Buffer: array[0..MAX_PATH - 1] of WideChar;
begin
  SetString(Result, Buffer, Tnt_GetFullPathNameW(PWideChar(FileName), MAX_PATH, Buffer, FName));
end;

function WideFileCreate(const FileName: WideString): Integer;
begin
  Result := Integer(Tnt_CreateFileW(PWideChar(FileName), GENERIC_READ or GENERIC_WRITE,
    0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0))
end;

function WideFileOpen(const FileName: WideString; Mode: LongWord): Integer;
const
  AccessMode: array[0..2] of LongWord = (
    GENERIC_READ,
    GENERIC_WRITE,
    GENERIC_READ or GENERIC_WRITE);
  ShareMode: array[0..4] of LongWord = (
    0,
    0,
    FILE_SHARE_READ,
    FILE_SHARE_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE);
begin
  Result := Integer(Tnt_CreateFileW(PWideChar(FileName), AccessMode[Mode and 3],
    ShareMode[(Mode and $F0) shr 4], nil, OPEN_EXISTING,
      FILE_ATTRIBUTE_NORMAL, 0));
end;

function WideFileExists(const Name: WideString): Boolean;
var
  Handle: THandle;
  FindData: TWin32FindDataW;
begin
  Result := False;
  Handle := Tnt_FindFirstFileW(PWideChar(Name), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
      Result := True;
  end;
end;

function WideDeleteFile(const FileName: WideString): Boolean;
begin
  Result := Tnt_DeleteFileW(PWideChar(FileName))
end;

function WideFileAge(const FileName: WideString): Integer;
var
  Handle: THandle;
  FindData: TWin32FindDataW;
  LocalFileTime: TFileTime;
begin
  Handle := Tnt_FindFirstFileW(PWideChar(FileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
    begin
      FileTimeToLocalFileTime(FindData.ftLastWriteTime, LocalFileTime);
      if FileTimeToDosDateTime(LocalFileTime, LongRec(Result).Hi, LongRec(Result).Lo) then
        Exit
    end;
  end;
  Result := -1;
end;

function WideFileAge(const FileName: WideString; out FileDateTime: TDateTime): Boolean;
var
  Handle: THandle;
  FindData: TWin32FindDataW;
  LSystemTime: TSystemTime;
  LocalFileTime: TFileTime;
begin
  Result := False;
  Handle := Tnt_FindFirstFileW(PWideChar(FileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
    begin
      Result := True;
      FileTimeToLocalFileTime(FindData.ftLastWriteTime, LocalFileTime);
      FileTimeToSystemTime(LocalFileTime, LSystemTime);
      with LSystemTime do
        FileDateTime := EncodeDate(wYear, wMonth, wDay) +
          EncodeTime(wHour, wMinute, wSecond, wMilliSeconds);
    end;
  end;
end;

function WideFileSetAttr(const FileName: WideString; Attr: Integer): Boolean;
begin
  Result := Tnt_SetFileAttributesW(PWideChar(FileName), Attr)
end;

function Tnt_GetFullPathNameW(lpFileName: PWideChar; nBufferLength: DWORD;
  lpBuffer: PWideChar; var lpFilePart: PWideChar): DWORD;
var
  AnsiBuff: AnsiString;
  AnsiFilePart: PAnsiChar;
  AnsiLeadingChars: Integer;
  WideLeadingChars: Integer;
begin
  if Win32PlatformIsUnicode then
    Result := GetFullPathNameW{TNT-ALLOW GetFullPathNameW}(lpFileName, nBufferLength, lpBuffer, lpFilePart)
  else begin
    SetLength(AnsiBuff, MAX_PATH * 2);
    SetLength(AnsiBuff, GetFullPathNameA{TNT-ALLOW GetFullPathNameA}(PAnsiChar(AnsiString(lpFileName)),
      Length(AnsiBuff), PAnsiChar(AnsiBuff), AnsiFilePart));
    Result := _HandlePathLengthResult(nBufferLength, lpBuffer, AnsiBuff, [poZeroSmallBuff]);
    // deal w/ lpFilePart
    if (AnsiFilePart = nil) or (nBufferLength < Result) then
      lpFilePart := nil
    else begin
      AnsiLeadingChars := AnsiFilePart - PAnsiChar(AnsiBuff);
      WideLeadingChars := Length(WideString(Copy(AnsiBuff, 1, AnsiLeadingChars)));
      lpFilePart := lpBuffer + WideLeadingChars;
    end;
  end;
end;

function WideRenameFile(const OldName, NewName: WideString): Boolean;
begin
  Result := Tnt_MoveFileW(PWideChar(OldName), PWideChar(NewName))
end;

function WideExtractFileDrive(const FileName: WideString): WideString;
var
  I, J: Integer;
begin
  if (Length(FileName) >= 2) and (FileName[2] = DriveDelim) then
    Result := Copy(FileName, 1, 2)
  else if (Length(FileName) >= 2) and (FileName[1] = PathDelim) and
    (FileName[2] = PathDelim) then
  begin
    J := 0;
    I := 3;
    While (I < Length(FileName)) and (J < 2) do
    begin
      if FileName[I] = PathDelim then Inc(J);
      if J < 2 then Inc(I);
    end;
    if FileName[I] = PathDelim then Dec(I);
    Result := Copy(FileName, 1, I);
  end else Result := '';
end;

{$IFNDEF DELPHI_6_BELOW}
function WideLibraryErrorMessage(const LibName: WideString; Dll: THandle; ErrorCode: Integer): WideString;
var
  Len: Integer;
  AnsiResult: AnsiString;
  Flags: Cardinal;
begin
  Flags := FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_IGNORE_INSERTS or FORMAT_MESSAGE_ARGUMENT_ARRAY;
  if Dll <> 0 then
    Flags := Flags or FORMAT_MESSAGE_FROM_HMODULE;
  if Win32PlatformIsUnicode then begin
    SetLength(Result, 256);
    Len := FormatMessageW(Flags, Pointer(Dll), ErrorCode, 0, PWideChar(Result), Length(Result), nil);
    SetLength(Result, Len);
  end else begin
    SetLength(AnsiResult, 256);
    Len := FormatMessageA(Flags, Pointer(Dll), ErrorCode, 0, PAnsiChar(AnsiResult), Length(AnsiResult), nil);
    SetLength(AnsiResult, Len);
    Result := AnsiResult;
  end;
  if Trim(Result) = '' then
    Result := WideFormat('Unspecified error (%d) from %s.', [ErrorCode, LibName]);
end;

function WideSysErrorMessage(ErrorCode: Integer): WideString;
begin
  Result := WideLibraryErrorMessage('system', 0, ErrorCode);
end;
{$ENDIF}

function Tnt_CreateFileW(lpFileName: PWideChar; dwDesiredAccess, dwShareMode: DWORD;
  lpSecurityAttributes: PSecurityAttributes; dwCreationDisposition, dwFlagsAndAttributes: DWORD;
    hTemplateFile: THandle): THandle;
begin
  if Win32PlatformIsUnicode then
    Result := CreateFileW{TNT-ALLOW CreateFileW}(lpFileName, dwDesiredAccess, dwShareMode,
      lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile)
  else
    Result := CreateFileA{TNT-ALLOW CreateFileA}(PAnsiChar(AnsiString(lpFileName)), dwDesiredAccess, dwShareMode,
      lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile)
end;

function Tnt_DeleteFileW(lpFileName: PWideChar): BOOL;
begin
  if Win32PlatformIsUnicode then
    Result := DeleteFileW{TNT-ALLOW DeleteFileW}(lpFileName)
  else
    Result := DeleteFileA{TNT-ALLOW DeleteFileA}(PAnsiChar(AnsiString(lpFileName)));
end;

function Tnt_MoveFileW(lpExistingFileName, lpNewFileName: PWideChar): BOOL;
begin
  if Win32PlatformIsUnicode then
    Result := MoveFileW{TNT-ALLOW MoveFileW}(lpExistingFileName, lpNewFileName)
  else
    Result := MoveFileA{TNT-ALLOW MoveFileA}(PAnsiChar(AnsiString(lpExistingFileName)), PAnsiChar(AnsiString(lpNewFileName)));
end;

function Tnt_FindFirstFileW(lpFileName: PWideChar; var lpFindFileData: TWIN32FindDataW): THandle;
var
  Ansi_lpFindFileData: TWIN32FindDataA;
begin
  if Win32PlatformIsUnicode then
    Result := FindFirstFileW{TNT-ALLOW FindFirstFileW}(lpFileName, lpFindFileData)
  else begin
    Result := FindFirstFileA{TNT-ALLOW FindFirstFileA}(PAnsiChar(AnsiString(lpFileName)),
      Ansi_lpFindFileData);
    if Result <> INVALID_HANDLE_VALUE then
      _MakeWideWin32FindData(lpFindFileData, Ansi_lpFindFileData);
  end;
end;

function Tnt_FindNextFileW(hFindFile: THandle; var lpFindFileData: TWIN32FindDataW): BOOL;
var
  Ansi_lpFindFileData: TWIN32FindDataA;
begin
  if Win32PlatformIsUnicode then
    Result := FindNextFileW{TNT-ALLOW FindNextFileW}(hFindFile, lpFindFileData)
  else begin
    Result := FindNextFileA{TNT-ALLOW FindNextFileA}(hFindFile, Ansi_lpFindFileData);
    if Result then
      _MakeWideWin32FindData(lpFindFileData, Ansi_lpFindFileData);
  end;
end;

function Tnt_SetFileAttributesW(lpFileName: PWideChar; dwFileAttributes: DWORD): BOOL;
begin
  if Win32PlatformIsUnicode then
    Result := SetFileAttributesW{TNT-ALLOW SetFileAttributesW}(lpFileName, dwFileAttributes)
  else
    Result := SetFileAttributesA{TNT-ALLOW SetFileAttributesA}(PAnsiChar(AnsiString(lpFileName)), dwFileAttributes);
end;

constructor TTntFileStream.Create(const FileName: WideString; Mode: Word);
var
  CreateHandle: Integer;
  {$IFNDEF DELPHI_6_BELOW}
  ErrorMessage: WideString;
  {$ENDIF}
begin
  OpenedHandle := 0;

  if Mode = fmCreate then
  begin
    CreateHandle := WideFileCreate(FileName);
    if CreateHandle < 0 then begin
      {$IFNDEF DELPHI_6_BELOW}
      ErrorMessage := WideSysErrorMessage(GetLastError);
      raise EFCreateError.CreateFmt(SFCreateErrorEx, [WideExpandFileName(FileName), ErrorMessage]);
      {$ELSE}
      raise EFCreateError.CreateFmt(SFCreateError, [WideExpandFileName(FileName)]);
      {$ENDIF}
    end;
  end else
  begin
    CreateHandle := WideFileOpen(FileName, Mode);
    if CreateHandle < 0 then begin
      {$IFNDEF DELPHI_6_BELOW}
      ErrorMessage := WideSysErrorMessage(GetLastError);
      raise EFOpenError.CreateFmt(SFOpenErrorEx, [WideExpandFileName(FileName), ErrorMessage]);
      {$ELSE}
      raise EFOpenError.CreateFmt(SFOpenError, [WideExpandFileName(FileName)]);
      {$ENDIF}
    end;
  end;

  OpenedHandle := CreateHandle;

  inherited Create(CreateHandle);
end;

destructor TTntFileStream.Destroy;
begin
  if OpenedHandle <> 0 then FileClose(OpenedHandle);
end;


initialization
  Win32PlatformIsUnicode := (Win32Platform = VER_PLATFORM_WIN32_NT);

finalization

end.
