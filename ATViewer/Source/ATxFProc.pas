unit ATxFProc;

interface

uses
  Windows;

function IsFileExist(const FileName: WideString; var IsDir: Boolean): Boolean; overload;
function IsFileExist(const FileName: WideString): Boolean; overload;
function IsFileOrDirExist(const FileName: WideString): Boolean;
function IsDirExist(const DirName: WideString): Boolean;
function IsFileAccessed(const FileName: WideString): Boolean;

function FFileOpen(const FileName: WideString): THandle;
function FFileCopy(const OldName, NewName: WideString): Boolean;
function FFileMove(const OldName, NewName: WideString): Boolean;
function FGetFileSize(const FileName: WideString): Int64; overload;
function FGetFileSize(Handle: THandle): Int64; overload;
function FGetFileInfo(const FileName: WideString; var Size: Int64; var Time: TFileTime): Boolean;
function FGetShortName(const FileName: WideString): WideString;
function FGetFullPathName(const FileName: WideString): WideString;

type
  PInt64Rec = ^TInt64Rec;
  TInt64Rec = packed record
    Lo, Hi: DWORD;
  end;

function FOpenURL(const URL: WideString; hWnd: THandle): Boolean;
function FExecute(const Command, Params: WideString; hWnd: THandle): Boolean;
function FCreateDir(const FileName: WideString): Boolean;

function IsFileUnicode(h: THandle): Boolean;
function IsFileUTF8(h: THandle): Boolean;
function IsFileRTF(h: THandle): Boolean;
function IsFileWeb(h: THandle): Boolean;
procedure IsFileRTFAndUTF8(const AFileName: WideString; var IsRTF, IsUTF8: Boolean);
function IsFileText(h: THandle; BufSizeKb: DWORD; DetectOEM: Boolean; var IsOEM: Boolean): Boolean;

//Convertion of Unicode filename to ANSI one:
//1. Trying to simply convert Unicode string_ to ANSI
//2. If not successfull, trying to get short name, it's always ANSI
//3. If not successfull, function returns empty string_ (fails)
function FFileNameWideToAnsi(const FileName: WideString): AnsiString;

function FFindFirstFile(const DirName, Mask: WideString): WideString;
function FDelete(const FileName: WideString): Boolean;
function FDeleteToRecycle(Handle: THandle; const FileName: WideString; ToRecycle: Boolean = True): Boolean;
procedure FShowProperties(const fn: WideString; hWnd: THandle);


implementation

uses
  SysUtils, ShellAPI;

function IsFileExist(const FileName: WideString; var IsDir: Boolean): Boolean; overload;
var
  h: THandle;
  fdA: TWin32FindDataA;
  fdW: TWin32FindDataW;
begin
  IsDir := False;
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    h := FindFirstFileW(PWideChar(FileName), fdW);
    Result := h <> INVALID_HANDLE_VALUE;
    if Result then
    begin
      IsDir := (fdW.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0;
      Windows.FindClose(h);
    end;
  end
  else
  begin
    h := FindFirstFileA(PAnsiChar(AnsiString(FileName)), fdA);
    Result := h <> INVALID_HANDLE_VALUE;
    if Result then
    begin
      IsDir := (fdA.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0;
      Windows.FindClose(h);
    end;
  end;
end;

function IsFileExist(const FileName: WideString): Boolean; overload;
var
  IsDir: Boolean;
begin
  Result := IsFileExist(FileName, IsDir) and (not IsDir);
end;

function IsFileOrDirExist(const FileName: WideString): Boolean;
var
  IsDir: Boolean;
begin
  Result := IsFileExist(FileName, IsDir);
end;

function SDelLastSlashW(const S: WideString): WideString;
begin
  Result := S;
  if (Result <> '') and (Result[Length(Result)] = '\') then
    SetLength(Result, Length(Result) - 1);
end;

function IsDirExist(const DirName: WideString): Boolean; overload;
var
  IsDir: Boolean;
begin
  Result := IsFileExist(SDelLastSlashW(DirName), IsDir) and IsDir;
end;

function FFileOpen(const FileName: WideString): THandle;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Result := CreateFileW(PWideChar(FileName),
              GENERIC_READ,
              FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
              nil, OPEN_EXISTING, 0, 0)
  else
    Result := CreateFileA(PAnsiChar(AnsiString(FileName)),
              GENERIC_READ,
              FILE_SHARE_READ or FILE_SHARE_WRITE, //FILE_SHARE_DELETE not supported under Win9x
              nil, OPEN_EXISTING, 0, 0);
end;

function FFileCopy(const OldName, NewName: WideString): Boolean;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Result := CopyFileW(PWideChar(OldName), PWideChar(NewName), False)
  else
    Result := CopyFileA(PAnsiChar(AnsiString(OldName)), PAnsiChar(AnsiString(NewName)), False);
end;

function FFileMove(const OldName, NewName: WideString): Boolean;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Result:= MoveFileW(PWideChar(OldName), PWideChar(NewName))
  else
    Result:= MoveFileA(PAnsiChar(AnsiString(OldName)), PAnsiChar(AnsiString(NewName)));
end;


function IsFileAccessed(const FileName: WideString): Boolean;
var
  h: THandle;
begin
  h := FFileOpen(FileName);
  Result := h <> INVALID_HANDLE_VALUE;
  if Result then CloseHandle(h);
end;

function FGetFileSize(const FileName: WideString): Int64; overload;
var
  h: THandle;
  fdA: TWin32FindDataA;
  fdW: TWin32FindDataW;
  SizeRec: TInt64Rec absolute Result;
begin
  Result := -1;
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    h := FindFirstFileW(PWideChar(FileName), fdW);
    if h <> INVALID_HANDLE_VALUE then
    begin
      SizeRec.Hi := fdW.nFileSizeHigh;
      SizeRec.Lo := fdW.nFileSizeLow;
      Windows.FindClose(h);
    end;
  end
  else
  begin
    h := FindFirstFileA(PAnsiChar(AnsiString(FileName)), fdA);
    if h <> INVALID_HANDLE_VALUE then
    begin
      SizeRec.Hi := fdA.nFileSizeHigh;
      SizeRec.Lo := fdA.nFileSizeLow;
      Windows.FindClose(h);
    end;
  end;
end;

function FGetFileSize(Handle: THandle): Int64; overload;
var
  Size: Int64;
  SizeRec: TInt64Rec absolute Size;
begin
  SizeRec.Lo := GetFileSize(Handle, @SizeRec.Hi);
  if (SizeRec.Lo = $FFFFFFFF) and (GetLastError <> NO_ERROR) then
    Result := -1
  else
    Result := Size;
end;

function FGetFileInfo(const FileName: WideString; var Size: Int64; var Time: TFileTime): Boolean;
var
  h: THandle;
  fdA: TWin32FindDataA;
  fdW: TWin32FindDataW;
  SizeRec: TInt64Rec absolute Size;
begin
  Result := False;

  Size := 0;
  FillChar(Time, SizeOf(Time), 0);

  if Win32Platform = VER_PLATFORM_WIN32_NT then
    begin
    h := FindFirstFileW(PWideChar(FileName), fdW);
    if h <> INVALID_HANDLE_VALUE then
      begin
      Result := True;
      //Attr := fdW.dwFileAttributes;
      SizeRec.Hi := fdW.nFileSizeHigh;
      SizeRec.Lo := fdW.nFileSizeLow;
      Time := fdW.ftLastWriteTime;
      Windows.FindClose(h);
      end;
    end
  else
    begin
    h := FindFirstFileA(PAnsiChar(AnsiString(FileName)), fdA);
    if h <> INVALID_HANDLE_VALUE then
      begin
      Result := True;
      //Attr := fdA.dwFileAttributes;
      SizeRec.Hi := fdA.nFileSizeHigh;
      SizeRec.Lo := fdA.nFileSizeLow;
      Time := fdA.ftLastWriteTime;
      Windows.FindClose(h);
      end;
    end;
end;


function FGetShortName(const FileName: WideString): WideString;
var
  bufA: array[0..MAX_PATH - 1] of AnsiChar;
  bufW: array[0..MAX_PATH - 1] of WideChar;
  resA: AnsiString;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    FillChar(bufW, SizeOf(bufW), 0);
    SetString(Result, bufW, GetShortPathNameW(PWideChar(FileName), bufW, SizeOf(bufW) div 2));
  end
  else
  begin
    FillChar(bufA, SizeOf(bufA), 0);
    SetString(resA, bufA, GetShortPathNameA(PAnsiChar(AnsiString(FileName)), bufA, SizeOf(bufA)));
    Result := resA;
  end;
end;


function FOpenURL(const URL: WideString; hWnd: THandle): Boolean;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Result := ShellExecuteW(hWnd, 'open', PWideChar(URL), nil, nil, SW_SHOW) > 32
  else
    Result := ShellExecuteA(hWnd, 'open', PAnsiChar(AnsiString(URL)), nil, nil, SW_SHOW) > 32;
end;

function FExecute(const Command, Params: WideString; hWnd: THandle): Boolean;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Result := ShellExecuteW(hWnd, 'open', PWideChar(Command), PWideChar(Params), nil, SW_SHOW) > 32
  else
    Result := ShellExecuteA(hWnd, 'open', PAnsiChar(AnsiString(Command)), PAnsiChar(AnsiString(Params)), nil, SW_SHOW) > 32;
end;

function FCreateDir(const FileName: WideString): Boolean;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Result := CreateDirectoryW(PWideChar(FileName), nil)
  else
    Result := CreateDirectoryA(PAnsiChar(AnsiString(FileName)), nil);
end;


function IsFileUnicode(h: THandle): Boolean;
var
  Buffer: Word;
  BytesRead: DWORD;
begin
  Buffer := 0;
  SetFilePointer(h, 0, nil, FILE_BEGIN);
  Result :=
    ReadFile(h, Buffer, SizeOf(Buffer), BytesRead, nil) and
    (BytesRead >= SizeOf(Buffer)) and
    ((Buffer = $FEFF) or (Buffer = $FFFE));
end;

function IsFileUTF8(h: THandle): Boolean;
var
  Buffer: packed array[0 .. 2] of Byte;
  BytesRead: DWORD;
begin
  FillChar(Buffer, SizeOf(Buffer), 0);
  SetFilePointer(h, 0, nil, FILE_BEGIN);
  Result :=
    ReadFile(h, Buffer, SizeOf(Buffer), BytesRead, nil) and
    (BytesRead >= SizeOf(Buffer)) and
    ((Buffer[0] = $EF) and (Buffer[1] = $BB) and (Buffer[2] = $BF));
end;

function IsFileRTF(h: THandle): Boolean;
const
  Sign = '{\rtf';
  SignLen = Length(Sign);
var
  Buffer: packed array[0 .. SignLen] of AnsiChar; //Sign + #0
  BytesRead: DWORD;
begin
  FillChar(Buffer, SizeOf(Buffer), 0);
  SetFilePointer(h, 0, nil, FILE_BEGIN);
  {$WARNINGS OFF}
  Result :=
    ReadFile(h, Buffer, SignLen, BytesRead, nil) and
    (BytesRead >= SignLen) and
    (Buffer = Sign);
  {$WARNINGS ON}
end;

function IsFileWeb(h: THandle): Boolean;
const
  Sign = '<?xml';
  SignLen = Length(Sign);
var
  Buffer: packed array[0 .. SignLen] of AnsiChar; //Sign + #0
  BytesRead: DWORD;
begin
  FillChar(Buffer, SizeOf(Buffer), 0);
  SetFilePointer(h, 0, nil, FILE_BEGIN);
  {$WARNINGS OFF}
  Result :=
    ReadFile(h, Buffer, SignLen, BytesRead, nil) and
    (BytesRead >= SignLen) and
    (Buffer = Sign);
  {$WARNINGS ON}
end;


procedure IsFileRTFAndUTF8(const AFileName: WideString; var IsRTF, IsUTF8: Boolean);
var
  h: THandle;
begin
  IsRTF := False;
  IsUTF8 := False;

  h := FFileOpen(AFileName);
  if h <> INVALID_HANDLE_VALUE then
    try
      IsRTF := IsFileRTF(h);
      IsUTF8 := IsFileUTF8(h);
    finally
      CloseHandle(h);
    end;
end;


type
  TFreqTable = array[$80 .. $FF] of Integer;

function IsFileText(h: THandle; BufSizeKb: DWORD; DetectOEM: Boolean; var IsOEM: Boolean): Boolean;
var
  Buffer: PAnsiChar;
  BufSize, BytesRead, i: DWORD;
  n: Integer;
  Table: TFreqTable;
  TableSize: Integer;
begin
  Result := False;
  IsOEM := False;

  if BufSizeKb = 0 then Exit;
  Buffer := nil;
  BufSize := BufSizeKb * 1024;

  //Init freq table
  TableSize := 0;
  FillChar(Table, SizeOf(Table), 0);

  try
    GetMem(Buffer, BufSize);
    FillChar(Buffer^, BufSize, 0);
    SetFilePointer(h, 0, nil, FILE_BEGIN);

    if ReadFile(h, Buffer^, BufSize, BytesRead, nil) then
      if BytesRead > 0 then
      begin
        Result := True;
        for i := 0 to BytesRead - 1 do
        begin
          n := Ord(Buffer[i]);

          //If control chars present, then non-text
          if (n < 32) and (n <> 09) and (n <> 13) and (n <> 10) then
            begin Result := False; Break end;

          //Calculate freq table
          if DetectOEM then
            if (n >= Low(Table)) and (n <= High(Table)) then
            begin
              Inc(TableSize);
              Inc(Table[n]);
            end;
        end;
      end;

    //Analize table
    if DetectOEM then
      if Result and (TableSize > 0) then
        for i := Low(Table) to High(Table) do
        begin
          Table[i] := Table[i] * 100 div TableSize;
          if ((i >= $B0) and (i <= $DF)) or (i = $FF) or (i = $A9) then
            if Table[i] >= 18 then
              begin IsOEM := True; Break end;
        end;

  finally
    if Assigned(Buffer) then
      FreeMem(Buffer);
  end;
end;


procedure SAddSlash(var S: AnsiString);
begin
  if (S <> '') and (S[Length(S)] <> '\') then
    S := S + '\';
end;

function FFileNameWideToAnsi(const FileName: WideString): AnsiString;
begin
  if IsDirExist(FileName) then
  begin
    Result := FileName;
    //Convert to short form only "pure Unicode" names:
    if FileName <> WideString(AnsiString(FileName)) then
    begin
      Result := FGetShortName(FileName);
      if not IsDirExist(Result) then
        Result := '';
    end;
    //Add trailing slash, Lister plugins expect it:
    SAddSlash(Result);
  end
  else
  begin
    Result := FileName;
    //Convert to short form only "pure Unicode" names:
    if FileName <> WideString(AnsiString(FileName)) then
    begin
      Result := FGetShortName(FileName);
      if not IsFileAccessed(Result) then
        Result := '';
    end;
  end;
end;


function FFindFirstFile(const DirName, Mask: WideString): WideString;
var
  h: THandle;
  fdA: TWin32FindDataA;
  fdW: TWin32FindDataW;
  IsDir: Boolean;
begin
  Result := '';
  h := INVALID_HANDLE_VALUE;
  try
    if Win32Platform = VER_PLATFORM_WIN32_NT then
    begin
      h := FindFirstFileW(PWideChar(DirName + '\' + Mask), fdW);
      if h <> INVALID_HANDLE_VALUE then
        repeat
          IsDir := (fdW.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0;
          if not IsDir then
          begin
            Result := DirName + '\' + fdW.cFileName;
            Exit
          end;
        until not FindNextFileW(h, fdW);
    end
    else
    begin
      h := FindFirstFileA(PAnsiChar(AnsiString(DirName+'\'+Mask)), fdA);
      if h <> INVALID_HANDLE_VALUE then
        repeat
          IsDir := (fdA.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0;
          if not IsDir then
          begin
            Result := DirName + '\' + fdA.cFileName;
            Exit
          end;
        until not FindNextFileA(h, fdA);
    end;
  finally
    Windows.FindClose(h);
  end;
end;


function FDeleteToRecycleA(Handle: THandle; const FileName: AnsiString; ToRecycle: Boolean): Boolean;
var
  op: TSHFileOpStructA;
  sFrom: AnsiString;
begin
  sFrom := FileName + #0#0;
  FillChar(op, SizeOf(op), 0);
  op.Wnd := Handle;
  op.wFunc := FO_DELETE;
  op.pFrom := PAnsiChar(sFrom);
  op.fFlags := FOF_NOCONFIRMATION;
  if ToRecycle then
    op.fFlags := op.fFlags or FOF_ALLOWUNDO;
  Result := SHFileOperationA(op) = 0;
end;

function FDeleteToRecycleW(Handle: THandle; const FileName: WideString; ToRecycle: Boolean): Boolean;
var
  op: TSHFileOpStructW;
  sFrom: WideString;
begin
  sFrom := FileName + #0#0;
  FillChar(op, SizeOf(op), 0);
  op.Wnd := Handle;
  op.wFunc := FO_DELETE;
  op.pFrom := PWideChar(sFrom);
  op.fFlags := FOF_NOCONFIRMATION;
  if ToRecycle then
    op.fFlags := op.fFlags or FOF_ALLOWUNDO;
  Result := SHFileOperationW(op) = 0;
end;

function FDeleteToRecycle(Handle: THandle; const FileName: WideString; ToRecycle: Boolean = True): Boolean;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Result := FDeleteToRecycleW(Handle, FileName, ToRecycle)
  else
    Result := FDeleteToRecycleA(Handle, FileName, ToRecycle);
end;

function FDelete(const FileName: WideString): Boolean;
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Result := DeleteFileW(PWideChar(FileName))
  else
    Result := DeleteFileA(PAnsiChar(AnsiString(FileName)));
end;


function FGetFullPathName(const FileName: WideString): WideString;
var
  bufA: array[0 .. MAX_PATH - 1] of AnsiChar;
  bufW: array[0 .. MAX_PATH - 1] of WideChar;
  partA: PAnsiChar;
  partW: PWideChar;
begin
  Result := '';
  if FileName <> '' then //Result for empty string_ should be empty string_!
    if Win32Platform = VER_PLATFORM_WIN32_NT then
    begin
      if GetFullPathNameW(PWideChar(FileName), SizeOf(bufW) div 2, bufW, partW) <> 0 then
        Result := bufW;
    end
    else
    begin
      if GetFullPathNameA(PAnsiChar(AnsiString(FileName)), SizeOf(bufA), bufA, partA) <> 0 then
        Result := AnsiString(bufA);
    end;
end;


procedure FShowPropertiesA(const fn: AnsiString; hWnd: THandle);
var
  sei: TShellExecuteInfoA;
begin
  FillChar(sei, SizeOf(sei), 0);
  sei.cbSize := SizeOf(sei);
  sei.wnd := hWnd;
  sei.lpFile := PAnsiChar(fn);
  sei.lpVerb := 'properties';
  sei.fMask := SEE_MASK_INVOKEIDLIST;
  ShellExecuteExA(@sei);
end;

procedure FShowPropertiesW(const fn: WideString; hWnd: THandle);
var
  sei: TShellExecuteInfoW;
begin
  FillChar(sei, SizeOf(sei), 0);
  sei.cbSize := SizeOf(sei);
  sei.wnd := hWnd;
  sei.lpFile := PWChar(fn);
  sei.lpVerb := 'properties';
  sei.fMask := SEE_MASK_INVOKEIDLIST;
  ShellExecuteExW(@sei);
end;

procedure FShowProperties(const fn: WideString; hWnd: THandle);
begin
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    FShowPropertiesW(fn, hWnd)
  else
    FShowPropertiesA(AnsiString(fn), hWnd);
end;


end.
