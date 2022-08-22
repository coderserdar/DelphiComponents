unit Misc;
{$IFDEF CONDITIONALEXPRESSIONS}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}
{$ENDIF}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, ShellApi, FileCtrl, ShlObj,
  ActiveX;

//procedure CheckPrevInstance;
function GetIniFileName: string;
procedure SwapInt(var V1, V2: Integer);
function IsInt(const S: string): Boolean;
function IsFloat(S: string): Boolean;
procedure SetStayOnTop(Form: TForm; Setting: Boolean);
function GetWindowsDir: string;
function GetWinTempDir: string;
function AddDirSuffix(Dir: string): string;
function AddThoundandFlag(Num: Integer): string;
procedure BeginWait;
procedure EndWait;
function GetFileSize(FileName: string): Integer;
function GetFileDate(FileName: string): TDateTime;
function SetFileDate(FileName: string; CreationTime, LastWriteTime, LastAccessTime: TFileTime): Boolean;
function GetFileIcon(FileName: string; var Icon: TIcon): Boolean;
function FileTimeToLocalSystemTime(FTime: TFileTime): TSystemTime;
function LocalSystemTimeToFileTime(STime: TSystemTime): TFileTime;
function GetWorkAreaRect: TRect;
function SelectDir(ParentHWnd: HWnd; const Caption: string; const Root: WideString;
  out Directory: string): Boolean;
function CheckWindows9598: Boolean;
function CreateBakFile(FileName, Ext: string): Boolean;
function ExecuteFile(FileName, Params, DefaultDir: string; ShowCmd: Integer): HWND;
procedure ShowHelp;
function Iif(Value: Boolean; Value1, Value2: Variant): Variant;
function IsValidDBName(S: string): Boolean;

implementation

uses MainFrm, OptionFrm;

{
procedure CheckPrevInstance;
var
  PrevWindow: HWND;
  i: Integer;
  Atom: TAtom;
begin
  PrevWindow := FindWindow('TMHMainForm', nil);
  if PrevWindow <> 0 then
  begin
    SetForeGroundWindow(PrevWindow);
    for i := 1 to ParamCount do
    begin
      Atom := GlobalAddAtom(PChar(ParamStr(i)));
      SendMessage(PrevWindow, WM_OPENFILE, 0, Atom);
      GlobalDeleteAtom(Atom);
    end;
    Halt;
  end;
end;
}

function GetIniFileName: string;
begin
  Result := ExtractFilePath(Application.ExeName) + 'Dbd.ini';
end;

procedure SwapInt(var V1, V2: Integer);
var
  Temp: Integer;
begin
  Temp := V1;
  V1 := V2;
  V2 := Temp;
end;

function IsInt(const S: string): Boolean;
var
  E, R: Integer;
begin
  Val(S, R, E);
  Result := E = 0;
  E := R; //avoid hints
end;

function IsFloat(S: string): Boolean;
var
  V: Extended;
begin
  Result := TextToFloat(PChar(S), V, fvExtended);
end;

procedure SetStayOnTop(Form: TForm; Setting: Boolean);
begin
  if Setting then
    SetWindowPos(Form.Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE)
  else
    SetWindowPos(Form.Handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
end;

function GetWindowsDir: string;
var
  Buf: array[0..MAX_PATH] of Char;
begin
  GetWindowsDirectory(Buf, MAX_PATH);
  Result := AddDirSuffix(Buf);
end;

function GetWinTempDir: string;
var
  Buf: array[0..MAX_PATH] of Char;
begin
  GetTempPath(MAX_PATH, Buf);
  Result := AddDirSuffix(Buf);
end;

function AddDirSuffix(Dir: string): string;
begin
  Result := Trim(Dir);
  if Result = '' then Exit;
  if Result[Length(Result)] <> '\' then Result := Result + '\';
end;

function AddThoundandFlag(Num: Integer): string;
var
  s: string;
  i, j: Integer;
begin
  s := IntToStr(Num);
  Result := '';
  j := 0;
  for i := Length(s) downto 1 do
  begin
    Result := s[i] + Result;
    Inc(j);
    if ((j mod 3) = 0) and (i <> 1) then Result := ',' + Result;
  end;
end;

procedure BeginWait;
begin
  Screen.Cursor := crHourGlass;
end;

procedure EndWait;
begin
  Screen.Cursor := crDefault;
end;

function GetFileSize(FileName: string): Integer;
var
  FileVar: file of Byte;
begin
  {$I-}
  try
    AssignFile(FileVar, FileName);
    Reset(FileVar);
    Result := FileSize(FileVar);
    CloseFile(FileVar);
  except
    Result := 0;
  end;
  {$I+}
end;

function GetFileDate(FileName: string): TDateTime;
var
  ADate: TDateTime;
  FileDate: Integer;
  FileHandle: Integer;
begin
  FileHandle := FileOpen(FileName, fmOpenRead or fmShareDenyNone);
  if FileHandle > 0 then
    FileDate := FileGetDate(FileHandle)
  else
    FileDate := 0;
  ADate := FileDateToDateTime(FileDate);
  FileClose(FileHandle);
  Result := ADate;
end;

function SetFileDate(FileName: string; CreationTime, LastWriteTime, LastAccessTime: TFileTime): Boolean;
var
  FileHandle: Integer;
begin
  FileHandle := FileOpen(FileName, fmOpenWrite or fmShareDenyNone);
  if FileHandle > 0 then
  begin
    SetFileTime(FileHandle, @CreationTime, @LastAccessTime, @LastWriteTime);
    FileClose(FileHandle);
    Result := True;
  end
  else
    Result := False;
end;

//取得与文件相关的图标
//FileName: e.g. "e:\hao\a.txt"
//Icon: 事先分配好内存
//成功则返回True

function GetFileIcon(FileName: string; var Icon: TIcon): Boolean;
var
  SHFileInfo: TSHFileInfo;
  h: HWnd;
begin
  h := SHGetFileInfo(PChar(FileName),
    0,
    SHFileInfo,
    SizeOf(SHFileInfo),
    SHGFI_ICON or SHGFI_SYSICONINDEX);
  Icon.Handle := SHFileInfo.hIcon;
  Result := (h <> 0);
end;

function FileTimeToLocalSystemTime(FTime: TFileTime): TSystemTime;
var
  STime: TSystemTime;
begin
  FileTimeToLocalFileTime(FTime, FTime);
  FileTimeToSystemTime(FTime, STime);
  Result := STime;
end;

function LocalSystemTimeToFileTime(STime: TSystemTime): TFileTime;
var
  FTime: TFileTime;
begin
  SystemTimeToFileTime(STime, FTime);
  LocalFileTimeToFileTime(FTime, FTime);
  Result := FTime;
end;

function GetWorkAreaRect: TRect;
begin
  SystemParametersInfo(SPI_GETWORKAREA, 0, @Result, 0);
end;

//浏览文件夹

function SelectDir(ParentHWnd: HWnd; const Caption: string; const Root: WideString;
  out Directory: string): Boolean;
var
  BrowseInfo: TBrowseInfo;
  Buffer: PChar;
  RootItemIDList, ItemIDList: PItemIDList;
  ShellMalloc: IMalloc;
  IDesktopFolder: IShellFolder;
  Eaten, Flags: LongWord;
begin
  Result := False;
  Directory := '';
  FillChar(BrowseInfo, SizeOf(BrowseInfo), 0);
  if (ShGetMalloc(ShellMalloc) = S_OK) and (ShellMalloc <> nil) then
  begin
    Buffer := ShellMalloc.Alloc(MAX_PATH);
    try
      SHGetDesktopFolder(IDesktopFolder);
      IDesktopFolder.ParseDisplayName(Application.Handle, nil,
        POleStr(Root), Eaten, RootItemIDList, Flags);
      with BrowseInfo do
      begin
        hwndOwner := ParentHWnd;
        pidlRoot := RootItemIDList;
        pszDisplayName := Buffer;
        lpszTitle := PChar(Caption);
        ulFlags := BIF_RETURNONLYFSDIRS;
      end;
      ItemIDList := ShBrowseForFolder(BrowseInfo);
      Result := ItemIDList <> nil;
      if Result then
      begin
        ShGetPathFromIDList(ItemIDList, Buffer);
        ShellMalloc.Free(ItemIDList);
        Directory := Buffer;
      end;
    finally
      ShellMalloc.Free(Buffer);
    end;
  end;
end;

function CheckWindows9598: Boolean;
var
  V: TOSVersionInfo;
begin
  V.dwOSVersionInfoSize := SizeOf(V);
  Result := False;
  if not GetVersionEx(V) then Exit;
  if V.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS then
    Result := True;
end;

function CreateBakFile(FileName, Ext: string): Boolean;
var
  BakFileName: string;
begin
  BakFileName := FileName + '.' + Ext;
  Result := CopyFile(PChar(FileName), PChar(BakFileName), False);
end;

function ExecuteFile(FileName, Params, DefaultDir: string; ShowCmd: Integer): HWND;
begin
  Result := ShellExecute(Application.MainForm.Handle, nil,
    PChar(FileName), PChar(Params), PChar(DefaultDir), ShowCmd);
end;

procedure ShowHelp;
var
  FileName: string;
begin
  FileName := ExtractFilePath(Application.ExeName) + 'TinyDB.chm';
  if not FileExists(FileName) then
    FileName := ExtractFilePath(Application.ExeName) + '..\Help\TinyDB.chm';
  if not FileExists(FileName) then
    FileName := 'TinyDB.chm';
  ExecuteFile(FileName, '', '', SW_SHOWDEFAULT);
end;

function Iif(Value: Boolean; Value1, Value2: Variant): Variant;
begin
  if Value then
    Result := Value1
  else
    Result := Value2;
end;

function IsValidDBName(S: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  if S = '' then Exit;
  for I := 1 to Length(S) do
    if S[I] < ' ' then Exit;
  Result := True;
end;

end.

